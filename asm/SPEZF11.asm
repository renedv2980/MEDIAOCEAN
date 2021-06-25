*          DATA SET SPEZF11    AT LEVEL 036 AS OF 12/04/18                      
*PHASE T23011A                                                                  
*INCLUDE SORTER                                                                 
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
*                                                                               
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
*  LEV 14    JUN25/98 ADD JW SOURCE = SDI, FIX CONVERTED DATE         *         
*  LEV 15    JUL24/98 MAKE AGYIDTAB LARGER FROM 100 TO 200            *         
*                     MAKE FINAL NET/GROSS EDITS LARGER               *         
*  LEV 16    OCT20/98 CHECK EACH INVOICE TO STOP BAD DELETES          *         
*  LEV 17    FEB16/99 ADD TO SOURCE CODE TABLE                        *         
*  LEV 18    JUN22/99 FIX SVWCPDAT TO CHECK FOR SPACES                *         
*  LEV 19    NOV17/00 FIX MOVE REC FROM SORT - CAUSED PGCK 11 PAGE EX *         
*  LEV 20    MAY29/01 FIX DUMMY                                       *         
*  LEV 21    SEP10/01 ADD H7 (MSNYA) TO SDI CHECK                     *         
*  LEV 22    SEP27/01 ADD TH (DFZNYA) TO SDI CHECK                    *         
*  LEV 23    OCT22/01 ADD H0 (MSHTOB) & H9 (MVNYR) TO SDI CHECK       *         
*                     AND SDIV, SVIT TO TABLE                         *         
*  LEV 24    FEB25/02 FIX AGENCY LIST                                 *         
*  LEV 25    APR16/02 PURGE FILES DIRECTLY, NOT JUST DELETE           *         
*                     ALLOW SDI BLANK FOR ALL, ADD NEW SOURCES        *         
*  LEV 26    APR29/02 ALLOW LARGER GROSS/NET DOLLAR AMONTS            *         
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
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    BADACTER                                                         
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    BADACTER                                                         
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    BADACTER                                                         
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE ON SCREEN                
         BE    BADACTER                                                         
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     DS   0H                                                                
         XIT1                                                                   
*                                                                               
RUNF     L     RE,ATWA                                                          
         MVI   29(RE),X'02'        INDICATE RUNLAST HOOK REQUIRED               
*                                                                               
         L     R1,TWADCONS         CLEAR FIRST PART OF INTER-REQUEST            
         L     R1,TSPFUSER-TWADCOND(R1)  USER SAVE AREA                         
         LHI   R0,JTOTSNQ                                                       
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         B     EXIT                                                             
*                                                                               
VKEY     DS    0H                                                               
         LA    R2,CONACTH                                                       
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
*                                                                               
         LA    R2,EZFSTAH          STATION                                      
         XC    SVSTA,SVSTA                                                      
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         CLI   5(R2),3                                                          
         BNE   *+14                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK100                                                            
*                                                                               
         GOTO1 VALISTA                                                          
         MVC   SVSTA,QSTA                                                       
*                                                                               
VK100    LA    R2,EZFDTEH          DATE                                         
         XC    SVDTE,SVDTE                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB,DMCB                                                        
         BZ    BADATE                                                           
         GOTO1 DATCON,(R1),(0,WORK),(2,SVDTE)                                   
*                                                                               
VK200    LA    R2,EZFSRCEH         SOURCE                                       
         XC    SVSRCE,SVSRCE                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK260                                                            
*                                                                               
         OC    EZFSRCE,SPACES                                                   
         LA    RE,SOURCE                                                        
         LA    RF,SOURCES                                                       
VK220    CLC   EZFSRCE,0(RE)                                                    
         BE    VK230                                                            
         LA    RE,L'SOURCE(,RE)                                                 
         BCT   RF,VK220                                                         
         B     BADSRCE                                                          
VK230    MVC   SVSRCE,0(RE)                                                     
*                                                                               
VK260    LA    R2,EZFSTATH         STATUS                                       
         XC    SVSTAT,SVSTAT                                                    
         CLI   5(R2),0                                                          
         BE    VK300                                                            
*                                                                               
         CLI   EZFSTAT,C'C'        ONLY CONVERTED                               
         BE    VK270                                                            
         CLI   EZFSTAT,C'U'        ONLY UNCONVERTED                             
         BNE   STATERR                                                          
*                                                                               
VK270    MVC   SVSTAT,EZFSTAT                                                   
*                                                                               
VK300    LA    R2,EZFDRFH          DRAFT                                        
*                                                                               
         MVI   SVDRAFT,C'Y'        DEFAULT IS DRAFT RUN                         
         CLI   5(R2),0                                                          
         BE    VK320                                                            
*                                                                               
         CLI   EZFDRF,C'Y'         YES, NO FILE UPDATE                          
         BE    VK310                                                            
         CLI   EZFDRF,C'N'         NO, REAL FILE UPDATE                         
         BNE   DRAFERR                                                          
*                                                                               
VK310    MVC   SVDRAFT,EZFDRF                                                   
*                                                                               
VK320    LA    R2,EZFRESH          RESTORE                                      
         XC    SVREST,SVREST                                                    
         CLI   5(R2),0                                                          
         BE    VK340                                                            
*                                                                               
         CLI   EZFRES,C'Y'         YES, RESTORE RUN                             
         BE    VK330                                                            
         CLI   EZFRES,C'N'         NO, REAL FILE UPDATE                         
         BNE   RESTERR                                                          
*                                                                               
VK330    MVC   SVREST,EZFRES                                                    
*                                                                               
VK340    DS    0H                                                               
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
*                                                                               
VK500    CLI   0(R4),0                                                          
         BE    VK700                                                            
*                                  USER CODE                                    
         CLI   12(R4),C'U'                                                      
         BNE   VK590                                                            
         MVC   CONHEAD(15),=C'INVALID USER ID'                                  
         CLI   1(R4),8                                                          
         BH    VKERR                                                            
         MVC   SVUID,22(R4)                                                     
*                                                                               
         MVI   SVUIDNUM,X'FF'      'ALL' IDS                                    
         CLC   SVUID(3),=C'ALL'                                                 
         BE    VK600                                                            
*                                                                               
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
*                                                                               
VK520    L     R6,AIO2                                                          
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVUIDNUM,2(R6)      BINARY USER ID (ORIGIN)                      
         MVC   SVUIDPC,6(R6)       POWER CODE                                   
         B     VK600                                                            
*                                                                               
VK590    B     VKERR                                                            
*                                  UP ERROR FIELD & NEXT BLOCK                  
VK600    ZIC   RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,32(R4)                                                        
         B     VK500                                                            
*                                                                               
VK700    MVC   CONHEAD,SPACES                                                   
*                                                                               
VKXIT    B     EXIT                                                             
MVCUID   MVC   KEY+15(0),22(R4)                                                 
*                                                                               
VKERR    OI    BYTE,X'F0'                                                       
         MVC   CONHEAD+17(5),=C'FIELD'                                          
         MVC   CONHEAD+23(1),BYTE                                               
MYERR    LA    R2,EZFOPTH                                                       
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         MVI   GCMODE,C' '         IF ERROR TURN OFF SLAVE & EXIT               
         GOTO1 ERREX2                                                           
*                                                                               
*              LIST RECORDS                                                     
*                                                                               
LIST     DS    0H                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A2C' DDWRKIO                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   AWRKIO,0(R1)                                                     
*                                                                               
         LA    RE,WRKIOB                                                        
         LHI   RF,WRKIOBL                                                       
         XCEFL                                                                  
*                                                                               
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO1                                                    
         MVC   WRKIABUF,ATIA                                                    
         MVI   WRKIFTYP,WRKIFTEZ                                                
*                                                                               
         MVI   USEIO,C'Y'                                                       
         ZAP   EZBATCT,=P'0'                                                    
         ZAP   MAXSPTS,=P'0'                                                    
         LA    RF,TCTRS                                                         
         LHI   RE,TCTRSNQ                                                       
         ZAP   0(8,RF),=P'0'                                                    
         LA    RF,8(RF)                                                         
         BCT   RE,*-10                                                          
*                                                                               
         ZAP   MAXDOLN,=P'0'                                                    
         ZAP   MAXDOLG,=P'0'                                                    
         MVI   REPORTSW,0                                                       
*                                                                               
         LHI   R0,SVLKEYLQ                                                      
         EDIT  (R0),SORTKLEN                                                    
         LHI   R0,SVLLINLQ                                                      
         EDIT  (R0),SORTRLEN                                                    
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         LA    R3,SORTWORK                                                      
         USING SVLEZLSD,R3                                                      
*                                                                               
         LA    R4,SVEZKEY                                                       
         USING EZWKRIXD,R4                                                      
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LS090                                                            
*                                                                               
         L     R1,=A(HEADING)                                                   
         A     R1,T23011RR                                                      
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         CLI   SVUIDNUM,X'FF'      UNLESS 'ALL' IDS                             
         BNE   LS090                                                            
*                                                                               
* MAKE A FAKE INDEX CALL TO GET WORKER FILE NAME                                
*                                                                               
         MVC   WRKEZUID,SVUIDNUM                                                
         MVI   WRKIACTN,WRKIANDX                                                
*                                                                               
         GOTO1 AWRKIO,WRKIOB                                                    
         CLI   WRKIERRS,0                                                       
         BE    LS060                                                            
         CLI   WRKIERRS,WRKIEEOF   FAKE CALL: DON'T CARE IF EOF                 
         BE    LS060                                                            
         DC    H'0'                                                             
*                                                                               
LS060    DS    0H                                                               
         MVC   EASIWK,WRKIFILE                                                  
* NOW GET LIST OF WORKER FILES                                                  
         MVI   WRKIACTN,WRKIAGLS                                                
         GOTO1 AWRKIO,WRKIOB                                                    
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AWRKFLST,WRKUSINF         LIST OF FILES                          
         L     R1,AWRKFLST                                                      
         AHI   R1,8                                                             
         ST    R1,CURWKPTR                                                      
*                                                                               
LS070    DS    0H                                                               
         L     R1,CURWKPTR                                                      
         CLI   0(R1),0             END OF LIST?                                 
         BE    LSEOJ                                                            
*                                                                               
         MVC   EASIWK+4(1),1(R1)   WORKER FILE CHARACTER                        
         AHI   R1,8                NEXT WORKER FILE                             
         ST    R1,CURWKPTR                                                      
*                                                                               
         CLI   EASIWK+4,C'G'       SKIP WORKER FILE G                           
         BE    LS070                                                            
*                                                                               
         MVI   WRKINDS,WRKIWFNQ                                                 
         MVC   WRKIFILE,EASIWK                                                  
*                                                                               
LS090    DS    0H                                                               
         XC    WRKEZKEY,WRKEZKEY                                                
         CLI   SVUIDNUM,X'FF'                                                   
         BE    *+10                                                             
         MVC   WRKEZUID,SVUIDNUM                                                
*                                                                               
LS100    DS    0H                                                               
         CLI   SVUIDNUM,X'FF'                                                   
         BNE   *+10                                                             
         XC    WRKEZUID,WRKEZUID                                                
*                                                                               
         MVI   WRKIACTN,WRKIANDX                                                
*                                                                               
         GOTO1 AWRKIO,WRKIOB                                                    
         CLI   WRKIERRS,0                                                       
         BE    LS150                                                            
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SVUIDNUM,X'FF'      UNLESS 'ALL' IDS                             
         BNE   LSEOJ                                                            
         B     LS070                                                            
*                                                                               
LS150    AP    TWKRECS,=P'1'                                                    
*                                                                               
         BRAS  RE,FAKEIND                                                       
*                                                                               
         CLI   EZWIDAY,X'99'       ONLY WANT EASI BATCHES                       
         BNE   LS100                                                            
*                                                                               
         AP    TEZRECS,=P'1'                                                    
*                                                                               
         CLI   SVUIDNUM,X'FF'      UNLESS 'ALL' IDS                             
         BE    *+14                                                             
         CLC   WRKEZUID,SVUIDNUM    TEST RIGHT ID                               
         BNE   LS100                                                            
*                                                                               
         MVC   WORK(4),EZWISTN         FORMAT STATION                           
         CLI   WORK+3,C' '                                                      
         BH    *+8                                                              
         MVI   WORK+3,C' '                                                      
         MVC   WORK+4(1),EZWIMED                                                
*                                                                               
* IF FILTERS MATCH ON FILTER                                                    
*                                                                               
         OC    SVSTA,SVSTA         STATION                                      
         BZ    *+14                                                             
         CLC   WORK(5),SVSTA                                                    
         BNE   LS100                                                            
*                                                                               
         MVC   SVDATEC,WRKEZBDT                                                 
         MVC   SVFILNO,WRKEZSQN                                                 
         MVI   READFLAG,C'N'                                                    
*                                                                               
LS200    DS    0H                                                               
         MVI   WRKIACTN,WRKIAGET                                                
         GOTO1 AWRKIO,WRKIOB                                                    
         CLI   WRKIERRS,0                                                       
         BE    *+14                                                             
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    LS250                                                            
         DC    H'0'                                                             
*                                                                               
         CLI   READFLAG,C'Y'                                                    
         BE    STR10                                                            
*                                                                               
         MVI   READFLAG,C'Y'                                                    
         MVC   SVWKCMT,WRKEZDSC                                                 
*                                                                               
* IF FILTERS MATCH ON FILTER                                                    
*                                                                               
         OC    SVSRCE,SVSRCE       BATCH SRCE                                   
         BZ    LS210                                                            
         CLC   SVWCSRCE,SVSRCE                                                  
         BE    LS210                                                            
*                                                                               
         CLC   =C'SDI',SVSRCE                                                   
         BNE   LS100                                                            
         CLI   SVSRCE+3,C' '                                                    
         BH    LS100                                                            
         CLC   =C'SDI',SVWCSRCE                                                 
         BNE   LS100                                                            
*                                                                               
LS210    DS    0H                                                               
         CLI   SVSTAT,0            SPECIAL STATUS                               
         BE    LS240                NO                                          
*                                                                               
         CLI   SVSTAT,C'C'         CONVERTED STATUS                             
         BE    LS220                YES                                         
         CLI   SVSTAT,C'U'         UNCONVERTED STATUS                           
         BE    LS230                YES                                         
         DC    H'0'                                                             
*                                                                               
LS220    TM    SVWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BZ    LS100                NO                                          
         B     LS240                                                            
*                                                                               
LS230    TM    SVWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BO    LS100                YES                                         
*                                                                               
LS240    LA    R1,BCTRS                                                         
         LHI   R0,BCTRSNQ                                                       
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         MVI   DELETESW,C'Y'       SET DELETE SW                                
*                                  CK FOR 31 RECS THAT ARE CONVRTD/DEL          
*                                                                               
         AP    EZBATCT,=P'1'       ADD TO TOTAL BATCHES                         
*                                                                               
STR10    DS    0H                                                               
         L     R6,AIO1                                                          
*                                                                               
         CLC   =C'31',4(R6)        THIS AN INVOICE HEADER                       
         BNE   STR20                                                            
         CLI   6(R6),X'5E'         THIS A FIELD SEPARATOR                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
         AP    BEZINV,=P'1'        INVOICES                                     
*                                                                               
         CLI   SVSTAT,C'C'         DELETE ONLY CONVERTED                        
         BE    STR14                                                            
*                                                                               
         CLI   SVSTAT,C'U'         DELETE ONLY UNCONVERTED                      
         BNE   STR50                                                            
*                                                                               
         TM    7(R6),EZIHCVQ+EZIHCDEL                                           
         BZ    STR50                                                            
         MVI   DELETESW,C'N'       SET DELETE SW OFF                            
         B     STR50                                                            
*                                                                               
STR14    DS    0H                                                               
         TM    7(R6),EZIHCVQ+EZIHCDEL                                           
         BNZ   STR50                                                            
         MVI   DELETESW,C'N'       SET DELETE SW OFF                            
         B     STR50                                                            
*                                                                               
*                                  CK FOR 34 TOTAL RECS                         
*                                                                               
STR20    DS    0H                                                               
* JAN10/2012 - SKIPPING '34' RECORD                                             
* TOO MUCH GARBAGE DATA IN NUMERIC-ONLY FIELDS LATELY                           
* AND NOBODY LOOKS AT THEM ANYWAY                                               
         B     STR40                                                            
*                                                                               
         CLC   =C'34',4(R6)        THIS AN INVOICE TOTAL                        
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
*                                                                               
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
*                                                                               
         CP    DUB,MAXDOLG                                                      
         BNH   *+10                                                             
         ZAP   MAXDOLG,DUB                                                      
*                                                                               
         AP    BEZGRS,DUB                                                       
*                                                                               
STR29    CLI   0(RF),X'15'         IF END OF REC, QUIT                          
         BE    STR50                                                            
*                                                                               
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
*                                                                               
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
*                                                                               
         CP    DUB,MAXDOLN                                                      
         BNH   *+10                                                             
         ZAP   MAXDOLN,DUB                                                      
*                                                                               
         AP    BEZNET,DUB                                                       
         B     STR50                                                            
STRPACK  PACK  DUB,0(0,RE)                                                      
*                                                                               
*                                  CK FOR 51 SPOT RECORDS                       
*                                                                               
STR40    CLC   =C'51',4(R6)        THIS AN SPOT RECORD                          
         BNE   STR50                                                            
         CLI   6(R6),X'5E'         THIS A FIELD SEPARATOR                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   7(R6),C'Y'          DID SPOT RUN                                 
         BNE   STR50                                                            
*                                                                               
         AP    BEZSPT,=P'1'              SPOTS                                  
*                                                                               
         CP    BEZSPT,MAXSPTS                                                   
         BNH   *+10                                                             
         MVC   MAXSPTS,BEZSPT                                                   
*                                                                               
* READ NEXT RECORD                                                              
*                                                                               
STR50    DS    0H                                                               
         B     LS200                                                            
*                                                                               
* EOF                                                                           
*                                                                               
LS250    DS    0H                                                               
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
*                                                                               
         OC    SVDATEC,SVDATEC                                                  
         BZ    LS340                                                            
         GOTO1 DATCON,DMCB,(2,SVDATEC),(8,LDATE)                                
LS340    OC    SVFILNO,SVFILNO                                                  
         BZ    LS360                                                            
         SR    R0,R0                                                            
         ICM   R0,15,SVFILNO                                                    
         EDIT  (R0),(6,LSEQ),COMMAS=YES                                         
*                                                                               
LS360    OC    SVWCPDAT,SVWCPDAT      TEST CONVERTED AT ALL                     
         BZ    LS364                                                            
         CLC   SVWCPDAT,SPACES        TEST CONVERTED AT ALL                     
         BE    LS364                                                            
*                                                                               
* TZIH 12/04/2018                                                               
* IF X'40' BIT IS ON IN THE MONTH BYTE, THAT MEANS THE WHOLE DATE               
* HAS BEEN OR'ED WITH SPACES AND WILL CAUSE PROBLEMS WITH DATCON                
         TM    SVWCPDAT+1,X'40'                                                 
         BZ    *+10                                                             
         NC    SVWCPDAT,=X'BFBFBF' TURN OFF X'40'S                              
*                                                                               
         GOTO1 DATCON,DMCB,(1,SVWCPDAT),(8,LSTAT)                               
*                                                                               
         TM    SVWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BO    LS370                YES                                         
         MVC   LSTAT+8(3),=C'(P)'     NO, MARK PARTIAL                          
         B     LS370                NO                                          
*                                                                               
LS364    TM    SVWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BZ    LS370                NO                                          
         MVC   LSTAT+8(3),=C'(D)'     NO, MARK DELETED                          
*                                                                               
LS370    OC    SVWCSRCE,SVWCSRCE                                                
         BZ    LS400                                                            
         MVC   LSRCE,SVWCSRCE                                                   
*                                                                               
LS400    MVC   LUID(7),=C'UNKNOWN'                                              
*                                                                               
         LA    R1,AGYIDTAB                                                      
         LA    R0,ENDAGYID-AGYIDTAB(,R1) END OF TABLE                           
*                                                                               
LS410    OC    0(8,R1),0(R1)       EMPTY ENTRY?                                 
         BZ    LS430                                                            
         CLC   8(2,R1),EZWIUID     THIS IN TABLE                                
         BE    LS420                                                            
         LA    R1,12(,R1)                                                       
         CR    R0,R1                                                            
         BH    LS410                                                            
         DC    H'0'                                                             
*                                                                               
LS420    MVC   LUID,0(R1)                                                       
         B     LS500                                                            
*                                                                               
LS430    OC    EZWIUID,EZWIUID                                                  
         BZ    LS500                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),EZWIUID                                                
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R6)                    
*                                                                               
         CLI   8(R1),0                                                          
         BNE   LS440               NO ID, PRINT AS UNKNOWN                      
*                                  LUID PREFILLED 'UNKNOWN'                     
*                                                                               
         CLC   KEY(25),0(R6)       KEY HAD BETTER BE THE SAME                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LUID,CTDSC-CTDSCD(R6)                                            
*                                                                               
LS440    DS    0H                                                               
         LA    R1,AGYIDTAB                                                      
         LA    R0,ENDAGYID-AGYIDTAB(,R1) END OF TABLE                           
*                                                                               
LS450    OC    0(8,R1),0(R1)       EMPTY ENTRY?                                 
         BZ    LS460                                                            
         LA    R1,12(,R1)                                                       
         CR    R0,R1                                                            
         BNL   LS450                                                            
         DC    H'0'                                                             
LS460    MVC   0(8,R1),LUID                                                     
         MVC   8(2,R1),EZWIUID                                                  
*                                                                               
LS500    EDIT  (P8,BEZINV),(7,LINVS),COMMAS=YES                                 
*                                                                               
         EDIT  (P8,BEZSPT),(5,LSPTS),COMMAS=YES                                 
*                                                                               
*        EDIT  (P8,BEZNET),(13,LNETD),2,COMMAS=YES                              
*                                                                               
*        EDIT  (P8,BEZGRS),(13,LGRSD),2,COMMAS=YES                              
*                                                                               
         AP    TEZINV,BEZINV                                                    
         AP    TEZSPT,BEZSPT                                                    
*        AP    TEZNET,BEZNET                                                    
*        AP    TEZGRS,BEZNET                                                    
         XC    SVEZWORK,SVEZWORK                                                
         XC    SORTWORK,SORTWORK                                                
*                                                                               
* SEE IF BATCH TO BE DELETED                                                    
*                                                                               
         CLC   SVDATEC,SVDTE       SEE IF TO BE DELETED                         
         BH    LS600                NO                                          
*                                                                               
         CLI   DELETESW,C'Y'       IS DELETE SW ON                              
         BNE   LS600                NO                                          
*                                                                               
         MVI   10(R1),C'D'                                                      
         MVC   LDEL,=C'DELETED'                                                 
         OI    SVLSTAT,SVLSTATD                                                 
*                                                                               
         AP    TEZINVD,BEZINV                                                   
         AP    TEZSPTD,BEZSPT                                                   
*        AP    TEZNETD,BEZNET                                                   
*        AP    TEZGRSD,BEZGRS                                                   
         OC    11(1,R1),BATMEDIA   SAVED MEDIA FOR THIS BATCH                   
*                                                                               
         AP    TEZDEL,=P'1'                                                     
*                                                                               
         CLI   EZFDRF,C'Y'         THIS A DRAFT RUN                             
         BE    LS600                YES, NO FILE UPDATE                         
*                                                                               
         CLI   TWAWRITE,C'Y'       THIS WRITE = YES RUN                         
         BNE   LS600                NO, NO FILE UPDATE                          
*                                                                               
         MVI   WRKIACTN,WRKIAPUR                                                
         GOTO1 AWRKIO,WRKIOB                                                    
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LS600    DS   0H                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* SAVE FOR SORTED LISTING AT EOJ                                                
*                                                                               
         MVC   SVLUID,EZWIUID                                                   
         MVC   SVLSTA(4),EZWISTN                                                
         MVC   SVLSTA+4(1),EZWIMED                                              
         MVC   SVLBDTE,SVDATEC                                                  
         MVC   SVLSRCE,SVWCSRCE                                                 
         MVC   SVLSEQ,SVFILNO                                                   
*                                                                               
         OC    SVWCPDAT,SVWCPDAT                                                
         BZ    LS620                                                            
         CLC   SVWCPDAT,SPACES                                                  
         BE    LS620                                                            
*                                                                               
* TZIH 12/04/2018                                                               
* IF X'40' BIT IS ON IN THE MONTH BYTE, THAT MEANS THE WHOLE DATE               
* HAS BEEN OR'ED WITH SPACES AND WILL CAUSE PROBLEMS WITH DATCON                
         TM    SVWCPDAT+1,X'40'                                                 
         BZ    *+10                                                             
         NC    SVWCPDAT,=X'BFBFBF'  TURN OFF X'40'S                             
*                                                                               
         GOTO1 DATCON,(R1),(1,SVWCPDAT),(2,SVLCDTE)                             
*                                                                               
LS620    TM    SVWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BZ    *+8                  NO                                          
         OI    SVLSTAT,SVLSTATC                                                 
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTWORK                                 
         B     LS100                                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
LSEOJ    GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R1,TWADCONS                                                      
         L     R1,TSPFUSER-TWADCOND(R1)  USER SAVE AREA                         
         USING JTOTS,R1                                                         
         AP    JTINV,TEZINVD                                                    
         AP    JTSPOTS,TEZSPTD                                                  
*        AP    JTNDOLS,TEZNETD                                                  
*        AP    JTGDOLS,TEZGRSD                                                  
         DROP  R1                                                               
*                                                                               
         EDIT  (P8,TWKRECS),(7,P+1),COMMAS=YES                                  
         MVC   P+10(14),=C'WORKER BATCHES'                                      
*                                                                               
         EDIT  (P8,TEZRECS),(7,P+30),COMMAS=YES                                 
         MVC   P+39(12),=C'EASI BATCHES'                                        
*                                                                               
         EDIT  (P8,TEZDEL),(7,P+55),COMMAS=YES                                  
         MVC   P+64(12),=C'EASI BATCHES'                                        
         MVC   P+77(7),=C'DELETED'                                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         EDIT  (P8,TEZINV),(11,P+26),COMMAS=YES                                 
         MVC   P+39(8),=C'INVOICES'                                             
*                                                                               
         EDIT  (P8,TEZINVD),(11,P+51),COMMAS=YES                                
         MVC   P+64(7),=C'DELETED'                                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         EDIT  (P8,TEZSPT),(11,P+26),COMMAS=YES                                 
         MVC   P+39(5),=C'SPOTS'                                                
*                                                                               
         EDIT  (P8,TEZSPTD),(11,P+51),COMMAS=YES                                
         MVC   P+64(7),=C'DELETED'                                              
*                                                                               
         EDIT  (P8,MAXSPTS),(11,P+90),COMMAS=YES                                
         MVC   P+103(10),=C'MAX IN BAT'                                         
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*        MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
*        ZAP   DUB,TEZNET                                                       
*        ED    WORK(21),DUB                                                     
*        MVC   P+20(18),WORK+3                                                  
*        MVC   P+39(3),=C'NET'                                                  
*                                                                               
*        MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
*        ZAP   DUB,TEZNETD                                                      
*        ED    WORK(21),DUB                                                     
*        MVC   P+45(18),WORK+3                                                  
*        MVC   P+64(7),=C'DELETED'                                              
*                                                                               
*        MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
*        ZAP   DUB,MAXDOLN                                                      
*        ED    WORK(21),DUB                                                     
*        MVC   P+84(18),WORK+3                                                  
*                                                                               
*        MVI   SPACING,2                                                        
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*        MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
*        ZAP   DUB,TEZGRS                                                       
*        ED    WORK(21),DUB                                                     
*        MVC   P+20(18),WORK+3                                                  
*        MVC   P+39(5),=C'GROSS'                                                
*                                                                               
*        MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
*        ZAP   DUB,TEZGRSD                                                      
*        ED    WORK(21),DUB                                                     
*        MVC   P+45(18),WORK+3                                                  
*        MVC   P+64(7),=C'DELETED'                                              
*                                                                               
*        MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
*        ZAP   DUB,MAXDOLG                                                      
*        ED    WORK(21),DUB                                                     
*        MVC   P+84(18),WORK+3                                                  
*        MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R3,AGYIDTAB                                                      
         LA    R2,ENDAGYID-AGYIDTAB(,R3) END OF TABLE                           
*                                                                               
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         OC    0(8,RE),0(RE)                                                    
         BZ    *+14                                                             
         LA    RE,12(,RE)                                                       
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
         LPR   R4,RF                                                            
         BZ    LSEOJ04                                                          
*                                                                               
         GOTO1 XSORT,DMCB,(R3),(R4),12,12,0                                     
*                                                                               
LSEOJ04  OC    0(8,R3),0(R3)       EMPTY ENTRY?                                 
         BZ    LSEOJ06                                                          
         MVC   P+1(8),0(R3)                                                     
         CLI   10(R3),C'D'                                                      
         BNE   *+10                                                             
         MVC   P+10(7),=C'DELETES'                                              
*                                                                               
         LA    RE,P+20                                                          
         TM    11(R3),X'01'        TV                                           
         BZ    *+12                                                             
         MVI   0(RE),C'T'                                                       
         LA    RE,2(,RE)                                                        
*                                                                               
         TM    11(R3),X'02'        RADIO                                        
         BZ    *+12                                                             
         MVI   0(RE),C'R'                                                       
         LA    RE,2(,RE)                                                        
*                                                                               
         TM    11(R3),X'04'        NETWORK RADIO                                
         BZ    *+12                                                             
         MVI   0(RE),C'X'                                                       
         LA    RE,2(,RE)                                                        
*                                                                               
         TM    11(R3),X'10'        CABLE                                        
         BZ    *+12                                                             
         MVI   0(RE),C'C'                                                       
         LA    RE,2(,RE)                                                        
*                                                                               
         TM    11(R3),X'20'        SYNDICATED                                   
         BZ    *+12                                                             
         MVI   0(RE),C'S'                                                       
         LA    RE,2(,RE)                                                        
*                                                                               
         TM    11(R3),X'40'        NETWORK                                      
         BZ    *+12                                                             
         MVI   0(RE),C'N'                                                       
         LA    RE,2(,RE)                                                        
*                                                                               
         TM    11(R3),X'80'        UNKNOWN                                      
         BZ    *+8                                                              
         MVI   0(RE),C'?'                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,12(,R3)                                                       
         CR    R2,R3                                                            
         BNL   LSEOJ04                                                          
LSEOJ06  GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XC    SVUIDNUM,SVUIDNUM                                                
         MVI   REPORTSW,1                                                       
*                                                                               
         SR    R5,R5                                                            
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
*                                                                               
         ICM   R2,15,4(R1)          GET REC ADDRESS                             
         BZ    NOBATERR                                                         
*                                                                               
         MVC   SORTWORK,0(R2)                                                   
*                                                                               
         LA    R3,SORTWORK                                                      
         BCTR  R5,0                                                             
         B     LSEOJ14                                                          
         DC    H'0'                                                             
*                                                                               
LSEOJ10  CLC   SVUIDNUM,SVLUID                                                  
         BE    LSEOJ20                                                          
LSEOJ12  GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  (P8,TEZRECS),(7,P+1),COMMAS=YES                                  
         MVC   P+10(7),=C'BATCHES'                                              
*                                                                               
         EDIT  (P8,TEZPCON),(7,P+20),COMMAS=YES                                 
         MVC   P+29(14),=C'PARTIALLY DONE'                                      
*                                                                               
         EDIT  (P8,TEZCON),(7,P+45),COMMAS=YES                                  
         MVC   P+54(12),=C'DONE BATCHES'                                        
*                                                                               
         EDIT  (P8,TEZDEL),(7,P+68),COMMAS=YES                                  
         MVC   P+77(15),=C'DELETED BATCHES'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LTR   R5,R5               ALL DONE                                     
         BZ    EXIT                                                             
*                                                                               
LSEOJ14  MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   PAGE,=H'1'                                                       
*                                                                               
         LA    RF,TCTRS                                                         
         LHI   RE,TCTRSNQ                                                       
         ZAP   0(8,RF),=P'0'                                                    
         LA    RF,8(RF)                                                         
         BCT   RE,*-10                                                          
*                                                                               
         MVC   SVUIDNUM,SVLUID                                                  
*                                                                               
         MVC   SVUID(7),=C'UNKNOWN'                                             
         MVI   SVUID+7,C' '                                                     
*                                                                               
         OC    SVLUID,SVLUID                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),SVLUID                                                 
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R6)                    
*                                                                               
         CLI   8(R1),0                                                          
         BNE   LSEOJ20             IF NOT FOUND, USE UNKNOWN                    
*                                                                               
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVUID,CTDSC-CTDSCD(R6)                                           
*                                                                               
LSEOJ20  AP    TEZRECS,=P'1'                                                    
         MVC   LUID,SVUID                                                       
         MVC   LSTA(4),SVLSTA                                                   
         LA    RF,LSTA+4                                                        
         CLI   LSTA+3,C' '                                                      
         BH    *+6                                                              
         BCTR  RF,0                                                             
         MVI   0(RF),C'-'                                                       
*        MVC   1(1,RF),WORK+13                                                  
         MVC   1(1,RF),SVLSTA+4                                                 
         CLI   1(RF),C' '                                                       
         BH    *+8                                                              
         MVI   1(RF),C'T'                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,SVLBDTE),(5,LDATE)                                
*                                                                               
         MVC   LSRCE,SVLSRCE                                                    
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,15,SVLSEQ                                                     
         EDIT  (R0),(6,LSEQ),FILL=0                                             
*                                                                               
         OC    SVLCDTE,SVLCDTE                                                  
         BZ    LSEOJ24                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,SVLCDTE),(5,LSTAT)                                
         TM    SVLSTAT,SVLSTATC                                                 
         BO    LSEOJ26                                                          
         MVC   LSTAT+8(3),=C'(P)'     NO, MARK PARTIAL                          
         AP    TEZPCON,=P'1'                                                    
         B     LSEOJ30                                                          
*                                                                               
LSEOJ24  TM    SVLSTAT,SVLSTATC                                                 
         BZ    LSEOJ30                                                          
         MVC   LSTAT+8(3),=C'(D)'     NO, MARK DELETE                           
*                                                                               
LSEOJ26  AP    TEZCON,=P'1'                                                     
*                                                                               
LSEOJ30  TM    SVLSTAT,SVLSTATD                                                 
         BZ    LSEOJ40                                                          
         MVC   LDEL,=C'DELETED'                                                 
         AP    TEZDEL,=P'1'                                                     
*                                                                               
LSEOJ40  DS   0H                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
*                                                                               
         ICM   R2,15,4(R1)          GET REC ADDRESS                             
         BZ    LSEOJ60                                                          
*                                                                               
         MVC   SORTWORK,0(R2)                                                   
         BCT   R5,LSEOJ10                                                       
         DC    H'0'                                                             
*                                                                               
* E-O-F                                                                         
*                                                                               
LSEOJ60  LPR   R5,R5                                                            
         CVD   R5,DUB                                                           
         CP    DUB,EZBATCT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         SR    R5,R5                                                            
         B     LSEOJ12                                                          
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(20),=CL20'JOB TOTALS:'                                         
         MVC   P2(20),=CL20'----------'                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R2,TWADCONS                                                      
         L     R2,TSPFUSER-TWADCOND(R2)  USER SAVE AREA                         
         USING JTOTS,R2                                                         
*                                                                               
         MVC   P(10),=CL10'INVOICES:'                                           
         EDIT  JTINV,(16,P+10),COMMAS=YES                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P(10),=CL10'SPOTS:'                                              
         EDIT  JTSPOTS,(16,P+10),COMMAS=YES                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*        MVC   P(10),=CL16'NET:'                                                
*        EDIT  JTNDOLS,(16,P+10),2,COMMAS=YES                                   
*        MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
*        ZAP   DUB,JTNDOLS                                                      
*        ED    WORK(21),DUB                                                     
*        MVC   P+10(18),WORK+3                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*        MVC   P(10),=CL10'GROSS:'                                              
*        EDIT  JTGDOLS,(16,P+10),2,COMMAS=YES                                   
*        MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
*        ZAP   DUB,JTGDOLS                                                      
*        ED    WORK(21),DUB                                                     
*        MVC   P+10(18),WORK+3                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         DROP  R2                                                               
         B     EXIT                                                             
*                                                                               
* HEAD HOOK RTN *                                                               
*                                                                               
HDHK     NTR1                                                                   
         MVC   H3+32(24),=C'ALL BATCHES ON OR BEFORE'                           
         GOTO1 DATCON,(R1),(2,SVDTE),(5,H3+57)                                  
*                                                                               
         LA    R4,H3+1                                                          
         CLI   SVSRCE,0                                                         
         BE    HDHK10                                                           
*                                                                               
         MVC   0(11,R4),=C'ONLY SOURCE'                                         
         MVC   12(4,R4),SVSRCE                                                  
         LA    R4,132(,R4)                                                      
*                                                                               
HDHK10   CLI   SVSTAT,0            STATUS                                       
         BE    HDHK20                                                           
*                                                                               
         MVC   0(14,R4),=C'ONLY CONVERTED'                                      
         CLI   SVSTAT,C'C'         STATUS                                       
         BE    HDHK16                                                           
         MVC   5(11,R4),=C'UNCONVERTED'                                         
HDHK16   LA    R4,132(,R4)                                                      
*                                                                               
HDHK20   CLI   SVDRAFT,C'Y'        ONLY UNCONVERTED                             
         BNE   HDHK30                                                           
         MVC   0(9,R4),=C'DRAFT RUN'                                            
         LA    R4,132(,R4)                                                      
*                                                                               
HDHK30   CLI   SVREST,C'Y'                                                      
         BNE   HDHK40                                                           
         MVC   0(9,R4),=C'RESTORE'                                              
*                                                                               
HDHK40   CLI   REPORTSW,0                                                       
         BE    EXIT                                                             
         MVC   H4+36(17),=C'BY AGENCY LISTING'                                  
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
*                                                                               
BADNUM   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
BADATE   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
BADACTER MVI   ERROR,INVACT                                                     
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
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
NOUPDER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOUPDMS),NOUPDMS                                       
         B     ERREXIT                                                          
NOBATERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOBATMS),NOBATMS                                       
ERREXIT  GOTO1 ERREX2                                                           
         DC    H'0'                                                             
*                                                                               
*                                                                               
SVEZTBLN DC    A(SVLLINLQ*1000)                                                 
*                                                                               
         LTORG                                                                  
*                                                                               
*                   1---5----10---15---20---25---30---35---40                   
SORTCARD DC    CL80'SORT FIELDS=(1,XX,A),FORMAT=BI,WORK=1'                      
*                                                                               
         ORG   SORTCARD                                                         
         DS    XL15                                                             
SORTKLEN DS    CL2                                                              
         ORG                                                                    
*                                                                               
*                   1---5----10---15---20---25---30---35---40                   
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=XX'                                    
*                                                                               
         ORG   RECCARD                                                          
         DS    XL21                                                             
SORTRLEN DS    CL2                                                              
         ORG                                                                    
*                                                                               
*                                                                               
*                                                                               
BDSRCMS  DC    C'* ERROR * INVALID SOURCE CODE *'                               
STATMS   DC    C'* ERROR * STATUS MUST BE C OR U *'                             
RESTMS   DC    C'* ERROR * RESTORE MUST BE Y OR N *'                            
DRAFMS   DC    C'* ERROR * DRAFT MUST BE Y OR N *'                              
NOBATMS  DC    C'* ERROR * NO BATCHES ON FILE *'                                
NOUPDMS  DC    C'* ERROR * NO UPDATES ALLOWED *'                                
*                                                                               
*                                                                               
ACIREC   DC    A(CIREC)                                                         
ACXREC   DC    A(CXREC)                                                         
AWRKFLST DS    A                                                                
CURWKPTR DS    A                                                                
UKEY     DS    XL40                                                             
*                                                                               
MEDTBL   DC    CL3' TV',X'01'                                                   
         DC    CL3'TTV',X'01'                                                   
         DC    CL3'AAM',X'02'                                                   
         DC    CL3'FFM',X'02'                                                   
         DC    CL3'CC ',X'10'                                                   
         DC    CL3'SS ',X'20'                                                   
         DC    CL3'NN ',X'40'                                                   
         DC    CL3'XX ',X'04'                                                   
         DC    CL3' ??',X'80'                                                   
*                                                                               
SOURCE   DC    CL5'ABCX'                                                        
         DC    CL5'AEN '                                                        
         DC    CL5'AVN '                                                        
         DC    CL5'BCTV'                                                        
         DC    CL5'BET '                                                        
         DC    CL5'BME '                                                        
         DC    CL5'BRVD'                                                        
         DC    CL5'CBS '                                                        
         DC    CL5'CFCF'                                                        
         DC    CL5'CJDS'                                                        
         DC    CL5'CMR '                                                        
         DC    CL5'CNBC'                                                        
         DC    CL5'CNN '                                                        
         DC    CL5'COKA'                                                        
         DC    CL5'COKE'                                                        
         DC    CL5'COM '                                                        
         DC    CL5'COMV'                                                        
         DC    CL5'COZE'                                                        
         DC    CL5'CRT '                                                        
         DC    CL5'CTP '                                                        
         DC    CL5'DSC '                                                        
         DC    CL5'EMT '                                                        
         DC    CL5'ENT '                                                        
         DC    CL5'ENTR'                                                        
         DC    CL5'ESGI'                                                        
         DC    CL5'FAM '                                                        
         DC    CL5'FOX '                                                        
         DC    CL5'FSN '                                                        
         DC    CL5'GNC '                                                        
         DC    CL5'GRPW'                                                        
         DC    CL5'GSN '                                                        
         DC    CL5'HGTV'                                                        
         DC    CL5'IPSD'                                                        
         DC    CL5'IPSG'                                                        
         DC    CL5'IPSM'                                                        
         DC    CL5'IPSO'                                                        
         DC    CL5'IPSP'                                                        
         DC    CL5'IPSR'                                                        
         DC    CL5'IPSY'                                                        
         DC    CL5'IPSZ'                                                        
         DC    CL5'IRNY'                                                        
         DC    CL5'JEFF'                                                        
         DC    CL5'KATZ'                                                        
         DC    CL5'KNCO'                                                        
         DC    CL5'LIF '                                                        
         DC    CL5'LIPA'                                                        
         DC    CL5'MTV '                                                        
         DC    CL5'MKTR'                                                        
         DC    CL5'NBC '                                                        
         DC    CL5'NBC '                                                        
         DC    CL5'NBCN'                                                        
         DC    CL5'NCC '                                                        
         DC    CL5'O2TV'                                                        
         DC    CL5'OMD '                                                        
         DC    CL5'PRN '                                                        
         DC    CL5'RAIN'                                                        
         DC    CL5'RAPP'                                                        
         DC    CL5'RMR '                                                        
         DC    CL5'SDI '                                                        
         DC    CL5'SDIC'                                                        
         DC    CL5'SDIJ'                                                        
         DC    CL5'SDIL'                                                        
         DC    CL5'SDIM'                                                        
         DC    CL5'SDIT'                                                        
         DC    CL5'SDIV'                                                        
         DC    CL5'SDIZ'                                                        
         DC    CL5'SHAI'                                                        
         DC    CL5'STRA'                                                        
         DC    CL5'TBS '                                                        
         DC    CL5'TECC'                                                        
         DC    CL5'TVFN'                                                        
         DC    CL5'TZIH'                                                        
         DC    CL5'TZCL'                                                        
         DC    CL5'UNI '                                                        
         DC    CL5'USA '                                                        
         DC    CL5'VNI '                                                        
         DC    CL5'WGN '                                                        
         DC    CL5'WIM '                                                        
         DC    CL5'WLA '                                                        
         DC    CL5'WW1 '                                                        
SOURCES  EQU   (*-SOURCE)/L'SOURCE                                              
*                                                                               
AGYIDTAB DC 200XL12'00'             AGENCY IDS PLUS FLAG FOR DELETED            
ENDAGYID EQU   *                                                                
*                                                                               
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
*        SSPEC H8,100,C'NET'                                                    
*        SSPEC H9,100,C'---'                                                    
*        SSPEC H8,112,C'GROSS'                                                  
*        SSPEC H9,112,C'-----'                                                  
         DC    X'00'                                                            
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* FAKEIND - ROUTINE TO FILL IN INDEX FIELDS WITH DDWRKIOD DATA                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
FAKEIND  NTR1  BASE=*,LABEL=*                                                   
         LAY   R1,WRKEZKEY                                                      
         USING WRKEZKEY,R1                                                      
         LA    R2,SVEZKEY                                                       
         USING UKINDEX,R2                                                       
*                                                                               
         MVC   UKUSRID,WRKEZUID                                                 
         MVC   UKSYSPRG(L'WRKEZSCL),WRKEZSCL                                    
         MVC   UKDAY,WRKEZDAY                                                   
         MVC   UKCLASS,WRKEZMED                                                 
* !!! UKTYPE AND UKATTB SACRIFICED TO FIT IN THE 4-BYTE SEQ NUMBER !!!          
         MVC   UKFILENO(L'WRKEZSQN),WRKEZSQN                                    
         MVC   UKSTAT,WRKEZSTA                                                  
         MVC   UKAGELD,WRKEZBDT                                                 
         MVC   UKUDATA,WRKEZUDT                                                 
*                                                                               
         DROP  R2,R1                                                            
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
*                                                                               
CXHDR    DC    CL16'**CXREC**CXREC**'                                           
CXREC    DS    18432C                                                           
*                                                                               
         DS    0L                                                               
CIHDR    DC    CL16'**CIREC**CIREC**'                                           
CIREC    DS    18432C                                                           
*                                                                               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
* CTGENFILE                                                                     
       ++INCLUDE SPGENEZ                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFFFD                                                       
         ORG   CONTAGH                                                          
* SPEZFFAD                                                                      
       ++INCLUDE SPEZFF6D                                                       
*                                                                               
* DDGENTWA                                                                      
* DMWRKFD                                                                       
* DMWRKFK                                                                       
* SPGENSTA                                                                      
* CTGENFILE                                                                     
* SPEZFWORKD                                                                    
*                                                                               
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
*                                                                               
* DMWRKFD                                                                       
       ++INCLUDE DMWRKFD                                                        
*                                                                               
* DMWRKFK                                                                       
       ++INCLUDE DMWRKFK                                                        
*                                                                               
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
*                                                                               
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
*                                                                               
* EZBLOCK                                                                       
       ++INCLUDE EZBLOCK                                                        
*XTRAINFD                                                                       
       ++INCLUDE FAXTRAINF                                                      
*COMFACSD                                                                       
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
* SPEZFWORKD                                                                    
       ++INCLUDE SPEZFWORKD                                                     
       ++INCLUDE DDTWADCOND                                                     
*                                                                               
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
T23011RR DS    F                                                                
SVLNNUM  DS    F                                                                
AWRKIO   DS    F                                                                
*                                                                               
* MAX PER BATCH                                                                 
*                                                                               
MAXSPTS  DS    PL8                 SPOTS                                        
MAXDOLN  DS    PL8                 NET DOLLARS                                  
MAXDOLG  DS    PL8                 GROSS DOLLARS                                
*                                                                               
* BATCH COUNTERS                                                                
*                                                                               
BCTRS    DS    0X                                                               
BEZINV   DS    PL8                 TOTAL INVOICES                               
BEZSPT   DS    PL8                 TOTAL SPOTS                                  
BEZNET   DS    PL8                 NET DOLLARS                                  
BEZGRS   DS    PL8                 GROSS DOLLARS                                
BCTRSNQ  EQU   (*-BCTRS)/8                                                      
*                                                                               
* TOTAL COUNTERS                                                                
*                                                                               
TCTRS    DS    0X                                                               
TWKRECS  DS    PL8                 TOTAL BATCHES ON FILE                        
TEZRECS  DS    PL8                 EZ BATCHES ON FILE                           
TEZDEL   DS    PL8                 DELETE BATCHES                               
TEZCON   DS    PL8                 TOTALLY CONVERTED BATCHES                    
TEZPCON  DS    PL8                 PATIALLY CONVERTED BATCHES                   
TEZINV   DS    PL8                 TOTAL INVOICES                               
TEZINVD  DS    PL8                 DELETED INVOICES                             
TEZSPT   DS    PL8                 TOTAL SPOTS                                  
TEZSPTD  DS    PL8                   "     "   DELETED                          
TEZNET   DS    PL8                 NET DOLLARS                                  
TEZNETD  DS    PL8                  "     "    DELETED                          
TEZGRS   DS    PL8                 GROSS DOLLARS                                
TEZGRSD  DS    PL8                   "      "    DELETED                        
TCTRSNQ  EQU   (*-TCTRS)/8                                                      
*                                                                               
EZBATCT  DS    PL8                 COUNT OF BATCHES PROCESSED                   
*                                                                               
READFLAG DS    C                                                                
*                                                                               
SVWKCMT  DS    0CL16                                                            
SVWCSTAT DS    XL1                                                              
SVWCPDAT DS    XL3                                                              
         DS    XL2                                                              
         DS    XL2                                                              
         DS    XL2                                                              
SVWCSRCE DS    CL4                                                              
         DS    XL2                                                              
*                                                                               
SVFILNO  DS    XL4                                                              
SVDATEC  DS    XL2                                                              
*                                                                               
* USED TO FIND MIXED BATCHES - IF REQ IS FOR CONVERTED OR UNCONVERTED           
* THIS IS SET IF BOTH ARE IN SAME BATCH AND BATCH CAN'T BE DELETED              
*                                                                               
DELETESW DS    XL1                                                              
*                                                                               
REPORTSW DS    XL1                                                              
SVSTA    DS    CL5                                                              
SVDTE    DS    XL2                                                              
SVSRCE   DS    CL4                                                              
SVSTAT   DS    CL1                                                              
SVDRAFT  DS    CL1                                                              
SVREST   DS    CL1                                                              
SVUID    DS    CL8                                                              
SVUIDNUM DS    XL2                                                              
SVUIDPC  DS    CL2                                                              
SVEZKEY  DS    CL42                                                             
BATMEDIA DS    CL1                 SAVED MEDIA FOR EACH BATCH                   
*                                                                               
* WORK AREA FOR ONE BATCH                                                       
*                                                                               
SVEZWORK DS    CL30                                                             
*                                                                               
SORTWORK DS    CL20                                                             
*                                                                               
       ++INCLUDE DDWRKIOD                                                       
*                                                                               
WRKFEND  EQU   *                                                                
*                                                                               
*                                                                               
JTOTS    DSECT                                                                  
JTINV    DS    PL8                 INVOICES                                     
JTSPOTS  DS    PL8                 SPOTS                                        
JTNDOLS  DS    PL8                 NET DOLLARS                                  
JTGDOLS  DS    PL8                 GROSS DOLLARS                                
JTOTSNQ  EQU   (*-JTOTS)/8                                                      
*                                                                               
*                                                                               
*                                                                               
* TABLE OF ALL EASI BATCHES ON WORKER FILE                                      
*                                                                               
SVLEZLSD DSECT                                                                  
SVLUID   DS    XL2                 AGENCY ID                                    
SVLSTA   DS    CL5                 COMPRESSED STATION                           
SVLBDTE  DS    XL2                 BATCH (LOADED) DATE                          
SVLSRCE  DS    CL4                 SOURCE                                       
SVLKEYLQ EQU   *-SVLEZLSD                                                       
SVLSEQ   DS    CL4                 BATCH SEQ #                                  
SVLCDTE  DS    XL2                 CONVERTED DATE                               
SVLSTAT  DS    XL1                                                              
SVLSTATC EQU   X'40'               ALL INVOICES IN BATCH CONVERTED              
SVLSTATD EQU   X'20'               BATCH DELETED                                
         DS    X                   SPARE                                        
SVLLINLQ EQU   *-SVLEZLSD                                                       
*                                                                               
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
*         DS    CL3                                                             
*LNETD    DS    CL13                                                            
*         DS    CL1                                                             
*LGRSD    DS    CL13                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036SPEZF11   12/04/18'                                      
         END                                                                    
