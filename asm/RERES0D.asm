*          DATA SET RERES0D    AT LEVEL 012 AS OF 04/20/09                      
*PHASE T8190DC                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T8190D - RERES0D - REP RESEARCH TITLES REPORT'                  
*                                                                               
**********************************************************************          
*                                                                    *          
*     RERES0D  (T8190D)  -- REP RESEARCH TITLES LISTING REPORT       *          
*                            (INVENTORY MASTER LISTING)              *          
*                                                                    *          
*--------------------------------------------------------------------*          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* AUG4/89 -- (SNS)  ADD OPTION TO SUPRESS MULTIPLE DAYPARTS          *          
*                                                                    *          
* MAR22/90 (MRR) --- PRINT MARKET NAME FROM THE STATION RECORD, NOT  *          
*                     THE DEMO SYSTEM NAME                           *          
*                                                                    *          
* AUG02/90 (MRR) --- 1)CHANGE DAYPART NAME TABLE IFF REP=NB(NBC)     *          
*                    2)CHANGE INV REC X'92' KEY LABELS AS PER DSECT  *          
*                                                                    *          
* SEP25/90 (MRR) --->CHANGE HEADLINE TO STANDARD                     *          
*                                                                    *          
* AUG27/92 (BU ) --->FIX TITLES BUG                                  *          
*                                                                    *          
* JUL25/01 (BU ) --- >CHANGES TO 'DUMMY' WORKSPACE USE.            * *          
*                                                                    *          
*  APR/09  (SMY) --->SUPPORT NEW INVENTORY KEY                       *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
         SPACE                                                                  
T8190D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**190D**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
*                                                                               
         L     RA,=A(SUBROUTS)     COMMON ROUTINES                              
         A     RA,RELO             RELOCATE ADDRESS                             
         USING SUBROUTS,RA                                                      
*                                                                               
         LH    RF,=Y(PL1-SYSD)     SET ADDRESS OF PRINT LINES                   
         LA    RF,SYSD(RF)                                                      
         ST    RF,APL1                                                          
*                                                                               
         LH    RF,=Y(STLIST-SYSD)  SET ADDRESS OF STATION LIST                  
         LA    RF,SYSD(RF)                                                      
         ST    RF,ASTLIST                                                       
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         TITLE 'T8190D --- RERES0D --- INVENTORY TITLES - VKEY'                 
********************************************************************            
*                                                                  *            
*     RERES0D (T8190D) --- INVENTORY TITLES                        *            
*                                                                  *            
*        VALIDATE KEY                                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VKEY     DS    0H                                                               
*                                     ANALYZE PFKEYS                            
         MVI   PRINTOPT,X'00'      CLEAR OUT PRINTOPT                           
         LA    RF,SAVELN                                                        
         LA    RE,SYSSPARE         INIT SAVEAREA                                
         XCEF                                                                   
         MVC   STAMP,=CL8'T8190D'  STAMP SAVEAREA                               
*                                                                               
         LA    R4,KEY              GET PARENT REP FROM REP RECORD               
         USING RREPKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
         DROP  R4                                                               
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RREPELEM,R6                                                      
         MVC   REPPAR,RREPPAR                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         B     XIT                    NO ERRORS                                 
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - VREC'                 
***********************************************************************         
*                                                                     *         
*        VALIDATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VREC     MVI   PRINTOPT,0          CLEAR OUT PRINTOPT                           
*                                                                               
         CLI   TRANSSW,C'Y'        IF A TRANSFER INTO PROGRAM                   
         BNE   VRTRANSX                                                         
*                                                                               
         GOTO1 =A(GETFLDS),RR=RELO     GET FIELDS                               
*                                                                               
VRTRANSX DS    0H                                                               
*                                                                               
         TITLE 'T8190D --- RERES0D --- INVENTORY MASTER - VSTA'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190D) --- INVENTORY TITLES                        *            
*                                                                  *            
*        VALIDATE STATION                                          *            
*                                                                  *            
*        LIST OF STATIONS OR A MENU ID DESIGNATED AS M=XXXX        *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VSTA     DS    0H                                                               
*                                                                               
         GOTO1 =A(VALSTA),RR=RELO                                               
*                                                                               
VSTAX    DS    0H                                                               
*                                                                               
VREC10   LA    R2,TITDPTH          VALIDATE DAYPART                             
*                                                                               
         XC    DPLIST,DPLIST       INIT DAYPART LIST                            
         XC    DPMENU,DPMENU       INIT DAYPART MENU CODE                       
*                                                                               
         CLI   5(R2),0             IF NOT ENTERED                               
         BE    *+10                                                             
         CLC   8(3,R2),=C'ALL'     OR ALL                                       
         BNE   *+14                                                             
         MVC   DPMENU,=C'ALL '        USE MENU 'ALL '                           
         B     DPTMENU                                                          
*                                                                               
****     CLI   5(R2),1                                                          
****     BNE   *+8                                                              
****     OI    PRINTOPT,X'20'      1 DPT/DAY/INV                                
*                                                                               
         CLC   =C'M=',8(R2)        MENU IF IT STARTS 'M='                       
         BNE   DPT05                                                            
*                                                                               
         MVC   DPMENU,10(R2)       SAVE MENU CODE                               
         OC    DPMENU,SPACES       SPACE FILL                                   
*                                                                               
         B     DPTMENU                                                          
*                                                                               
DPT05    DS    0H                                                               
*                                                                               
         ZIC   RF,5(R2)            NUMBER OF DAYPARTS                           
         STC   RF,NOREQDPT         SAVE NUMBER OF REQUESTED DAYPARTS            
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),8(R2)     SAVE DAYPART LIST                            
*                                                                               
         B     DPTMENUX                                                         
*                                                                               
*        READ SET RECORD FOR DAYPART MENU                                       
*                                                                               
DPTMENU  DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH SET RECORD KEY                     
         USING RSETKEY,R4                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ   SET AS SET RECORD RECORD                     
         MVC   RSETKREP,REPPAR     SET REP CODE                                 
         MVC   RSETKSET,=C'DP'     SET SET CODE                                 
         MVC   RSETKID,DPMENU      SET SET IDENTIFIER                           
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTMENUE                                                         
*                                                                               
         GOTO1 GETREC              READ IN SET RECORD                           
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,RSETMCDQ     FIND MEMBERS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DPTMENUE            MUST FIND ELEMENT                            
*                                                                               
         USING RSETMEMD,R6         ESTABLISH MEMBERS ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETMTOV)     DECREMENT BY OVERHEAD LENGTH                 
         BNP   DPTMENUE            MUST HAVE SOME MEMBERS                       
*                                                                               
         STC   RF,NOREQDPT         SET NUMBER OF DAYPARTS                       
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),RSETMEMB  COPY DAYPARTS                                
*                                                                               
DPTMENUX DS    0H                                                               
*                                                                               
*        VALIDATE INDIVIDUALLY ENTERED DAYPARTS                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH DAYPART RECORD KEY                 
         USING RRDPKEY,R4                                                       
*                                                                               
         MVI   RRDPKTYP,RRDPKIDQ   SET AS RESEARCH DAYPART RECORD               
         MVC   RRDPKREP,REPPAR     SET REP CODE                                 
*                                                                               
         LA    R5,DPLIST           START OF INPUT                               
*                                                                               
         ZIC   R0,NOREQDPT         # OF REQUESTED DAYPARTS                      
*                                                                               
         LA    R3,DPTBL            ESTABLISH DAYPART TABLE                      
         USING DPTBLD,R3                                                        
         XC    DPTBLD(DPTBLL),DPTBL    INIT FIRST ENTRY                         
*                                                                               
DPTLOOP  DS    0H                                                               
*                                                                               
         MVC   RRDPKDPT,0(R5)      SET NEXT DAYPART IN KEY                      
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTINVE                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,IO               POINT TO FOUND RECORD                        
         MVI   ELCODE,X'01'        SEARCH FOR DAYPART ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DPTLOOP             IGNORE IF NOT FOUND                          
*                                                                               
         USING RRDPELEM,R6         ESTABLISH DAYPART ELEMENT                    
*                                                                               
         MVC   DPTBCODE,RRDPKDPT   SAVE DAYPART CODE                            
         MVC   DPTBSNAM,RRDPSNAM   SAVER SHORT NAME                             
         MVC   DPTBLNAM,RRDPLNAM   SAVE LONG NAME                               
*                                                                               
DPTCONT  DS    0H                                                               
*                                                                               
         LA    R3,DPTBLL(R3)       BUMP TO NEXT ENTRY IN DPTBL                  
         XC    DPTBLD(DPTBLL),DPTBL   INIT NEXT ENTRY                           
         LA    R5,1(R5)            BUMP TO NEXT ENTERED DAYPART                 
         BCT   R0,DPTLOOP                                                       
*                                                                               
DPTDONE  DS    0H                                                               
*                                                                               
         B     DPTVALX                                                          
*                                                                               
DPTMENUE DS    0H                  INVALID DAYPART MENU ID                      
         MVC   CONHEAD(L'DPMENUER),DPMENUER                                     
         B     MYEND                                                            
*                                                                               
DPTINVE  DS    0H                  INVALID DAYPART                              
         MVC   CONHEAD(L'DPINVER),DPINVER                                       
         B     MYEND                                                            
*                                                                               
DPTVALX  DS    0H                                                               
*                                                                               
         EJECT                                                                  
         SPACE 1                                                                
VREC40   LA    R2,TITSTRTH         START DATE                                   
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VREC50                                                           
         OI    PRINTOPT,X'20'      INDICATE START DATE USED                     
         GOTO1 VVALDATE                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,SDATE)  SDATE=2 BYTE COMPRESSED          
         SPACE 1                                                                
VREC50   LA    R2,TITENDH          END DATE                                     
         MVC   EDATE,=X'FFFF'                                                   
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VREC60                                                           
         OI    PRINTOPT,X'10'      INDICATE END DATE USED                       
         GOTO1 VVALDATE                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,EDATE)   EDATE=2 BYTE COMPRESSED         
         SPACE 1                                                                
         CLC   SDATE,EDATE         START CAN'T BE GREATER THAN END              
         BNH   VREC60                                                           
         MVC   CONHEAD(L'SEDATES),SEDATES                                       
         B     MYEND                                                            
         SPACE 1                                                                
VREC60   LA    R2,TITFILTH         FILTERS                                      
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VREC70                                                           
         OI    PRINTOPT,X'40'      INDICATE FILTERS USED                        
         MVC   NUMFILT,5(R2)       SAVE NUMBER OF FILTERS                       
         CLI   5(R2),6                                                          
         BNH   VREC70              MAXIMUM OF 6 ALLOWED                         
         MVC   CONHEAD(L'MANYFLT),MANYFLT                                       
         B     MYEND                                                            
*                                                                               
VREC70   LA    R2,TITOP1H          PROTECTED ONLY                               
         MVI   ERROR,2                                                          
         MVI   SAVOP1,0            INIT OPTION SAVEAREA                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,5(R2)          LENGTH OF ENTRY                              
         BZ    VREC80              NO ENTRY OKAY - OPTIONAL FIELD               
*                                                                               
         CLI   8(R2),C'N'          DEFAULT IS NO                                
         BE    VREC80                                                           
*                                                                               
         LA    RF,8(R2)            START OF INPUT                               
*                                                                               
VREC70LP DS    0H                                                               
*                                                                               
         CLI   0(RF),C','          ALLOW FOR SEPARATORS                         
         BE    *+8                                                              
         CLI   0(RF),C'/'                                                       
         BE    VREC70CN                                                         
*                                                                               
         CLI   0(RF),C'N'          DEFAULT VALUE                                
         BE    VREC70CN                                                         
*                                                                               
         CLI   0(RF),C'Y'                                                       
         BNE   *+12                                                             
         OI    SAVOP1,PROTTRNQ+PROTCHAQ+PROTDELQ+PROTGLBQ  ALL OPTIONS          
         B     VREC70CN                                                         
*                                                                               
         CLI   0(RF),C'T'                                                       
         BNE   *+12                                                             
         OI    SAVOP1,PROTTRNQ     PROTECTED FROM OVERNIGHT TRANSFERS           
         B     VREC70CN                                                         
*                                                                               
         CLI   0(RF),C'G'                                                       
         BNE   *+12                                                             
         OI    SAVOP1,PROTGLBQ     PROTECTED FROM GLOBAL    TRANSFERS           
         B     VREC70CN                                                         
*                                                                               
         CLI   0(RF),C'C'                                                       
         BNE   *+12                                                             
         OI    SAVOP1,PROTCHAQ     PROTECTED FROM OVERNIGHT CHANGES             
         B     VREC70CN                                                         
*                                                                               
         CLI   0(RF),C'D'                                                       
         BNE   *+12                                                             
         OI    SAVOP1,PROTTRNQ     PROTECTED FROM OVERNIGHT DELETES             
         B     VREC70CN                                                         
*                                                                               
         B     ERREND              UNKNOWN OPTION                               
*                                                                               
VREC70CN DS    0H                                                               
*                                                                               
         LA    RF,1(RF)            BUMP INPUT POINTER                           
         BCT   R0,VREC70LP                                                      
*                                                                               
VREC70DN DS    0H                                                               
*                                                                               
*****    OI    PRINTOPT,X'80'                                                   
*                                                                               
VREC80   LA    R2,TITOP2H          PRINT ALL TITLES                             
         MVI   ERROR,2                                                          
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VREC100                                                          
         CLI   8(R2),C'N'          DEFAULT IS NO                                
         BE    VREC100                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         TM    PRINTOPT,X'30'      IF START OR END DATES USED                   
         BZ    VREC85              CAN'T ALSO USE THIS FIELD                    
         MVC   CONHEAD(L'OP2ERR),OP2ERR                                         
         B     MYEND                                                            
VREC85   OI    PRINTOPT,X'08'                                                   
*                                                                               
VREC100  LA    R2,TITOP3H          SUPPRESS MULTIPLE PRINTING OF DPT            
         MVI   ERROR,2                                                          
         MVI   OPTFLAG,0           DEFAULT                                      
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VROP3X                                                           
         CLI   8(R2),C'N'          DEFAULT IS NO                                
         BE    VROP3X                                                           
         MVI   OPTFLAG,1           SET FLAG                                     
         CLI   8(R2),C'Y'                                                       
         BE    VROP3X                                                           
         MVI   OPTFLAG,2           SET FLAG                                     
         CLI   8(R2),C'P'                                                       
         BNE   ERREND              PRIMARY DEMO ONLY                            
*                                                                               
VROP3X   DS    0H                                                               
*                                                                               
VROP4    DS    0H                                                               
*                                                                               
         MVI   SORTOPT,0           INIT SORT OPTION                             
*                                                                               
         LA    R2,TITOP4H          SORT BY DAY/TIMES                            
*                                                                               
         MVI   ERROR,2                                                          
*                                                                               
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VROP4X                                                           
*                                                                               
         CLI   8(R2),C'N'          DEFAULT IS NO                                
         BE    VROP4X                                                           
*                                                                               
         MVI   SORTOPT,C'D'        SET FLAG                                     
*                                                                               
         CLI   8(R2),C'Y'          SORT BY DAY/TIME                             
         BE    VROP4X                                                           
*                                                                               
         B     ERREND                                                           
*                                                                               
VROP4X   DS    0H                                                               
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
* PRINT DATA TYPE REPORT                                                        
*                                                                               
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         L     R1,=A(HOOK)                                                      
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         CLC   STAMP,=CL8'T8190D'  STOP IF STORAGE NOT STAMPED                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   OFFLINE,C'Y'        OFFLINE REPORT?                              
         BNE   ONLIN020            NO  - ONLINE                                 
         L     R2,VADUMMY          OFFLINE DUMMY ASSIGNMENT                     
         B     SETSTACK                                                         
ONLIN020 EQU   *                                                                
         L     R2,=V(DUMMY)                                                     
         A     R2,RELO                                                          
SETSTACK EQU   *                                                                
         ST    R2,ASTACK                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,XTODAY)                                
*                                                                               
         LA    R0,9                                                             
         L     R3,APL1             INIT PRINTING BUFFER                         
*                                                                               
         MVC   0(132,R3),SPACES                                                 
         LA    R3,132(R3)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVC   H7(3),SPACES                                                     
         MVC   H8(3),SPACES                                                     
*                                                                               
         EJECT                                                                  
*              CONTROL THE IO ROUTINES                                          
*              -----------------------                                          
         SPACE 2                                                                
TIT10    LA    R2,DPLIST                                                        
         SR    R3,R3                                                            
         IC    R3,NOREQDPT                                                      
         SPACE 1                                                                
TIT20    CLI   0(R2),0             CONTROL FOR EACH DP                          
         BE    XIT                                                              
         MVC   SVDPT,0(R2)                                                      
         SPACE 1                                                                
         MVC   PAGE,=H'1'          NEW DAYPART                                  
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
         BAS   RE,STKBLD                                                        
         SPACE 1                                                                
         LA    R2,1(R2)                                                         
         BCT   R3,TIT20                                                         
         B     PREPX                                                            
*                                                                               
         TITLE 'T8190D --- RERES0D --- INVENTORY TITLES - STKDPT'               
********************************************************************            
*                                                                  *            
*     RERES0C (T8190D) --- INVENTORY TITLES                        *            
*                                                                  *            
*        BUILD STACK OF DISK ADDRESSES USING DAYPART PASSIVE PTR   *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
         DS    0D                                                               
STKBLD   NTR1  LABEL=*             BUILD A STACK OF D/A                         
*                                                                               
         L     R5,ASTACK           POINT TO START OF D/A STACK                  
         USING STCKD,R5            ESTABLISH ENTRY IN STACK                     
         XC    STCKD(STCKL),STCKD  INIT FIRST ENTRY IN LIST                     
*                                                                               
         SR    R6,R6               INIT STACK ITEM COUNTER                      
*                                                                               
         L     R3,ASTLIST          POINT TO LIST OF STATIONS                    
*                                                                               
STKSTALP DS    0H                                                               
*                                                                               
         USING STLISTD,R3          ESTABLISH ENTRY IN STATION LIST              
*                                                                               
         OC    STLISTD(STLISTL),STLISTD CHECK FOR END OF LIST                   
         BZ    STKSTADN                                                         
*                                                                               
         LA    R4,KEY              ESTABLISH DAYPART PASSIVE PTR                
         USING RIDPKEY,R4                                                       
         XC    KEY,KEY                                                          
*                                                                               
*SMY*    MVI   RIDPKTYP,X'92'      SET PASSIVE PTR ID                           
         MVI   RIDPKTYP,RIDPKTYQ   SET PASSIVE PTR ID                           
         MVC   RIDPKREP,REPPAR     USE PARENT REP                               
         MVC   RIDPKSTA,STLSSTAC   SET ACTIVE STATION                           
         CLI   RIDPKSTA+4,C' '     DEFAULT TO TV IF NO BAND                     
         BH    *+8                                                              
         MVI   RIDPKSTA+4,C'T'                                                  
         MVC   RIDPKDPT,0(R2)      SET CURRENT DAYPART                          
*                                                                               
         GOTO1 HIGH                READ FIRST POINTER                           
*                                                                               
TITSTKLP DS    0H                                                               
*                                                                               
         CLC   RIDPKEY(RIDPKDAY-RIDPKEY),KEYSAVE DONE AT STA CHANGE             
         BNE   TITSTKDN                                                         
*                                                                               
         CLI   RIDPKINV,0          ACCEPT ONLY NEW FORMAT KEY                   
         BE    TITSTKCN                                                         
*                                                                               
*        BUILD ENTRY IN STACK                                                   
*                                                                               
         MVC   STCKINV,RIDPKINV    INVENTORY NUMBER                             
         MVC   STCKSTCD,STLSSTCD   SORT CODE                                    
         MVC   STCKSTAC,STLSSTAC   STATION CALL LETTERS                         
         MVC   STCKDA,KEY+28       DISK ADDRESS                                 
*                                                                               
         CLI   SORTOPT,C'D'        IF DAY/TIME ORDER REQUESTED                  
         BNE   TITSTK20                                                         
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         LR    R0,R6               SAVE ITEM COUNTER                            
*                                                                               
         LA    R6,IO               POINT TO FOUND RECORD                        
         MVI   ELCODE,X'02'        FIND DAY/TIME ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   TITSTK10            NONE FOUND                                   
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         MVC   STCKDYS,RIDTDAY     ADD DAYS TO SORT KEY                         
         XI    STCKDYS,X'FF'       SORT IN DESCENDING ORDER                     
*                                                                               
         MVC   STCKTIME,RIDTTIME   ADD TIME TO SORT KEY                         
*                                                                               
TITSTK10 DS    0H                                                               
*                                                                               
         LR    R6,R0               RESTORE ITEM COUNTER                         
*                                                                               
TITSTK20 DS    0H                                                               
*                                                                               
         LA    R5,STCKL(R5)        NEXT ENTRY IN LIST                           
         XC    STCKD(STCKL),STCKD  INITIALIZE                                   
         LA    R6,1(R6)            BUMP ITEM COUNTER                            
*                                                                               
TITSTKCN DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT PASSIVE                            
*                                                                               
         B     TITSTKLP                                                         
*                                                                               
TITSTKDN DS    0H                                                               
*                                                                               
STKSTACN DS    0H                                                               
*                                                                               
         LA    R3,STLISTL(R3)      BUMP TO NEXT STATION IN LIST                 
         B     STKSTALP                                                         
*                                                                               
STKSTADN DS    0H                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
TIT60    LTR   R6,R6                                                            
         BZ    XIT                                                              
*                                                                               
         GOTO1 XSORT,DMCB,(0,ASTACK),(R6),STCKL,STCKL,0  SORT STACK             
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        LOOP THROUGH DISK ADDRESS STACK AND PRINT DATA FOR EACH                
*                                                                               
         L     R5,ASTACK                                                        
         B     TIT80                                                            
         SPACE 1                                                                
TIT70    LM    R5,R6,SAVESTAK                                                   
         LA    R5,STCKL(R5)                                                     
         BCT   R6,TIT80                                                         
         XIT1                                                                   
TIT80    DS    0H                                                               
*                                                                               
         MVC   KEY+28(4),STCKDA    PUT NEXT DISK ADDR IN KEY                    
*                                                                               
         STM   R5,R6,SAVESTAK      SAVE STACK POINTERS                          
*                                                                               
         MVC   SVSTCKD,STCKD       SAVE STACK ENTRY                             
*                                                                               
         OI    DMINBTS,X'08'       SET PASS DELETED RECORDS                     
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'       UNSET PASS DELETED RECORDS                   
*                                                                               
         MVC   KEY(27),IO                                                       
*                                                                               
         TM    IO+29,X'80'         TEST RECORD IS DELETED                       
         BO    TIT70                                                            
*                                                                               
         MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RINVPEL,R6                                                       
*                                                                               
         CLI   OPTFLAG,0          OPTION-SUPRESS PRNT IN MULT DPTS              
         BE    TIT102                                                           
         ZIC   R1,NOREQDPT        # OF REQ. DAYPARTS                            
         C     R1,=F'1'           NO CHANCE OF DUPLICATE IF ONLY 1 DPT          
         BE    TIT102                                                           
*                                                                               
         BAS   RE,TSTDPT                                                        
         CLI   SKIPREC,0                                                        
         BNE   TIT70               DON'T PRINT IT IN THIS DAYPART               
*                                                                               
TIT102   TM    PRINTOPT,X'40'      ANY FILTER FILTERS                           
         BZ    TIT125                                                           
         SPACE 1                                                                
         ZIC   RE,NUMFILT                                                       
         LA    R5,TITFILT                                                       
         SPACE 1                                                                
TIT105   LA    R1,RINVPFLT                                                      
         LA    R0,6                                                             
         SPACE 1                                                                
TIT110   CLI   0(R5),C'A'          IF ANY FILTER SPECIFIED                      
         BL    TIT123                                                           
TIT115   CLC   0(1,R5),0(R1)       MATCHES ANY FILTER ON RECORD                 
         BE    TIT125              WE WANT IT                                   
         SPACE 1                                                                
TIT120   LA    R1,1(R1)                                                         
         BCT   R0,TIT115                                                        
TIT123   LA    R5,1(R5)                                                         
         BCT   RE,TIT105                                                        
         B     TIT70               NOTHING MATCHES-DON'T WANT IT                
*                                                                               
TIT125   DS    0H                  FILTER ON PROTECTED STATUS IF NEEDED         
*                                                                               
         CLI   SAVOP1,0            SKIP IF NO FLTR ON PROTECTED STATUS          
         BE    TIT130                                                           
*                                                                               
         MVC   BYTE,SAVOP1         MOVE PROTECTION OPTIONS TO WORKAREA          
         NC    BYTE,RINVGPRO       SKIP IF NO OPTION MATCH                      
         BZ    TIT70                                                            
*                                                                               
TIT130   CLC   SDATE(4),=X'0000FFFF' FILTER ON EFFECTIVE DATE(S)                
         BE    TIT136                                                           
         CLC   RINVPEFF+0(2),EDATE IGNORE ANY                                   
         BH    TIT70               STARTING AFTER SELECTED END DATE             
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    TIT140                                                           
         CLC   RINVPEFF+2(2),SDATE IGNORE ANY                                   
         BL    TIT70               ENDING BEFORE SELECTED START DATE            
         B     TIT140                                                           
*                                                                               
TIT136   TM    PRINTOPT,X'08'      PRINT ALL TITLES                             
         BO    TIT140              SO SKIP TODAY TEST                           
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    TIT140                                                           
         CLC   RINVPEFF+2(2),XTODAY      SEE IF IT ENDS BEFORE TODAY            
         BL    TIT70                                                            
         SPACE 1                                                                
TIT140   DS    0H                                                               
*                                                                               
         LA    RF,IO               DETERMINE IF NEW INVENTORY RECORD            
*                                                                               
         GOTO1 =A(HDRNEW),RR=RELO  USE NEW ROUTINE                              
*                                                                               
TIT160   B     TIT70                                                            
*                                                                               
PREPX    B     XIT                                                              
*                                                                               
         TITLE 'RERES0D - GET FIELDS FROM GLOBBER AREA - GETFLDS'               
***********************************************************************         
*                                                                     *         
*        GET FIELDS FROM GLOBBER AREA - GETFLDS                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
GETFLDS  NTR1  LABEL=*                                                          
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',BLOCK1,TTLDATAL,GLRDATA GET DATA          
         GOTO1 VGLOBBER,DMCB,=C'DELE'    DELETE TRANSFERRED DATA                
*                                                                               
         MVI   TRANSSW,0           RESET TRANSFER SWITCH                        
*                                                                               
         LA    R3,BLOCK1           ESTABLISH TRANSFERRED DATA                   
         USING TTLDATAD,R3                                                      
*                                                                               
         SR    RF,RF                                                            
*                                                                               
GFLSTAT  DS    0H                                                               
*                                                                               
         ICM   RF,1,TTLSTNL        GET STATION INPUT LENGTH                     
         BZ    GFLSTATX                                                         
*                                                                               
         CH    RF,=Y(L'TITSTAT)    MASTER FIELD SLIGHTLY LARGER                 
         BNH   *+8                                                              
         LH    RF,=Y(L'TITSTAT)                                                 
*                                                                               
         XC    TITSTAT,TITSTAT     INIT STATION FIELD                           
         STC   RF,TITSTATH+5       SET FIELD LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TITSTAT(0),TTLSTN   DISPLAY STATION                              
*                                                                               
         OI    TITSTATH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
GFLSTATX DS    0H                                                               
*                                                                               
GFLDPT   DS    0H                                                               
*                                                                               
         ICM   RF,1,TTLDPTL        GET DAYPART INPUT LENGTH                     
         BZ    GFLDPTX                                                          
*                                                                               
         XC    TITDPT,TITDPT       INIT DAYPART FIELD                           
         STC   RF,TITDPTH+5        SET FIELD LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TITDPT(0),TTLDPT    DISPLAY DAYPART                              
*                                                                               
         OI    TITDPTH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
GFLDPTX  DS    0H                                                               
*                                                                               
GFLSTRT  DS    0H                                                               
*                                                                               
         ICM   RF,1,TTLESDTL       GET START DATE INPUT LENGTH                  
         BZ    GFLSTRTX                                                         
*                                                                               
         XC    TITSTRT,TITSTRT     INIT START DATE FIELD                        
         STC   RF,TITSTRTH+5       SET FIELD LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TITSTRT(0),TTLESDT  DISPLAY START DATE                           
*                                                                               
         OI    TITSTRTH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
GFLSTRTX DS    0H                                                               
*                                                                               
GFLEND   DS    0H                                                               
*                                                                               
         ICM   RF,1,TTLEEDTL       GET END DATE INPUT LENGTH                    
         BZ    GFLENDX                                                          
*                                                                               
         XC    TITEND,TITEND       INIT END DATE FIELD                          
         STC   RF,TITENDH+5        SET FIELD LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TITEND(0),TTLEEDT   DISPLAY END DATE                             
*                                                                               
         OI    TITENDH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
GFLENDX DS     0H                                                               
*                                                                               
GFLFILT  DS    0H                                                               
*                                                                               
         ICM   RF,1,TTLFTRL        GET FILTER INPUT LENGTH                      
         BZ    GFLFILTX                                                         
*                                                                               
         XC    TITFILT,TITFILT     INIT FILTER FIELD                            
         STC   RF,TITFILTH+5       SET FIELD LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TITFILT(0),TTLFTR   DISPLAY FILTER                               
*                                                                               
         OI    TITFILTH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
GFLFILTX DS    0H                                                               
*                                                                               
GFLOP1   DS    0H                                                               
*                                                                               
         ICM   RF,1,TTLOP3L        GET NO DUPES OPTION INPUT LENGTH             
         BZ    GFLOP1X                                                          
*                                                                               
         XC    TITOP1,TITOP1       INIT NO DUPES OPTION FIELD                   
         STC   RF,TITOP1H+5        SET FIELD LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TITOP1(0),TTLOP3    DISPLAY NO DUPES OPTION                      
*                                                                               
         OI    TITOP1H+6,X'80'     TRANSMIT FIELD                               
*                                                                               
GFLOP1X  DS    0H                                                               
*                                                                               
GETFLDSX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
* TSTDPT -- CHECKS IF PROGRAM HAS BEEN PRINTED ALREADY                          
*           PROGRAMS PRINT IN REQUEST ORDER                                     
         SPACE                                                                  
         DS    0D                                                               
TSTDPT   NTR1  LABEL=*                                                          
         MVI   SKIPREC,0          0=USE RECORD                                  
*                                                                               
         CLI   OPTFLAG,2           IF ONLY TO APPEAR IN PRIMARY DPT             
         BNE   TSTD15                                                           
*                                                                               
         CLC   0(1,R2),RINVDP         THEN CURRENT DPT MUST = PRIMARY           
         BE    *+8                                                              
         MVI   SKIPREC,1              ELSE SET TO SKIP RECORD                   
*                                                                               
         B     XIT                                                              
*                                                                               
TSTD15   DS    0H                                                               
         LA    R1,DPLIST          REQUEST LIST                                  
TSTD20   CLC   0(1,R1),0(R2)      CURRENT DAYPART?                              
         BE    XIT                USE THIS RECORD                               
*                                                                               
         LA    R3,6               # OF DPTS IN RECORD                           
         LA    RE,RINVDP          DAYPARTS CODED FOR PROGRAM                    
TSTD50   CLI   0(RE),C' '         ANY MORE DPTS?                                
         BE    TSTD80             NO                                            
*                                                                               
         CLC   0(1,R1),0(RE)                                                    
         BNE   TSTD70                                                           
         MVI   SKIPREC,1          SKIP RECORD - ALREADY PRINTED                 
         B     XIT                                                              
*                                                                               
TSTD70   LA    RE,1(RE)           CHECK NEXT DPT IN PROGRAM                     
         BCT   R3,TSTD50                                                        
*                                                                               
TSTD80   LA    R1,1(R1)           CHECK NEXT DPT IN REQUEST                     
         B     TSTD20                                                           
         EJECT                                                                  
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
SUBROUTS DS    0D                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
ERREND   GOTO1 VERRXIT                                                          
         SPACE 2                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 2                                                                
         GETEL (R6),34,ELCODE                                                   
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**  MY OWN ERROR MESSAGES                                                       
MANYFLT  DC    C'** ERROR ** TOO MANY FILTERS - LIMIT IS 6'                     
SEDATES  DC    C'** ERROR ** START CANNOT BE LATER THAN END'                    
OP2ERR   DC    C'** ERROR ** ALREADY SELECTED START/END DATES'                  
DPMENUER DC    C'* ERROR * MENU NOT FOUND'                                      
DPINVER  DC    C'* ERROR * DAYPART NOT FOUND'                                   
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0,1                                                              
         PSPEC H1,001,AGYNAME                                                   
         PSPEC H1,059,C'INVENTORY TITLES'                                       
         PSPEC H1,099,REPORT                                                    
         PSPEC H1,115,PAGE                                                      
         PSPEC H2,001,REQUESTOR                                                 
         PSPEC H2,059,16C'-'                                                    
         PSPEC H2,099,RUN                                                       
         PSPEC H5,001,C'DAYPART -'                                              
         SPACE 1                                                                
         SPROG 0                                                                
         PSPEC H7,07,C'INV NO  DAY'                                             
         PSPEC H8,07,C'------  ---'                                             
         PSPEC H7,29,C'TIME          PROGRAM NAME'                              
         PSPEC H8,29,C'----          ------------'                              
         PSPEC H7,73,C'EFFECTIVE DATES'                                         
         PSPEC H8,73,C'---------------'                                         
         PSPEC H7,92,C'DAYPART  FILTERS   PRT'                                  
         PSPEC H8,92,C'-------  -------   ---'                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
* HOOK ROUTINE FOR HEADLINE DETAILS                                             
*                                                                               
         DS    0D                                                               
HOOK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        PRINT STATION ID                                                       
*                                                                               
         MVC   H4(46),SPACES       INIT STATION ID AREA                         
*                                                                               
         LA    R5,SVSTCKD          ESTABLISH CURRENT STACK ENTRY                
         USING STCKD,R5                                                         
*                                                                               
         CLI   NOREQSTA,1          SKIP IF MORE THAN ONE STATION                
         BH    HKSTA5                                                           
*                                                                               
         MVC   H4(9),=C'STATION -'                                              
         MVC   H4+10(4),STCKSTAC   STATION CALL LETTERS                         
*                                                                               
         CLI   STCKSTAC+4,C' '     SKIP IF TV                                   
         BE    *+8                                                              
         CLI   STCKSTAC+4,C'T'                                                  
         BE    *+14                                                             
         MVI   H4+14,C'-'                                                       
         MVC   H4+15(1),STCKSTAC+4 PRINT BAND                                   
*                                                                               
         MVC   H4+17(29),MKTSV     MARKET NAME                                  
*                                                                               
         B     HKSTAX                                                           
*                                                                               
HKSTA5 DS      0H                                                               
*                                                                               
         MVC   H7(3),=C'STN'                                                    
         MVC   H8(3),=C'---'                                                    
*                                                                               
         OC    STMENU,STMENU       SKIP IF NOT A STATION MENU                   
         BZ    HKSTA8                                                           
*                                                                               
         MVC   H4(9),=C'MENU    -'                                              
         MVC   H4+10(4),STMENU     MENU ID                                      
         MVC   H4+17(29),STMENUNM                                               
*                                                                               
         B     HKSTAX                                                           
*                                                                               
HKSTA8 DS      0H                  MULTIPLE STATIONS                            
*                                                                               
         MVC   H4(9),=C'STATIONS-'                                              
         MVC   H4+10(7),SPACES                                                  
         MVC   H4+17(29),=CL29'VARIOUS'                                         
*                                                                               
HKSTAX DS      0H                                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R3,DPTBL            LOOK UP DAYPART                              
         USING DPTBLD,R3           ESTABLISH DAYPART TABLE                      
*                                                                               
HK10     CLC   SVDPT,DPTBCODE      MATCH DAYPART CODE                           
         BE    HK20                                                             
         LA    R3,DPTBLL(R3)                                                    
         OC    DPTBLD(DPTBLL),DPTBLD  CHECK FOR END OF TABLE                    
         BNZ   HK10                                                             
*                                                                               
HK20     MVC   H5+10(L'DPTBCODE),DPTBCODE  DAYPART CODE                         
         MVC   H5+17(L'DPTBLNAM),DPTBLNAM  DAYPART NAME                         
*                                                                               
*  IF START OR END DATE FILTER, SHOW IN HEADLINES                               
*                                                                               
         LA    R3,H4+98                                                         
         TM    PRINTOPT,X'20'      START DATE ENTERED?                          
         BZ    HOOK10                                                           
         MVC   0(7,R3),=C'START -'                                              
         GOTO1 DATCON,DMCB,(2,SDATE),(8,8(R3))                                  
         LA    R3,17(R3)                                                        
*                                                                               
HOOK10   TM    PRINTOPT,X'10'      END DATE ENTERED?                            
         BZ    HOOK30                                                           
         MVC   0(5,R3),=C'END -'                                                
         GOTO1 DATCON,DMCB,(2,EDATE),(8,6(R3))                                  
         SPACE 1                                                                
HOOK30   LA    R3,H5+98                                                         
         TM    PRINTOPT,X'40'      FILTERS ENTERED?                             
         BZ    HOOK40                                                           
         MVC   0(9,R3),=C'FILTERS -'                                            
         LA    R4,TITFILTH                                                      
         ZIC   RE,5(R4)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R3),8(R4)                                                   
         LA    R3,17(R3)                                                        
         SPACE 1                                                                
HOOK40   TM    PRINTOPT,X'80'      NT ONLY                                      
         BZ    HOOKX                                                            
         MVC   0(7,R3),=C'NT ONLY'                                              
         SPACE 2                                                                
HOOKX    B     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         TITLE 'T8190D --- RERES0D --- INVENTORY TITLES - HDRNEW'               
***********************************************************************         
*                                                                     *         
*        PROCESSING HEADER RECORDS                                    *         
*        -------------------------                                    *         
*        R4, ON ENTRY, POINTS TO INVENTORY KEY                        *         
*        R6, ON ENTRY, POINTS TO INVENTORY 01 EL                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
HDRNEW   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,APL1             ESTABLISH FIRST PRINT LINE                   
         USING PONE,R3                                                          
*                                                                               
         USING RINVKEY,R4          ESTABLISH INVENTORY KEY                      
*                                                                               
*        INVENTORY ID                                                           
*                                                                               
         CLI   NOREQSTA,1          SKIP IF MORE THAN ONE STATION                
         BNH   HDNSTAX                                                          
*                                                                               
         LA    R5,SVSTCKD          ESTABLISH CURRENT STACK ENTRY                
         USING STCKD,R5                                                         
*                                                                               
         LR    R1,R3               START OF INVENTORY NO. PRINT AREA            
*                                                                               
         MVC   0(4,R1),STCKSTAC    STATION CALL LETTERS                         
         LA    R1,4(R1)            BUMP PRINT POINTER                           
*                                                                               
         CLI   STCKSTAC+4,C' '     TV SKIPS BAND                                
         BE    *+8                                                              
         CLI   STCKSTAC+4,C' '     TV SKIPS BAND                                
         BE    *+18                                                             
         MVI   0(R1),C'-'          PRINT BAND                                   
         MVC   1(1,R1),STCKSTAC+4                                               
         LA    R1,2(R1)            BUMP PRINT POINTER                           
*                                                                               
         DROP  R5                                                               
*                                                                               
HDNSTAX  DS    0H                                                               
*                                                                               
         ST    R3,SPL1             STARTING PRINT POSITION                      
*                                                                               
         MVC   PINV,RINVKINV       PRINT INVENTORY ID                           
*                                                                               
*        PROGRAM DAY/TIMES                                                      
*                                                                               
*        USE AVAIL DAY/TIMES IF PRESENT IN INVENTORY RECORD                     
*                                                                               
HDNWAV   DS    0H                                                               
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'04'        AVAIL DAY/TIMES USED IF FOUND                
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   HDNWAVN             NONE FOUND                                   
*                                                                               
HDNWAVLP DS    0H                                                               
*                                                                               
         AH    R2,=H'1'            BUMP ELEMENT COUNTER                         
*                                                                               
         USING RIAPELEM,R6         ESTABLISH AVAIL ELEMENT                      
*                                                                               
         MVC   WORK,SPACES         INIT WORKAREA                                
*                                                                               
         MVC   PDAY,RIADAY         AVAIL DAYS                                   
         MVC   PTIM,RIATIME        AVAIL TIMES                                  
*                                                                               
         LA    R3,132(R3)          NEXT PRINT LINE                              
*                                                                               
HDNWAVCN DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           NEXT AVAIL ELEMENT                           
         BE    HDNWAVLP            ONE FOUND                                    
*                                                                               
HDNWAVDN DS    0H                  END OF DAY/TIME ELEMENTS                     
*                                                                               
         LA    R3,SPL1             POINT TO STARTING PRINT LINE                 
*                                                                               
HDNWAVX  DS    0H                                                               
*                                                                               
         B     HDNWDTX                                                          
*                                                                               
HDNWAVN  DS    0H                                                               
*                                                                               
*        PRINT DAYS/TIMES                                                       
*                                                                               
*        PROGRAM DAY/TIMES                                                      
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'02'        SET TO FIND A DAY/TIME ELEMENTS              
*                                                                               
         BAS   RE,GETEL            FIND FIRST DAY/TIME ELEMENT                  
         BNE   HDNWDTX             NONE FOUND - SKIP PRINTING                   
*                                                                               
HDNWDTLP DS    0H                                                               
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         CLC   PDAY,SPACES         SKIP IF AVAILS ALREADY PRINTED               
         BH    HDNWDT10                                                         
*                                                                               
         GOTO1 UNDAY,PARAS,RIDTDAY,PDAY       DAY                               
*                                                                               
         OC    WORK,SPACES                                                      
*                                                                               
HDNWDT10 DS    0H                                                               
*                                                                               
         CLC   PTIM,SPACES         SKIP IF AVAIL TIME ALREADY PRINTED           
         BH    HDNWDT20                                                         
*                                                                               
         GOTO1 UNTIME,PARAS,RIDTTIME,PTIM     TIME                              
*                                                                               
HDNWDT20 DS    0H                                                               
*                                                                               
HDNWDTCN DS    0H                                                               
*                                                                               
         LA    R3,132(R3)          BUMP TO NEXT PRINT LINE                      
         BAS   RE,NEXTEL           NEXT DAY/TME ELEMENT                         
         BE    HDNWDTLP            ONE FOUND                                    
*                                                                               
HDNWDTDN DS    0H                  END OF DAY/TIME ELEMENTS                     
*                                                                               
HDNWDTX  DS    0H                  END OF DAY/TIME ELEMENTS                     
*                                                                               
*        PRINT PROGRAM NAMES                                                    
*                                                                               
         L     R3,SPL1             RESET LINE POINTER TO FIRST                  
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'        FIND A PROGRAM ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   HDNWPGX                                                          
*                                                                               
HDNWPGLP DS    0H                                                               
*                                                                               
         USING RIPGELEM,R6         ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RIPGLEN          ELEMENT LENGTH                               
         SH    RF,=Y(RIPGNAME-RIPGELEM)  PROGRAM NAME LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PPROG(0),RIPGNAME   PRINT PROGRAM NAME                           
*                                                                               
HDNWPGCN DS    0H                                                               
*                                                                               
         LA    R3,132(R3)          BUMP POINTER                                 
         BAS   RE,NEXTEL           NEXT PROGRAM ELEMENT                         
         BE    HDNWPGLP            ONE FOUND                                    
*                                                                               
HDNWPGDN DS    0H                                                               
*                                                                               
HDNWPGX  DS    0H                                                               
*                                                                               
*        EFFECTIVE DATE(S)                                                      
*                                                                               
         L     R3,SPL1             RESET PRINT POINTER                          
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'        FIND HEADER ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   HDNWEFX                                                          
*                                                                               
         USING RINVPEL,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         CLC   RINVPEFF(2),RINVPEFF+2   CHECK FOR SINGLE DATE                   
         BNE   HDNW13                                                           
*                                                                               
         MVC   PDTS(4),=C'ONLY'                                                 
*                                                                               
         GOTO1 DATCON,PARAS,(2,RINVPEFF),(8,PDTS+5)                             
*                                                                               
         B     HDNW16                                                           
*                                                                               
HDNW13   DS    0H                                                               
*                                                                               
         MVC   PDTS(4),=C'FROM'                                                 
*                                                                               
         GOTO1 DATCON,PARAS,(2,RINVPEFF),(8,PDTS+5)                             
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    HDNW16                                                           
*                                                                               
         MVC   PDTS(8),PDTS+5                                                   
         MVC   PDTS+8(56),SPACES                                                
         MVI   PDTS+8,C'-'                                                      
*                                                                               
         GOTO1 DATCON,PARAS,(2,RINVPEFF+2),(8,PDTS+9)                           
*                                                                               
HDNW16   DS    0H                                                               
*                                                                               
HDNWEFX  DS    0H                                                               
*                                                                               
         MVC   PDPT,RINVDP         -DAYPART CODE(S)-                            
*                                                                               
         MVC   PFLT,RINVPFLT       FILTERS                                      
*                                                                               
         LA    RF,PTRNSFR          START OF PROTECTED PRINT AREA                
*                                                                               
         TM    RINVGPRO,X'80'      TRANSFER PROTECTED                           
         BNO   *+14                                                             
         MVC   0(4,RF),=C'TRN,'                                                 
         LA    RF,4(RF)               BUMP PRINT POINTER                        
*                                                                               
         TM    RINVGPRO,X'40'      CHANGE   PROTECTED                           
         BNO   *+14                                                             
         MVC   0(4,RF),=C'PRJ,'                                                 
         LA    RF,4(RF)               BUMP PRINT POINTER                        
*                                                                               
         TM    RINVGPRO,X'20'      DELETE   PROTECTED                           
         BNO   *+14                                                             
         MVC   0(4,RF),=C'DEL,'                                                 
         LA    RF,4(RF)               BUMP PRINT POINTER                        
*                                                                               
         BCTR  RF,0                ELIMINATE TRAILING COMMA                     
         CLI   0(RF),C','                                                       
         BNE   *+8                                                              
         MVI   0(RF),C' '                                                       
*                                                                               
*        PRINT DETAIL LINES - THERE COULD BE 8                                  
*                                                                               
         L     R3,APL1             START OF PRINT LINES                         
*                                                                               
HDNWPR1L DS    0H                                                               
*                                                                               
         LA    R2,P                START OF PRINT AREA (4 LINES WORTH)          
         LA    R0,4                MAX 4 LINES AT A TIME                        
*                                                                               
HDNWPR2L DS    0H                                                               
*                                                                               
         MVC   0(132,R2),0(R3)                                                  
         MVC   0(132,R3),SPACES                                                 
*                                                                               
HDNWPR2C DS    0H                                                               
*                                                                               
         LA    R2,132(R2)                                                       
         LA    R3,132(R3)                                                       
         BCT   R0,HDNWPR2L                                                      
*                                                                               
HDNWPR2D DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,PARAS,(R8)   PRINT THE DATA                                
*                                                                               
HDNWPR1C DS    0H                                                               
*                                                                               
         CLC   0(132,R3),SPACES    DONE IF ONLY SPACES TO PRINT                 
         BH    HDNWPR1L            THERE IS EXTRA LINE TO STOP LOOP             
*                                                                               
         GOTO1 SPOOL,PARAS,(R8)   SPACING LINE                                  
*                                                                               
HDRNEWX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8190D --- RERES0C --- INVENTORY TITLES - VALSTA'               
********************************************************************            
*                                                                  *            
*     RERES0D (T8190D) --- INVENTORY TITLES                        *            
*                                                                  *            
*        VALIDATE STATION                                          *            
*                                                                  *            
*        LIST OF STATIONS OR A MENU ID DESIGNATED AS M=XXXX        *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
         DS    0D                                                               
VALSTA   NTR1  BASE=*,LABEL=*                                                   
*                                  **********************************           
*                                  *                                *           
*                                  *           STATION              *           
*                                  *                                *           
*                                  **********************************           
*                                                                               
         USING GEND,RC                                                          
         USING SUBROUTS,RA                                                      
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
*                                                                               
         XC    STMENU,STMENU       INIT STATION MENU CODE                       
         XC    STMENUNM,STMENUNM   INIT STATION MENU NAME                       
*                                                                               
         L     R5,ASTLIST          ESTABLISH STATION LIST                       
         USING STLISTD,R5                                                       
         XC    STLISTD(STLISTL),STLISTD   INIT FIRST ENTRY IN LIST              
*                                                                               
         LA    R2,TITSTATH         POINT TO STATION INPUT FIELD                 
*                                                                               
         GOTO1 ANY                 INPUT REQUIRED                               
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(16,APL1),0,0 SCAN INPUT                       
*                                                                               
         MVC   ACTUAL,DMCB+4       SAVE NUMBER OF ENTRIES                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,ACTUAL           NUMBER OF ENTRIES IN FIELD                   
*                                                                               
         L     R4,APL1             START OF SCAN BLOCK ENTRIES                  
*                                                                               
*        IF ENTRY IS 'M' THEN MUST BE A MENU ID                                 
*                                                                               
         CLI   0(R4),1             IF ENTRY IS 1 LONG                           
         BNE   VSTAMNUN                                                         
         CLI   12(R4),C'M'         AND MENU INDICATED                           
         BNE   VSTAMNUN                                                         
*                                                                               
         CLI   ACTUAL,1            MAX 1 SCANNED ENTRY ALLOWED                  
         BH    VSTAMN1E                                                         
*                                                                               
         CLI   1(R4),4             MENU ID MAX 4 LONG                           
         BH    VSTAMNXE                                                         
*                                                                               
         MVC   STMENU,22(R4)       SAVE MENU CODE                               
         OC    STMENU,SPACES       SPACE FILL                                   
*                                                                               
*        READ MARKET STATIONS LIST                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY              ESTABLISH SET RECORD KEY                     
         USING RSETKEY,R3                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ   SET AS SET RECORD RECORD                     
         MVC   RSETKREP,REPPAR     SET REP CODE                                 
         MVC   RSETKSET,=C'MS'     SET RECORD ID                                
         MVC   RSETKID,STMENU      SET SET IDENTIFIER                           
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   VSTAMNNF                                                         
*                                                                               
         GOTO1 GETREC              READ IN SET RECORD                           
*                                                                               
         SR    RF,RF                                                            
         LA    R6,IO                                                            
         LA    R6,RSETELEM-RSETREC(R6) FIRST ELEMENT                            
*                                                                               
         USING RSETDESD,R6         ESTABLISH DESCRIPTIVE ELEMENT                
*                                                                               
         CLI   RSETDCDE,0          CHECK FOR END OF RECORD                      
         BE    VSTAMN10               SKIP IF ELEMENT NOT FOUND                 
         CLI   RSETDCDE,RSETDCDQ   LOOKING FOR DESCRIPTIVE ELEMENT              
         BE    *+16                                                             
         IC    RF,RSETDELN         GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)                                                      
         B     *-24                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETDELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETDOV)      DECREMENT BY OVERHEAD LENGTH                 
         BNP   VSTAMN10            IGNORE IF NOT THERE                          
*                                                                               
         MVC   STMENUNM,SPACES     INIT DESCRIPTION                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   STMENUNM(0),RSETDESC SAVE MENU NAME                              
*                                                                               
VSTAMN10 DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         LA    R6,IO                                                            
         LA    R6,RSETELEM-RSETREC(R6) FIRST ELEMENT                            
*                                                                               
         USING RSETMEMD,R6         ESTABLISH DESCRIPTIVE ELEMENT                
*                                                                               
         CLI   RSETMCDE,0          CHECK FOR END OF RECORD                      
         BE    VSTAMNVD               MUST FIND ELEMENT                         
         CLI   RSETMCDE,RSETMCDQ   LOOKING FOR MEMBERS ELEMENT                  
         BE    *+16                                                             
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)                                                      
         B     *-24                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETMTOV)     DECREMENT BY OVERHEAD LENGTH                 
         BNP   VSTAMNVD            MUST HAVE SOME MEMBERS                       
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'5'            CALCULATE NUMBER OF STATIONS IN LIST         
*                                                                               
         STC   RF,NOREQSTA         SET NUMBER OF STATIONS                       
         SR    R0,R0               INIT STATION SORT ORDER                      
*                                                                               
         LA    R1,RSETMEMB         POINT TO FIRST STATION IN LIST               
*                                                                               
VSTAMNLP DS    0H                                                               
*                                                                               
         MVC   STLSSTAC,0(R1)      SAVE STATION CALL LETTERS                    
         STC   R0,STLSSTCD         SET SORT ORDER                               
*                                                                               
VSTAMNCN DS    0H                                                               
*                                                                               
         LA    R1,5(R1)            BUMP TO NEXT STATION                         
         LA    R5,STLISTL(R5)      BUMP TO NEXT STATION AREA                    
         XC    STLISTD(STLISTL),STLISTD   INIT NEXT ENTRY IN LIST               
         AH    R0,=H'1'            BUMP SORT ORDER ID                           
*                                                                               
         BCT   RF,VSTAMNLP                                                      
*                                                                               
VSTAMNDN DS    0H                                                               
*                                                                               
         B     VSTASTAX            END OF MENU LIST                             
*                                                                               
VSTAMNUN DS    0H                                                               
*                                                                               
*        BUILD LIST OF INDIVIDUALLY ENTERED STATIONS                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,ACTUAL           NUMBER OF ENTRIES                            
         STC   RE,NOREQSTA         NUMBER OF REQUESTED STATIONS                 
*                                                                               
         SR    R0,R0               INIT STATION SORT ORDER                      
         SR    RF,RF                                                            
*                                                                               
VSTASTLP DS    0H                                                               
*                                                                               
         OC    STLSSTAC,SPACES     INIT STATION CALL LETTERS                    
         ICM   RF,1,0(R4)          ENTRY LENGTH                                 
         BZ    VSTASTNE            ENTRY REQUIRED                               
*                                                                               
         LA    R3,12-1(RF,R4)      POINT TO LAST OF STATION ID                  
*                                                                               
         CLI   0(R3),C'-'          FIND '-'                                     
         BE    *+18                                                             
         BCTR  R3,0                BACK UP A CHARACTER                          
         BCT   RF,*-10                                                          
         IC    RF,0(R4)            USE FULL ID LENGTH                           
         B     *+6                                                              
*                                                                               
         BCTR  RF,0                RECTIFY CALL LETTERS LENGTH                  
*                                                                               
         CH    RF,=H'4'            MAX 4 CHARACTERS FOR CALL LETTERS            
         BH    VSTASTXE                                                         
*                                                                               
         MVC   STLSSTAC,SPACES     INIT CALL LETTERS                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   STLSSTAC(0),12(R4)  SAVE STATION CALL LETTERS                    
*                                                                               
         MVI   STLSSTAC+4,C'T'     ASSUME TV                                    
*                                                                               
         CLI   0(R3),C'-'          IF THERE IS A BAND ENTERED                   
         BNE   *+10                                                             
         MVC   STLSSTAC+4(1),1(R3)    ADD IT TO CALL LETTERS                    
*                                                                               
         STC   R0,STLSSTCD         SET SORT ORDER                               
*                                                                               
VSTASTCN DS    0H                                                               
*                                                                               
         LA    R4,32(R4)           POINT TO NEXT STATION                        
         LA    R5,STLISTL(R5)      BUMP TO NEXT STATION AREA                    
         XC    STLISTD(STLISTL),STLISTD   INIT NEXT ENTRY IN LIST               
         AH    R0,=H'1'            BUMP SORT ORDER ID                           
*                                                                               
         BCT   RE,VSTASTLP                                                      
*                                                                               
VSTASTDN DS    0H                                                               
*                                                                               
VSTASTAX DS    0H                                                               
*                                                                               
*        VALIDATE STATIONS IN LIST                                              
*                                                                               
         L     R5,ASTLIST          LIST OF STATIONS TO BE VALIDATED             
         XC    MKTSV(20),MKTSV     INIT MARKET NAME SAVEAREA                    
*                                                                               
VSTAVALL DS    0H                                                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD CHECK FOR END OF LIST                   
         BZ    VSTAVALD                                                         
*                                                                               
*        READ STATION FILE TO VALIDATE STATION                                  
*                                                                               
         XC    KEY,KEY             ESTABLISH STATION RECORD KEY                 
         LA    R3,KEY                                                           
         USING RSTAKEY,R3                                                       
*                                                                               
         MVI   RSTAKTYP,X'02'      RECORD TYPE                                  
         MVC   RSTAKREP,AGENCY     REP ID                                       
         MVC   RSTAKSTA,STLSSTAC   STATION                                      
*                                                                               
         CLI   RSTAKSTA+4,C'T'     MEDIA IS BLANK FOR ANY TV STATION            
         BE    VSTASTE5                                                         
         CLI   RSTAKSTA+4,X'F0'    IE. MEDIA= T,1-9                             
         BL    VSTASTE9                                                         
         CLI   RSTAKSTA+4,X'F9'                                                 
         BH    VSTASTE9                                                         
*                                                                               
VSTASTE5 DS    0H                                                               
*                                                                               
         MVI   RSTAKSTA+4,C' '                                                  
*                                                                               
VSTASTE9 DS    0H                                                               
*                                                                               
         GOTO1 HIGH                READ FOR STATION POINTER                     
*                                                                               
         CLC   RSTAKEY,KEYSAVE     MUST FIND STATION                            
         BNE   VSTASTNV                                                         
*                                                                               
*        GET MARKET NAME FROM STATION RECORD                                    
*                                                                               
         OC    MKTSV(20),MKTSV     SKIP IF NOT FIRST STATION IN LIST            
         BNZ   VSTAMKTX                                                         
*                                                                               
         GOTO1 GETREC              READ IN STATION RECORD                       
*                                                                               
         SR    RF,RF                                                            
         LA    R6,IO                                                            
         LA    R6,RSTAELEM-RSTAREC(R6) FIRST ELEMENT                            
*                                                                               
         USING RSTAELEM,R6         ESTABLISH STATION ELEMENT                    
*                                                                               
         CLI   RSTACODE,0          CHECK FOR END OF RECORD                      
         BE    VSTAMKTX               SKIP IF ELEMENT NOT FOUND                 
         CLI   RSTACODE,X'01'      LOOKING FOR STATION ELEMENT                  
         BE    *+16                                                             
         IC    RF,RSTAELLN         GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)                                                      
         B     *-24                                                             
*                                                                               
         MVC   MKTSV(20),2(R6)     SAVE MARKET NAME                             
*                                                                               
VSTAMKTX DS    0H                                                               
*                                                                               
VSTAVALC DS    0H                                                               
*                                                                               
         LA    R5,STLISTL(R5)      NEXT STATION IN LIST                         
         B     VSTAVALL                                                         
*                                                                               
VSTAVALD DS    0H                                                               
*                                                                               
         B     VALSTAX                                                          
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VSTASTNV DS    0H                  STATION NOT ON FILE                          
*                                                                               
         MVC   CONHEAD(L'VSTASTNM),VSTASTNM                                     
*                                                                               
         B     VSTAERR                                                          
*                                                                               
VSTASTNM DC    C'** ERROR ** STATION NOT ON FILE'                               
*                                                                               
VSTASTXE DS    0H                  STATION ID TOO LONG                          
*                                                                               
         MVC   CONHEAD(L'VSTASTXM),VSTASTXM                                     
*                                                                               
         B     VSTAERR                                                          
*                                                                               
VSTASTXM DC    C'** ERROR ** STATION CALL LETTERS ARE 3-4 LONG'                 
*                                                                               
VSTASTNE DS    0H                  STATION ENTRY NEEDED                         
*                                                                               
         MVC   CONHEAD(L'VSTASTEM),VSTASTEM                                     
*                                                                               
         B     VSTAERR                                                          
*                                                                               
VSTASTEM DC    C'** ERROR ** STATION CALL LETTERS REQUIRED'                     
*                                                                               
VSTAMNVD DS    0H                  EMPTY MENU                                   
*                                                                               
         MVC   CONHEAD(L'VSTAMNVM),VSTAMNVM                                     
*                                                                               
         B     VSTAERR1                                                         
*                                                                               
VSTAMNVM DC    C'** ERROR ** NO STATIONS IN MENU'                               
*                                                                               
VSTAMNNF DS    0H                  MENU NOT FOUND                               
*                                                                               
         MVC   CONHEAD(L'VSTAMNFM),VSTAMNFM                                     
*                                                                               
         B     VSTAERR1                                                         
*                                                                               
VSTAMNFM DC    C'** ERROR ** MENU RECORD NOT ON FILE'                           
*                                                                               
VSTAMNXE DS    0H                  MENU ID MAX 4 LONG                           
*                                                                               
         MVC   CONHEAD(L'VSTAMNXM),VSTAMNXM                                     
*                                                                               
         B     VSTAERR1                                                         
*                                                                               
VSTAMNXM DC    C'** ERROR ** MENU ID MUST BE 1-4 CHARACTERS'                    
*                                                                               
VSTAMNNE DS    0H                  NO MENU ID                                   
*                                                                               
         MVC   CONHEAD(L'VSTAMNNM),VSTAMNNM                                     
*                                                                               
         B     VSTAERR1                                                         
*                                                                               
VSTAMNNM DC    C'** ERROR ** MENU ID MUST BE SUPPLIED'                          
*                                                                               
VSTAMN1E DS    0H                  AT MOST ONE MENU                             
*                                                                               
         MVC   CONHEAD(L'VSTAMAXM),VSTAMAXM                                     
*                                                                               
         B     VSTAERR1                                                         
*                                                                               
VSTAMAXM DC    C'** ERROR ** USE MENU ID OR STATION LIST.'                      
*                                                                               
VSTAERR  DS    0H                                                               
*                                                                               
         OC    STMENU,STMENU       SKIP IF VALIDATING A MENU                    
         BNZ   VSTAERR1                                                         
*                                                                               
         ST    R2,FADDR            A(FIELD IN ERROR)                            
*                                                                               
         L     RF,ASTLIST          START OF STATIONS                            
         SR    R5,RF                                                            
         LR    RF,R5                                                            
         SR    RE,RE                                                            
         D     RE,=A(STLISTL)      RELATIVE NUMBER OF CURRENT STATION           
*                                                                               
         LA    RF,1(RF)            ABSOLUTE NUMBER                              
         STC   RF,FADDR            SET ITEM NUMBER                              
*                                                                               
         MVI   ERROR,SUPPLIED      ERROR MESSAGE SUPPLIED                       
*                                                                               
         GOTO1 VMYCURS                                                          
*                                                                               
VSTAERR1 DS    0H                                                               
*                                                                               
         MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VERRXIT                                                          
*                                                                               
SRCTBL   DC    C'NSA'              ARB, NSI, SRC                                
         DC    X'FF'                                                            
*                                                                               
VALSTAX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*  PRINT LINE DSECT                                                             
         SPACE 2                                                                
P1D      DSECT                                                                  
PONE     DS    0CL132                                                           
PSTA     DS    CL6                 STATION                                      
         DS    CL1                                                              
PINV     DS    CL4                 INVENTORY NUMBER                             
         DS    CL3                                                              
PDAY     DS    CL11                DAY                                          
         DS    CL3                                                              
PTIM     DS    CL11                TIME                                         
         DS    CL3                                                              
PPROG    DS    CL27                PROGRAM NAME                                 
         DS    CL3                                                              
PDTS     DS    CL17                EFFECTIVE DATES                              
         DS    CL3                                                              
PDPT     DS    CL6                 DAYPART                                      
         DS    CL3                                                              
PFLT     DS    CL6                 FILTERS                                      
         DS    CL3                                                              
PTRNSFR  DS    CL11                TRANSFER                                     
         DS    CL11                                                             
         TITLE 'T8190D --- RERES0D --- INVENTORY TITLES-TTLDATAD'               
***********************************************************************         
*                                                                     *         
*        DSECT FOR DATA PASSED TO TITLES SCREEN                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TTLDATAD DSECT                                                                  
TTLSTNL  DS    XL1          STATION LENGTH                                      
TTLSTN   DS    CL65         STATION                                             
TTLDPTL  DS    XL1          DAYPART LENGTH                                      
TTLDPT   DS    CL19         DAYPART                                             
TTLESDTL DS    XL1          EFFECTIVE START DATE LENGTH                         
TTLESDT  DS    CL9          EFFECTIVE START DATE                                
TTLEEDTL DS    XL1          EFFECTIVE END DATE LENGTH                           
TTLEEDT  DS    CL9          EFFECTIVE END   DATE                                
TTLFTRL  DS    XL1          FILTERS LENGTH                                      
TTLFTR   DS    CL7          FILTERS                                             
TTLOP3L  DS    XL1          NO DUPLICATES OPTION LENGTH                         
TTLOP3   DS    CL2          NO DUPLICATES OPTION                                
*                                                                               
TTLDATAL EQU   *-TTLDATAD          LENGTH OF PASSED DATA                        
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RERESFDD                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE            LOCAL WORKING STORAGE                        
SAVESTAK DS    2F                                                               
         DS    A                   SPARE                                        
XTODAY   DS    XL2                 TODAY'S DATE (COMPRESSED)                    
REPPAR   DS    CL2                 PARENT REP                                   
SDATE    DS    CL2                 START DATE (COMPRESSED)                      
EDATE    DS    CL2                 END DATE (COMPRESSED)                        
DPLIST   DS    CL20                DAYPART LIST                                 
         DS    CL1                                                              
NUMFILT  DS    XL1                 NUMBER OF FILTER CODES                       
PRINTOPT DS    XL1                                                              
*                                  X'40'  FILTERS REQUESTED                     
*                                  X'20'  START DATE REQUESTED                  
*                                  X'10'  END DATE REQUESTED                    
*                                  X'08'  PRINT ALL TITLES                      
OPTFLAG  DS    XL1                 FLAG FOR OP3                                 
SKIPREC  DS    XL1                 FLAG SKIP RECORD OR NOT                      
NOREQDPT DS    XL1                 NO OF REQUESTED DAYPARTS                     
NOREQSTA DS    XL1                 NO OF REQUESTED STATIONS                     
*                                                                               
SAVOP1   DS    XL1                 PROTECTED ONLY OPTIONS                       
PROTTRNQ EQU   X'80'               TRANSFER PROTECTED                           
PROTCHAQ EQU   X'40'               CHANGE   PROTECTED                           
PROTDELQ EQU   X'20'               DELETION PROTECTED                           
PROTGLBQ EQU   X'10'               GLOBAL   PROTECTED                           
*                                                                               
DPMENU   DS    CL4                 DAYPART MENU CODE                            
DPMENUNM DS    CL20                DAYPART MENU NAME                            
*                                                                               
STMENU   DS    CL4                 STATION MENU CODE                            
STMENUNM DS    CL60                STATION MENU NAME                            
*                                                                               
SORTOPT  DS    XL1                 SORT OPTION                                  
*                                  C'D' - SORT IN DAYS/TIMES ORDER              
*                                                                               
SVSTCKD  DS    XL(STCKL)           CURRENT STACK ENTRY                          
*                                                                               
STAMP    DS    CL8                 WORKING STORAGE STAMP                        
*                                                                               
SAVELN   EQU   *-SYSSPARE          SAVED AREA LENGTH                            
*                                                                               
MYBASE   DS    A                                                                
RELO     DS    A                                                                
*                                                                               
ASTACK   DS    A                                                                
ASTLIST  DS    A                   A(STLIST)                                    
APL1     DS    A                   A(PRINT LINES)                               
SPL1     DS    A                   A(START OF PRINTING)                         
*                                                                               
DPTBL    DS    XL(24*DPTBLL)       DAYPART TABLE                                
*                                                                               
STLIST   DS    XL(24*STLISTL)      STATIONS LIST                                
*                                                                               
PL1      DS    9CL132              9 DETAIL PRINT LINES BUFFER                  
*                                                                               
         TITLE 'T8190D --- RERES0D --- INVENTORY TITLES-DPTBL'                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DAYPART CODES AND NAMES                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTBLD   DSECT                                                                  
DPTBCODE DS    CL(L'RRDPCODE)      DAYPART CODE                                 
DPTBSNAM DS    CL(L'RRDPSNAM)      DAYPART SHORT NAME                           
DPTBLNAM DS    CL(L'RRDPLNAM)      DAYPART LONG NAME                            
DPTBLL   EQU   *-DPTBLD            LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'T8190D --- RERES0D --- INVENTORY TITLES-STACKD'                 
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF INVENTORY DISK ADDRESSES                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STCKD    DSECT                                                                  
STCKDYS  DS    XL1                 DAYS CODE                                    
STCKTIME DS    XL4                 PROGRAM TIMES                                
STCKINV  DS    CL4                 INVENTORY NUMBER                             
STCKSTCD DS    CL1                 STATION SORT CODE                            
STCKSTAC DS    CL5                 STATION CALL LETTERS                         
STCKDA   DS    CL4                 INVENTORY RECORD DISK ADDRESS                
STCKL    EQU   *-STCKD             LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'T8190D --- RERES0D --- INVENTORY TITLES-STLISTD'                
***********************************************************************         
*                                                                     *         
*        DSECT FOR LIST OF STATIONS                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STLISTD  DSECT                                                                  
STLSSTCD DS    CL1                 STATION SORT CODE                            
STLSSTAC DS    CL5                 STATION CALL LETTERS                         
STLISTL  EQU   *-STLISTD           LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'T8190D --- RERES0D --- INVENTORY TITLES-INCLUDES'               
*                                                                               
RRDPRECD DSECT                                                                  
       ++INCLUDE REGENRDP                                                       
RSETRECD DSECT                                                                  
       ++INCLUDE REGENSET                                                       
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
*                                                                               
       ++INCLUDE FATIOB                                                         
         SPACE 2                                                                
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
STACK    CSECT                                                                  
         DS    D                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012RERES0D   04/20/09'                                      
         END                                                                    
