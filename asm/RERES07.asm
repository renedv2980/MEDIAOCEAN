*          DATA SET RERES07    AT LEVEL 059 AS OF 04/24/09                      
*PHASE T81907C                                                                  
         TITLE 'T81907 - RERES07 - REP RESEARCH TITLES REPORT'                  
*                                                                               
**********************************************************************          
*                                                                    *          
*     RERES07  (T81907)  -- REP RESEARCH TITLES LISTING REPORT       *          
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
* APR24/09 (SMY) --->SUPPORT NEW INVENTORY KEY                       *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
         SPACE                                                                  
T81907   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1907**,RA,RR=R2                                              
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
         LH    RF,=Y(BUFF-SYSD)                                                 
         LA    RF,SYSD(RF)                                                      
         ST    RF,SBUFF                                                         
*                                                                               
         CLC   AGENCY(2),=C'NB'                                                 
         BNE   MAIN10                                                           
         L     RF,=A(DPTBL2)                                                    
         AR    RF,R2               RELO STILL IN R2                             
         ST    RF,ADPTBL                                                        
         B     MAIN20                                                           
MAIN10   EQU   *                                                                
         L     RF,=A(DPTBL1)                                                    
         AR    RF,R2               RELO STILL IN R2                             
         ST    RF,ADPTBL                                                        
MAIN20   EQU   *                                                                
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
*                                                                               
VKEY     MVI   PRINTOPT,0          CLEAR OUT PRINTOPT                           
         XC    SYSSPARE(SAVELN),SYSSPARE   INIT SAVEAREA                        
         MVC   STAMP,=CL8'T81907'  STAMP SAVEAREA                               
         SPACE 1                                                                
         LA    R2,TITSTATH         VALIDATE STATION                             
         MVC   TITMKT,SPACES                                                    
         OI    TITMKTH+6,X'80'     TRANSMIT FIELD                               
         SPACE 1                                                                
         MVI   STATSW,C'I'         STATION SWITCH - ALLOW INVALID STA           
         GOTO1 VVALSTA                                                          
         CLI   STATSW,C'I'         WAS STATION INVALID                          
         BE    VKEY05                                                           
         SPACE 1                                                                
         GOTO1 VVALMKT                                                          
**       MVC   MKTSV(29),WORK+8    SAVE MARKET NAME FOR RPT HEADLINES           
**       MVC   TITMKT(24),WORK+8   AND PUT IT TO SCREEN                         
* GET IT NOW FROM STATION RECORD                                                
VKEY05   EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),ACTSTAT                                                
         CLI   KEY+26,C'T'                                                      
         BE    STA05                                                            
         CLI   KEY+26,X'F0'                                                     
         BL    STA10                                                            
         CLI   KEY+26,X'F9'                                                     
         BH    STA10                                                            
STA05    MVI   KEY+26,C' '                                                      
STA10    GOTO1 HIGH                                                             
         MVI   ERROR,112          STATION RECORD NOT ON FILE                    
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   VKEY10                                                           
         MVC   MKTSV(20),2(R6)    MARKET NAME                                   
         MVC   TITMKT(20),2(R6)    AND PUT IT TO SCREEN                         
         SPACE 1                                                                
VKEY10   LA    R2,TITDPTH          VALIDATE DAYPART                             
         XC    DPLIST,DPLIST                                                    
         GOTO1 ANY                 REQUIRED FIELD                               
         SPACE 1                                                                
         CLC   8(3,R2),=C'ALL'     ALL = PREDETERMINED LIST OF DPTS             
         BNE   VKEY15                                                           
         MVC   DPLIST(16),=C'MDERATLWKFNPVSJO'                                  
         B     VKEY40                                                           
         SPACE 1                                                                
VKEY15   MVI   ERROR,2             INVALID INPUT FIELD                          
         LA    R5,8(R2)                                                         
         ZIC   R6,5(R2)                                                         
         STC   R6,NOREQDPT         # OF REQUESTED DAYPARTS                      
VKEY20   L     R4,ADPTBL           MAKE SURE INPUT IS IN TABLE                  
VKEY30   CLI   0(R4),X'FF'                                                      
         BE    ERREND                                                           
         CLC   0(1,R5),0(R4)                                                    
         BE    VKEY35                                                           
         LA    R4,L'DPTBL1(R4)                                                  
         B     VKEY30                                                           
         SPACE 1                                                                
VKEY35   LA    R5,1(R5)                                                         
         BCT   R6,VKEY20                                                        
         SPACE 1                                                                
         ZIC   R6,5(R2)            SAVE INPUT IN DPLIST                         
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     VKEY40                                                           
         MVC   DPLIST(0),8(R2)                                                  
         SPACE 1                                                                
VKEY40   LA    R2,TITSTRTH         START DATE                                   
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VKEY50                                                           
         OI    PRINTOPT,X'20'      INDICATE START DATE USED                     
         GOTO1 VVALDATE                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,SDATE)  SDATE=2 BYTE COMPRESSED          
         SPACE 1                                                                
VKEY50   LA    R2,TITENDH          END DATE                                     
         MVC   EDATE,=X'FFFF'                                                   
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VKEY60                                                           
         OI    PRINTOPT,X'10'      INDICATE END DATE USED                       
         GOTO1 VVALDATE                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,EDATE)   EDATE=2 BYTE COMPRESSED         
         SPACE 1                                                                
         CLC   SDATE,EDATE         START CAN'T BE GREATER THAN END              
         BNH   VKEY60                                                           
         MVC   CONHEAD(L'SEDATES),SEDATES                                       
         B     MYEND                                                            
         SPACE 1                                                                
VKEY60   LA    R2,TITFILTH         FILTERS                                      
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VKEY70                                                           
         OI    PRINTOPT,X'40'      INDICATE FILTERS USED                        
         MVC   NUMFILT,5(R2)       SAVE NUMBER OF FILTERS                       
         CLI   5(R2),6                                                          
         BNH   VKEY70              MAXIMUM OF 6 ALLOWED                         
         MVC   CONHEAD(L'MANYFLT),MANYFLT                                       
         B     MYEND                                                            
         SPACE 1                                                                
VKEY70   LA    R2,TITOP1H          NT ONLY                                      
         MVI   ERROR,2                                                          
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VKEY80                                                           
         CLI   8(R2),C'N'          DEFAULT IS NO                                
         BE    VKEY80                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         OI    PRINTOPT,X'80'                                                   
         SPACE 1                                                                
VKEY80   LA    R2,TITOP2H          PRINT ALL TITLES                             
         MVI   ERROR,2                                                          
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VKEY100                                                          
         CLI   8(R2),C'N'          DEFAULT IS NO                                
         BE    VKEY100                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         TM    PRINTOPT,X'30'      IF START OR END DATES USED                   
         BZ    VKEY85              CAN'T ALSO USE THIS FIELD                    
         MVC   CONHEAD(L'OP2ERR),OP2ERR                                         
         B     MYEND                                                            
VKEY85   OI    PRINTOPT,X'08'                                                   
         SPACE 1                                                                
VKEY100  LA    R2,TITOP3H          SUPPRESS MULTIPLE PRINTING OF DPT            
         MVI   OPTFLAG,0           DEFAULT                                      
         MVI   ERROR,2                                                          
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VKEYX                                                            
         CLI   8(R2),C'N'          DEFAULT IS NO                                
         BE    VKEYX                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         MVI   OPTFLAG,1           SET FLAG                                     
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         EJECT                                                                  
* PRINT DATA TYPE REPORT                                                        
*                                                                               
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         CLC   STAMP,=CL8'T81907'  STOP IF STORAGE NOT STAMPED                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RCSUBPRG,0                                                       
         L     R2,=A(STACK)                                                     
         A     R2,RELO                                                          
         ST    R2,ASTACK                                                        
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,XTODAY)                                
         SPACE 1                                                                
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
         DROP  R6                                                               
         EJECT                                                                  
*              CONTROL THE IO ROUTINES                                          
*              -----------------------                                          
         SPACE 2                                                                
TIT10    LA    R2,DPLIST                                                        
         LA    R3,NDPT                                                          
         SPACE 1                                                                
TIT20    CLI   0(R2),0             CONTROL FOR EACH DP                          
         BE    XIT                                                              
         MVC   SVDPT,0(R2)                                                      
         SPACE 1                                                                
         MVC   PAGE,=H'1'          NEW DAYPART                                  
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
         BAS   RE,TIT30                                                         
         SPACE 1                                                                
         LA    R2,1(R2)                                                         
         BCT   R3,TIT20                                                         
         B     PREPX                                                            
         EJECT                                                                  
TIT30    NTR1                      BUILD A STACK OF D/A                         
         L     R5,ASTACK                                                        
         SR    R6,R6                                                            
         LA    R4,KEY                                                           
         USING RIDPKEY,R4                                                       
         XC    KEY,KEY                                                          
*SMY*    MVI   RIDPKTYP,X'92'                                                   
         MVI   RIDPKTYP,RIDPKTYQ                                                
         MVC   RIDPKREP,REPPAR     USE PARENT REP                               
         MVC   RIDPKSTA,ACTSTAT                                                 
         MVC   RIDPKDPT,0(R2)                                                   
         DROP  R4                                                               
         GOTO1 HIGH                                                             
         B     TIT50                                                            
         SPACE 1                                                                
TIT40    GOTO1 SEQ                                                              
         SPACE 1                                                                
TIT50    CLC   KEYSAVE(11),KEY     STATION D/P C/B                              
         BNE   TIT60                                                            
         MVC   0(4,R5),KEY+28                                                   
         LA    R5,4(R5)                                                         
         LA    R6,1(R6)                                                         
         B     TIT40                                                            
         SPACE 1                                                                
TIT60    LTR   R6,R6                                                            
         BZ    XIT                                                              
         L     R5,ASTACK                                                        
         B     TIT80                                                            
         SPACE 1                                                                
TIT70    LM    R5,R6,SAVESTAK                                                   
         LA    R5,4(R5)                                                         
         BCT   R6,TIT80                                                         
         B     XIT                                                              
         EJECT                                                                  
TIT80    MVC   KEY+28(4),0(R5)                                                  
         STM   R5,R6,SAVESTAK                                                   
         OI    DMINBTS,X'08'       SET PASS DELETED RECORDS                     
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'       UNSET PASS DELETED RECORDS                   
         MVC   KEY(27),IO                                                       
         TM    IO+29,X'80'         TEST RECORD IS DELETED                       
         BO    TIT70                                                            
         SPACE 2                                                                
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
         SPACE 1                                                                
TIT125   CLI   RINVPAUT,C'N'       IS IT NT (NO AUTO TRANSFER)                  
         BE    TIT130                                                           
         TM    PRINTOPT,X'80'      USE NT PROGRAMS ONLY                         
         BO    TIT70                                                            
         SPACE 1                                                                
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
TIT140   BAS   RE,HDR              PRINT DETAILS                                
         SPACE 2                                                                
TIT160   B     TIT70                                                            
         SPACE 2                                                                
PREPX    B     XIT                                                              
         EJECT                                                                  
* TSTDPT -- CHECKS IF PROGRAM HAS BEEN PRINTED ALREADY                          
*           PROGRAMS PRINT IN REQUEST ORDER                                     
         SPACE                                                                  
TSTDPT   NTR1                                                                   
         MVI   SKIPREC,0          0=USE RECORD                                  
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
*              PROCESSING HEADER RECORDS                                        
*              -------------------------                                        
         SPACE 2                                                                
**** R6 POINTS AT RINVPEL                                                       
         SPACE 1                                                                
HDR      NTR1                                                                   
         SPACE 2                                                                
         LA    R3,P                                                             
         USING PONE,R3                                                          
         GOTO1 UNDAY,PARAS,RINVPDAY,PDAY      DAY                               
         GOTO1 UNTIME,PARAS,RINVPTIM,PTIM     TIME                              
*                                                                               
         ZIC   R5,RINVPLEN                                                      
         SH    R5,=H'40'                                                        
         GOTO1 CHOPPER,PARAS,((R5),RINVPROG),(27,PPROG),(C'P',3)                
*                                                                               
HDR12    CLC   RINVPEFF(2),RINVPEFF+2          EFFECTIVE DATES                  
         BNE   HDR13                                                            
         MVC   PDTS(4),=C'ONLY'                                                 
         GOTO1 DATCON,PARAS,(2,RINVPEFF),(8,PDTS+5)                             
         B     HDR16                                                            
         SPACE 1                                                                
HDR13    DS    0H                                                               
         MVC   PDTS(4),=C'FROM'                                                 
         GOTO1 DATCON,PARAS,(2,RINVPEFF),(8,PDTS+5)                             
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    HDR16                                                            
         MVC   PDTS(8),PDTS+5                                                   
         MVC   PDTS+8(56),SPACES                                                
         MVI   PDTS+8,C'-'                                                      
         GOTO1 DATCON,PARAS,(2,RINVPEFF+2),(8,PDTS+9)                           
         SPACE 1                                                                
HDR16    MVC   PDPT(6),RINVDP      DAYPART CODE(S)                              
         MVC   PFLT,RINVPFLT       FILTERS                                      
         CLI   RINVPAUT,C'N'       NO AUTO TRANSFER                             
         BNE   HDR20                                                            
         MVC   PTRNSFR,=C'NT'                                                   
         SPACE 1                                                                
HDR20    LA    R4,KEY              INVENTORY NUMBER                             
         USING RINVKEY,R4                                                       
         CLI   RINVKINV+3,0        IF NEW INV NUMBER                            
         BE    *+14                                                             
         MVC   PINV,RINVKINV          IT IS ALREADY IN CHARACTER                
         B     HDR30                                                            
*                                                                               
         EDIT  (1,RINVKINV),(2,PINV),FILL=0  QUARTER HOUR                       
         MVC   PINV+2(2),RINVKINV+1  DAY AND PROGRAM LENGTH                     
         CLI   PINV+3,C'0'                                                      
         BNE   *+8                                                              
         MVI   PINV+3,C' '                                                      
         SPACE 1                                                                
HDR30    DS    0H                                                               
         GOTO1 SPOOL,PARAS,(R8)   PRINT THE DATA                                
         GOTO1 SPOOL,PARAS,(R8)   SPACING LINE                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE DETAILS                                             
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+10(6),TITSTAT    STATION CALL LETTERS                         
         MVC   H4+17(29),MKTSV     MARKET NAME                                  
         SPACE 1                                                                
         L     R2,ADPTBL           LOOK UP DAYPART NAME                         
HOOK3    CLC   0(1,R2),SVDPT                                                    
         BE    HOOK5                                                            
         LA    R2,L'DPTBL1(R2)                                                  
         CLI   0(R2),X'FF'                                                      
         BNE   HOOK3                                                            
         SPACE 1                                                                
HOOK5    MVC   H5+10(1),0(R2)      DAYPART NUMBER                               
         MVC   H5+17(20),1(R2)     DAYPART NAME                                 
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
         EJECT                                                                  
         DS    0H                                                               
DPTBL1   DS    0CL21               DEFAULT DAYPART TABLE                        
         DC    CL21'MMORNING'                                                   
         DC    CL21'DDAYTIME'                                                   
         DC    CL21'EEARLY FRINGE'                                              
         DC    CL21'REARLY NEWS'                                                
         DC    CL21'APRIME ACCESS'                                              
         DC    CL21'TLATE NEWS'                                                 
         DC    CL21'LLATE FRINGE'                                               
         DC    CL21'WWEEKEND'                                                   
         DC    CL21'KKIDS'                                                      
         DC    CL21'FFRINGE'                                                    
         DC    CL21'NNEWS'                                                      
         DC    CL21'PPRIME'                                                     
         DC    CL21'VMOVIES'                                                    
         DC    CL21'SSPECIALS'                                                  
         DC    CL21'JSPORTS'                                                    
         DC    CL21'OSOAPS'                                                     
         DC    CL21'UCOMPETITIVE'                                               
         DC    CL21'XLOCAL'                                                     
         DC    CL21'YOTHER'                                                     
         DC    X'FF'                                                            
         DC    CL20'GENERAL'                                                    
         SPACE 1                                                                
NDPT     EQU   (*-DPTBL1)/L'DPTBL1                                              
         SPACE 3                                                                
DPTBL2   DS    0CL21               NBC DAYPART TABLE                            
         DC    CL21'MMORNING'                                                   
         DC    CL21'DDAYTIME'                                                   
         DC    CL21'EEARLY FRINGE'                                              
         DC    CL21'REARLY NEWS'                                                
         DC    CL21'APRIME ACCESS'                                              
         DC    CL21'TLATE NEWS'                                                 
         DC    CL21'LLATE FRINGE'                                               
         DC    CL21'WWEEKEND'                                                   
         DC    CL21'KKIDS'                                                      
         DC    CL21'FFRINGE'                                                    
         DC    CL21'NNEWS'                                                      
         DC    CL21'PPRIME'                                                     
         DC    CL21'VMOVIES'                                                    
         DC    CL21'SSPECIALS'                                                  
         DC    CL21'JSPORTS'                                                    
         DC    CL21'OOLYMPICS'                                                  
         DC    CL21'UCOMPETITIVE'                                               
         DC    CL21'XLOCAL'                                                     
         DC    CL21'YOTHER'                                                     
         DC    X'FF'                                                            
         DC    CL20'GENERAL'                                                    
         EJECT                                                                  
* COMMON ROUTINES                                                               
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
RELO     DS    A                                                                
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
         PSPEC H4,001,C'STATION -'                                              
         PSPEC H5,001,C'DAYPART -'                                              
         SPACE 1                                                                
         SPROG 0                                                                
         PSPEC H7,03,C'INV NO  DAY'                                             
         PSPEC H8,03,C'------  ---'                                             
         PSPEC H7,25,C'TIME          PROGRAM NAME'                              
         PSPEC H8,25,C'----          ------------'                              
         PSPEC H7,69,C'EFFECTIVE DATES'                                         
         PSPEC H8,69,C'---------------'                                         
         PSPEC H7,88,C'DAYPART  FILTERS   NT'                                   
         PSPEC H8,88,C'-------  -------   --'                                   
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*  PRINT LINE DSECT                                                             
         SPACE 2                                                                
P1D      DSECT                                                                  
PONE     DS    0CL132                                                           
         DS    CL3                                                              
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
PTRNSFR  DS    CL2                 TRANSFER                                     
         DS    CL24                                                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RERESF7D                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE            LOCAL WORKING STORAGE                        
SAVESTAK DS    2F                                                               
XTODAY   DS    XL2                 TODAY'S DATE (COMPRESSED)                    
REPPAR   DS    CL2                 PARENT REP                                   
SDATE    DS    CL2                 START DATE (COMPRESSED)                      
EDATE    DS    CL2                 END DATE (COMPRESSED)                        
DPLIST   DS    CL20                DAYPART LIST                                 
         DS    CL1                 SAVE DAYPART CODE                            
NUMFILT  DS    XL1                 NUMBER OF FILTER CODES                       
PRINTOPT DS    XL1                 X'80'  PRINT NT RECORDS ONLY                 
*                                  X'40'  FILTERS REQUESTED                     
*                                  X'20'  START DATE REQUESTED                  
*                                  X'10'  END DATE REQUESTED                    
*                                  X'08'  PRINT ALL TITLES                      
OPTFLAG  DS    XL1                 FLAG FOR OP3                                 
SKIPREC  DS    XL1                FLAG SKIP RECORD OR NOT                       
NOREQDPT DS    XL1                NO OF REQUESTED DAYPARTS                      
*                                                                               
STAMP    DS    CL8                 STORAGE STAMP                                
SAVELN   EQU   *-SYSSPARE          SAVE AREA LENGTH                             
SBUFF    DS    A                  A(START OF PRINT BUFFER)                      
MYBASE   DS    A                                                                
ASTACK   DS    A                                                                
ADPTBL   DS    A                   A(DAYPART TABLE TO USE)                      
         DS    CL(L'SYSSPARE-(*-SYSSPARE)) SPARE                                
*                                                                               
         SPACE 2                                                                
STACK    CSECT                                                                  
         DS    D                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059RERES07   04/24/09'                                      
         END                                                                    
