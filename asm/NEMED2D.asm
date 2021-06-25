*          DATA SET NEMED2D    AT LEVEL 037 AS OF 03/22/19                      
*PHASE T31E2DA,+0                                                               
*INCLUDE NETNET                                                                 
*INCLUDE SPBVAL                                                                 
         TITLE 'T31E2D - NETWORK AGENCY SUMMARY'                                
         PRINT NOGEN                                                            
*******************************************************************             
* NETWORK AGENCY SUMMARY                                                        
*                                                                               
*    USING EXTENDED STATION BUFFER                                              
*                                                                               
*  NOTE FOR DDS ACCTG AGY IS SET TO ALL.                                        
*  *** NOTE: BECAUSE THERE ARE SEVERAL DDS AGENCY HEADERS WITH AGY/MED          
*          =F3 (THE DDS ID), A FLAG IS SET TO ONLY PROCESS THE DATA             
*           THE FIRST TIME. THIS IS A HARD FUDGE BUT NECESSARY                  
*           UNLESS THE AGYFILE IS CLEANED UP                                    
*                                                                               
* GLOBALS:  R6 - WORKING STORAGE BASE                                           
*           R9 - A(NETSYSD, NETBLOCK)                                           
*                                                                               
*  INPUTS: ANETWS1 - CONTAINS THE CLIENT RECORD                                 
*          NETBLOCK - NBSELSTR,NBSELEND - USER SUPPLIED MONTHS.                 
*       PRBEFFLG - SET IF 'BEFORE' TOTALS ARE TO BE PRINTED                     
*       PRAFTFLG - SET IF 'AFTER' TOTALS ARE TO BE PRINTED                      
*       PRZLFLG - SET IF ZERO LINES ARE TO BE PRINTED                           
*       PRZCFLG - SET IF ZERO CLIENTS ARE TO BE PRINTED                         
*       MONTYPE - C'BMON' FOR BRODCAST MONTH, C'MON' FOR CAL MONS               
*       ENDWK   - IF BROD MONTH, THIS IS DAY BMON STARTS ON.                    
*       CSTMASKS - FULLWORD BIT MASKS, ONE FOR EACH COST TYPE. TELLS            
*                   WHICH PRINT FIELDS THIS COST IS TO BE ADDED TO.             
*                                                                               
*       HOLDAGNM - NAME OF FIRST AGENCY                                         
*       PRNOCLI - FLAG SET IF NO CLIENT DEATAILS ARE TO BE PRINTED              
*                                                                               
*                                                                               
T31E2D   CSECT                                                                  
         NMOD1 800,**NTAG**,RR=R5                                               
         LA    RA,2048(RB)         RA IS SECOND BASE                            
         LA    RA,2048(RA)                                                      
         USING T31E2D+4096,RA                                                   
         LR    R3,RC              SAVE NMOD STORAGE                             
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         DROP  R8                                                               
*                                                                               
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R6,ANETWS2                                                       
         USING WORKD,R6                                                         
         A     R6,=F'1000'                                                      
         OC    NBSELAGY,NBSELAGY                                                
         BZ    SKPIT                                                            
         ST    R3,NBANBUFF          SET NMOD STORAGE FOR MEDIA BUFF             
         LR    RE,R3                CLEAR AREA FOR MEDIA BUFFER                 
         L     RF,=F'6000'                                                      
         OI    NBINDS7,NBI7NTIO    SET EXTENED STA BUFFER                       
         XCEF                                                                   
SKPIT    ST    R5,RELO                                                          
         MVI   SJRFLAG,0           SET FLAG TO  0                               
*                                                                               
**** INITIALIZE  ************                                                   
**   FOR  ON-LINE                                                               
         OI    NBSBKEND,NBNODPT2   NO READ FOR 2 CHAR DPT                       
         LA    R1,TOTAREA                                                       
         ST    R1,ATOTAREA                                                      
         LA    R1,16               MAX OF 16 MONTHS FOR ON-LINE                 
         ST    R1,MAXMONTS                                                      
*                                                                               
******** FOR OFFLINE                                                            
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   ONOROFFX              GET XTRA MEMORY.                           
***      L     R2,=V(DUMMY)                                                     
***      A     R2,RELO                                                          
         L     R2,VADUMMY          CHANGE FOR NEW GENCON                        
         ST    R2,ATOTAREA         PUT ADDRESS OF XTRA WORK IN R2               
         LA    R1,60               MAX OF 60 MONTHS FOR OFFLINE                 
         ST    R1,MAXMONTS                                                      
         OI    NBINDS5,NBI5XRD     BILLRDR SPECIAL SFILE READ                   
*                                                                               
ONOROFFX L     R3,ATOTAREA                                                      
         ST    R3,AMONLIST                                                      
         L     R1,MAXMONTS                                                      
         SLL   R1,2                4 BYTES PER DATE SET                         
         LA    R3,0(R1,R3)                                                      
         ST    R3,ASPERLST         SPECIAL PERIOD DATE SET LIST                 
         OI    NBINDS5,X'02'       SKIP MUCH OF NETVALUE                        
                                                                                
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SB1S'                                                 
         NI    WORK,X'BF'           LOWER CASE                                  
         MVC   WORK+4(2),NBSELAGY                                               
         GOTO1 NBGTPROF,DMCB,(X'C0',WORK),WORK+20,NBDM                          
         CLI   WORK+23,C'Y'                ONLY WRITING NEW BILLING ?           
         BNE   *+8                                                              
         OI    NBINDS2,NBBILLRD+NBLCMPR    TELL NETIO TO READBILL               
*                                                                               
         MVI   NBNOWRIT,C'N'       DOESN'T WRITE TO FILE                        
*        BRAS  RE,GET56K                                                        
*                                                                               
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK   PROCESS DATE                             
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GOTDATE                                                          
         CLI   NBMODE,NBREQLST                                                  
         BE    ASXIT                                                            
         B     PROCDAT                                                          
*                                                                               
GOTDATE  MVC   NBUSER+2(1),PRDOVR  SET CALENDAR TYPE                            
***      GOTO1 =V(PRNTBL),DMCB,=C'GOT',NBAIO,C'DUMP',200,=C'1D'                 
         BAS   RE,INITMON          GET THE MONTH LIST FOR STRT,END              
*                                  *** ALSO SET SPEC PERIOD LIST                
         L     R3,ASPERLST          SET UP REST OF ADDRESSES BASED              
*                                     ON NUMMONS,NUMSROW                        
         LH    R1,NUMSROW          NUMBER OF SPEC PERS                          
         SLL   R1,2                4 BYTES PER DATE SET                         
         LA    R3,0(R1,R3)                                                      
         ST    R3,AAGYTOTS                                                      
         L     R1,NUMMONS          SET OF COLUMNS FOR EACH MONTH                
         MH    R1,NUMCOLS                                                       
         SLL   R1,3                DOUBLEWORDS                                  
         LA    R3,0(R1,R3)                                                      
         ST    R3,ACLITOD                                                       
         LH    R1,NUMCOLS          SPECIAL PERIOD TOTALS                        
         SLL   R1,3                                                             
         MH    R1,NUMSROW          NUM SPECIAL PERIODS                          
         LA    R3,0(R1,R3)                                                      
         ST    R3,AAGYTOD          AGENCY SPECIAL PERIOD TOTALS                 
         LH    R1,NUMCOLS                                                       
         SLL   R1,3                                                             
         MH    R1,NUMSROW          NUM SPECIAL PERIODS                          
         LA    R3,0(R1,R3)                                                      
         ST    R3,ACLITOTS                                                      
         LH    R1,NUMCOLS                                                       
         SLL   R1,3                                                             
         LA    R3,0(R1,R3)                                                      
         ST    R3,ACURTOTS                                                      
         L     R1,NUMMONS          SET OF COLUMNS FOR EACH MONTH                
         MH    R1,NUMCOLS                                                       
         SLL   R1,3                DOUBLEWORDS                                  
         LA    R3,0(R1,R3)                                                      
         OC    NBSELAGY,NBSELAGY   FOR AGY=ALL (DDS REPORT)                     
         BNZ   AG2                   (MUST BE OFFLINE)                          
         ST    R3,ADDSTOTS         DDS TOTALS                                   
         L     R1,NUMMONS          SET OF COLUMNS FOR EACH MONTH                
         MH    R1,NUMCOLS                                                       
         SLL   R1,3                DOUBLEWORDS                                  
         LA    R3,0(R1,R3)                                                      
         ST    R3,ADDSTOD          DDS SPECIAL PERIOD TOTALS                    
         LH    R1,NUMCOLS                                                       
         SLL   R1,3                                                             
         MH    R1,NUMSROW          NUM SPECIAL PERIODS                          
         LA    R3,0(R1,R3)                                                      
*                                                                               
AG2      BAS   RE,INITCUR          INITIALIZE TOTALS                            
         BAS   RE,INITCLT                                                       
         BAS   RE,INITAGY                                                       
         BAS   RE,INITTOD          CLIENT SPEC PER(FORMERLY TODAY ONLY)         
         BAS   RE,INITTODA         AGENCY SPECIAL PERIOD                        
         BAS   RE,INITDDS          DDS TOTALS                                   
         BAS   RE,INITDTOD         DDS SPECIAL PERIOD                           
         ZAP   MARGTOT,ZERO                                                     
*                                                                               
         MVI   NBDATA,C'U'         SELECT UNIT RECORDS                          
         MVC   NBSELPRD,=C'ALL'    DONT FILTER ON PRODUCT                       
*                                                                               
*                                                                               
         BAS   RE,NEWAGY                                                        
         BAS   RE,NEWCLI           FILL IN CLIENT INFO                          
         MVI   NBRESUME,NBPROCPK                                                
         CLI   MANONLY,C'M'        CHK IF ONLY MANUALS                          
         BNE   GETUNIT                                                          
         BAS   RE,MANBILS          YES/DO IT 'N XIT                             
         B     TOTALS                                                           
*                                                                               
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK    GET NEXT RECORD                         
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BE    *+8                 YES                                          
         BAS   R4,CHKMAXIO         NO/ARE WE OVER MAX IO ?                      
                                                                                
GUCK     CLI   NBERROR,0           CHECK FOR ERROR                              
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST     IF NO MORE UNITS                             
         BE    TOTALS                                                           
         CLI   NBMODE,NBVALDAT     VALIDATES DATE AFTER EACH CLI                
         BE    NEWDAT                                                           
         CLI   NBMODE,NBVALAGY     IF NEW AGENCY, MUST BE AGY ALL               
         BNE   GUN2                                                             
*                              **** NEW AGY ***                                 
         BAS   RE,DUMPCLT          DUMP OLD CLT                                 
         BAS   RE,FINAGY           DUMP OLD AGY                                 
         CLI   SJRFLAG,C'Y'        IF FLAG SET AND AGY/MED =F3                  
         BNE   GUN1                  THEN SKIP THIS AGY                         
         CLI   NBACTAM,X'F3'                                                    
         BNE   GUN1                                                             
         MVI   NBSELMOD,NBVALAGY   SKIP TO NEXT AGY                             
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         MVI   NBSELMOD,0                                                       
         B     GUCK                                                             
*                                                                               
GUN1     L     R3,NBAIO                                                         
         USING AGYHDR,R3           GET NEW AGENCY NAME                          
         MVC   HOLDAGNM,AGYNAME                                                 
         MVC   HOLDAGBP(2),AGYPROF+2  AGENCY BILLING PCTG                       
         DROP  R3                                                               
         BAS   RE,NEWAGY                                                        
         B     GETUNIT                                                          
*                                                                               
GUN2     CLI   NBMODE,NBVALCLI     IF A NEW CLIENT                              
         BNE   CKUN                                                             
         BAS   RE,DUMPCLT          FINISH OLD CLIENT                            
         BAS   RE,NEWCLI           GET NEW CLIENT                               
         MVI   NBFUNCT,NBFRDHI     IN CASE FILTERING PROD GROUPS                
         B     CKUN                                                             
*                                                                               
NEWDAT   OC    PRBEFFLG,PRBEFFLG   IF BEFORES REQUIRED                          
         BZ    GUN4                                                             
         MVC   NBCMPSTR,MINDAT                                                  
GUN4     OC    PRAFTFLG,PRAFTFLG   IF AFTERS REQUIRED                           
         BZ    *+10                                                             
         MVC   NBCMPEND,MAXDAT                                                  
         BAS   RE,MANBILS                                                       
         B     GETUNIT                                                          
*                                                                               
*                                                                               
CKUN     CLI   NBMODE,NBPROCUN     IF A UNIT RECORD                             
         BNE   GETUNIT                                                          
         CLI   OFFLINE,C'Y'                                                     
         BNE   CKUN10                                                           
         L     R1,=A(PRDEXTBL)         ARE WE FILTERING PROD(GRPS)              
CKUN5    CLI   0(R1),0                                                          
         BE    CKUN7                                                            
         CLC   NBSPLPRN,0(R1)                                                   
         BE    GETUNIT             MATCH/SKIP UNIT                              
         LA    R1,1(R1)                                                         
         B     CKUN5                                                            
CKUN7    L     R1,=A(PRDINTBL)     INCLUDE PRODUCTS                             
         CLI   0(R1),0                                                          
         BE    CKUN10                                                           
CKUN8    CLC   NBSPLPRN,0(R1)                                                   
         BE    CKUN10              OK                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BE    GETUNIT              NO MATCH /SKIP UNIT                         
         B     CKUN8                                                            
CKUN10   BAS   RE,PROCUN                                                        
         B     GETUNIT                                                          
*                                                                               
*                                                                               
TOTALS   BAS   RE,DUMPCLT          FINISH LAST CLIENT                           
         BAS   RE,FINAGY           FINISH LAST AGENCY                           
         BAS   RE,FINDDS           FINISH DDS TOTALS                            
*                                                                               
*!!! ASXIT    BRAS  RE,FREE56K                                                  
ASXIT    XMOD1                                                                  
******                                                                          
PROCERR  DC    F'0'                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*********************************************************                       
*  GETS SPECIAL RATES AND RETURNS IN FULL                                       
SPECIALS NTR1                                                                   
         XC    FULL,FULL                                                        
         L     R3,NBAIO            NOW GET ACTUAL UNIT                          
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL         SET UP R2 FOR GETEL                          
         MVI   SRCHEL,X'03'        GET SPECIAL RATES ELEMENT                    
FL0      BAS   RE,NEXTEL                                                        
         USING NUSPRD,R2                                                        
         BNE   FLX                                                              
         ICM   R0,15,NUSPRAMT                                                   
***************************************************                             
* CHECK RATE TYPE VS COST WE ARE DEALING WITH                                   
         CLI   NBRTTYPE,0          IS THERE RATE TYPE?                          
         BE    FL2                                                              
         CLI   NBSDRTCV,C'A'       A=APPLY TO ALL COSTS                         
         BH    FL2                   NO                                         
         GOTO1 =V(NETNET),DMCB,(NBRTTYPE,NUSPRAMT),RATEWRK,RR=RELO              
         ICM   R0,15,RATEWRK+4          NET                                     
         CLI   NETOPT,YES                                                       
         BE    FL3                                                              
         ICM   R0,15,RATEWRK                                                    
         B     FL3                                                              
**************************************************************                  
FL2      CLI   NETOPT,YES           IS IT NET OPTION                            
         BNE   FL3                                                              
         CLI   NUSPRCOM,C'C'        IS IT COMMISION                             
         BE    FL2A                                                             
         CLI   NUSPRCOM,C'Y'        IS IT COMMISION                             
         BNE   *+8                                                              
FL2A     BAS   RE,FINDNET                                                       
FL3      A     R0,FULL                                                          
         ST    R0,FULL                                                          
         B     FL0                                                              
FLX      B     EXIT                                                             
***************************************************************8                
         EJECT                                                                  
***************************************************************8                
* READS STATION BUCKETS FOR MANUAL BILLS                                        
*                                                                               
*MANBILS  NTR1  WORK=(R2,150)                                                   
MANBILS  NTR1  WORK=(R2,250)                                                    
         LR    RE,R2                                                            
         LA    RF,2000                                                          
         XCEF                                                                   
         LR    R5,R2               SAVE START OF NTR1 WORK AREA                 
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E01'                                                  
         L     R1,ANETWS1                 ANETWS1=A(CLIENT RECORD)              
         CLI   MANONLY,C'M'                                                     
         BE    *+10                                                             
         MVC   KEY+2(3),1(R1)             AM/CLT                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     ST5                                                              
STBSEQ   LR    R2,R5              RESTORE R2 TO START OF NTR1 WORK AREA         
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
ST5      CLI   MANONLY,C'M'                                                     
         BNE   ST7                                                              
         CLC   KEY(2),KEYSAVE                                                   
         BE    ST9                                                              
ST7      CLC   KEY(5),KEYSAVE             ID/AM/CLT                             
         BNE   STABX                                                            
ST9      MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,(R2),(0,DMWORK)          
*                                                                               
         USING STABELEM,R2                                                      
         MVI   SRCHEL,X'0E'                                                     
         LA    R2,24(R2)                                                        
         CLI   0(R2),X'0E'                                                      
         BE    *+12                                                             
NXTELEM  BAS   RE,NEXTEL                                                        
         BNE   STBSEQ                                                           
*                             CONVERT BINARY(YYMM) TO COMPRESSED                
* NOTE THAT STAB RECS HAS STABPER=YYMM, STABBDT=2 BYTE COMPRESSED               
*                                                                               
DATCONVT MVI   WORK+2,X'01'                                                     
         MVC   WORK(2),STABPER                                                  
         GOTO1 DATCON,DMCB,(3,WORK),(2,WORK+50)    WORK+50=BILLPERIOD           
*                                                                               
         CLC   NBCMPSTR(2),WORK+50      CHK DATE RANGE                          
         BH    NXTELEM                                                          
         CLC   NBCMPEND(2),WORK+50                                              
         BL    NXTELEM                                                          
*                                                                               
         LA    R0,STABGRS          POINT TO GROSS AMOUNT                        
         CLI   NETOPT,YES          TEST FOR NET OPTION                          
         BNE   *+8                                                              
         LA    R0,STABNET          YES-POINT TO NET FIGURE                      
         GOTO1 ADDEM,DMCB,(R0),WORK+50,=AL4(BTGMSK)                             
         GOTO1 ADDEM,DMCB,STABNET,WORK+50,=AL4(BTNMSK)                          
         GOTO1 ADDTODAY,DMCB,(R0),STABBDT,=AL4(BTGMSK)                          
         GOTO1 ADDTODAY,DMCB,STABNET,STABBDT,=AL4(BTNMSK)                       
         B     NXTELEM                                                          
*                                                                               
STABX    DS    0H                                                               
         NETGO NVSETUNT,DMCB                                                    
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***************************************************************8                
*  HANDLE A NEW CLIENT                                                          
         USING COLINFO,R5                                                       
NEWCLI   NTR1                                                                   
         NI    NBSPLOPT,X'FF'-X'C0'      TURN OFF TO SPLIT UNITS                
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R5,P1               FIRST PRINT LINE FOR CLI HEADERS             
         LA    R4,P1                                                            
         L     R3,ANETWS1          ADDRESS OF CLIENT RECORD                     
         USING CLTHDR,R3                                                        
         MVC   CLCNAM(L'CNAME),CNAME  MOVE IN CLIENT NAME                       
         MVC   CLCLI,NBCLICOD         CLI CODE                                  
         GOTO1 UNDERLIN,DMCB,(24,CLCLI),CLUNDER   UNDERLINE IT                  
         LA    R4,P3               USE NEXT PRINT LINE FOR OTHERS               
         LA    R5,P3                                                            
         CLI   NBSELOFF,0          IF REQUESTED BY FILTER                       
         BE    NC05                                                             
         MVC   OFFSAV,COFFICE                                                   
         B     NC10                                                             
*                                                                               
NC05     OC    COFFICE,COFFICE                                                  
         BZ    NC10                                                             
         MVC   CLCLI(6),=C'OFFICE'                                              
*!!!     MVC   CLCLI+7(1),COFFICE                                               
*                                                                               
         LA    RF,DUB                                                           
         USING OFFICED,RF                                                       
         XC    0(OFCLENQ,RF),0(RF)                                              
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         GOTO1 AOFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                        
         LA    RF,DUB                                                           
         MVC   CLCLI+7(2),OFCOFC2                                               
         DROP  RF                                                               
*                                                                               
         LA    R4,P4                  GET NEXT PRINT LINE                       
         LA    R5,10(R5)              BUMP CURRENT PRINT LINE                   
NC10     OC    CACCOFC,CACCOFC                                                  
         BZ    NC20                                                             
         CLI   CACCOFC,X'40'                                                    
         BH    *+12                                                             
         CLI   CACCOFC+1,X'40'                                                  
         BNH   NC20                                                             
         MVC   CLCLI(10),=C'ACC OFFICE'                                         
         MVC   CLCLI+12(2),CACCOFC                                              
         LA    R4,P4                                                            
         DROP  R3                                                               
*                                                                               
NC20     ST    R4,CURLINE          SAVE CURRENT LINE                            
*                                                                               
         MVI   NBUSER+13,C'N'      OVERRIDE PROFILE. DONT FILTER                
*                                   PRE-EMPTS                                   
         MVI   NBUSER+8,C'Y'       OVERRIDE PROFILE. USE ASSIGNED COST          
* - ARE WE FILTERING PRODGROUPS                                                 
         CLI   OFFLINE,C'Y'                                                     
         BNE   NCX                                                              
         L     RE,=A(PRDEXTBL)                                                  
         MVI   0(RE),0                                                          
         L     RE,=A(PRDINTBL)                                                  
         MVI   0(RE),0                                                          
         L     R8,ATWA                                                          
         USING T31EFFD,R8                                                       
         GOTO1 SCANNER,DMCB,AGYPRODH,OUTAREA                                    
         ZIC   R2,DMCB+4                       R2=NO OF SCAN ENTRIES            
         LTR   R2,R2                                                            
         BZ    NCX                 ...NOT FILTERING PROD GROUPS/XIT             
         L     RE,=A(PRDEXTBL)        ...YES FILTERING / CLEAR TABLE            
         LA    RF,200                                                           
         XCEF                                                                   
         L     RE,=A(PRDINTBL)        ...YES FILTERING / CLEAR TABLE            
         LA    RF,200                                                           
         XCEF                                                                   
         LA    R3,OUTAREA                                                       
NC30     ZIC   R4,0(R3)            LENGTH OF 1ST HALF OF FIELD                  
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   13(0,R3),NBCLICOD                                                
         BNE   NC35                                                             
         L     R5,=A(PRDEXTBL)                                                  
         CLI   12(R3),C'-'                                                      
         BE    NC40                                                             
         L     R5,=A(PRDINTBL)                                                  
         B     NC40                                                             
NC35     LA    R3,32(R3)                                                        
         BCT   R2,NC30                                                          
         B     NCX                                                              
NC40     DS    0H                  FILL PRODUCT EXCLUSION TABLE                 
         OI    NBSPLOPT,X'C0'      SET TO  SPLIT UNITS                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D81'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),NBACTCLI                                                
         MVC   KEY+5(1),22(R3)             SET IN GROUP ID                      
         PACK  KEY+6(3),23(5,R3)           GET PACKED WITHOUT SIGN              
         MVI   KEY+8,0                                                          
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
NC50     CLC   KEY(8),KEYSAVE                                                   
         BNE   NC35                                                             
         L     RF,ANETWS1          ADDRESS OF CLIENT RECORD                     
         USING CLTHDR,RF                                                        
         LA    RF,CLIST                                                         
         LA    RE,220                                                           
NC55     CLC   0(3,RF),KEY+8       SET PROD TO EXCLUSION TBL                    
         BE    NC57                                                             
         LA    RF,4(RF)                                                         
         BCT   RE,NC55                                                          
*                                                                               
* THIS CAUSED AN ASSEMBLY ERROR B/C OF BAD BRANCH                               
* ASSUMING IT GOES AND GETS THE NEXT PRODUCT RECORD                             
*        B     *+14                NO MATCH SKIP IT                             
         B     NC58                                                             
NC57     CLI   0(R5),0             TABLE POSITION EMPTY                         
         BE    *+12                                                             
         LA    R5,1(R5)                                                         
         B     *-12                                                             
         MVC   0(1,R5),3(RF)                                                    
         LA    R5,1(R5)            BUMP TABLE                                   
NC58     GOTO1 SEQ                                                              
         BE    NC50                                                             
         B     NC35                                                             
NCX      B     EXIT                                                             
         DROP  R5                                                               
         DROP  R8                                                               
****************************************************************                
         EJECT                                                                  
***************************************************************8                
*  HANDLE A NEW AGENCY.                                                         
         USING COLINFO,R5                                                       
NEWAGY   NTR1                                                                   
         OC    NBSELAGY,NBSELAGY   IF AGENCY = ALL                              
         BNZ   NA2                                                              
         CLI   NBACTAM,X'F3'       IF AGYMED = F3 THEN SET FLAG                 
         BNE   NWA2                  SEE TOP FOR DETAILS WHY                    
         MVI   SJRFLAG,C'Y'                                                     
NWA2     L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
**       GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   R7,PRINTIT                                                       
*                                                                               
         LA    R5,P1               FIRST PRINT LINE FOR CLI HEADERS             
         MVC   P2+30(8),=C'AGENCY= '  MOVE IN AGENCY NAME                       
         MVC   P2+40(2),NBEFFAGY                                                
         MVC   P2+44(33),HOLDAGNM                                               
         GOTO1 UNDERLIN,DMCB,(50,P2+30),(C'*',P1+30)  OVERLINE IT               
         GOTO1 UNDERLIN,DMCB,(50,P2+30),(C'*',P3+30)  UNDERLINE IT              
**       GOTO1 SPOOL,DMCB,(R8)          PRINT WHATS THERE                       
**       GOTO1 SPOOL,DMCB,(R8)          PRINT BLANK LINE                        
         BAS   R7,PRINTIT                                                       
         BAS   R7,PRINTIT                                                       
*                                                                               
NA2      MVI   NBUSER+13,C'N'      OVERRIDE PROFILE. DONT FILTER                
*                                   PRE-EMPTS                                   
         MVI   NBUSER+8,C'Y'       OVERRIDE PROFILE. USE ASSIGNED COST          
         B     EXIT                                                             
         DROP  R5                                                               
         DROP  R8                                                               
****************************************************************                
         EJECT                                                                  
****************************************************************                
*  SUM THE CURRENT UNIT RECORD INTO THE CURRENT TOTALS                          
*                                                                               
*     INPUTS: UNIT RECORD                                                       
*             MASK FOR EACH OUTPUT FIELD                                        
*                                                                               
*     CALLS TO: ADDEM - ADDS COST IN APPROPRIATE COLUMN                         
*                  ARGS:         COST                                           
*                                DATE COST INCURRED                             
*                                MASK BIT CORRESPONDING TO THIS COST            
*                                                                               
PROCUN   NTR1                                                                   
*                                                                               
         TM    NBUNITST,X'42'      IF PRE EMPT OR MISSED                        
         BZ    PU1                                                              
         XC    NBACTUAL,NBACTUAL     SET COSTS TO 0                             
         XC    NBASSIGN,NBASSIGN     BUT LEAVE BILLING AND PAYING               
         XC    NBINTEG,NBINTEG                                                  
*                                                                               
*                                                                               
PU1      DS    0H                                                               
         CLI   NBRTTYPE,0          CHK FOR RATE TYPES                           
         BE    PU1A                                                             
         BAS   RE,RTTYPES                                                       
PU1A     CLI   NETOPT,YES          TEST FOR NET OPTION                          
         BNE   *+8                                                              
         BAS   RE,NETCOSTS         YES-NET IN PLACE                             
         GOTO1 ADDEM,DMCB,NBCALCOS,NBACTDAT,=AL4(CASSMSK)                       
         GOTO1 ADDEM,DMCB,NBACTUAL,NBACTDAT,=AL4(ACTMSK)                        
         GOTO1 ADDEM,DMCB,NBASSIGN,NBACTDAT,=AL4(ASSMSK)                        
         GOTO1 ADDEM,DMCB,NBINTEG,NBACTDAT,=AL4(INTMSK)                         
         GOTO1 ADDEM,DMCB,NBFEED,NBACTDAT,=AL4(FEEDMSK)                         
*                                                                               
         TM    NBUNITST,X'42'      IF PRE-EMPT OR MISSED                        
         BNZ   PU1AA                                                            
         BAS   RE,SPECIALS         ELSE ADD SPECIAL CHARGES                     
         GOTO1 ADDEM,DMCB,FULL,NBACTDAT,=AL4(ACTMSK)    TO ACTUAL               
         BAS   RE,SPECIALS                                                      
         GOTO1 ADDEM,DMCB,FULL,NBACTDAT,=AL4(ASSMSK)    TO ASSIGNED             
         BAS   RE,SPECIALS                                                      
         GOTO1 ADDEM,DMCB,FULL,NBACTDAT,=AL4(CASSMSK)   TO CALC ASS             
*                                                                               
PU1AA    L     R3,NBAIO            NOW GET ACTUAL UNIT                          
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL         SET UP R2 FOR GETEL                          
         MVI   SRCHEL,X'00'                                                     
NEXTELEM BAS   RE,NEXTEL                                                        
         BNE   XITPC               LAST ELEMENT                                 
         CLI   0(R2),X'10'         BILLING ELEMENT                              
         BE    DOBILL                                                           
         CLI   0(R2),X'12'         PAYING ELEMENT                               
         BE    DOPAY                                                            
         B     NEXTELEM                                                         
*                                                                               
         USING NUBILD,R2                                                        
DOBILL   TM    NUBILST,X'20'       SKIP UNBILLED                                
         BO    NEXTELEM                                                         
         GOTO1 =V(SPBVAL),DMCB,(C'U',(R2)),WORK,0,RR=RELO                       
         LA    R1,WORK                                                          
         USING SPBVALD,R1                                                       
         MVC   NUBILGRS,SPBVEGRS                                                
         MVC   NUBILNET,SPBVENET                                                
         DROP  R1                                                               
         LA    R0,NUBILGRS         POINT TO GROSS AMOUNT                        
         CLI   NETOPT,YES          TEST FOR NET OPTION                          
         BNE   *+8                                                              
         LA    R0,NUBILNET         YES-POINT TO NET FIGURE                      
         CLI   NUBILTYP,C'T'       TIME                                         
         BNE   BILLINT                                                          
         GOTO1 ADDEM,DMCB,(R0),NBACTDAT,=AL4(BTGMSK)                            
         GOTO1 ADDEM,DMCB,NUBILNET,NBACTDAT,=AL4(BTNMSK)                        
         GOTO1 ADDTODAY,DMCB,(R0),NUBILDAT,=AL4(BTGMSK)                         
         GOTO1 ADDTODAY,DMCB,NUBILNET,NUBILDAT,=AL4(BTNMSK)                     
         B     NEXTELEM                                                         
*                                                                               
*                                  INTEG                                        
BILLINT  DS    0H                                                               
         GOTO1 ADDEM,DMCB,(R0),NBACTDAT,=AL4(BIGMSK)                            
         GOTO1 ADDEM,DMCB,NUBILNET,NBACTDAT,=AL4(BINMSK)                        
         GOTO1 ADDTODAY,DMCB,(R0),NUBILDAT,=AL4(BIGMSK)                         
         GOTO1 ADDTODAY,DMCB,NUBILNET,NUBILDAT,=AL4(BINMSK)                     
         B     NEXTELEM                                                         
         DROP  R2                                                               
*                                                                               
         USING NUPAYD,R2                                                        
DOPAY    LA    R0,NUPAYGRS         POINT AT GROSS PAID                          
         CLI   NETOPT,YES          TEST FOR NET OPTION                          
         BNE   *+8                                                              
         LA    R0,NUPAYNET         YES-USE NET PAID AS GROSS                    
         CLI   NUPAYTYP,C'T'        TIME PAY                                    
         BNE   PAYINT                                                           
         GOTO1 ADDEM,DMCB,(R0),NBACTDAT,=AL4(PTGMSK)                            
         GOTO1 ADDEM,DMCB,NUPAYNET,NBACTDAT,=AL4(PTNMSK)                        
         GOTO1 ADDTODAY,DMCB,(R0),NUPAYDAT,=AL4(PTGMSK)                         
         GOTO1 ADDTODAY,DMCB,NUPAYNET,NUPAYDAT,=AL4(PTNMSK)                     
         B     NEXTELEM                                                         
*                                                                               
*                                    INTEG                                      
PAYINT   DS    0H                                                               
         GOTO1 ADDEM,DMCB,(R0),NBACTDAT,=AL4(PIGMSK)                            
         GOTO1 ADDEM,DMCB,NUPAYNET,NBACTDAT,=AL4(PINMSK)                        
         GOTO1 ADDTODAY,DMCB,(R0),NUPAYDAT,=AL4(PIGMSK)                         
         GOTO1 ADDTODAY,DMCB,NUPAYNET,NUPAYDAT,=AL4(PINMSK)                     
         B     NEXTELEM                                                         
         DROP  R2                                                               
*                                                                               
XITPC    B     EXIT                                                             
****************************************************************                
         EJECT                                                                  
****************************************************************                
* ADDEM - ADDS COST INTO APPROPRIATE LINE                                       
* INS:   ARG1: A(FULLWORD COST)                                                 
*        ARG2: A(DATE COST INCURRED)                                            
*        ARG3: BIT MASK CORRESPONDING TO THIS COST                              
*                                                                               
ADDEM    NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)            A(BIT MASK)                                  
         L     R4,0(R4)            BIT MASK                                     
*                                                                               
         L     R5,0(R2)            GET COST                                     
         CVD   R5,DUB              PUT CONVERTED COST IN DUB                    
*                                                                               
         LA    R5,CSTMASKS                                                      
         LH    R7,NUMCOLS                                                       
         L     R8,ACURTOTS                                                      
AMLOOP   ST    R4,FULL             PUT MASK FOR THIS COST IN FULL               
         NC    FULL,0(R5)          IF MASK MATCH                                
         BZ    CKSUB                                                            
         BAS   RE,GETDATN             GET DATE OFFSET IN R2                     
         LH    R1,NUMCOLS                                                       
         SLL   R1,3                   EACH IS DOUBLEWORD                        
         STH   R1,HALF                                                          
         MH    R2,HALF                                                          
         LA    R2,0(R8,R2)                                                      
         AP    0(8,R2),DUB            ADD TO TOTALS                             
*                                                                               
CKSUB    ST    R4,FULL             IF TO BE SUBTRACTED                          
         NC    FULL,4(R5)                                                       
         BZ    NXTMSK                                                           
         BAS   RE,GETDATN             GET DATE OFFSET IN R2                     
         LH    R1,NUMCOLS                                                       
         SLL   R1,3                EACH IS DOUBLWORD                            
         STH   R1,HALF                                                          
         MH    R2,HALF                                                          
         LA    R2,0(R8,R2)                                                      
         SP    0(8,R2),DUB            DO SUBTRACTION                            
*                                                                               
NXTMSK   LA    R5,8(R5)            NEXT MASK SET                                
         LA    R8,8(R8)            NEXT SET OF COLUMNS                          
         BCT   R7,AMLOOP                                                        
         B     EXIT                                                             
****************************************************************                
         EJECT                                                                  
****************************************************************                
* RATETYPES - NBRTTYPE NOT = 0, GOTO NETNET                                     
*            NETNET RETURNS GROSS AND NET AMOUNTS                               
*            CHECK NBSDRTCV IF APPLYING RATE TYPE                               
RTTYPES  NTR1                                                                   
         LA    R5,1             COUNT OF COSTAB $ FIELDS                        
*                               1=ACT,2=ASS,3=INT,4=CALCOS(ASS OR ACT)          
         LA    R4,COSTAB           R4 POINTS TO TABLE                           
         LA    R3,COSTS            R3 POINTS TO COSTS                           
RTY2     ICM   R2,7,0(R4)          GET DISPLACEMENT TO COST                     
         LA    R2,NETBLOCK(R2)     POINT TO FIELD                               
         CLI   NBSDRTCV,C'A'       A=APPLY TO ALL COSTS                         
         BNH   RTY3                                                             
* CHECK RATE TYPE VS COST WE ARE DEALING WITH                                   
         CLI   NBSDRTCV,C'T'       T=APPLY TO TIME ONLY                         
         BNE   RTY3                                                             
         C     R5,=F'3'            IF INTEGRATION                               
         BE    RTY5                SKIP RATETYPE                                
*                                                                               
RTY3     GOTO1 =V(NETNET),DMCB,(NBRTTYPE,(R2)),RATEWRK,RR=RELO                  
         CLI   NETOPT,C'Y'         IS IT NET OPT                                
         BE    RTY4                                                             
         MVC   0(4,R2),RATEWRK     NO                                           
         B     *+10                                                             
RTY4     MVC   0(4,R2),RATEWRK+4   YES                                          
RTY5     LA    R5,1(R5)            BUMP TO NEXT COSTAB COUNT                    
         LA    R4,L'COSTAB(R4)                                                  
         BCT   R3,RTY2                                                          
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
* NETCOSTS- NET DOWN NETBLOCK COSTS FOR BUCKETS                                 
* IF USING RATETYPE, GROSS AND NET ALREADY ADJUSTED                             
*                                                                               
NETCOSTS NTR1                                                                   
         LA    R5,1                                                             
         CLI   NBRTTYPE,X'40'      APPLYING RATE TYPE?                          
         BNH   NETC1               NO                                           
*                                  1=ACT,2=ASS,3=INT,4=CALCOS(ACT/ASS)          
NETC1    LA    R4,COSTAB           R4 POINTS TO TABLE                           
         LA    R3,COSTS            R3 POINTS TO COSTS                           
NETC2    ICM   R2,7,0(R4)          GET DISPLACEMENT TO COST                     
         LA    R2,NETBLOCK(R2)     POINT TO FIELD                               
         L     R0,0(R2)            GET COST                                     
         CLI   NBRTTYPE,X'40'      RATE TYPE APPLIED?                           
         BNH   NETC5               NO                                           
         CLI   NBSDRTCV,C'T'       YES - APPLIED TO TIME ONLY?                  
         BNE   NETCX                         NO                                 
         C     R5,=F'1'                      YES/ SO SKIP TIME                  
         BE    NETC6                                                            
NETC5    BAS   RE,FINDNET          DERIVE NET COST                              
         ST    R0,0(R2)            REPLACE COST WITH NET COST                   
NETC6    LA    R5,1(R5)                                                         
         LA    R4,L'COSTAB(R4)                                                  
         BCT   R3,NETC2                                                         
NETCX    B     EXIT                                                             
         SPACE 2                                                                
* FINDNET - COMPUTES NET COST - AT ENTRY R0 CONTAINS GROSS COST                 
*                                                                               
FINDNET  DS    0H                                                               
**       SRDA  R0,32               PREPARE MULTIPLICAND                         
**       M     R0,=F'8500'                                                      
**       SLDA  R0,1                DOUBLE FOR ROUNDING                          
**       D     R0,=F'10000'                                                     
**       LTR   R1,R1                                                            
**       BM    *+8                                                              
**       AH    R1,=H'1'                                                         
**       SRA   R1,1                                                             
**       LR    R0,R1               REPLACE COST WITH NET                        
**       BR    RE                                                               
         CVD   R0,DUB                                                           
         ZAP   PAK16,DUB                                                        
         MP    PAK16,=P'8500'                                                   
         SRP   PAK16,60,5          DIVIDE BY 10000 AND ROUND                    
         ZAP   DUB,PAK16                                                        
         CVB   R0,DUB                                                           
         BR    RE                                                               
         EJECT                                                                  
****************************************************************                
* ADDTODAY- IF DATE IS TODAY, ADDS COST INTO TODAY LINE                         
*  *** MODIFIED TO HANDLE ALL SPECIAL PERIODS                                   
*********** FOR SPEC PERS DONT USE SUBTRACTION PART OF MASK                     
*********** E.G. SHOULD ONLY SHOW TODAY BILLED, ACTUAL, BILLABLE                
*********** HAVE NO MEANING                                                     
* INS:   ARG1:  A(FULLWORD COST)                                                
*        ARG2:  A(DATE COST INCURRED)                                           
*        ARG3: A(BIT MASK CORRESPONDING TO THIS COST)                           
*                                                                               
* LOCALS : DUB - COST                                                           
*          R3 - A(DATE COST INCURRED)                                           
*          BITMASK - BIT MASK                                                   
*                                                                               
ADDTODAY NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)            A(BIT MASK)                                  
         L     R4,0(R4)            BIT MASK                                     
         ST    R4,BITMASK                                                       
         L     R5,0(R2)            GET COST                                     
         CVD   R5,DUB              PUT CONVERTED COST IN DUB                    
*                                                                               
         L     R2,ASPERLST                                                      
         LH    R4,NUMSROW          NUMBER OF SPEC PERS                          
         L     R8,ACLITOD          SPECIAL PERIOD TOTALS                        
*                                                                               
CKNXTSP  CLC   0(2,R3),0(R2)       IF IN SPECIAL PER                            
         BL    NOTINSP                                                          
         CLC   0(2,R3),2(R2)                                                    
         BNH   OKINSP              MATCHES SPEC PER DATE SET                    
*                                                                               
NOTINSP  LH    R1,NUMCOLS                                                       
         SLL   R1,3                BUMP TO NEXT SPEC PER TOTAL                  
         LA    R8,0(R8,R1)                                                      
         B     NXTSP                                                            
*                                                                               
OKINSP   LA    R5,CSTMASKS                                                      
         LH    R7,NUMCOLS                                                       
ATDYLOOP MVC   FULL,BITMASK        PUT MASK FOR THIS COST IN FULL               
         NC    FULL,0(R5)          IF MASK MATCH                                
         BZ    CKTDYSUB                                                         
*                                                                               
         AP    0(8,R8),DUB            ADD TO TOTALS                             
*                                                                               
CKTDYSUB EQU   *                   LEFT OVER FROM USING SUBTRACTIONS            
NXTTDMSK LA    R5,8(R5)            NEXT MASK SET                                
         LA    R8,8(R8)            NEXT SET OF COLUMNS                          
         BCT   R7,ATDYLOOP                                                      
NXTSP    LA    R2,4(R2)            NEXT SPEC PER                                
         BCT   R4,CKNXTSP                                                       
*                                                                               
XITTODAY B     EXIT                                                             
****************************************************************                
         EJECT                                                                  
****************************************************************                
* GETDATN - FINDS THE DATE IN THE DATELIST, RETURNS THE OFFSET                  
*           NUMBER.                                                             
*                                                                               
*  INPUT: R3 - A(DATE)                                                          
*  OUTPUT: R2 - DATE NUMBER (FIRST IS 0)                                        
*                                                                               
GETDATN  NTR1                                                                   
         L     R4,FRSTMON                                                       
         SR    R2,R2                                                            
GDLOOP   CLC   0(2,R3),2(R4)                                                    
         BNH   GOTDAT                                                           
         LA    R4,4(R4)                                                         
         LA    R2,1(R2)                                                         
         B     GDLOOP                                                           
GOTDAT   XIT1  REGS=(R2)                                                        
****************************************************************                
         EJECT                                                                  
****************************************************************                
* END OF A CLIENT                                                               
*  ADD BLOCK(S) TO CLIENT TOTALS                                                
*  ADD BLOCK(S) TO AGENCY TOTALS (BY MONTH)                                     
*  PRINT EACH BLOCK                                                             
*  PRINT CLI TOTALS                                                             
******************                                                              
DUMPCLT  NTR1                                                                   
         L     R3,ACURTOTS                                                      
         BAS   RE,ADDCLT          ADD CURTOTS TO CLI TOTS (R3)                  
         BAS   RE,ADDAGY                                                        
         BAS   RE,ADDTOD           UPDATE AGENCY TODAY BUCKETS                  
*                                                                               
         CLI   PRNOCLI,0           IF PRNOCLI SET, DONT PRINT                   
         BNE   DCENDPR                                                          
*                                                                               
         L     R2,NUMMONS         ALL MONTHS                                    
         L     R7,FRSTMON                                                       
         SR    R4,R4               MONTH NUMBER                                 
*                                                                               
PMONLOOP BAS   RE,FILMONTH           PRINT MONTH BASED ON R7                    
         BAS   RE,FILPCT                                                        
         BAS   RE,PRDATLIN                                                      
*                                                                               
         LA    R4,1(R4)                                                         
         LA    R7,4(R7)                                                         
         BCT   R2,PMONLOOP            NEXT MONTH                                
*                                                                               
         SR    R4,R4                                                            
         L     R3,ACLITOTS                                                      
         L     R5,CURLINE                                                       
         USING COLINFO,R5                                                       
         MVC   CLMONTH,=C'TOTAL'                                                
         MVI   PRNTFLAG,1                                                       
         BAS   RE,FILPCT                                                        
         BAS   RE,PRDATLIN                                                      
*                                                                               
         L     R3,ACLITOD                                                       
         LH    R2,NUMSROW          DO SPECIAL PERIODS                           
         LA    R7,SPERHEAD         SPEC PER HEADERS                             
         SR    R4,R4                                                            
*                                                                               
PRCLSP   L     R5,CURLINE                                                       
         MVC   CLMONTH(10),0(R7)                                                
         DROP  R5                                                               
         BAS   RE,FILPCT                                                        
         BAS   RE,PRDATLIN                                                      
         LA    R7,10(R7)                                                        
         LA    R4,1(R4)                                                         
         BCT   R2,PRCLSP                                                        
*                                                                               
         MVI   PRNTFLAG,1                                                       
         BAS   RE,NEXTLINE                                                      
         MVI   PRNTFLAG,1          PRINT BLANK LINE                             
         BAS   RE,NEXTLINE                                                      
*                                                                               
DCENDPR  BAS   RE,INITCUR          RESET CURRENT TOTALS                         
         BAS   RE,INITCLT                                                       
         BAS   RE,INITTOD                                                       
*                                                                               
         B     EXIT                                                             
****************************************************************                
         EJECT                                                                  
****************************************************************                
* END OF AN AGENCY                                                              
*  ADD BLOCK(S) TO CLIENT TOTALS                                                
*    (USE CLIENT TOTALS BLOCK TO HOLD AGENCY TOTALS)                            
*  PRINT EACH BLOCK                                                             
*  PRINT AGY TOTALS                                                             
******************                                                              
         USING COLINFO,R5                                                       
FINAGY   NTR1                                                                   
*                                                                               
         BAS   RE,ADDDDS           ADD AGY TOTALS TO DDS TOTALS                 
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R5,P1                                                            
         MVC   CLCLI(3),=C'***'                                                 
         MVC   CLCNAM(17),=C'AGENCY TOTALS ***'                                 
         OC    NBSELAGY,NBSELAGY   DIFFERENT END LINE FOR DDS                   
         BNZ   FAG2                                                             
         MVC   CLCNAM(33),HOLDAGNM                                              
         MVC   CLCNAM+34(10),=C'TOTALS ***'                                     
         GOTO1 SQUASHER,DMCB,CLCLI,44                                           
FAG2     GOTO1 UNDERLIN,DMCB,(44,CLCLI),CLUNDER   UNDERLINE IT                  
         LA    R5,P3               SET NEXT PRINT AFTER SPACE                   
         ST    R5,CURLINE                                                       
         DROP  R8                                                               
*                                                                               
*                                                                               
         L     R3,AAGYTOTS                                                      
         BAS   RE,ADDCLT          ADD AGYTOTS TO CLITOTS BUFFER (R3)            
*                                                                               
         L     R2,NUMMONS         ALL MONTHS                                    
         L     R7,FRSTMON                                                       
         SR    R4,R4               MONTH NUMBER                                 
*                                                                               
FMONLOOP BAS   RE,FILMONTH           PRINT MONTH BASED ON R7                    
         BAS   RE,FILPCT                                                        
         BAS   RE,PRDATLIN                                                      
*                                                                               
         LA    R4,1(R4)                                                         
         LA    R7,4(R7)                                                         
         BCT   R2,FMONLOOP            NEXT MONTH                                
*                                                                               
         SR    R4,R4                                                            
         L     R3,ACLITOTS                                                      
         L     R5,CURLINE                                                       
         MVC   CLMONTH(5),=C'TOTAL'                                             
         BAS   RE,FILPCT                                                        
         BAS   RE,PRDATLIN                                                      
*                                                                               
         L     R3,AAGYTOD                                                       
         LA    R7,SPERHEAD                                                      
         SR    R4,R4                                                            
         LH    R2,NUMSROW          NUMBER OF SPEC PERS                          
*                                                                               
PRASP    L     R5,CURLINE                                                       
         MVC   CLMONTH(10),0(R7)    SPEC PER HEADER                             
         BAS   RE,FILPCT                                                        
         BAS   RE,PRDATLIN                                                      
*                                                                               
         LA    R7,10(R7)                                                        
         LA    R4,1(R4)                                                         
         BCT   R2,PRASP                                                         
*                                                                               
         CLI   PRMARGA,C'M'        IF MARGA TOTALS TO BE PRINTED                
         BNE   PRASDON                                                          
         BAS   RE,MARGALIN                                                      
*                                                                               
PRASDON  MVI   PRNTFLAG,1          PRINT BLANK LINE                             
         BAS   RE,NEXTLINE                                                      
*                                                                               
         BAS   RE,INITAGY          INITIALIZE MONTH AGENCY TOTALS               
         BAS   RE,INITCLT          INITIALIZE TOT AGY TOTALS                    
         BAS   RE,INITTODA         AND AGENCY TODAY TOTALS                      
*                                                                               
         B     EXIT                                                             
         DROP  R5                                                               
****************************************************************                
         EJECT                                                                  
****************************************************************                
* END OF A REPORT : PRINT DDS TOTALS                                            
* SUM UP MONTH TOTALS IN CLIENT TOTALS BLOCK                                    
*  PRINT EACH MONTH                                                             
*  PRINT DDS TOTALS                                                             
******************                                                              
         USING COLINFO,R5                                                       
FINDDS   NTR1                                                                   
*                                                                               
         OC    NBSELAGY,NBSELAGY   ONLY PRINT FOR DDS (ALL AGY REPORT)          
         BNZ   XITFDDS                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         LA    R5,P1                                                            
         MVC   P2+30(10),=C'DDS TOTALS'   MOVE IN AGENCY NAME                   
         GOTO1 UNDERLIN,DMCB,(10,P2+30),(C'*',P1+30)  OVERLINE IT               
         GOTO1 UNDERLIN,DMCB,(10,P2+30),(C'*',P3+30)  UNDERLINE IT              
**       GOTO1 SPOOL,DMCB,(R8)          PRINT WHATS THERE                       
**       GOTO1 SPOOL,DMCB,(R8)          PRINT BLANK LINE                        
         BAS   R7,PRINTIT                                                       
         BAS   R7,PRINTIT                                                       
         ST    R5,CURLINE                                                       
         DROP  R8                                                               
*                                                                               
*                                                                               
         L     R3,ADDSTOTS                                                      
         BAS   RE,ADDCLT          ADD AGYTOTS TO CLITOTS BUFFER (R3)            
*                                                                               
         L     R2,NUMMONS         ALL MONTHS                                    
         L     R7,FRSTMON                                                       
         SR    R4,R4               MONTH NUMBER                                 
*                                                                               
DDSMLOOP BAS   RE,FILMONTH           PRINT MONTH BASED ON R7                    
         BAS   RE,FILPCT                                                        
         BAS   RE,PRDATLIN                                                      
*                                                                               
         LA    R4,1(R4)                                                         
         LA    R7,4(R7)                                                         
         BCT   R2,DDSMLOOP            NEXT MONTH                                
*                                                                               
         SR    R4,R4                                                            
         L     R3,ACLITOTS                                                      
         L     R5,CURLINE                                                       
         MVC   CLMONTH,=C'TOTAL'                                                
         BAS   RE,FILPCT                                                        
         BAS   RE,PRDATLIN                                                      
*                                                                               
         L     R3,ADDSTOD                                                       
         LH    R2,NUMSROW          NUM SPECIAL PERIODS                          
         LA    R7,SPERHEAD                                                      
         SR    R4,R4                                                            
PRFSP    L     R5,CURLINE                                                       
         MVC   CLMONTH(10),0(R7)                                                
         BAS   RE,FILPCT                                                        
         BAS   RE,PRDATLIN                                                      
*                                                                               
         LA    R7,10(R7)                                                        
         LA    R4,1(R4)                                                         
         BCT   R2,PRFSP                                                         
*                                                                               
         MVI   PRNTFLAG,1          PRINT BLANK LINE                             
         BAS   RE,NEXTLINE                                                      
*                                                                               
         CLI   PRMARGA,C'M'        IF MARGA OPTION                              
         BNE   XITFDDS               PRINT DDS BILLING                          
*                                                                               
         L     R5,CURLINE                                                       
         LA    R5,CLPERCL                                                       
         MVC   0(17,R5),=C'TOTAL DDS BILLING'                                   
         MVC   22(5,R5),=C'IS  $'                                               
         EDIT  (P8,MARGTOT),(10,28(R5)),2,FLOAT=-,ZERO=BLANK                    
         MVI   PRNTFLAG,1          FLUSH BUFFER                                 
         BAS   RE,NEXTLINE                                                      
*                                                                               
XITFDDS  B     EXIT                                                             
         DROP  R5                                                               
****************************************************************                
         EJECT                                                                  
****************************************************************                
*   PRDATLIN - PRINTS THE CURRENT DATE LINE                                     
*                                                                               
*  INPUTS:  R3 - CURRENT TOTAL AREA (IN MONTHS AND COLUMNS)                     
*           R4 - MONTH NUMBER                                                   
*                                                                               
*  LOCALS:                                                                      
*           R5 - POINTS AT PRINT AREA FOR THIS COST                             
*                                                                               
         USING COLINFO,R5                                                       
PRDATLIN NTR1                                                                   
         L     R5,CURLINE                                                       
         LA    R5,CLCOSTS                                                       
*                                                                               
         LH    R1,NUMCOLS                                                       
         SLL   R1,3                DOUBLEWORDS                                  
         STH   R1,HALF                                                          
         MH    R4,HALF             POINT TO CURRENT DATE LINE                   
         LA    R3,0(R3,R4)                                                      
*                                                                               
         LH    R8,NUMCOLS             DO FOR EACH COLUMN                        
PRCOLLOP CLC   0(8,R3),=PL8'0'           IF ZERO DONT PRINT                     
         BE    PRCO2                                                            
         EDIT  (P8,0(R3)),(15,0(R5)),2,COMMAS=YES,FLOAT=-                       
*                                                                               
         CLC   =X'0000',0(R3)            ARE WE INTO BILLIONS                   
         BE    PRCO1                                                            
         UNPK  WORK(15),0(8,R3)            YES                                  
         MVC   BYTE,WORK+14        SAVE SIGN                                    
         OI    WORK+14,X'F0'                                                    
         MVC   13(2,R5),WORK+13                                                 
         MVI   12(R5),C'.'                                                      
         MVC   0(11,R5),WORK+1                                                  
         LA    R1,11                                                            
         LR    RE,R5                                                            
         MVI   DUB,0                                                            
PRCOO    CLI   0(RE),X'F0'         DROP LEADING ZEROS                           
         BNE   PRCOO3                                                           
         MVI   0(RE),X'40'         GOT ONE/LOOP HERE UNTIL NO MORE              
         LA    RE,1(RE)                                                         
         BCT   R1,PRCOO                                                         
PRCOO3   TM    BYTE,X'D0'          IS IT NEGATIVE                               
         BNO   PRCO2                                                            
         LR    R1,R5               YES/SET MINUS 1ST SIGNIFICANT DIGIT          
         CLI   1(R1),X'40'                                                      
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         MVI   0(R1),C'-'                                                       
         B     PRCO2                                                            
         EJECT                                                                  
*                                                                               
PRCO1    CLI   0(R5),X'40'         IF NUMBER IS TOO BIG                         
         BE    PRCO2                                                            
         XC    0(15,R5),0(R5)      CLEAR AREA AND EDIT WITHOUT COMMAS           
         EDIT  (P8,0(R3)),(14,0(R5)),2,FLOAT=-                                  
PRCO2    LA    R3,8(R3)                                                         
         LA    R5,COSTLEN(R5)                                                   
         BCT   R8,PRCOLLOP            NEXT COLUMN                               
*                                                                               
         CLI   PRZLFLG,1           IF BLANK LINES TO BE PRINTED                 
         BE    PRINMON               GO RIGHT TO PRINT LINE                     
         L     R5,CURLINE          CHECK FOR BLANK LINE                         
         LA    R5,CLCOSTS                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
***      CLC   0(CLCOSTL,R5),CKSPACES                                           
         CLC   0(CLCOSTL,R5),SPACES                                             
         BNE   PRINMON                                                          
         L     R5,CURLINE                                                       
***      MVC   CLHEADS,CKSPACES                                                 
         MVC   CLHEADS,SPACES                                                   
         CLI   PRNTFLAG,0                                                       
         BE    PRDTXIT                                                          
PRINMON  MVI   CLIFLAG,1                                                        
         BAS   RE,NEXTLINE                                                      
*                                                                               
PRDTXIT  MVI   PRNTFLAG,0                                                       
         B     EXIT                                                             
         DROP  R5,R8                                                            
EDPATRN  DC    X'40202020202020202020202020214B2020'                            
************************************************************                    
         EJECT                                                                  
****************************************************************                
*   MARGALIN - PRINTS MARGAS DDS BILLING PCTG LINE                              
*                                                                               
*  INPUTS:  R3 - CURRENT TOTAL AREA (IN MONTHS AND COLUMNS)                     
*                                                                               
*  LOCALS:                                                                      
*           R5 - POINTS AT PRINT AREA FOR THIS COST                             
*                                                                               
         USING COLINFO,R5                                                       
MARGALIN NTR1                                                                   
         LA    R7,SPERHEAD                                                      
         SR    R4,R4                                                            
         LH    R2,NUMSROW          NUMBER OF SPEC PERS                          
*                                                                               
MARLOP   CLC   0(3,R7),=C'CUR'      FIND CUR MONTH TOTAL LINE                   
         BE    GOTMM                                                            
         LA    R7,10(R7)                                                        
         LA    R4,1(R4)                                                         
         BCT   R2,MARLOP                                                        
*                                                                               
GOTMM    LH    R1,NUMCOLS                                                       
         SLL   R1,3                DOUBLEWORDS                                  
         STH   R1,HALF                                                          
         MH    R4,HALF             POINT TO CURRENT DATE LINE                   
         LA    R3,0(R3,R4)                                                      
*                                                                               
         L     R5,CURLINE                                                       
         LA    R5,CLPERCL                                                       
         MVC   0(16,R5),=C'DDS BILLING  AT '                                    
         MVC   17(2,R5),HOLDAGBP                                                
         MVI   19(R5),C'%'                                                      
         MVC   22(5,R5),=C'IS  $'                                               
*                                                                               
*                                                                               
         ZIC   R8,MBILCOL             GET COLUMN MARGA BILLS OFF                
         SLL   R8,3                      (EACH IS DOUBLEWORD)                   
         LA    R3,0(R8,R3)            R3 POINTS TO PROPER COLUMN                
         XC    PCTCLRWK,PCTCLRWK     USE PCTCLRWK AS TEMP STORAGE               
         MVC   PCTCLRWK+8(8),0(R3)   PCT =BILLGROSS/BILLPCT                     
         PACK  HALF,HOLDAGBP                                                    
         MP    PCTCLRWK,HALF                                                    
         AP    PCTCLRWK,=P'5000'     ROUND AND DO AS PCTG                       
         DP    PCTCLRWK,=P'10000'                                               
*                                                                               
         EDIT  (P6,PCTREM-1),(10,28(R5)),2,FLOAT=-,ZERO=BLANK                   
         AP    MARGTOT,PCTREM-1(6)      ADD TOT TOTAL BILLING                   
*                                                                               
         BAS   RE,NEXTLINE         PRINT IT                                     
*                                                                               
         B     EXIT                                                             
         DROP  R5                                                               
************************************************************                    
         EJECT                                                                  
****************************************************************                
*   PERFORMS CLIENT TOTALS. ADDS EACH LINE OF CURRENT TOTALS TO                 
*       CLIENT TOTALS                                                           
*  INPUT  : R3 - POINTS AT AREA TO SUM INTO CLIENT TOTALS                       
*                                                                               
*  LOCALS:  R4 - POINTS AT COST SUM AREA FOR THIS CLIENT, COLUMNN               
*           R2 - COUNTER FOR MONTHS                                             
*           R8 - COUNTER FOR COLUMNS                                            
*                                                                               
ADDCLT   NTR1                                                                   
         L     R2,NUMMONS         ALL MONTHS                                    
DMONLOOP L     R4,ACLITOTS         DO FOR EACH MONTH                            
         LH    R8,NUMCOLS             DO FOR EACH COLUMN                        
DCOLLOOP AP    0(8,R4),0(8,R3)          ADD COLUMN INTO CLT TOTAL               
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R8,DCOLLOOP            NEXT COLUMN                               
         BCT   R2,DMONLOOP                                                      
*                                                                               
         B     EXIT                                                             
************************************************************                    
         EJECT                                                                  
****************************************************************                
*   PERFORMS AGY TOTALS (BY MONTH)                                              
*                                                                               
*  LOCALS : R3 - POINTS AT COST SUM AREA FOR CURRENT DATE, COLUMN               
*           R2 - COUNTER FOR MONTHS                                             
*           R8 - COUNTER FOR COLUMNS                                            
*           R7 - POINTS AT COST SUM AREA FOR CURRENT AGENCY, DATE, COL          
*                                                                               
ADDAGY   NTR1                                                                   
         L     R3,ACURTOTS                                                      
         L     R7,AAGYTOTS                                                      
         L     R2,NUMMONS         ALL MONTHS                                    
AMONLOOP LH    R8,NUMCOLS             DO FOR EACH COLUMN                        
ACOLLOOP AP    0(8,R7),0(8,R3)          ADD COLUMN INTO AGY TOTALS              
         LA    R3,8(R3)                                                         
         LA    R7,8(R7)                                                         
         BCT   R8,ACOLLOOP            NEXT COLUMN                               
         BCT   R2,AMONLOOP                                                      
*                                                                               
         B     EXIT                                                             
************************************************************                    
         EJECT                                                                  
****************************************************************                
*   ADDS AGY TOTALS TO DDS TOTALS                                               
*     FIRST PART IS FOR MONTHS, SECOND PART IS FOR TODAY                        
*                                                                               
*  LOCALS : R3 - POINTS AT COST SUM AREA FOR CURRENT DATE, COLUMN               
*           R2 - COUNTER FOR MONTHS                                             
*           R8 - COUNTER FOR COLUMNS                                            
*           R7 - POINTS AT DDS SUM AREA FOR CURRENT DATE, COL                   
*                                                                               
ADDDDS   NTR1                                                                   
         OC    NBSELAGY,NBSELAGY   ONLY DO IF DDS ALL AGY SUMMARY               
         BNZ   XITADDS                                                          
         L     R3,AAGYTOTS                                                      
         L     R7,ADDSTOTS                                                      
         L     R2,NUMMONS         ALL MONTHS                                    
DDSALOOP LH    R8,NUMCOLS             DO FOR EACH COLUMN                        
DDSCLOOP AP    0(8,R7),0(8,R3)          ADD COLUMN INTO DDS TOTALS              
         LA    R3,8(R3)                                                         
         LA    R7,8(R7)                                                         
         BCT   R8,DDSCLOOP            NEXT COLUMN                               
         BCT   R2,DDSALOOP                                                      
*                                                                               
*             R2 COUNTER FOR SPECIAL PERIOD                                     
*             R3 POINTS TO AGENCY SPEC PER TOTALS                               
*             R7 POINTS TO DDS SPEC PER TOTALS                                  
*             R8 COUNTER FOR COLUMNS                                            
*                                                                               
         L     R3,AAGYTOD                                                       
         L     R7,ADDSTOD                                                       
         LH    R2,NUMSROW                                                       
DDSPLOOP LH    R8,NUMCOLS                                                       
DDSTLOOP AP    0(8,R7),0(8,R3)                                                  
         LA    R3,8(R3)                                                         
         LA    R7,8(R7)                                                         
         BCT   R8,DDSTLOOP                                                      
         BCT   R2,DDSPLOOP                                                      
XITADDS  B     EXIT                                                             
************************************************************                    
         EJECT                                                                  
************************************************************                    
*     ADDS CLIENT SPEC PER TOTALS TO AGENCY SPEC PER  TOTALS                    
*                                                                               
*     LOCALS  R3 POINTS TO CLIENT SPEC PER TOTALS                               
*             R7 POINTS TO AGENCY SPEC PER TOTALS                               
*             R8 COUNTER FOR COLUMNS                                            
*             R2 COUNTER FOR NUMBER OF SPEC PERS                                
ADDTOD   NTR1                                                                   
         L     R3,ACLITOD                                                       
         L     R7,AAGYTOD                                                       
         LH    R2,NUMSROW                                                       
TSPLOOP  LH    R8,NUMCOLS                                                       
TCOLLOOP AP    0(8,R7),0(8,R3)                                                  
         LA    R3,8(R3)                                                         
         LA    R7,8(R7)                                                         
         BCT   R8,TCOLLOOP                                                      
         BCT   R2,TSPLOOP                                                       
         B     EXIT                                                             
****************************************************************                
         EJECT                                                                  
****************************************************************                
*  FILLMONTH - FILL IN CLMONTH BASED ON THE CURRENT PERIOD IN R7                
*                                                                               
         USING COLINFO,R5                                                       
FILMONTH NTR1                                                                   
         L     R5,CURLINE                                                       
*                                                                               
DM1      CLC   0(2,R7),MINDAT      IF START OF PERIOD IS FIRST                  
         BNE   DM2                    THEN THIS IS PRIOR                        
         MVC   CLMONTH,=C'PRIOR'                                                
         B     XITFM                                                            
DM2      CLC   2(2,R7),MAXDAT      IF END OF PERIOD IS LAST                     
         BNE   DM4                    THEN THIS IS AFTER                        
         MVC   CLMONTH,=C'AFTER'                                                
         B     XITFM                                                            
*                  ***** OTHERWISE LOOK UP MONTH NAME.                          
DM4      GOTO1 DATCON,DMCB,(2,0(R7)),(4,CLMONTH)                                
         MVI   CLDASH,C'-'                                                      
         GOTO1 DATCON,DMCB,(2,2(R7)),(8,CLMON2)   INCLUDE END YR                
*                                                                               
XITFM    B     EXIT                                                             
         DROP  R5                                                               
****************************************************************                
         EJECT                                                                  
****************************************************************                
*  BUILD THE MONTH LIST, SET VALUES BASED ON CONTROLS.                          
*   OUTPUTS: EBSTRTDT,EBENDDT - EBCDIC START, END DATES FOR HEADER              
*            NBSELSTR,NBSELEND - DATES TO BE USED FOR FILTERING                 
*            MONLIST - LIST OF BEGIN-END DATE SETS FOR PERIOD.                  
*            FRSTMON  - A(FIRST USABLE DATE SET IN LIST)                        
*            LASTMON  - A(LAST USABLE DATE SET IN LIST)                         
*            NUMMONS - NUMBER OF USABLE DATE SETS IN LIST                       
*   LOCALS:                                                                     
*        R3 - A(FIRST DATE SET IN LIST)                                         
*        R4 - CURRENT NUMBER OF USABLE DATE SETS IN LIST                        
*                                                                               
INITMON  NTR1                                                                   
         MVI   NREPTYP,C'A'        ID AS ACCOUNTING REPORT                      
         XC    PERTYPE,PERTYPE                                                  
         MVI   PERTYPE,C'M'        USE MONTHS                                   
         L     R4,MAXMONTS                                                      
         ST    R4,NUMMONS          MAX NUMBER OF MONTHS FOR GETLST              
*                                                                               
         L     R8,ASPOOLD          GET SYSTEM TODAY DATE                        
         USING SPOOLD,R8                                                        
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY)       GET TODAYS DATE.          
         DROP  R8                                                               
*                                                                               
         L     R2,ASPERLST         SPECIAL PERIOD LIST                          
         MVC   0(2,R2),TODAY       CURRENTLY TODAY IS ONLY SPEC PER             
         MVC   2(2,R2),TODAY                                                    
         MVC   SPERHEAD(10),=CL10'TODAY '                                       
         MVC   NUMSROW,=H'1'       NUM SPEC PERS                                
*                                                                               
         CLC   NBSELAGY,=C'JW'     FOR JW GIVE EM THE REST                      
         BE    *+14                                                             
         OC    NBSELAGY,NBSELAGY   ONLY TODAY FOR NON-DDS                       
         BNZ   IM1                                                              
*                                                                               
         LA    R2,4(R2)            2ND SPECIAL PERIOD OF LIST                   
         NETGO NDFSTDAY,DMCB,TODAY,0(R2)   GET FIRST OF MONTH                   
         NETGO NDLSTDAY,DMCB,TODAY,2(R2)   GET LAST OF MONTH                    
         MVC   SPERHEAD+10(10),=CL10'CUR MONTH'                                 
         MVC   NUMSROW,=H'2'       NUM SPEC PERS                                
*                                                                               
         LA    R2,4(R2)            3RD SPECIAL PERIOD OF LIST                   
         NETGO NDFSTMON,DMCB,TODAY,0(R2)   GET FIRST OF YEAR                    
         MVC   2(2,R2),TODAY               GET TODAY                            
         MVC   SPERHEAD+20(10),=CL10'YTD'                                       
         MVC   NUMSROW,=H'3'       NUM SPEC PERS                                
*                                                                               
IM1      L     R5,AMONLIST                                                      
         NETGO  NVWKLBEF,DMCB,NUMMONS,(R5),PERTYPE                              
********  RETURNS MONLIST AND NUMMONS IN LIST                                   
*                                                                               
         L     R4,NUMMONS          UPDATED NUMBER OF DATE SETS                  
         L     R3,AMONLIST                                                      
*                                                                               
         OC    PRBEFFLG,PRBEFFLG   IF NO BEFORES REQUIRED                       
         BNZ   IM2                                                              
         LA    R3,4(R3)            THEN IGNORE FIRST SET OF DATES               
         BCTR  R4,0                ONE LESS DATE IN LIST                        
         MVC   NBCMPSTR,0(R3)      RESET START DATE                             
IM2      OC    PRAFTFLG,PRAFTFLG   IF NO AFTERS REQUIRED                        
         BNZ   IM3                                                              
         BCTR  R4,0                                                             
         B     IM4                                                              
IM3      MVC   NBCMPEND,MAXDAT     DONT LIMIT END DATE                          
*                                                                               
IM4      DS    0H                  NOW STORE VALUES                             
         ST    R3,FRSTMON                                                       
         ST    R4,NUMMONS                                                       
         GOTO1 DATCON,DMCB,(2,0(R3)),(0,NBSELSTR)                               
IM6      BCTR  R4,0                DECREMENT NUMMONS TO GET END ADDRESS         
         SLL   R4,2                MULT BY 4 TO GET OFFSET INTO LIST            
         L     R1,FRSTMON                                                       
         LA    R4,0(R1,R4)         ADDRESS OF LAST DATE SET                     
         ST    R4,LASTMON                                                       
IMXIT    B     EXIT                                                             
****************************************************************                
         EJECT                                                                  
****************************************************************                
****** INITIALIZATION ROUTINES                                                  
*                                                                               
* INITIALIZE THE CURRENT SUMS TO PACKED ZEROES                                  
INITCUR  NTR1                                                                   
         MVI   CURFLAG,0           ALL ZEROS                                    
         L     R2,NUMMONS          GET NUM OF DUBWORDS TO ZAP IN R2             
         MH    R2,NUMCOLS                                                       
         L     R3,ACURTOTS                                                      
*                                                                               
ZAPCUR   ZAP   0(8,R3),ZERO                                                     
         LA    R3,8(R3)            NEXT DOUBLEWORD                              
         BCT   R2,ZAPCUR                                                        
*                                                                               
         B     EXIT                                                             
*                                                                               
* INITIALIZE THE DDS SUMS TO PACKED ZEROES                                      
INITDDS  NTR1                                                                   
         OC    NBSELAGY,NBSELAGY   ONLY INIT IF DDS SUMMARY                     
         BNZ   XITDDS                                                           
         L     R2,NUMMONS          GET NUM OF DUBWORDS TO ZAP IN R2             
         MH    R2,NUMCOLS                                                       
         L     R3,ADDSTOTS                                                      
*                                                                               
ZAPDDS   ZAP   0(8,R3),ZERO                                                     
         LA    R3,8(R3)            NEXT DOUBLEWORD                              
         BCT   R2,ZAPDDS                                                        
*                                                                               
XITDDS   B     EXIT                                                             
*                                                                               
***                                                                             
***** INITIALIZE CLIENT TOTALS                                                  
*                                                                               
INITCLT  NTR1                                                                   
         MVI   CLIFLAG,0           ALL ZEROS                                    
         LH    R2,NUMCOLS          NUMBER OF DOUBLEWORDS TO ZAP                 
         L     R3,ACLITOTS                                                      
*                                                                               
ZAPCLT   ZAP   0(8,R3),ZERO                                                     
         LA    R3,8(R3)            NEXT DOUBLEWORD                              
         BCT   R2,ZAPCLT                                                        
*                                                                               
         B     EXIT                                                             
*                                                                               
***                                                                             
******* INITIALIZE CLIENT SPEC PER TOTALS                                       
*                                                                               
INITTOD  NTR1                                                                   
         LH    R2,NUMCOLS                                                       
         MH    R2,NUMSROW          NUM SPEC PERS                                
         L     R3,ACLITOD                                                       
ZAPTOD   ZAP   0(8,R3),ZERO                                                     
         LA    R3,8(R3)                                                         
         BCT   R2,ZAPTOD                                                        
*                                                                               
         B     EXIT                                                             
***                                                                             
******* INITIALIZE AGENCY SPECIAL PERIOD TOTALS                                 
*                                                                               
INITTODA NTR1                                                                   
         LH    R2,NUMCOLS                                                       
         MH    R2,NUMSROW          NUMBER OF SPECIAL PERIODS                    
         L     R3,AAGYTOD                                                       
ZAPTODA  ZAP   0(8,R3),ZERO                                                     
         LA    R3,8(R3)                                                         
         BCT   R2,ZAPTODA                                                       
*                                                                               
         B     EXIT                                                             
***                                                                             
******* INITIALIZE DDS SPECIAL PERIODS                                          
*                                                                               
INITDTOD NTR1                                                                   
         OC    NBSELAGY,NBSELAGY   ONLY INIT IF DDS ALL AGY SUMMARY             
         BNZ   XITTDDS                                                          
         LH    R2,NUMCOLS                                                       
         MH    R2,NUMSROW          NUMBER OF SPECIAL PERIODS                    
         L     R3,ADDSTOD                                                       
ZAPDTOD  ZAP   0(8,R3),ZERO                                                     
         LA    R3,8(R3)                                                         
         BCT   R2,ZAPDTOD                                                       
*                                                                               
XITTDDS  B     EXIT                                                             
***                                                                             
***** INITIALIZE AGENCY TOTALS                                                  
*                                                                               
INITAGY  NTR1                                                                   
         L     R2,NUMMONS          GET NUM DUBWORDS TO ZAP IN R2                
         MH    R2,NUMCOLS                                                       
         L     R3,AAGYTOTS                                                      
*                                                                               
ZAPAGY   ZAP   0(8,R3),ZERO                                                     
         LA    R3,8(R3)            NEXT DOUBLEWORD                              
         BCT   R2,ZAPAGY                                                        
*                                                                               
         B     EXIT                                                             
*                                                                               
****************************************************************                
         EJECT                                                                  
***************************************************************                 
* NEXTLINE - UPDATES CURRENT PRINT LINE. IF BUFFER FULL, PRINTS IT              
*  AND RE-INITIALIZES.                                                          
*     ARGS:                                                                     
*           PRINTFLAG - IF ONE LINE IS PRINTED IMMEDIATELY,                     
*                   OTHERWISE ONLY PRINTED WHEN PRINTLINES FULL.                
*                   SET TO ZERO ON EXIT.                                        
*      OUTPUT: CURLINE - A(CURRENT PRINT LINE)                                  
*                                                                               
NEXTLINE NTR1                                                                   
*                                                                               
*                                    GET NEXT PRINT LINE.                       
         L     R5,CURLINE                                                       
         USING COLINFO,R5                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   PRNTFLAG,0                                                       
         BNE   DOPRNT                                                           
         LA    R1,P4                                                            
         CR    R5,R1                 IF USED LAST PRINT LINE                    
         BL    INCLINE                                                          
**DOPRNT   GOTO1 SPOOL,DMCB,(R8)          PRINT WHATS THERE                     
DOPRNT   BAS   R7,PRINTIT               PRINT WHATS THERE                       
         LA    R5,P1                    POINT R5 TO FIRST PRINT LINE            
         B     XITPRNT                                                          
*                                    ELSE                                       
INCLINE  LA    R5,132(R5)               NEXT PRINTLINE                          
*                                    FI                                         
XITPRNT  ST    R5,CURLINE                                                       
         MVI   PRNTFLAG,0            RESET PRNTFLAG                             
         B     EXIT                                                             
         DROP  R5                                                               
         DROP  R8                                                               
****************************************************************                
         EJECT                                                                  
***************************************************************                 
**** FILPCT - FILLS IN PERCENTAGE FIELD                                         
*                                                                               
*  INPUTS:  R3 - CURRENT TOTAL AREA (IN MONTHS AND COLUMNS)                     
*           R4 - MONTH NUMBER                                                   
FILPCT   NTR1                                                                   
         L     R5,CURLINE                                                       
         USING COLINFO,R5                                                       
*                                                                               
         LH    R1,NUMCOLS                                                       
         SLL   R1,3                DOUBLEWORDS                                  
         STH   R1,HALF                                                          
         MH    R4,HALF             POINT TO CURRENT DATE LINE                   
         LA    R3,0(R3,R4)                                                      
*                                                                               
         ZIC   R1,PCTDIVA          GET OFFSET FOR DIVA IN R1                    
         SLL   R1,3                FIELDS ARE 8 BYTES                           
         LA    R1,0(R1,R3)                                                      
         CP    0(8,R1),=P'0'       IF ZERO DONT PRINT                           
         BE    FPXIT                                                            
         XC    PCTCLRWK,PCTCLRWK                                                
         MVC   PCTCLRWK+8(8),0(R1)   PCT =DIVIDEND*1000.00/DIVISOR              
         MP    PCTCLRWK,=P'10000'                                               
         ZIC   R1,PCTDIVB          GET OFFSET FOR DIVB IN R1                    
         SLL   R1,3                FIELDS ARE 8 BYTES                           
         LA    R1,0(R1,R3)                                                      
         CP    0(8,R1),=P'0'       CHECK FOR 0 DIVISOR                          
         BE    FPXIT                                                            
*                                                                               
FPDIV    DP    PCTCLRWK,0(8,R1)                                                 
         MVI   CLPCTG,C'%'         NON-ZERO PCTG                                
*                                                                               
FPED     EDIT  (P6,PCTLST6),(6,CLPERCL),2,ZERO=BLANK                            
FPXIT    B     EXIT                                                             
**************************************************************                  
         DROP  R5                                                               
         EJECT                                                                  
* SET MAXIOCTR TO 90% OF MAX IO'S SHOWN BY GETFACT                              
* CHECK IF I/O OVER                                                             
* RETURN VIA R4OVER                                                             
CHKMAXIO DS    0H                                                               
         LR    R0,R4                                                            
                                                                                
         L     R5,ACOMFACS         GET MAX IO                                   
         USING COMFACSD,R5                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
                                                                                
         CLI   MAXIOCTR,0          HAVE WE SET MAXIO ?                          
         BNE   MAX20                           YES                              
         MVC   MAXIOCTR,FATMAXIO-FACTSD(R1)    NO                               
         SR    R2,R2                                                            
         LH    R3,MAXIOCTR                                                      
         LA    R4,9                MULTIPLY MAX IO BY 9                         
         MR    R2,R4                                                            
         LA    R4,10               DIVIDE MAXIMUM BY 10                         
         DR    R2,R4                                                            
         STH   R3,MAXIOCTR                                                      
         B     MAXOK                                                            
MAX20    DS    0H                                                               
         CLC   MAXIOCTR,FATIOCNT-FACTSD(R1)   MAXED OUT ?                       
         BH    MAXOK                                                            
                                                                                
         L     R8,ATWA                                                          
         USING T31EFFD,R8                                                       
         MVC   CONHEAD(38),=C'*-IO TIMEOUT REDUCE REQUEST PARAMETERS'           
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2,DMCB                                                      
         B     EXIT                                                             
                                                                                
MAXOK    LR    R4,R0                                                            
         BR    R4                                                               
         DROP  R5,R8                                                            
                                                                                
MAXIOCTR DS    H                                                                
                                                                                
         EJECT                                                                  
HOOK     NTR1                                                                   
         BRAS  RE,HOOKIT                                                        
         XIT1                                                                   
***********************************************************                     
         EJECT                                                                  
***********************************************************                     
* GETEL,NEXTEL MACRO DEFINITION                                                 
*                                                                               
         GETEL R2,NBDTADSP,SRCHEL                                               
***********************************************************                     
         EJECT                                                                  
*********************************                                               
* RETURN VIA R7                                                                 
PRINTIT  DS    0H                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BR    R7                                                               
         DROP  R8                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
******* CONSTANTS                                                               
*                                                                               
EFFEFFS  DC    50XL1'FF'           HEX FF'S TO COMPARE VS. END OF TBL           
ZEROES   DC    50XL1'00'           HEX ZEROES                                   
ZERO     DC    XL1'0F'             PACKED ZERO                                  
MINDAT   DC    XL2'0000'           MINIMUM COMPRESSED DATE                      
MAXDAT   DC    XL2'FFFF'           MAXIMUM COMPRESSED DATE                      
***CKSPACES DC    132CL1' '           SPACES                                    
         DS    0D                                                               
PAK16    DS    PL16                                                             
*                                                                               
* TABLE OF NETBLOCK COST FIELDS TO BE NETTED DOWN                               
*                                                                               
COSTAB   DS    0AL3                                                             
         DC    AL3(NBACTUAL-NETBLOCK)                                           
         DC    AL3(NBASSIGN-NETBLOCK)                                           
         DC    AL3(NBINTEG-NETBLOCK)                                            
         DC    AL3(NBCALCOS-NETBLOCK)                                           
COSTS    EQU   (*-COSTAB)/L'COSTAB                                              
*                                                                               
OUTAREA  DS    CL400                                                            
PRDEXTBL DS    CL200               ALLOWS FOR 200 PROD                          
PRDINTBL DS    CL200               ALLOWS FOR 200 PROD                          
*                                                                               
**************************************************************                  
* HOOK - BUILDS THE HEADER. SUPPLEMENTS THE SSPECS IN NEMED50                   
************************************************************                    
HOOKIT   NTR1  BASE=*,LABEL=*                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
* MAIN HEADER INFO                                                              
         LA    R5,HEAD1            BASE ADDRESS FOR OFFSETS                     
         USING PHEAD,R5                                                         
         CLI   NBSELMFL,0          ANY MEDIA FILTERING?                         
         BNE   *+10                                                             
         MVC   H1+6(9),=C'ALL MEDIA'                                            
         CLI   NBSELMFL,C'C'                                                    
         BNE   *+10                                                             
         MVC   H1+6(5),=C'CABLE'                                                
         CLI   NBSELMFL,C'O'                                                    
         BNE   *+10                                                             
         MVC   H1+6(5),=C'OTHER'                                                
         CLI   NBSELMFL,C'D'       RADIO?                                       
         BNE   *+10                                                             
         MVC   H1+6(10),=C'NTWK RADIO'                                          
         CLI   NBSELMFL,C'S'                                                    
         BNE   *+10                                                             
         MVC   H1+6(11),=C'SYNDICATION'                                         
         CLI   NBSELMFL,C'N'                                                    
         BNE   *+10                                                             
         MVC   H1+6(7),=C'NETWORK'                                              
*                                                                               
HOOK20   MVC   PHNET,NBSELNET                                                   
         OC    NBSELNET,NBSELNET                                                
         BNZ   *+10                                                             
         MVC   PHNET,=CL4'ALL '                                                 
*                                                                               
         CLI   NETOPT,YES          TEST FOR NET DOLLARS OPTION                  
         BNE   *+10                                                             
         MVC   PHNETDOL,=C'(NET DOLLARS)'                                       
         CLI   NBSELOFF,0                                                       
         BE    PRCOLS                                                           
         LA    R5,H7                                                            
         USING COLINFO,R5                                                       
         CLI   OFFSAV,0                                                         
         BE    PRCOLS                                                           
*************************************************************                   
         MVC   CLMONTH(6),=C'OFFICE'                                            
******** MVC   CLMONTH+7(1),OFFSAV                                              
         LA    R4,DUB                                                           
         USING OFFICED,R4                                                       
         XC    0(OFCLENQ,R4),0(R4)                                              
*****    MVI   OFCSYS,C'N'                                                      
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,OFFSAV                                                    
         GOTO1 AOFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                        
         MVC   CLMONTH+7(2),OFCOFC2                                             
         DROP  R4                                                               
***************************************************************                 
*                                                                               
* COLUMN HEADERS                                                                
PRCOLS   LA    R5,H9               BASE ADDRESS FOR PROPER OFFSETS              
         USING COLINFO,R5                                                       
         MVC   CLMONTH(5),=C'MONTH'                                             
         MVC   CLPERCL(7),=C'PERCENT'                                           
*                                                                               
         LA    R5,132(R5)          SECOND LINE                                  
*                                                                               
         MVC   CLMONTH(5),=C'-----'                                             
         MVC   CLPERCL(7),PCTHEAD     PCT HEADER S                              
*                                                                               
         LA    R5,H9               FIRST LINE AGAIN                             
*                                                                               
         LH    R3,NUMCOLS          MOVE COST HEADERS                            
         LA    R4,CSTHEADS                                                      
         LA    R5,CLCOSTS                                                       
         LA    R5,COSTLEN-10(R5)    OFFSET FOR PROPER ALIGNMENT                 
NXTCHEAD MVC   0(10,R5),0(R4)                                                   
         GOTO1 UNDERLIN,DMCB,(10,0(R5)),132(R5)   UNDERLINE IT                  
         LA    R5,COSTLEN(R5)                                                   
         LA    R4,10(R4)                                                        
         BCT   R3,NXTCHEAD                                                      
         DROP  R8                                                               
         XIT1                                                                   
*&&DO                                                                           
*        GET 56K FOR FULL TRACK READ                                            
GET56K   NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'                                                     
         BNE   GT56X                                                            
         GETMAIN EU,LV=57000,A=ANSWADD                                          
         LTR   RF,RF                                                            
         BNZ   GT56X                                                            
         MVC   NBA56K,ANSWADD                                                   
GT56X    XIT1                                                                   
*                                                                               
FREE56K  NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'                                                     
         BNE   FR56X                                                            
         OC    NBA56K,NBA56K                                                    
         BZ    FR56X                                                            
         FREEMAIN EU,LV=57000,A=ANSWADD                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
FR56X    XIT1                                                                   
*                                                                               
ANSWADD  DC    F'0'                                                             
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPBVALD                                                        
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFDD                                                       
       ++INCLUDE DDGENTWA                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLN                                                       
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
**** WORKING STORAGE                                                            
WORKD    DS    0D                                                               
**** COMMON WITH EDIT                                                           
       ++INCLUDE NEAGSMINCL                                                     
**                                                                              
*** ARGS FROM EDIT                                                              
HOLDAGNM DS    CL33                CURRENT AGENCY NAME                          
PRNOCLI  DS    CL1                 FLAG SET IF NO CLI DETAILS TO BE             
*                                         PRINTED                               
HOLDAGBP DS    H                   AGENCY BILLING PCTG                          
PRMARGA  DS    CL1                 FLAG SET IF MARGA DETAILS PRINTED            
MBILCOL  DS    CL1                 COL NUMBER FOR MARGA BILLING PCT             
PRDOVR   DS    CL1                 CALENDAR/BROADCAST OVERRIDE                  
MANONLY  DS    CL1                                                              
STWRDLST DS    CL256                                                            
*                                                                               
*                                                                               
SRCHEL   DS    CL1                                                              
CURLINE  DS    A                   A(CURRENT PRINT LINE)                        
*                                                                               
PRNTFLAG DS    CL1                 FLAG FOR PRINTLIN. SET IF PRINT NOW.         
CURFLAG  DS    CL1                 SET IF CURR LINE HAS NON-ZERO TOTS           
CLIFLAG  DS    CL1                 SET IF CURR CLI HAS NON-ZERO TOTS            
OFFSAV   DS    CL1                 COFFICE                                      
ACCOFFSV DS    CL2                 CACCOFF                                      
*                                                                               
BITMASK  DS    F                   BIT MASK ARG FOR SPEC PERIODS                
NUMSROW  DS    H                   NUMBER OF SPECIAL PERIODS                    
*                                                                               
RELO     DS    A                                                                
*                                                                               
PCTCLRWK DS    0CL16                                                            
         DS    CL2                                                              
PCTLST6  DS    CL6                 LOW ORDER PCTG DIGITS                        
PCTREM   DS    CL8                 REMAINDER                                    
ATODTOTS DS    A                                                                
ACLITOD  DS    A                   A(TODAY'S TOTALS FOR CLIENT)                 
AAGYTOD  DS    A                   A(TODAY'S TOTALS FOR AGENCY)                 
ADDSTOTS DS    A                   A(DDS TOTALS)                                
ADDSTOD  DS    A                   A(TODAYS TOTALS FOR DDS)                     
ASPERLST DS    A                   A(SPECIAL PERIOD LIST)                       
SJRFLAG  DS    CL1                 SET IF A DDS AGY HEDR ALREADY USED           
SPERHEAD DS    CL(10*4)             HEADERS FOR SPEC PER                        
MARGTOT  DS    CL8                 DDS BILLING TOTALS                           
RATEWRK  DS    2F                                                               
*                                                                               
******** TOTALS AREAS                                                           
TOTAREA  DS    0D                                                               
*                                                                               
* ORGANIZATION:                                                                 
*                                                                               
**MONLIST   (NUMMONS)*4BYTES            MONTH LIST                              
*                                                                               
**AGYTOTS   ((NUMMONS)*NUMCOLS)D        AGENCY TOTALS                           
*                                                                               
**CLITOD    (NUMCOLS)D                  TODAY TOTALS CLIENT                     
*                                                                               
**AGYTOD    (NUMCOLS)D                  TODAY TOTALS AGENCY                     
*                                                                               
**CLTTOTS   (NUMCOLS)D                  CLIENT TOTALS                           
*                                                                               
**CURTOTS   ((NUMMONS)*NUMCOLS)D      CURRENT  TOTALS                           
*                                                                               
**DDSTOTS   ((NUMMONS)*NUMCOLS)D      DDS (ALL AGY) TOTALS                      
*                                                                               
**DDSTOD    (NUMCOLS)D                TODAY DDS TOTALS                          
*****                                                                           
*                                                                               
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
*OFFSETS FOR PAGE HEADER INFORMATION                                            
         DSECT                                                                  
PHEAD    EQU   *                                                                
PHLENGTH EQU   132                                                              
*                                                                               
*                                                                               
         ORG   PHEAD+4*PHLENGTH+48                                              
PHNET    DS    CL4                 NETWORK                                      
         ORG   PHEAD+6*PHLENGTH+44                                              
PHNETDOL DS    CL13                                                             
*                                                                               
*DSECT FOR PRINT LINES                                                          
         DSECT                                                                  
COLINFO  EQU   *                                                                
*                                                                               
CLCLI    DS    CL3                                                              
         DS    CL1                                                              
CLCNAM   DS    CL20                                                             
         ORG   COLINFO+132                                                      
CLUNDER  DS    CL24                                                             
*                                                                               
         ORG   COLINFO                                                          
CLHEADS  DS    0CL24                                                            
CLMONTH  DS    CL5                 START MMMDD                                  
CLDASH   DS    CL1                 -                                            
CLMON2   DS    CL8                 END MMMDD/YY                                 
         DS    CL2                                                              
CLPERCL  DS    CL6                 NNN.NN                                       
CLPCTG   DS    CL1                 %                                            
         DS    CL1                                                              
CLCOSTS  DS    0C                  COSTS START HERE                             
*                                                                               
* EQUATES                                                                       
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037NEMED2D   03/22/19'                                      
         END                                                                    
