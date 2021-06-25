*          DATA SET NEMED2E    AT LEVEL 007 AS OF 07/16/07                      
*          DATA SET NEMED2E    AT LEVEL 178 AS OF 01/13/93                      
*PHASE T31E2EA                                                                  
*INCLUDE BUFFAHI                                                                
*INCLUDE BINSRCH                                                                
*INCLUDE DMDADDS                                                                
*INCLUDE CLUNPK                                                                 
         TITLE 'T31E2E - NETWORK BILLING PREP'                                  
         PRINT NOGEN                                                            
*******************************************************************             
* NETWORK BILLING PREP                                                          
*    SIMILAR TO AGY SUMMARY                                                     
*                                                                               
* GLOBALS:  R6 - WORKING STORAGE BASE                                           
*           R9 - A(NETSYSD, NETBLOCK)                                           
*           ACURLINE - THE CURRENT PRINT LINE ADDRESS                           
*           NCURLINS - CURRENT NUMBER OF PRINT LINES                            
*                                                                               
*  ANETWS1 - IS USED TO STORE THE CLIENT RECORD.                                
*                                                                               
*  INPUTS: ANETWS1 - A(CLI RECORD)                                              
*          NETBLOCK - NBSELSTR,NBSELEND - USER SUPPLIED MONTHS.                 
*       PRZLFLG - SET IF ZERO LINES ARE TO BE PRINTED                           
*       PRZCFLG - SET IF ZERO CLIENTS ARE TO BE PRINTED                         
*       MONTYPE - C'BMON' FOR BRODCAST MONTH, C'MON' FOR CAL MONS               
*       ENDWK   - IF BROD MONTH, THIS IS DAY BMON STARTS ON.                    
*       CSTMASKS - FULLWORD BIT MASKS, ONE FOR EACH COST TYPE. TELLS            
*                   WHICH PRINT FIELDS THIS COST IS TO BE ADDED TO.             
*       SPLEOP - (FROM TWA) C'A' - ACTIVE ESTS ONLY                             
*                           C'I' - INACTIVE ESTS ONLY                           
*                           ELSE BOTH ACTIVE AND INACTIVE ESTS                  
*       OPTALLOC - Y IF ALLOCATED UNITS TO BE PRINTED                           
*       OPTUNAL  - Y IF UNALLOCATED UNITS TO BE PRINTED                         
*                                                                               
T31E2E   CSECT                                                                  
         NMOD1 0,**NTBP**,RA,RR=R5                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
*                                                                               
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R6,ANETWS2                                                       
         A     R6,=F'500'                                                       
         USING WORKD,R6                                                         
*                                                                               
         ST    R5,RELO                                                          
*                                                                               
**** INITIALIZE  ************                                                   
******** (ALWAYS OFFLINE)                                                       
         LA    R1,13               MAX OF 13 MONTHS                             
         ST    R1,MAXMONTS                                                      
*                                                                               
***      L     R2,=V(DUMMY)                                                     
***      A     R2,RELO                                                          
         L     R2,VADUMMY                                                       
         ST    R2,ATOTAREA         PUT ADDRESS OF XTRA WORK IN R2               
         ST    R2,AMONLIST                                                      
         SPACE 1                                                                
         L     R1,=A(NETLIST)      PROVIDE BUFFER FOR MEDIA FILTER              
         A     R1,RELO                                                          
         ST    R1,NBANBUFF                                                      
         OI    NBINDS7,NBI7NTIO    EXPAND NETLIST BUFF                          
*                                                                               
         MVC   ABUFFALO,BUFFALO     STORE A(BUFFALO) WHERE IT IS ALWAYS         
*                                        ADRRESSABLE                            
         GOTO1 ABUFFALO,DMCB,=C'SET',BUFFBUFF                                   
*                                                                               
         BAS   RE,INITPBUF         INITIALIZE PRINT BUFFER                      
         L     R7,ABOX                                                          
         USING BOXD,R7                                                          
         MVI   BOXYORN,C'Y'        INITIALIZE FOR BOXES                         
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R7                                                               
*                                                                               
*** SET FLAGS BASED ON TWA VALUES                                               
         L     R3,ATWA                                                          
         USING T31EFFD,R3                                                       
         CLI   SPLASS,C'Y'         IF ASSIGNED COST OPTION SET                  
         BNE   SF2                                                              
         MVI   CASSFLG,1           USE ASSIGNED COSTS                           
SF2      DS    0H                                                               
         MVI   NBSPLOPT,X'C0'      OPTION TO SPLIT EVEN IF POOL                 
         DROP  R3                                                               
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
GOTDATE  BAS   RE,INITMON          GET THE MONTH LIST FOR STRT,END              
*                                                                               
         L     R3,ATOTAREA          SET UP ADDRESSES                            
         L     R1,NUMMONS                                                       
         SLL   R1,2                4 BYTES PER DATE SET                         
         LA    R3,0(R1,R3)                                                      
         ST    R3,AAGYTOTS                                                      
*                                                                               
         L     R1,NUMMONS                                                       
         MH    R1,NUMROWS                                                       
         SLL   R1,3                DOUBLEWORDS                                  
         LA    R3,0(R1,R3)                                                      
         ST    R3,APRDTOTS                                                      
*                                                                               
         LA    R3,0(R1,R3)                                                      
         ST    R3,ACLITOTS                                                      
*                                                                               
         LA    R3,0(R1,R3)                                                      
         ST    R3,ABUFTOTS                                                      
*                                                                               
         MVI   NBDATA,C'U'         SELECT UNIT RECORDS                          
         MVC   NBSELPRD,=C'ALL'    DONT FILTER ON PRODUCT                       
*                                                                               
         BAS   RE,INITAGY          INITIALIZE AGENCY TOTALS                     
         BAS   RE,NEWCLI           FIRST CLIENT. ALREADY READ BY EDIT           
         BAS   RE,MANBILS          GET MANUAL BILLS                             
***      MVI   WRTESTSW,C'N'                                                    
***      MVI   ESTBUFF,0                                                        
***      MVI   ESTSV,0                                                          
*                                                                               
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK    GET NEXT RECORD                         
         CLI   NBERROR,0           CHECK FOR ERROR                              
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST     IF NO MORE UNITS                             
         BE    TOTALS                                                           
         CLI   NBMODE,NBVALCLI     IF A NEW CLIENT                              
         BNE   CKUN                                                             
         MVI   THISPRD,0                                                        
         MVI   THISEST,0                                                        
         BAS   RE,DUMPCLT          FINISH OLD CLIENT                            
         BAS   RE,NEWCLI           GET NEW CLIENT                               
         BAS   RE,MANBILS          GET MANUAL BILLS                             
CKUN     MVC   SAVECLT,NBACTCLI                                                 
         MVC   SAVCLICD,NBCLICOD   SAVE CLIENT CODE                             
         CLI   NBMODE,NBPROCUN     IF A UNIT RECORD                             
         BNE   GETUNIT                                                          
         CLI   NBSPLPRN,X'FF'      IF UNALLOCATED                               
         BNE   CKUN2                                                            
         CLI   OPTUNAL,0          AND UNALLOCATED FLAG = 0                      
         BE    GETUNIT             THEN SKIP UNIT                               
         B     CKUN5                                                            
CKUN2    CLI   OPTALLOC,0           ELSE ALLOCATED/AND FLAG = 0                 
         BE    GETUNIT               THEN SKIP IT                               
CKUN5    BAS   RE,PROCUN               PROCESS UNIT                             
         B     GETUNIT                                                          
*                                                                               
*                                                                               
TOTALS   BAS   RE,DUMPCLT          FINISH LAST CLIENT                           
         BAS   RE,FINAGY                                                        
*                                                                               
ASXIT    DS    0H                                                               
*        SR    R0,R0                                                            
*        LA    R1,7                                                             
*        SR    R0,R1                STUFF FOR FRED                              
*        SVC   247                  PUT -7 IN R0                                
         XMOD1                                                                  
******                                                                          
PROCERR  DC    F'0'                                                             
***************************************************************8                
         EJECT                                                                  
***************************************************************8                
*  HANDLE A NEW CLIENT                                                          
*    INPUT: CLT RECORD IS IN ANETWS1                                            
NEWCLI   NTR1                                                                   
         BAS   RE,INITCLT          INITIALIZE CLIENT TOTALS                     
         GOTO1 ABUFFALO,DMCB,=C'RESET',BUFFBUFF                                 
*                                SAVE CLT RECORD IN OLDCLREC                    
         L     R2,ANETWS1                                                       
         LA    R1,2000                                                          
         LA    R3,OLDCLREC                                                      
         MOVE  ((R3),2000),(R2)                                                 
*                                                                               
         MVI   NBUSER+13,C'N'      OVERRIDE PROF. DONT FILT PREEMPTS            
         MVI   NBUSER+8,C'Y'       AND USE ASSIGNED COSTS                       
         XIT1                                                                   
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
**       L     R1,ATWA                                                          
**       USING T31EFFD,R1                                                       
**       CLI   SPLEOP,C'I'         TEST INACTVE EST ONLY                        
**       BNE   PROCUNAA                                                         
**       DROP  R1                                                               
**       CLI   ESTSV,0                  IS IT FIRST TIME                        
**       BNE   *+10                                                             
**       MVC   ESTSV,NBACTEST              YES/FIRST TIME SET EST               
**       CLC   ESTSV,NBACTEST              NO /   HAS EST CHANGED               
**       BE    PROCUNAA                                                         
**       BAS   RE,INACTEST                        YES/TEST IF NEED TO           
**       MVC   ESTSV,NBACTEST                         WRITE BUFF REC            
*                                                                               
PROCUNAA XC    ARGTOADD,ARGTOADD   CLEAR SPECIAL ARG                            
*                                                                               
         MVC   THISPRD,NBSPLPRN    PXZ                                          
****     CLI   THISPRD,X'6F'                                                    
****     BNE   *+6                                                              
****     DC    H'0'                                                             
         MVC   THISEST,NBACTEST    PXZ                                          
         TM    NBUNITST,X'42'      IF PRE-EMPT OR MISSED                        
         BZ    PU1                                                              
         XC    NBACTUAL,NBACTUAL   SET COSTS TO 0 BUT                           
         XC    NBASSIGN,NBASSIGN   LEAVE BILLING AND PAYING                     
         XC    NBINTEG,NBINTEG                                                  
*                                                                               
*  CONDITINAL ASSIGNED COST                                                     
PU1      CLI   CASSFLG,1           IF FLAG SET TO USE ASSIGNED                  
         BNE   PU2                                                              
         OC    NBASSIGN,NBASSIGN   AND IF ASSIGNED COST NON-ZERO                
         BZ    PU2                                                              
         MVC   CASSCOST,NBASSIGN     THEN USE ASSIGNED COST                     
         B     PU4                                                              
PU2      MVC   CASSCOST,NBACTUAL   ELSE USE ACTUAL COST                         
*                                                                               
PU4      GOTO1 ADDEM,DMCB,CASSCOST,NBACTDAT,=AL4(CASSMSK)                       
         BAS   RE,SPECIALS                                                      
         GOTO1 ADDEM,DMCB,FULL,NBACTDAT,=AL4(CASSMSK)                           
         GOTO1 ADDEM,DMCB,NBACTUAL,NBACTDAT,=AL4(ACTMSK)                        
         BAS   RE,SPECIALS                                                      
         GOTO1 ADDEM,DMCB,FULL,NBACTDAT,=AL4(ACTMSK)                            
         GOTO1 ADDEM,DMCB,NBASSIGN,NBACTDAT,=AL4(ASSMSK)                        
         BAS   RE,SPECIALS                                                      
         GOTO1 ADDEM,DMCB,FULL,NBACTDAT,=AL4(ASSMSK)                            
         GOTO1 ADDEM,DMCB,NBINTEG,NBACTDAT,=AL4(INTMSK)                         
         GOTO1 ADDEM,DMCB,NBFEED,NBACTDAT,=AL4(FEEDMSK)                         
*                                                                               
*                                                                               
         L     R3,NBAIO            NOW GET ACTUAL UNIT                          
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL         SET UP R2 FOR GETEL                          
         MVI   SRCHEL,X'00'                                                     
*        XC    ARGTOADD,ARGTOADD                                                
*        B     DOPAY                                                            
NEXTELEM XC    ARGTOADD,ARGTOADD                                                
         MVC   THISPRD,NBSPLPRN                                                 
****     CLI   THISPRD,X'6F'                                                    
****     BNE   *+6                                                              
****     DC    H'0'                                                             
         BAS   RE,NEXTEL                                                        
         BNE   XITPC               LAST ELEMENT                                 
         CLI   0(R2),X'10'         BILLING ELEMENT                              
         BE    DOBILL                                                           
         CLI   0(R2),X'12'         PAYING ELEMENT                               
         BE    DOPAY                                                            
         B     NEXTELEM                                                         
*                                                                               
         USING NUBILD,R2                                                        
DOBILL   TM    NUBILST,X'20'       CHECK IF UNBILLED                            
         BO    NEXTELEM                                                         
         CLC   NUBILPRD,NBSPLPRN   CHECK PROD                                   
         BE    DOBILLA                                                          
         CLC   NUBILPRD,NBPRD      DOES IT MATCH THOSE ON UNIT                  
         BE    NEXTELEM            YES/GET IT NEXT TIME                         
         CLC   NUBILPRD,NBPRD2                                                  
         BE    NEXTELEM                                                         
         MVC   THISPRD,NUBILPRD    BILLED PROD BUT PROD NOT ON UNIT             
****     CLI   THISPRD,X'6F'                                                    
****     BNE   *+6                                                              
****     DC    H'0'                                                             
         OI    NUBILST,X'20'  PXZ  FUDGE IT UNBILLED SO SKIP NEXT READ          
*                                                                               
DOBILLA  MVC   ARGTOADD,=AL4(CBMSK4)   SET SPECIAL MASK BIT                     
         CLI   NUBILBTY,C'4'            BASED ON BUBILBTY                       
         BE    DOBILL2                                                          
         MVC   ARGTOADD,=AL4(CBMSK5)                                            
         CLI   NUBILBTY,C'5'                                                    
         BE    DOBILL2                                                          
         MVC   ARGTOADD,=AL4(CBMSK6)                                            
         CLI   NUBILBTY,C'6'                                                    
         BE    DOBILL2                                                          
         MVC   ARGTOADD,=AL4(CBMSK7)                                            
         CLI   NUBILBTY,C'7'                                                    
         BE    DOBILL2                                                          
*                                                                               
         B     DOBILL2             SHOULD NEVER GET HERE                        
*                                                                               
DOBILL2  CLI   NUBILTYP,C'T'       TIME                                         
          BNE   BILLINT                                                         
         GOTO1 ADDEM,DMCB,NUBILGRS,NBACTDAT,=AL4(BTGMSK)                        
         GOTO1 ADDEM,DMCB,NUBILNET,NBACTDAT,=AL4(BTNMSK)                        
         B     NEXTELEM                                                         
*                                                                               
*                                  INTEG                                        
BILLINT  GOTO1 ADDEM,DMCB,NUBILGRS,NBACTDAT,=AL4(BIGMSK)                        
         GOTO1 ADDEM,DMCB,NUBILNET,NBACTDAT,=AL4(BINMSK)                        
         B     NEXTELEM                                                         
         DROP  R2                                                               
*                                                                               
         USING NUPAYD,R2                                                        
DOPAY    CLI   NUPAYTYP,C'T'        TIME PAY                                    
         BNE   PAYINT                                                           
         GOTO1 ADDEM,DMCB,NUPAYGRS,NBACTDAT,=AL4(PTGMSK)                        
         GOTO1 ADDEM,DMCB,NUPAYNET,NBACTDAT,=AL4(PTNMSK)                        
         B     NEXTELEM                                                         
*                                                                               
*                                    INTEG                                      
PAYINT   GOTO1 ADDEM,DMCB,NUPAYGRS,NBACTDAT,=AL4(PIGMSK)                        
         GOTO1 ADDEM,DMCB,NUPAYNET,NBACTDAT,=AL4(PINMSK)                        
         B     NEXTELEM                                                         
*DOPAY    GOTO1 ADDEM,DMCB,NBPAYTGR,NBACTDAT,=AL4(PTGMSK)                       
         GOTO1 ADDEM,DMCB,NBPAYTNT,NBACTDAT,=AL4(PTNMSK)                        
         GOTO1 ADDEM,DMCB,NBPAYIGR,NBACTDAT,=AL4(PIGMSK)                        
         GOTO1 ADDEM,DMCB,NBPAYINT,NBACTDAT,=AL4(PINMSK)                        
         B     NEXTELEM                                                         
         DROP  R2                                                               
*                                                                               
XITPC    XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   RETURNS SPECIAL CHARGE IN FULL                                              
SPECIALS NTR1                                                                   
         XC    FULL,FULL                                                        
         TM    NBUNITST,X'42'                                                   
         BNZ   XITPC                                                            
         SR    R4,R4                                                            
         L     R3,NBAIO            NOW GET ACTUAL UNIT                          
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL         SET UP R2 FOR GETEL                          
         MVI   SRCHEL,X'03'        AND FOR SPECIAL CHARGE ELEM                  
         USING NUSPRD,R2                                                        
SPC5     BAS   RE,NEXTEL                                                        
         BNE   SPCX                                                             
         ICM   R3,15,NUSPRAMT                                                   
         AR    R4,R3                                                            
         B     SPC5                                                             
SPCX     ST    R4,FULL                                                          
         B     XITPC                                                            
         EJECT                                                                  
****************************************************************                
* ADDEM - ADDS COST INTO APPROPRIATE LINE                                       
*            BY BUILDING A BUFFALO RECORD.                                      
* INS:   ARG1: A(FULLWORD COST)                                                 
*        ARG2: A(DATE COST INCURRED)                                            
*        ARG3: BIT MASK CORRESPONDING TO THIS COST                              
*        ARGTOADD: SPECIALL FULLWORD MASK, MUST ALSO BE A MATCH IF              
*                    ANY OF THE SPECIAL MASK BITS ARE SET.                      
*                                                                               
* LOCALS: R8 -USED LOCALLY -DOESN'T ADDRESS SPOOL                               
*                                                                               
ADDEM    NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)            A(BIT MASK)                                  
         L     R4,0(R4)                                                         
*                                                                               
         L     R5,0(R2)            GET COST                                     
*                                                                               
         L     R7,ATWA                                                          
         USING T31EFFD,R7                                                       
         CLI   SPLEOP,C'A'         IF ESTOPT =ACTIVE ESTS ONLY                  
         BNE   AD2                                                              
         LTR   R5,R5                IF COST IS 0                                
         BZ    XITADDM               THEN EXIT. DONT BUILD A BUFF REC           
*                                                                               
AD2      CLI   SPLEOP,C'I'         IF ESTOPT=INACTIVE ESTS ONLY                 
         BNE   AD4                                                              
**       MVI   WRTESTSW,C'Y'               SET INACTIVE WRT TO YES              
         LTR   R5,R5                IF COST IS ZERO (INACTIVE)                  
**       BZ    AD3                     BUILD BUFF REC                           
**       MVI   WRTESTSW,C'N'        ELSE SET INACTIVE WRT TO NO                 
         BNZ   XITADDM               AND XIT. DONT BUILD BUFF REC(EVER)         
         DROP  R7                                                               
*                                                                               
AD4      CVD   R5,DUB              PUT CONVERTED COST IN DUB                    
         SR    R1,R1                                                            
         SR    R1,R5               PUT NEGATIVE COST IN DUB2                    
         CVD   R1,DUB2                                                          
*                                                                               
         LA    R5,CSTMASKS                                                      
         LH    R7,NUMROWS                                                       
         L     R8,ACURTOTS                                                      
AMLOOP   ST    R4,FULL             PUT MASK FOR THIS COST IN FULL               
         NC    FULL,0(R5)          IF MASK MATCH                                
         BZ    CKSUB                                                            
         MVC   FULL,0(R5)                                                       
         NC    FULL,=AL4(SPECMSK)  IF A SPECIAL MASK SET                        
         BZ    AML2                                                             
         OC    ARGTOADD,ARGTOADD   IF SPECIAL ARG GIVEN                         
         BZ    AML2                                                             
         NC    FULL,ARGTOADD        THEN MUST ALSO MATCH SPECIAL ARG            
         BZ    CKSUB                                                            
*                                                                               
AML2     BAS   RE,GETDATN             GET DATE OFFSET IN R2                     
         BNZ   NXTMSK                 IF DATE OUT OF RANGE, SKIP IT             
         STC   R2,BUFFMON                                                       
*        MVC   BUFFPRD,NBPRD                                                    
***      MVC   BUFFPRD,NBSPLPRN                                                 
         MVC   BUFFPRD,THISPRD                                                  
*******  CLI   THISPRD,X'6F'                                                    
*******  BNE   *+6                                                              
*******  DC    H'0'                                                             
**       CLI   BUFFPRD,0                                                        
**       BNE   *+6                                                              
**       DC    H'0'                                                             
         MVC   BUFFEST,THISEST                                                  
         CLI   BUFFEST,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         LH    R1,NUMROWS          CALC ROW NUMBER                              
         SR    R1,R7                                                            
         STC   R1,BUFFROW                                                       
         MVC   BUFFCOST,DUB                                                     
         GOTO1 ABUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFFREC                           
*                                                                               
CKSUB    ST    R4,FULL             IF TO BE SUBTRACTED                          
         NC    FULL,4(R5)                                                       
         BZ    NXTMSK                                                           
         MVC   FULL,4(R5)                                                       
         NC    FULL,=AL4(SPECMSK)  IF A SPECIAL MASK SET                        
         BZ    AML4                                                             
         OC    ARGTOADD,ARGTOADD   IF SPECIAL ARG GIVEN                         
         BZ    AML4                                                             
         NC    FULL,ARGTOADD        THEN MUST ALSO MATCH SPECIAL ARG            
         BZ    NXTMSK                                                           
*                                                                               
AML4     BAS   RE,GETDATN             GET DATE OFFSET IN R2                     
         BNZ   NXTMSK                 IF DATE OUT OF RANGE, SKIP IT             
*        MVC   BUFFPRD,NBPRD                                                    
*        MVC   BUFFPRD,NBSPLPRN                                                 
         MVC   BUFFPRD,THISPRD                                                  
******   CLI   THISPRD,X'6F'                                                    
******   BNE   *+6                                                              
******   DC    H'0'                                                             
         MVC   BUFFEST,THISEST                                                  
         LH    R1,NUMROWS          CALC ROW NUMBER                              
         SR    R1,R7                                                            
         STC   R1,BUFFROW                                                       
         STC   R2,BUFFMON                                                       
         MVC   BUFFCOST,DUB2                                                    
         GOTO1 ABUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFFREC                           
*                                                                               
NXTMSK   LA    R5,8(R5)            NEXT MASK SET                                
         LA    R8,8(R8)            NEXT SET OF COLUMNS                          
         BCT   R7,AMLOOP                                                        
XITADDM  XIT1                                                                   
****************************************************************                
         EJECT                                                                  
****************************************************************                
* GETDATN - FINDS THE DATE IN THE DATELIST, RETURNS THE OFFSET                  
*           NUMBER.                                                             
*                                                                               
*  INPUT: R3 - A(DATE)                                                          
*  OUTPUT: R2 - DATE NUMBER (FIRST IS 0)                                        
*          STATUS BIT - ZERO IF WITHIN RANGE, ELSE 1                            
GETDATN  NTR1                                                                   
         L     R4,AMONLIST                                                      
         SR    R2,R2                                                            
         CLC   0(2,R3),0(R4)       IF LESS THAN 1ST DATE                        
         BL    NOTINRNG                                                         
         L     R5,NUMMONS                                                       
GDLOOP   CLC   0(2,R3),2(R4)       TEST AGAINST END DATE OF MONTH               
         BNH   GOTDAT              FOUND IF L.E. END DATE                       
         LA    R4,4(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   R5,GDLOOP                                                        
NOTINRNG LA    R1,1                                                             
         B     XITGD                                                            
GOTDAT   LA    R1,0                                                             
XITGD    LTR   R1,R1                                                            
         XIT1  REGS=(R2)                                                        
****************************************************************                
         EJECT                                                                  
****************************************************************                
*  DUMP A CLIENT - PRINT EST LINES, PROD TOTALS, CLI TOTALS                     
****************************************************************                
*                                                                               
DUMPCLT  NTR1                                                                   
*                                                                               
         MVI   FRSTPRD,0                                                        
         MVI   FRSTEST,0                                                        
         MVI   FRSTROW,0                                                        
         XC    BUFFREC(BUFFRECL),BUFFREC     GET FIRST RECORD                   
*                                                                               
HIGHREC  MVC   SAVBKEY,BUFFKEY                                                  
         GOTO1 ABUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFFREC                          
         CLI   DMCB+8,X'80'        CHECK FOR LAST RECORD                        
         BE    XITDC                                                            
         B     FIRSTTIM            FIRST TIME THRU, INIT AS NEW PRODUCT         
*                                                                               
CKPROD   CLC   SAVEPRD,BUFFPRD     IF NEW PROD                                  
         BE    CKEST                                                            
         BAS   RE,TOTROW                                                        
         BAS   RE,TOTEST                                                        
         BAS   RE,TOTPRD                                                        
FIRSTTIM BAS   RE,NEWPRD                                                        
         BAS   RE,NEWEST                                                        
         BAS   RE,NEWROW                                                        
         B     ENDCK                                                            
*                                                                               
CKEST    CLC   SAVEEST,BUFFEST     ELSEIF NEW EST                               
         BE    CKROW                                                            
         BAS   RE,TOTROW                                                        
         BAS   RE,TOTEST                                                        
         BAS   RE,NEWEST                                                        
         BAS   RE,NEWROW                                                        
         B     ENDCK                                                            
*                                                                               
CKROW    CLC   SAVEROW,BUFFROW     ELSEIF NEW ROW                               
         BE    ENDCK                                                            
         BAS   RE,TOTROW                                                        
         BAS   RE,NEWROW                                                        
         B     ENDCK                                                            
*                                                                               
*                                  FI                                           
ENDCK    DS    0H                                                               
         BAS   RE,FILLDATE                                                      
         BAS   RE,ADDTOPRD         ADD TO PRODUCT TOTALS                        
*                                                                               
NEXTBUFR MVC   SAVBKEY,BUFFKEY                                                  
         GOTO1 ABUFFALO,DMCB,=C'SEQ',BUFFBUFF,BUFFREC                           
         CLI   DMCB+8,X'80'                                                     
         BE    ENDCLI                                                           
         B     CKPROD                                                           
*                                                                               
ENDCLI   BAS   RE,TOTROW                                                        
         BAS   RE,TOTEST                                                        
         BAS   RE,TOTPRD                                                        
XITDC    XIT1                                                                   
****************************************************************                
         EJECT                                                                  
****************************************************************                
* TOTROW - FINISH PRINTING A ROW                                                
*                                                                               
**********                                                                      
TOTROW   NTR1                                                                   
         CLI   FRSTROW,0           IF SOMETHING IN ROW                          
         BE    XITTOTR                                                          
         MVI   FRSTROW,0           RESET FOR NEXT TIME                          
*                                                                               
         USING COLINFO,R5          PRINT ROW TOTAL                              
         L     R5,ACURLINE                                                      
         CLC   CURTOTS,=PL8'0'      IF ROW TOTAL IS ZERO                        
         BNE   TOTR2                   DONT PRINT IT                            
         MVC   0(132,R5),SPACES      AND CLEAR LINE                             
         B     XITTOTR                                                          
*                                                                               
TOTR2    LA    R5,CLDATES                                                       
         DROP  R5                                                               
         LA    R1,OUTDATLN         L'DATES                                      
         STH   R1,HALF                                                          
         L     R1,NUMMONS                                                       
         MH    R1,HALF             OFFSET TO TOTAL COLUMN                       
         LA    R5,0(R1,R5)                                                      
         EDIT  (P8,CURTOTS),(11,0(R5)),FLOAT=-,ZERO=BLANK                       
         MVC   9(2,R5),=CL2'  '    DONT PRINT CENTS                             
*                                                                               
         BAS   RE,NEXTLINE                                                      
XITTOTR  XIT1                                                                   
****************************************************************                
         EJECT                                                                  
****************************************************************                
* TOTEST - FINISH PRINTING A EST                                                
*  I/O - FRSTEST - FLAG SET IF EST IS NON-ZERO                                  
*                  CURRENTLY NOT USED                                           
**********                                                                      
TOTEST   NTR1                                                                   
         MVI   FRSTEST,0           RESET FOR NEXT TIME                          
         MVC   BOXBOTLN,NCURLINS   BOTTOM OF BOX                                
*                                                                               
         BAS   RE,NEXTLINE         PRINT 3 BLANK LINES                          
         BAS   RE,NEXTLINE                                                      
         BAS   RE,NEXTLINE                                                      
         BAS   RE,FLUSHIT          FLUSH PRINT BUFFER                           
XITTOTE  XIT1                                                                   
****************************************************************                
         EJECT                                                                  
****************************************************************                
* TOTPRD - FINISH PRINTING A PRODUCT                                            
*                                                                               
*  I/OS:   FRSTPRD - FLAG SET IF PRODUCT HAS ACTIVE ESTS                        
*                    CURRENTLY NOT USED                                         
*  LOCALS: R4 - CURRENT TOTAL                                                   
*          R3 -CURRENT MONTH                                                    
*          R2 - COUNTER FOR MONTHS                                              
*          R7 - CLIENT TOTALS                                                   
**********                                                                      
TOTPRD   NTR1                                                                   
         CLI   FRSTPRD,0           IF SOMETHING IN ROW                          
         BE    XITTOTP                                                          
         MVI   FRSTPRD,0           RESET FOR NEXT TIME                          
*                                                                               
         BAS   RE,NEXTLINE         ADD BLANK LINE                               
         L     R5,ACURLINE                                                      
         USING COLINFO,R5                                                       
         MVC   CLKEST(14),=C'PRODUCT TOTALS'                                    
         MVI   FRSTEST,1           FAKE PROD TOTALS TO LOOK LIKE AN EST         
         BAS   RE,NEXTLINE                                                      
         BAS   RE,NEXTLINE         PRINT BLANK LINE                             
         MVC   FULL,NBCMPSTR                                                    
         BAS   RE,PRDATES          PRINT DATE HEADERS                           
         BAS   RE,NEXTLINE                                                      
         DROP  R5                                                               
*                                                                               
         MVC   SAVBREC(BUFFRECL),BUFFREC    SAVE BUFF RECRD FOR LOOP            
         L     R7,ACLITOTS                                                      
         L     R4,APRDTOTS                                                      
         LH    R5,NUMROWS                                                       
TPROWLOP SR    R3,R3                                                            
         L     R2,NUMMONS                                                       
         XC    BUFFREC,BUFFREC                                                  
         LH    R1,NUMROWS                                                       
         SR    R1,R5               GET ROW NUMBER IN R1                         
         STC   R1,BUFFROW                                                       
         BAS   RE,NEWROW           SET UP FOR NEW ROW                           
TPDATLOP STC   R3,BUFFMON            FOR FILLDATE                               
         MVC   BUFFCOST(8),0(R4)                                                
         BAS   RE,FILLDATE                                                      
         AP    0(8,R7),0(8,R4)     ADD TO CLT TOTALS                            
         LA    R7,8(R7)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,TPDATLOP                                                      
         BAS   RE,TOTROW           PRINT THIS ROW W/ TOTALS                     
         BCT   R5,TPROWLOP                                                      
         MVC   BUFFREC(BUFFRECL),SAVBREC     RESTORE BUFFALO RECORD             
*                                                                               
         BAS   RE,TOTEST                                                        
*                                                                               
XITTOTP  XIT1                                                                   
****************************************************************                
         EJECT                                                                  
****************************************************************                
* NEWROW - SET UP FOR A NEW ROW                                                 
*                                                                               
*  INPUT:   BUFFREC                                                             
*  OUTPUT: CURRENT PRINT LINE                                                   
****************************************************************                
         USING COLINFO,R5                                                       
NEWROW   NTR1                                                                   
         MVI   FRSTROW,1                                                        
         ZAP   CURTOTS,ZERO        ZERO ROW TOTALS                              
         L     R5,ACURLINE                                                      
*                                                                               
         LA    R1,10               GET OFFSET INTO ROW HEADERS                  
         STH   R1,HALF              10 IS HARD LENGTH OF HEADER                 
         ZIC   R2,BUFFROW                                                       
         MH    R2,HALF                                                          
         LA    R3,CSTHEADS         BEGINNING OF ROW HEADERS                     
         LA    R3,0(R2,R3)                                                      
*                                                                               
         MVC   CLHEADER,0(R3)                                                   
*                                                                               
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
****************************************************************                
         EJECT                                                                  
****************************************************************                
* NEWEST - SET UP FOR A NEW ESTIMATE                                            
*                                                                               
*  INPUT:   BUFFREC                                                             
*  OUTPUT: CURRENT PRINT LINE                                                   
*  LOCALS: FULL - COMPRESSED START, END DATES OF ESTIMATE                       
****************************************************************                
         USING COLINFO,R5                                                       
NEWEST   NTR1                                                                   
         MVI   FRSTEST,1                                                        
         L     R5,ACURLINE                                                      
         MVC   CLKEST(8),=C'ESTIMATE'                                           
*                                                                               
         OC    CURPROD,CURPROD     IF UNALLOCATED                               
         BNZ   NE2                     DONT READ ESTIMATE RECORD                
         MVC   CLESTNAM(11),=C'UNALLOCATED'                                     
         MVC   FULL,NBCMPSTR       USE SELECTED START,END DATES                 
         EDIT  BUFFEST,(3,CLEST),FILL=0                                         
         B     NE4                                                              
*                                                                               
NE2      BAS   RE,GETESREC         GET EST RECORD IN IO                         
         BZ    NE3                 IF NOT FOUND                                 
         MVC   CLESTNAM(18),=C'NO ESTIMATE HEADER'                              
         MVC   FULL,NBCMPSTR           USE SELECTED START, END DATES            
         EDIT  BUFFEST,(3,CLEST),FILL=0                                         
         B     NE4                                                              
*                                                                               
NE3      LA    R7,IO                                                            
         USING ESTHDR,R7                                                        
*                                                                               
*** EVENTUAL FILTERING ON ESTHEADER DATES GOES HERE                             
*                                                                               
         EDIT  EKEYEST,(3,CLEST),FILL=0                                         
         GOTO1 DATCON,DMCB,(0,ESTART),(5,CLESTDAT)                              
         GOTO1 DATCON,DMCB,(0,ESTART),(2,FULL)                                  
         MVI   CLESTDAT+8,C'-'                                                  
         GOTO1 DATCON,DMCB,(0,EEND),(5,CLESTDAT+9)                              
         GOTO1 DATCON,DMCB,(0,EEND),(2,FULL+2)                                  
         MVC   CLESTNAM,EDESC                                                   
*                                                                               
         OC    EPROF(3),EPROF       IF HAS A FILTER                             
         BZ    NE4                                                              
         MVC   CLESTFTN,=C'FILT='                                               
         MVC   CLESTFLT,EPROF                                                   
*                                                                               
NE4      BAS   RE,NEXTLINE                                                      
         BAS   RE,NEXTLINE         PRINT BLANK LINE                             
*                                                                               
         BAS   RE,PRDATES          PRINT DATE HEADERS                           
         BAS   RE,NEXTLINE         PRINT BLANK LINE                             
*                                                                               
*                                                                               
         XIT1                                                                   
         DROP  R5                                                               
         DROP  R7                                                               
*                                                                               
****************************************************************                
         EJECT                                                                  
****************************************************************                
*  PRDATES - PRINT DATE HEADER LINE                                             
*                                                                               
PRDATES  NTR1                                                                   
         USING COLINFO,R5                                                       
         L     R5,ACURLINE                                                      
         USING BOXD,R7                                                          
         L     R7,ABOX                                                          
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'        SET UP LEFT BOX CORNER                       
*                                                                               
         LH    R1,NCURLINS                                                      
         BCTR  R1,0                PUT TOP OF BOX B4 THIS LINE                  
         STH   R1,BOXTOPLN         TOP OF BOX                                   
*                                                                               
         L     R2,NUMMONS                                                       
         LA    R4,CLDATES          STRT DATE HEADERS                            
         L     R3,AMONLIST                                                      
PRSTRTLP GOTO1 DATCON,DMCB,(2,0(R3)),(4,2(R4))   PRINT START OF PER             
*                                                                               
         LR    R1,R4               SET UP BOXES                                 
         SR    R1,R5                                                            
         LA    R1,BOXCOLS(R1)                                                   
         MVI   0(R1),C'C'                                                       
*                                                                               
         LA    R3,4(R3)                                                         
         LA    R4,OUTDATLN(R4)                                                  
         BCT   R2,PRSTRTLP                                                      
*                                                                               
         LR    R1,R4               SET UP BOXES                                 
         SR    R1,R5                                                            
         LA    R1,BOXCOLS(R1)                                                   
         MVI   0(R1),C'C'                                                       
         MVI   10(R1),C'R'         SET RIGHT BOX CORNER                         
*                                                                               
         MVC   3(5,R4),=C'TOTAL'                                                
*                                                                               
         BAS   RE,NEXTLINE         PRINT THIS LINE                              
         L     R5,ACURLINE                                                      
*                                                                               
         L     R2,NUMMONS                                                       
         LA    R4,CLDATES          END DATE HEADERS                             
         L     R3,AMONLIST                                                      
PRENDLP  GOTO1 DATCON,DMCB,(2,2(R3)),(4,2(R4))   PRINT END OF PER               
         LA    R3,4(R3)                                                         
         LA    R4,OUTDATLN(R4)                                                  
         BCT   R2,PRENDLP                                                       
*                                                                               
         BAS   RE,NEXTLINE         PRINT THIS LINE                              
         MVC   BOXMIDLN,NCURLINS   MIDDLE OF BOX                                
         DROP  R5                                                               
         DROP  R7                                                               
         XIT1                                                                   
****************************************************************                
         EJECT                                                                  
****************************************************************                
* NEWPRD - SET UP FOR A NEW PRODUCT                                             
*       FILL IN NEW HEADER STUFF AND FORCE PAGE BREAK                           
*       READS PRODUCT RECORD                                                    
*                                                                               
*  INPUT:   BUFFREC                                                             
*  OUTPUT:  CURPROD - ALPHA LOOKUP OF PRODUCT                                   
*           CURPNAM - CURRENT PRODUCT NAME                                      
****************************************************************                
NEWPRD   NTR1                                                                   
         MVI   FRSTPRD,1                                                        
         BAS   RE,INITPRD          INITIALIZE THE TOTALS AREA                   
*                                                                               
         LA    R4,OLDCLREC         ADDRESS OF CLIENT RECORD                     
         USING CLTHDR,R4                                                        
         LA    R2,CLIST            LOOK UP PRODUCT CODE                         
         OC    BUFFPRD,BUFFPRD     IF PROD UNALLOCATED (ZERO)                   
         BNZ   PRD1LOOP                                                         
         XC    CURPROD,CURPROD                                                  
         MVC   CURPNAM,=CL20'PRODUCT UNALLOCATED'                               
         B     XITNP                                                            
PRD1LOOP CLI   3(R2),0             IF END-OF-LIST THEN SET FOR                  
         BNE   NEXTPRD                UNALLOCATED.                              
         XC    CURPROD,CURPROD                                                  
         MVC   CURPNAM,=CL20'PRODUCT UNDEFINED '                                
         B     XITNP                                                            
NEXTPRD  CLC   3(1,R2),BUFFPRD                                                  
         BE    PRD1FND                                                          
         LA    R2,4(R2)            NEXT IN LIST                                 
         B     PRD1LOOP                                                         
PRD1FND  MVC   CURPROD,0(R2)                                                    
         DROP  R4                                                               
*                                                                               
*                                                                               
         CLC   SAVECLT,=2X'00'     IF NO CLIENT                                 
         BNE   NEWPRD5                                                          
         XC    CURPROD,CURPROD     CLEAR CURPROD AND EXIT                       
         B     XITNPX                                                           
NEWPRD5  NETGO NVSETSPT,DMCB      SET UP TO READ SPOT FILE                      
         USING PRDHDR,R4           READ PRODUCT RECORD                          
         LA    R4,KEY                                                           
         XC    PKEY,PKEY                                                        
         MVC   PKEYAM,NBACTAM                                                   
         MVC   PKEYCLT,SAVECLT                                                  
         MVC   PKEYPRD,CURPROD                                                  
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(7),KEY                                                   
         BE    GOTPREC                                                          
         DC    H'0'                PROBLEMS                                     
GOTPREC  MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         MVC   CURPNAM,PNAME       SAVE PROD NAME                               
         XC    FILENAME,FILENAME   RESTORE TO DEFAULT                           
         DROP  R4                                                               
*                                                                               
XITNP    MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         NETGO NVSETUNT,DMCB       RESET TO UNIT FILE                           
XITNPX   XIT1                                                                   
*                                                                               
****************************************************************                
         EJECT                                                                  
****************************************************************                
*  GETESREC - READS THE ESTIMATE RECORD FOR THE CURRENT EST, PRD                
*               PUTS THE RECORD IN IO                                           
*                                                                               
*   OUTPUT: STAUS BIT SET TO ZERO IF RECORD FOUND                               
*   LOCAL : R2 - SET TO 0 IF ALL OK                                             
****************                                                                
         USING ESTHDR,R4                                                        
GETESREC NTR1                                                                   
         LA    R4,KEY                                                           
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,SAVECLT                                                  
         MVC   EKEYPRD,CURPROD                                                  
         MVC   EKEYEST,BUFFEST                                                  
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         NETGO NVSETSPT,DMCB      SET UP TO READ SPOT FILE                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         SR    R2,R2               ASSUME OK                                    
         CLC   KEYSAVE(13),KEY                                                  
         BE    GOTEREC                                                          
         LA    R2,1                SET FLAG                                     
GOTEREC  MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME   RESTORE DEFAULT                              
         NETGO NVSETUNT,DMCB       RESET TO READ UNIT FILE                      
         LTR   R2,R2                                                            
         XIT1                                                                   
         DROP  R4                                                               
****************************************************************                
         EJECT                                                                  
****************************************************************                
*  FILLDATE - EDIT THE BUFFALO RECORD INTO APPROPRIATE MONTH COLUMN             
*            OF THE CURRENT PRINT LINE.                                         
*                                                                               
*  INPUT:  BUFFREC                                                              
*  OUTPUT: CURRENT PRINT LINE                                                   
****************************************************************                
FILLDATE NTR1                                                                   
         USING COLINFO,R5                                                       
         L     R5,ACURLINE                                                      
         LA    R5,CLDATES          START OF DATE FIELDS                         
         DROP  R5                                                               
*                                                                               
         LA    R1,OUTDATLN         GET OFFSET INTO DATE FIELDS                  
         STH   R1,HALF                                                          
         ZIC   R2,BUFFMON                                                       
         MH    R2,HALF                                                          
         LA    R5,0(R2,R5)                                                      
*                                                                               
         EDIT  (P8,BUFFCOST),(10,0(R5)),FLOAT=-,ZERO=BLANK                      
         MVC   8(2,R5),=C'  '           DONT PRINT CENTS                        
*                                                                               
         AP    CURTOTS,BUFFCOST    ADD TO CURRENT LINE TOTALS                   
*                                                                               
         XIT1                                                                   
****************************************************************                
         EJECT                                                                  
****************************************************************                
* ADDTOPRD - ADD CURRENT BUFFALO RECORD TO PRODUCT TOTALS                       
*                                                                               
ADDTOPRD NTR1                                                                   
         L     R4,APRDTOTS                                                      
*                                                                               
         L     R2,NUMMONS          GET OFFSET TO CURRENT ROW IN R2              
         SLL   R2,3                                                             
         ZIC   R1,BUFFROW                                                       
         STH   R1,HALF                                                          
         MH    R2,HALF                                                          
         LA    R4,0(R2,R4)         A(CURRENT ROW) IN R4                         
*                                                                               
         ZIC   R1,BUFFMON          MONTH NUMBER                                 
         SLL   R1,3                EACH TOTAL IS 8 -BYTES                       
         LA    R4,0(R1,R4)                                                      
*                                                                               
         AP    0(8,R4),BUFFCOST                                                 
         XIT1                                                                   
****************************************************************                
         EJECT                                                                  
****************************************************************                
* END OF AN AGENCY                                                              
******************                                                              
         USING COLINFO,R5                                                       
FINAGY   NTR1                                                                   
*                                                                               
*                                                                               
         XIT1                                                                   
         DROP  R5                                                               
****************************************************************                
         EJECT                                                                  
****************************************************************                
*  BUILD THE MONTH LIST, SET VALUES BASED ON CONTROLS.                          
*  OUTPUTS:  MONLIST - LIST OF BEGIN-END DATE SETS FOR PERIOD.                  
*            NUMMONS - NUMBER OF USABLE DATE SETS IN LIST                       
*                                                                               
INITMON  NTR1                                                                   
         MVI   NREPTYP,C'A'        ID AS ACCTG REPORT                           
         XC    PERTYPE,PERTYPE                                                  
         MVI   PERTYPE,C'M'        USE MONTHS                                   
         L     R4,MAXMONTS                                                      
         ST    R4,NUMMONS          MAX NUMBER OF MONTHS FOR GETLST              
*                                                                               
         L     R5,AMONLIST                                                      
        NETGO  NVWKLST,DMCB,NUMMONS,(R5),PERTYPE                                
********  RETURNS MONLIST AND NUMMONS IN LIST                                   
*                                                                               
IMXIT    XIT1                                                                   
****************************************************************                
         EJECT                                                                  
****************************************************************                
****** INITIALIZATION ROUTINES                                                  
*                                                                               
* INITIALIZE THE PRODUCT SUMS TO PACKED ZEROES                                  
INITPRD  NTR1                                                                   
         MVI   CURFLAG,0           ALL ZEROS                                    
         L     R2,NUMMONS          GET NUM OF DUBWORDS TO ZAP IN R2             
         MH    R2,NUMROWS                                                       
         L     R3,APRDTOTS                                                      
*                                                                               
ZAPCUR   ZAP   0(8,R3),ZERO                                                     
         LA    R3,8(R3)            NEXT DOUBLEWORD                              
         BCT   R2,ZAPCUR                                                        
*                                                                               
         XIT1                                                                   
*                                                                               
***                                                                             
***** INITIALIZE CLIENT TOTALS                                                  
*                                                                               
INITCLT  NTR1                                                                   
         MVI   CLIFLAG,0           ALL ZEROS                                    
         L     R2,NUMMONS          NUMBER OF DOUBLEWORDS TO ZAP                 
         MH    R2,NUMROWS                                                       
         L     R3,ACLITOTS                                                      
*                                                                               
ZAPCLT   ZAP   0(8,R3),ZERO                                                     
         LA    R3,8(R3)            NEXT DOUBLEWORD                              
         BCT   R2,ZAPCLT                                                        
*                                                                               
         XIT1                                                                   
*                                                                               
***                                                                             
***** INITIALIZE AGENCY TOTALS                                                  
*                                                                               
INITAGY  NTR1                                                                   
         L     R2,NUMMONS          GET NUM DUBWORDS TO ZAP IN R2                
         MH    R2,NUMROWS                                                       
         L     R3,AAGYTOTS                                                      
*                                                                               
ZAPAGY   ZAP   0(8,R3),ZERO                                                     
         LA    R3,8(R3)            NEXT DOUBLEWORD                              
         BCT   R2,ZAPAGY                                                        
*                                                                               
         XIT1                                                                   
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
*      OUTPUT: ACURLINE - A(CURRENT PRINT LINE)                                 
*              NCURLINS - NUMBER OF CURRENT LINES                               
NEXTLINE NTR1                                                                   
*                                                                               
*                                    GET NEXT PRINT LINE.                       
         L     R5,ACURLINE                                                      
         LH    R4,NCURLINS                                                      
         USING COLINFO,R5                                                       
*                                                                               
         LA    R1,PRLAST                                                        
         CR    R5,R1                 IF USED LAST PRINT LINE                    
         BL    INCLINE                                                          
         DC    H'0'                     CRASH                                   
*                                    ELSE                                       
INCLINE  LA    R5,132(R5)               NEXT PRINTLINE                          
         LA    R4,1(R4)                                                         
*                                    FI                                         
XITPRNT  ST    R5,ACURLINE                                                      
         STH   R4,NCURLINS                                                      
         XIT1                                                                   
         DROP  R5                                                               
****************************************************************                
         EJECT                                                                  
**************************************************************                  
* FLUSHIT - FLUSHES INTERNAL PRINT BUFFER                                       
*            SETS UP BOXES APPROPRIATELY                                        
****************************************************************                
FLUSHIT  NTR1                                                                   
         L     R7,ABOX                                                          
         USING BOXD,R7                                                          
         ZIC   R3,MAXLINES         IF NOT ENOUGH ROOM ON PAGE                   
         ZIC   R2,LINE               FOR INTERNAL PRINT BUFFER                  
         AH    R2,NCURLINS                                                      
         CR    R2,R3                                                            
         BL    FL2                                                              
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
FL2      MVI   BOXINIT,0           SET UP FOR NEW BOXES                         
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVC   BOXROWS,SPACES                                                   
         ZIC   R2,LINE                                                          
         LA    R1,BOXROWS                                                       
         LA    R2,0(R1,R2)        R2 IS RELATIVE START OF BOX                   
         BCTR  R2,0                DECREMENT TO ALIGN TO CUR ROWS               
*                                                                               
         LH    R1,BOXTOPLN         BOX TOP                                      
         LA    R1,0(R1,R2)                                                      
         MVI   0(R1),C'T'                                                       
         LH    R1,BOXMIDLN         BOX MIDDLE                                   
         LA    R1,0(R1,R2)                                                      
         MVI   0(R1),C'M'                                                       
         LH    R1,BOXBOTLN         BOX BOTTOM                                   
         LA    R1,0(R1,R2)                                                      
         MVI   0(R1),C'B'                                                       
*                                                                               
         LH    R4,NCURLINS                                                      
         LA    R5,PRFIRST                                                       
PRINNEXT MVC   P1(132),0(R5)                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,132(R5)                                                       
         BCT   R4,PRINNEXT                                                      
*                                                                               
         BAS   RE,INITPBUF         RE-INITIALIZE PRINT BUFFER                   
*                                                                               
         XIT1                                                                   
         DROP  R7                                                               
**************************************************************                  
         EJECT                                                                  
**************************************************************                  
* INITPBUF -INITIALIZE INTERNAL PRINT BUFFER                                    
*    SETS PRINT LINES TO BLANKS                                                 
*    SETS NUMBER OF CURRENT LINES (NCURLINS) TO 0                               
*    SETS ACURLINE TO TOP OF BUFFER                                             
*                                                                               
**************************************************************                  
INITPBUF NTR1                                                                   
         LA    R2,PRFIRST          RESET INTERNAL PRINT BUFFER                  
         ST    R2,ACURLINE                                                      
         XC    NCURLINS,NCURLINS                                                
*                                                                               
         LA    R3,PBUFMAXL                                                      
IPBLOOP  MVC   0(132,R2),SPACES                                                 
         LA    R2,132(R2)                                                       
         BCT   R3,IPBLOOP                                                       
*                                                                               
         XIT1                                                                   
**************************************************************                  
         EJECT                                                                  
**************************************************************                  
* HOOK - BUILDS THE HEADER. SUPPLEMENTS THE SSPECS IN NEMED1E                   
************************************************************                    
*                                                                               
HOOK     NTR1                                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
* MAIN HEADER INFO                                                              
         LA    R5,HEAD1            BASE ADDRESS FOR OFFSETS                     
         USING PHEAD,R5                                                         
         LA    R4,OLDCLREC         A(CLIENT RECORD)                             
         USING CLTHDR,R4                                                        
         MVC   PHCLICOD,SAVCLICD   3-BYTE CLI CODE                              
         MVC   PHCLINAM,CNAME                                                   
         MVC   PHPRDCOD,CURPROD                                                 
         MVC   PHPRDNAM,CURPNAM                                                 
         OC    NBSELNET,NBSELNET   IF ONE NETWORK                               
         BZ    HK2                                                              
         MVC   PHNETHED(7),=C'NETWORK'                                          
         MVC   PHNET,NBSELNET                                                   
*                                                                               
HK2      OC    NBSELEFL,NBSELEFL   IF ESTIMATE FILTERS                          
         BZ    HK4                                                              
         MVC   PHFLTHED(6),=C'FILTER'                                           
         MVC   PHEFILT,NBSELEFL                                                 
*                                                                               
HK4      MVI   PHDUMCHR,X'41'      FORCE A BLANK LINE                           
*                                                                               
         DROP  R4                                                               
         DROP  R8                                                               
         DROP  R5                                                               
         XIT1                                                                   
***********************************************************                     
         EJECT                                                                  
**************************************************************                  
*                                                                               
***********************************************************                     
* GETEL,NEXTEL MACRO DEFINITION                                                 
*                                                                               
         GETEL R2,NBDTADSP,SRCHEL                                               
***********************************************************                     
         EJECT                                                                  
* READS STATION BUCKETS FOR MANUAL BILLS                                        
*                                                                               
MANBILS  NTR1  WORK=(R2,150)                                                    
         CLI   FRMTYPE,3                                                        
         BNE   STABXX                                                           
         MVC   ARGTOADD,=AL4(CBMSK8)                                            
         LR    R5,R2               SAVE START OF NTR1 WORK AREA                 
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E01'                                                  
         L     R1,ANETWS1                 ANETWS1=A(CLIENT RECORD)              
         MVC   KEY+2(3),1(R1)             AM/CLT                                
         MVC   SAVECLT,KEY+3       PXZPXZPXZ                                    
         GOTO1 =V(CLUNPK),DMCB,KEY+3,SAVCLICD,RR=RELO                           
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     ST5                                                              
STBSEQ   LR    R2,R5              RESTORE R2 TO START OF NTR1 WORK AREA         
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
ST5      CLC   KEY(5),KEYSAVE             ID/AM/CLT                             
         BNE   STABX                                                            
         MVC   THISPRD,KEY+5       SET PRODUCT                                  
*******  CLI   THISPRD,X'6F'                                                    
*******  BNE   *+6                                                              
*******  DC    H'0'                                                             
         CLI   NBSELEST,0          CHECK ESTIMATE                               
         BE    ST10                                                             
         CLC   NBSELEST,KEY+6                                                   
         BNE   STBSEQ                                                           
ST10     MVC   THISEST,KEY+6                                                    
         MVC   FILENAME,=C'SPTFIL  '                                            
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
         GOTO1 ADDEM,DMCB,(R0),WORK+50,=AL4(BTGMSK)                             
         B     NXTELEM                                                          
*                                                                               
STABX    DS    0H                                                               
         NETGO NVSETUNT,DMCB                                                    
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
STABXX   XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
AD3      CLI   ESTBUFF,0           IS BUFF FILLED                               
**       BNE   XITADDM                                                          
**       MVC   ESTBUFF,NBACTEST    NO/FILL IT  FOR FUTURE BUFF REC              
**       MVC   PRDBUFF,NBPRD                                                    
**       B     XITADDM                                                          
*                                                                               
****************************                                                    
INACTEST NTR1                                                                   
         L     R1,ATWA                                                          
         USING T31EFFD,R1                                                       
         CLI   SPLEOP,C'I'                                                      
         BNE   INACX                                                            
         DROP  R1                                                               
         CLI   WRTESTSW,C'Y'                                                    
         BNE   INACX                                                            
         MVI   WRTESTSW,C'N'                                                    
*                                                                               
         LA    R4,1                  BIT MASK FOR ACTUAL COST                   
*                                    COST AND DATE FUDGED BELOW                 
         LA    R5,CSTMASKS                                                      
         LH    R7,NUMROWS                                                       
         L     R8,ACURTOTS                                                      
ACTLOOP  ST    R4,FULL             PUT MASK FOR THIS COST IN FULL               
         NC    FULL,0(R5)          IF MASK MATCH                                
         BZ    LOOPIT                                                           
         MVC   FULL,0(R5)                                                       
         NC    FULL,=AL4(SPECMSK)  IF A SPECIAL MASK SET                        
         BZ    ACT2                                                             
         OC    ARGTOADD,ARGTOADD   IF SPECIAL ARG GIVEN                         
         BZ    ACT2                                                             
         NC    FULL,ARGTOADD        THEN MUST ALSO MATCH SPECIAL ARG            
         BZ    LOOPIT                                                           
*                                                                               
**ACT2     BAS   RE,GETDATN             GET DATE OFFSET IN R2                   
**         BNZ   LOOPIT                 IF DATE OUT OF RANGE, SKIP IT           
         SR    R2,R2               SET DATE TO FIRST IN LIST                    
ACT2     STC   R2,BUFFMON                                                       
         MVC   BUFFPRD,PRDBUFF                                                  
         MVC   BUFFEST,ESTBUFF                                                  
         LH    R1,NUMROWS          CALC ROW NUMBER                              
         SR    R1,R7                                                            
         STC   R1,BUFFROW                                                       
         ZAP   BUFFCOST,=P'0'                                                   
         GOTO1 ABUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFFREC                           
         MVI   ESTBUFF,0                                                        
         B     INACX                                                            
LOOPIT   LA    R5,8(R5)            NEXT MASK SET                                
         LA    R8,8(R8)            NEXT SET OF COLUMNS                          
         BCT   R7,ACTLOOP                                                       
         XC    ESTBUFF(2),ESTBUFF                                               
INACX    XIT1                                                                   
****************************************************************                
*                                                                               
******* CONSTANTS                                                               
*                                                                               
EFFEFFS  DC    50XL1'FF'           HEX FF'S TO COMPARE VS. END OF TBL           
ZEROES   DC    50XL1'00'           HEX ZEROES                                   
ZERO     DC    XL1'0F'             PACKED ZERO                                  
MINDAT   DC    XL2'0000'           MINIMUM COMPRESSED DATE                      
MAXDAT   DC    XL2'FFFF'           MAXIMUM COMPRESSED DATE                      
*                                                                               
WRTESTSW DS    CL1                                                              
ESTSV    DS    CL1                                                              
ESTBUFF  DS    CL1                                                              
PRDBUFF  DS    CL1                                                              
DATBUFF  DS    CL2                                                              
*                                                                               
****** ADDRESSES                                                                
BUFFBUFF DC    V(BUFFALOC)                                                      
*                                                                               
         BUFF  LINES=900,ROWS=1,COLUMNS=1,FLAVOR=PACKED,               X        
               KEYLIST=(4,A)                                                    
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*NETLIST'                                                      
NETLIST  DC    6000X'00'                                                        
         SPACE 1                                                                
*****************  1ST ARG IS L'BUFFKEY                                         
*                                                                               
*        INCLUDES                                                               
*                                                                               
*        NEGENUNIT                                                              
*        SPGENCLT                                                               
*        SPGENPRD                                                               
*        SPGENEST                                                               
*        DDBIGBOX                                                               
*        DDBUFFALOD                                                             
*        NEMEDFFD                                                               
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE NEMEDFFD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFED                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLN                                                       
       ++INCLUDE SPGENSTAB                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
**** WORKING STORAGE                                                            
WORKD    DSECT                                                                  
**** COMMON WITH EDIT                                                           
OPTALLOC DS    CL1                 Y IF ALLOCATED UNITS TO BE PRINTED           
OPTUNAL  DS    CL1                 Y IF UNALLOCATED UNITS TO BE PRINTED         
FRMTYPE  DS    CL1                 3=INCLUDE MANUAL BILLS                       
       ++INCLUDE NEAGSMINCL                                                     
*                                                                               
NUMROWS  EQU   NUMCOLS             ROWS HERE ARE COLUMNS IN AGY SUM             
*                                                                               
CASSFLG  DS    CL1                                                              
CASSCOST DS    F                                                                
ARGTOADD DS    F                                                                
*                                                                               
SRCHEL   DS    CL1                                                              
ACURLINE DS    A                   A(CURRENT PRINT LINE)                        
NCURLINS DS    H                   NUMBER OF CURRENT LINES                      
CURPROD  DS    CL3                 CURRENT PRODUCT CODE                         
CURPNAM  DS    CL20                NAME OF CURRENT PRODUCT                      
CURTOTS  DS    D                   CURRENT LINE TOTALS                          
SAVECLT  DS    CL2                 SAVED CLIENT                                 
SAVCLICD DS    CL3                 3-BYTE CLIENT CODE                           
*                                                                               
CURFLAG  DS    CL1                 SET IF CURR LINE HAS NON-ZERO TOTS           
CLIFLAG  DS    CL1                 SET IF CURR CLI HAS NON-ZERO TOTS            
FRSTCLT  DS    CL1                 ZERO IF 1ST CLIENT                           
FRSTPRD  DS    CL1                 ZERO IF FIRST PROD                           
FRSTEST  DS    CL1                 ZERO IF FIRST ESTIMATE                       
FRSTROW  DS    CL1                 ZERO IF FIRST ROW                            
*                                                                               
DUB2     DS    D                   2ND DUB AREA                                 
RELO     DS    A                                                                
*                                                                               
APRDTOTS DS    A                   A(PRD TOTAL AREA)                            
ABUFTOTS DS    A                   A(BEGINNING OF BUFALLO BUFFER)               
*                                                                               
BOXTOPLN DS    H                   LINE NUMBER OF BOX TOP                       
BOXMIDLN DS    H                   LINE NUMBER OF BOX MIDDLE                    
BOXBOTLN DS    H                   LINE NUMBER OF BOX BOTTOM                    
*                                                                               
SAVPCOD  DS    CL3                 SAVE CURRENT PROD CODE                       
SAVPNAM  DS    CL20                SAVE CURRENT PROD NAME                       
*                                                                               
ABUFFALO DS    A                   A(BUFFALO)                                   
*                                                                               
BUFFREC  DS    0D                  RECORD TO PASS TO/FROM BUFFALO               
BUFFKEY  DS    0CL4                IF LENGTH CHANGES BE SURE TO SET             
BUFFPRD  DS    CL1                    BUFFALO MACRO CALL TO NEW LENGTH          
BUFFEST  DS    CL1                                                              
BUFFROW  DS    CL1                                                              
BUFFMON  DS    CL1                                                              
*                                                                               
BUFFCOST DS    CL8                 PACKED DOUBLEWORD TO ADD                     
BUFFRECL EQU   *-BUFFREC                                                        
*                                                                               
SAVBREC  DS    0D                                                               
SAVBKEY  DS    0CL(L'BUFFKEY)      SAVE OLD BUFFALO KEY                         
SAVEPRD  DS    CL1                                                              
SAVEEST  DS    CL1                                                              
SAVEROW  DS    CL1                                                              
SAVEMON  DS    CL1                                                              
*                                                                               
SAVBCOST DS    CL8                                                              
*                                                                               
PBUFMAXL EQU   20                  MAX LINES IN PRINT BUFFER                    
PRFIRST  DS    (PBUFMAXL-1)CL132     INTERNAL PRINT BUFFER                      
PRLAST   DS    CL132                 LAST LINE OF INTERNAL P BUFFER             
PRBUFL   EQU   *-PRFIRST                                                        
*                                                                               
THISPRD  DS    CL1                                                              
THISEST  DS    CL1                                                              
*                                                                               
OLDCLREC DS    CL2000              OLD CLIENT RECORD                            
*                                                                               
******** TOTALS AREAS              KEPT IN DUMMY AREA                           
*                                                                               
TOTAREA  DS    0D                                                               
*                                                                               
* ORGANIZATION:                                                                 
*                                                                               
**MONLIST   (NUMMONS)*4BYTES            MONTH LIST                              
*                                                                               
**AGYTOTS   (NUMMONS*NUMROWS)D          AGENCY TOTALS                           
*                                                                               
**ESTTOTS   (NUMMONS*NUMROWS)D          ESTIMATE  TOTALS                        
*                                                                               
**CLTTOTS   (NUMMONS)D                  TOTALS FOR ONE SET OF DATES             
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
         ORG   PHEAD+3*PHLENGTH                                                 
         DS    CL9                                                              
PHCLICOD DS    CL3                 CLIENT CODE                                  
         DS    CL1                                                              
PHCLINAM DS    CL20                CLIENT NAME                                  
*                                                                               
         ORG   PHEAD+4*PHLENGTH                                                 
         DS    CL9                                                              
PHPRDCOD DS    CL3                 PRODUCT CODE                                 
         DS    CL1                                                              
PHPRDNAM DS    CL20                PRODUCT NAME                                 
         DS    CL11                                                             
PHNETHED DS    CL7                                                              
         DS    CL1                                                              
PHNET    DS    CL4                                                              
         ORG   PHEAD+5*PHLENGTH                                                 
PHFLTHED DS    CL6                  =C'FILTER'                                  
         DS    CL3                                                              
PHEFILT  DS    CL3                  EST FILTER                                  
         ORG   PHEAD+6*PHLENGTH                                                 
PHDUMCHR DS    CL1                 DUMMY CHARACTER TO CAUSE A BLNK LINE         
*                                                                               
*DSECT FOR PRINT LINES                                                          
         DSECT                                                                  
COLINFO  EQU   *                                                                
*                                                                               
CLKEST   DS    CL9                 =C'ESTIMATE'                                 
CLEST    DS    CL3                                                              
         DS    CL1                                                              
CLESTNAM DS    CL20                                                             
         DS    CL1                                                              
CLESTDAT DS    CL17                MMMDD/YY-MMMDD/YY                            
         DS    CL3                                                              
CLESTFTN DS    CL5                 =C'FILT='                                    
         DS    CL1                                                              
CLESTFLT DS    CL3                                                              
*                                                                               
         ORG   COLINFO                                                          
         DS    CL1                                                              
CLHEADER DS    CL10                                                             
         DS    CL1                                                              
CLDATES  DS    0C                  DATES START HERE                             
OUTDATLN EQU   8                   LENGTH OF OUTPUT DATE FIELDS                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007NEMED2E   07/16/07'                                      
         END                                                                    
