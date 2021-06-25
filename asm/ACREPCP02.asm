*          DATA SET ACREPCP02  AT LEVEL 026 AS OF 01/17/13                      
*PHASE ACCP02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE ACCEDIT                                                                
*INCLUDE CHOPCON                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'PERSONNEL INTERFACE TAPE'                                       
ACCP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCP**,R9,R7    R9=2ND BASE REGISTER, R7=3RD BASE            
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACCP02D,RC          RC=A(SAVE W/S)                               
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8         R8=ADDRESSES WIDE PRINT                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,LEDGFRST       LEDGER FIRST                                 
         BE    LDGF                                                             
         CLI   MODE,REQLAST        REQUEST LAST - PRINT SUMMARY                 
         BE    REQL                                                             
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
*              RUN FIRST                                              *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
*                                                                               
         L     R2,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              REQUEST FIRST                                          *         
***********************************************************************         
*                                                                               
REQF     GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         GOTO1 DATCON,DMCB,(5,TODAYP),(1,TODAYP)                                
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         USING ACCOMPD,R4                                                       
         L     R4,ADCMPEL                                                       
         MVC   CMPABBR,ACMPABBR    COMPANY ABBREVIATION                         
         MVC   CMPNAME,SPACES                                                   
         L     R4,ADCMPNAM                                                      
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CMPNAME(0),2(R4)    COMPANY NAME                                 
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              LEDGER FIRST                                           *         
***********************************************************************         
*                                                                               
LDGF     DS    0H                                                               
         USING ACHEIRD,R4                                                       
         L     R4,ADLDGHIR         HEIRARCHY ELEMENT                            
         MVC   LEVELA,ACHRLEVA     LEVEL LENGTHS                                
         MVC   LEVELB,ACHRLEVB                                                  
         MVC   LEVELC,ACHRLEVC                                                  
         MVC   LEVELD,ACHRLEVD                                                  
         MVC   LEVELANM,ACHRDESA   LEVEL NAMES                                  
         MVC   LEVELBNM,ACHRDESB                                                
         MVC   LEVELCNM,ACHRDESC                                                
         MVC   LEVELDNM,ACHRDESD                                                
*                                                                               
         LA    RF,COUNTERS         CLEAR COUNTERS                               
         LA    R0,COUNT#                                                        
         ZAP   0(L'COUNTERS,RF),=P'0'                                           
         LA    RF,L'COUNTERS(RF)                                                
         BCT   R0,*-10                                                          
         MVC   LASTACT,SPACES                                                   
*                                                                               
         OPEN  (INTAPE,(INPUT))    OPEN INPUT TAPE                              
*                                                                               
         USING TAPED,R3                                                         
LDGF100  L     R3,AIOTAPE          R3=A(TAPE RECORD)                            
         GET   INTAPE,(R3)                                                      
         AP    COUNTIN,=P'1'                                                    
         GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         B     LDGF100                                                          
*                                                                               
LDGF200  CLOSE (INTAPE)            CLOSE INPUT TAPE                             
*                                                                               
LDGF300  GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,DMCB+4                                                        
         ST    R3,AIOSORT                                                       
         LTR   R3,R3                                                            
         BZ    LDGFX                                                            
         BAS   RE,VALIDATE         VALIDATE HIGHER LEVEL ACCOUNTS               
         BNE   LDGF300                                                          
         BAS   RE,PROCREC          BUILD/UPDATE ACCFIL RECORDS                  
         B     LDGF300                                                          
*                                                                               
LDGFX    DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE INCOMING TAPE RECORD                          *         
***********************************************************************         
*                                                                               
VALIDATE NTR1                                                                   
         USING TAPED,R3                                                         
         L     R3,AIOSORT          R3=A(TAPE RECORD)                            
         MVI   SVTYPE,BUFERROR                                                  
         MVI   SVSTAT,0                                                         
         MVC   SVNMLV,SPACES                                                    
*                                                                               
*              CHECK FOR MISSING EFFECTIVE DATE                                 
*                                                                               
VAL100   DS    0H                                                               
         CLC   TAPEACT,LASTACT     CHECK FOR DUPLICATE RECORDS                  
         BNE   *+8                                                              
         OI    SVSTAT,BUFIDUP                                                   
         MVC   LASTACT,TAPEACT                                                  
*                                                                               
         CLC   TAPEEFFC,SPACES     CHECK FOR INVALID EFFECTIVE DATE             
         BH    *+8                                                              
         OI    SVSTAT,BUFIEFFC                                                  
         CLC   TAPEEFFC,=C'000000'                                              
         BH    *+8                                                              
         OI    SVSTAT,BUFIEFFC                                                  
*                                                                               
*              VALIDATE PERSON CODE                                             
*                                                                               
         MVI   FLAG,0                                                           
         LA    RF,TAPEACT+L'TAPEACT-1                                           
         SR    R0,R0                                                            
         IC    R0,LEVELD                                                        
         SR    R1,R1                                                            
         IC    R1,LEVELC           RF=A(END OF PERSON CODE)                     
         SR    R0,R1               R0=LENGTH OF LEVELD                          
VAL150   CLI   0(RF),C' '          IF < SPACE THEN PERSON IS INVALID            
         BL    VAR155                                                           
         BE    *+12                                                             
         OI    FLAG,FLAGCHAR       CHARACTER WAS FOUND                          
         B     *+12                                                             
         TM    FLAG,FLAGCHAR       IF HIT A SPACE BUT ALREADY HAD               
         BO    VAR155              A CHARACTER THEN PERSON IS INVALID           
         BCTR  RF,0                                                             
         BCT   R0,VAL150                                                        
         TM    FLAG,FLAGCHAR       PERSON IS OK IF AT LEAST 1 CHAR              
         BO    *+8                                                              
VAR155   OI    SVSTAT,BUFIPER      INVALID PERSON CODE                          
*                                                                               
*              VALIDATE OFFICE LEVEL                                            
*                                                                               
         USING ACTRECD,R4                                                       
VAL200   DS    0H                                                               
         L     R4,AIO1             R4=A(IO1 BUFFER)                             
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY                                                 
         MVC   ACTKUNT(2),=C'1R'                                                
         SR    R1,R1                                                            
         ICM   R1,1,LEVELA                                                      
         SH    R1,=H'1'                                                         
         BM    VALX                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),TAPEACT                                               
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
         CLI   DMCB+8,0                                                         
         BE    *+12                                                             
         OI    SVSTAT,BUFIOFF+BUFIDPT+BUFISDPT                                  
         B     VAL500                                                           
         BAS   RE,GETNAME                                                       
         MVC   SVNMLV1,WORK        SAVE OFFICE NAME                             
*                                                                               
*              VALIDATE DEPARTMENT LEVEL                                        
*                                                                               
VAL300   DS    0H                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY                                                 
         MVC   ACTKUNT(2),=C'1R'                                                
         SR    R1,R1                                                            
         ICM   R1,1,LEVELB                                                      
         SH    R1,=H'1'                                                         
         BM    VALX                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),TAPEACT                                               
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
         CLI   DMCB+8,0                                                         
         BE    *+12                                                             
         OI    SVSTAT,BUFIDPT+BUFISDPT                                          
         B     VAL500                                                           
         BAS   RE,GETNAME                                                       
         MVC   SVNMLV2,WORK        SAVE DEPARTMENT NAME                         
*                                                                               
*              VALIDATE SUBDEPARTMENT LEVEL                                     
*                                                                               
VAL400   DS    0H                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY                                                 
         MVC   ACTKUNT(2),=C'1R'                                                
         SR    R1,R1                                                            
         ICM   R1,1,LEVELC                                                      
         SH    R1,=H'1'                                                         
         BM    VALX                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),TAPEACT                                               
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
         CLI   DMCB+8,0                                                         
         BE    *+12                                                             
         OI    SVSTAT,BUFISDPT                                                  
         B     VAL500                                                           
         BAS   RE,GETNAME                                                       
         MVC   SVNMLV3,WORK        SAVE DEPARTMENT NAME                         
*                                                                               
*              VALIDATE STANDARD HOURS RECORD                                   
*                                                                               
VAL500   DS    0H                                                               
         CLC   TAPEHRS,=C'ACT'     IF USING ACTUAL HOURS                        
         BNE   VALX                THEN STNDRD HRS REC MUST NOT EXIST           
         USING STDRECD,R4                                                       
         L     R4,AIO1             R4=A(IO1 BUFFER)                             
         MVC   STDKEY,SPACES                                                    
         MVI   STDKTYP,STDKTYPQ    X'3E'                                        
         MVI   STDKSUB,STDKSUBQ    X'0D'                                        
         MVC   STDKCPY,QCOMPANY                                                 
         LA    RF,TAPEACT                                                       
         ZIC   R1,LEVELA           *** OFFICE ***                               
         SH    R1,=H'1'                                                         
         BM    VAL525                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STDKOFC(0),0(RF)                                                 
*                                                                               
         LA    RF,TAPEACT          *** DEPARTMENT ***                           
         ZIC   R0,LEVELA                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELB                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         BM    VAL525                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STDKDPT(0),0(RF)                                                 
*                                                                               
         LA    RF,TAPEACT          *** SUBDEPARTMENT ***                        
         ZIC   R0,LEVELB                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELC                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         BM    VAL525                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STDKSBD(0),0(RF)                                                 
*                                                                               
         LA    RF,TAPEACT          *** PERSON ***                               
         ZIC   R0,LEVELC                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELD                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         BM    VAL525                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STDKPER(0),0(RF)                                                 
*                                                                               
VAL525   EQU   *                                                                
*MN      MVI   STDKSEQ,0                                                        
*MN      GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
*MN      CLI   DMCB+8,0                                                         
*MN      BNE   *+8                                                              
*MN      OI    SVSTAT,BUFISTHR                                                  
                                                                                
         MVI   STDKYR,0                                                         
         MVI   STDKSEQ,0                                                        
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO1,AIO2                             
         L     R0,AIO1                                                          
         L     R1,AIO2                                                          
         CLC   0(19,R1),0(R1)                                                   
         BNE   *+8                                                              
         OI    SVSTAT,BUFISTHR                                                  
*MN                                                                             
VALX     DS    0H                                                               
         CLI   SVSTAT,0                                                         
         BE    *+14                                                             
         BAS   RE,PUTBUF                                                        
         AP    COUNTERR,=P'1'                                                   
         CLI   SVSTAT,0            SET CONDITION FLAG                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PROCESS INCOMING TAPE RECORD - BUILD/UPDATE            *         
***********************************************************************         
*                                                                               
PROCREC  NTR1                                                                   
         USING TAPED,R3                                                         
         L     R3,AIOSORT          R3=A(TAPE RECORD)                            
         MVI   LAP,1                                                            
         MVI   SVSTAT,0            CLEAR STATUS FLAGS                           
         MVI   SVSTREC,0                                                        
         MVI   SVPEREC,0                                                        
*                                                                               
PROC100  CLI   LAP,1                                                            
         BNE   PROC200                                                          
         USING ACTRECD,R4          LAP1 = BUILD 1R STAFF RECORD KEY             
         L     R4,AIO1                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY                                                 
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT,TAPEACT                                                  
         MVI   RECORD,RECSTAFF                                                  
         B     PROC300                                                          
*                                                                               
PROC200  CLI   LAP,2                                                            
         BNE   PROCX                                                            
         USING PERRECD,R4          LAP2 = BUILD X'0F' PERSON REC KEY            
         L     R4,AIO1                                                          
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F'                                        
         MVC   PERKCPY,QCOMPANY                                                 
         LA    R2,TAPEACT                                                       
         ZIC   R0,LEVELC                                                        
         AR    R2,R0                                                            
         ZIC   R1,LEVELD                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERKCODE(0),0(R2)                                                
         MVI   RECORD,RECPERSN                                                  
*                                                                               
PROC300  MVC   SVKEY,0(R4)                                                      
         MVI   ACTION,UPDATE       ASSUME RECORD WILL EXIST                     
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCFIL,AIO1,AIO1                     
         TM    DMCB+8,X'10'        TEST RECORD NOT FOUND                        
         BZ    *+8                                                              
         MVI   ACTION,BUILD                                                     
         MVI   ACCOSTAT(R4),0      RESET RECORD STATUS                          
*                                                                               
         LA    R1,SVSTREC          SAVE RECORD/ACTION COMBINATION               
         CLI   RECORD,RECSTAFF                                                  
         BE    *+8                                                              
         LA    R1,SVPEREC                                                       
         OC    0(1,R1),ACTION                                                   
*                                                                               
         L     R4,AIO1                                                          
         CLI   ACTION,BUILD        ONLY CLEAR IO AREA IF BUILDING               
         BNE   *+8                 A NEW RECORD IN IO BUFFER                    
         BAS   RE,XCIO                                                          
         MVC   0(L'SVKEY,R4),SVKEY                                              
*                                                                               
         USING ELTABD,R1           LOOP THROUGH ELEMENT TABLE AND               
         L     R1,AELTAB           PROCESS ALL ELEMENTS FOR THIS                
         LA    R0,ELTAB#           RECORD/ACTION COMBINATION                    
PROC400  MVC   BYTE,RECORD                                                      
         NC    BYTE,ELTRECRD       MUST MATCH RECORD                            
         BZ    PROC450                                                          
         MVC   BYTE,ACTION                                                      
         NC    BYTE,ELTACTN        MUST MATCH ACTION                            
         BZ    PROC450                                                          
         SR    RF,RF                                                            
         ICM   RF,15,ELTADDR       RF=A(ELEMENT ROUTINE)                        
         BASR  RE,RF               BRANCH TO ELEMENT ROUTINE                    
PROC450  LA    R1,L'ELTAB(R1)                                                   
         BCT   R0,PROC400                                                       
*                                                                               
         CLI   QOPT1,C'D'          OPT1=D --> DUMP RECORDS                      
         BNE   *+8                                                              
         BAS   RE,DUMP                                                          
*                                                                               
         CLI   RCWRITE,C'N'        ACTION=UPDATE --> WRITE RECORD BACK          
         BE    PROC500             ACTION=BUILD  --> ADD NEW RECORD             
         MVC   COMMAND,DMWRT                                                    
         CLI   ACTION,BUILD                                                     
         BNE   *+10                                                             
         MVC   COMMAND,DMADD                                                    
         GOTO1 DATAMGR,DMCB,COMMAND,ACCFIL,AIO1,AIO1                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROC500  ZIC   R1,LAP              BUMP TO NEXT LAP                             
         LA    R1,1(R1)                                                         
         STC   R1,LAP                                                           
         B     PROC100                                                          
*                                                                               
PROCX    MVI   SVTYPE,BUFREPRT     SHOW ITEM ON REPORT                          
         BAS   RE,PUTBUF                                                        
*                                                                               
         LA    RF,COUNT1RB         COUNT 1R RECORD/ACTION                       
         TM    SVSTREC,BUILD                                                    
         BO    *+8                                                              
         LA    RF,COUNT1RU                                                      
         AP    0(L'COUNTERS,RF),=P'1'                                           
*                                                                               
         LA    RF,COUNTPEB         COUNT PERSON RECORD/ACTION                   
         TM    SVPEREC,BUILD                                                    
         BO    *+8                                                              
         LA    RF,COUNTPEU                                                      
         AP    0(L'COUNTERS,RF),=P'1'                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PUT RECORD TO BUFFALO                                  *         
***********************************************************************         
*                                                                               
PUTBUF   NTR1                                                                   
         USING TAPED,R3                                                         
         L     R3,AIOSORT                                                       
*                                                                               
         XC    BUFREC(BUFLNQ),BUFREC                                            
         MVC   BUFTYPE,SVTYPE                                                   
         MVC   BUFSTAT,SVSTAT                                                   
         MVC   BUFDATA,TAPEREC                                                  
         MVC   BUFNMLV1,SVNMLV1    OFFICE NAME                                  
         MVC   BUFNMLV2,SVNMLV2    DEPARTMENT NAME                              
         MVC   BUFNMLV3,SVNMLV3    SUBDEPARTMENT NAME                           
         MVC   BUFSTREC,SVSTREC                                                 
         MVC   BUFPEREC,SVPEREC                                                 
         ZAP   BUFACCUM,=P'1'      FORCE BUFFALO TO RETURN RECORD               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              BUILD NAME ELEMENT - X'20'                             *         
***********************************************************************         
*                                                                               
*              ACTION=BUILD  --> BUILD NEW NAME ELEMENT                         
*              ACTION=UPDATE --> DELETE OLD NAME ELEMENT & ADD NEW ONE          
*                                                                               
ELEM20   NTR1                                                                   
         USING TAPED,R3                                                         
         L     R3,AIOSORT                                                       
*                                                                               
         USING NAMELD,R5                                                        
         CLI   ACTION,UPDATE                                                    
         BNE   *+12                                                             
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,DELEL            DELETE OLD NAME ELEMENT                      
*                                                                               
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NAMEL,NAMELQ        X'20' ELEMENT                                
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'TAPELNAM),TAPELNAM                                        
*                                                                               
         LA    R0,L'WORK                                                        
         LA    R1,WORK+L'WORK-1    FLOAT IN COMMA                               
         CLI   0(R1),C' '                                                       
         BH    *+16                                                             
         SH    R1,=H'1'                                                         
         BCT   R0,*-12                                                          
         B     *+8                                                              
         MVI   1(R1),C','                                                       
*                                                                               
         MVC   WORK+L'TAPELNAM(L'TAPEFNAM+L'TAPEMNAM),TAPEFNAM                  
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         ICM   R1,15,DMCB+4                                                     
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NAMEREC(0),WORK                                                  
         ICM   R1,15,DMCB+4                                                     
         LA    R1,NAMLN1Q(R1)                                                   
         STC   R1,NAMLN                                                         
         BAS   RE,ADDEL            ADD NEW NAME ELEMENT TO RECORD               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD ADDRESS ELEMENT - X'22'                          *         
***********************************************************************         
*                                                                               
*              ACTION=BUILD  --> BUILD NEW ADDRESS ELEMENT                      
*              ACTION=UPDATE --> UPDATE EXISTING ELEMENT OR IF ONE              
*                                DOESNT EXIST THEN ADD A NEW ONE                
*                                                                               
ELEM22   NTR1                                                                   
         USING TAPED,R3                                                         
         L     R3,AIOSORT                                                       
*                                                                               
         USING ADRELD,R5                                                        
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ADREL,ADRELQ        X'22' ELEMENT                                
         MVI   ADRLN,ADRLN1Q                                                    
         MVI   ADRNUM,1                                                         
         MVC   ADRADD1,SPACES                                                   
         MVC   ADRADD1(L'TAPELOC),TAPELOC                                       
*                                                                               
         CLI   ACTION,BUILD        ACTION=BUILD  --> ADD NEW ELEMENT            
         BNE   *+12                                                             
         BAS   RE,ADDEL                                                         
         B     EXIT                                                             
*                                                                               
         L     R4,AIO1             ACTION=UPDATE --> GET ADDRESS ELEM           
         MVI   ELCODE,ADRELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         BAS   RE,ADDEL            IF ONE DOESNT EXIST ADD A NEW ONE            
         B     EXIT                ELSE UPDATE LOCATION                         
         MVC   ADRADD1-ADRELD(L'ADRADD1,R4),ADRADD1                             
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD STATUS ELEMENT - X'30'                           *         
***********************************************************************         
*                                                                               
*              ACTION=BUILD  --> BUILD NEW STATUS ELEMENT                       
*              ACTION=UPDATE --> UPDATE EXISTING ELEMENT OR IF ONE              
*                                DOESNT EXIST THEN ADD A NEW ONE                
*                                                                               
ELEM30   NTR1                                                                   
         USING TAPED,R3                                                         
         L     R3,AIOSORT                                                       
*                                                                               
         USING RSTELD,R5                                                        
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RSTEL,RSTELQ        X'30' ELEMENT                                
         MVI   RSTLN,RSTLN2Q                                                    
         MVI   RSTFILT1,C' '                                                    
         MVI   RSTFILT2,C' '                                                    
         MVI   RSTFILT3,C' '                                                    
         MVI   RSTFILT4,C' '                                                    
         MVI   RSTFILT5,C' '                                                    
         MVI   RSTCOSTG,C' '                                                    
         GOTO1 DATCON,DMCB,(5,0),(1,RSTTDATE)                                   
         MVC   RSTBDATE,RSTTDATE                                                
*                                                                               
         CLI   ACTION,BUILD        ACTION=BUILD  --> ADD NEW ELEMENT            
         BNE   *+12                                                             
         BAS   RE,ADDEL                                                         
         B     EXIT                                                             
*                                                                               
         L     R4,AIO1             ACTION=UPDATE --> GET STATUS ELEM            
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         BAS   RE,ADDEL            IF ONE DOESNT EXIST ADD A NEW ONE            
         B     EXIT                ELSE UPDATE ACTIVITY DATES                   
         MVC   RSTTDATE-RSTELD(L'RSTTDATE,R4),RSTTDATE                          
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD BALANCE ELEMENT - X'32'                          *         
***********************************************************************         
*                                                                               
*              ACTION=BUILD  --> BUILD NEW BALANCE ELEMENT                      
*              ACTION=UPDATE --> N/A                                            
*                                                                               
ELEM32   NTR1                                                                   
         USING ABLELD,R5                                                        
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ABLEL,ABLELQ        X'32' ELEMENT                                
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         ZAP   ABLURG,=P'0'                                                     
         BAS   RE,ADDEL                                                         
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD ACCOUNT PEEL ELEMENT - X'33'                     *         
***********************************************************************         
*                                                                               
*              ACTION=BUILD  --> BUILD NEW PEEL ELEMENT                         
*              ACTION=UPDATE --> N/A                                            
*                                                                               
ELEM33   NTR1                                                                   
         USING APOELD,R5                                                        
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   APOEL,APOELQ        X'33' ELEMENT                                
         MVI   APOLN,APOLN2Q                                                    
         ZAP   APODR,=P'0'                                                      
         ZAP   APOCR,=P'0'                                                      
         BAS   RE,ADDEL                                                         
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              EMPLOYEE HISTORY ELEMENT - X'56'                       *         
***********************************************************************         
*                                                                               
ELEM56   NTR1                                                                   
         USING TAPED,R3                                                         
         L     R3,AIOSORT                                                       
*                                                                               
         USING EMPELD,R5                                                        
         XC    SVHIRE,SVHIRE                                                    
         XC    SVTERM,SVTERM                                                    
         CLI   ACTION,UPDATE                                                    
         BNE   ELEM56AA                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,EMPELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   ELEM56AA                                                         
         LR    R5,R4                                                            
         MVC   SVHIRE,EMPHIR       SAVE OLD HIRE AND TERM                       
         MVC   SVTERM,EMPTRM                                                    
         MVI   ELCODE,EMPELQ                                                    
         BAS   RE,DELEL            DELETE OLD ELEMENT                           
*                                                                               
ELEM56AA LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   EMPEL,EMPELQ        X'56' ELEMENT                                
         MVI   EMPLN,EMPLNQ                                                     
         CLC   TAPEHIRE,SPACES                                                  
         BE    ELEM56A                                                          
         GOTO1 DATCON,DMCB,(0,TAPEHIRE),(1,EMPHIR)                              
*                                                                               
ELEM56A  CLC   TAPETERM,SPACES                                                  
         BE    ELEM56B                                                          
         GOTO1 DATCON,DMCB,(0,TAPETERM),(1,EMPTRM)                              
*                                                                               
ELEM56B  CLC   TAPELOCK,SPACES                                                  
         BE    ELEM56C                                                          
         GOTO1 DATCON,DMCB,(0,TAPELOCK),(1,EMPLOCK)                             
*                                                                               
ELEM56C  TM    RECORD,RECPERSN     ONLY SET UP STATUS ON 1R 56 ELEM             
         BO    ELEM56D                                                          
         OC    SVHIRE,SVHIRE                                                    
         BZ    *+10                                                             
         MVC   EMPHIR,SVHIRE       RESTORE HIRE ON 1R'S                         
         OC    SVTERM,SVTERM                                                    
         BZ    *+10                                                             
         MVC   EMPTRM,SVTERM       RESTORE TERM ON 1R'S                         
         CLI   TAPEEXEC,C'Y'       EXECUTIVE                                    
         BNE   *+8                                                              
         OI    EMPSTAT,EMPSEXEC                                                 
         CLC   TAPEHRS,=C'ACT'     ACTUAL HOURS                                 
         BNE   *+8                                                              
         OI    EMPSTAT,EMPSACT                                                  
         CLC   TAPEST(4),=C'TERM'  TERMINATED                                   
         BNE   *+8                                                              
         OI    EMPCSTAT,EMPCTRM                                                 
         CLC   TAPEST(3),=C'LOA'   LEAVE OF ABSENCE                             
         BNE   *+8                                                              
         OI    EMPCSTAT,EMPCLOA                                                 
*                                                                               
ELEM56D  BAS   RE,ADDEL                                                         
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD NAME ELEMENT - X'5A'                             *         
***********************************************************************         
*                                                                               
*              ACTION=BUILD  --> BUILD NEW NAME ELEMENT                         
*              ACTION=UPDATE --> DELETE OLD NAME ELEMENTS AND THEN              
*                                ADD 2 NEW ONES (FIRST, LAST)                   
*                                                                               
ELEM5A   NTR1                                                                   
         USING TAPED,R3                                                         
         L     R3,AIOSORT                                                       
*                                                                               
         USING GPNELD,R5                                                        
         CLI   ACTION,UPDATE                                                    
         BNE   *+12                                                             
         MVI   ELCODE,GPNELQ                                                    
         BAS   RE,DELEL            DELETE OLD NAME ELEMENT                      
*                                                                               
*              ADD NAME ELEMENT - (LAST NAME)                                   
*                                                                               
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   GPNEL,GPNELQ        X'5A' ELEMENT                                
         MVI   GPNTYP,GPNTLST      LAST NAME                                    
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'TAPELNAM),TAPELNAM                                        
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         ICM   R1,15,DMCB+4                                                     
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GPNNME(0),WORK                                                   
         ICM   R1,15,DMCB+4                                                     
         LA    R1,GPNLNQ(R1)                                                    
         STC   R1,GPNLN                                                         
         BAS   RE,ADDEL                                                         
*                                                                               
*              ADD NAME ELEMENT - (FIRST NAME & M.I.)                           
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   GPNEL,GPNELQ        X'5A' ELEMENT                                
         MVI   GPNTYP,GPNTFST      FIRST NAME                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'TAPEFNAM+L'TAPEMNAM),TAPEFNAM                             
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         ICM   R1,15,DMCB+4                                                     
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GPNNME(0),WORK                                                   
         ICM   R1,15,DMCB+4                                                     
         LA    R1,GPNLNQ(R1)                                                    
         STC   R1,GPNLN                                                         
         BAS   RE,ADDEL                                                         
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD LOCATION ELEMENT - X'83'                         *         
***********************************************************************         
*                                                                               
ELEM83   NTR1                                                                   
         USING TAPED,R3                                                         
         L     R3,AIOSORT                                                       
*                                                                               
         USING LOCELD,R5                                                        
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   LOCEL,LOCELQ        X'83' ELEMENT                                
         MVI   LOCLN,LOCLNQ                                                     
         GOTO1 DATCON,DMCB,(0,TAPEEFFC),(1,LOCSTART)                            
*                                                                               
         LA    RF,TAPEACT          OFFICE                                       
         ZIC   R1,LEVELA                                                        
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LOCOFF(0),0(RF)                                                  
         OC    LOCOFF,SPACES                                                    
*                                                                               
         LA    RF,TAPEACT          DEPARTMENT                                   
         ZIC   R0,LEVELA                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELB                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LOCDEPT(0),0(RF)                                                 
         OC    LOCDEPT,SPACES                                                   
*                                                                               
         LA    RF,TAPEACT          SUBDEPARTMENT                                
         ZIC   R0,LEVELB                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELC                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LOCSUB(0),0(RF)                                                  
         OC    LOCSUB,SPACES                                                    
*                                                                               
         CLC   TAPELOCK,SPACES     TIMESHEET LOCK DATE                          
         BNH   ELEM83K                                                          
         GOTO1 DATCON,DMCB,(0,TAPELOCK),(1,LOCLOCK)                             
*                                                                               
ELEM83K  CLI   TAPEEXEC,C'Y'       EMPLOYEE IS AN EXECUTIVE                     
         BNE   *+8                                                              
         OI    LOCATTR,LOCAEXEC                                                 
         CLC   TAPEHRS,=C'ACT'     USE ACTUAL HOURS AS STANDARD HRS             
         BNE   *+8                                                              
         OI    LOCATTR,LOCAACT                                                  
         CLC   TAPEST(4),=C'TERM'  TERMINATED                                   
         BNE   *+8                                                              
         OI    LOCSTAT,LOCSTRM                                                  
         CLC   TAPEST(3),=C'LOA'   LEAVE OF ABSENCE                             
         BNE   *+8                                                              
         OI    LOCSTAT,LOCSLOA                                                  
*                                                                               
         CLI   ACTION,BUILD        ACTION=BUILD  --> ADD NEW ELEMENT            
         BNE   *+12                                                             
         BAS   RE,ADDEL                                                         
         B     EXIT                                                             
*                                                                               
         L     R4,AIO1             FIND MOST CURRENT X'83' ELEMENT              
         AH    R4,=Y(ACCORFST)                                                  
         MVI   ELCODE,LOCELQ       X'83' ELEMENT                                
         XC    WORK,WORK                                                        
ELEM83A  BAS   RE,NEXTEL                                                        
         BNE   ELEM83B                                                          
         CLC   LOCSTART-LOCELD(L'LOCSTART,R4),WORK+LOCSTART-LOCELD              
         BL    ELEM83A                                                          
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
         ST    R4,AELEMENT                                                      
         B     ELEM83A                                                          
*                                                                               
ELEM83B  OC    WORK,WORK           ADD X'83' ELEM IF ONE DIDNT EXIST            
         BNZ   *+12                                                             
         BAS   RE,ADDEL                                                         
         B     EXIT                                                             
*                                                                               
         LA    R4,WORK                                                          
         CLC   LOCSTART,LOCSTART-LOCELD(R4)                                     
         BL    EXIT                                                             
         CLC   LOCOFF,LOCOFF-LOCELD(R4)                                         
         BNE   ELEM83E                                                          
         CLC   LOCDEPT,LOCDEPT-LOCELD(R4)                                       
         BNE   ELEM83E                                                          
         CLC   LOCSUB,LOCSUB-LOCELD(R4)                                         
         BNE   ELEM83E                                                          
*                                                                               
*              IF CURRENT X'83' ELEMENT DOESNT HAVE AN END DATE                 
*              THEN DO NOT REPLACE LOCATION START DATE                          
*                                                                               
         OC    LOCEND-LOCELD(L'LOCEND,R4),LOCEND-LOCELD(R4)                     
         BNZ   ELEM83C                                                          
         MVC   LOCSTART,LOCSTART-LOCELD(R4)                                     
         CLC   TAPETERM,SPACES                                                  
         BE    ELEM83D                                                          
         GOTO1 DATCON,DMCB,(0,TAPETERM),(1,LOCEND)                              
         B     ELEM83D                                                          
*                                                                               
*              IF CURRENT X'83' ELEMENT HAS AN END DATE                         
*              THEN CHECK IF (OLD START) < NEW START < (OLD END)                
*              IF NEW START DATE IS AFTER (OLD ENDDATE) THEN                    
*              ADD THE NEW LOCATION ELEMENT AND ADJUST PERIOD DATES             
*                                                                               
ELEM83C  CLC   LOCSTART,LOCEND-LOCELD(R4)                                       
         BNH   *+16                                                             
         BAS   RE,CHECKDUP         MARK DUPLICATE LOCATIONS                     
         BAS   RE,ADDEL                                                         
         B     ELEM83H                                                          
*                                                                               
*              IF NEW START DATE IS BETWEEN (OLD START) & (OLD END)             
*              THEN USE OLD LOCATION START AND END DATES                        
*                                                                               
         MVC   LOCSTART,LOCSTART-LOCELD(R4)                                     
         MVC   LOCEND,LOCEND-LOCELD(R4)                                         
*                                                                               
ELEM83D  L     RF,AELEMENT         A(83 ELEMENT IN RECORD)                      
         MVI   0(RF),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         BAS   RE,DELEL            DELETE OLD 83 ELEMENT                        
ELEM83E  BAS   RE,CHECKDUP         MARK DUPLICATE LOCATIONS                     
         BAS   RE,ADDEL            THEN ADD NEW LOCATION ELEMENT                
*                                                                               
*              ADJUST START/END PERIOD DATES                                    
*                                                                               
ELEM83F  L     R4,AIO1                                                          
         AH    R4,=Y(ACCORFST)                                                  
         MVI   ELCODE,LOCELQ       X'83' ELEMENT                                
ELEM83G  BAS   RE,NEXTEL                                                        
         BNE   ELEM83H                                                          
         CLC   WORK(LOCLNQ),0(R4)                                               
         BNE   ELEM83G                                                          
         ZIC   RF,1(R4)                                                         
         LA    RF,0(R4,RF)         BUMP TO NEXT ELEMENT                         
         CLI   0(RF),LOCELQ                                                     
         BNE   ELEM83H                                                          
         MVC   WKDATE,LOCSTART-LOCELD(RF)                                       
         GOTO1 DATCON,DMCB,(1,WKDATE),(0,TEMPDT1)                               
         GOTO1 ADDAY,DMCB,TEMPDT1,TEMPDT2,F'-1'                                 
         GOTO1 DATCON,DMCB,(0,TEMPDT2),(1,WKDATE)                               
         MVC   LOCEND-LOCELD(L'LOCEND,R4),WKDATE                                
*                                                                               
ELEM83H  DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DETERMINE HIGHEST DATE FOR CURRENT LOCATION                         *         
***********************************************************************         
         SPACE 1                                                                
CHECKDUP NTR1                                                                   
         L     R4,AIO1                                                          
         AH    R4,=Y(ACCORFST)                                                  
         MVI   ELCODE,LOCELQ       X'83' ELEMENT                                
CHECK10  BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
*                                                                               
         USING LOCELD,R4                                                        
         CLC   LOCOFF(L'LOCOFF+L'LOCDEPT+L'LOCSUB),ELEM+LOCOFF-LOCEL            
         BNE   CHECK10                                                          
         OI    LOCSTAT2,LOCSDUP    MARK AS DUPLICATE                            
         MVI   LOCSTAT,LOCSTRM     MARK AS TERMINATED                           
         B     CHECK10                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              ADD ELEMENT TO RECORD IN AIO1                          *         
***********************************************************************         
*                                                                               
ADDEL    NTR1                                                                   
         LA    R5,ELEM                                                          
         L     R4,AIO1                                                          
         GOTO1 HELLO,DMCB,(C'P',ACCBIG),(R4),(R5)                               
         B     EXIT                                                             
         SPACE 3                                                                
***********************************************************************         
*              DELETE ELEMENT IN AIO1                                 *         
***********************************************************************         
*                                                                               
DELEL    NTR1                                                                   
         L     R4,AIO1                                                          
         GOTO1 HELLO,DMCB,(C'D',ACCBIG),(ELCODE,(R4)),0                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              EXTRACT NAME FROM X'20' ELEMENT                        *         
***********************************************************************         
*                                                                               
GETNAME  NTR1                                                                   
         L     R4,AIO1             R4=A(IO1 BUFFER)                             
         MVC   WORK,SPACES                                                      
         MVI   ELCODE,NAMELQ       X'20'                                        
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
*                                                                               
         USING NAMELD,R4                                                        
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    EXIT                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),NAMEREC                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              CLEAR CURRENT AIO                                      *         
***********************************************************************         
*                                                                               
XCIO     NTR1                                                                   
         LR    R0,R4                                                            
         LA    R1,MAXACC                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              REQUEST LAST                                           *         
***********************************************************************         
*                                                                               
REQL     DS    0H                                                               
         BAS   RE,REPORT                                                        
         BAS   RE,ERROR                                                         
         BAS   RE,RECAP                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PRINT ITEM REPORT                                      *         
***********************************************************************         
*                                                                               
REPORT   NTR1                                                                   
         MVI   RCSUBPRG,0          SET CORRECT HEADINGS                         
         MVI   FORCEHED,C'Y'                                                    
         XC    SVACT,SVACT         CLEAR HEADLINE SAVE ACCOUNT                  
         LA    RF,BUFLNQ                                                        
         XCEF  BUFREC,(RF)                                                      
         MVI   BUFTYPE,BUFREPRT                                                 
         MVC   COMMAND,=CL8'HIGH'                                               
         B     *+10                                                             
*                                                                               
REP100   MVC   COMMAND,=CL8'SEQ'                                                
         GOTO1 BUFFALO,DMCB,COMMAND,ABUFF,BUFREC,1                              
         TM    DMCB+8,X'80'        TEST E-O-F                                   
         BO    EXIT                                                             
         CLI   BUFTYPE,BUFREPRT                                                 
         BNE   REP100                                                           
*                                                                               
         USING PLINED,R6                                                        
         LA    R6,XP                                                            
         MVC   PLINSTAF,BUFPERSN   EMPLOYEE NUMBER & NAME                       
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFLNAM),BUFLNAM                                          
*                                                                               
         LA    R0,L'WORK                                                        
         LA    R1,WORK+L'WORK-1    FLOAT IN COMMA                               
         CLI   0(R1),C' '                                                       
         BH    *+16                                                             
         SH    R1,=H'1'                                                         
         BCT   R0,*-12                                                          
         B     *+8                                                              
         MVI   1(R1),C','                                                       
*                                                                               
         MVC   WORK+L'BUFLNAM(L'BUFFNAM),BUFFNAM                                
         MVC   WORK+L'BUFLNAM+L'BUFFNAM(L'BUFMNAM),BUFMNAM                      
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PLINNAME,WORK                                                    
         CLC   BUFHIRE,SPACES                                                   
         BNH   REP120                                                           
         GOTO1 DATCON,DMCB,(0,BUFHIRE),(5,PLINHIRE)                             
*                                                                               
REP120   CLC   BUFTERM,SPACES                                                   
         BNH   REP130                                                           
         GOTO1 DATCON,DMCB,(0,BUFTERM),(5,PLINTERM)                             
*                                                                               
REP130   CLC   BUFEFFC,SPACES                                                   
         BNH   REP140                                                           
         GOTO1 DATCON,DMCB,(0,BUFEFFC),(5,PLINEFFC)                             
*                                                                               
REP140   CLC   BUFLOCK,SPACES                                                   
         BNH   REP150                                                           
         GOTO1 DATCON,DMCB,(0,BUFLOCK),(5,PLINLOCK)                             
*                                                                               
REP150   MVC   PLINLOC,BUFLOC          LOCATION                                 
*                                                                               
         MVC   PLINHRS,=CL8'STANDARD'  TYPE OF HRS (STANDARD/ACTUAL)            
         CLC   BUFHRS(3),=C'ACT'                                                
         BE    *+10                                                             
         MVC   PLINHRS,=CL8'ACTUAL'                                             
*                                                                               
         CLI   BUFEXEC,C'Y'                                                     
         BNE   *+10                                                             
         MVC   PLINEXEC,=C'YES'    EXECUTIVE INDICATOR                          
*                                                                               
         MVC   PLINST,BUFST        (ACT/LOA/TERM/BLANK)                         
*                                                                               
         MVC   PLINSTRC,=C'UPDATED'     STAFF RECORD STATUS                     
         TM    BUFSTREC,UPDATE                                                  
         BO    *+10                                                             
         MVC   PLINSTRC,=C'CREATED'                                             
*                                                                               
         MVC   PLINPERC,=C'UPDATED'     PERSONNEL RECORD STATUS                 
         TM    BUFPEREC,UPDATE                                                  
         BO    *+10                                                             
         MVC   PLINPERC,=C'CREATED'                                             
*                                                                               
         BAS   RE,PRNTXP                                                        
         B     REP100                                                           
*                                                                               
REP200   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PRINT ERROR REPORT                                     *         
***********************************************************************         
*                                                                               
ERROR    NTR1                                                                   
         MVI   RCSUBPRG,1          SET CORRECT HEADINGS                         
         MVI   FORCEHED,C'Y'                                                    
         LA    RF,BUFLNQ                                                        
         XCEF  BUFREC,(RF)                                                      
         MVI   BUFTYPE,BUFERROR                                                 
         MVC   COMMAND,=CL8'HIGH'                                               
         B     *+10                                                             
*                                                                               
ERR100   MVC   COMMAND,=CL8'SEQ'                                                
         GOTO1 BUFFALO,DMCB,COMMAND,ABUFF,BUFREC,1                              
         TM    DMCB+8,X'80'        TEST E-O-F                                   
         BO    ERRX                                                             
         CLI   BUFTYPE,BUFERROR                                                 
         BNE   ERR100                                                           
*                                                                               
         USING PLINED,R6                                                        
         LA    R6,XP                                                            
*                                                                               
         LA    RE,PERRACT          OFFICE                                       
         LA    RF,BUFACT                                                        
         ZIC   R1,LEVELA                                                        
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         LA    RE,2(R1,RE)                                                      
*                                                                               
         ZIC   R0,LEVELA           DEPARMENT                                    
         LA    RF,BUFACT                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELB                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         LA    RE,2(R1,RE)                                                      
*                                                                               
         ZIC   R0,LEVELB           SUBDEPARTMENT                                
         LA    RF,BUFACT                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELC                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         LA    RE,2(R1,RE)                                                      
*                                                                               
         ZIC   R0,LEVELC           PERSON                                       
         LA    RF,BUFACT                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELD                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         LA    RE,2(R1,RE)                                                      
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFLNAM),BUFLNAM                                          
         LA    R0,L'WORK                                                        
         LA    R1,WORK+L'WORK-1    FLOAT IN COMMA                               
         CLI   0(R1),C' '                                                       
         BH    *+16                                                             
         SH    R1,=H'1'                                                         
         BCT   R0,*-12                                                          
         B     *+8                                                              
         MVI   1(R1),C','                                                       
         MVC   WORK+L'BUFLNAM(L'BUFFNAM),BUFFNAM                                
         MVC   WORK+L'BUFLNAM+L'BUFFNAM(L'BUFMNAM),BUFMNAM                      
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PERRNAME,WORK                                                    
*                                                                               
         TM    BUFSTAT,BUFIOFF     INVALID OFFICE                               
         BZ    *+8                                                              
         MVI   PERROFFC,C'I'                                                    
         TM    BUFSTAT,BUFIDPT     INVALID DEPARTMENT                           
         BZ    *+8                                                              
         MVI   PERRDPT,C'I'                                                     
         TM    BUFSTAT,BUFISDPT    INVALID SUB DEPARTMENT                       
         BZ    *+8                                                              
         MVI   PERRSDPT,C'I'                                                    
*                                                                               
         MVC   WRK2,XSPACES                                                     
         LA    RF,WRK2                                                          
         TM    BUFSTAT,BUFIPER     INVALID PERSON CODE                          
         BZ    *+14                                                             
         MVC   0(25,RF),=CL25'(INVALID PERSON CODE)'                            
         LA    RF,25(RF)                                                        
         TM    BUFSTAT,BUFIEFFC    MISSING OR INVALID EFFECTIVE DATE            
         BZ    *+14                                                             
         MVC   0(30,RF),=CL30'(INVALID EFFECTIVE DATE)'                         
         LA    RF,30(RF)                                                        
         TM    BUFSTAT,BUFISTHR    STANDARD HOURS RECORD EXISTS                 
         BZ    *+14                                                             
         MVC   0(30,RF),=CL30'(STANDARD HOURS REC EXISTS)'                      
         LA    RF,30(RF)                                                        
         TM    BUFSTAT,BUFIDUP     DUPLICATE RECORD ENTRY                       
         BZ    *+10                                                             
         MVC   0(30,RF),=CL30'(DUPLICATE TAPE ENTRY)'                           
         GOTO1 SQUASHER,DMCB,WRK2,L'WRK2                                        
         MVC   PERROTHR,WRK2                                                    
         BAS   RE,PRNTXP                                                        
         B     ERR100                                                           
*                                                                               
ERRX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PRINT RECAP REPORT                                     *         
***********************************************************************         
*                                                                               
RECAP    NTR1                                                                   
         MVI   RCSUBPRG,2          SET CORRECT HEADINGS                         
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         USING PLINED,R6                                                        
         LA    R6,XP                                                            
         LA    R2,COUNTERS                                                      
         LA    R3,RECAPMSG                                                      
         LA    R4,COUNT#                                                        
*                                                                               
RECAP100 MVC   PXDESC,0(R3)                                                     
         EDIT  (P8,0(R2)),(7,PXCOUNT)                                           
         BAS   RE,PRNTXP                                                        
         LA    R2,L'COUNTERS(R2)                                                
         LA    R3,L'PXDESC(R3)                                                  
         BCT   R4,RECAP100                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PRINT A LINE                                           *         
***********************************************************************         
*                                                                               
PRNTXP   NTR1                                                                   
         MVC   XHEAD2+15(L'CMPABBR),CMPABBR                                     
         MVC   XHEAD2+24(L'CMPNAME),CMPNAME                                     
*                                                                               
         CLI   RCSUBPRG,0          REGULAR REPORT BOXES                         
         BNE   PRNTX                                                            
*                                                                               
         SR    R1,R1               NEW PAGE ON CHANGE OF OFF/DPT/SDPT           
         ICM   R1,1,LEVELC                                                      
         SH    R1,=H'1'                                                         
         EXCLC R1,BUFACT,SVACT                                                  
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVACT,BUFACT                                                     
*                                                                               
         MVC   XHEAD4(L'LEVELANM),LEVELANM                                      
         LA    RF,BUFACT                                                        
         ZIC   R1,LEVELA                                                        
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   XHEAD4+15(0),0(RF)                                               
         MVC   XHEAD4+24(L'BUFNMLV1),BUFNMLV1                                   
*                                                                               
         MVC   XHEAD5(L'LEVELBNM),LEVELBNM                                      
         LA    RF,BUFACT                                                        
         ZIC   R0,LEVELA                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELB                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   XHEAD5+15(0),0(RF)                                               
         MVC   XHEAD5+24(L'BUFNMLV2),BUFNMLV2                                   
*                                                                               
         MVC   XHEAD6(L'LEVELCNM),LEVELCNM                                      
         LA    RF,BUFACT                                                        
         ZIC   R0,LEVELB                                                        
         AR    RF,R0                                                            
         ZIC   R1,LEVELC                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   XHEAD6+15(0),0(RF)                                               
         MVC   XHEAD6+24(L'BUFNMLV3),BUFNMLV3                                   
*                                                                               
PRNTX    GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DUMP RECORDS                                           *         
***********************************************************************         
*                                                                               
DUMP     NTR1                                                                   
         L     R4,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,ACCORLEN(R4)                                                
         LA    R2,=C'UPDATE REC'                                                
         CLI   ACTION,BUILD                                                     
         BNE   *+8                                                              
         LA    R2,=C'BUILD REC '                                                
         GOTO1 PRNTBL,DMCB,(10,(R2)),(R4),C'DUMP',(RF),=C'2D'                   
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              EXTERNAL ADDRESS LIST                                  *         
***********************************************************************         
*                                                                               
ADCONS   DS    0F                                                               
         DC    V(SORTER)                                                        
         DC    V(PRNTBL)                                                        
         DC    V(DATVAL)                                                        
         DC    V(SQUASHER)                                                      
         DC    V(CHOPCON)                                                       
         DC    V(HELLO)                                                         
         DC    A(ELTAB)                                                         
         DC    A(BUFFALOC)                                                      
         DC    A(IO1)                                                           
         DC    A(IO2)                                                           
         DC    A(IOTAPE)                                                        
         EJECT                                                                  
***********************************************************************         
*              ELEMENT TABLES                                         *         
***********************************************************************         
*                                                                               
*        DS    CL1 = RECORD TYPES WHERE THIS ELEM MUST BE BUILT/UPDATED         
*                    X'10' = 1R RECORD     (CPY/U/L/OF/DPT/SDPT/PER)            
*                    X'01' = PERSON RECORD (X'0F'/CPY/PERSON)                   
*        DS    AL4 = ADDRESS OF ROUTINE TO HANDLE ELEMENT                       
*        DS    AL1 = ACTIONS WHEN THIS ROUTINE SHOULD BE EXECUTED               
*                    BUILD  = ONLY EXECUTE IF BUILDING A NEW RECORD             
*                    UPDATE = ONLY EXECUTE IF UPDATING AN OLD RECORD            
*                                                                               
*                                                                               
ELTAB    DS    0XL6                                                             
         DC    X'10',AL4(ELEM20),AL1(BUILD+UPDATE)                              
         DC    X'10',AL4(ELEM22),AL1(BUILD+UPDATE)                              
         DC    X'10',AL4(ELEM30),AL1(BUILD+UPDATE)                              
         DC    X'10',AL4(ELEM32),AL1(BUILD)                                     
         DC    X'10',AL4(ELEM33),AL1(BUILD)                                     
         DC    X'11',AL4(ELEM56),AL1(BUILD+UPDATE)                              
         DC    X'11',AL4(ELEM5A),AL1(BUILD+UPDATE)                              
         DC    X'01',AL4(ELEM83),AL1(BUILD+UPDATE)                              
ELTAB#   EQU   (*-ELTAB)/L'ELTAB                                                
         EJECT                                                                  
***********************************************************************         
*              LITERALS                                               *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
RECAPMSG DC    CL30'RECORDS IN FROM TAPE      '                                 
         DC    CL30'1R RECORDS CREATED        '                                 
         DC    CL30'1R RECORDS UPDATED        '                                 
         DC    CL30'PERSON RECORDS CREATED    '                                 
         DC    CL30'PERSON RECORDS UPDATED    '                                 
         DC    CL30'RECORDS FROM TAPE W/ERRORS'                                 
*                                                                               
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCBIG   DC    CL8'ACCBIG  '                                                    
*                                                                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=112 '                                     
SORTCARD DC    C'SORT FIELDS=(1,12,A),FORMAT=BI,WORK=1 '                        
*                                                                               
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,MACRF=GM,EODAD=LDGF200,          X        
               RECFM=FB,LRECL=112,BLKSIZE=24528,BUFNO=1                         
*                                                                               
         BUFF  FLAVOR=PACKED,                                          X        
               KEYLIST=(13,A),                                         X        
               COMMENT=211,                                            X        
               LINES=100,                                              X        
               COLUMNS=1,                                              X        
               ROWS=1                                                           
         EJECT                                                                  
***********************************************************************         
*              BOX HOOK                                               *         
***********************************************************************         
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
*                                                                               
         USING BOXD,R4                                                          
         L     R4,ADBOX                                                         
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXCOLS,C'L'                                                     
*                                                                               
         CLI   RCSUBPRG,0          REGULAR REPORT BOXES                         
         BNE   BX100                                                            
         MVI   BOXCOLS+22,C'C'                                                  
         MVI   BOXCOLS+55,C'C'                                                  
         MVI   BOXCOLS+66,C'C'                                                  
         MVI   BOXCOLS+77,C'C'                                                  
         MVI   BOXCOLS+88,C'C'                                                  
         MVI   BOXCOLS+99,C'C'                                                  
         MVI   BOXCOLS+109,C'C'                                                 
         MVI   BOXCOLS+120,C'C'                                                 
         MVI   BOXCOLS+127,C'C'                                                 
         MVI   BOXCOLS+134,C'C'                                                 
         MVI   BOXCOLS+144,C'C'                                                 
         MVI   BOXCOLS+154,C'R'                                                 
         B     BX300                                                            
*                                                                               
BX100    CLI   RCSUBPRG,1          ERROR REPORT BOXES                           
         BNE   BX200                                                            
         MVI   BOXCOLS+22,C'C'                                                  
         MVI   BOXCOLS+55,C'C'                                                  
         MVI   BOXCOLS+59,C'C'                                                  
         MVI   BOXCOLS+63,C'C'                                                  
         MVI   BOXCOLS+68,C'C'                                                  
         MVI   BOXCOLS+162,C'R'                                                 
         B     BX300                                                            
*                                                                               
BX200    CLI   RCSUBPRG,2          RECAP BOXES                                  
         BNE   BX300                                                            
         MVI   BOXCOLS+35,C'C'                                                  
         MVI   BOXCOLS+50,C'R'                                                  
*                                                                               
BX300    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
         XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              BUFFERS                                                *         
***********************************************************************         
*                                                                               
IO1      DS    0D                  IOAREA #1                                    
         DS    2000C                                                            
MAXACC   EQU   *-IO1                                                            
*                                                                               
IO2      DS    0D                  IOAREA #2                                    
         DS    2000C                                                            
*                                                                               
IOTAPE   DS    0D                  IO AREA FOR IN-TAPE                          
         DS    500C                                                             
         EJECT                                                                  
***********************************************************************         
*              ELEMENT TABLE DSECT                                    *         
***********************************************************************         
*                                                                               
ELTABD   DSECT                                                                  
ELTRECRD DS    XL1                 RECORD TYPE                                  
ELTADDR  DS    AL4                 A(ELEMENT ROUTINE)                           
ELTACTN  DS    XL1                 ACTION                                       
ELTABQ   EQU   *-ELTABD                                                         
         EJECT                                                                  
***********************************************************************         
*              PRINT LINE DSECT                                       *         
***********************************************************************         
*                                                                               
PLINED   DSECT                                                                  
PLIN     DS    0C                                                               
         DS    CL2                                                              
PLINSTAF DS    CL5                 EMPLOYEE NUMBER                              
         DS    CL17                                                             
PLINNAME DS    CL30                NAME (LAST, FIRST M.)                        
         DS    CL3                                                              
PLINHIRE DS    CL8                 HIRE DATE YYMMDD                             
         DS    CL3                                                              
PLINTERM DS    CL8                 TERMINATION DATE                             
         DS    CL3                                                              
PLINEFFC DS    CL8                 EFFECTIVE DATE YYMMDD                        
         DS    CL3                                                              
PLINLOCK DS    CL8                 TIMESHEET LOCK DATE YYMMDD                   
         DS    CL3                                                              
PLINLOC  DS    CL5                 LOCATION                                     
         DS    CL5                                                              
PLINHRS  DS    CL8                 HOURS TYPE (STANDARD/ACTUAL)                 
         DS    CL3                                                              
PLINEXEC DS    CL3                 EXECUTIVE                                    
         DS    CL3                                                              
PLINST   DS    CL5                 'ACT' 'LOA' 'TERM' OR BLANK                  
         DS    CL3                                                              
PLINSTRC DS    CL7                 STAFF RECORD STATUS                          
         DS    CL3                                                              
PLINPERC DS    CL7                 PERSONNEL RECORD STATUS                      
PLINLNQ  EQU   *-PLINED                                                         
*                                                                               
         ORG   PLIN                                                             
         DS    CL2                                                              
PERRACT  DS    CL15                OF/DEPT/SDPT/PERSON                          
         DS    CL7                                                              
PERRNAME DS    CL30                NAME (LAST, FIRST M.)                        
         DS    CL3                                                              
PERROFFC DS    CL1                                                              
         DS    CL3                                                              
PERRDPT  DS    CL1                                                              
         DS    CL3                                                              
PERRSDPT DS    CL1                                                              
         DS    CL4                                                              
PERROTHR DS    CL60                                                             
*                                                                               
         ORG   PLIN                                                             
         DS    CL2                                                              
PXDESC   DS    CL30                                                             
         DS    CL10                                                             
PXCOUNT  DS    CL7                                                              
         EJECT                                                                  
***********************************************************************         
*              INPUT TAPE DSECT                                       *         
***********************************************************************         
*                                                                               
TAPED    DSECT                                                                  
TAPEREC  DS    0CL112                                                           
TAPEACT  DS    CL12                OFF/DPT/SDPT/PERS                            
*                                                                               
TAPELNAM DS    CL18                LAST NAME                                    
TAPEFNAM DS    CL18                FIRST NAME                                   
TAPEMNAM DS    CL1                 MIDDLE INITIAL                               
*                                                                               
TAPEHIRE DS    CL6                 HIRE DATE YYMMDD                             
TAPETERM DS    CL6                 TERMINATION DATE                             
TAPELOC  DS    CL5                 LOCATION                                     
TAPEHRS  DS    CL3                 HOURS TYPE (STANDARD/ACTUAL)                 
TAPEEFFC DS    CL6                 EFFECTIVE DATE YYMMDD                        
TAPEEXEC DS    CL1                 EXECUTIVE                                    
TAPELOCK DS    CL6                 TIMESHEET LOCK DATE YYMMDD                   
TAPEST   DS    CL5                 'ACT' 'LOA' 'TERM' OR BLANK                  
         DS    CL25                SPARE                                        
TAPELNQ  EQU   *-TAPED                                                          
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE                                        *         
***********************************************************************         
*                                                                               
ACCP02D  DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
SORTER   DS    A                   SORTER                                       
PRNTBL   DS    A                   PRNTBL                                       
DATVAL   DS    A                   DATE VALIDATION                              
SQUASHER DS    A                   SQUASHER                                     
CHOPCON  DS    A                   CHOPPER                                      
HELLO    DS    A                   HELLO                                        
AELTAB   DS    A                   ELEMENT TABLE                                
ABUFF    DS    A                   BUFFALO                                      
AIO1     DS    A                   IO AREA #1 (2000 BYTES)                      
AIO2     DS    A                   IO AREA #2 (2000 BYTES)                      
AIOTAPE  DS    A                   IO AREA FOR IN-TAPE                          
VTYPLNQ  EQU   *-VTYPES                                                         
AIOSORT  DS    A                   A(SORT RECORD)                               
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
AELEMENT DS    A                   TEMPORARY SAVE ADDRESS OF ELEMENT            
CMPABBR  DS    CL7                 COMPANY ABBREVIATION                         
CMPNAME  DS    CL36                COMPANY NAME                                 
ELCODE   DS    XL1                                                              
FLAG     DS    XL1                                                              
FLAGSPCE EQU   X'01'               SPACE WAS ENCOUNTERED                        
FLAGCHAR EQU   X'02'               CHARACTER WAS ENCOUNTERED                    
LAP      DS    XL1                                                              
COMMAND  DS    CL8                 USED IN DATAMGR CALL                         
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
ELEM     DS    CL255               ELEMENT BUFFER                               
*                                                                               
WKDATE   DS    PL3                 TEMPORARY WORK DATE                          
TEMPDT1  DS    CL6                 TEMPORARY WORK DATE                          
TEMPDT2  DS    CL6                 TEMPORARY WORK DATE                          
*                                                                               
LASTACT  DS    CL12                                                             
SVTYPE   DS    XL1                                                              
SVSTAT   DS    XL1                                                              
SVACT    DS    CL12                                                             
SVNMLV   DS    0CL108                                                           
SVNMLV1  DS    CL36                OFFICE LEVEL NAME                            
SVNMLV2  DS    CL36                DEPARTMENT LEVEL NAME                        
SVNMLV3  DS    CL36                SUBDEPARTMENT LEVEL NAME                     
SVSTREC  DS    XL1                 STAFF RECORD ACTION                          
SVPEREC  DS    XL1                 PERSONNEL RECORD ACTION                      
SVKEY    DS    CL42                                                             
SVHIRE   DS    CL3                                                              
SVTERM   DS    CL3                                                              
*                                                                               
COUNTERS DS    0PL8                                                             
COUNTIN  DS    PL8                 NUMBER OF INPUT RECORDS FROM TAPE            
COUNT1RB DS    PL8                 NUMBER OF 1R RECORDS BUILT                   
COUNT1RU DS    PL8                 NUMBER OF 1R RECORDS UPDATED                 
COUNTPEB DS    PL8                 NUMBER OF PERSON RECORDS BUILT               
COUNTPEU DS    PL8                 NUMBER OF PERSON RECORDS UPDATED             
COUNTERR DS    PL8                 NUMBER OF INPUT RECORDS WITH ERRORS          
COUNT#   EQU   (*-COUNTERS)/8                                                   
*                                                                               
LEVELS   DS    0H                                                               
LEVELA   DS    CL1                 LENGTH OF LEVEL A                            
LEVELB   DS    CL1                 LENGTH OF LEVEL B                            
LEVELC   DS    CL1                 LENGTH OF LEVEL C                            
LEVELD   DS    CL1                 LENGTH OF LEVEL D                            
*                                                                               
LEVELNM  DS    0CL15                                                            
LEVELANM DS    CL15                LEDGER LEVEL NAMES (HIERARCHY)               
LEVELBNM DS    CL15                                                             
LEVELCNM DS    CL15                                                             
LEVELDNM DS    CL15                                                             
*                                                                               
RECORD   DS    XL1                 RECORD TYPE                                  
RECSTAFF EQU   X'10'               1R STAFF RECORD                              
RECPERSN EQU   X'01'               X'0F' PERSON RECORD                          
*                                                                               
ACTION   DS    XL1                 MODE FOR CURRENT INPUT RECORD                
BUILD    EQU   X'80'               BUILD A NEW RECORD                           
UPDATE   EQU   X'40'               UPDATE AN OLD RECORD                         
*                                                                               
*              BUFFALO RECORD                                                   
*                                                                               
BUFREC   DS    0CL124                                                           
BUFKEY   DS    0CL13               *** BUFFALO KEY ***                          
BUFTYPE  DS    CL1                                                              
BUFREPRT EQU   X'01'               SHOW ON REGULAR REPORT                       
BUFERROR EQU   X'02'               SHOW ON ERROR REPORT                         
BUFDATA  DS    0CL112                                                           
BUFACT   DS    0CL12                                                            
*                                  *** BUFFALO DATA ***                         
BUFOFFC  DS    CL2                 OFFICE                                       
BUFDPT   DS    CL3                 DEPARTMENT                                   
BUFSDPT  DS    CL2                 SUBDEPARTMENT                                
BUFPERSN DS    CL5                 EMPLOYEE NUMBER                              
BUFLNAM  DS    CL18                LAST NAME                                    
BUFFNAM  DS    CL18                FIRST NAME                                   
BUFMNAM  DS    CL1                 MIDDLE INITIAL                               
BUFHIRE  DS    CL6                 HIRE DATE YYMMDD                             
BUFTERM  DS    CL6                 TERMINATION DATE                             
BUFLOC   DS    CL5                 LOCATION                                     
BUFHRS   DS    CL3                 HOURS TYPE (STANDARD/ACTUAL)                 
BUFEFFC  DS    CL6                 EFFECTIVE DATE YYMMDD                        
BUFEXEC  DS    CL1                 EXECUTIVE                                    
BUFLOCK  DS    CL6                 TIMESHEET LOCK DATE YYMMDD                   
BUFST    DS    CL5                 'ACT' 'LOA' 'TERM' OR BLANK                  
         DS    CL25                SPARE                                        
BUFNAMES DS    0CL108                                                           
BUFNMLV1 DS    CL36                OFFICE NAME                                  
BUFNMLV2 DS    CL36                DEPARTMENT NAME                              
BUFNMLV3 DS    CL36                SUBDEPARTMENT NAME                           
BUFSTAT  DS    CL1                                                              
BUFIOFF  EQU   X'01'               INVALID OFFICE                               
BUFIDPT  EQU   X'02'               INVALID DEPARTMENT                           
BUFISDPT EQU   X'04'               INVALID SUB DEPARTMENT                       
BUFIPER  EQU   X'08'               INVALID PERSON CODE                          
BUFIEFFC EQU   X'10'               MISSING OR INVALID EFFECTIVE DATE            
BUFISTHR EQU   X'20'               STANDARD HOURS RECORD EXISTS                 
BUFIDUP  EQU   X'40'               DUPLICATE RECORD ON INPUT TAPE               
BUFSTREC DS    XL1                                                              
BUFPEREC DS    XL1                                                              
*                                                                               
BUFACCUM DS    CL8                 *** BUFFALO ACCUMS ***                       
BUFLNQ   EQU   *-BUFREC                                                         
*                                                                               
WRK2     DS    CL150                                                            
         EJECT                                                                  
***********************************************************************         
*              OTHER INCLUDES                                         *         
***********************************************************************         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026ACREPCP02 01/17/13'                                      
         END                                                                    
