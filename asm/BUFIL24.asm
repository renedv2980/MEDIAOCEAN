*          DATA SET BUFIL24    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T50214A                                                                  
         TITLE 'T50214 - BUDGET CONTROL LFM - PLAN COPY FUNCTION'               
T50214   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI14**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R5,ANODBLK          R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
*                                                                               
         GOTO1 VSETADD                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BE    VKEY                                                             
         CLI   MODE,VALREC                                                      
         BE    COPY                COPY PLAN RECORDS                            
         CLI   MODE,PRINTREP                                                    
         BE    COPY                COPY PLAN RECORDS AND PRINT REPORT           
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE KEY FIELDS                                                           
*                                                                               
* VALIDATE FROM CLIENT                                                          
*                                                                               
VKEY     BAS   RE,CLRNAME                                                       
         GOTO1 VVALCLT,PARAS,PCYFCLTH,0                                         
         MVC   FRCLT,CLTCODE       SAVE 'FROM' CLIENT CODE                      
         MVC   FRFISCAL,CLTFIS     AND ITS FISCAL YEAR                          
         GOTO1 CLTOUT,0            DISPLAY 'FROM' CLIENT DATA                   
*                                                                               
* VALIDATE FROM PRODUCT                                                         
*                                                                               
VKEY2    GOTO1 VVALPRD,PARAS,PCYFPRDH,0                                         
         MVC   FRPRD,PRDCODE       SAVE 'FROM' PRODUCT CODE                     
         MVC   PCYFPRN,PRDNAM                                                   
*                                                                               
* VALIDATE FROM PLAN                                                            
*                                                                               
VKEY4    GOTO1 VVALPLAN,PARAS,PCYFPLAH,0                                        
         MVC   FRPLAN,PLANCODE                                                  
         MVC   FRPLNKEY,NODKEY     SAVE 'FROM' PLAN NODAL KEY                   
         MVC   PCYFPLN(L'PLANNAM),PLANNAM                                       
         GOTO1 VPEROUT,PARAS,(1,PLANST),WORK                                    
         LA    R2,PCYFPLN+L'PLANNAM+1                                           
         MVC   0(13,R2),WORK                                                    
*                                                                               
         TM    WHEN,X'18'          TEST PROCESSING OVERNIGHT                    
         BNZ   VKEY5               YES-SKIP PLAN SIZE CHECK                     
         CLC   PLANCNT,=Y(MAXPLAN)  TEST IF PLAN IS TOO BIG FOR COPY            
         BNH   VKEY5                                                            
         MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BIGMSG),BIGMSG                                            
         B     SPERR                                                            
*                                                                               
* INITIALIZE FOR TO FIELD EDITS                                                 
*                                                                               
VKEY5    XC    CLTVALS,CLTVALS                                                  
         XC    PRDVALS,PRDVALS                                                  
         XC    PLANVALS,PLANVALS                                                
*                                                                               
* VALIDATE TO CLIENT                                                            
*                                                                               
VKEY6    GOTO1 VVALCLT,PARAS,PCYTCLTH,(C'D',FRCLT)                              
         MVC   TOCLT,CLTCODE                                                    
         MVC   TOFISCAL,CLTFIS     SAVE 'TO' CLIENT FISCAL YEAR                 
         GOTO1 CLTOUT,1                                                         
*                                                                               
* VALIDATE TO PRODUCT                                                           
*                                                                               
VKEY8    GOTO1 VVALPRD,PARAS,PCYTPRDH,(C'D',FRPRD)                              
         MVC   TOPRD,PRDCODE                                                    
         MVC   PCYTPRN,PRDNAM                                                   
*                                                                               
* VALIDATE TO PLAN                                                              
*                                                                               
VKEY10   GOTO1 VGETFLD,PARAS,(X'FF',PCYTPLAH)                                   
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    SPERR                                                            
         MVC   PLANCODE,FLD        SAVE NEW PLAN CODE                           
         MVC   TOPLAN,FLD                                                       
         GOTO1 VSETKEY                                                          
         MVC   TOPLNKEY,NODKEY                                                  
*                                                                               
VKEY11   MVI   ERROR,INVALID                                                    
         CLC   FRCNTLS(FRCNTLN),TOCNTLS   TEST TO PLAN=FROM PLAN                
         BE    SPERR                                                            
*                                                                               
         XC    NDHOOK,NDHOOK                                                    
         MVI   NDDELRSW,YES        SET TO PASS DELETES                          
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0             TEST FOR ERROR                               
         BNE   VKEY12              NO-SO OK TO COPY INTO                        
*                                                                               
         MVI   ERROR,RECEXIST                                                   
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),BUKEY                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 READ                                                             
         TM    KEY+(BUKCSTA-BUKEY),X'80'                                        
         BZ    *+8                                                              
         MVI   ERROR,DELEXIST                                                   
         B     SPERR                                                            
*                                                                               
* EDIT TO PLAN NAME                                                             
*                                                                               
VKEY12   MVI   NDDELRSW,NO         TURN OFF PASS DELETES SWITCH                 
         GOTO1 VGETFLD,PARAS,PCYTNAMH                                           
         CLI   FLDH+5,0                                                         
         BE    *+10                                                             
         MVC   TOPLNNAM,FLD        NEW PLAN NAME                                
*                                                                               
* EDIT TO PLAN PERIOD                                                           
*                                                                               
VKEY14   GOTO1 VGETFLD,PARAS,PCYTPERH                                           
         CLI   FLDH+5,0                                                         
         BNE   VKEY15                                                           
         CLC   TOFISCAL,FRFISCAL   TEST FOR DIFFERENT FISCAL YEARS              
         BE    VKEYX               NO-DON'T NEED NEW FISCAL PERIOD              
         MVI   ERROR,MISSING                                                    
         B     SPERR                                                            
*                                                                               
VKEY15   MVI   ERROR,INVDATE       VALIDATE THE PERIOD                          
         XC    FULL,FULL                                                        
         SR    R0,R0                                                            
         ICM   R0,8,CLTSTART+1                                                  
         ICM   R0,4,CLTTYPE                                                     
         ICM   R0,2,YESPARM        ACCEPT OPEN ENDED PLAN                       
         GOTO1 VMONVAL,DMCB,FLD,FULL,(R0)                                       
         OC    4(4,R1),4(R1)                                                    
         BZ    SPERR                                                            
         MVC   TOPLNST,4(R1)                                                    
         MVC   TOPLNEND,6(R1)                                                   
*                                                                               
VKEY16   CLI   TOPLNST,0           TEST FOR OPEN-ENDED PLAN                     
         BE    VKEYX               YES-SKIP CHECKS ON DATES                     
         MVI   ERROR,INVEBFRS                                                   
         CLC   TOPLNST,TOPLNEND                                                 
         BH    SPERR                                                            
         MVI   ERROR,MOREFIS                                                    
*                                                                               
         LA    RF,12               RF=N'MONTHS IN YEAR                          
         CLI   CLTTYPE,10                                                       
         BNE   *+8                                                              
         LA    RF,13                                                            
         ST    RF,FULL                                                          
         ZIC   R1,TOPLNST          GET START YEAR                               
         M     R0,FULL             CONVERT TO MONTHS                            
         ZIC   R0,TOPLNST+1                                                     
         AR    R1,R0               ADD IN MONTHS                                
         ZIC   RF,TOPLNEND         GET END YEAR                                 
         M     RE,FULL                                                          
         ZIC   RE,TOPLNEND+1                                                    
         AR    RF,RE               CONVERT END YEAR/MONTH TO MONTHS             
         SR    RF,R1               RF=N'MONTHS BETWEEN                          
         LA    RF,1(RF)            N'MONTHS INCLUSIVE                           
         C     RF,FULL             TEST FOR MORE THAN FISCAL YEAR               
         BH    SPERR                                                            
*                                                                               
VKEYX    TM    WHEN,X'18'          TEST PROCESSING OVERNIGHT                    
         BNZ   *+8                 YES                                          
         OI    WHENOK,X'01'        BYPASS GENCON REPORT MAINTENANCE             
         B     XIT                                                              
         EJECT                                                                  
* COPY PLAN AND OUTLINES (VALREC AND PRINTREP PROCESSING)                       
*                                                                               
COPY     CLI   MODE,VALREC         TEST PROCESSING ON-LINE                      
         BE    COPY1                                                            
         LA    R1,HEDSPECS         INITIALIZE FOR REPORT                        
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
COPY1    XC    NDHOOK,NDHOOK                                                    
         MVC   NODKEY,TOPLNKEY                                                  
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    COPY2               YES-ADD TO END OF PLANS                      
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   BEFCODE,NDLVCOD     EXTRACT NEXT HIGHER PLAN CODE                
*                                                                               
COPY2    LA    R1,COPY6                                                         
         ST    R1,NDHOOK                                                        
         CLI   OFFLINE,YES         IF OFF-LINE                                  
         BNE   *+8                 FORCE NODIO TO UPDATE MASTER                 
         MVI   NDUPMAST,YES        WHEN ADDING EACH NEW NODE                    
         MVI   BYTE,C'B'           SET TO ADD BEFORE                            
         LA    R0,L'PLANCODE                                                    
         LA    R3,BEFCODE                                                       
         OC    BEFCODE,BEFCODE     TEST FOR POSITIONING CODE                    
         BNZ   COPY4               YES                                          
         MVI   BYTE,C'A'                                                        
         SR    R0,R0                                                            
         SR    R3,R3                                                            
*                                                                               
COPY4    GOTO1 VNODIO,DMCB,NODBLKD,(BYTE,=C'COPY'),FRPLNKEY,           X        
               ((R0),(R3)),TOPLNKEY                                             
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   MODE,PRINTREP       TEST PRINTING REPORT                         
         BNE   *+12                YES-ALL DONE                                 
         BAS   RE,REP              PRODUCE REPORT                               
         B     COPYX                                                            
*                                                                               
         MVC   CONHEAD(L'COPYMSG),COPYMSG                                       
         LA    R2,CONACTH                                                       
         ST    R2,ACURFORC                                                      
*                                                                               
COPYX    B     XIT                                                              
         SPACE 2                                                                
* HOOK ROUTINE FOR PROCESSING RECORDS DURING COPY                               
*                                                                               
COPY6    NTR1                                                                   
*                                                                               
         CLI   NDMODE,NDPROC       TEST FOR PROCESS BEFORE I/O                  
         BNE   COPY10                                                           
*                                                                               
* BEFORE I/O PROCESSING                                                         
*                                                                               
         L     R4,NDIOA                                                         
         ST    R4,AIO              MAKE SURE AIO=NDIOA                          
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCPLN     TEST FOR PLAN RECORD                         
         BZ    COPY8                                                            
*                                                                               
         MVI   ELCODE,BUPLNELQ                                                  
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING BUPLND,R6                                                        
         OC    TOPLNNAM,TOPLNNAM   TEST FOR NEW PLAN NAME                       
         BZ    *+10                                                             
         MVC   BUPLNNAM,TOPLNNAM   SET NEW PLAN NAME                            
         MVC   PLANNAM,BUPLNNAM    GET PLAN NAME FOR PRINTING                   
*                                                                               
         OC    TOPLNST,TOPLNST     TEST FOR NEW PERIOD                          
         BZ    COPY7               NO                                           
         MVC   BUPLNST,TOPLNST                                                  
         MVC   BUPLNEND,TOPLNEND                                                
*                                                                               
COPY7    NI    BUPLNIND,X'FF'-BUPLNDAT TURN OFF DATA RECORD BIT                 
         GOTO1 ADDELEM                                                          
         MVI   ELCODE,BUSNELQ      DELETE ANY SNAPSHOT ELEMENT                  
         GOTO1 REMELEM                                                          
         CLC   FRCLT,TOCLT         TEST FROM=TO CLIENT                          
         BE    COPYX               YES                                          
         MVI   ELCODE,BUFOVELQ     DELETE FISCAL YEAR OVERRIDES                 
         GOTO1 REMELEM                                                          
         B     COPYX                                                            
*                                                                               
COPY8    TM    BURCTYP,BUKCOUT     TEST FOR OUTLINE RECORD                      
         BZ    COPYX               NO                                           
*                                                                               
         MVI   ELCODE,BUPTRELQ     CHANGE POINTER ELEMENT                       
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING BUPTRD,R6                                                        
         LA    R1,BUPOINT                                                       
         USING BUCRECD,R1                                                       
         MVC   BUCCLT,TOCLT        SET NEW CLIENT/PRODUCT/PLAN                  
         MVC   BUCPRD,TOPRD        IN PASSIVE POINTER                           
         MVC   BUCPLAN,TOPLAN                                                   
         GOTO1 ADDELEM                                                          
         B     COPYX                                                            
         DROP  R1,R6                                                            
*                                                                               
* AFTER I/O PROCESSING - ADD OUTLINE PASSIVE POINTERS                           
*                                                                               
COPY10   CLI   NDMODE,NDLOOK       TEST FOR LOOK AFTER I/O                      
         BNE   COPYX                                                            
         L     R4,NDIOA                                                         
         TM    BURCTYP,BUKCOUT     TEST FOR OUTLINE                             
         BZ    COPYX                                                            
*                                                                               
         L     R3,NDLEVPTR         R3=LEVEL TABLE POINTER                       
         USING NDLVTABD,R3                                                      
         MVI   ELCODE,BUPTRELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPTRD,R6                                                        
         LA    RE,PASSKEY                                                       
         USING BUCRECD,RE                                                       
         MVC   BUCKEY,BUPOINT      SET KEY                                      
         MVC   BUCCTL,BURCTL                                                    
         MVC   BUCDA,NDLVDA        SET DISK ADDRESS FROM NODIO                  
         DROP  RE                                                               
*                                                                               
COPY12   XC    KEY,KEY             ADD PASSIVE POINTER                          
         MVC   KEY(L'PASSKEY),PASSKEY                                           
         GOTO1 ADD                                                              
*                                                                               
COPY15   B     COPYX                                                            
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY CLIENT NAME AND FISCAL YEAR ON SCREEN                  
*                                                                               
* AT ENTRY, R1 = 0 DISPLAY 'FROM' CLIENT                                        
*           R1 = 1 DISPLAY 'TO' CLIENT                                          
*                                                                               
* ROUTINE USES WORK AND ASSUMES CLTVALS HAS JUST BEEN SET                       
*                                                                               
CLTOUT   NTR1                                                                   
         STC   R1,BYTE             SAVE PARAMETER                               
         MVC   WORK(L'CLTNAM),CLTNAM                                            
         LA    R2,WORK+L'CLTNAM+1                                               
         MVC   DUB+1(2),CLTSTART                                                
         MVI   DUB,80                                                           
         GOTO1 DATCON,DMCB,(3,DUB),(7,0(R2))                                    
         LA    R2,6(R2)                                                         
*                                                                               
         LA    RE,DAYTAB                                                        
         LA    R0,DAYS                                                          
CLTOUT1  CLC   CLTDAY,0(RE)                                                     
         BE    CLTOUT2                                                          
         LA    RE,L'DAYTAB(RE)                                                  
         BCT   R0,CLTOUT1                                                       
         DC    H'0'                                                             
*                                                                               
CLTOUT2  MVC   0(3,R2),1(RE)       EXTRACT DAY OUTPUT FROM TABLE                
         LA    R2,4(R2)                                                         
*                                                                               
         LA    R0,TYPES                                                         
         LA    RE,TYPTAB                                                        
CLTOUT3  CLC   CLTTYPE,0(RE)       MATCH ON TYPE NUMBER                         
         BE    CLTOUT4                                                          
         LA    RE,L'TYPTAB(RE)                                                  
         BCT   R0,CLTOUT3                                                       
         DC    H'0'                                                             
*                                                                               
CLTOUT4  MVC   0(5,R2),1(RE)       SET TYPE NAME                                
         LA    R3,PCYFCLN          R3=A(SCREEN FIELD)                           
         LA    R1,L'PCYFCLN        R1=L'SCREEN FIELD                            
         CLI   BYTE,0              TEST FOR DISPLAYING FROM CLIENT              
         BE    *+12                YES                                          
         LA    R3,PCYTCLN                                                       
         LA    R1,L'PCYTCLN                                                     
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO CLEAR PROTECTED NAME FIELDS AT START OF VALKEY                 
*                                                                               
CLRNAME  ST    RE,SAVERE                                                        
         LA    RE,NAMETAB          RE=TABLE POINTER                             
         LA    R0,NAMES            R0=COUNTER                                   
*                                                                               
CLRNAME2 SR    R2,R2                                                            
         ICM   R2,3,0(RE)          RE=FIELD HEADER DISPLACEMENT                 
         A     R2,ATWA             R2=A(FIELD HEADER)                           
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'8'                                                         
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         BCTR  R1,0                FOR EXECUTE                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
CLRNAME4 LA    RE,L'NAMETAB(RE)                                                 
         BCT   R0,CLRNAME2                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GENERATE A REPORT FOR NEW PLAN                                 
*                                                                               
REP      NTR1                                                                   
         XC    NDHOOK,NDHOOK                                                    
         MVC   NODKEY,TOPLNKEY                                                  
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    REP2                                                             
         DC    H'0'                                                             
*                                                                               
REP2     LA    R1,PRTOUT                                                        
         ST    R1,NDHOOK                                                        
         MVI   NDSQBACK,3                                                       
         GOTO1 VNODIO,DMCB,NODBLKD,=C'LSEQ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    REPX                                                             
         DC    H'0'                                                             
*                                                                               
REPX     B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PRINT A LINE FOR EACH OUTLINE                                  
*                                                                               
PRTOUT   NTR1                                                                   
         CLI   NDMODE,NDPROC                                                    
         BNE   PRTOUTX                                                          
         L     R4,NDIOA                                                         
         TM    BURCTYP,BUKCOUT     TEST FOR OUTLINE                             
         BZ    PRTOUTX                                                          
*                                                                               
         GOTO1 VGETVAL                                                          
*                                                                               
         LA    R3,P                R3=A(PRINT LINE)                             
         USING PLIND,R3                                                         
         CLI   OUTLEV,1            TEST FOR FIRST LEVEL OUTLINE                 
         BNE   PRTOUT2                                                          
         GOTO1 SPOOL,PARAS,(R8)    SKIP A LINE BEFORE PRINTING                  
*                                                                               
PRTOUT2  ZIC   R1,OUTLEV           GET OUTLINE LEVEL                            
         BCTR  R1,0                DEVELOP INDEX INTO PRINT FIELD               
         SLL   R1,1                X 2 TO INDENT FOR LEVEL                      
         LA    RE,POUTCODE(R1)                                                  
         MVC   0(L'OUTCODE,RE),OUTCODE EXTRACT OUTLINE CODE                     
         OC    0(L'OUTCODE,RE),SPACES SPACE FILL FIELD                          
         LA    RE,POUTNAME(R1)      INDEX INTO DESCRIPTION                      
         MVC   0(L'OUTNAME,RE),OUTNAME                                          
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
PRTOUTX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* HEADLINE PRINTING ROUTINE HOOK                                                
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+10(3),CLTCODE    SHOW TO CLIENT CODE/NAME                     
         OC    H4+10(3),SPACES                                                  
         MVC   H4+15(L'CLTNAM),CLTNAM                                           
*                                                                               
         MVC   H5+10(3),PRDCODE                                                 
         OC    H5+10(3),SPACES                                                  
         MVC   H5+15(L'PRDNAM),PRDNAM                                           
*                                                                               
         MVC   H6+10(3),PLANCODE                                                
         OC    H6+10(3),SPACES                                                  
         MVC   H6+15(L'PLANNAM),PLANNAM                                         
*                                                                               
         ICM   R3,15,ABOX                                                       
         BZ    HOOKX                                                            
         USING BOXD,R3                                                          
         MVI   BOXROWS+7,C'T'      SET UP FOR BOXES                             
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   29(R2),C'L'                                                      
         MVI   46(R2),C'C'                                                      
         MVI   73(R2),C'R'                                                      
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,YES                                                      
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* TABLE OF DISPLACEMENTS OF PROTECTED 'NAME' FIELDS                             
*                                                                               
NAMETAB  DS    0XL2                                                             
         DC    AL2(PCYFCLNH-T502FFD)                                            
         DC    AL2(PCYFPRNH-T502FFD)                                            
         DC    AL2(PCYFPLNH-T502FFD)                                            
         DC    AL2(PCYTCLNH-T502FFD)                                            
         DC    AL2(PCYTPRNH-T502FFD)                                            
NAMES    EQU   (*-NAMETAB)/L'NAMETAB                                            
         SPACE 2                                                                
* TABLE OF DAY BITS AND THEIR CORRESPONDING OUTPUT                              
*                                                                               
DAYTAB   DS    0CL4                                                             
         DC    X'40',CL3'MON'                                                   
         DC    X'20',CL3'TUE'                                                   
         DC    X'10',CL3'WED'                                                   
         DC    X'08',CL3'THU'                                                   
         DC    X'04',CL3'FRI'                                                   
         DC    X'02',CL3'SAT'                                                   
         DC    X'01',CL3'SUN'                                                   
DAYS     EQU   (*-DAYTAB)/L'DAYTAB                                              
         SPACE 2                                                                
* TABLE OF MOBILE TYPES AND THEIR EXPANDED VALUES                               
*                                                                               
TYPTAB   DS    0CL6                                                             
         DC    AL1(00),CL5'BROAD'                                               
         DC    AL1(02),CL5'CALEN'                                               
         DC    AL1(06),CL5'544'                                                 
         DC    AL1(07),CL5'454'                                                 
         DC    AL1(08),CL5'445'                                                 
         DC    AL1(10),CL5'444'                                                 
TYPES    EQU   (*-TYPTAB)/L'TYPTAB                                              
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
YESPARM  DC    C'Y'                                                             
COPYMSG  DC    C'PLAN COPIED'                                                   
BIGMSG   DC    C'* PLAN HAS TOO MANY OUTLINES TO COPY ON-LINE *'                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* REPORT SPEC POOL                                                              
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,47,C'PLAN COPY REPORT'                                        
         SSPEC H2,47,C'----------------'                                        
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'PLAN'                                                     
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,100,PAGE                                                      
         SSPEC H9,31,C'CODE'                                                    
         SSPEC H9,48,C'DESCRIPTION'                                             
         DC    X'00'                                                            
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER PLAN COPY SCREEN                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILC4D                                                       
         EJECT                                                                  
* WORKING STORAGE VALUES                                                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
FRCNTLS  DS    0C                  FROM CONTROL FIELDS                          
FRCLT    DS    CL(L'CLTCODE)       FROM CLIENT                                  
FRPRD    DS    CL(L'PRDCODE)       FROM PRODUCT                                 
FRPLAN   DS    CL(L'PLANCODE)      FROM PLAN                                    
FRCNTLN  EQU   *-FRCNTLS           CONTROL FIELDS LENGTH                        
*                                                                               
TOCNTLS  DS    0CL(FRCNTLN)                                                     
TOCLT    DS    CL(L'CLTCODE)       TO CLIENT                                    
TOPRD    DS    CL(L'PRDCODE)       TO PRODUCT                                   
TOPLAN   DS    CL(L'PLANCODE)      TO PLAN                                      
*                                                                               
FRFISCAL DS    CL(L'CLTFIS)        FROM CLIENT FISCAL YEAR                      
TOFISCAL DS    CL(L'CLTFIS)        TO CLIENT FISCAL YEAR                        
*                                                                               
TOPLNNAM DS    CL(L'PLANNAM)       TO PLAN NAME                                 
TOPLNST  DS    XL2                 TO PLAN START PERIOD                         
TOPLNEND DS    XL2                 TO PLAN END PERIOD                           
*                                                                               
BEFCODE  DS    CL(L'BUKCODE)       POSITIONING CODE FOR TO PLAN                 
*                                                                               
FRPLNKEY DS    CL(L'NODKEY)        FROM PLAN NODAL KEY                          
TOPLNKEY DS    CL(L'NODKEY)        TO PLAN NODAL KEY                            
*                                                                               
PASSKEY  DS    CL(BUKLNQ)          PASSIVE POINTER AREA                         
*                                                                               
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PLIND    DSECT                                                                  
         DS    CL30                                                             
POUTCODE DS    CL14                OUTLINE CODE                                 
         DS    CL3                                                              
POUTNAME DS    CL26                OUTLINE NAME                                 
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXPLAN  EQU   100                 MAXIMUM PLAN SIZE FOR COPY                   
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008BUFIL24   05/01/02'                                      
         END                                                                    
