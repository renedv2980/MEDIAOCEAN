*          DATA SET SPTRA60    AT LEVEL 037 AS OF 02/27/15                      
*PHASE T21660C                                                                  
*INCLUDE SORTER                                                                 
*        TITLE 'T21660 WARNER LAMBERT PATTERN BOOKLET LIST'                     
*---------------------------------------------------------------------*         
*>>>>>>> NOTE THIS PROGRAM DOES NOT WORK WELL W/SVT2PR04=N   <<<<<<<<<*         
*>>>>>>> PER RAMUNE (WILL FIX LATER IF CLIENTS ASK FOR IT)   <<<<<<<<<*         
*---------------------------------------------------------------------*         
*                                                                               
* THIS PROGRAM ONLY RUNS OVERNITE - USES SORTER AND UPDATES PATTERNS            
*                                                                               
***********************************************************************         
* OPTIONS WILL BE - REVISIONS                                                   
* ALL HIATUS PATTERNS ARE BYPASSED - NOT AS OF LEV 29                           
* ALL COPY CODE = EST PATTERNS ARE BYPASSED                                     
***********************************************************************         
*                                                                               
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - PATTERN RECORD IN PRINT RTN                                
*                                                                               
*             AIO3 - MARKET NAMES                                               
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE REG                                                   
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
* LEV 17 MAR11/91 FIX BUG - DOESN'T PRINT PTR IF PRD NOT EQV          *         
* LEV 18 NOV19/92 ADD CODE FOR MARKET GROUP AND COMBINED PATTERN      *         
* LEV 19 DEC22/92 FIX FOR STATION FILE                                *         
* LEV 20 JAN20/93 ADD CODE FOR 3 LINE COMML DESC/SPOT LEN OVERRIDE    *         
* LEV 21 MAR10/93 ADD CODE CABLE HEAD STATIONS                        *         
* LEV 22 MAR22/93 FIX INVERT COMML CODE BUG, FIX ALLOWLIN             *         
* LEV 23 JUN21/93 FIX BAD CHECK FOR ACTIVE STATUS                     *         
* LEV 24 JAN21/94 15 COMMLS                                           *         
* LEV 25 JUL19/94 PRINT PERCENT ROTATIONS IF T2 PR 04 SET             *         
* LEV 26 JUL21/94 CHANGE TO FILENAME                                  *         
* LEV 27 SEP15/94 FIX TO PERCENTS                                     *         
* LEV 28 DEC01/94 SHOW ??? FOR DELETED PRODUCTS                       *         
* LEV 29 SMUR 5/23/97 SHOW HIATUS PATTERNS                            *         
* LEV 30 BGRI 7/17/98 ADD TELECASTER                                  *         
* LEV 31 SMUR 4/11/01 USE TRAFFIC OFFICE                              *         
* LEV 32 BGRI MAY07/01 CHANGE DUMMY                                   *         
* LEV 33 SMUR JAN06/08 PRINT NO COMML FOR INCOMPLETE PAT REC          *         
* LEV 36 SMUR OCT07/09 AD-ID SUPPORT                                  *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21660 WARNER LAMBERT PATTERN BOOKLET LIST'                     
T21660   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21660**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPRELOC                                                       
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE 2                                                                
VK       LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
VK10     LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT(6),BCLT        CLEAR BCLT, BPRD, BSLN, BPRD2, BSLN2         
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         GOTO1 VALICLT                                                          
*                                                                               
* READ T0,T1 PROFILES *                                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
         MVI   WORK+3,C'2'                                                      
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT2PR04,ELEM+3                                                  
*                                                                               
VK20     LA    R2,TRAPRDH          PRODUCT                                      
         XC    BPRD(2),BPRD        CLEAR IF NONE                                
         XC    QPRD,QPRD                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK30                                                             
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK                                                     
         BE    VK30                                                             
         MVC   QPRD,WORK                                                        
         MVC   BPRD(2),WORK+3      GET BIN PROD AND SPOT LENGTH                 
*                                                                               
VK30     LA    R2,TRACONH          CONTACT NAME                                 
         XC    QUESTOR,QUESTOR                                                  
         XC    CONTEL,CONTEL                                                    
         GOTO1 ANY                                                              
         MVC   QUESTOR(L'TRACON),TRACON                                         
         CLC   =C'CON=',WORK       THIS AGY CONTACT KEY                         
         BNE   VK40                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CNTKEY,R4                                                        
         MVC   CNTKID,=X'0A36'                                                  
         MVC   CNTKAM(3),BAGYMD                                                 
         MVC   CNTKNAME,12(R2)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VK34                                                             
         MVC   KEY,KEYSAVE                                                      
         XC    CNTKCLT,CNTKCLT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VK34                                                             
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
VK34     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CNTDTAEL,R6                                                      
         MVC   QUESTOR,CNTNAME                                                  
         MVC   CONTEL,CNTTEL                                                    
         DROP  R4,R6                                                            
*                                                                               
VK40     LA    R2,TRAPERH          PERIOD                                       
         GOTO1 =A(VPER),RR=SPRELOC                                              
*                                                                               
         LA    R2,TRAOPTH         OPTIONS                                       
         BAS   RE,VOPT                                                          
*                                                                               
* NOW CHECK FOR TEXT FOR 'LETTER' TO PRECEDE LIST *                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(1),SVPROF12                                                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE    TEST ALL MKT/STA STEXT FOUND                  
         BNE   MISTXTER             NO                                          
*                                                                               
* NOW BUILD KEY                                                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM(7),BAGYMD                                                 
*        MVC   PATKCLT,BCLT                                                     
*        MVC   PATKPRD(2),BPRD     PRODUCT AND SPOT LEN                         
*        MVC   PATKPRD2(2),BPRD2   PARTNER PRODUCT AND SPOT LEN                 
         MVC   PATKREF,BREFSUB                                                  
*                                                                               
* SET UP COMPARE KEY TO CONTROL END OF LIST                                     
*                                                                               
         MVC   COMPKEY,KEY                                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* OFFLINE REPORT ROUTINE *                                                      
         SPACE 2                                                                
LR       MVC   AIO,AIO1                                                         
         CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LM    RE,RF,=A(HEADING,HDHK) SSPECS AND HEADHOOK RTN                   
         ST    RE,SPECS                                                         
         ST    RF,HEADHOOK                                                      
*                                                                               
         L     RE,VADUMMY         FOR SAVED PRODUCT NAMES                       
         LH    RF,=H'5720'         26*220                                       
         LA    R0,0(RE,RF)                                                      
         ST    R0,ASVCMLS                                                       
         XCEF                                                                   
*                                                                               
         L     RE,ASVCMLS                                                       
         L     RF,SVCMLEN                                                       
         LA    R0,0(RE,RF)         START OF CMML/PATTERN EXPLODE TABLES         
         ST    R0,ACMLEXP                                                       
         AH    R0,=H'5000'                                                      
         ST    R0,AENDEXP                                                       
         XCEF                                                                   
*                                                                               
         BAS   RE,RDNAMES          BUILD TABLE IN DUMMY                         
*                                                                               
         GOTO1 =V(SORTER),DMCB,A(SORTCARD),A(RECCARD)                           
*                                                                               
* BUILD KEY, AND DO READHI                                                      
*                                                                               
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID(2),=X'0A22'                                               
         MVC   PATKAM(7),BAGYMD BAGYMD AND ALL BELOW                            
*        MVC   PATKCLT(6),BCLT     CLIENT                                       
*        MVC   PATKPRD,BPRD        PRODUCT                                      
*        MVC   PATKSLN,BSLN        COMMERCIAL LENGTH                            
*        MVC   PATKPRD2,BPRD2      PARTNER PRODUCT                              
*        MVC   PATKSLN2,BSLN2      PARTNER COMMERCIAL LENGTH                    
*                                                                               
LR10     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEY(5),KEYSAVE      AT END OF CLIENT                             
         BNE   LRR                  YES, ALL DONE                               
         CLI   COMPKEY+5,0        PROD ENTERED                                  
         BE    *+14               NO                                            
         CLC   KEY+5(1),COMPKEY+5                                               
         BNE   LR24                                                             
         CLI   COMPKEY+6,0        SLN ENTERED                                   
         BE    *+14               NO                                            
         CLC   KEY+6(1),COMPKEY+6                                               
         BNE   LR24                                                             
         CLI   COMPKEY+7,0        PROD2 ENTERED                                 
         BE    *+14               NO                                            
         CLC   KEY+7(1),COMPKEY+7                                               
         BNE   LR24                                                             
         CLI   COMPKEY+8,0        SLN2 ENTERED                                  
         BE    *+14               NO                                            
         CLC   KEY+8(1),COMPKEY+8                                               
         BNE   LR24                                                             
         CLI   COMPKEY+9,0        COPY CODE ENTERED                             
         BE    *+14               NO                                            
         CLC   KEY+9(1),COMPKEY+9                                               
         BNE   LR24                                                             
         B     LR30                                                             
*                                                                               
LR24     BAS   RE,NXK             BUMP KEY                                      
         B     LR10               AND GET NEXT                                  
*                                                                               
LR30     MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,NXK             SET UP FOR NEXT KEY                           
*                                                                               
         L     R4,AIO                                                           
         BAS   RE,FTR                                                           
         BNE   LR10                                                             
*                                                                               
* FORMAT RECORDS FOR SORT *                                                     
*                                                                               
         XC    SORTREC,SORTREC                                                  
         MVC   BYTE,PATKPRD                                                     
         BRAS  RE,FPRD             GET 3 CHAR PRODUCT CODE                      
         MVC   SRTPROD,0(R1)                                                    
         MVC   SRTSLN,PATKSLN                                                   
         MVI   SRTPB,1             INDICATE NO PIGGYBACK PROD                   
         CLI   PATKPRD2,0                                                       
         BE    LR40                                                             
         MVC   BYTE,PATKPRD2                                                    
         BRAS  RE,FPRD             IF NO PRD2                                   
         MVC   SRTPROD2,0(R1)                                                   
         MVC   SRTSLN2,PATKSLN2                                                 
         MVI   SRTPB,0             SET AS PIGGY - SORT AHEAD OF SINGLE          
*                                                                               
LR40     MVC   SRTDAYPT,PATKCODE                                                
         MVI   ELCODE,X'10'                                                     
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         USING PATDTAEL,R6                                                      
         TM    PATSTAT1,PATSADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         CLI   PATKPRD2,0          ANY P/B PROD                                 
         BE    LR44                 NO                                          
         TM    PATSTAT,X'04'       INVERT PRODS                                 
         BZ    LR44                                                             
         OC    SRTPROD2,SRTPROD2                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(3),SRTPROD                                                  
         MVC   SRTPROD,SRTPROD2                                                 
         MVC   SRTPROD2,WORK                                                    
         MVC   SRTSLN,SRTSLN2                                                   
         MVC   SRTSLN2,PATKSLN                                                  
         DROP  R4                                                               
*                                                                               
LR44     CLC   PERSTART,PATEND                                                  
         BH    LR10                                                             
         CLC   PEREND,PATSTART                                                  
         BL    LR10                                                             
         CLC   PERSTART,PATSTART                                                
         BNH   *+10                                                             
         MVC   PATSTART,PERSTART                                                
*                                                                               
         CLC   PEREND,PATEND                                                    
         BNL   *+10                                                             
         MVC   PATEND,PEREND                                                    
*                                                                               
         MVC   SRTDTS,PATSTART                                                  
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATLSTEL,R6                                                      
         CLC   PATLSTTY(6),=X'D40000000000'  ALL MARKETS                        
         BNE   *+12                                                             
         MVI   SRTORD,X'10'                                                     
         B     LR50                                                             
         CLI   PATLSTTY,C'T'       STATION TYPE                                 
         BNE   *+12                                                             
         MVI   SRTORD,X'20'                                                     
         B     LR50                                                             
         CLI   PATLSTTY,C'A'       STATION AFFILIATE                            
         BNE   *+12                                                             
         MVI   SRTORD,X'30'                                                     
         B     LR50                                                             
         CLI   PATLSTTY,C'G'       MARKET GROUP                                 
         BNE   *+12                                                             
         MVI   SRTORD,X'40'                                                     
         B     LR50                                                             
         CLI   PATLSTTY,C'C'       COMBINED MARKET/AFFILIATE LIST               
         BNE   *+12                                                             
         MVI   SRTORD,X'50'                                                     
         B     LR50                                                             
         CLI   PATLSTTY,C'M'       MARKET SPECIFIC                              
         BNE   *+12                                                             
         MVI   SRTORD,X'60'                                                     
         B     LR50                                                             
         CLI   PATLSTTY,C'S'       STATION SPECIFIC                             
         BNE   *+12                                                             
         MVI   SRTORD,X'70'                                                     
         B     LR50                                                             
         DC    H'0'                                                             
LR50     MVC   SRTDKAD,KEY+14                                                   
         L     R1,PATLSTCT                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,PATLSTCT                                                      
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         B     LR10                                                             
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE                                                    
*                                                                               
* PRINT STEXT HERE, THEN MIDLINES *                                             
*                                                                               
LRR      XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(1),SVPROF12                                                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE    TEST ALL MKT/STA STEXT FOUND                  
         BNE   PRT120               NO                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRT100   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PRT104   BAS   RE,NEXTEL                                                        
         BNE   PRT110                                                           
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'4'                                                         
         EX    RE,PRT100EX                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRT104                                                           
PRT100EX MVC   P+3(0),3(R6)                                                     
*                                                                               
PRT110   MVI   ELCODE,X'50'        TEST FOR CONTINUATION ELEM                   
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   PRT120                                                           
         ZIC   R1,KEY+12           BUMP TYP (PAGE NUMBER)                       
         LA    R1,1(,R1)                                                        
         STC   R1,KEY+12                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRT100                                                           
PRT120   MVI   FORCEHED,C'Y'                                                    
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    NOSRTER                                                          
         MVC   SORTREC,0(R1)                                                    
         MVC   SVPB,SRTPB                                                       
         MVC   SVPROD,SRTPROD                                                   
         MVC   SVCOMP,SRTPROD      CHANGE IN PROD/PROD2/SLN/SLN2                
         MVI   HDHKSW,C'N'                                                      
         B     LRR004                                                           
         EJECT                                                                  
LRR000   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    LRREND                                                           
         MVC   SORTREC,0(R1)                                                    
*                                                                               
         CLI   HDHKSW,C'Y'         JUST PRINTED HEADING?                        
         BE    LRR004               YES                                         
*                                                                               
         MVC   P1+13(L'MARK1),MARK1                                             
*                                                                               
         CLC   SRTPROD,SVPROD                                                   
         BE    *+14                                                             
         MVC   P1+2(L'MARK3),MARK3                                              
         B     LRR002                                                           
*                                                                               
         CLC   SVCOMP,SRTPROD      CHANGE IN PROD/PROD2/SLN/SLN2                
         BE    LRR002                                                           
         MVC   P1+2(L'MARK2),MARK2                                              
*                                                                               
LRR002   MVI   SPACING,3                                                        
         BAS   RE,PRT              GO PRINT LINE(S)                             
*                                                                               
LRR004   CLC   SRTPB,SVPB          FORCE PAGE BRK AT END OF PIGGYBSCKS          
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   SVPB,SRTPB                                                       
*                                                                               
         MVC   SVCOMP,SRTPROD                                                   
         MVC   KEY+14(4),SRTDKAD                                                
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         LR    R4,R6                                                            
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         USING PATDTAEL,R6                                                      
         TM    PATSTAT1,PATSADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         LA    R3,SRTPROD                                                       
         MVC   PPROD,SRTPROD                                                    
         BAS   RE,FPRDNM           OUTPUT WILL BE IN FLD                        
         MVC   PPRODNM,PRDNM                                                    
*                                                                               
         CLI   SRTPROD2,0                                                       
         BE    LRR010                                                           
*                                                                               
         LA    R1,PPROD+2                                                       
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'/'                                                       
*                                                                               
         LA    R1,PPRODNM+19                                                    
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'/'                                                       
*                                                                               
LRR010   MVI   SVPATST,0                                                        
*                                                                               
         CLI   PATDTALN,38         OLD PAT LEN                                  
         BE    LRR014                                                           
         MVC   SVPATST,PATSTAT                                                  
         TM    PATSTAT,X'02'       ALREADY PRINTED                              
         BO    LRR014                                                           
         OI    PATSTAT,X'02'       SET ON PRINTED                               
         TM    FLAGFTR,FTRTEST     DON'T MARK FOR TEST                          
         BO    LRR014                                                           
         GOTO1 PUTREC                                                           
*                                                                               
LRR014   CLC   PERSTART,PATSTART                                                
         BNH   *+10                                                             
         MVC   PATSTART,PERSTART                                                
         CLC   PEREND,PATEND                                                    
         BNL   *+10                                                             
         MVC   PATEND,PEREND                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,PATSTART),(5,PPERIOD)                             
         MVI   PPERIOD+8,C'-'                                                   
*                                                                               
         GOTO1 (RF),(R1),(3,PATEND),(5,PPERIOD+9)                               
         EJECT                                                                  
         LA    RF,PSLN+2                                                        
*                                                                               
         CLI   SRTSLN2,0          IF PIGGYBACK                                  
         BE    LRR016               NO                                          
         BCTR  RF,0                                                             
*                                                                               
LRR016   CLI   SRTSLN,99           IF 3 DIGITS                                  
         BNH   LRR018                                                           
         BCTR  RF,0                START 1 POS LEFT                             
         B     LRR020                                                           
*                                                                               
LRR018   CLI   SRTSLN2,99          IF 3 DIGITS                                  
         BNH   LRR020                                                           
         BCTR  RF,0                START 1 POS LEFT                             
*                                                                               
LRR020   ZIC   R0,SRTSLN           EDIT SPOT LENGTH                             
         BAS   RE,EDITSLN                                                       
*                                                                               
         CLI   SRTSLN2,0           IS THERE A PIGGYBACK                         
         BE    LRR024                                                           
*                                                                               
         MVI   0(RF),C'/'                                                       
         LA    RF,1(,RF)                                                        
*                                                                               
         ZIC   R0,SRTSLN2                                                       
         BAS   RE,EDITSLN                                                       
*                                                                               
LRR024   MVC   SVPSLN,PSLN                                                      
         MVI   SVPSLNF,0           SET OFF PRINTED OVERRIDE - 1ST LINE          
*                                                                               
* NOW TABLE ALL COMMERCIALS AND ROTATION *                                      
*                                                                               
* COMMERCIAL ELEMENT *                                                          
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BAS   RE,NEXTEL                                                        
         BE    LRR024C                                                          
                                                                                
         L     R2,ACMLEXP                                                       
         USING EXPCTABD,R2                                                      
         MVC   EXPCENT,SPACES                                                   
         MVC   EXPCML(13),=C'**NO COMMLS**'                                     
         B     LRR040                                                           
         DROP  R2                                                               
                                                                                
         USING PATCMLEL,R6                                                      
LRR024C  LA    R3,PATCML                                                        
         LR    R5,R3                                                            
         ZIC   R4,PATCMLLN                                                      
         SRL   R4,4                DIV BY 16 TO GET ENTRIES (DROPS ODD)         
*                                                                               
         TM    SVPATST,X'04'       INVERT PRODS/COMMLS                          
         BZ    LRR026                                                           
         LR    RF,R3                                                            
         LR    R1,R4                                                            
*                                                                               
LRR025   OC    8(8,RF),8(RF)                                                    
         BZ    LRR025C                                                          
         MVC   WORK(8),0(RF)                                                    
         MVC   0(8,RF),8(RF)                                                    
         MVC   8(8,RF),WORK                                                     
LRR025C  LA    RF,16(,RF)                                                       
         BCT   R1,LRR025                                                        
*                                                                               
LRR026   L     R2,ACMLEXP                                                       
         USING EXPCTABD,R2                                                      
         MVC   EXPCENT,SPACES                                                   
         DROP  R6                                                               
*                                                                               
LRR030   CLC   0(8,R3),=XL8'5C00000000000000' DELETED COMML                     
         BE    LRR066                                                           
         CLC   =X'C8C9C1E3E4E2',0(R3) HIATUS                                    
         BNE   *+14                                                             
         MVC   EXPCML(6),=C'HIATUS'                                             
         B     LRR040                                                           
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING CMLTBLD,R6                                                       
         MVC   CMLTCML,0(R3)                                                    
         BRAS  RE,FCML             GO FIND COMML TITLE                          
*                                                                               
         MVI   EXPCFLG,C'0'                                                     
         MVC   EXPCML(8),CMLTCML                                                
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   LRR031                                                           
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'U',CMLTCML),EXPCML                               
                                                                                
LRR031   OC    8(8,R3),8(R3)       P/B COMML                                    
         BZ    LRR032                                                           
         CLC   0(8,R3),8(R3)       SAME COMML                                   
         BE    LRR032                                                           
         LA    RF,EXPCML+8                                                      
         CLI   0(RF),X'40'         SPACE                                        
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         MVI   0(RF),C'-'                                                       
*                                                                               
LRR032   MVC   EXPCML+15(15),CMLTDSC1                                           
*                                                                               
         CLI   CMLTOV1,0           IS THERE AN OVERRIDE PRTBLE SPOT LEN         
         BE    LRR038               NO                                          
*                                                                               
         MVC   EXPCSLN,SPACES                                                   
         ZIC   R0,CMLTOV1                                                       
         LA    RF,EXPCSLN+1                                                     
*                                                                               
         CLI   CMLTOV1,99          IF 3 DIGITS                                  
         BNH   LRR034                                                           
         BCTR  RF,0                START 1 POS LEFT                             
         B     LRR036                                                           
*                                                                               
LRR034   CLI   CMLTOV2,99          IF 3 DIGITS                                  
         BNH   LRR036                                                           
         BCTR  RF,0                START 1 POS LEFT                             
*                                                                               
LRR036   BAS   RE,EDITSLN                                                       
         MVI   0(RF),C'/'                                                       
         LA    RF,1(,RF)                                                        
         ZIC   R0,CMLTOV2                                                       
         BAS   RE,EDITSLN                                                       
         MVI   SVPSLNF,1           SET ON PRINTED OVERRIDE SPOT LENGTHS         
*                                                                               
         B     LRR040                                                           
*                                                                               
LRR038   CLI   SVPSLNF,1           WAS OVERRIDE PRINTED                         
         BNE   LRR040                                                           
         MVC   EXPCSLN,SVPSLN                                                   
         MVI   SVPSLNF,0           SET OFF PRINTED OVERRIDE SPOT LEN            
*                                                                               
LRR040   LA    R2,EXPCNEXT                                                      
         C     R2,AENDEXP                                                       
         BH    EXPSIZER                                                         
*                                                                               
* GO BACK TO PREV ENTRY AND SEE IF HIATUS                                       
*                                                                               
         LR    RF,R2                                                            
         LA    RE,L'EXPCENT                                                     
         SR    RF,RE               RF = A(PREV ENTRY)                           
         CLC   =X'C8C9C1E3E4E2',0(RF) HIATUS                                    
         BE    LRR066                                                           
         CLC   =C'**NO COMMLS**',0(RF) NO CML ON PAT REC                        
         BE    LRR066C                                                          
*                                                                               
         MVC   EXPCENT,SPACES                                                   
*                                                                               
         OC    CMLTDSC2,CMLTDSC2   SECOND LINE OF DESCRIPTION                   
         BZ    LRR044                                                           
         MVC   EXPCML+8(L'CMLTDSC2),CMLTDSC2                                    
         LA    R2,EXPCNEXT                                                      
         C     R2,AENDEXP                                                       
         BH    EXPSIZER                                                         
         MVC   EXPCENT,SPACES                                                   
*                                                                               
LRR044   OC    CMLTDSC3,CMLTDSC3   SECOND LINE OF DESCRIPTION                   
         BZ    LRR046                                                           
*                                                                               
         MVC   EXPCML+8(L'CMLTDSC3),CMLTDSC3                                    
         LA    R2,EXPCNEXT                                                      
         C     R2,AENDEXP                                                       
         BH    EXPSIZER                                                         
         MVC   EXPCENT,SPACES                                                   
*                                                                               
LRR046   OC    CMLTCLT,CMLTCLT            CLT COMML NO?                         
         BZ    LRR048                                                           
*                                                                               
         MVC   EXPCML+8(L'CMLTCLT),CMLTCLT                                      
         LA    R2,EXPCNEXT                                                      
         C     R2,AENDEXP                                                       
         BH    EXPSIZER                                                         
         MVC   EXPCENT,SPACES                                                   
*                                                                               
LRR048   OC    CMLTELCS,CMLTELCS          TELECASTER?                           
         BZ    LRR050                                                           
*                                                                               
         MVC   EXPCML+8(L'CMLTELCS),CMLTELCS                                    
         LA    R2,EXPCNEXT                                                      
         C     R2,AENDEXP                                                       
         BH    EXPSIZER                                                         
         MVC   EXPCENT,SPACES                                                   
         DROP  R6                                                               
*                                                                               
LRR050   OC    8(8,R3),8(R3)       P/B COMML                                    
         BZ    LRR066                                                           
         CLC   0(8,R3),8(R3)       SAME COMML                                   
         BE    LRR066                                                           
*                                                                               
         MVI   EXPCFLG,C'0'                                                     
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING CMLTBLD,R6                                                       
         MVC   CMLTCML,8(R3)                                                    
         BRAS  RE,FCML                                                          
         MVC   EXPCML+1(8),CMLTCML                                              
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   LRR051                                                           
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'U',CMLTCML),EXPCML+1                             
                                                                                
LRR051   MVC   EXPCML+15(L'CMLTDSC1),CMLTDSC1                                   
*                                                                               
         CLI   CMLTOV1,0           IS THERE AN OVERRIDE PRTBLE SPOT LEN         
         BE    LRR056               NO                                          
*                                                                               
         MVC   EXPCSLN,SPACES                                                   
         ZIC   R0,CMLTOV1                                                       
         LA    RF,EXPCSLN+1                                                     
*                                                                               
         CLI   CMLTOV1,99          IF 3 DIGITS                                  
         BNH   LRR052                                                           
         BCTR  RF,0                START 1 POS LEFT                             
         B     LRR054                                                           
*                                                                               
LRR052   CLI   CMLTOV2,99          IF 3 DIGITS                                  
         BNH   LRR054                                                           
         BCTR  RF,0                START 1 POS LEFT                             
*                                                                               
LRR054   BAS   RE,EDITSLN                                                       
         MVI   0(RF),C'/'                                                       
         LA    RF,1(,RF)                                                        
         ZIC   R0,CMLTOV2                                                       
         BAS   RE,EDITSLN                                                       
         MVI   SVPSLNF,1           SET ON PRINTED OVERRIDE SPOT LENGTHS         
         B     LRR058                                                           
*                                                                               
LRR056   CLI   SVPSLNF,1           WAS OVERRIDE PRINTED                         
         BNE   LRR058                                                           
         MVC   EXPCSLN,SVPSLN                                                   
         MVI   SVPSLNF,0           SET OFF PRINTED OVERRIDE SPOT LEN            
*                                                                               
LRR058   LA    R2,EXPCNEXT                                                      
         C     R2,AENDEXP                                                       
         BH    EXPSIZER                                                         
         MVC   EXPCENT,SPACES                                                   
         OC    CMLTDSC2,CMLTDSC2   SECOND LINE OF DESCRIPTION                   
         BZ    LRR060                                                           
         MVC   EXPCML+8(L'CMLTDSC2),CMLTDSC2                                    
         LA    R2,EXPCNEXT                                                      
         C     R2,AENDEXP                                                       
         BH    EXPSIZER                                                         
         MVC   EXPCENT,SPACES                                                   
*                                                                               
LRR060   OC    CMLTDSC3,CMLTDSC3   SECOND LINE OF DESCRIPTION                   
         BZ    LRR062                                                           
         MVC   EXPCML+8(L'CMLTDSC3),CMLTDSC3                                    
         LA    R2,EXPCNEXT                                                      
         C     R2,AENDEXP                                                       
         BH    EXPSIZER                                                         
         MVC   EXPCENT,SPACES                                                   
*                                                                               
LRR062   OC    CMLTCLT,CMLTCLT            CLT COMML NO?                         
         BZ    LRR064                                                           
         MVC   EXPCML+8(L'CMLTCLT),CMLTCLT                                      
         LA    R2,EXPCNEXT                                                      
         C     R2,AENDEXP                                                       
         BH    EXPSIZER                                                         
         MVC   EXPCENT,SPACES                                                   
*                                                                               
LRR064   OC    CMLTELCS,CMLTELCS          TELECASTER?                           
         BZ    LRR066                                                           
         MVC   EXPCML+8(L'CMLTELCS),CMLTELCS                                    
         LA    R2,EXPCNEXT                                                      
         C     R2,AENDEXP                                                       
         BH    EXPSIZER                                                         
         MVC   EXPCENT,SPACES                                                   
         DROP  R6                                                               
LRR066   LA    R3,16(,R3)                                                       
         BCT   R4,LRR030                                                        
*                                                                               
LRR066C  MVI   0(R2),0                                                          
         MVC   1(8,R2),=C'*EXPTTN*'                                             
         LA    R2,9(,R2)                                                        
         ST    R2,APTNEXP                                                       
*                                                                               
* GO BACK TO PREV ENTRY AND SEE IF HIATUS                                       
*                                                                               
         LR    RF,R2                                                            
         LA    RE,L'EXPCENT                                                     
         AH    RE,=H'9'            _*EXPTTN*                                    
         SR    RF,RE               RF = A(PREV ENTRY)                           
         CLC   =X'C8C9C1E3E4E2',0(RF) HIATUS                                    
         BE    LRR073                                                           
         CLC   =C'**NO COMMLS**',0(RF) NO CML ON PAT REC                        
         BE    LRR073                                                           
*                                                                               
* ROTATION ELEM                                                                 
*        R4 = ROTATION                                                          
*                                                                               
         CLI   SVT2PR04,C'Y'       PRINT PERCENT ROTATIONS                      
         BNE   LRR068                                                           
*                                                                               
         MVI   BYTE,1                                                           
         L     R6,AIO2                                                          
         MVI   ELCODE,X'36'                                                     
         BAS   RE,GETEL                                                         
         BE    LRR069                                                           
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'34'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR068                                                           
*                                                                               
         CLI   1(R6),5             ROTATION OF 1 LETTER                         
         BNE   LRR069               NO                                          
         MVC   WORK(9),=C'(100 PCT)'                                            
         MVI   BYTE,1                                                           
         LA    R4,1                                                             
         LA    R3,2(,R6)                                                        
         B     LRR071                                                           
*                                                                               
LRR068   L     R6,AIO2                                                          
         MVI   BYTE,0                                                           
         MVI   ELCODE,X'32'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SVT2PR04,C'Y'       PRINT PERCENTAGES IF AVAILABLE               
         BNE   LRR069                                                           
*                                                                               
         CLI   1(R6),3             ROTATION OF 1 LETTER                         
         BNE   LRR069               NO                                          
         MVC   WORK(9),=C'(100 PCT)'                                            
         MVI   BYTE,1                                                           
         LA    R4,1                                                             
         LA    R3,2(,R6)                                                        
         B     LRR071                                                           
*                                                                               
         USING PATPTNEL,R6                                                      
LRR069   LA    R3,PATPTN                                                        
         ZIC   R4,PATPTNLN                                                      
         BCTR  R4,0                SUB                                          
         BCTR  R4,0                    2                                        
*                                                                               
         CLI   BYTE,0              DOING PERCENTS                               
         BE    LRR070               NO                                          
         LR    R1,R4                                                            
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R4,R1                                                            
*                                                                               
LRR070   CLI   BYTE,0              DOING PERCENTS                               
         BE    LRR071               NO                                          
         SR    RE,RE                                                            
         ICM   RE,3,1(R3)                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   WORK,C'('                                                        
         UNPK  WORK+1(2),DUB+6(2)                                               
         MVI   WORK+3,C' '                                                      
         MVC   WORK+4(4),=C'PCT)'                                               
         MVI   WORK+8,C' '                                                      
*                                                                               
LRR071   XC    0(25,R2),0(R2)                                                   
         ZIC   RE,0(R3)            GET COMMERCIAL LETTER                        
         SH    RE,=H'193'          CONVERT A (=193) TO 0                        
         CH    RE,=H'10'                                                        
         BL    *+8                                                              
         SH    RE,=H'7'            CONVERT J (=209) TO 9                        
*                                                                               
* INDEX INTO SAVED ELEMENT TO FIND COMMERCIAL CODE *                            
*                                                                               
         SLL   RE,4                X 16                                         
         LA    RE,0(RE,R5)                                                      
         CLI   0(RE),C'*'          TEST DELETED                                 
         BE    LRR072                                                           
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BE    LRR071B                                                          
*                                                                               
         MVC   0(8,R2),0(RE)  MOVE COMMERCIAL ID TO PRINT BUFFER                
         MVC   9(8,R2),8(RE)                                                    
         OC    8(8,RE),8(RE)         TEST PIGGYBACK                             
         BZ    *+8                                                              
         MVI   8(R2),C'-'                                                       
         B     LRR071C                                                          
                                                                                
LRR071B  MVC   DUB,0(RE)                                                        
         ST    RE,SVRE             SAVE RE                                      
                                                                                
         GOTO1 VTRPACK,DMCB,(C'U',DUB),0(R2)                                    
         L     RE,SVRE             RESTORE RE                                   
         OC    8(8,RE),8(RE)                                                    
         BZ    LRR071C                                                          
         MVC   DUB,8(RE)                                                        
                                                                                
         LA    RF,8(R2)                                                         
         CLI   0(RF),X'40'         SPACE                                        
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         MVI   0(RF),C'-'                                                       
         LA    RF,1(RF)                                                         
         ST    R2,FULL             SAVE R2                                      
         LR    R2,RF                                                            
                                                                                
         ST    RE,SVRE             SAVE RE                                      
         GOTO1 VTRPACK,DMCB,(C'U',DUB),0(R2)                                    
         L     RE,SVRE             RESTORE RE                                   
         L     R2,FULL              AND R2                                      
                                                                                
LRR071C  OC    8(8,RE),8(RE)         TEST PIGGYBACK                             
         BZ    LRR071E                                                          
*                                                                               
         CLI   BYTE,0              ROT BY PERCENT                               
         BE    LRR071H              NO                                          
         MVC   25(25,R2),SPACES                                                 
         MVC   25+4(9,R2),WORK                                                  
         LA    R2,25(,R2)                                                       
         B     LRR071H                                                          
*                                                                               
LRR071E  CLI   BYTE,0              ROT BY PERCENT                               
         BE    LRR071H              NO                                          
         MVC   13(12,R2),WORK                                                   
         CLI   WORK+12,C' '                                                     
         BNH   LRR071H                                                          
         MVC   12(13,R2),WORK                                                   
*                                                                               
LRR071H  LA    R2,25(,R2)                                                       
         C     R2,AENDEXP                                                       
         BH    EXPSIZER                                                         
*                                                                               
LRR072   LA    R3,1(,R3)                                                        
         CLI   BYTE,0              ROT BY PERCENT                               
         BE    *+8                  NO                                          
         LA    R3,2(,R3)                                                        
         BCT   R4,LRR070                                                        
*                                                                               
* CALCULATE LINE COUNT *                                                        
*                                                                               
LRR073   MVI   0(R2),0             MARK END OF LIST                             
*                                                                               
* NOW CALC # OF LINES TO PRINT                                                  
*                                                                               
         LA    R2,2                                                             
         CLI   SRTPROD2,0                                                       
         BE    *+8                                                              
         LA    R2,1(,R2)                                                        
*                                                                               
         CLI   SRTDAYPT,0                                                       
         BE    *+8                                                              
         LA    R2,3(,R2)                                                        
         MVI   ELCODE,X'20'        MKT/STA LIST                                 
         L     R6,AIO2                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE               GET ELEM LEN                                 
         ZIC   RF,1(R6)                                                         
         D     RE,=F'5'                                                         
         CH    RE,=H'3'            MUST BE REM OF 3                             
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,3(,RF)           ADD BOXES LINES + 1 SPACE                    
         AR    R2,RF               SAVE NUMBER OF SUB ELEMS                     
         MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   LRR076                                                           
         LA    R2,3(,R2)                                                        
LRR074   BAS   RE,NEXTEL                                                        
         BNE   LRR076                                                           
         LA    R2,1(,R2)                                                        
         B     LRR074                                                           
LRR076   CLI   SRTDAYPT,0          ANY DAYPART                                  
         BE    *+8                                                              
         LA    R2,3(,R2)                                                        
*                                                                               
* SEARCH BOTH AEXMCML AND A EXPPTN - ADD LARGER CT                              
*                                                                               
         L     R3,ACMLEXP                                                       
         L     R4,APTNEXP                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
         CLI   0(R3),0                                                          
         BE    *+12                                                             
         LA    R3,EXPCNEXT-EXPCENT(,R3)                                         
         BCT   RE,*-12                                                          
*                                                                               
         CLI   0(R4),0                                                          
         BE    *+12                                                             
         LA    R4,25(,R4)                                                       
         BCT   RF,*-12                                                          
*                                                                               
         LPR   RE,RE                                                            
         LPR   RF,RF                                                            
         CR    RE,RF                                                            
         BNH   *+10                                                             
         AR    R2,RE                                                            
         B     *+6                                                              
*                                                                               
         AR    R2,RF                                                            
*                                                                               
         STC   R2,ALLOWLIN                                                      
*                                                                               
*--NOW PRINT OUT BOTH EXPLODED LISTS                                            
*                                                                               
         L     R2,ACMLEXP                                                       
         L     R4,APTNEXP                                                       
*                                                                               
         CLI   0(R2),0                                                          
         BE    LRR084                                                           
         MVC   PCOMML(30),EXPCML                                                
*                                                                               
         CLC   EXPCSLN,SPACES                                                   
         BE    LRR080                                                           
         MVC   PSLN,EXPCSLN                                                     
*                                                                               
LRR080   LA    R2,EXPCNEXT                                                      
*                                                                               
LRR084   CLI   0(R4),0                                                          
         BE    LRR086                                                           
         MVC   PROT,0(R4)                                                       
         LA    R4,25(,R4)                                                       
*                                                                               
LRR086   BAS   RE,PRT              GO PRINT LINE(S)                             
*                                                                               
         CLI   SRTPROD2,0                                                       
         BE    LRR100                                                           
         LA    R3,SRTPROD2                                                      
         MVC   PPROD2,SRTPROD2                                                  
         BAS   RE,FPRDNM                                                        
         MVC   PPRODN2,PRDNM                                                    
*                                                                               
LRR100   CLI   0(R2),0                                                          
         BE    LRR120                                                           
         MVC   PCOMML(30),EXPCML                                                
*                                                                               
         CLC   EXPCSLN,SPACES                                                   
         BE    LRR110                                                           
         MVC   PSLN,EXPCSLN                                                     
*                                                                               
LRR110   LA    R2,EXPCNEXT                                                      
*                                                                               
LRR120   CLI   0(R4),0                                                          
         BE    LRR124                                                           
         MVC   PROT,0(R4)                                                       
         LA    R4,25(,R4)                                                       
*                                                                               
LRR124   BAS   RE,PRT              GO PRINT LINE(S)                             
*                                                                               
         CLI   0(R2),0                                                          
         BNE   LRR100                                                           
         CLI   0(R4),0                                                          
         BNE   LRR100                                                           
         EJECT                                                                  
         LA    R3,SRTPROD                                                       
*                                                                               
         BAS   RE,PRTEPROD         PRINT PRODUCT EQUIVALENTS                    
*                                                                               
* MARKET/STATION LIST ELEMENT                                                   
*                                                                               
         BAS   RE,PMS              PRINT MKT/STA LIST                           
*                                                                               
         LA    R4,EQVPRDTB                                                      
         LA    R5,PATLSTB                                                       
*                                                                               
         SR    R6,R6                                                            
LRR130   OC    0(48,R4),0(R4)                                                   
         BZ    LRR134                                                           
         MVC   P1+2(48),0(R4)                                                   
         LA    R4,48(,R4)                                                       
         BCTR  R6,0                                                             
LRR134   OC    0(40,R5),0(R5)                                                   
         BZ    LRR136                                                           
         MVC   P1+51(40),0(R5)                                                  
         LA    R5,40(,R5)                                                       
         BCTR  R6,0                                                             
LRR136   LTR   R6,R6                                                            
         BZ    LRR150                                                           
         OC    0(48,R4),0(R4)                                                   
         BNZ   *+14                                                             
         OC    0(40,R5),0(R5)                                                   
         BZ    LRR140                                                           
         BAS   RE,PRT              GO PRINT LINE(S)                             
         SR    R6,R6                                                            
         B     LRR130                                                           
LRR140   MVI   SPACING,2                                                        
         BAS   RE,PRT              GO PRINT LINE(S)                             
*                                                                               
* PRINT ANY COPY CODE *                                                         
*                                                                               
LRR150   CLI   SRTDAYPT,0                                                       
         BE    LRR160                                                           
*                                                                               
         MVC   PCOMMLT-2(45),=45C'*'                                            
         MVI   PCOMMLT+132-2,C'*'                                               
         MVC   PCOMMLT+132(39),DAYPTMS                                          
         MVC   PCOMMLT+40+132(1),SRTDAYPT                                       
         MVI   PCOMMLT+132+42,C'*'                                              
         MVC   PCOMMLT+264-2(45),=45C'*'                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRT              GO PRINT LINE(S)                             
*                                                                               
* PRINT ANY PATTERN COMMENTS *                                                  
*                                                                               
LRR160   MVI   ELCODE,X'40'                                                     
         L     R6,AIO2                                                          
         BAS   RE,GETEL                                                         
         BNE   LRR176                                                           
         USING PATCMTEL,R6                                                      
         MVC   PCOMMLT-7(8),=C'* NOTE *'                                        
LRR170   ZIC   R1,PATCMTLN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,LRRMVCC                                                       
         BAS   RE,NEXTEL                                                        
         BNE   LRR174                                                           
*                                                                               
         BAS   RE,PRT              GO PRINT LINE(S)                             
         B     LRR170                                                           
*                                                                               
LRR174   MVI   SPACING,2                                                        
         BAS   RE,PRT              GO PRINT LINE(S)                             
*                                                                               
LRR176   BAS   RE,DCML             CHECK FOR DELETED COMMERCIALS                
*                                                                               
         B     LRR000                                                           
*                                                                               
LRREND   B     EXIT                                                             
LRRMVCA  MVC   9(0,RE),ELEM                                                     
LRRMVCB  MVC   8(0,RE),PATPTN-PATPTNEL(R6)                                      
LRRMVCC  MVC   PCOMMLT+3(0),PATCMT-PATCMTEL(R6)                                 
         DROP  R6                                                               
         EJECT                                                                  
PRT      NTR1                                                                   
         MVI   HDHKSW,C'N'                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
EDITSLN  CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         LA    R0,3                                                             
         LA    R1,DUB                                                           
EDITS10  CLI   0(R1),C'0'                                                       
         BNE   EDITS20                                                          
         LA    R1,1(,R1)                                                        
         BCT   R0,EDITS10                                                       
EDITS20  MVC   0(1,RF),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,EDITS20                                                       
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
* BUMP KEY FOR NEXT RECORD *                                                    
*                                                                               
NXK      LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10               RIGHT JUSTIFY, DROPPING SUBLINE              
         X     R1,=XL4'00003FFF'   NOW MAKE POSITIVE                            
         STH   R1,BREF                                                          
         SH    R1,=H'1'            BUILD NEXT KEY                               
         BP    NXK20               IF STILL NOT ZERO, OK                        
         ZIC   R1,PATKCODE         BUMP CODE                                    
         LA    R1,1(,R1)                     BY 1                               
         STC   R1,PATKCODE                                                      
         CLI   PATKCODE,0          IF OVER 255, BUMP SPOT LEN                   
         BNE   NXK10                                                            
         ZIC   R1,PATKSLN2        BUMP SPOT LEN2                                
         LA    R1,1(,R1)                     BY 1                               
         STC   R1,PATKSLN2                                                      
NXK10    SR    R1,R1               SET BREF                                     
         B     *+8                         ZERO AND LEAVE IT ZERO               
NXK20    X     R1,=XL4'00003FFF'             RESET REF TO 1'S COMPL             
         SLL   R1,10                         AND SUBLINE ZERO                   
         STCM  R1,7,PATKREF                                                     
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
* PRINT PRODUCT CODE AND SPOT LEN                                               
*                                                                               
PPRD     NTR1                                                                   
         LA    R5,FLD              ADDRESS OF OUTPUT AREA                       
         XC    FLD,FLD                                                          
         CLI   0(R3),0             ANY PRODUCT CODE                             
         BE    EXIT                NO,DONE                                      
         L     R1,ASVCLIST         ADDRESS OF SAVED C LIST (VALICLT)            
PPRD10   CLI   0(R1),C' '                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R3),3(R1)                                                    
         BE    PPRD12                                                           
         LA    R1,4(R1)                                                         
         B     PPRD10                                                           
PPRD12   MVC   0(3,R5),0(R1)                                                    
         CLI   1(R3),0             ANY SPOT LEN                                 
         BE    PPRD16              NO                                           
         LA    R5,2(,R5)                                                        
         CLI   0(R5),C' '                                                       
         BNH   PPRD14                                                           
         LA    R5,1(,R5)                                                        
PPRD14   MVI   0(R5),C'-'                                                       
         EDIT  (B1,1(R3)),(3,1(R5)),ALIGN=LEFT                                  
PPRD16   B     EXIT                                                             
         EJECT                                                                  
* READ ALL PRODUCTS FOR A CLT & STORE IN DUMMY                                  
* PRODUCT CODE AND PRODUCT NAME                                                 
*                                                                               
RDNAMES  NTR1                                                                   
*                                                                               
         LA    R2,220                                                           
         L     R3,ASVCLIST         PRODUCT LIST FOR A CLIENT                    
         LA    R4,KEY                                                           
         USING PKEY,R4                                                          
         L     R5,VADUMMY                                                       
         USING PRDTBLD,R5                                                       
*                                                                               
RDNM05   XC    KEY,KEY                                                          
         MVC   PKEYAM(3),BAGYMD & BCLT                                          
         MVC   PKEYPRD,0(R3)                                                    
         DROP  R4                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
         USING PRDHDRD,R6                                                       
         MVC   PRDTPRD,PKEYPRD     MOVE PRD CODE IN TABLE                       
         MVC   PRDTNAME,PNAME      MOVE PRD NAME IN TABLE                       
         LA    R5,PRDTNEXT                                                      
         L     R0,VADUMMY                                                       
         A     R0,SVCMLEN                                                       
         CR    R5,R0                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    R3,4(R3)                                                         
         CLI   0(R3),C' '          END OF PRODUCT LIST?                         
         BNH   RDNM10                                                           
         BCT   R2,RDNM05                                                        
*                                                                               
* READ THROUGH TABLE AND MOVE IN BASE PRODUCT FOR                               
* PRODUCT EQUIVALENTS *                                                         
*                                                                               
RDNM10   DS    0H                                                               
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         L     R5,VADUMMY                                                       
*                                                                               
         USING PEQKEY,R4                                                        
RDNM15   XC    KEY,KEY                                                          
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQPAM,BAGYMD                                                    
         MVC   PEQPCLT,BCLT                                                     
         MVC   PEQPEPRD,0(R5)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   RDNM20                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PEQDTAEL,R6                                                      
*                                                                               
RDNM17   CLC   PEQPROD,PRDTPRD     SAME PROD EQUIVALENT                         
         BNE   RDNM18                                                           
*                                                                               
* NO LONGER CHECK FOR ACTIVE/INACTIVE STATUS                                    
*                                                                               
*        CLI   PEQACT,C'A'         ONLY WANT ACTIVE                             
*        BNE   RDNM20               DON'T WANT IT                               
         MVC   PRDTPRDB,PEQPBPRD   SAVE BASE PROD                               
         B     RDNM20                                                           
*                                                                               
RDNM18   BAS   RE,NEXTEL                                                        
         BE    RDNM17                                                           
         DC    H'0'                                                             
*                                                                               
RDNM20   LA    R5,PRDTNEXT                                                      
         OC    0(26,R5),0(R5)                                                   
         BNZ   RDNM15                                                           
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* READ THROUGH TABLE AND PRINT PRODUCT EQUIVALENCIES                            
*                                                                               
         DS    0H                                                               
PRTEPROD NTR1                                                                   
         L     R5,VADUMMY                                                       
         USING PRDTBLD,R5                                                       
         LR    R6,R5                                                            
         A     R6,SVCMLEN                                                       
*                                                                               
         SR    R1,R1               PRINT PTR                                    
         SR    R2,R2               FOR SECOND LINE                              
         SR    R4,R4               ITEM CT ON 1 LINE                            
         XC    EQVPRDTB,EQVPRDTB                                                
         LA    RF,EQVPRDTB                                                      
*                                                                               
PRTE10   CLC   PRDTPRDB,0(R3)       SAME PRODUCT                                
         BE    PRTE14                                                           
         LA    R5,PRDTNEXT                                                      
         CR    R5,R6                                                            
         BH    PRTE30                                                           
         OC    PRDTPRD,PRDTPRD                                                  
         BNZ   PRTE10                                                           
         B     PRTE30                                                           
*                                                                               
PRTE14   LTR   R2,R2               DON'T WANT , FIRST ON 2ND LINE               
         BNZ   PRTE20                                                           
         LTR   R4,R4                                                            
         BZ    PRTE16              FIRST TIME AROUND                            
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         B     PRTE20                                                           
*                                                                               
PRTE16   MVC   0(19,RF),=CL19'EQUIVALENT PRODUCTS'                              
         LA    RF,48(,RF)                                                       
*                                                                               
         LR    R1,RF                                                            
         MVC   0(3,R1),23(R5)                                                   
         MVI   3(R1),C'='                                                       
         LA    R1,4(R1)                                                         
*                                                                               
PRTE20   MVC   0(3,R1),0(R5)                                                    
         LA    R5,26(R5)                                                        
         LA    R4,1(R4)                                                         
         SR    R2,R2               NOW NEED COMMA                               
         OC    0(3,R5),0(R5)                                                    
         BZ    PRTE30                                                           
         C     R4,=F'10'                                                        
         BNL   PRTE24                                                           
         LA    R1,3(R1)                                                         
         B     PRTE10                                                           
*                                                                               
PRTE24   LA    R1,48(,RF)          NEXT LINE FOR EQUIV PRODUCTS                 
         LA    RF,48(,RF)                                                       
         SR    R4,R4                                                            
         LA    R2,1(R2)                                                         
         B     PRTE10                                                           
*                                                                               
PRTE30   OC    3(3,R3),3(R3)                                                    
         BZ    EXIT                                                             
         L     R5,VADUMMY                                                       
*                                                                               
         LTR   R1,R1                                                            
         BZ    PRTE34                                                           
         LA    R1,48(,RF)                                                       
         LA    RF,48(,RF)                                                       
*                                                                               
PRTE34   SR    R4,R4                                                            
         SR    R2,R2               FOR SECOND LINE                              
PRTE35   CLC   PRDTPRDB,3(R3)       SAME PRODUCT                                
         BE    PRTE40                                                           
         LA    R5,PRDTNEXT                                                      
         CR    R5,R6                                                            
         BH    EXIT                                                             
         OC    PRDTPRD,PRDTPRD                                                  
         BNZ   PRTE35                                                           
         B     EXIT                                                             
PRTE40   LTR   R2,R2               DON'T WANT , FIRST ON 2ND LINE               
         BNZ   PRTE44                                                           
         LTR   R4,R4                                                            
         BZ    PRTE42              FIRST TIME AROUND                            
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         B     PRTE44                                                           
*                                                                               
PRTE42   LTR   R1,R1                                                            
         BNZ   PRTE43                                                           
         MVC   0(19,RF),=CL19'EQUIVALENT PRODUCTS'                              
         LA    RF,48(,RF)                                                       
         LR    R1,RF                                                            
*                                                                               
PRTE43   MVC   0(3,R1),PRDTPRDB                                                 
         MVI   3(R1),C'='                                                       
         LA    R1,4(R1)                                                         
*                                                                               
PRTE44   MVC   0(3,R1),0(R5)                                                    
         LA    R5,26(R5)                                                        
         CR    R5,R6                                                            
         BNL   EXIT                                                             
         LA    R4,1(R4)                                                         
         SR    R2,R2               NOW NEED COMMA                               
         OC    0(3,R5),0(R5)                                                    
         BZ    EXIT                                                             
         C     R4,=F'10'                                                        
         BE    PRTE50                                                           
         LA    R1,3(R1)                                                         
         B     PRTE35                                                           
*                                                                               
PRTE50   LA    R1,48(,RF)          NEXT LINE FOR EQUIV PRODUCTS                 
         LA    RF,48(,RF)                                                       
         SR    R4,R4                                                            
         LA    R2,1(R2)                                                         
         B     PRTE35                                                           
         EJECT                                                                  
* PRINT MARKET STATION LIST                                                     
*                                                                               
         DS    0H                                                               
PMS      NTR1                                                                   
         LA    R4,PATLSTB                                                       
*                                                                               
         LR    RE,R4                                                            
         LA    RF,4000                                                          
         XCEFL                                                                  
*                                                                               
         MVI   ELCODE,X'20'        MKT/STA LIST                                 
         L     R6,AIO2                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATLSTEL,R6                                                      
*                                                                               
         SR    R0,R0               GET ELEM LEN                                 
         ZIC   R1,PATLSTLN                                                      
         D     R0,=F'5'                                                         
         CH    R0,=H'3'            MUST BE REM OF 3                             
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R3,R1               SAVE NUMBER OF SUB ELEMS                     
         LA    R2,PATLST           START OF LIST                                
*                                                                               
         CLI   PATLSTTY,C'T'                                                    
         BNE   PMS10                                                            
         MVI   0(R4),C'*'                                                       
         MVC   1(25,R4),0(R4)                                                   
         LA    R4,40(,R4)                                                       
         MVC   2(20,R4),=C'FOR ALL STATION TYPE'                                
PMS06    MVI   0(R4),C'*'                                                       
         MVI   25(R4),C'*'                                                      
         MVC   23(1,R4),0(R2)                                                   
         LA    R4,40(,R4)                                                       
         LA    R2,5(,R2)                                                        
         BCT   R3,PMS06                                                         
         MVC   0(26,R4),PATLSTB                                                 
         B     EXIT                                                             
*                                                                               
PMS10    CLI   PATLSTTY,C'A'                                                    
         BNE   PMS20                                                            
         MVI   0(R4),C'*'                                                       
         MVC   1(39,R4),0(R4)                                                   
         LA    R4,40(,R4)                                                       
         MVC   2(31,R4),=C'FOR ALL STATION AFFILIATED WITH'                     
PMS14    MVI   0(R4),C'*'                                                       
         MVC   34(3,R4),0(R2)                                                   
         MVI   39(R4),C'*'                                                      
         LA    R4,40(,R4)                                                       
         LA    R2,5(,R2)                                                        
         BCT   R3,PMS14                                                         
         MVC   0(40,R4),PATLSTB                                                 
         B     PMSX                                                             
*                                                                               
PMS20    CLI   PATLSTTY,C'G'       MARKET GROUP                                 
         BE    PMS60                                                            
         CLI   PATLSTTY,C'C'       COMBINED MARKET/AFFILIATE LIST               
         BE    PMS30                                                            
         CLI   PATLSTTY,C'M'                                                    
         BNE   PMS30                                                            
         OC    0(5,R2),0(R2)       ALL MKTS LIST                                
         BNZ   PMS26                                                            
         MVI   0(R4),C'*'                                                       
         MVC   1(19,R4),0(R4)                                                   
         MVI   40(R4),C'*'                                                      
         MVC   42(16,R4),=C'FOR ALL STATIONS'                                   
         MVI   59(R4),C'*'                                                      
         MVC   80(20,R4),0(R4)                                                  
         LA    R4,120(,R4)                                                      
         B     PMSX                                                             
*                                                                               
PMS26    MVI   0(R4),C'*'                                                       
         MVC   1(31,R4),0(R4)                                                   
         LA    R4,40(,R4)                                                       
         MVC   2(3,R4),=C'FOR'                                                  
         B     PMS40                                                            
*                                                                               
PMS30    MVI   0(R4),C'*'                                                       
         MVC   1(39,R4),0(R4)                                                   
         MVC   42(26,R4),=C'FOR THE FOLLOWING STATIONS'                         
         LA    R4,40(,R4)                                                       
*                                                                               
PMS40    MVI   0(R4),C'*'                                                       
         CLI   PATLSTTY,C'C'       COMBINED MARKET/AFFILIATE LIST               
         BE    PMS42                                                            
         CLI   PATLSTTY,C'M'       MARKET LIST                                  
         BNE   PMS50                                                            
PMS42    CLI   0(R2),0             THIS THE FIRST AFFILIATE                     
         BNE   PMS44                YES                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,3(R2)                                                       
         BRAS  RE,FMKT                                                          
         MVC   6(24,R4),MKTNM                                                   
         MVI   31(R4),C'*'                                                      
         B     PMS54                                                            
*                                                                               
PMS44    MVC   4(19,R4),=C'AND AFFILIATED WITH'                                 
*                                                                               
PMS46    MVC   24(4,R4),0(R2)                                                   
         MVI   0(R4),C'*'                                                       
         MVI   31(R4),C'*'                                                      
         LA    R4,40(,R4)                                                       
         LA    R2,5(,R2)                                                        
         BCT   R3,PMS46                                                         
         B     PMS56                                                            
         EJECT                                                                  
* PRINT OUT STATION ELEMENT                                                     
*                                                                               
PMS50    CLI   PATLSTTY,C'S'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   39(R4),C'*'                                                      
         OC    0(2,R2),0(R2)       IS THIS A CABLE HEAD STATION                 
         BNZ   PMS52                                                            
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',(R2)),WORK,WORK+4                             
*                                                                               
         MVC   30(8,R4),WORK+4                                                  
         MVI   34(R4),C'/'                                                      
         B     PMS54                                                            
*                                                                               
PMS52    MVC   30(4,R4),0(R2)                                                   
         LA    R1,34(,R4)                                                       
         CLI   33(R4),C' '                                                      
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(R2)                                                    
         MVI   2(R1),C'V'                                                       
         CLI   4(R2),C'T'                                                       
         BE    PMS54                                                            
         MVI   2(R1),C'M'                                                       
         CLI   4(R2),C'A'                                                       
         BE    PMS54                                                            
         CLI   4(R2),C'F'                                                       
         BE    PMS54                                                            
         MVI   2(R1),C' '                                                       
*                                                                               
PMS54    LA    R4,40(,R4)                                                       
         LA    R2,5(,R2)                                                        
         BCT   R3,PMS40                                                         
PMS56    MVI   0(R4),C'*'                                                       
         MVC   1(31,R4),0(R4)                                                   
         CLI   PATLSTTY,C'M'                                                    
         BE    PMSX                                                             
         MVC   32(8,R4),0(R4)                                                   
         B     PMSX                                                             
*                                                                               
* PRINT MARKET GROUP LIST - GET HEADING - THEN GROUP, THEN MARKETS *            
*                                                                               
PMS60    MVI   0(R4),C'*'                                                       
         MVC   1(31,R4),0(R4)                                                   
         LA    R4,40(,R4)                                                       
*                                                                               
*                                                                               
PMS62    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+8(1),0(R2)                                                   
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PMS66                                                            
         OC    KEY+5(3),KEY+5      IS THIS A PRODUCT GROUP                      
         BZ    PMS64                NO                                          
         MVC   KEYSAVE+5(3),KEY+5                                               
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PMS66                                                            
*                                                                               
* TRY FOR ALL CLIENTS MARKET GROUP *                                            
*                                                                               
PMS64    MVC   KEY(13),KEYSAVE                                                  
         XC    KEY+3(5),KEY+3      TRY NON-CLIENT SPECIFIC                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PMS66                                                            
         CLC   KEY(5),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    KEY+5(3),KEY+5      IS THIS A PRODUCT GROUP                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   KEYSAVE+5(3),KEY+5                                               
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PMS66                                                            
         DC    H'0'                                                             
PMS66    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'01'        GET BREAK NAME                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   0(R4),C'*'                                                       
         MVC   2(24,R4),2(R6)                                                   
         MVI   31(R4),C'*'                                                      
         LA    R4,40(,R4)                                                       
         MVC   KEY+8(3),0(R2)                                                   
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PMS70    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'        GET BREAK NAME                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   0(R4),C'*'                                                       
         MVC   2(24,R4),2(R6)                                                   
         MVI   31(R4),C'*'                                                      
         LA    R4,40(,R4)                                                       
*                                                                               
         MVI   KEY+1,X'82'                                                      
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   2(3,R4),=C'FOR'                                                  
*                                                                               
PMS74    MVI   0(R4),C'*'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,KEY+11                                                      
         BRAS  RE,FMKT                                                          
         MVC   6(24,R4),MKTNM                                                   
         MVI   31(R4),C'*'                                                      
         LA    R4,40(,R4)                                                       
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 SEQ                                                              
         CLC   KEY(11),KEYSAVE                                                  
         BE    PMS74                                                            
*                                                                               
         LA    R2,5(,R2)                                                        
         BCT   R3,PMS62                                                         
         MVI   0(R4),C'*'                                                       
         MVC   1(31,R4),0(R4)                                                   
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
PMSX     LA    RF,PATLSTB                                                       
         LA    RF,4000(,RF)                                                     
         CR    R4,RF                                                            
         BNH   EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* SUBROUTINE TO EXTRACT PRD NAME *                                              
*                                                                               
         DS    0H                                                               
FPRDNM   NTR1                                                                   
*                                                                               
         L     R5,VADUMMY                                                       
         LR    R6,R5                                                            
         A     R6,SVCMLEN                                                       
*                                                                               
FPRDNM10 CLI   0(R5),0                                                          
         BE    EXIT                                                             
         CLC   0(3,R5),0(R3)       MATCH                                        
         BE    FPRDNM30                                                         
         LA    R5,26(R5)                                                        
         B     FPRDNM10                                                         
*                                                                               
FPRDNM30 MVC   PRDNM(20),3(R5)                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         EJECT                                                                  
* CHECK FOR DELETED COMMERCIALS *                                               
*                                                                               
DCML     NTR1                                                                   
         L     R6,AIO2             COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   DCML30              INCOMPLETE PATTERN                           
*NOP     DC    H'0'                                                             
         USING PATCMLEL,R6                                                      
         ZIC   R3,PATCMLLN         GET ELEM LEN                                 
         SRL   R3,3                DIV BY 8=NO OF CMML PRS (DROPS ODD)          
         LA    R4,PATCML           1ST CMML                                     
         DROP  R6                                                               
         CLC   =X'C8C9C1E3E4E2',0(R4) HIATUS                                    
         BE    DCML30                                                           
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO1            USE I/O 1                                    
*                                                                               
DCML10   OC    0(8,R4),0(R4)       IS THIS A CML                                
         BZ    DCML20                                                           
         CLC   DELCML,0(R4)        DELETED CML                                  
         BE    DCML20                                                           
         XC    KEY(13),KEY                                                      
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
*                                                                               
         MVC   CMLKID,=XL2'0A21'                                                
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   CMLKID,=XL2'0AC1'                                                
*                                                                               
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,0(R4)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       CMML DELETED                                 
         BZ    DCML20              NO                                           
         TM    PRDMATSW,1          FOUND DELETED CMLS YET                       
         BO    DCML12              NO                                           
         MVC   P+80(27),=C'* NOTE COMMERCIAL DELETED *'                         
*                                                                               
         BAS   RE,PRT              GO PRINT LINE(S)                             
*                                                                               
         OI    PRDMATSW,01                                                      
DCML12   MVC   P+80(8),0(R4)                                                    
         MVC   P+90(15),CMLTITLE                                                
*                                                                               
         BAS   RE,PRT              GO PRINT LINE(S)                             
*                                                                               
         B     DCML20                                                           
DCML20   LA    R4,8(,R4)                                                        
         BCT   R3,DCML10                                                        
DCML30   B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE COMMERCIAL ROTATION                                                  
* FILTER CMMLS FOR LIST FUNCTION                                                
*                                                                               
FTR      NTR1                                                                   
         USING PATKEY,R4                                                        
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         USING PATDTAEL,R6                                                      
         CLI   PATDTALN,38         IS THIS OLD PAT LEN (NO PATSTAT)             
         BE    FTR06               YES, NO SOFT DELETE CK                       
*                                                                               
         TM    PATSTAT,X'80'                                                    
         BO    FTRNO                                                            
         TM    PATSTAT,X'10'       BYPASS COPY CODE = ESTIMATE                  
         BO    FTRNO                                                            
*                                                                               
         TM    FLAGFTR,FTRREV      THIS REVISIONS ONLY                          
         BZ    FTR06                                                            
         TM    PATSTAT,X'02'       THIS ALREADY PRINTED                         
         BO    FTRNO                                                            
*                                                                               
FTR06    MVI   ELCODE,X'30'                                                     
         BAS   RE,NEXTEL                                                        
         USING PATCMLEL,R6                                                      
         ZIC   R0,PATCMLLN                                                      
         SRL   R0,4                DIVIDE BY 16 = NUMBER OF CML'S               
         LA    R1,PATCML           START OF CML LIST                            
*NOP     CLC   =C'HIATUS',0(R1)    BYPASS HIATUS PATTERNS                       
*NOP     BE    FTRNO                                                            
         CR    R1,R1               SET COND CODE FILTERED OK                    
         B     EXIT                                                             
FTRNO    CR    RB,RD               SET COND CODE NO FILTER                      
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* FORMAT PERCENT ELEMENT *                                                      
*                                                                               
         DS    0H                                                               
PCT      NTR1                                                                   
*                                                                               
         USING PATPCTEL,R6                                                      
         SR    R0,R0                                                            
         ZIC   R1,PATPCTLN                                                      
         D     R0,=F'3'                                                         
         CH    R0,=H'2'                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R1                                                            
         SR    R1,R1                                                            
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R3,PATPCTLT                                                      
         B     PCT20                                                            
PCT10    MVI   0(R2),C','                                                       
         LA    R1,1(,R1)                                                        
         LA    R2,1(,R2)                                                        
PCT20    MVC   0(1,R2),0(R3)                                                    
         MVI   1(R2),C'='                                                       
*                                                                               
         ZIC   R0,1(R3)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R2),DUB                                                      
*                                                                               
         LA    R1,4(,R1)                                                        
         LA    R2,4(,R2)                                                        
         LA    R3,3(,R3)                                                        
         BCT   R5,PCT10                                                         
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE FILTERS                                                              
*                                                                               
VOPT     NTR1                                                                   
*                                                                               
         XC    FILTERS,FILTERS                                                  
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPT96              NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPT06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VOPT02                                                           
         LA    R1,4                                                             
         B     VOPT04                                                           
VOPT02   ZIC   R1,5(R2)                                                         
VOPT04   EX    R1,VOPTCLCH                                                      
         BNE   VOPT08                                                           
VOPT06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTHELP),OPTHELP                                       
         B     ERREXIT2                                                         
*                                                                               
VOPT08   GOTO1 SCANNER,DMCB,(20,TRAOPTH),(7,BLOCK+64)                           
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
VOPT10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VOPT14              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VOPT20              NO, NETHER                                   
VOPT14   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
         EJECT                                                                  
VOPT20   EX    R1,VOPTCLCA         REVISIONS                                    
         BNE   VOPT30                                                           
         OI    FLAGFTR,FTRREV                                                   
         B     VOPT90                                                           
*                                                                               
VOPT30   EX    R1,VOPTCLCB         TEST                                         
         BNE   VOPT100                                                          
         OI    FLAGFTR,FTRTEST                                                  
*                                                                               
VOPT90   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
VOPT96   B     EXIT                                                             
*                                                                               
VOPT100  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTMSG+L'OPTHELP),OPTMSG                               
         B     ERREXIT2                                                         
*                                                                               
OPTMSG   DC    C'* ERROR *'                                                     
OPTHELP  DC    C'FILTERS-REVISION/TEST'                                         
VOPTCLCA CLC   12(0,R4),=CL10'REVISIONS'                                        
VOPTCLCB CLC   12(0,R4),=CL5'TEST'                                              
VOPTCLCH CLC   12(0,R4),=CL5'HELP'                                              
         EJECT                                                                  
* IF GETS HERE, EXCEEDED COMMERCIAL/PATTERN ROTATION TABLE SIZE                 
*                                                                               
EXPSIZER DC    H'0'                                                             
*                                                                               
NOSRTER  L     R1,=A(NOSRTMS)                                                   
         B     ERREXIT                                                          
MISTXTER L     R1,=A(MISTXTMS)                                                  
*                                                                               
ERREXIT  XC    CONHEAD,CONHEAD                                                  
         A     R1,SPRELOC                                                       
         BCTR  R1,0                                                             
         ZIC   RE,0(R1)                                                         
         EX    RE,ERRMVC                                                        
ERREXIT2 GOTO1 ERREX2                                                           
*                                                                               
ERRMVC   MVC   CONHEAD(0),1(R1)                                                 
CMLLENER MVI   ERROR,INVCMMLN      CML ID MUST BE 8 CHAR LEN, WASN'T            
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
VCMLERR  MVI   ERROR,INVCOMM       NO SUCH CMML FOR CLT                         
         B     TRAPERR                                                          
PRDSQERR MVI   ERROR,INVPRDSQ      PRODS OUT OF SEQ                             
         B     PRDERR                                                           
EQPRDERR MVI   ERROR,INVEQPRD      PROD/PARTNER PROD EQ                         
PRDERR   LA    R2,TRAPRDH          POINT TO PROD-SPOT LEN                       
         B     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
INVALERR MVI   ERROR,INVALID                                                    
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
DAYPTMS  DC    CL39'ONLY SHOW THESE COMMERCIALS IN DAYPART ='                   
SVCMLEN  DC    F'21500' 250 * 86                                                
ATBL     DC    CL12'ABCDEFGHIJKLMNO'  15 LETTERS FOR 15 COMMERCIALS             
DELCML   DS    0XL16                                                            
         DC    C'*'                                                             
         DC    XL15'00'                                                         
DELMSG   DS    0CL9                                                             
         DC    C'*'                                                             
DELETE   DC    CL6'DELETE'                                                      
         DC    C'D*'                                                            
MARK1    DC    CL82'* - - - - - - - - - - - - - - - - - - - - - - - - -C        
                - - - - - - - - - - - - - - *'                                  
MARK2    DC    CL104'* - - - - - - - - - - - - - - - - - - - - - - - - C        
               - - - - - - - - - - - - - - - - - - - - - - - - - - *'           
MARK3    DC    CL104'* * * * * * * * * * * * * * * * * * * * * * * * * C        
               * * * * * * * * * * * * * * * * * * * * * * * * * * *'           
         EJECT                                                                  
         DC    AL1(L'NOSRTMS-1)                                                 
NOSRTMS  DC    C'* ERROR * NO PATTERNS FOR REQUEST *'                           
         DC    AL1(L'MISTXTMS-1)                                                
MISTXTMS DC    C'* ERROR * MISSING TEXT FOR 1ST PAGE LETTER *'                  
         DC    AL1(L'MISTXTMS-1)                                                
ONEDATMS DS    C'* ERROR * MUST ENTER START AND END DATES *'                    
SORTCARD DC    CL80'SORT FIELDS=(1,17,A),FORMAT=BI '                            
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=21 '                                   
HEADING  SSPEC H1,3,C'CONTACT'                                                  
         SSPEC H1,40,C'COMMERCIAL ROTATION LISTING'                             
         SSPEC H1,85,AGYNAME                                                    
         SSPEC H2,40,C'---------------------------'                             
         SSPEC H2,85,AGYADD                                                     
         SSPEC H3,85,RUN                                                        
         SSPEC H3,41,C'PERIOD'                                                  
         SSPEC H4,85,REPORT                                                     
         SSPEC H6,103,PAGE                                                      
         SSPEC H4,3,C'MEDIA'                                                    
         SSPEC H6,3,C'CLIENT'                                                   
         DC    X'00'               END MARKER FOR SSPEC                         
         DROP  RB,RC                                                            
         EJECT                                                                  
* SUBROUTINE TO READ MARKET REC AND EXTRACT NAME *                              
*                                                                               
         DS    0H                                                               
FMKT     NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         L     R5,AIO3                                                          
         LA    R6,1976(,R5)                                                     
*                                                                               
FMKT10   CLI   0(R5),0                                                          
         BE    FMKT20                                                           
         CH    R0,0(R5)             MATCH                                       
         BE    FMKT30                                                           
         LA    R5,26(R5)                                                        
         CR    R5,R6                                                            
         BL    FMKT10                                                           
         SH    R5,=H'26'                                                        
*                                                                               
FMKT20   XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
         MVI   KEY+8,C'0'                                                       
         MVC   KEY+9(6),KEY+8                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO1                     
         L     R6,AIO1                                                          
         CLC   KEY(8),0(R6)                                                     
         BE    FMKT26                                                           
         STH   R0,0(R5)                                                         
         MVC   2(24,R5),=CL24'** UNKNOWN MARKET **'                             
         B     FMKT30                                                           
*                                                                               
         USING MKTRECD,R6                                                       
*                                                                               
FMKT26   STH   R0,0(R5)                                                         
         MVC   2(24,R5),MKTNAME                                                 
FMKT30   MVC   MKTNM,2(R5)                                                      
         J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
FPRD     NTR1  BASE=*,LABEL=*                                                   
         LA    R0,220                                                           
         L     R1,ASVCLIST         ADDRESS OF SAVED C LIST (VALICLT)            
FPRD10   CLI   0(R1),C' '                                                       
         BL    FPRD14                                                           
*                                                                               
         CLC   BYTE,3(R1)                                                       
         BE    FPRD20                                                           
         LA    R1,4(R1)                                                         
         BCT   R0,FPRD10                                                        
*                                                                               
FPRD14   LA    R1,=C'???'          FOR UNKNOWN PRODUCT CODE                     
*                                                                               
FPRD20   XIT1  REGS=(R1)                                                        
                                                                                
*------------------------------------------------------                         
* SUBROUTINE TO READ COMMERCIAL REC AND EXTRACT TITLE *                         
*------------------------------------------------------                         
         DS    0H                                                               
FCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ASVCMLS                                                       
         LR    R6,R5                                                            
         A     R6,SVCMLEN                                                       
         USING CMLTBLD,R5                                                       
*                                                                               
FCML10   CLI   0(R5),0                                                          
         BE    FCML20                                                           
         CLC   CMLTCML,ELEM        MATCH                                        
         BE    FCML30                                                           
         LA    R5,CMLTNEXT                                                      
         CR    R5,R6                                                            
         BL    FCML10                                                           
         DC    H'0'                                                             
*                                                                               
FCML20   XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(2),=X'0A21'                                                  
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   KEY(2),=XL2'0AC1'                                                
*                                                                               
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),ELEM                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
*                                                                               
         MVC   CMLTCML,ELEM                                                     
         MVC   CMLTSLN,CMLSLN                                                   
         MVC   CMLTDSC1,CMLTITLE                                                
         MVC   CMLTCLT,CMLCLTNO                                                 
         MVC   CMLTOV1,CMLOVRD1                                                 
         MVC   CMLTOV2,CMLOVRD2                                                 
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   FCML30                                                           
         MVC   CMLTDSC2,3(R6)                                                   
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   FCML30                                                           
         MVC   CMLTDSC3,3(R6)                                                   
*                                                                               
FCML30   L     R6,AIO1                                                          
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FCML40                                                           
*                                                                               
         MVC   CMLTELCS,2(R6)                                                   
*                                                                               
FCML40   MVC   ELEM+8(L'CMLTENT-L'CMLTCML),CMLTSLN                              
         J     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
                                                                                
*------------------------                                                       
* VALIDATE PERIOD                                                               
*------------------------                                                       
         DS    0H                                                               
VPER     NMOD1 0,**VPER**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSERR1                                                         
         LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),DATE                                            
         L     R5,DMCB             GET LENGTH OF FIELD                          
         LTR   R5,R5                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,PERSTART)                                
         CLM   R5,1,5(R2)          WAS ONLY 1 DATE ENTERED                      
         BE    ONEDATER             YES, ERROR                                  
         LA    R3,1(R5,R3)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R3),DATE                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,PEREND)                                  
         CLC   PERSTART,PEREND                                                  
         BH    DATERR                                                           
         XIT1                                                                   
*                                                                               
MISSERR1 MVI   ERROR,MISSING                                                    
         B     *+8                                                              
DATERR   MVI   ERROR,INVDATE                                                    
         GOTO1 ERREX                                                            
ONEDATER L     R1,=A(ONEDATMS)                                                  
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         A     R1,SPRELOC                                                       
         BCTR  R1,0                                                             
         ZIC   RE,0(R1)                                                         
         EX    RE,ERRMVC1                                                       
         GOTO1 ERREX2                                                           
ERRMVC1  MVC   CONHEAD(0),1(R1)                                                 
*                                                                               
         DROP  RB,RC                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* HEADING ROUTINE FOR OFF-LINE REPORTS                                          
*                                                                               
HDHK     NMOD1 0,**+HDH**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         MVI   HDHKSW,C'Y'                                                      
*                                                                               
         MVC   H1+12(L'QUESTOR),QUESTOR                                         
         MVC   H2+12(12),CONTEL                                                 
         OC    CONTEL+13(5),CONTEL+13 ANY EXTENTSION                            
         BZ    HDHK10                                                           
         MVC   H2+25(3),=C'EXT'                                                 
         MVC   H2+29(5),CONTEL+13                                               
HDHK10   GOTO1 DATCON,DMCB,(3,PERSTART),(5,H3+47)                               
         MVI   H3+55,C'-'                                                       
         GOTO1 (RF),(R1),(3,PEREND),(5,H3+56)                                   
         MVC   H4+10(L'QMED),QMED                                               
         MVC   H4+15(L'MEDNM),MEDNM                                             
         MVC   H6+10(L'QCLT),QCLT                                               
         MVC   H6+15(L'CLTNM),CLTNM                                             
         TM    FLAGFTR,FTRTEST     THIS TEST RUN ONLY                           
         BZ    HDHK16                                                           
         MVC   H5+46(14),=C'** TEST RUN **'                                     
*                                                                               
HDHK16   TM    FLAGFTR,FTRREV      THIS REVISIONS ONLY                          
         BZ    HDHK20                                                           
         MVC   H5+85(18),=C'* REVISIONS ONLY *'                                 
*                                                                               
HDHK20   CLC   PAGE,=H'1'                                                       
         BE    HDHKX                                                            
*                                                                               
         MVC   H8(L'MTITLE),MTITLE                                              
         MVC   H9(L'MTITLE),MTITLE+L'MTITLE                                     
*                                                                               
         L     R1,=A(MARK1)                                                     
         CLC   P1+13(L'MARK1),0(R1)                                             
         BE    HDHK30                                                           
         L     R1,=A(MARK2)                                                     
         CLC   P1+2(L'MARK3),0(R1)                                              
         BE    HDHK30                                                           
         L     R1,=A(MARK3)                                                     
         CLC   P1+2(L'MARK2),0(R1)                                              
         BNE   HDHKX                                                            
HDHK30   MVC   P1,SPACES                                                        
         MVI   SPACING,0                                                        
*                                                                               
HDHKX    XIT1                                                                   
         LTORG                                                                  
MTITLE   DC    CL108'  CODE  PRODUCT/PARTNER       LENGTH   COMMERCIAL C        
                   TITLE             START-END DATES     ROTATION PATTEC        
               RN'                                                              
         DC    CL108'  ----  ---------------       ------   ---------- C        
                   -----             ----------------    --------------C        
               --'                                                              
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAGYCON                                                     
         EJECT                                                                  
EQVPRD   DSECT                                                                  
       ++INCLUDE SPTRPRDEQV                                                     
*                                                                               
* INCLUDED DSECTS                                                               
* INCLUDE SPGENCLT                                                              
* INCLUDE SPGENPRD                                                              
* INCLUDE SPGENMKT                                                              
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
         EJECT                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA70D                                                       
* INCLUDED DSECTS                                                               
* INCLUDE SPTRAWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPRELOC  DS    F                                                                
ASVCMLS  DS    F                   COMML TABLE                                  
ACMLEXP  DS    F                   PRINTABLE COMML CODES/DESC/SPT LENS          
APTNEXP  DS    F                   PRINTABLE COMML ROTATIONS                    
AENDEXP  DS    F                   END OF ABOVE 2 TABLES                        
SVRE     DS    F                   SAVE RE                                      
VTRPACK  DS    A                                                                
*                                                                               
ALLCTRS  DS   0XL12                                                             
MSCTR    DS    F                                                                
*                                                                               
QUESTOR  DS    CL24                1ST 17 FROM SCREEN, ONLY FULL 24 IF          
*                                  AGENCY CONTACT REC                           
CONTEL   DS    CL18                CONTACT TEL # FROM CONTACT REC               
COMPKEY  DS    CL13                                                             
*                                                                               
SVT2PR04 DS    CL1                 PRINT PERCENT ROTATIONS                      
*                                                                               
* SAVED INFO FROM CURRENT PATTERN *                                             
*                                                                               
SVPATST  DS    CL1                 SAVED PATTERN STATUS                         
HDHKSW   DS    CL1                                                              
SVPB     DS    CL1                                                              
SVCOMP   DS    0CL8                                                             
SVPROD   DS    CL3                                                              
         DS    CL3                 PTR PROD                                     
         DS    CL1                 SLN                                          
         DS    CL1                 SLN2                                         
DATE     DS    CL6                                                              
BREF     DS    H                   REFERENCE NUMBER-BINARY                      
BSUB     DS    H                   SUBLINE                                      
PERSTART DS    XL3                                                              
PEREND   DS    XL3                                                              
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
BREFSUB  DS    XL3                 REF NUM (14 BITS)/SUBLINE (10 BITS)          
REF      DS    CL5                                                              
PRDMATSW DS    XL1                                                              
*                                  01 - LIST - DELETED CMLS FOUND               
*                                                                               
PATLSTCT DS    F                   CT OF PAT RECS IF SORT=DATE                  
*                                                                               
* HOLD AREA FOR STANDARD SPOT LENGTH(S)                                         
*                                                                               
SVPSLN   DS    CL7                                                              
SVPSLNF  DS    XL1                 0=NO PRINT OVERRIDE SPOT LENGTHS             
*                                  1=PRINTED PRINT OVERRIDE SPOT LEN            
*                                                                               
FILTERS  DS    0CL2                                                             
FLAGFTR  DS    CL1                                                              
FTRREV   EQU   X'80'               80 = REVISIONS                               
FTRTEST  EQU   X'40'               40 = TEST                                    
HOLDSIGN DS    CL1                                                              
*                                                                               
HOLDMS   DS    CL45                                                             
         EJECT                                                                  
* SORT PATTERN RECORD *                                                         
*                                                                               
SORTREC  DS    0XL21                                                            
SRTPB    DS    XL1                                                              
SRTPROD  DS    CL3                                                              
SRTPROD2 DS    CL3                                                              
SRTSLN   DS    XL1                                                              
SRTSLN2  DS    XL1                                                              
SRTDAYPT DS    XL1                                                              
SRTORD   DS    XL1                 10 = ALL MKTS                                
*                                  20 = STATION TYPE                            
*                                  30 = AFFILIATE                               
*                                  40 = MARKET LIST                             
*                                  50 = STATION LIST                            
SRTDTS   DS    XL6                                                              
SRTDKAD  DS    XL4                                                              
*                                                                               
EQVPRDTB DS    CL240                                                            
*                                                                               
PATLSTB  DS    CL4000                                                           
*                                                                               
* SAVED COMMERCIALS TABLE                                                       
*                                                                               
CMLTBLD  DSECT                                                                  
CMLTENT  DS    0CL94                                                            
CMLTCML  DS    CL8                                                              
CMLTSLN  DS    XL1                                                              
CMLTDSC1 DS    CL15                                                             
CMLTDSC2 DS    CL20                                                             
CMLTDSC3 DS    CL20                                                             
CMLTCLT  DS    CL20                                                             
CMLTELCS DS    CL8                 TELECASTER                                   
CMLTOV1  DS    CL1                                                              
CMLTOV2  DS    CL1                                                              
CMLTNEXT EQU   *                                                                
*                                                                               
* SAVED PRODUCTS TABLE                                                          
*                                                                               
PRDTBLD  DSECT                                                                  
PRDTPRD  DS    CL3                                                              
PRDTNAME DS    CL20                                                             
PRDTPRDB DS    CL3                                                              
PRDTNEXT EQU   *                                                                
*                                                                               
* EXPLODED COMMLS TABLE                                                         
*                                                                               
EXPCTABD DSECT                                                                  
EXPCENT  DS    0CL38                                                            
EXPCML   DS    CL30                                                             
EXPCSLN  DS    CL7                                                              
EXPCFLG  DS    CL1                                                              
EXPCNEXT EQU   *                                                                
*                                                                               
* OFFLINE PRINT                                                                 
*                                                                               
SPOOLD   DSECT       START END                                                  
         ORG   P                                                                
         DS    CL2       1   2                                                  
PPROD    DS    CL3       3   5                                                  
         ORG   PPROD+1                                                          
PPROD2   DS    CL3       3   5                                                  
         DS    CL2       6   8                                                  
PPRODNM  DS    CL20      9  28                                                  
         ORG   PPRODNM+1                                                        
PPRODN2  DS    CL20     10  29                                                  
*        DS    CL1      29  29                                                  
PSLN     DS    CL7      30  36                                                  
         DS    CL3      37  39                                                  
PCOMMLA  DS   0CL12                ADID CML                                     
PCOMML   DS    CL8      40  47                                                  
         DS    CL6      48  53                                                  
PCOMMLT  DS    CL15     54  68                                                  
         DS    CL4      69  72                                                  
PPERIOD  DS    CL17     73  89                                                  
         DS    CL3      90  92                                                  
PROT     DS    CL25     93 114                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037SPTRA60   02/27/15'                                      
         END                                                                    
