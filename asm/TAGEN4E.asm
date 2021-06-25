*          DATA SET TAGEN4E    AT LEVEL 004 AS OF 06/03/10                      
*PHASE T7024EC,*                                                                
         TITLE 'T7024E - BALANCE REPORT'                                        
T7024E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7024E,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9              ROOT STORAGE AREA                           
         L     RA,ATWA                                                          
         USING T702FFD,RA           SCREEN                                      
*                                                                               
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY                                                      
         BE    VKEY                                                             
         CLI   MODE,PRINTREP                                                    
         BNE   M10                                                              
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS            SPECS                                        
         LA    R1,MYHOOK                                                        
         ST    R1,HEADHOOK                                                      
         BAS   RE,PRREC                                                         
         B     XIT                                                              
*                                                                               
M10      CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   M20                                                              
         BAS   RE,DISKEY                                                        
         B     MX                                                               
*                                                                               
M20      CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   MX                                                               
         L     R3,AIO              LOCATION OF THE RECORD                       
         USING TLBAD,R3                                                         
         MVC   KEY,0(R3)           RE-READ RECORD TO SET READ SEQUENCE          
         GOTO1 HIGH                                                             
         BAS   RE,DISREC                                                        
*                                                                               
MX       B     XIT                                                              
         DROP  R3                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*              VALIDATE KEY                                                     
*                                                                               
         SPACE 1                                                                
VKEY     DS    0H                                                               
         MVI   KEYCHG,C'N'                                                      
         LA    R2,SBCPERH          RUN FOR WHAT PERIOD                          
         CLI   ACTNUM,ACTREP       IF REPORT - ALWAYS RE-VALIDATE               
         BE    *+12                                                             
         TM    4(R2),X'20'         ALREADY VALID                                
         BO    VKEY40                                                           
         MVI   KEYCHG,C'Y'                                                      
         NI    SBCCURRH+4,X'DF'                                                 
*                                                                               
         CLI   ACTNUM,ACTREP       IF REPORT - VALIDATE PERIOD                  
         BE    VKEY5                                                            
         GOTO1 DTVAL,DMCB,THEPDTE                                               
         MVC   TGDDTE,THEPDTE      REQUESTED DATE - COMPLEMENTED                
         XC    TGDDTE,=X'FFFFFF'                                                
         B     VKEY40                                                           
*                                                                               
VKEY5    LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   PENDDTE,PVALPEND                                                 
         MVC   PSTRDTE,PVALPSTA                                                 
         MVC   THEPDTE,PVALPSTA                                                 
         DROP  R3                                                               
*                                                                               
VKEY40   OI    4(R2),X'20'                                                      
         LA    R2,SBCCURRH         FILTER BY CURRENCY                           
         TM    4(R2),X'20'         ALREADY VALID                                
         BO    VKEY60                                                           
         MVI   KEYCHG,C'Y'                                                      
         NI    SBCEMPH+4,X'DF'                                                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKEY45                                                           
         CLI   8(R2),C'U'                                                       
         BNE   VKEY50                                                           
*                                                                               
VKEY45   CLI   ACTNUM,ACTREP       IF REPORT - CURRENCY NOT NECESSARY           
         BE    VKEY60                                                           
         MVI   FILTCURR,C'U'                                                    
         MVI   TGTYCUR,C'U'                                                     
         MVC   8(6,R2),=C'US    '  DEFAULT TO US$                               
         B     VKEY60                                                           
*                                                                               
VKEY50   CLI   8(R2),C'C'          CANADIAN                                     
         BNE   VKEY55                                                           
         MVC   8(6,R2),=C'CANADA'                                               
         MVI   TGTYCUR,C'C'                                                     
         MVI   FILTCURR,C'C'                                                    
         B     VKEY60                                                           
*                                                                               
VKEY55   CLI   8(R2),C'E'          EUROS                                        
         BNE   INVERR                                                           
         MVC   8(6,R2),=C'EUROS '                                               
         MVI   TGTYCUR,C'E'                                                     
         MVI   FILTCURR,C'E'                                                    
*                                                                               
VKEY60   OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         LA    R2,SBCEMPH          FILTER BY EMPLOYER                           
         TM    4(R2),X'20'         ALREADY VALID                                
         BO    VKEY70                                                           
         MVI   KEYCHG,C'Y'                                                      
         NI    SBCTYPEH+4,X'DF'                                                 
*                                                                               
         MVC   SBCEMPN,SPACES      CLEAR SCREEN                                 
         OI    SBCEMPNH+6,X'80'                                                 
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKEY65                                                           
         CLI   ACTNUM,ACTREP       IF NOT REPORT - MUST HAVE EMPLOYER           
         BE    VKEY70                                                           
         MVC   8(3,R2),=C'TP '     DEFAULT TO TALENT PARTNERS                   
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
VKEY65   GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',(R2)),SBCEMPNH                        
         MVC   FILTEMP,SBCEMP                                                   
         OC    FILTEMP,SPACES                                                   
*                                                                               
VKEY70   OI    4(R2),X'20'                                                      
         CLI   ACTNUM,ACTREP       IF REPORT - CHECK ONLY OPTIONS               
         BE    VKEY110                                                          
         LA    R2,SBCTYPEH         WHAT ANALYSIS TYPE                           
         TM    4(R2),X'20'         ALREADY VALID                                
         BO    VKEY100                                                          
         MVI   KEYCHG,C'Y'                                                      
         NI    SBCSTRTH+4,X'DF'                                                 
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKEY80                                                           
         CLI   8(R2),C'D'                                                       
         BNE   VKEY90                                                           
*                                                                               
VKEY80   MVI   TYPE,C'D'           DEFAULT TO DUE DATE                          
         MVC   8(3,R2),=C'DUE'                                                  
         B     VKEY100                                                          
*                                                                               
VKEY90   CLI   8(R2),C'I'                                                       
         BNE   INVERR                                                           
         MVI   TYPE,C'I'              CHECK IF INVOICE ANALYSIS                 
         MVC   8(3,R2),=C'INV'                                                  
*                                                                               
VKEY100  OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,SBCSTRTH         START AT A PARTICULAR DATE                   
         CLI   KEYCHG,C'Y'         IF THE KEY HAS BEEN CHANGED                  
         BNE   VKEY102                                                          
         CLI   CONT,C'Y'                                                        
         BE    VKEY105             AND DATE IS A CONTINUATION                   
         B     VKEY103             ERASE DATE                                   
*                                                                               
VKEY102  TM    4(R2),X'20'         ALREADY VALID                                
         BO    VKEY110                                                          
*                                                                               
VKEY103  NI    SBCOPTH+4,X'DF'                                                  
         MVI   KEYCHG,C'Y'                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKEY105                                                          
         GOTO1 DATVAL,DMCB,(0,8(R2)),STARTDTE                                   
         GOTO1 DATCON,DMCB,(0,STARTDTE),(1,STARTDTP)                            
         B     VKEY110                                                          
*                                                                               
VKEY105  MVC   SBCSTRT,SPACES                                                   
         XC    STARTDTP,STARTDTP                                                
*                                                                               
VKEY110  OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         LA    R2,SBCOPTH          FOR FUTURE OPTIONS                           
         TM    4(R2),X'20'         ALREADY VALID                                
         BO    VKEY150                                                          
         CLI   5(R2),0                                                          
         BE    VKEY150                                                          
*                                                                               
         XC    HALF,HALF           CLEAR FIELD DISP. COUNTER                    
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
*                                                                               
VKEY140  DS    0H                                                               
         LA    R3,SCANNEXT         BUMP TO IT                                   
         BCT   R0,VKEY140          AND CONTINUE                                 
*                                                                               
VKEY150  OI    4(R2),X'20'                                                      
         CLI   KEYCHG,C'Y'                                                      
         BNE   VKEY160                                                          
         XC    TOT1(TOTS),TOT1     CLEAR ALL TOTALS FOR NEW RECORD              
         MVC   SBCDCHG,SPACES                                                   
         OI    SBCDCHGH+6,X'80'    CLEAR ACTIVITY INFO                          
         MVC   SBCTCHG,SPACES                                                   
         OI    SBCTCHGH+6,X'80'                                                 
*                                                                               
VKEY160  CLI   ACTNUM,ACTREP       IF REPORT - DON'T BUILD KEY                  
         BE    VKEYX                                                            
         GOTO1 RECVAL,DMCB,TLBACDQ,(X'C0',0)                                    
         LA    R2,SBCPERH          SET FOR GENCON ERRORS                        
*                                                                               
VKEYX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        DISPLAY THE KEY                                                        
*                                                                               
         SPACE 1                                                                
DISKEY   NTR1                                                                   
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         L     R3,AIO              LOCATION OF THE RECORD                       
         USING TLBAD,R3                                                         
         MVC   TEMPDATE,TLBADATE   DATE                                         
         XC    TEMPDATE,=X'FFFFFF'                                              
         GOTO1 DATCON,DMCB,(1,TEMPDATE),(8,SBCPER)                              
         OI    SBCPERH+6,X'80'                                                  
*                                                                               
         MVC   SBCCURR(1),TLBACURR    CURRENCY                                  
         CLI   SBCCURR,C'U'                                                     
         BNE   DK10                                                             
         MVC   SBCCURR,=C'US    '                                               
         B     DK20                                                             
*                                                                               
DK10     CLI   SBCCURR,C'C'                                                     
         BNE   DK15                                                             
         MVC   SBCCURR,=C'CANADA'                                               
         B     DK20                                                             
*                                                                               
DK15     MVC   SBCCURR,=C'EUROS '                                               
*                                                                               
DK20     OI    SBCCURRH+6,X'80'                                                 
*                                                                               
         MVC   SBCEMP,TLBAEMP      EMPLOYER                                     
         OI    SBCEMPH+6,X'80'                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'88',SBCEMP),SBCEMPNH                      
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   SBCTYPE,=C'DUE'                                                  
         OI    SBCTYPEH+6,X'80'                                                 
*                                                                               
         MVI   TYPE,C'D'                                                        
         XC    STARTDTP,STARTDTP                                                
         LA    R2,SBCSTRTH         START AT A PARTICULAR DATE                   
         CLI   5(R2),0                                                          
         BE    DK30                                                             
         GOTO1 DATVAL,DMCB,(0,8(R2)),STARTDTE                                   
         GOTO1 DATCON,DMCB,(0,STARTDTE),(1,STARTDTP)                            
*                                                                               
DK30     DS    0H                                                               
         CLI   CONT,C'Y'                                                        
         BE    DKX                                                              
         XC    TOT1(TOTS),TOT1     CLEAR ALL TOTALS FOR NEW RECORD              
*                                                                               
DKX      MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        DISPLAY A BALANCE RECORD                                               
*                                                                               
         SPACE 1                                                                
DISREC   NTR1                                                                   
         MVC   KEYSAVE,KEY         SET KEYSAVE FOR SEQ READ/COMPARE             
         TWAXC SBCDATEH                                                         
         MVC   SBCDCHG,SPACES                                                   
         MVC   SBCTCHG,SPACES                                                   
         OI    SBCDCHGH+6,X'80'    TRASMIT                                      
         OI    SBCTCHGH+6,X'80'    TRASMIT                                      
*                                                                               
         L     R4,AIO              FIND ACTIVITY ELEMENT                        
         USING TAACD,R4                                                         
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR05                                                             
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(5,SBCDCHG)                             
         OI    SBCDCHGH+6,X'80'    TRASMIT                                      
         GOTO1 TIMECON,DMCB,TAACCTIM,TAACCDTE,(8,SBCTCHG)                       
         OI    SBCTCHGH+6,X'80'    TRASMIT                                      
*                                                                               
DR05     L     R4,AIO              FIND CORRECT ELEMENTS                        
         LA    R2,SBCDATEH                                                      
         USING TABAD,R4                                                         
         MVI   ELCODE,TABAELQ                                                   
         GOTO1 GETL,DMCB,(1,TYPE)                                               
         BNE   DR47                                                             
         L     R4,TGELEM           ADDRESS OF ELEMENT                           
*                                                                               
DR07     LA    R2,SBCDATEH                                                      
         CLI   TYPE,C'D'           DUE DATE ANALYSIS                            
         BNE   DR10                                                             
         MVC   SBCDTNM,=C'Due    '                                              
         B     DR20                                                             
*                                                                               
DR10     MVC   SBCDTNM,=C'Invoice'                                              
*                                                                               
DR20     OI    SBCDTNMH+6,X'80'                                                 
         CLC   TABATYPE,TYPE       IS THIS THE REQUESTED TYPE                   
         BNE   DR50                                                             
*                                                                               
         LR    R5,R4               SAVE ADDRESS OF LAST ELEMENT                 
         LA    R1,SBCLLIN          LAST LINE                                    
         CR    R2,R1                                                            
         BL    DR30                                                             
         B     MEXIT                                                            
*                                                                               
DR30     OC    STARTDTP,STARTDTP   START LISTING FROM THIS DATE                 
         BZ    DR35                                                             
         CLC   TABADATE,STARTDTP                                                
         BL    DR45                                                             
*                                                                               
DR35     GOTO1 DATCON,DMCB,(1,TABADATE),(4,TDATE)                               
         MVC   8(5,R2),TDATE       1ST LOCATION TO PRINT TO                     
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)            POINT TO FIRST AMOUNT FIELD                  
         AR    R2,R1                                                            
*                                                                               
         LR    RF,R2               PRINT OUT AMOUNTS                            
         BAS   RE,OUTAMT                                                        
         LA    R3,6                BUMP TO NEXT LINE                            
*                                                                               
DR40     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,DR40                                                          
*                                                                               
DR45     BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    DR20                                                             
*                                                                               
DR47     MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                 CHECK IF NEXT RECORD IS PART                 
         CLC   KEY(TLBASEQ-TLBAD),KEYSAVE                                       
         BNE   DR50                OF SAME 'LOGICAL' RECORD                     
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              IF YES - GET IT & DISPLAY IT                 
         L     R4,AIO              FIND CORRECT ELEMENTS                        
         USING TABAD,R4                                                         
         MVI   ELCODE,TABAELQ                                                   
         GOTO1 GETL,DMCB,(1,TYPE)                                               
         BNE   DR47                                                             
         L     R4,TGELEM           ADDRESS OF ELEMENT                           
         LR    R5,R4                                                            
         LA    R1,SBCLLIN          LAST LINE                                    
         CR    R2,R1                                                            
         BL    DR07                                                             
         B     DR57                                                             
*                                                                               
DR50     LA    R1,SBCLLINH         LAST LINE                                    
         CR    R2,R1                                                            
         BL    DR60                                                             
         BE    DR67                                                             
         LA    R2,SBCLLINH         LAST LINE - ERASE IT                         
         MVC   8(5,R2),SPACES      ERASE DATE & AMOUNTS                         
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         LA    R3,6                7 FIELDS                                     
*                                                                               
DR55     MVC   8(11,R2),SPACES                                                  
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         BCT   R3,DR55                                                          
*                                                                               
DR57     LR    R4,R5               RESTORE ADDRESS OF LAST ELEMENT READ         
         BAS   RE,SUBAMT           SUBTRACT AMOUNT FROM TOTALS                  
         B     MEXIT                                                            
*                                                                               
DR60     LA    R3,7                BUMP TO NEXT LINE                            
*                                                                               
DR65     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,DR65                                                          
*                                                                               
DR67     MVC   8(5,R2),=C'TOTAL'                                                
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         LR    RF,R2               TOTAL LINE                                   
         BAS   RE,TOTOUT           PRINT OUT TOTAL LINE                         
         XC    TOT1(TOTS),TOT1     CLEAR ALL TOTALS FOR NEW RECORD              
         MVC   SBCSTRT,SPACES                                                   
         MVI   SBCSTRTH+5,0                                                     
         OI    SBCSTRTH+6,X'80'                                                 
         MVI   CONT,C'N'                                                        
         NI    SBCPERH+4,X'DF'                                                  
         B     DRX                                                              
*                                                                               
DR70     DS    0H                                                               
DRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        EXIT WITH CONTINUE MESSAGE                                             
*                                                                               
         USING TABAD,R4                                                         
MEXIT    GOTO1 DATCON,DMCB,(1,TABADATE),(5,TDATE)                               
         GOTO1 DATCON,DMCB,(1,TABADATE),(1,STARTDTP)                            
         LA    R2,SBCSTRTH         EXIT WITH 'MORE' MESSAGE                     
         MVC   8(8,R2),TDATE                                                    
         MVI   5(R2),8             LENGTH OF INPUT                              
         OI    6(R2),X'80'                                                      
         MVI   CONT,C'Y'                                                        
         B     MOREXIT                                                          
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        PRINT OUT REPORT                                                       
*                                                                               
         SPACE 1                                                                
*                                                                               
PRREC    NTR1                                                                   
         LA    R2,P                                                             
         USING PLINED,R2                                                        
*                                                                               
PR10     GOTO1 DATCON,DMCB,(1,THEPDTE),(5,PRDATE)                               
         XC    SVKEY(SVLEN),SVKEY  CLEAR SAVED VALUES                           
         XC    KEY,KEY             SET KEY                                      
         LA    R1,KEY                                                           
         USING TLBAD,R1                                                         
         MVI   TLBACD,TLBACDQ                                                   
         MVC   TEMPDATE,THEPDTE    REQUESTED DATE - COMPLEMENTED                
         XC    TEMPDATE,=X'FFFFFF'                                              
         MVC   TLBADATE,TEMPDATE                                                
         MVI   LENKEY,TLBACURR-TLBAD-1                                          
         OC    FILTCURR,FILTCURR                                                
         BZ    PR15                                                             
         MVI   LENKEY,TLBAEMP-TLBAD-1                                           
         MVC   TLBACURR,FILTCURR   SET CURRENCY                                 
         OC    FILTEMP,FILTEMP                                                  
         BZ    PR15                                                             
         MVC   TLBAEMP,FILTEMP     SET EMPLOYER                                 
         MVI   LENKEY,TLBASEQ-TLBAD-1                                           
*                                                                               
PR15     GOTO1 HIGH                REQUESTED DAY'S BAL RECORD                   
         B     PR18                (OR NEXT CURR/EMP RECORD)                    
*                                                                               
PR17     GOTO1 SEQ                                                              
*                                                                               
PR18     ZIC   R3,LENKEY                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   PR105                                                            
*                                                                               
PR20     LA    R1,KEY              SET R1 TO KEY                                
         MVC   SVKEY,KEY                                                        
         MVC   SVEMP,TLBAEMP       SET SAVED VALUES                             
         MVC   SVCURR,TLBACURR                                                  
         XC    TOT1(TOTS),TOT1     CLEAR ALL TOTALS FOR NEW RECORD              
         DROP  R1                                                               
*                                                                               
         MVI   NXTBYTE,C'N'                                                     
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET BALANCE RECORD                           
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   SVEMPNMH,24         SET 'LENGTH'                                 
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'8C',SVEMP),SVEMPNMH                       
*                                  GET NEW EMPLOYER NAME                        
         MVI   FORCEHED,C'Y'       AND FORCE A PAGE EJECT                       
*                                                                               
PR22     MVC   AIO,AIO1            RESET IO AREA                                
         L     R4,AIO                                                           
         USING TABAD,R4                                                         
         MVI   ELCODE,TABAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PR17                GET NEXT CURR/EMP                            
*                                                                               
PR25     CLI   TABATYPE,TABATDUE   IF NO DUE DATE ANALYSIS AT ALL               
         BNE   PR40                   DON'T PRINT TITLE                         
         MVC   P1TITLE(17),=C'DUE DATE ANALYSIS'                                
         LA    R2,P2                                                            
         MVC   P1TITLE(17),=17X'BF'                                             
         BAS   RE,SPOOLIT                                                       
         BAS   RE,SPOOLIT          LEAVE A BLANK LINE                           
         LA    R2,P                                                             
         MVC   P1TYPE(12),=C'INVOICES DUE'                                      
*                                                                               
PR30     CLI   TABATYPE,TABATDUE   DUE DATE ANALYSIS                            
         BNE   PR35                                                             
         GOTO1 DATCON,DMCB,(1,TABADATE),(4,P1DATE)                              
         LA    RF,P1PINV           1ST LOCATION TO PRINT TO                     
         LA    R1,P1LEN                                                         
         STH   R1,ADDLEN                                                        
         BAS   RE,OUTAMT                                                        
         BAS   RE,SPOOLIT                                                       
         BAS   RE,NEXTEL                                                        
         BE    PR30                                                             
         MVI   NXTBYTE,C'N'                                                     
         BAS   RE,NXTREC           FIND NEXT PHYSICAL REC                       
         L     R4,FULL             RESET R4 WITH A(ELEMENT)                     
         BE    PR30                                                             
*                                                                               
PR35     BAS   RE,SPOOLIT                                                       
         LA    R2,P                                                             
         MVC   P1TITLE(14),=C'*** TOTALS ***'                                   
*                                                                               
         LA    RF,P1PINV           1ST LOCATION TO PRINT TO                     
         LA    R1,P1LEN                                                         
         STH   R1,ADDLEN                                                        
         BAS   RE,TOTOUT           PRINT OUT TOTAL LINE                         
         BAS   RE,SPOOLIT                                                       
         BAS   RE,SPOOLIT                                                       
*                                                                               
PR40     CLI   TABATYPE,TABATINV   IF NO INV DATE ANALYSIS AT ALL               
         BNE   PR70                GET NEXT CURR/EMP                            
*                                                                               
PR45     BAS   RE,BXMID                                                         
         XC    TOT1(TOTS),TOT1                                                  
         BAS   RE,SPOOLIT          LEAVE A BLANK LINE                           
         MVC   P1TITLE(21),=C'INVOICE DATE ANALYSIS'                            
         LA    R2,P2                                                            
         MVC   P1TITLE(21),=21X'BF'                                             
         BAS   RE,SPOOLIT                                                       
         BAS   RE,SPOOLIT                                                       
         LA    R2,P                                                             
         MVC   P1TYPE(12),=C'INVOICE DATE'                                      
*                                                                               
PR50     CLI   TABATYPE,TABATINV   INVOICE DATE ANALYSIS                        
         BNE   PR60                                                             
         GOTO1 DATCON,DMCB,(1,TABADATE),(4,P1DATE)                              
         LA    RF,P1PINV           1ST LOCATION TO PRINT TO                     
         LA    R1,P1LEN                                                         
         STH   R1,ADDLEN                                                        
         BAS   RE,OUTAMT                                                        
         BAS   RE,SPOOLIT                                                       
         BAS   RE,NEXTEL                                                        
         BE    PR50                                                             
         MVI   NXTBYTE,C'N'                                                     
         BAS   RE,NXTREC           FIND NEXT PHYSICAL REC                       
         L     R4,FULL             RESET R4 WITH A(ELEMENT)                     
         BE    PR50                                                             
*                                                                               
PR60     BAS   RE,SPOOLIT                                                       
         LA    R2,P                                                             
         MVC   P1TITLE(14),=C'*** TOTALS ***'                                   
         LA    RF,P1PINV           1ST LOCATION TO PRINT TO                     
         LA    R1,P1LEN                                                         
         STH   R1,ADDLEN                                                        
         BAS   RE,TOTOUT           PRINT OUT TOTAL LINE                         
         BAS   RE,SPOOLIT                                                       
*                                                                               
PR70     CLI   NXTBYTE,C'Y'        IF WE GOT NEXT RECORD                        
         BE    PR18                SEE IF REQUESTED                             
         B     PR17                ELSE GET NEXT CURR/EMP                       
*                                                                               
PR105    CLC   PENDDTE,THEPDTE     IF LAST REQUESTED DATE                       
         BE    PR110                                                            
         GOTO1 DATCON,DMCB,(1,THEPDTE),(0,THEEDTE)                              
         GOTO1 ADDAY,DMCB,THEEDTE,TEMPEDTE,1                                    
         GOTO1 DATCON,DMCB,(0,TEMPEDTE),(1,THEPDTE)                             
         MVI   FORCEHED,C'Y'                                                    
         B     PR10                GET NEXT DAY & GO BACK TO BEGINING           
*                                                                               
PR110    TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    PRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
PRX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*        FIND NEXT 'PHYSICAL' RECORD TO 'LOGICAL' RECORD                        
*                                                                               
NXTREC   NTR1                                                                   
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                RESTORE READ SEQUENCE                        
         MVC   KEYSAVE,KEY         SET KEYSAVE FOR SEQ READ/COMPARE             
         GOTO1 SEQ                                                              
         CLC   KEY(TLBASEQ-TLBAD),KEYSAVE                                       
         BE    NXT10                                                            
         MVI   NXTBYTE,C'Y'                                                     
         B     NO                                                               
*                                                                               
NXT10    MVC   SVKEY,KEY           RESET SVKEY                                  
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET BALANCE RECORD                           
         L     R4,AIO                                                           
         USING TABAD,R4                                                         
         MVI   ELCODE,TABAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         ST    R4,FULL             RETURN A(ELEMENT) TO CALLER                  
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*        OUTPUT 5 AMOUNTS TO PRINT LINE                                         
*        RF = A(OUTPUT)                                                         
*        ADDLEN = LENGTH TO NEXT PRINTED FIELD                                  
*                                                                               
         USING TABAD,R4                                                         
OUTAMT   NTR1                                                                   
         XC    TOTAL,TOTAL                                                      
         LA    R3,TABABPRE         START OF AMOUNTS                             
         DROP  R4                                                               
         LA    R5,TOT1             A(TOTALS)                                    
         LA    R4,3                                                             
*                                                                               
OUT10    L     R6,0(R3)            AMOUNT                                       
         BAS   RE,EDITIT                                                        
         L     R1,TOTAL                                                         
         AR    R1,R6               DATE TOTAL = A + B + C - D - E               
         ST    R1,TOTAL                                                         
*                                                                               
         L     R1,0(R5)            RESTORE AMOUNT                               
         AR    R1,R6               ADD TO COLUMN TOTALS                         
         ST    R1,0(R5)                                                         
*                                                                               
         LA    R3,4(R3)            NEXT AMOUNT                                  
         LA    R5,4(R5)            NEXT TOTAL                                   
         CLI   ACTNUM,ACTREP                                                    
         BNE   OUT15                                                            
         LH    R1,ADDLEN                                                        
         AR    RF,R1                                                            
         B     OUT17                                                            
*                                                                               
OUT15    ZIC   R1,0(RF)                                                         
         AR    RF,R1               NEXT FIELD                                   
*                                                                               
OUT17    BCT   R4,OUT10                                                         
*                                                                               
         LA    R4,2                LAST 2 AMOUNTS MUST BE SUBTRACTED            
*                                                                               
OUT20    L     R6,0(R3)            AMOUNT                                       
         BAS   RE,EDITIT                                                        
         L     R1,TOTAL                                                         
         SR    R1,R6               DATE TOTAL = A + B + C - D - E               
         ST    R1,TOTAL                                                         
*                                                                               
         L     R1,0(R5)            RESTORE AMOUNT                               
         AR    R1,R6               ADD TO COLUMN TOTALS                         
         ST    R1,0(R5)                                                         
*                                                                               
         LA    R3,4(R3)            NEXT AMOUNT                                  
         LA    R5,4(R5)            NEXT TOTAL                                   
         CLI   ACTNUM,ACTREP                                                    
         BNE   OUT30                                                            
         LH    R1,ADDLEN                                                        
         AR    RF,R1                                                            
         B     OUT40                                                            
*                                                                               
OUT30    ZIC   R1,0(RF)                                                         
         AR    RF,R1               NEXT FIELD                                   
*                                                                               
OUT40    BCT   R4,OUT20                                                         
*                                                                               
         L     R6,TOTAL            TOTAL                                        
         BAS   RE,EDITIT                                                        
         L     R1,TOT6             TOTAL OF DATE TOTALS                         
         AR    R1,R6                                                            
         ST    R1,TOT6                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SUBTRACT AMOUNTS FROM TOTAL                                            
*                                                                               
         USING TABAD,R4                                                         
SUBAMT   NTR1                                                                   
         LA    R3,TABABPRE         START OF AMOUNTS                             
         DROP  R4                                                               
         LA    R5,TOT1             A(TOTALS)                                    
         LA    R4,3                                                             
*                                                                               
SUB10    L     R6,0(R3)            AMOUNT                                       
         L     R1,TOTAL                                                         
         SR    R1,R6               DATE TOTAL = A + B + C - D - E               
         ST    R1,TOTAL                                                         
*                                                                               
         L     R1,0(R5)            RESTORE AMOUNT                               
         SR    R1,R6               SUB FROM COLUMN TOTALS                       
         ST    R1,0(R5)                                                         
*                                                                               
         LA    R3,4(R3)            NEXT AMOUNT                                  
         LA    R5,4(R5)            NEXT TOTAL                                   
         BCT   R4,SUB10                                                         
*                                                                               
         LA    R4,2                LAST 2 AMOUNTS MUST BE ADDED                 
*                                                                               
SUB20    L     R6,0(R3)            AMOUNT                                       
         L     R1,TOTAL                                                         
         AR    R1,R6               DATE TOTAL = A + B + C - D - E               
         ST    R1,TOTAL                                                         
*                                                                               
         L     R1,0(R5)            RESTORE AMOUNT                               
         SR    R1,R6               SUB FROM COLUMN TOTALS                       
         ST    R1,0(R5)                                                         
*                                                                               
         LA    R3,4(R3)            NEXT AMOUNT                                  
         LA    R5,4(R5)            NEXT TOTAL                                   
         BCT   R4,SUB20                                                         
*                                                                               
         L     R6,TOTAL            TOTAL                                        
         L     R1,TOT6             TOTAL OF DATE TOTALS                         
         SR    R1,R6                                                            
         ST    R1,TOT6                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINT OUT TOTAL LINE                                                   
*        R2 = A(PRINT LINE)                                                     
*        RF = A(OUTPUT)                                                         
*        ADDLEN = LENGTH TO NEXT PRINTED FIELD                                  
*                                                                               
TOTOUT   NTR1                                                                   
*        CLI   ACTNUM,ACTREP                                                    
*        BNE   TOT05                                                            
*        MVC   0(14,R2),=C'*** TOTALS ***'                                      
*                                                                               
TOT05    LA    R3,TOT1             1ST TOTAL                                    
         LA    R5,6                6 TOTALS TO PRINT                            
*                                                                               
TOT10    L     R6,0(R3)                                                         
         BAS   RE,EDITIT                                                        
         LA    R3,4(R3)            NEXT AMOUNT                                  
         CLI   ACTNUM,ACTREP                                                    
         BNE   TOT20                                                            
         LH    R1,ADDLEN                                                        
         AR    RF,R1                                                            
         B     TOT30                                                            
*                                                                               
TOT20    ZIC   R1,0(RF)                                                         
         AR    RF,R1               NEXT FIELD                                   
*                                                                               
TOT30    BCT   R5,TOT10                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SEND LINE TO SPOOL                                                     
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SPACING,1                                                        
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        EDIT                                                                   
*                                                                               
EDITIT   NTR1                                                                   
         CLI   ACTNUM,ACTREP                                                    
         BNE   ED10                                                             
         EDIT  (R6),(12,(RF)),2,FLOAT=-,ZERO=BLANK                              
         B     EDX                                                              
*                                                                               
ED10     EDIT  (R6),(11,8(RF)),2,FLOAT=-,ZERO=BLANK                             
         OI    6(RF),X'80'                                                      
*                                                                               
EDX      B     XIT                                                              
         SPACE 2                                                                
*                                                                               
         USING SCAND,R3                                                         
ADDISP   NTR1                                                                   
         ZIC   RF,SCLEN1           L'LHS                                        
         ZIC   RE,SCLEN2           + L'RHS                                      
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         AH    RE,=H'1'            + '=' SIGN IF THERE IS A RIGHT HALF          
         LA    RF,1(RF,RE)         + DELIMITER                                  
         AH    RF,HALF             + L'SO FAR                                   
         STH   RF,HALF             = CURRENT DISPLACEMENT INTO FIELD            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        HEADHOOK                                                               
*                                                                               
MYHOOK   NTR1                                                                   
         MVC   HEAD3(8),=C'CURRENCY'     CURRENCY                               
         MVC   HEAD3+10(9),=CL9'US$'                                            
         CLI   SVCURR,C'U'                                                      
         BE    MY10                                                             
         MVC   HEAD3+10(9),=C'CANADIAN$'                                        
         CLI   SVCURR,C'C'                                                      
         BE    MY10                                                             
         MVC   HEAD3+10(9),=C'EUROS    '                                        
*                                                                               
MY10     MVC   HEAD4(8),=C'EMPLOYER'                                            
         MVC   HEAD4+10(L'SVEMP),SVEMP                                          
         MVC   HEAD4+15(16),SVEMPNME                                            
         MVC   HEAD4+60(L'PRDATE),PRDATE                                        
*                                                                               
         USING BOXD,R3                                                          
         L     R3,ABOX             NOW HANDLE BOXES                             
         LTR   R3,R3                                                            
         BZ    MYHX                                                             
*                                                                               
         LA    R1,BXHOOK                                                        
         ST    R1,BOXHOOK                                                       
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         LA    R2,6                                                             
         LA    R2,BOXROWS-1(R2)    SET TOP OF BOX                               
         MVI   0(R2),C'T'                                                       
         LA    R2,5(R2)                                                         
         MVI   0(R2),C'M'          SET MIDDLE LINE                              
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS          SET R2                                       
         USING PLINED,R2                                                        
         MVI   COL1,C'L'                                                        
         MVI   COL6,C'R'                                                        
         MVI   COL4,C'C'                                                        
*                                                                               
MYHX     B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*              ROUTINE TO POP IN A BOX MIDDLE LINE                              
*                                                                               
         SPACE 1                                                                
BXMID    NTR1                                                                   
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         LTR   R3,R3                                                            
         BZ    BXX                                                              
*                                                                               
         MVC   BOXROWS,SPACES      CLEAR ROWS/COLS                              
         ZIC   RF,LINE                                                          
         CH    RF,=H'13'           JUST STARTED NEW PAGE - DON'T PUT            
         BNH   BXX                      MIDDLE LINE                             
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'          SET MIDDLE ROW                               
*                                                                               
         MVI   BOXINIT,0                                                        
         BAS   RE,SPOOLIT                                                       
*                                                                               
BXX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERXIT                                                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERXIT                                                            
*                                                                               
MOREXIT  MVI   MYMSGNO1,36                                                      
         OI    GENSTAT2,USGETTXT                                                
         B     ERXIT                                                            
*                                                                               
NTFND    MVI   ERROR,NOTFOUND                                                   
         B     ERXIT                                                            
*                                                                               
ERXIT    GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  R7                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*              BOX HOOK ROUTINE                                                 
*                                  P1, BYTE 0 = ROW CHARACTER                   
*                                  P1, BYTES 1-3 = A(PRINT LINE)                
*                                  P2, BYTE 0 = LINE NUMBER                     
*                                  P2, BYTES 1-3 = A(BOXD)                      
         DS    0D                                                               
BXHOOK   NMOD1 0,BOXHOOK                                                        
         LM    R2,R3,0(R1)         =A(ABOX)                                     
         USING BOXD,R3                                                          
         LTR   R3,R3                                                            
         BZ    BXIT                                                             
         LA    R4,BOXCOLS          SET R4                                       
         USING PLINED,R4                                                        
*                                                                               
         CLI   4(R1),7             ON 7TH LINE INSERT MIDLINE                   
         BNE   BXH10                                                            
         MVI   BOXROWS+7,C'M'                                                   
         B     BXIT                                                             
*                                                                               
BXH10    CLI   4(R1),8             ON EIGHTH LINE FIX COLUMNS                   
         BNE   BXH20                                                            
         MVI   BOXCOLS+126,C'R'                                                 
         MVI   COL2,C'C'                                                        
         MVI   COL3,C'C'           SET NEW COLUMNS TOO                          
         MVI   COL4,C'C'                                                        
         MVI   COL5,C'C'                                                        
         MVI   COL6,C'C'                                                        
         B     BXIT                                                             
*                                                                               
BXH20    CLI   4(R1),11            ON 11TH LINE CHANGE BOX                      
         BNE   BXIT                                                             
         MVI   BOXCOLS+4,C'L'      AT LINE 11 MOVE LHS & RHS OVER               
         MVI   COL1,C'C'           OLD LHS & RHS BECOME CENTERS                 
*                                                                               
BXIT     XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*        SPECS                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,107,REPORT                                                    
         SSPEC H1,123,PAGE                                                      
         SSPEC H2,107,REQUESTOR                                                 
         SPACE 1                                                                
         SSPEC H1,51,C'DAILY FILE BALANCING SUMMARY'                            
         SSPEC H2,51,28X'BF'                                                    
         SPACE 1                                                                
         SSPEC H7,48,C'INVOICES'                                                
         SSPEC H7,90,C'PAYROLL'                                                 
         SPACE 1                                                                
         SSPEC H9,117,C'BILLED'                                                 
         SSPEC H9,31,C'PREVIOUS          CURRENT        CANCELLED'              
         SSPEC H9,82,C'PREVIOUS          CURRENT'                               
         SPACE 1                                                                
         SSPEC H10,115,C'LESS PAID'                                             
         SSPEC H10,82,C'INVOICES         INVOICES'                              
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL5                                                              
P1TITLE  DS    CL21                                                             
         ORG   P1TITLE                                                          
P1TYPE   DS    CL12                                                             
         DS    CL1                                                              
P1DATE   DS    CL5                                                              
         DS    CL3                                                              
COL1     DS    CL1                                                              
P1PINV   DS    CL12                                                             
         DS    CL2                                                              
COL2     DS    CL1                                                              
         DS    CL2                                                              
P1LEN    EQU   *-P1PINV            LENGTH BETWEEN FIELDS                        
P1CINV   DS    CL12                                                             
         DS    CL2                                                              
COL3     DS    CL1                                                              
         DS    CL2                                                              
P1CAN    DS    CL12                                                             
         DS    CL2                                                              
COL4     DS    CL1                                                              
         DS    CL2                                                              
P1PPAY   DS    CL12                                                             
         DS    CL2                                                              
COL5     DS    CL1                                                              
         DS    CL2                                                              
P1CPAY   DS    CL12                                                             
         DS    CL2                                                              
COL6     DS    CL1                                                              
         DS    CL2                                                              
P1TOT    DS    CL12                                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR4ED                                                       
         EJECT                                                                  
         ORG   SBCWORK                                                          
         SPACE 1                                                                
CONT     DC    C'N'                       CONTINUE SELECT SCREEN                
         SPACE 1                                                                
         DS    0F                                                               
TOT1     DS    F                        TOTALS                                  
TOT2     DS    F                                                                
TOT3     DS    F                                                                
TOT4     DS    F                                                                
TOT5     DS    F                                                                
TOT6     DS    F                                                                
TOTAL    DS    F                                                                
TOTS     EQU   *-TOT1                                                           
*                                                                               
ADDLEN   DS    H                   LENGTH BETWEEN PRINT FIELDS                  
*                                                                               
NXTBYTE  DS    CL1                 FLAG - READ NEXT RECORD                      
LENKEY   DS    XL1                 LENGTH TO COMPARE KEYS                       
PENDDTE  DS    PL3                 END DATE PWOS                                
PSTRDTE  DS    PL3                 START DATE PWOS                              
THEPDTE  DS    PL3                 LOOP DATE PWOS                               
THEEDTE  DS    CL6                           EBCDIC                             
PRDATE   DS    CL8                           PRINTED                            
TEMPDATE DS    PL3                 TEMP DATE PWOS                               
TEMPEDTE DS    CL6                           EBCDIC                             
FILTCURR DS    CL1                 FILTER ON CURRENCY                           
FILTEMP  DS    CL3                 FILTER ON EMPLOYER                           
KEYCHG   DS    CL1                 FLAG - KEY CHANGED                           
TYPE     DS    CL1                 ANALYSIS TYPE                                
STARTDTP DS    PL3                 START DISPLAYING FROM XXX DATE               
STARTDTE DS    CL6                       YYMMDD                                 
TDATE    DS    CL5                                                              
SVKEY    DS    CL48                SAVED KEY                                    
SVCURR   DS    CL1                 SAVED CURRENCY                               
SVEMP    DS    CL3                       EMPLOYER                               
SVEMPNMH DS    CL8                       FAKE HEADER                            
SVEMPNME DS    CL16                      EMPLOYER NAME                          
SVLEN    EQU   *-SVKEY                                                          
LCURR    DS    CL1                 LAST CURRENCY                                
LEMP     DS    CL3                      EMPLOYER                                
*                                                                               
         EJECT                                                                  
*DDPERVAL                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*TAGENWORKD                                                                     
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004TAGEN4E   06/03/10'                                      
         END                                                                    
