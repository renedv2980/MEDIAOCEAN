*          DATA SET BUFIL14    AT LEVEL 006 AS OF 05/01/02                      
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
* EDIT COPY OUTLINE LEVEL LIMIT                                                 
*                                                                               
VKEY12   GOTO1 VGETFLD,PARAS,PCYLEVH                                            
         CLI   FLDH+5,0                                                         
         BE    VKEY14                                                           
         MVI   ERROR,NOTNUM                                                     
         TM    FLDH+4,X'08'        TEST FOR A NUMBER                            
         BZ    SPERR                                                            
         MVI   ERROR,INVALID                                                    
         LTR   R0,R0               TEST 1-MAXIMUM OUTLINE LEVEL                 
         BZ    SPERR                                                            
         CH    R0,=Y(MAXOUTS)                                                   
         BH    SPERR                                                            
         STC   R0,LEVLIM                                                        
*                                                                               
* EDIT TO PLAN NAME                                                             
*                                                                               
VKEY14   MVI   NDDELRSW,NO         TURN OFF PASS DELETES SWITCH                 
         GOTO1 VGETFLD,PARAS,PCYTNAMH                                           
         CLI   FLDH+5,0                                                         
         BE    *+10                                                             
         MVC   TOPLNNAM,FLD        NEW PLAN NAME                                
*                                                                               
* EDIT TO PLAN PERIOD                                                           
*                                                                               
VKEY16   GOTO1 VGETFLD,PARAS,PCYTPERH                                           
         CLI   FLDH+5,0                                                         
         BNE   VKEY17                                                           
         CLC   TOFISCAL,FRFISCAL   TEST FOR DIFFERENT FISCAL YEARS              
         BE    VKEYX               NO-DON'T NEED NEW FISCAL PERIOD              
         MVI   ERROR,MISSING                                                    
         B     SPERR                                                            
*                                                                               
VKEY17   MVI   ERROR,INVDATE       VALIDATE THE PERIOD                          
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
VKEY18   CLI   TOPLNST,0           TEST FOR OPEN-ENDED PLAN                     
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
* COPY THE PLAN AND ITS OUTLINES                                                
*                                                                               
COPY     CLI   OFFLINE,YES         TEST PROCESSING ON-LINE                      
         BNE   COPY1                                                            
         LA    R1,HEDSPECS         INITIALIZE FOR REPORT                        
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
COPY1    LA    RE,NODBLK2                                                       
         LA    RF,LENODBLK                                                      
         L     R0,ANODBLK                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0               MOVE NODBLK TO NODBLK2                       
*                                                                               
         LA    R5,NODBLK2          USE NODBLK2 TO POSITION 'TO' PLAN            
         MVC   NDIOA2,AIO3                                                      
         MVI   NDREREAD,YES        FORCE RE-READ OF HIGHER LEVELS               
         XC    NDHOOK,NDHOOK                                                    
         MVC   NODKEY2,TOPLNKEY                                                 
         GOTO1 VNODIO,DMCB,(R5),=C'HIGH',NODKEY2,0                              
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    COPY2               YES-ADD TO END OF PLANS                      
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   BEFCODE,NDLVCOD     EXTRACT NEXT HIGHER PLAN CODE                
*                                                                               
COPY2    L     R5,ANODBLK          POINT BACK TO ORIGINAL NODBLK                
         XC    NDHOOK,NDHOOK                                                    
         MVC   NODKEY,FRPLNKEY                                                  
         GOTO1 VNODIO,DMCB,(R5),=C'READ',NODKEY,0                               
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VGETVAL                                                          
         BAS   RE,PLAN             ADD NEW PLAN                                 
*                                                                               
COPY4    LA    R1,COPYHK           READ THE OUTLINES                            
         ST    R1,NDHOOK                                                        
*                                                                               
COPY5    MVI   NDSQBACK,3                                                       
         GOTO1 VNODIO,DMCB,(R5),=C'LSEQ',NODKEY,0                               
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COPY6    CLI   OFFLINE,YES                                                      
         BE    COPYX                                                            
*                                                                               
         MVC   CONHEAD(L'COPYMSG),COPYMSG                                       
         LA    R3,CONHEAD+L'COPYMSG                                             
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LH    R0,NOUTS                                                         
         EDIT  (R0),(5,0(R3)),ALIGN=LEFT                                        
         AR    R3,R0                                                            
         LA    R3,1(R3)                                                         
         MVC   0(14,R3),=C'OUTLINES ADDED'                                      
         LA    R2,CONACTH                                                       
         ST    R2,ACURFORC                                                      
*                                                                               
COPYX    B     XIT                                                              
         SPACE 2                                                                
* HOOK ROUTINE FOR COPYING OUTLINES                                             
*                                                                               
COPYHK   NTR1                                                                   
         CLI   NDMODE,NDPROC                                                    
         BNE   COPYHKX                                                          
*                                                                               
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCOUT     TEST FOR OUTLINE                             
         BZ    COPYHKX                                                          
         GOTO1 VGETVAL                                                          
         CLI   LEVLIM,0            TEST FOR LEVEL FILTER                        
         BE    COPYHK2             NO                                           
         CLC   OUTLEV,LEVLIM       TEST IF BELOW LIMIT                          
         BH    COPYHKX             YES                                          
*                                                                               
COPYHK2  BAS   RE,OUT                                                           
         CLI   OFFLINE,YES                                                      
         BNE   COPYHKX                                                          
         LA    R3,P                R3=A(PRINT LINE)                             
         USING PLIND,R3                                                         
         CLI   OUTLEV,1            TEST FOR FIRST LEVEL OUTLINE                 
         BNE   COPYHK4                                                          
         GOTO1 SPOOL,PARAS,(R8)    SKIP A LINE BEFORE PRINTING                  
*                                                                               
COPYHK4  ZIC   R1,OUTLEV           GET OUTLINE LEVEL                            
         BCTR  R1,0                DEVELOP INDEX INTO PRINT FIELD               
         SLL   R1,1                X 2 TO INDENT FOR LEVEL                      
         LA    RE,POUTCODE(R1)                                                  
         MVC   0(L'OUTCODE,RE),OUTCODE EXTRACT OUTLINE CODE                     
         OC    0(L'OUTCODE,RE),SPACES SPACE FILL FIELD                          
         LA    RE,POUTNAME(R1)      INDEX INTO DESCRIPTION                      
         MVC   0(L'OUTNAME,RE),OUTNAME                                          
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
COPYHKX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO ADD 'TO' PLAN RECORD                                           
*                                                                               
* AT ENTRY, NDIOA=A(FROM PLAN)                                                  
*                                                                               
PLAN     NTR1                                                                   
         L     R4,NDIOA                                                         
         ST    R4,AIO                                                           
         USING BURECD,R4                                                        
         MVC   BUKEY,NDKEY         INITIALIZE KEY                               
         LA    R6,BUFRSTEL                                                      
         SR    R0,R0                                                            
*                                                                               
PLAN2    CLI   0(R6),0             TEST FOR EOR                                 
         BE    PLAN6               YES                                          
*                                                                               
         CLI   0(R6),BUPLNELQ      TEST FOR PLAN ELEMENT                        
         BE    PLAN4               YES-KEEP IT                                  
         CLI   0(R6),BUFOVELQ      TEST FOR FISCAL OVERRIDE ELEMENT             
         BNE   *+14                NO                                           
         CLC   FRCLT,TOCLT         TEST COPYING TO SAME CLIENT                  
         BE    PLAN4               YES-KEEP THE FISCAL OVERRIDES                
         MVI   0(R6),X'FF'         NO-MARK OTHER ELEMENTS FOR DELETION          
*                                                                               
PLAN4    IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PLAN2                                                            
*                                                                               
PLAN6    GOTO1 HELLO,DMCB,(C'D',SYSFIL),(X'FF',(R4)),0                          
         MVI   ELCODE,BUPLNELQ                                                  
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING BUPLND,R6                                                        
         NI    BUPLNIND,X'FF'-BUPLNDAT TURN OFF DATA INDICATOR                  
         XC    BUPLNCNT,BUPLNCNT   ZERO OUTLINE COUNT                           
         MVI   BUPLNLOW,0          AND LOWEST OUTLINE LEVEL                     
         OC    TOPLNNAM,TOPLNNAM   TEST FOR NEW NAME                            
         BZ    *+10                                                             
         MVC   BUPLNNAM,TOPLNNAM                                                
         MVC   PLANCODE,TOPLAN     RESET PLAN CODE FOR PRINTING                 
         MVC   PLANNAM,TOPLNNAM    SET NEW NAME FOR PRINTING                    
         OC    TOPLNST,TOPLNST     TEST FOR NEW PERIOD                          
         BZ    PLAN8                                                            
         MVC   BUPLNST,TOPLNST                                                  
         MVC   BUPLNEND,TOPLNEND                                                
*                                                                               
PLAN8    GOTO1 ADDELEM                                                          
         GOTO1 VADDACTV                                                         
         LA    R5,NODBLK2          POINT TO SECOND NODIO BLOCK                  
         MVC   NDIOA,AIO                                                        
         MVC   NDIOA2,AIO3                                                      
         XC    NDHOOK,NDHOOK                                                    
         MVI   BYTE,C'B'                                                        
         LA    R0,L'PLANCODE                                                    
         LA    R3,BEFCODE                                                       
         OC    BEFCODE,BEFCODE     TEST ADD BEFORE SET                          
         BNZ   PLAN10              YES                                          
         MVI   BYTE,C'A'                                                        
         SR    R0,R0                                                            
         SR    R3,R3                                                            
*                                                                               
PLAN10   GOTO1 VNODIO,DMCB,(R5),(BYTE,=C'ADD'),TOPLNKEY,((R0),(R3)),0           
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   TOPLNDIR,NDLVKEY    SAVE DIRECTORY ENTRY                         
         MVC   TOPLNDA,NDLVDA      AND DISK ADDRESS                             
*                                                                               
PLANX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO ADD AN OUTLINE RECORD TO 'TO' PLAN                             
*                                                                               
* AT ENTRY, NDIOA=A(OUTLINE RECORD)                                             
*                                                                               
OUT      NTR1                                                                   
         L     R4,NDIOA                                                         
         ST    R4,AIO                                                           
         USING BURECD,R4                                                        
         MVC   BUKEY,NDKEY                                                      
         LA    R6,BUFRSTEL                                                      
         SR    R0,R0                                                            
*                                                                               
OUT2     CLI   0(R6),0                                                          
         BE    OUT4                                                             
*                                                                               
         CLI   0(R6),BUOUTELQ      KEEP OUTLINE ELEMENT                         
         BE    OUT3                                                             
         CLI   0(R6),BURULELQ      AND RULES ELEMENT                            
         BE    OUT3                                                             
         CLI   0(R6),BUINELQ       AND SCREEN INPUT ELEMENTS                    
         BE    OUT3                                                             
         CLI   0(R6),BUROPELQ      TEST FOR ROW OPERAND ELEMENT                 
         BE    OUT3                                                             
         CLI   0(R6),BUPOLELQ      OR POLISH FORMULA ELEMENT                    
         BE    OUT3                                                             
         CLI   0(R6),BUPTRELQ                                                   
         BE    OUT3                                                             
         MVI   0(R6),X'FF'                                                      
*                                                                               
OUT3     IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     OUT2                                                             
*                                                                               
OUT4     GOTO1 HELLO,DMCB,(C'D',SYSFIL),(X'FF',(R4)),0                          
         MVI   ELCODE,BUPTRELQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING BUPTRD,R6                                                        
         LA    R1,BUPOINT                                                       
         USING BUCRECD,R1                                                       
         MVC   BUCCLT,TOCLT        CHANGE PASSIVE POINTER                       
         MVC   BUCPRD,TOPRD                                                     
         MVC   BUCPLAN,TOPLAN                                                   
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 VADDACTV                                                         
*                                                                               
OUT6     MVC   NODKEY2,TOPLNKEY    INITIALIZE NEW OUTLINE KEY                   
         LA    R6,NODKEY2                                                       
         CLI   0(R6),C' '          TEST FOR END OF KEY                          
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
*                                                                               
         MVC   0(1,R6),NDDELIM     PLACE DELIMITER AFTER PLAN CODE              
         LA    R6,1(R6)            NOW ATTACH THE OUTLINE CODE(S)               
         LA    RE,NODKEY                                                        
         LA    R0,3                FIND START OF OUTLINE CODES                  
OUT7     CLC   NDDELIM,0(RE)       TEST FOR DELIMITER                           
         BE    *+12                YES                                          
         LA    RE,1(RE)                                                         
         B     *-14                                                             
*                                                                               
         LA    RE,1(RE)                                                         
         BCT   R0,OUT7                                                          
*                                                                               
OUT8     CLI   0(RE),C' '          TEST FOR END OF OUTLINE KEY                  
         BNH   OUT10                                                            
         MVC   0(1,R6),0(RE)                                                    
         LA    R6,1(R6)                                                         
         LA    RE,1(RE)                                                         
         B     OUT8                                                             
*                                                                               
OUT10    LA    R5,NODBLK2          POINT TO SECOND NODIO BLOCK                  
         MVC   NDIOA,AIO                                                        
         MVC   NDIOA2,AIO3                                                      
         XC    NDHOOK,NDHOOK       ADD TO END OF PLAN                           
         GOTO1 VNODIO,DMCB,(R5),(C'A',=C'ADD'),NODKEY2,0                        
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVI   ELCODE,BUPTRELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPTRD,R6                                                        
         LA    RE,PASSKEY          BUILD A PASSIVE POINTER                      
         USING BUCRECD,RE                                                       
         MVC   BUCKEY,BUPOINT      LOGICAL KEY                                  
         MVC   BUCCTL,BURCTL                                                    
         MVC   BUCDA,NDLVDA                                                     
         DROP  RE                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PASSKEY),PASSKEY                                           
         GOTO1 ADD                                                              
*                                                                               
OUT12    LH    R1,NOUTS            INCREMENT OUTLINE COUNT                      
         LA    R1,1(R1)                                                         
         STH   R1,NOUTS                                                         
         CLC   OUTLEV,LOWLEV       TEST IF THIS LEVEL IS LOWEST                 
         BL    *+10                                                             
         MVC   LOWLEV,OUTLEV                                                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    RE,KEY+(BUKDA-BUKEY)                                             
         MVC   0(L'BUKDA,RE),TOPLNDA GET THE PLAN RECORD                        
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,BUPLNELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPLND,R6                                                        
         MVC   BUPLNCNT,NOUTS                                                   
         MVC   BUPLNLOW,LOWLEV                                                  
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
OUTX     B     XIT                                                              
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
         MVI   25(R2),C'L'                                                      
         MVI   46(R2),C'C'                                                      
         MVI   77(R2),C'R'                                                      
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
*&&US*&& SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
*&&UK*&& SSPEC H1,2,C'ABS - ACCOUNT BUDGET SYSTEM'                              
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
         SSPEC H9,27,C'CODE'                                                    
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
LEVLIM   DS    X                   OUTLINE LEVEL LIMIT                          
BEFCODE  DS    CL(L'BUKCODE)       POSITIONING CODE FOR TO PLAN                 
*                                                                               
FRPLNKEY DS    CL(L'NODKEY)        FROM PLAN NODAL KEY                          
TOPLNKEY DS    CL(L'NODKEY)        TO PLAN NODAL KEY                            
TOPLNDIR DS    CL(L'BUKEY)         TO PLAN DIRECTORY KEY                        
TOPLNDA  DS    XL(L'BUKDA)         TO PLAN DISK ADDRESS                         
*                                                                               
NODKEY2  DS    CL(L'NODKEY)        SECOND NODAL KEY                             
PASSKEY  DS    CL(BUKLNQ)          PASSIVE POINTER AREA                         
LOWLEV   DS    X                   LOWEST LEVEL ON TO PLAN                      
NOUTS    DS    H                   N'OUTLINES ON TO PLAN                        
*                                                                               
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         SPACE 2                                                                
         ORG   TWA1USER                                                         
NODBLK2  DS    CL(LENODBLK)        SECOND NODIO BLOCK                           
NODBLK2X EQU   *                                                                
         ORG   NODBLK                                                           
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PLIND    DSECT                                                                  
         DS    CL26                                                             
POUTCODE DS    CL18                OUTLINE CODE                                 
         DS    CL3                                                              
POUTNAME DS    CL30                OUTLINE NAME                                 
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXPLAN  EQU   25                  MAXIMUM PLAN SIZE FOR ON-LINE COPY           
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006BUFIL14   05/01/02'                                      
         END                                                                    
