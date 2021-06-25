*          DATA SET ACREPXY02T AT LEVEL 106 AS OF 05/01/02                      
*PHASE ACXY02B                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE CASHVAL                                                                
*INCLUDE QSORT                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'USE BATCH ITEM RECORDS TO FIND 1R TRANS TO BE RE-ADDED'         
ACXY02   CSECT                                                                  
         PRINT NOGEN                                                            
         USING ACWORKD,RA                                                       
         USING ACXYD,RC                                                         
REC      USING SORTRECD,SORTREC                                                 
         NMOD1 0,**ACXY**,R9                                                    
         L     RA,0(,R1)                                                        
         LA    RC,SPACEND                                                       
         EJECT                                                                  
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
EXIT     XIT1                                                                   
*                                                                               
***********************************************************************         
* RUNFIRST                                                                      
***********************************************************************         
RUNF     ZAP   RECOCNT,=P'0'                                                    
         ZAP   DROTOT,=P'0'        OVERALL TOTAL                                
         ZAP   CROTOT,=P'0'        OVERALL TOTAL                                
         ZAP   TOTOINC,=P'0'                                                    
         XC    TABCOUNT,TABCOUNT                                                
         MVC   VTYPES(VTYPLNQ),EXTADDS                                          
         NI    BIT,X'FF'-SJLEDG                                                 
         BAS   RE,GETLDG           READ THE 1R LEDGER STRUCTURE                 
         MVI   BIT,0                                                            
         MVC   STDISP,=H'0'                                                     
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
         B     EXIT                                                             
*                                                                               
***********************************************************************         
* REQFIRST                                                                      
***********************************************************************         
*                                                                               
REQF     MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   RECCNT,=P'0'                                                     
         ZAP   TOT1R,=P'0'                                                      
                                                                                
         MVI   POSTFLAG,0                                                       
         XC    ID,ID               POTENTIAL POSTING FILE NAME                  
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'AA1'                                                  
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         BAS   RE,OPENWKR                                                       
*                                                                               
         USING TBARECD,R5                                                       
         LA    R5,DKEY                                                          
         XC    DKEY,DKEY                                                        
         MVI   TBAKTYP,TBAKTYPQ    SET KEY FOR TRANS BATCH RECORDS              
         MVC   TBAKCPY,RCCOMPFL                                                 
         MVI   HIT,0                                                            
         BAS   RE,HIGH                                                          
         B     RS01                                                             
*                                                                               
RS00     BAS   RE,SEQ                                                           
*                                                                               
RS01     LA    R5,DIR                                                           
         CLC   TBAKCPY,RCCOMPFL                                                 
         BNE   EXIT                                                             
         CLI   TBARECD,TBAKTYPQ    HAS TO BE TRANS BATCH RECORD                 
         BNE   EXIT                I WON'T BE BACK                              
*                                                                               
         OC    TBAKTSEQ,TBAKTSEQ   HEADER ?                                     
         BNZ   RS01AC              NO                                           
         MVI   HIT,0                                                            
         TM    TBAKHSTA,TBAHSUPD   IS IT MARKED UPDATED?                        
         BZ    RS00                                                             
         CLC   QUESTOR(4),SPACES                                                
         BE    RS01A                                                            
         CLC   TBAKBREF,QUESTOR                                                 
         BNE   RS00                                                             
*                                                                               
RS01A    CLI   TBAKBTYP,X'31'      TYPE 49                                      
         BNE   RS00                                                             
         OI    HIT,HITBATCH        YES                                          
         B     RS00                UPDATED                                      
*                                                                               
RS01AC   TM    HIT,HITBATCH                                                     
         BZ    RS00                                                             
         CLI   TBAKBTYP,X'31'                                                   
         BNE   RS00                                                             
         TM    TBAKESTA,TBAESLDE   ENTRY IF LOGICALLY DELETED?                  
         BO    RS00                SKIP                                         
*                                                                               
         NI    BIT,X'FF'-SJLEDG                                                 
         BAS   RE,GETLDG           READ THE 1R LEDGER STRUCTURE                 
         MVC   BATCHMOS,TBAKBMOS                                                
         MVC   BATCHCMO,BATCHMOS                                                
         OI    BATCHCMO,X'F0'                                                   
         OI    BATCHCMO+1,X'F0'                                                 
         CLI   BATCHMOS+1,X'10'                                                 
         BL    RS02                                                             
         MVI   BATCHCMO+1,C'A'                                                  
         BE    RS02                                                             
         MVI   BATCHCMO+1,C'B'                                                  
         CLI   BATCHMOS+1,X'11'                                                 
         BE    RS02                                                             
         MVI   BATCHCMO+1,C'C'                                                  
*                                                                               
RS02     MVC   BATCHCDE,TBAKBREF                                                
         MVC   DKEYSV,DIR          SAVE KEY FOR DIRECTORY RESET                 
         L     R3,AIO2                                                          
         BAS   RE,GET              GET BATCH RECORD                             
         L     R5,AIO2                                                          
*                                                                               
         CLI   QOPT2,C'T'          READ FOR TRANSACTIONS?                       
         BE    RS15                                                             
*                                                                               
         OI    BIT,OFFDPT          PROCESSING OFFICE/DEPT                       
         LA    R3,ACCNT                                                         
         MVC   ACCNT,SPACES                                                     
*                                                                               
         USING SFSELD,R2                                                        
         LA    R2,TBARFST                                                       
RS03     CLI   0(R2),0                                                          
         BE    RS50                                                             
         CLI   0(R2),SFSELQ        X'E1'                                        
         BE    RS08                                                             
*                                                                               
RS06     SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     RS03                                                             
*                                                                               
RS08     TM    BIT,OFFDPT                                                       
         BZ    RS09                                                             
         CLI   SFSFLDN,X'0B'       MUST BE THIS FIELD                           
         BNE   RS06                                                             
         LA    RE,SFSFIELD                                                      
         ZIC   RF,LN1RLEV1                                                      
         SHI   RF,1                                                             
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RE)                                                    
         LA    RE,1(RF,RE)                                                      
         LA    R3,1(RF,R3)                                                      
*                                                                               
         ZIC   RF,LN1RLEV2                                                      
         SHI   RF,1                                                             
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RE)                                                    
         LA    RE,1(RF,RE)                                                      
         LA    R3,1(RF,R3)                                                      
         NI    BIT,X'FF'-OFFDPT                                                 
         OI    BIT,SUBDPT                                                       
         B     RS06                GET THE NEXT E1 ELEMENT                      
*                                                                               
RS09     TM    BIT,SUBDPT          PROCESSING SUB-DEPT?                         
         BZ    RS10                                                             
         LA    RE,SFSFIELD                                                      
         ZIC   RF,SFSLN                                                         
         SHI   RF,(SFSLN1Q+1)                                                   
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RE)                                                    
         LA    RE,1(RF,RE)                                                      
         LA    R3,1(RF,R3)                                                      
         NI    BIT,X'FF'-SUBDPT                                                 
         OI    BIT,PERSON                                                       
         B     RS06                GET THE NEXT E1 ELEMENT                      
*                                                                               
RS10     TM    BIT,PERSON                                                       
         BZ    RS20                                                             
         LA    RE,SFSFIELD                                                      
         ZIC   RF,SFSLN                                                         
         SHI   RF,(SFSLN1Q+1)                                                   
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RE)                                                    
         NI    BIT,X'FF'-PERSON                                                 
*        CLC   ACCNT,=C'8FF2TP001853'  CATCH THE ONE RE-ADDED                   
*        BE    RS20A                   DON'T BOTHER CHECKING                    
         B     RS20                                                             
*                                                                               
         USING ASKELD,R2                                                        
RS15     MVC   ACCNT1R,SPACES                                                   
         LA    R2,TBARFST                                                       
RS15A    CLI   0(R2),0                                                          
         BE    RS20                                                             
         CLI   0(R2),ASKELQ        X'E2'                                        
         BE    RS16                                                             
*                                                                               
RS15B    SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     RS15A                                                            
*                                                                               
RS16     CLC   ASKKEY+1(2),=C'1R'                                               
         BNE   RS15B                                                            
         MVC   ACCNT1R,ASKKEY      SAVE 1R ACC,OFFICE AND CONTRA                
         BAS   RE,RDTRANS                                                       
         TM    BIT,NOTRN                                                        
         BZ    RS50                                                             
         MVC   ACCNT,ACCNT1R+3                                                  
         B     RS20A                                                            
*                                                                               
RS20     BAS   RE,RD1RACC             READ FOR THE 1R ACCOUNT                   
         TM    BIT,NO1R            DIDN'T FIND THE 1R ACCOUNT?                  
         BZ    RS50                                                             
*        CLC   ACCNT,=C'8C1AHF002098'  #10                                      
*        BE    RS20A                                                            
*        CLC   ACCNT,=C'8A02ZZ002007'  #12                                      
*        BE    RS20A                                                            
*        CLC   ACCNT,=C'8C02CF001649'  #20                                      
*        BE    RS20A                                                            
*        CLC   ACCNT,=C'8C02CF001647'  #21                                      
*        BNE   RS50                                                             
*                                                                               
         USING TAB1RD,RF                                                        
RS20A    L     RF,ATAB1R                                                        
RS21     CLC   TAB1RACC,ACCNT      IS THIS 1R ACCNT ALREADY IN TABLE            
         BE    RS25                                                             
         LA    RF,TAB1RLNQ(RF)                                                  
         L     R1,=A(TABEND)                                                    
         CR    RF,R1                                                            
         BL    RS21                                                             
         B     RS30                                                             
*                                                                               
RS25     AP    RECCNT,=P'1'                                                     
         B     RS40                                                             
         DROP  RF                                                               
*                                                                               
         USING TAB1RD,RF                                                        
RS30     AP    TOT1R,=P'1'         KEEP TRACK OF TOTAL 1R ACCOUNTS              
         AP    RECCNT,=P'1'        AS WELL AS TOTAL TRANSACTIONS                
         L     RF,ATAB1R                                                        
         AH    RF,STDISP                                                        
         L     R1,=A(TABEND)                                                    
         CR    RF,R1                                                            
         BL    *+6                                                              
         DC    H'0'                MUST EXPAND TABLE                            
*                                                                               
         MVC   TAB1RACC,ACCNT         MOVE IN ACCOUNT                           
         LA    RF,TAB1RLNQ(RF)                                                  
         L     R1,ATAB1R                                                        
         SR    RF,R1                                                            
         STH   RF,STDISP                                                        
         LH    R1,TABCOUNT                                                      
         LA    R1,1(R1)                                                         
         STH   R1,TABCOUNT                                                      
*                                                                               
RS40     L     R3,AIO2                                                          
         BAS   RE,DMPGET                                                        
*                                                                               
         BAS   RE,SVITEM           REREAD ELEMS TO SAVE INFO FOR BUILD          
         BAS   RE,BLDWRKR          BUILD WORKER FIEL AND POST                   
         BAS   RE,REPORTIT                                                      
         BAS   RE,POSTSORT                                                      
*                                                                               
RS50     MVC   DKEY,DKEYSV         RESET DIRECTORY FOR BATCH RECORDS            
         BAS   RE,READ             NEXT BATCH                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         B     RS00                                                             
         EJECT                                                                  
*                                                                               
         USING BIGPRNTD,R7                                                      
         USING PLINE,R6                                                         
RUNL     L     R7,VBIGPRNT                                                      
         BAS   RE,CLOSEWKR                                                      
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         MVC   P(31),=C'TOTAL 1R TRANSACTIONS MISSING: '                        
         EDIT  (P8,RECCNT),(12,P+33),COMMAS=YES,ZERO=NOBLANK                    
         GOTO1 ACREPORT                                                         
         MVC   P(27),=C'TOTAL 1R ACCOUNTS MISSING: '                            
         EDIT  (P8,TOT1R),(12,P+30),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 ACREPORT                                                         
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         BAS   RE,GETSORT                                                       
*                                                                               
*&&DO                                                                           
         LH    R2,TABCOUNT                                                      
         GOTO1 =V(QSORT),DMCB,(0,ATAB1R),(R2),TAB1RLNQ,L'TAB1RACC      +        
               TAB1RACC-TAB1RD                                                  
         MVC   P(19),=C'SORTED 1R ACCOUNTS:'                                    
         GOTO1 ACREPORT                                                         
         L     R2,ATAB1R                                                        
         LH    RF,TABCOUNT                                                      
RUNL10   MVC   P(12),TAB1RACC                                                   
         GOTO1 ACREPORT                                                         
         LA    R2,TAB1RLNQ(R2)                                                  
         BCT   RF,RUNL10                                                        
         B     RUNL10                                                           
*&&                                                                             
*                                                                               
RUNL20   B     XIT                                                              
*                                                                               
*---------------------------------------------------------------------*         
*        READ ELEMENTS OF ITEM RECORD TO SAVE INFO                    *         
*---------------------------------------------------------------------*         
*                                                                               
         USING TBARECD,R5                                                       
SVITEM   NTR1                                                                   
         LA    R6,P                                                             
         L     R5,AIO2                                                          
         MVI   SVNARRLN,0                                                       
         XC    SVCLI,SVCLI                                                      
         XC    SVPRD,SVPRD                                                      
         XC    SVJOB,SVJOB                                                      
         XC    SVNARR,SVNARR                                                    
         MVC   SVBREF,TBAKBREF       BATCH REFERENCE                            
         MVC   SVSJ,SPACES                                                      
         MVC   SVWC,SPACES                                                      
         LA    R2,TBARFST                                                       
SV10     CLI   0(R2),0                                                          
         BE    SVX                                                              
         CLI   0(R2),X'E1'                                                      
         BE    SV20                                                             
         CLI   0(R2),X'E2'                                                      
         BE    SV30                                                             
         CLI   0(R2),X'E8'                                                      
         BE    SV40                                                             
*                                                                               
SV15     SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     SV10                                                             
*                                                                               
         USING SFSELD,R2                                                        
SV20     CLC   SFSFLDN,FLD#TT                                                   
         BNE   SV22                                                             
         MVC   SVTYPE,SFSFIELD     TYPE OF TIME                                 
         B     SV15                                                             
*                                                                               
SV22     CLC   SFSFLDN,FLD#CLI     CLIENT                                       
         BNE   SV23                                                             
         MVC   SVUL,=C'SJ'                                                      
         ZIC   R1,SFSLN                                                         
         SHI   R1,SFSLN1Q                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SVCLI(0),SFSFIELD                                                
         B     SV15                                                             
*                                                                               
SV23     CLC   SFSFLDN,FLD#PRD     PRODUCT                                      
         BNE   SV24                                                             
         ZIC   R1,SFSLN                                                         
         SHI   R1,SFSLN1Q                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SVPRD(0),SFSFIELD                                                
         B     SV15                                                             
*                                                                               
SV24     CLC   SFSFLDN,FLD#PRD     JOB                                          
         BNE   SV25                                                             
         ZIC   R1,SFSLN                                                         
         SHI   R1,SFSLN1Q                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SVJOB(0),SFSFIELD                                                
         B     SV15                                                             
*                                                                               
SV25     CLC   SFSFLDN,FLD#WC      TASK / WORKCODE                              
         BNE   SV26                                                             
         MVC   SVWC,SFSFIELD                                                    
         B     SV15                                                             
*                                                                               
SV26     CLC   SFSFLDN,FLD#NARR    NARRATIVE                                    
         BNE   SV15                                                             
         ZIC   R1,SFSLN                                                         
         SHI   R1,SFSLN1Q                                                       
         STC   R1,SVNARRLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SVNARR(0),SFSFIELD                                               
         B     SV15                                                             
*                                                                               
         USING ASKELD,R2                                                        
SV30     CLC   ASKKEY+1(2),=C'1R'                                               
         BNE   SV35                                                             
         MVC   SVACCNT,ASKKEY+1    SAVE ACCOUNT                                 
         MVC   SVOFF,ASKKEY+15     SAVE OFFICE                                  
         MVC   SVCONTRA,ASKKEY+18  SAVE CONTRA                                  
         CLC   =C'1N',SVCONTRA                                                  
         BNE   *+10                                                             
         MVC   SVSJ,SPACES                                                      
         MVC   SVTDATE,ASKKEY+32   SAVE TRANSACTION DATE                        
         BAS   RE,GETNAME          GET CONTRA NAME                              
         B     SV15                                                             
*                                                                               
SV35     CLC   ASKKEY+1(2),=C'SJ'                                               
         BNE   SV15                                                             
         MVC   SVSJ,ASKKEY+1       SAVE SJ ACCOUNT                              
         B     SV15                                                             
*                                                                               
         USING BIAELD,R2                                                        
SV40     MVC   SVIREF,BIAREF       SAVE ITEM REFERENCE #                        
         ZAP   SVHRS,BIAAMT        SAVE OFF HOURS                               
         SR    R1,R1                                                            
         IC    R1,BIASIS           GET SEQUENCE NUMBER                          
         BCTR  R1,0                                                             
         MHI   R1,X'0D'                                                         
         AHI   R1,X'11'                                                         
         STC   R1,FLD#TT           TYPE OF TIME                                 
         AHI   R1,2                                                             
         STC   R1,FLD#CLI                                                       
         AHI   R1,1                                                             
         STC   R1,FLD#PRD                                                       
         AHI   R1,1                                                             
         STC   R1,FLD#JOB                                                       
         AHI   R1,1                                                             
         STC   R1,FLD#WC                                                        
         AHI   R1,7                                                             
         STC   R1,FLD#NARR                                                      
         B     SV15                                                             
*                                                                               
SVX      B     XIT                                                              
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
*        BUILD WORKER FILE                                            *         
*---------------------------------------------------------------------*         
*                                                                               
         USING PSHEADD,R2                                                       
BLDWRKR  NTR1                                                                   
         LA    R2,T                                                             
         XC    TLEN,TLEN                                                        
         LR    RE,R2                                                            
         LA    RF,L'T                                                           
         XCEFL                                                                  
         MVC   PSHDEL(2),=X'5046'        BUILD 50 HEADER ELEMENT                
         MVC   PSHDACC(1),RCCOMPFL                                              
         MVC   PSHDACC+1(14),SVACCNT     ACCOUNT                                
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC(1),RCCOMPFL      CONTRA                                 
         MVC   PSHDSBAC+1(14),SVCONTRA                                          
         MVC   PSHDSBNM,SVCNAME                                                 
*                                                                               
         USING TRNELD,R2                                                        
         ZIC   R1,1(,R2)           BUILD 44 TRANS ELEMENT                       
         AR    R2,R1               POINT TO NEXT LOCATION                       
         L     R5,AIO1                                                          
         XC    TRNEL(TRNLN1Q),TRNEL                                             
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q+1     ADD ONE BYTE OF NARRATIVE                    
         MVC   TRNNARR(1),SPACES                                                
         SR    RF,RF                                                            
         ICM   RF,1,SVNARRLN                                                    
         BZ    BLD08                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   TRNNARR(0),SVNARR                                                
         ZIC   R0,TRNLN                                                         
         AR    R0,RF                                                            
         STC   R0,TRNLN                                                         
*                                                                               
BLD08    OI    TRNSTAT,TRNSDR      SET TO DR                                    
         MVC   TRNDATE,SVTDATE                                                  
         MVC   TRNREF,SVIREF                                                    
         MVI   TRNTYPE,TRNTCLTM    SET TO TYPE 49                               
         ZAP   TRNAMNT,=P'0'                                                    
         AP    POSTAMNT,TRNAMNT                                                 
         MVC   TRNBTCH,BATCHREF                                                 
         MVC   TRNOFFC,SVOFF                                                    
         SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
*                                                                               
         USING FFTELD,R2                                                        
         MVI   FFTEL,FFTELQ        DB ELEMENT                                   
         LA    RF,FFTLN1Q+1        ADD ONE FOR THE DATA LENGTH                  
         LA    RE,L'SVIREF                                                      
         STC   RE,FFTDLEN          DATA LENGTH                                  
         BCTR  RE,0                                                             
         EXMVC RE,FFTDATA,SVIREF   ITEM REFERENCE                               
         LA    RE,1(RE)            RESTORE TRUE LENGTH OF REF                   
         AR    RF,RE                                                            
         STC   RF,FFTLN            ELEMENT LENGHT                               
         MVI   FFTTYPE,FFTTKREF    TYPE                                         
         MVI   FFTSEQ,0            SEQUENCE                                     
         SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
*                                                                               
         USING PRTELD,R2                                                        
         XC    PRTEL(PRTLNQ),PRTEL                                              
         MVI   PRTEL,PRTELQ        X'40' ELEMENT                                
         MVI   PRTLN,PRTLNQ                                                     
         ZAP   PRTRATE,=P'0'       ZERO HOURLY RATE                             
         ZAP   PRTHOUR,SVHRS(6)                                                 
         CLI   SVTYPE,C'B'                                                      
         BNE   *+12                                                             
         OI    PRTSTAT,PRTSBILQ                                                 
         B     BLD10                                                            
         CLI   SVTYPE,C'N'                                                      
         BNE   *+12                                                             
         OI    PRTSTAT,PRTSNOTQ                                                 
         B     BLD10                                                            
         CLI   SVTYPE,C'R'                                                      
         BNE   BLD10                                                            
         OI    PRTSTAT,PRTSRTEQ                                                 
BLD10    SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
*                                                                               
         USING SCIELD,R2                                                        
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ        '50' ELEMENT                                 
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITHOUR                                                 
         MVC   SCIAMNT,SVHRS                                                    
         CLC   SVSJ,SPACES         IS THERE AN SJ ACCOUNT                       
         BE    BLD20               NO THEN DON'T ADD 51 ELEM                    
         SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
*                                                                               
         USING PCIELD,R2                                                        
         OI    BIT,SJLEDG                                                       
         BAS   RE,GETLDG                                                        
         NI    BIT,X'FF'-SJLEDG                                                 
         MVI   PCIEL,PCIELQ        X'51' ELEMENT                                
         MVI   PCILN,PCILN2Q                                                    
         MVC   PCICLI(1),RCCOMPFL                                               
         MVC   PCICLI+1(L'SVSJ),SVSJ                                            
         MVC   PCIPRJT(1),RCCOMPFL                                              
         MVC   PCIPRJT+1(L'SVSJ),SVSJ                                           
         MVC   PCITSK,SVWC                                                      
                                                                                
*                                                                               
BLD20    BAS   RE,PUTIT                                                         
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        GET LEDGER LENGTHS                                           *         
*---------------------------------------------------------------------*         
         USING LDGRECD,R3                                                       
GETLDG   NTR1                                                                   
         L     R3,AIO1                                                          
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,RCCOMPFL    COMPANY                                      
         MVI   LDGKUNT,C'1'                                                     
         MVI   LDGKLDG,C'R'                                                     
         TM    BIT,SJLEDG          DOING THIS FOR THE SJ LEDGER?                
         BZ    *+12                                                             
         MVI   LDGKUNT,C'S'                                                     
         MVI   LDGKLDG,C'J'                                                     
         MVC   SAVEDIR,0(R3)                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCFIL',(R3),(R3)                     
         CLC   SAVEDIR(L'SAVEDIR),LDGKEY                                        
         BNE   GETLX                                                            
*                                                                               
         USING ACLELD,R3                                                        
         AH    R3,DATADISP                                                      
GETL10   CLI   0(R3),0                                                          
         BE    GETLX                                                            
         CLI   0(R3),ACLELQ                                                     
         BE    *+16                                                             
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     GETL10                                                           
*                                                                               
         LR    R6,R3                                                            
         ZIC   R1,1(R6)            TOTAL EL LENGTH                              
         SH    R1,=Y(ACLLN1Q)      - CODE AND LENGTH                            
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         LA    RF,L'ACLVALS(RF)    LENGTH OF EACH ACCT DESC AND LENGTH          
         DR    R0,RF                                                            
         STC   R1,NUMLEVS          SAVE NUMBER OF ACCOUNT LEVELS                
*                                                                               
         ZIC   R7,NUMLEVS                                                       
         AH    R6,=Y(ACLLN1Q)      BUMP TO FIRST LENGTH                         
         LA    RF,ABCDLEN          FIELD TO CONTAIN ALL 4 LENGTHS               
         XC    ABCDLEN,ABCDLEN                                                  
         SR    R1,R1               ACUMULATES HOW MUCH TO SUBTRACT              
         ZIC   R2,0(R6)                                                         
GETLDG5  STC   R2,0(RF)                                                         
         AR    R1,R2               ACCUMULATE LENGTHS                           
         LA    RF,1(RF)                                                         
         LA    R6,L'ACLVALS(R6)    NEXT LENGTH                                  
         ZIC   R2,0(R6)                                                         
         SR    R2,R1                                                            
         BCT   R7,GETLDG5                                                       
*                                                                               
GETLX    B     XIT                                                              
*                                                                               
*---------------------------------------------------------------------*         
*        READ FOR THE 1R ACCOUNT                                      *         
*        NTRY - ACCNT CONTAINS THE 1R ACCOUNT                                   
*---------------------------------------------------------------------*         
*                                                                               
         USING ACTRECD,R4                                                       
RD1RACC  NTR1                                                                   
         LA    R4,KEY2             DOES 1R RECORD EXIST                         
         MVC   KEY2,SPACES                                                      
         MVC   SAVEDIR,SPACES                                                   
         NI    BIT,X'FF'-NO1R                                                   
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         MVC   ACTKACT,ACCNT                                                    
         MVC   SAVEDIR,KEY2                                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,KEY2,KEY2                             
         CLC   KEY2(L'SAVEDIR),SAVEDIR                                          
         BE    RD1RX                                                            
         OI    BIT,NO1R                                                         
*                                                                               
RD1RX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------*         
*        READ FOR 1R TRANSACTIONS                                     *         
*---------------------------------------------------------------------*         
*                                                                               
         USING TRNRECD,R4                                                       
RDTRANS  NTR1                                                                   
         LA    R4,KEY2             DOES 1R RECORD EXIST                         
         MVC   KEY2,SPACES                                                      
         MVC   SAVEDIR,SPACES                                                   
         NI    BIT,X'FF'-NOTRN                                                  
         NI    BIT2,X'FF'-(NO1RACC+NOTRNACC)                                    
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY(L'ACCNT1R),ACCNT1R                                       
         MVC   SAVEDIR,KEY2                                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,KEY2,KEY2                             
         CLC   KEY2(TRNKOFF-TRNKEY),SAVEDIR                                     
         BE    *+16                                                             
         OI    BIT2,NO1RACC                                                     
         OI    BIT,NOTRN                                                        
         B     RDTX                                                             
         CLC   KEY2(TRNKDATE-TRNKEY),SAVEDIR                                    
         BE    RDTX                                                             
         OI    BIT,NOTRN                                                        
         OI    BIT2,NOTRNACC                                                    
*                                                                               
RDTX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              PUT WORKER RECORD TO ACPOST                                      
*---------------------------------------------------------------------*         
PUTIT    NTR1                                                                   
         AP    POSTREC,=P'1'                                                    
         LA    R2,T                                                             
         ZIC   R3,1(,R2)                                                        
PUT2     AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BE    PUT4                                                             
         ZIC   R3,1(,R2)                                                        
         LTR   R3,R3                                                            
         BNZ   PUT2                                                             
         MVI   0(R2),0                                                          
PUT4     LA    R2,1(,R2)                                                        
         LA    R3,TLEN                                                          
         SR    R2,R3                                                            
         STH   R2,TLEN                                                          
         BAS   RE,ADDPOST                                                       
         BAS   RE,DMPPOST                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* WORKER POSTING FILE  ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
OPENWKR  ZAP   POSTREC,=P'0'                                                    
         MVC   COMMAND,=CL6'OPEN'                                               
         B     FILE                                                             
*                                                                               
ADDPOST  MVC   COMMAND,=CL6'ADD'                                                
         OI    POSTFLAG,POSTADD                                                 
         B     FILE                                                             
*                                                                               
CLOSEWKR CP    POSTREC,=P'0'                                                    
         BE    CLOSEW20                                                         
*                                                                               
         ST    RE,SVRE                                                          
         XC    T(80),T                                                          
         MVC   TLEN,=X'0021'                                                    
         MVC   T(2),=X'521D'                                                    
         MVC   T+2(15),=CL15'POSTINGS'                                          
         ZAP   T+17(6),RECCNT                                                   
         ZAP   T+23(6),POSTAMNT                                                 
         BAS   RE,ADDPOST                                                       
         L     RE,SVRE                                                          
*                                                                               
CLOSEW20 MVC   COMMAND,=CL6'CLOSE'                                              
*                                                                               
FILE     NTR1                                                                   
         CLI   RCPOSTNG,C'N'                                                    
         BE    XIT                                                              
         LA    R3,TLEN                                                          
         L     R4,=A(POSTBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*********************************************************************           
* PRINT REPORT OF MISSING ACCOUNTS                                              
*********************************************************************           
*                                                                               
         USING PLINE,R6                                                         
REPORTIT NTR1                                                                   
         L     R7,VBIGPRNT                                                      
         L     R5,AIO2                                                          
         LA    R6,P                                                             
         MVC   PACCOUNT,SVACCNT                                                 
         TM    BIT2,NOTRNACC                                                    
         BZ    *+8                                                              
         MVI   PACCOUNT+14,C'*'                                                 
         MVC   PCONTRA,SVCONTRA                                                 
         GOTO1 DATCON,DMCB,(1,SVTDATE),(5,PDATE)                                
         EDIT  (P6,SVHRS),(6,PHRS),2,ZERO=NOBLANK,MINUS=YES                     
         OC    PHRS,SPACES                                                      
         MVC   PTTYPE,SVTYPE                                                    
         MVC   PREF,SVBREF                                                      
         MVC   PIREF,SVIREF                                                     
         EDIT  (P8,TOT1R),(3,P1R),COMMAS=YES,ZERO=NOBLANK                       
REPORTX  GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R7,R6                                                            
         EJECT                                                                  
*                                                                               
*********************************************************************           
* PUT TRANSACTION DATA TO SORTER  FOR END REPORTING                             
*********************************************************************           
*                                                                               
         EJECT                                                                  
POSTSORT NTR1                                                                   
         XC    SORTREC,SORTREC                                                  
         MVC   REC.SORT1R,SVACCNT                                               
         MVC   REC.SORTBTCH,SVBREF                                              
         MVC   REC.SORTDATE,SVTDATE                                             
         MVC   REC.SORTCNTR,SVCONTRA                                            
         MVC   REC.SORTREF,SVIREF                                               
         MVC   REC.SORTMOA,BATCHMOS                                             
         CLC   =C'1N',REC.SORTCNTR                                              
         BE    *+10                                                             
         MVC   REC.SORTUL,=C'SJ'                                                
         OC    REC.SORTUL,SPACES                                                
         MVC   REC.SORTCLI,SVCLI                                                
         MVC   REC.SORTPRD,SVPRD                                                
         MVC   REC.SORTJOB,SVJOB                                                
         MVC   REC.SORTWC,SVWC                                                  
         MVC   REC.SORTOFF,SVOFF                                                
         ZAP   REC.SORTHRS,SVHRS                                                
         MVC   REC.SORTTYP,SVTYPE                                               
         GOTO1 ADSORTER,DMCB,=C'PUT',SORTREC                                    
         B     XIT                                                              
*                                                                               
*********************************************************************           
* GET TRANSACTION DATA FROM SORTER TO PRINT REPORT                              
*********************************************************************           
*                                                                               
         EJECT                                                                  
GETSORT  NTR1                                                                   
         MVC   PREV1R,SPACES                                                    
         MVC   STDATE,SPACES                                                    
         MVC   ENDATE,SPACES                                                    
         ZAP   TOTALHRS,=P'0'                                                   
         ZAP   HRSBTIME,=P'0'                                                   
         ZAP   HRSRTIME,=P'0'                                                   
         ZAP   HRSNTIME,=P'0'                                                   
         ZAP   HRS1C,=P'0'                                                      
         ZAP   HRS1N,=P'0'                                                      
         ZAP   HRSJOBLV,=P'0'                                                   
         ZAP   HRSPRDLV,=P'0'                                                   
         ZAP   HRSCLILV,=P'0'                                                   
         ZAP   HRSNOLV,=P'0'                                                    
*                                                                               
GETSRT10 XC    SORTREC,SORTREC                                                  
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R6,15,DMCB+4                                                     
         BZ    GETSRT80                                                         
*                                                                               
         USING PSORTD,R2                                                        
         LA    R2,P                                                             
         MVC   SORTREC,0(R6)                                                    
         CLC   REC.SORT1R,PREV1R                                                
         BE    GETSRT20                                                         
         CLC   PREV1R,SPACES       FIRST TIME IN                                
         BE    GETSRT20                                                         
         BAS   RE,PRINTTOT                                                      
         MVC   STDATE,SPACES                                                    
         MVC   ENDATE,SPACES                                                    
                                                                                
GETSRT20 CLC   STDATE,SPACES                                                    
         BNE   *+10                                                             
         MVC   STDATE,REC.SORTDATE                                              
         CLC   ENDATE,SPACES                                                    
         BNE   *+10                                                             
         MVC   ENDATE,REC.SORTDATE                                              
         CLC   REC.SORTDATE,STDATE                                              
         BNL   *+10                                                             
         MVC   STDATE,REC.SORTDATE KEEP THE EARLIEST DATE                       
         CLC   REC.SORTDATE,ENDATE                                              
         BNH   *+10                                                             
         MVC   ENDATE,REC.SORTDATE KEEP THE LATEST DATE                         
*                                                                               
         MVC   PREV1R,REC.SORT1R                                                
         MVC   PSACNT,REC.SORT1R                                                
         MVC   PSBREF,REC.SORTBTCH                                              
         MVC   PSTTYPE,REC.SORTTYP                                              
         GOTO1 DATCON,DMCB,(1,REC.SORTDATE),(5,PSDATE)                          
         MVC   PSCNTR,REC.SORTCNTR                                              
         MVC   PSREF,REC.SORTREF                                                
         MVC   WORK(2),REC.SORTMOA                                              
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,PSMOA)                                   
         MVC   PSSJ,REC.SORTSJ                                                  
         MVC   PSOFF,REC.SORTOFF                                                
         AP    TOTALHRS,REC.SORTHRS                                             
         CLI   REC.SORTTYP,C'N'                                                 
         BNE   *+10                                                             
         AP    HRSNTIME,REC.SORTHRS                                             
         CLI   REC.SORTTYP,C'R'                                                 
         BNE   *+10                                                             
         AP    HRSRTIME,REC.SORTHRS                                             
         CLI   REC.SORTTYP,C'B'                                                 
         BNE   *+10                                                             
         AP    HRSBTIME,REC.SORTHRS                                             
         CLC   REC.SORTJOB,SPACES                                               
         BE    GETSRT22                                                         
         AP    HRSJOBLV,REC.SORTHRS                                             
         B     GETSRT30                                                         
*                                                                               
GETSRT22 CLC   REC.SORTPRD,SPACES                                               
         BE    GETSRT24                                                         
         AP    HRSPRDLV,REC.SORTHRS                                             
         B     GETSRT30                                                         
*                                                                               
GETSRT24 CLC   REC.SORTCLI,SPACES                                               
         BE    GETSRT26                                                         
         AP    HRSCLILV,REC.SORTHRS                                             
         B     GETSRT30                                                         
*                                                                               
GETSRT26 AP    HRSNOLV,REC.SORTHRS                                              
*                                                                               
GETSRT30 CLC   REC.SORTCNTR(2),=C'1C'                                           
         BNE   GETSRT32                                                         
         AP    HRS1C,REC.SORTHRS                                                
         B     GETSRT36                                                         
*                                                                               
GETSRT32 CLC   REC.SORTCNTR(2),=C'1N'                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         AP    HRS1N,REC.SORTHRS                                                
*                                                                               
GETSRT36 EDIT  (P8,REC.SORTHRS),PSHRS,2,COMMAS=YES,ZERO=NOBLANK,       X        
               MINUS=YES                                                        
         GOTO1 ACREPORT                                                         
         B     GETSRT10                                                         
*                                                                               
GETSRT80 BAS   RE,PRINTTOT                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
PRINTTOT NTR1                                                                   
         MVC   PTOTFOR,=CL10'TOTAL FOR'                                         
         EDIT  (P8,TOTALHRS),PSHRS,2,COMMAS=YES,ZERO=NOBLANK,MINUS=YES          
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+2(13),=C'TOTAL B HOURS'                                        
         EDIT  (P8,HRSBTIME),(12,P+20),2,COMMAS=YES,ZERO=NOBLANK,      X        
               MINUS=YES                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+2(13),=C'TOTAL N HOURS'                                        
         EDIT  (P8,HRSNTIME),(12,P+20),2,COMMAS=YES,ZERO=NOBLANK,      X        
               MINUS=YES                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+2(13),=C'TOTAL R HOURS'                                        
         EDIT  (P8,HRSRTIME),(12,P+20),2,COMMAS=YES,ZERO=NOBLANK,      X        
               MINUS=YES                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+2(13),=C'TOTAL 1C TIME'                                        
         EDIT  (P8,HRS1C),(12,P+20),2,COMMAS=YES,ZERO=NOBLANK,         X        
               MINUS=YES                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+2(13),=C'TOTAL 1N TIME'                                        
         EDIT  (P8,HRS1N),(12,P+20),2,COMMAS=YES,ZERO=NOBLANK,         X        
               MINUS=YES                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+2(14),=C'TOTAL JOB HOURS'                                      
         EDIT  (P8,HRSJOBLV),(12,P+20),2,COMMAS=YES,ZERO=NOBLANK,      X        
               MINUS=YES                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+2(14),=C'TOTAL PRD HOURS'                                      
         EDIT  (P8,HRSPRDLV),(12,P+20),2,COMMAS=YES,ZERO=NOBLANK,      X        
               MINUS=YES                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+2(14),=C'TOTAL CLI HOURS'                                      
         EDIT  (P8,HRSCLILV),(12,P+20),2,COMMAS=YES,ZERO=NOBLANK,      X        
               MINUS=YES                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+2(13),=C'TOTAL NON-JOB'                                        
         EDIT  (P8,HRSNOLV),(12,P+20),2,COMMAS=YES,ZERO=NOBLANK,       X        
               MINUS=YES                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+2(10),=C'START DATE'                                           
         GOTO1 DATCON,DMCB,(1,STDATE),(5,P+15)                                  
         MVC   P+25(8),=C'END DATE'                                             
         GOTO1 DATCON,DMCB,(1,ENDATE),(5,P+35)                                  
         GOTO1 ACREPORT                                                         
*                                                                               
         ZAP   TOTALHRS,=P'0'                                                   
         ZAP   HRSBTIME,=P'0'                                                   
         ZAP   HRSRTIME,=P'0'                                                   
         ZAP   HRSNTIME,=P'0'                                                   
         ZAP   HRS1C,=P'0'                                                      
         ZAP   HRS1N,=P'0'                                                      
         ZAP   HRSJOBLV,=P'0'                                                   
         ZAP   HRSPRDLV,=P'0'                                                   
         ZAP   HRSCLILV,=P'0'                                                   
         ZAP   HRSNOLV,=P'0'                                                    
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R2                                                               
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
HIGH     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
READ     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SEQ      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
WRT      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R3),DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ADD      LR    R0,RE                                                            
         XC    DA,DA                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PUT      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP SOME RECORDS                                                   *         
***********************************************************************         
         SPACE 1                                                                
DMPPOST  NTR1                                                                   
         LA    R6,=C'PUTP'                                                      
         LA    R3,TLEN                                                          
         SR    R4,R4                                                            
         LH    R4,TLEN                                                          
         B     DUMPIT                                                           
*                                                                               
DMPGDIR  NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GETD'                                                      
         LA    R4,L'DIR                                                         
         LA    R3,DIR                                                           
         B     DUMPIT                                                           
*                                                                               
DMPPDIR1 NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'DRRD'                                                      
         LA    R4,L'DIR                                                         
         LA    R3,DIR                                                           
         B     DUMPIT                                                           
*                                                                               
DMPPDIR2 NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'DRSQ'                                                      
         LA    R4,L'DIR                                                         
         LA    R3,DIR                                                           
         B     DUMPIT                                                           
*                                                                               
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET '                                                      
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT '                                                      
*                                                                               
DUMP     L     R3,AIO2                                                          
         SR    R4,R4                                                            
         ICM   R4,3,TIMRLEN-TIMRECD(R3)                                         
*                                                                               
DUMPIT   CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         GOTO1 PRNTBL,DMCB,(4,(R6)),(R3),C'DUMP',(R4),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(,R1)                                                        
         ZIC   R5,4(,R1)                                                        
         GOTO1 HELLO,DMCB,(C'G',ACCMST),((R4),(R2)),((R5),(R3))                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',ACCMST),((R4),(R2)),((R5),(R3))                 
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3)                               
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*   GET ACCOUNT NAME                                                            
***********************************************************************         
*                                                                               
         USING ACTRECD,R3                                                       
GETNAME  NTR1                                                                   
         MVC   SVCNAME,SPACES                                                   
         LA    R3,DKEY                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKCPY+1(14),SVCONTRA                                           
         BAS   RE,READ                                                          
         CLC   DKEY(L'ACTKEY),DIR                                               
         BNE   GETNX                                                            
         L     R3,AIO1                                                          
         BAS   RE,GET                                                           
         LA    R3,ACTRFST                                                       
*                                                                               
         USING NAMELD,R3                                                        
GNM10    CLI   0(R3),0                                                          
         BE    GETNX                                                            
         CLI   0(R3),NAMELQ                                                     
         BE    GNM30                                                            
*                                                                               
GNM20    SR    RF,RF                                                            
         IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     GNM10                                                            
*                                                                               
GNM30    SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVCNAME(0),NAMEREC   SAVE THE NAME                               
         OC    SVCNAME,SPACES                                                   
*                                                                               
GETNX    B     XIT                                                              
         DROP  R3                                                               
         DROP  REC                                                              
*****************************************                                       
*        EXTERNAL ADDRESSES                                                     
*****************************************                                       
EXTADDS  DS    0F                                                               
         DC    A(TAB1R)                                                         
*****************************************                                       
*        LITERALS                                                               
*****************************************                                       
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
         EJECT                                                                  
SORTCARD DC    CL80'SORT FIELDS=(001,023,A),FORMAT=BI,WORK=1'                   
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(070,,,,)'                             
*****************************************                                       
*        EQUATES                                                                
*****************************************                                       
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   0                   END OF TABLE                                 
*                                                                               
TRECCNT  DC    PL8'0'                                                           
RECCNT   DC    PL8'0'                                                           
TOT1R    DC    PL8'0'                                                           
DRCTOT   DC    PL8'0'                                                           
CRCTOT   DC    PL8'0'                                                           
RECOCNT  DC    PL8'0'                                                           
DROTOT   DC    PL8'0'                                                           
CROTOT   DC    PL8'0'                                                           
TOTCINC  DC    PL8'0'                                                           
TOTOINC  DC    PL8'0'                                                           
ITEMAMNT DC    PL6'0'                                                           
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
POSTREC  DC    PL6'0'                                                           
POSTAMNT DC    PL6'0'                                                           
TOTALHRS DC    PL8'0'                                                           
HRSBTIME DC    PL8'0'                                                           
HRSNTIME DC    PL8'0'                                                           
HRSRTIME DC    PL8'0'                                                           
HRS1C    DC    PL8'0'                                                           
HRS1N    DC    PL8'0'                                                           
HRSJOBLV DC    PL8'0'                                                           
HRSPRDLV DC    PL8'0'                                                           
HRSCLILV DC    PL8'0'                                                           
HRSNOLV  DC    PL8'0'                                                           
MAXDUMP  DC    PL4'2000'                                                        
*                                                                               
         EJECT                                                                  
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
DATVAL   DC    V(DATVAL)                                                        
HEXIN    DC    V(HEXIN)                                                         
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
AIO3     DC    A(IO3)                                                           
*                                                                               
DKEY     DS    CL(L'ACCKEY)                                                     
KEY2     DS    CL56                                                             
DKEYSV   DS    CL(L'ACCKEY)                                                     
SAVEDIR  DS    CL(L'ACCKEY)                                                     
DIR      DS    CL64                                                             
DA       DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
IO1      DS    CL2000                                                           
IO2      DS    CL2000                                                           
IO3      DS    CL2000                                                           
*                                                                               
POSTBUFF DS    0D                                                               
         DC    4500X'00'                                                        
TAB1R    DS    0D                  TABLE OF 1R ACCOUNTS                         
         DS    500C                                                             
TABEND   DS    0C                                                               
*                                                                               
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1       (1)                                                    
PACCOUNT DS    CL14      (2-15)    ACCOUNT                                      
         DS    CL2       (16-17)                                                
PCONTRA  DS    CL14      (18-31)   CONTRA                                       
         DS    CL2       (32-33)                                                
PDATE    DS    CL8       (34-41)   DATE                                         
         DS    CL2       (42-43)                                                
PHRS     DS    CL6       (44-49)   HOURS                                        
         DS    CL2       (50-51)                                                
PTTYPE   DS    CL1       (52)      TYPE OF TIME                                 
         DS    CL2       (53-54)                                                
PREF     DS    CL4       (55-58)   BATCH REFERENCE                              
         DS    CL2       (59-60)                                                
PIREF    DS    CL6       (61-66)   ITEM REFERENCE                               
         DS    CL2       (67-68)                                                
P1R      DS    CL3       (69-71)   RUNNING COUNT OF 1R'S                        
         ORG   PTTYPE                                                           
PCOUNT   DS    CL14                RECORD COUNT                                 
         DS    CL2       (95-96)                                                
PLNQ     EQU   *-PLINE                                                          
*                                                                               
PLINE2   DSECT                                                                  
         DS    CL9       (1-9)                                                  
PACCT    DS    CL14      (10-23)                                                
         DS    CL2       (24-25)                                                
PCNTR    DS    CL14      (26-39)                                                
         DS    CL2       (40-41)                                                
PBDTE    DS    CL8       (42-49)                                                
         DS    CL2       (50-51)                                                
PREF2    DS    CL6       (52-57)                                                
         DS    CL2       (58-59)                                                
PBREF2   DS    CL6       (60-65)                                                
         DS    CL2       (66-67)                                                
POFF     DS    CL2       (68-69)                                                
         DS    CL11      (70-80)                                                
PDRAMT   DS    CL14      (81-94)                                                
         DS    CL2       (95-96)                                                
PCRAMT   DS    CL14      (97-100)                                               
         DS    CL2       (101-102)                                              
PCNAME   DS    CL36      (103-138)                                              
         DS    CL2       (139-140)                                              
PSALEAC  DS    CL14      (141-154)                                              
*                                                                               
ACXYD    DSECT                                                                  
SVRE     DS    F                                                                
COMMAND  DS    CL6                                                              
ELCODE   DS    XL1                                                              
ELM      DS    CL255                                                            
ABCDLEN  DS    0CL4                                                             
LN1RLEV1 DS    CL1                                                              
LN1RLEV2 DS    CL1                                                              
LN1RLEV3 DS    CL1                                                              
LN1RLEV4 DS    CL1                                                              
*                                                                               
TREF     DS    CL6                 ORIGINAL REFERENCE                           
TDATE    DS    XL3                 ORIGINAL DATE                                
TBATREF  DS    CL6                 ORIGINAL BATCH REF                           
*                                                                               
         DS    XL1                 ALIGNMENT                                    
TABCOUNT DS    H                                                                
SVTYPE   DS    CL1                 TYPE OF TIME                                 
SVHRS    DS    PL6                 HOURS                                        
SVUNHRS  DS    CL6                 HOURS (UNPACKED AND EBCDIC)                  
SVHRSNUM DS    XL1                 LENGTH OF HOURS FOR EX                       
SVTDATE  DS    XL8                 TRANSACTION DATE                             
SVACCNT  DS    CL14                1R ACCOUNT                                   
SVCONTRA DS    CL14                CONTRA ACCOUNT                               
SVSJ     DS    0CL14               SJ ACCOUNT                                   
SVUL     DS    CL2                                                              
SVCLI    DS    CL3                                                              
SVPRD    DS    CL3                                                              
SVJOB    DS    CL6                                                              
SVWC     DS    CL2                                                              
SVBREF   DS    CL4                 BATCH REFERENCE                              
SVOFF    DS    CL2                 OFFICE                                       
SVCNAME  DS    CL36                CONTRA ACOCUNT NAME                          
SVIREF   DS    CL6                 ITEM REFERENCE NUMBER                        
SVNARRLN DS    XL1                                                              
SVNARR   DS    CL120                                                            
PREV1R   DS    CL14                                                             
STDATE   DS    XL3                                                              
ENDATE   DS    XL3                                                              
*                                                                               
STDISP   DS    H                                                                
BATCHMOS DS    XL2                 PACKED FORMAT                                
BATCHREF DS    0CL6                                                             
BATCHCMO DS    CL2                 CHARACTER FORMAT                             
BATCHCDE DS    CL4                                                              
ACCNT    DS    CL12                                                             
ACCNT1R  DS    CL32                                                             
CONTRA   DS    CL14                                                             
NUMLEVS  DS    XL1                                                              
*                                                                               
POSTFLAG DS    XL1                                                              
POSTOPEN EQU   X'80'                                                            
POSTADD  EQU   X'40'                                                            
REPORTSW DS    CL1                                                              
BIT      DS    XL1                                                              
OFFDPT   EQU   X'80'                                                            
SUBDPT   EQU   X'40'                                                            
PERSON   EQU   X'20'                                                            
NO1R     EQU   X'10'                                                            
SJLEDG   EQU   X'08'                                                            
NEGHRS   EQU   X'04'                                                            
NOTRN    EQU   X'02'                                                            
BIT2     DS    XL1                                                              
NO1RACC  EQU   X'80'                                                            
NOTRNACC EQU   X'40'                                                            
FLD#TT   DS    XL1                 TYPE OF TYPE FIELD NUMBER                    
FLD#CLI  DS    XL1                 CLIENT       FIELD NUMBER                    
FLD#PRD  DS    XL1                 PRODUCT      FIELD NUMBER                    
FLD#JOB  DS    XL1                 JOB          FIELD NUMBER                    
FLD#WC   DS    XL1                 WORKCODE     FIELD NUMBER                    
FLD#NARR DS    XL1                 NARRATIVE    FIELD NUMBER                    
*                                                                               
HIT      DS    XL1                                                              
HITSJ    EQU   X'80'                                                            
HIT1C    EQU   X'40'                                                            
HITBATCH EQU   X'01'                                                            
*                                                                               
SORTREC  DS    CL(SORTLNQ)                                                      
*                                                                               
TLEN     DS    F                   RECORD LENGTH                                
T        DS    CL400                                                            
         DS    CL500                                                            
AREA     DS    CL200                                                            
ID       DS    CL16                                                             
SVTYTIM  DS    CL1                                                              
*                                                                               
VTYPES   DS    0A                                                               
ATAB1R   DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
MAXCOUNT EQU   2500                MAX # OF 1R ACCOUNTS                         
         EJECT                                                                  
*********************                                                           
* DSECTS                                                                        
*********************                                                           
TAB1RD   DSECT                                                                  
TAB1RACC DS    CL12                                                             
TAB1RLNQ EQU   *-TAB1RACC                                                       
*                                                                               
SORTRECD DSECT                                                                  
SORTKEY  DS    0CL23                                                            
SORT1R   DS    CL14                                                             
SORTDATE DS    XL3                                                              
SORTBTCH DS    CL4                                                              
SORTCNTR DS    CL14                                                             
SORTREF  DS    CL6                                                              
SORTMOA  DS    XL2                                                              
SORTOFF  DS    CL2                                                              
SORTSJ   DS    0CL14                                                            
SORTUL   DS    CL2                                                              
SORTCLI  DS    CL3                                                              
SORTPRD  DS    CL3                                                              
SORTJOB  DS    CL6                                                              
SORTWC   DS    CL2                                                              
SORTTYP  DS    CL1                                                              
SORTHRS  DS    PL8                                                              
SORTLNQ  EQU   *-SORTRECD                                                       
         SPACE 1                                                                
*                                                                               
PSORTD   DSECT                                                                  
PSACNT   DS    CL14                                                             
         DS    CL1                                                              
PSOFF    DS    CL2                                                              
         DS    CL1                                                              
PSCNTR   DS    CL14                                                             
         DS    CL1                                                              
PSDATE   DS    CL8                                                              
         DS    CL1                                                              
PSREF    DS    CL6                                                              
         DS    CL1                                                              
PSBREF   DS    CL4                                                              
         DS    CL1                                                              
PSTTYPE  DS    CL1                                                              
         DS    CL1                                                              
PTOTFOR  DS    0CL10                                                            
PSMOA    DS    CL6                                                              
         DS    CL1                                                              
PSSJ     DS    CL14                                                             
         DS    CL1                                                              
PSHRS    DS    CL12                                                             
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  ACGENPOST                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
*  ACGENMODES                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*  ACGENPOST                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
*  ACGENPOST                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*  DMDTFIS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'106ACREPXY02T05/01/02'                                      
         END                                                                    
