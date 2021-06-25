*          DATA SET ACREPPC02  AT LEVEL 059 AS OF 03/23/15                      
*PHASE ACPC02A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE SORTER                                                                 
         TITLE 'ACPC02 - PROJECT CONTROL'                                       
ACPC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACPC                                                         
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING ACPC02+4096,R9                                                   
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACPC02D,RC                                                       
         EJECT                                                                  
PC1      CLI   MODE,RUNFRST                                                     
         BNE   PC10                                                             
         RELOC RELO                                                             
         LA    RE,RELOTAB          RELOCATE A-TYPES                             
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
         L     R4,AREC                                                          
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'1R'                                             
         BAS   RE,HIGH             READ 1R LEDGER RECORD                        
         CLC   ACKEYACC,SAVEKEY                                                 
         BE    *+6                                                              
         DC    H'0'                CAN'T READ 1R LEDGER                         
         MVI   ELCODE,X'16'                                                     
         BAS   RE,GETEL                                                         
         BE    PC2                                                              
         DC    H'0'                NO HIERARCHY ELEMENT                         
         SPACE 1                                                                
         USING ACHEIRD,R4                                                       
PC2      CLI   ACHRLEVD,0          IS IT 4 LEVELS                               
         BE    PC8                 NO SO SKIP PROFILE                           
         CLI   PROGPROF,C'Y'       SHOULD IT LOOK LIKE 3 LEVELS                 
         BNE   PC8                 NO LEAVE IT ALONE                            
         MVI   ACHRLEVD,0          MAKE A 1-2-2-7                               
         MVI   ACHRLEVC,12         LOOK LIKE A 1-2-9                            
         MVI   SRTK1+8,22          FIX UP SORT KEYS                             
         MVI   SRTK1+12,20                                                      
         MVI   SRTK2+8,22                                                       
         MVI   SRTK2+12,20                                                      
         MVI   SRTK3+8,7                                                        
         MVI   SRTK3+12,5                                                       
         MVI   SRTK4+20,7                                                       
         MVI   SRTK4+24,5                                                       
         SPACE 1                                                                
PC8      GOTO1 LEVBLD,DMCB,(R4),APERLEV                                         
         BAS   RE,RDPER            BUILD PERSON LIST                            
         L     R8,APERLEV                                                       
         USING LVD,R8                                                           
         MVC   NUMPLEV,LVNUM       SAVE NUMBER OF LEVELS                        
PCXIT    XIT1                                                                   
         EJECT                                                                  
PC10     CLI   MODE,REQFRST                                                     
         BNE   PC30                                                             
         XC    PERPDTE,PERPDTE     PACKED PERIOD DATES                          
         XC    PERMDTE,PERMDTE     MOS FORMAT                                   
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,END)                                     
         MVC   WORK(4),QSTART                                                   
         LA    R2,PERPDTE                                                       
         LA    R3,PERMDTE                                                       
         SR    R0,R0                                                            
         SPACE 1                                                                
PC12     GOTO1 DATCON,DMCB,(0,WORK),(1,0(R2))                                   
         AH    R0,=H'1'                                                         
         STC   R0,PERMTHS          MONTHS IN PERIOD                             
         MVC   0(1,R3),WORK+1      YEAR FOR MOS                                 
         ZIC   R1,WORK+3                                                        
         CLI   WORK+2,C'1'                                                      
         BNE   *+8                                                              
         SH    R1,=H'47'                                                        
         STC   R1,1(R3)            MONTH FOR MOS                                
         CLC   0(3,R2),END         AM I UP TO END DATE                          
         BE    PC14                YES                                          
         LA    R6,45               MIDDLE OF NEXT MONTH                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R6)                                      
         MVC   WORK(4),WORK+6                                                   
         LA    R2,3(R2)                                                         
         LA    R3,2(R3)                                                         
         B     PC12                                                             
         SPACE 1                                                                
PC14     L     R4,ADCMPEL                                                       
         USING ACCOMPD,R4                                                       
         ZIC   R1,ACMPSTM          FINANCIAL YEAR                               
         CLI   ACMPSTM,C'0'                                                     
         BH    *+8                 NUMERIC                                      
         AH    R1,=H'63'           I.E. C=195 + 63 = 258                        
         SH    R1,=H'240'          MINUS 240= 18 = X'12'                        
         BP    *+8                                                              
         LA    R1,1                IF MINUS - NO FIN- YEAR USE JANUARY          
         STC   R1,WORK+1           MONTH PACKED                                 
         MVI   WORK+2,1            DAY PACKED                                   
         MVC   YTDPDTE,PERPDTE                                                  
         MVC   YTDMDTE,PERMDTE                                                  
         MVC   YTDMTHS,PERMTHS                                                  
         CLC   WORK+1(2),END+1     IF FINANCIAL = END DATE                      
         BE    PC18                PERIOD DATES SAME AS YTD                     
         ZIC   R1,END                                                           
         LA    R3,1                                                             
         TM    END,X'0F'           WORK OUT START OF                            
         BNZ   *+8                 FINANCIAL YEAR                               
         LA    R3,7                                                             
         CLC   WORK+1(2),END+1     R3=0 IF MONTH LOWER THAN END MONTH           
         BH    *+8                 R3=1 IF MONTH HIGH AND NOT A DECADE          
         LA    R3,0                R3=7 IF MONTH HIGH AND A DECADE              
         SR    R1,R3                                                            
         STC   R1,WORK                                                          
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+6)                                  
         MVC   WORK(6),WORK+6                                                   
         XC    YTDPDTE,YTDPDTE                                                  
         XC    YTDMDTE,YTDMDTE                                                  
         LA    R2,YTDPDTE                                                       
         LA    R3,YTDMDTE                                                       
         SR    R0,R0                                                            
         SPACE 1                                                                
PC16     GOTO1 DATCON,DMCB,(0,WORK),(1,0(R2))                                   
         AH    R0,=H'1'                                                         
         STC   R0,YTDMTHS                                                       
         MVC   0(1,R3),WORK+1      YEAR FOR MOS                                 
         ZIC   R1,WORK+3                                                        
         CLI   WORK+2,C'1'                                                      
         BNE   *+8                                                              
         SH    R1,=H'47'                                                        
         STC   R1,1(R3)            MONTH FOR MOS                                
         CLC   0(3,R2),END                                                      
         BE    PC18                                                             
         LA    R6,45                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R6)                                      
         MVC   WORK(4),WORK+6                                                   
         LA    R2,3(R2)                                                         
         LA    R3,2(R3)                                                         
         B     PC16                                                             
         SPACE 1                                                                
PC18     CLC   YTDPDTE(3),PERPDTE                                               
         BH    PC18A               IF YTD HIGH USE REQUEST PERIOD               
         CLC   YTDMTHS,PERMTHS                                                  
         BL    PC18A               IF LES MONTHS IN YTD USE PERIOD              
         B     PC18B                                                            
         SPACE 1                                                                
PC18A    MVC   YTDPDTE,PERPDTE                                                  
         MVC   YTDMDTE,PERMDTE                                                  
         MVC   YTDMTHS,PERMTHS                                                  
         SPACE 1                                                                
PC18B    MVC   PRJOPT(5),QOPT1                                                  
         CLC   PRJOPT(5),SPACES                                                 
         BNE   *+8                                                              
         MVI   PRJOPT,C'1'         DEFAULT IS CLIENT                            
         SPACE 1                                                                
         L     R4,ADCMPNAM                                                      
         LA    R6,COMPNAM                                                       
         BAS   RE,NAMOUT                                                        
         SPACE 1                                                                
         GOTO1 ASORTER,DMCB,SORTCARD,RECCARD,(40,ASORTC)                        
         EJECT                                                                  
*              SETUP HEADLINES                                                  
         MVC   PERD,SPACES                                                      
         LA    R1,HDFLD1                                                        
         LA    R3,10                                                            
         MVC   0(L'HDFLDS,R1),SPACES                                            
         LA    R1,L'HDFLDS(R1)                                                  
         BCT   R3,*-10                                                          
         SPACE 1                                                                
         MVI   HDFLDS+5,C'-'                                                    
         MVC   HDFLDS+6(16),HDFLDS+5                                            
         MVC   HDFLDS+35(22),HDFLDS                                             
         MVC   PERD(14),=C'FOR THE PERIOD'                                      
         GOTO1 DATCON,DMCB,(1,PERPDTE),(9,WORK)                                 
         MVC   WORK+3(2),WORK+4         JAN/82 TO JAN82                         
         LA    R3,4                     FOR EXECUTE                             
         CLI   PERMTHS,1                                                        
         BE    PC20                     ONLY ONE MONTH                          
         GOTO1 DATCON,DMCB,(1,END),(9,WORK+6)                                   
         MVC   WORK+9(2),WORK+10                                                
         MVI   WORK+5,C'-'                                                      
         LA    R3,10                                                            
         SPACE 1                                                                
PC20     EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   PERD+15(0),WORK     MONTH(S) AND Y.T.D INTO                      
         LA    R1,HDFLDS+11        HEADLINE FIELDS                              
         CLI   PERMTHS,1                                                        
         BE    *+8                                                              
         LA    R1,HDFLDS+8                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),WORK                                                     
         MVC   HDFLD1+46(5),=C'Y.T.D'                                           
         MVC   HDFLD2(L'HDFLDS),HDFLD1                                          
         MVC   HDFLD3(L'HDFLDS),HDFLD1                                          
         MVC   HDFLD4(L'HDFLDS),HDFLD1                                          
         MVC   HDFLD5(L'HDFLDS),HDFLD1                                          
         SPACE 1                                                                
*                                  SET UP ALL POSSIBLE OPTIONS FIELDS           
         MVC   HDOP1,SPACES                                                     
         MVC   HDOP2,SPACES                                                     
         MVC   HDOP3,SPACES                                                     
         MVC   HDOP4,SPACES                                                     
         MVC   HDOP4A,SPACES                                                    
         MVC   HDOP1+00(22),=C'     HOURS        COST'                          
         MVC   HDOP1+35(22),=C'     HOURS        COST'                          
         MVC   HDOP2+00(22),=C'           HOURS      '                          
         MVC   HDOP2+35(22),=C'           HOURS      '                          
         MVC   HDOP3+00(22),=C'           COST       '                          
         MVC   HDOP3+35(22),=C'           COST       '                          
         SPACE 1                                                                
         ZIC   R3,PERMTHS          TOTAL MONTHS                                 
         LR    R2,R3                                                            
         SH    R2,=H'5'                                                         
         BP    *+8                 FOR OPTION 4                                 
         LA    R2,1                NUMBER OF MONTHS IN FIRST BUCKET             
         STC   R2,FRSTBUCK                                                      
         SR    R3,R2               R2=MONTHS IN FIRST , R3=REMAINING            
         LA    R5,PERPDTE                                                       
         LA    R6,HDOP4                                                         
         CLI   FRSTBUCK,1                                                       
         BNE   *+8                                                              
         LA    R6,4(R6)                                                         
         GOTO1 DATCON,DMCB,(1,0(R5)),(9,WORK)                                   
         MVC   0(3,R6),WORK                                                     
         MVC   L'HDOP4(3,R6),=3C'-'                                             
         LA    R6,7(R6)                                                         
         LA    R5,3(R5)                                                         
         CLI   FRSTBUCK,1                                                       
         BE    PC23                                                             
         SH    R6,=H'4'                                                         
         MVI   0(R6),C'-'                                                       
         ZIC   R2,FRSTBUCK                                                      
         BCTR  R2,0                                                             
         MH    R2,=H'3'                                                         
         LA    R5,PERPDTE(R2)                                                   
         GOTO1 DATCON,DMCB,(1,0(R5)),(9,WORK)                                   
         MVC   1(3,R6),WORK                                                     
         MVC   L'HDOP4(4,R6),=4C'-'                                             
         LA    R6,8(R6)                                                         
         LA    R5,3(R5)                                                         
         SPACE 1                                                                
PC23     LTR   R3,R3                                                            
         BZ    PC26                                                             
PC25     GOTO1 DATCON,DMCB,(1,0(R5)),(9,WORK)                                   
         MVC   0(3,R6),WORK                                                     
         MVC   L'HDOP4(3,R6),=3C'-'                                             
         LA    R6,7(R6)                                                         
         LA    R5,3(R5)                                                         
         BCT   R3,PC25                                                          
PC26     BCTR  R6,0                                                             
         MVC   0(5,R6),=C'TOTAL'                                                
         MVC   L'HDOP4(5,R6),=5C'-'                                             
         SPACE 1                                                                
         LA    R1,PRJOPT           MOVE THE HDOP TO THE                         
         LA    R2,HDFLD1           HDFLD FOR REPORT                             
         LA    R4,L'HDFLDS                                                      
         MH    R4,=H'2'                                                         
         LA    R3,5                                                             
         SPACE 1                                                                
PC27     CLI   0(R1),C'1'                                                       
         BNE   *+10                                                             
         MVC   L'HDFLDS(L'HDOP1,R2),HDOP1                                       
         CLI   0(R1),C'2'                                                       
         BNE   *+10                                                             
         MVC   L'HDFLDS(L'HDOP2,R2),HDOP2                                       
         CLI   0(R1),C'3'                                                       
         BNE   *+10                                                             
         MVC   L'HDFLDS(L'HDOP3,R2),HDOP3                                       
         CLI   0(R1),C'4'                                                       
         BNE   *+16                                                             
         MVC   0(L'HDFLDS,R2),HDOP4                                             
         MVC   L'HDFLDS(L'HDOP4A,R2),HDOP4A                                     
         AR    R2,R4                                                            
         LA    R1,1(R1)                                                         
         BCT   R3,PC27                                                          
         SPACE 1                                                                
         LA    R0,4                                                             
         LA    R1,QSELECT                                                       
         SR    R3,R3                                                            
         CLC   QSELECT,SPACES                                                   
         BE    PC29                                                             
PC28     CLI   0(R1),C' '                                                       
         BE    PC29                                                             
         AH    R3,=H'1'                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,PC28                                                          
PC29     BCTR  R3,0                                                             
         STC   R3,FILTLEN                                                       
         B     PCXIT                                                            
         EJECT                                                                  
PC30     CLI   MODE,LEDGFRST                                                    
         BNE   PC37                                                             
         L     R4,ADLDGHIR                                                      
         L     R5,ACLILEV                                                       
         GOTO1 LEVBLD,DMCB,(R4),(R5)                                            
         SPACE 1                                                                
         USING ACANALD,R4                                                       
         L     R4,ADLEDGER                                                      
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         SPACE 1                                                                
PC33     BNE   PC36                                                             
         MVC   WORK,SPACES         ADD WORKCODES/NAMES TO TASK LIST             
         MVC   WORK(2),ACANCODE                                                 
         MVC   WORK+15(15),ACANDESC                                             
         GOTO1 ADDNME,DMCB,WORK,ATSKLST                                         
         BAS   RE,NEXTEL                                                        
         B     PC33                                                             
         SPACE 1                                                                
PC36     DC    0H'0'                                                            
         B     PCXIT                                                            
         EJECT                                                                  
PC37     CLI   MODE,LEVAFRST                                                    
         BNE   PC38                                                             
         MVI   LEVASW,C'N'                                                      
         B     PCXIT                                                            
         SPACE 1                                                                
PC38     CLI   MODE,LEVBFRST                                                    
         BNE   PC39                                                             
         MVI   LEVBSW,C'N'                                                      
         B     PCXIT                                                            
         SPACE 1                                                                
PC39     CLI   MODE,PROCACC                                                     
         BNE   PC40                                                             
         MVI   LEVCSW,C'N'                                                      
         B     PCXIT                                                            
         EJECT                                                                  
PC40     CLI   MODE,ANALFRST                                                    
         BNE   PC43                                                             
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         MVC   SVANAL,TRNSANAL                                                  
         B     PCXIT                                                            
         SPACE 1                                                                
PC43     CLI   MODE,SBACFRST                                                    
         BNE   PC45                                                             
         XC    THISREC,THISREC                                                  
         XC    SVSBAC,SVSBAC                                                    
         L     R4,ADSUBAC                                                       
         USING TRSUBHD,R4                                                       
         MVI   WANT,C'Y'                                                        
         CLC   QSELECT,SPACES                                                   
         BE    PC44                                                             
         ZIC   R3,FILTLEN                                                       
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   TRSBACNT+3(0),QSELECT                                            
         BE    PC44                                                             
         MVI   WANT,C'N'                                                        
         B     PCXIT                                                            
         SPACE 1                                                                
PC44     ZIC   R3,TRSBLEN                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SVSBAC(0),TRSBEL                                                 
         B     PCXIT                                                            
         SPACE 1                                                                
PC45     CLI   MODE,PROCHIST       DO WE HAVE A HISTORY RECORD ?                
         BNE   PC50                NO, SEE IF SUB-ACCT LAST                     
         CLI   WANT,C'N'           YES, DO WE WANT IT ?                         
         BE    PCXIT               NO, BYPASS IT                                
         USING TRHISTD,R4                                                       
         L     R4,ADTRANS                                                       
         CLI   TRHSEL,X'45'                                                     
         BNE   PCXIT                                                            
         USING RECD,R6                                                          
         LA    R6,THISREC                                                       
         LA    R2,RECACCUM         R2 TO BUCKETS                                
         CLI   BUCKTYPE,C'H'                                                    
         BE    *+8                                                              
         LA    R2,4(R2)            HOURS/COST                                   
         LA    R3,YTDPDTE                                                       
         ZIC   R0,YTDMTHS          NUMBER OF MONTHS                             
         ZAP   DUB,TRHSDR                                                       
         CVB   R1,DUB              ONLY DEBITS                                  
         SPACE 1                                                                
PC47     CLC   TRHSYEAR(2),0(R3)                                                
         BNE   PC48                                                             
         L     RE,0(R2)            ADD THIS AMOUNT                              
         AR    RE,R1               TO BUCKETS                                   
         ST    RE,0(R2)                                                         
         B     PCXIT                                                            
         SPACE 1                                                                
PC48     LA    R3,3(R3)            NEXT DATE                                    
         LA    R2,8(R2)            NEXT BUCKET                                  
         BCT   R0,PC47                                                          
         B     PCXIT                                                            
         EJECT                                                                  
PC50     CLI   MODE,SBACLAST                                                    
         BNE   PC100                                                            
         CLI   WANT,C'N'                                                        
         BE    PCXIT                                                            
         USING RECD,R6                                                          
         LA    R6,THISREC                                                       
         OC    RECTYP(RECLEN),RECTYP NO AMOUNTS                                 
         BZ    PCXIT                                                            
         MVI   RECTYP,1                                                         
         L     R4,ADACC                                                         
         MVC   RECACC,0(R4)        ACCOUNT TO KEY                               
         MVC   RECTSK,SVANAL                                                    
         CLC   RECTSK,SPACES                                                    
         BNE   *+10                                                             
         MVC   RECTSK,=C'Z '                                                    
         MVC   RECCOUNT,=F'1'                                                   
         LA    R4,SVSBAC                                                        
         USING TRSUBHD,R4                                                       
         MVC   RECCON,TRSBACNT                                                  
         SPACE 1                                                                
         MVC   WORK(15),TRSBACNT   ADD CONTRA ACCOUNT                           
         MVC   WORK+15(36),SPACES  CODE AND NAME TO PERLST                      
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+15(0),TRSBNAME                                              
         GOTO1 ADDNME,DMCB,WORK,APERLST                                         
         SPACE 1                                                                
         LA    R7,SRTREC                                                        
         MVC   0(L'SRTREC,R7),RECTYP                                            
         CLI   PRJOPT,C' '                                                      
         BE    PC66                NOT CLIENT RECORD                            
         GOTO1 ASORTER,DMCB,=C'PUT',(R7)                                        
PC66     MVI   0(R7),2             TYPE                                         
         L     R8,ACLILEV                                                       
         USING LVD,R8                                                           
         L     R8,LVLOW                                                         
         ZIC   R1,LVDSP                                                         
         LA    R1,RECACC-RECD(R1,R7)                                            
         ZIC   R3,LVLN                                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)       CLEAR LOW LEVEL OF CLIENT                    
         CLI   DIVOPT,C' '                                                      
         BE    PC68                NO CLIENT TASK SUMMARY                       
         GOTO1 ASORTER,DMCB,=C'PUT',(R7)                                        
         SPACE 1                                                                
PC68     MVI   0(R7),3             TYPE 3 PERSON/CLIENT                         
         MVC   RECACC-RECD(L'RECACC,R7),RECCON                                  
         XC    RECCON-RECD(L'RECCON,R7),RECCON-RECD(R7)                         
         CLI   DEPOPT,C' '                                                      
         BE    PC70                                                             
         GOTO1 ASORTER,DMCB,=C'PUT',(R7)                                        
         SPACE 1                                                                
PC70     MVI   0(R7),4             DEPT. TASK SUMMARY                           
         MVC   RECCON-RECD(L'RECCON,R7),RECACC                                  
         CLI   SUBOPT,C' '                                                      
         BE    PC72                                                             
         GOTO1 ASORTER,DMCB,=C'PUT',(R7)                                        
         SPACE 1                                                                
PC72     MVI   0(R7),5                                                          
         MVC   RECCON-RECD(L'RECCON,R7),RECCON  PERSON                          
         XC    RECACC-RECD(L'RECACC,R7),RECACC-RECD(R7)                         
         L     R8,APERLEV                                                       
         USING LVD,R8                                                           
         ZIC   R3,LVTOT                                                         
         AH    R3,=H'2'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   RECACC-RECD(0,R7),RECCON    OFFICE DEPT                          
         LA    R2,2(R3,R7)                                                      
         MVC   0(2,R2),RECTSK              TASK                                 
         CLI   TSKOPT,C' '                                                      
         BE    PC72A                                                            
         GOTO1 ASORTER,DMCB,=C'PUT',(R7)                                        
         SPACE 1                                                                
PC72A    CLI   LEVASW,C'Y'                                                      
         BE    PC72B               ALREADY ADDED CLIENT NAME                    
         L     R4,ADHEIRA          ADD LEVEL A, B AND ACCOUNT CODES             
         MVC   WORK(15),0(R4)      AND NAMES TO CLIENT LIST                     
         L     R4,ADLVANAM                                                      
         LA    R6,WORK+15                                                       
         BAS   RE,NAMOUT                                                        
         GOTO1 ADDNME,DMCB,WORK,ACLILST                                         
         MVI   LEVASW,C'Y'                                                      
         SPACE 1                                                                
PC72B    CLI   LEVBSW,C'Y'                                                      
         BE    PC72C                                                            
         L     R4,ADHEIRB                                                       
         MVC   WORK(15),0(R4)                                                   
         L     R4,ADLVBNAM                                                      
         LA    R6,WORK+15                                                       
         BAS   RE,NAMOUT                                                        
         GOTO1 ADDNME,DMCB,WORK,ACLILST                                         
         MVI   LEVBSW,C'Y'                                                      
         SPACE 1                                                                
PC72C    CLI   LEVCSW,C'Y'                                                      
         BE    PC73                                                             
         L     R4,ADACC                                                         
         MVC   WORK(15),0(R4)                                                   
         L     R4,ADACCNAM                                                      
         LA    R6,WORK+15                                                       
         BAS   RE,NAMOUT                                                        
         GOTO1 ADDNME,DMCB,WORK,ACLILST                                         
         MVI   LEVCSW,C'Y'                                                      
         SPACE 1                                                                
PC73     DC    0H'0'                                                            
         B     PCXIT                                                            
         EJECT                                                                  
PC100    CLI   MODE,REQLAST                                                     
         BNE   PCXIT                                                            
         XC    LSTKEY,LSTKEY                                                    
         XC    THISREC,THISREC                                                  
KYNXT1   CLI   THISREC,X'FF'                                                    
         BE    KYNXTX              EOF                                          
KYNXT1A  GOTO1 ASORTER,DMCB,=C'GET'                                             
         MVI   THISREC,X'FF'                                                    
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    KYNXT2                                                           
         USING RECD,R6                                                          
         LA    R2,RECACCUM                                                      
         GOTO1 AGETVALS,DMCB,(RC),(R6),(R2)                                     
         CLI   VALS,C'Y'           ANY SIGNIFICANT DATA                         
         BNE   KYNXT1A             NO SO GET NEXT                               
         MVC   THISREC,0(R6)       SAVE THIS                                    
KYNXT2   CLI   LSTKEY,0                                                         
         BNE   KYNXT4                                                           
KYNXT3   MVC   LSTKEY,THISREC      FIRST TIME                                   
         XC    LSTPKEY,LSTPKEY                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         L     R4,VACCUMS                                                       
         LA    R3,10                                                            
         XC    0(L'ACCUMS,R4),0(R4)                                             
         LA    R4,L'ACCUMS(R4)                                                  
         BCT   R3,*-10                                                          
         L     R4,VACCUMS                                                       
         USING RECD,R6                                                          
         MVC   0(L'ACCUMS,R4),RECACCUM                                          
         B     KYNXT1                                                           
         SPACE 1                                                                
         USING RECD,R6                                                          
KYNXT4   LA    R6,LSTKEY                                                        
         ZIC   RE,RECTYP           LAST RECORD TYPE                             
         BCTR  RE,0                                                             
         LA    RF,L'TYPTAB                                                      
         MR    RE,RE                                                            
         LA    RE,TYPTAB(RF)                                                    
         CLI   NUMPLEV,3                                                        
         BE    *+8                                                              
         LA    RE,TYPTABA(RF)      USE A TABLE IF 1R IS 4 DEEP                  
         ZICM  R5,1(RE),4          A(SORTKEY LIST) FOR LAST TYPE                
         A     R5,RELO                                                          
         SPACE 1                                                                
         USING SRTKD,R5                                                         
KYNXT5   CLI   SRTKLEN,X'FF'                                                    
         BE    KYNXT3              END OF THIS TYPE - START AGAIN               
         LA    R7,LSTKEY                                                        
         LA    R6,THISREC                                                       
         ZIC   R3,SRTKLEN          LENGTH OF COMPARE                            
         BCTR  R3,0                                                             
         EX    R3,CLCKY                                                         
         BE    KYNXT6                                                           
         B     KYNXT10                                                          
CLCKY    CLC   0(0,R6),0(R7)                                                    
         SPACE 1                                                                
KYNXT6   CLI   SRTKLEV,0                                                        
         BNE   KYNXT9                                                           
         LA    R7,RECACCUM                                                      
         L     R8,VACCUMS                                                       
         LA    R3,L'ACCUMS                                                      
         SRL   R3,2                NUMBER OF ACCUMS                             
         BCTR  R3,0                DON'T COUNT AS TWO RECORDS                   
         SPACE 1                                                                
KYNXT8   L     RF,0(R7)                                                         
         A     RF,0(R8)                                                         
         ST    RF,0(R8)            ADD THISREC TO LEVEL ONE ACCUMS              
         LA    R7,4(R7)                                                         
         LA    R8,4(R8)                                                         
         BCT   R3,KYNXT8                                                        
         MVC   LSTKEY,THISREC                                                   
         B     KYNXT1                                                           
         SPACE 1                                                                
KYNXT9   MVC   LSTKEY,THISREC                                                   
         L     R4,VACCUMS                                                       
         USING RECD,R6                                                          
         MVC   0(L'ACCUMS,R4),RECACCUM                                          
         B     KYNXT1                                                           
         SPACE 1                                                                
KYNXT10  ZIC   R2,SRTKLEV                                                       
         LA    R3,L'ACCUMS                                                      
         MR    R2,R2                                                            
         L     R8,VACCUMS          R8 TO ACCUMS                                 
         AR    R8,R3                                                            
         GOTO1 AGETVALS,DMCB,(RC),LSTKEY,(R8)                                   
         CLI   VALS,C'Y'                                                        
         BNE   KYNXT12                                                          
         BAS   RE,PRNTIT           PRINT THIS LEVEL                             
         BAS   RE,ADDUP                                                         
KYNXT12  XC    0(L'ACCUMS,R8),0(R8)                                             
         LA    R5,SRTKLN(R5)        TABLE ENTRY FOR NEXT FIELD                  
         B     KYNXT5                                                           
         SPACE 2                                                                
KYNXTX   GOTO1 ASORTER,DMCB,=C'END'                                             
         B     PCXIT                                                            
         EJECT                                                                  
ADDUP    NTR1                                                                   
         ZIC   R2,SRTKLEV          LEVEL OF ACCUMS                              
         LA    R3,L'ACCUMS                                                      
         MR    R2,R2                                                            
         L     R8,VACCUMS          LEVEL I WANT TO ADD FROM                     
         AR    R8,R3                                                            
         LA    R7,L'ACCUMS(R8)     LEVEL I WANT TO ADD TO                       
         LA    R3,L'ACCUMS                                                      
         SRL   R3,2                ACCUMS/4                                     
         BCTR  R3,0                                                             
         SPACE 1                                                                
ADDUP2   L     RF,0(R7)            ADD'EM UP                                    
         A     RF,0(R8)                                                         
         ST    RF,0(R7)                                                         
         LA    R8,4(R8)                                                         
         LA    R7,4(R7)                                                         
         BCT   R3,ADDUP2                                                        
         L     RF,0(R7)                                                         
         A     RF,=F'1'                                                         
         ST    RF,0(R7)            COUNT ITEMS ADDED TO HIGHER LEVEL            
         B     PCXIT                                                            
         EJECT                                                                  
*              PRINT ROUTINE                                                    
         SPACE 1                                                                
         USING SRTKD,R5                                                         
         USING RECD,R6                                                          
         USING LVD,R8                                                           
PRNTIT   NTR1                                                                   
         LA    R6,LSTKEY                                                        
         CLI   RECTYP,2                                                         
         BH    PRNTD               DEPT. REPORTS                                
         SPACE 1                                                                
PRNTC    LA    R4,RECCON           SAVE ADDRESS OF 1R KEY                       
         ST    R4,PKEY                                                          
         LA    R4,RECACC                                                        
         CLI   SRTKTYP,C'C'                                                     
         BE    *+8                                                              
         LA    R4,RECCON                                                        
         CLI   SRTKTYP,C'T'                                                     
         BNE   PRNT50              NOT A LOW LEVEL                              
         L     R8,APERLEV                                                       
         ZIC   R2,LVDSP                                                         
         LA    R4,RECCON-RECD(R2,R6)    CONTRA                                  
         LA    R7,LSTPKEY               CONTRA OF LAST PRINTED                  
         LA    R7,RECCON-RECD(R2,R7)                                            
         MVI   LEVEL,1                                                          
         BAS   RE,PRNTUP                                                        
         SPACE 1                                                                
PRNTC10  ZIC   R3,LVLN                                                          
         LA    R8,LVLEN(R8)                                                     
         AR    R4,R3               NEXT FIELD IN PERSON KEY                     
         AR    R7,R3                                                            
         MVI   LEVEL,2                                                          
         BAS   RE,PRNTUP                                                        
         SPACE 1                                                                
PRNTC12  CLI   NUMPLEV,4                                                        
         BNE   PRNTC15                                                          
         ZIC   R3,LVLN                                                          
         LA    R8,LVLEN(R8)                                                     
         AR    R4,R3               NEXT FIELD IN PERSON KEY                     
         AR    R7,R3                                                            
         MVI   LEVEL,3                                                          
         BAS   RE,PRNTUP                                                        
         SPACE 1                                                                
PRNTC15  ZIC   R3,LVLN                                                          
         LA    R8,LVLEN(R8)                                                     
         AR    R4,R3               NEXT FIELD IN PERSON KEY                     
         AR    R7,R3                                                            
         ZIC   R3,LVLN                                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R7)       IS PERSON  THE SAME                          
         BE    PRNTC17                                                          
         L     R8,APERLEV                                                       
         ZIC   R3,LVNUM            NUMBER OF LOWEST LEVEL                       
         GOTO1 GTNME,DMCB,(C'P',RECCON),(R3)                                    
         MVC   P+1(9),CDA          PERSON CODE                                  
         GOTO1 CHOPPER,DMCB,(36,CDAN),(24,P+11),(C'P',2)                        
         SPACE 1                                                                
PRNTC17  GOTO1 GTNME,DMCB,(C'T',RECTSK)                                         
         MVC   P+36(15),CDAN       TASK NAME                                    
         GOTO1 AFORMAT,DMCB,(RC)                                                
         BAS   RE,HEADUP                                                        
         MVC   LSTPKEY,LSTKEY                                                   
         B     PRNTXIT                                                          
         SPACE 1                                                                
*              CHECK HIGHER LEVEL PRINT IF A CHANGE                             
PRNTUP   NTR1                                                                   
         ZIC   R3,LVLN                                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R7)                                                    
         BE    PCXIT               SAME DEPT                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R4)       UPDATE LSTPKEY                               
         LA    R3,2(R3,R7)                                                      
         MVI   0(R3),X'FF'         FORCE SUB-DEPT OF LAST TO NOT EQUAL          
         ZIC   R7,LEVEL                                                         
         L     R8,PKEY                                                          
         GOTO1 GTNME,DMCB,(C'P',(R8)),(R7)                                      
         MVC   P+1(48),CDANME                                                   
         GOTO1 SQUASHER,DMCB,P+1,48                                             
         GOTO1 UNDERLIN,DMCB,(48,P+1),(0,PSECOND+1)                             
         MVI   SPACING,2                                                        
         BAS   RE,HEADUP                                                        
         B     PCXIT                                                            
         EJECT                                                                  
*              PRINT DEPT. REPORTS                                              
PRNTD    CLI   RECTYP,5                                                         
         BE    PRNTR5                                                           
         LA    R4,RECACC           SAVE ADDRESS OF 1R KEY                       
         ST    R4,PKEY                                                          
         LA    R4,RECACC                                                        
         CLI   SRTKTYP,C'P'                                                     
         BE    *+8                                                              
         LA    R4,RECCON                                                        
         CLI   SRTKTYP,C'T'                                                     
         BNE   PRNT50              NOT A LOW LEVEL                              
         CLI   RECTYP,3                                                         
         BNE   PRNTD4                                                           
         L     R8,APERLEV                                                       
         LA    R8,LVLEN(R8)        SUB-DEPT LEVEL                               
         ZIC   R2,LVDSP                                                         
         LA    R4,RECACC-RECD(R2,R6)    ACCOUNT                                 
         LA    R7,LSTPKEY                                                       
         LA    R7,RECACC-RECD(R2,R7)    LAST PRINTED                            
         MVI   LEVEL,2                                                          
         BAS   RE,PRNTUP                                                        
         SPACE 1                                                                
         CLI   NUMPLEV,3                                                        
         BE    PRNTD4                                                           
         ZIC   R3,LVLN                                                          
         LA    R8,LVLEN(R8)                                                     
         AR    R4,R3               NEXT FIELD IN PERSON KEY                     
         AR    R7,R3                                                            
         MVI   LEVEL,3                                                          
         BAS   RE,PRNTUP                                                        
         SPACE 1                                                                
PRNTD4   L     R8,APERLEV                                                       
         L     R8,LVLOW                                                         
         ZIC   R2,LVDSP                                                         
         LA    R4,RECACC-RECD(R2,R6)    CONTRA                                  
         LA    R7,LSTPKEY               CONTRA OF LAST PRINTED                  
         LA    R7,RECACC-RECD(R2,R7)                                            
         ZIC   R3,LVLN                                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R7)                                                    
         BE    PRNTD7              SAME DEPT                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R4)       UPDATE LSTPKEY                               
         LA    R3,2(R3,R7)                                                      
         MVI   0(R3),X'FF'         FORCE SUB-DEPT OF LAST TO NOT EQUAL          
         ZIC   R3,LVNUM                                                         
         GOTO1 GTNME,DMCB,(C'P',RECACC),(R3)                                    
         MVC   P+1(48),CDANME                                                   
         GOTO1 SQUASHER,DMCB,P+1,48                                             
         CLI   RECTYP,3                                                         
         BE    PRNTD7                                                           
         GOTO1 UNDERLIN,DMCB,(48,P+1),(0,PSECOND+1)                             
         MVI   SPACING,2                                                        
         BAS   RE,HEADUP                                                        
         SPACE 1                                                                
PRNTD7   L     R8,ACLILEV                                                       
         ZIC   R2,LVDSP                                                         
         LA    R4,RECCON-RECD(R2,R6)    CONTRA IS CLIENT                        
         LA    R7,LSTPKEY                                                       
         LA    R7,RECCON-RECD(R2,R7)                                            
         ZIC   R3,LVLN                                                          
         LA    R8,LVLEN(R8)                                                     
         ZIC   R1,LVLN                  COMPARE CLI AND DIVISION                
         AR    R3,R1                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R7)                                                    
         BE    PRNTD10                  SAME CLEINT DIVISION                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R4)       UPDATE LSTPKEY                               
         MVC   P+1(3),0(R4)        CLIENT CODE                                  
         MVC   P+5(3),3(R4)       DIVISION                                      
         SPACE 1                                                                
PRNTD10  CLI   RECTYP,3                                                         
         BE    PRNTD15             NO PROJECT                                   
         L     R8,ACLILEV                                                       
         ZIC   R3,LVNUM            NUMBER OF LOWEST LEVEL                       
         GOTO1 GTNME,DMCB,(C'C',RECCON),(R3)                                    
         MVC   P+9(6),CDA                                                       
         GOTO1 CHOPPER,DMCB,(36,CDAN),(19,P+16),(C'P',2)                        
         SPACE 1                                                                
PRNTD15  GOTO1 GTNME,DMCB,(C'T',RECTSK)                                         
         MVC   P+36(15),CDAN                                                    
         GOTO1 AFORMAT,DMCB,(RC)                                                
         BAS   RE,HEADUP                                                        
         MVC   LSTPKEY,LSTKEY                                                   
         B     PRNTXIT                                                          
         EJECT                                                                  
*              REPORT 5                                                         
PRNTR5   LA    R4,RECCON                                                        
         ST    R4,PKEY                                                          
         LA    R4,RECCON                                                        
         CLI   SRTKTYP,C'T'                                                     
         BNE   PRNT50                                                           
         L     R8,APERLEV                                                       
         LA    R8,LVLEN(R8)        SUB-DEPT LEVEL                               
         ZIC   R2,LVDSP                                                         
         LA    R4,RECCON-RECD(R2,R6)    ACCOUNT                                 
         LA    R7,LSTPKEY                                                       
         LA    R7,RECCON-RECD(R2,R7)    LAST PRINTED                            
         MVI   LEVEL,2                                                          
         BAS   RE,PRNTUP                                                        
         SPACE 1                                                                
         CLI   NUMPLEV,3                                                        
         BE    PRNTR5A                                                          
         ZIC   R3,LVLN                                                          
         LA    R8,LVLEN(R8)                                                     
         AR    R4,R3               NEXT FIELD IN PERSON KEY                     
         AR    R7,R3                                                            
         MVI   LEVEL,3                                                          
         BAS   RE,PRNTUP                                                        
         SPACE 1                                                                
PRNTR5A  L     R8,APERLEV                                                       
         L     R8,LVLOW                                                         
         ZIC   R2,LVDSP                                                         
         LA    R4,RECCON-RECD(R2,R6)    CONTRA                                  
         LA    R7,LSTPKEY               CONTRA OF LAST PRINTED                  
         LA    R7,RECCON-RECD(R2,R7)                                            
         ZIC   R3,LVLN                                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R7)                                                    
         BE    PRNTR5C             SAME PERSON                                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R4)       UPDATE LSTPKEY                               
         LA    R3,2(R3,R7)                                                      
         MVI   0(R3),X'FF'         FORCE SUB-DEPT OF LAST TO NOT EQUAL          
         ZIC   R3,LVNUM                                                         
         GOTO1 GTNME,DMCB,(C'P',RECCON),(R3)                                    
         MVC   P+1(9),CDA                                                       
         GOTO1 CHOPPER,DMCB,(36,CDAN),(21,P+14),(C'P',2)                        
         SPACE 1                                                                
PRNTR5C  DC    0H'0'                                                            
         GOTO1 AFORMAT,DMCB,(RC)                                                
         BAS   RE,HEADUP                                                        
         MVC   LSTPKEY,LSTKEY                                                   
         B     PRNTXIT                                                          
         EJECT                                                                  
PRNT50   DC    0H'0'                                                            
         MVC   P+1(10),=C'TOTALS FOR'                                           
         CLI   SRTKTYP,C'R'                                                     
         BE    PRNT55              REQUEST                                      
         ZIC   R3,SRTKTYP                                                       
         ZIC   R7,SRTKFLD                                                       
         CLI   SRTKTYP,C'W'                                                     
         BNE   PRNT51                                                           
         SH    R3,=H'3'            W BECOMES A T                                
         LA    R4,RECTSK                                                        
PRNT51   GOTO1 GTNME,DMCB,((R3),(R4)),(R7)                                      
         GOTO1 CHOPPER,DMCB,(36,CDAN),(23,P+12),(C'P',2)                        
         CLI   LINE,51             IF END OF PAGE NO NEED TO SPACE              
         BH    *+8                                                              
         MVI   SPACING,2                                                        
         CLC   COUNT,=F'1'                                                      
         BE    PRNT52              NO TOTAL FOR ONE RECORD                      
         GOTO1 AFORMAT,DMCB,(RC)                                                
         B     PRNT53                                                           
         SPACE 1                                                                
PRNT52   MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVI   SPACING,1                                                        
         B     PRNT54                                                           
PRNT53   CLC   P,SPACES                                                         
         BNE   PRNT54                                                           
         CLC   PSECOND,SPACES                                                   
         BNE   PRNT54                                                           
         B     PRNT54A                                                          
PRNT54   BAS   RE,HEADUP                                                        
PRNT54A  MVC   LSTPKEY,LSTKEY                                                   
         ZIC   R3,SRTKLEN                                                       
         LR    R2,R3                                                            
         LH    R1,=H'32'                                                        
         SR    R1,R2                                                            
         LA    R3,LSTPKEY(R3)                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R3),0(R3)       CLEAR END OF LSTPKEY                         
         B     PRNTXIT                                                          
         SPACE 1                                                                
PRNT55   MVC   P+12(7),=C'REQUEST'                                              
PRNT57   GOTO1 AFORMAT,DMCB,(RC)                                                
         MVI   SPACING,2                                                        
         BAS   RE,HEADUP                                                        
         SPACE 1                                                                
PRNTXIT  DC    0H'0'                                                            
         B     PCXIT                                                            
         EJECT                                                                  
*              SET UP HEADLINE                                                  
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         LA    R6,LSTKEY                                                        
         USING RECD,R6                                                          
         ZIC   R2,RECTYP                                                        
         BCTR  R2,0                                                             
         LA    R3,L'SUBHD                                                       
         MR    R2,R2                                                            
         LA    R1,SUBHD(R3)                                                     
         MVC   HEAD3+41(L'SUBHD),0(R1)                                          
         SPACE 1                                                                
         ZIC   R2,RECTYP                                                        
         BCTR  R2,0                                                             
         LA    R3,L'HDFLDS*2                                                    
         MR    R2,R2                                                            
         LA    R1,HDFLDS(R3)                                                    
         MVC   HEAD9+52(L'HDFLDS),0(R1)                                         
         LA    R1,L'HDFLDS(R1)                                                  
         MVC   HEAD10+52(L'HDFLDS),0(R1)                                        
         MVC   HEAD5+84(L'PERD),PERD                                            
         MVC   HEAD4+1(7),=C'COMPANY'                                           
         SPACE 1                                                                
         ZIC   R1,RECTYP                                                        
         CLI   RECTYP,2                                                         
         BH    HEADD1              DEPT REPORT                                  
HEADC1   L     R8,ACLILEV                                                       
         ZIC   R3,LVLONG                                                        
         LA    R2,HEAD4+1                                                       
         LA    R2,2(R3,R2)                                                      
         MVC   0(36,R2),COMPNAM    COMPANY NAME                                 
         SPACE 1                                                                
         MVC   HEAD5+1(36),LVDES   LEVEL A DESCRIPTION                          
         GOTO1 GTNME,DMCB,(C'C',RECACC),1                                       
         LA    R2,HEAD5+1                                                       
         LA    R2,2(R3,R2)                                                      
         MVC   0(48,R2),CDANME     CODE AND NAME -CLENT                         
         GOTO1 SQUASHER,DMCB,(R2),48                                            
         SPACE 1                                                                
         LA    R8,LVLEN(R8)        LEVEL B                                      
         MVC   HEAD6+1(36),LVDES   DESCRIPTION                                  
         GOTO1 GTNME,DMCB,(C'C',RECACC),2                                       
         LA    R2,HEAD6+1                                                       
         LA    R2,2(R3,R2)                                                      
         MVC   0(48,R2),CDANME     DIV. CODE AND NAME                           
         GOTO1 SQUASHER,DMCB,(R2),48                                            
         CLI   RECTYP,2                                                         
         BE    HEADC2              CLIENT TASK SUMMARY                          
         SPACE 1                                                                
         LA    R8,LVLEN(R8)        LEVEL C                                      
         MVC   HEAD7+1(36),LVDES                                                
         GOTO1 GTNME,DMCB,(C'C',RECACC),3                                       
         LA    R2,HEAD7+1                                                       
         LA    R2,2(R3,R2)                                                      
         MVC   0(48,R2),CDANME     PROJECT                                      
         SPACE 1                                                                
HEADC2   GOTO1 SQUASHER,DMCB,(R2),48                                            
         CLI   SRTKTYP,C'T'                                                     
         BNE   HEADC4              NO NEW PAGE FOR TOTALS                       
         ZIC   R3,LVTOT            TOTAL LENGTH AT THIS LEVEL                   
         AH    R3,=H'3'                                                         
         EX    R3,*+8              IF REPORT 1 PAGE FOR PROJECT                 
         B     *+10                IF REPORT 2 PAGE FOR DIVISION                
         CLC   LSTKEY(0),LSTPKEY                                                
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         EX    R3,*+8                                                           
         B     HEADC4                                                           
         MVC   LSTPKEY(0),LSTKEY                                                
         SPACE 1                                                                
HEADC4   L     R8,APERLEV          1R LEDGER                                    
         MVC   HEAD9+1(15),LVDES   LEVEL A                                      
         LA    R8,LVLEN(R8)                                                     
         MVC   HEAD9+17(15),LVDES  AND  B                                       
         L     R8,LVLOW                                                         
         MVC   HEAD9+33(15),LVDES  PERSON                                       
         GOTO1 SQUASHER,DMCB,HEAD9+1,47                                         
         GOTO1 UNDERLIN,DMCB,(47,HEAD9+1),(0,HEAD10+1)                          
         MVC   HEAD9+35(5),=C' TASK'                                            
         MVC   HEAD10+35(5),=C' ----'                                           
         B     HEADX                                                            
         EJECT                                                                  
*              HEADLINES FOR DEPARTMENT REPORTS                                 
HEADD1   DC    0H'0'                                                            
         L     R8,APERLEV                                                       
         ZIC   R3,LVLONG                                                        
         LA    R2,HEAD4+1                                                       
         LA    R2,2(R3,R2)                                                      
         MVC   0(36,R2),COMPNAM    COMPANY NAME                                 
         SPACE 1                                                                
         MVC   HEAD5+1(36),LVDES   LEVEL A DESCRIPTION                          
         GOTO1 GTNME,DMCB,(C'P',RECACC),1                                       
         LA    R2,HEAD5+1                                                       
         LA    R2,2(R3,R2)                                                      
         MVC   0(48,R2),CDANME     DEPT. CODE AND NAME                          
         GOTO1 SQUASHER,DMCB,(R2),48                                            
         CLI   RECTYP,5                                                         
         BE    HEADR5                                                           
         CLI   RECTYP,3                                                         
         BE    HEADD2                                                           
         SPACE 1                                                                
         LA    R8,LVLEN(R8)        LEVEL B                                      
         MVC   HEAD6+1(36),LVDES                                                
         GOTO1 GTNME,DMCB,(C'P',RECACC),2                                       
         LA    R2,HEAD6+1                                                       
         LA    R2,2(R3,R2)                                                      
         MVC   0(48,R2),CDANME     SUB-DEPT CODE AND NAME                       
         GOTO1 SQUASHER,DMCB,(R2),48                                            
         SPACE 1                                                                
HEADD2   CLI   SRTKTYP,C'T'                                                     
         BNE   HEADD4              NO NEW PAGE FOR TOTALS                       
         ZIC   R3,LVTOT            TOTAL LENGTH AT THIS LEVEL                   
         AH    R3,=H'3'                                                         
         EX    R3,*+8              IF REPORT 3 PAGE FOR SUB-DEPT                
         B     *+10                IF REPORT 4 PAGE FOR DEPT                    
         CLC   LSTKEY(0),LSTPKEY                                                
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         EX    R3,*+8                                                           
         B     HEADD4                                                           
         MVC   LSTPKEY(0),LSTKEY                                                
         SPACE 1                                                                
HEADD4   L     R8,ACLILEV                                                       
         CLI   RECTYP,3                                                         
         BE    HEADD6                                                           
         MVC   HEAD9+1(15),LVDES   CLIENT                                       
         MVI   HEAD9+16,C'/'                                                    
         LA    R8,LVLEN(R8)                                                     
         MVC   HEAD9+17(15),LVDES                                               
         MVI   HEAD9+32,C'/'                                                    
         LA    R8,LVLEN(R8)                                                     
         MVC   HEAD9+33(15),LVDES                                               
         GOTO1 SQUASHER,DMCB,HEAD9+1,47                                         
         GOTO1 UNDERLIN,DMCB,(47,HEAD9+1),(0,HEAD10+1)                          
         MVC   HEAD9+35(5),=C' TASK'                                            
         MVC   HEAD10+35(5),=C' ----'                                           
         B     HEADX                                                            
         SPACE 1                                                                
HEADD6   L     R8,APERLEV                                                       
         LA    R8,LVLEN(R8)                                                     
         MVC   HEAD9+1(15),LVDES   SUB-DEPT                                     
         LA    R8,LVLEN(R8)                                                     
         MVC   HEAD9+17(15),LVDES  STAFF                                        
         GOTO1 SQUASHER,DMCB,HEAD9+1,47                                         
         GOTO1 UNDERLIN,DMCB,(47,HEAD9+1),(0,HEAD10+1)                          
         MVC   HEAD9+35(5),=C' TASK'                                            
         MVC   HEAD10+35(5),=C' ----'                                           
         B     HEADX                                                            
         SPACE 1                                                                
         EJECT                                                                  
HEADR5   DC    0H'0'                                                            
         MVC   HEAD9+36(4),SPACES                                               
         MVC   HEAD10+36(4),SPACES                                              
         L     R8,APERLEV                                                       
         LA    R8,LVLEN(R8)                                                     
         MVC   HEAD9+1(15),LVDES   SUB-DEPT                                     
         LA    R8,LVLEN(R8)                                                     
         MVC   HEAD9+17(15),LVDES  STAFF                                        
         GOTO1 SQUASHER,DMCB,HEAD9+1,47                                         
         GOTO1 UNDERLIN,DMCB,(47,HEAD9+1),(0,HEAD10+1)                          
         SPACE 1                                                                
         GOTO1 GTNME,DMCB,(C'T',RECTSK)                                         
         MVC   HEAD6+1(4),=C'TASK'                                              
         ZIC   R3,LVLONG                                                        
         LA    R2,HEAD6+1                                                       
         LA    R2,2(R3,R2)                                                      
         MVC   0(15,R2),CDAN                                                    
         CLI   SRTKTYP,C'T'                                                     
         BNE   HEADR5A                                                          
         L     R8,APERLEV                                                       
         ZIC   R3,LVTOT            TOTAL LENGTH AT THIS LEVEL                   
         AH    R3,=H'5'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   LSTKEY(0),LSTPKEY                                                
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       NEW PAGE FOR DEPT/TASK                       
         EX    R3,*+8                                                           
         B     HEADR5A                                                          
         MVC   LSTPKEY(0),LSTKEY                                                
         SPACE 1                                                                
HEADR5A  DC    0H'0'                                                            
         SPACE 1                                                                
HEADX    CLC   P,SPACES                                                         
         BNE   HEADX1                                                           
         CLI   PRTSW,C'Y'          DON'T PRINT TWO BLANK LINES                  
         BE    PCXIT                                                            
         MVI   PRTSW,C'Y'                                                       
         B     HEADX3                                                           
HEADX1   MVI   PRTSW,C'N'                                                       
HEADX3   GOTO1 ACREPORT                                                         
         B     PCXIT                                                            
         EJECT                                                                  
*              GET CODE AND NAME FOR ANY LEVEL                                  
         SPACE 1                                                                
*              PARAM 1   BYTE 0    T=TASK, C=CLIENT, P=PERSON                   
*                             1-3  A(KEY)                                       
*              PARAM 2   BYTE 3    LEVEL                                        
         SPACE 1                                                                
GTNME    NTR1                                                                   
         MVC   CDANME,SPACES                                                    
         CLI   0(R1),C'T'                                                       
         BE    GTNMET                                                           
         L     R8,ACLILEV          CLIENT LEDGER                                
         CLI   0(R1),C'C'                                                       
         BE    *+8                                                              
         L     R8,APERLEV          PERSON LEDGER                                
         L     R2,4(R1)            LEVEL                                        
         BCTR  R2,0                                                             
         LA    R3,LVLEN                                                         
         MR    R2,R2                                                            
         AR    R8,R3               ENTRY FOR THIS LEVEL                         
         SPACE 1                                                                
         USING LVD,R8                                                           
         ZIC   R4,LVDSP            DISP TO THIS LEVEL                           
         L     R2,0(R1)            A(KEY)                                       
         AR    R4,R2                                                            
         ZIC   R3,LVLN             LENGTH OF THIS                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   CDA(0),0(R4)        SIGNIFICANT PART OF KEY                      
         SPACE 1                                                                
         MVC   WORK,SPACES                                                      
         ZIC   R3,LVTOT            LENGTH OT END OF THIS LEVEL                  
         AH    R3,=H'2'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R2)       FULL KEY TO END OF THIS LEVEL                
         SPACE 1                                                                
         L     R3,ACLILST          CLIENT OR                                    
         CLI   0(R1),C'C'                                                       
         BE    *+8                                                              
         L     R3,APERLST          PERSON LIST                                  
         B     GTNMX                                                            
         SPACE 1                                                                
GTNMET   L     R2,0(R1)            TASK                                         
         MVC   CDA(2),0(R2)                                                     
         MVC   WORK,SPACES                                                      
         MVC   WORK(2),0(R2)                                                    
         L     R3,ATSKLST                                                       
         SPACE 1                                                                
GTNMX    GOTO1 GETNME,DMCB,WORK,(R3)                                            
         L     R3,DMCB             GET NAME AND MOVE TO CDA                     
         USING NAMD,R3                                                          
         MVC   CDAN,NAME                                                        
         B     PCXIT                                                            
         EJECT                                                                  
*              ROUTINE TO BUILD LEVEL DESCRIPTION                               
         USING ACHEIRD,R4                                                       
         USING LVD,R8                                                           
LEVBLD   NTR1                                                                   
         L     R4,0(R1)            A(HIERARCHY ELEMENT)                         
         L     R8,4(R1)            A(LEVEL CSECT)                               
         ST    R8,FULL                                                          
         LA    R2,ACHRLEVA                                                      
         SR    R3,R3                                                            
         MVI   WORK,7              LONGEST DESCRIPTION                          
         SR    R0,R0                                                            
         LA    R5,3                                                             
         SPACE 1                                                                
LEVBLD2  CLI   0(R2),0                                                          
         BE    LEVBLD4                                                          
         STC   R5,LVDSP            DISP TO THIS LEVEL                           
         MVC   LVTOT,0(R2)         CUMULATIVE LENGTH                            
         MVC   LVDES,1(R2)                                                      
         OC    LVDES,SPACES                                                     
         GOTO1 SQUASHER,DMCB,LVDES,15                                           
         ZIC   R6,DMCB+7                                                        
         ZIC   R7,WORK                                                          
         CR    R6,R7                                                            
         BL    *+8                                                              
         STC   R6,WORK             SAVE LONGEST DESCRIPTION                     
         ZIC   R7,LVTOT                                                         
         SR    R7,R3                                                            
         STC   R7,LVLN             LENGTH OF THIS LEVEL                         
         ZIC   R3,LVTOT            SAVE FOR NEXT TIME                           
         AR    R5,R7               GET START OF NEXT                            
         AH    R0,=H'1'            COUNT LEVELS                                 
         CH    R0,=H'4'                                                         
         BE    LEVBLD4                                                          
         LA    R2,16(R2)                                                        
         LA    R8,LVLEN(R8)                                                     
         B     LEVBLD2                                                          
LEVBLD4  L     R8,FULL             START OF LEV TABLE                           
         LR    R3,R0               NUMBER OF LEVELS                             
         LA    R7,LVLEN            LENGTH OF TABLE                              
         BCTR  R3,0                                                             
         SR    R2,R2               NUMBER - 1 X LENGTH                          
         MR    R2,R7                                                            
         LA    R7,0(R3,R8)         R7 TO LOWEST LEVEL                           
         LR    R3,R0                                                            
         SPACE 1                                                                
LEVBLD5  MVC   LVLONG,WORK         LONGEST DESCRIPTION                          
         STC   R0,LVNUM            NUMBER OF LEVELS                             
         ST    R7,LVLOW            A(LOWEST LEVEL)                              
         LA    R8,LVLEN(R8)                                                     
         BCT   R3,LEVBLD5                                                       
         B     PCXIT                                                            
         EJECT                                                                  
*              ROUTINE TO ADD 1R CODES AND NAMES                                
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
RDPER    NTR1                                                                   
         L     R4,AREC                                                          
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'1R'                                             
         MVI   ACKEYACC+3,X'41'                                                 
RDPER1   BAS   RE,HIGH                                                          
RDPER2   CLC   ACKEYACC(3),SAVEKEY                                              
         BNE   PCXIT                                                            
         L     R8,APERLEV                                                       
         USING LVD,R8                                                           
         L     R8,LVLOW                                                         
         ZIC   R1,LVDSP            DISP. TO LOWEST ACCOUNT                      
         AR    R1,R4                                                            
         CLI   0(R1),C' '                                                       
         BE    RDPER3              NOT A LOW LEVEL                              
         BCTR  R1,0                                                             
         ZIC   R3,0(R1)            LOWEST LEVEL                                 
         AH    R3,=H'1'            SO SKIP TO NEXT HIGH                         
         STC   R3,0(R1)                                                         
         MVC   1(42,R1),SPACES                                                  
         B     RDPER1                                                           
         SPACE 1                                                                
RDPER3   MVC   WORK(15),ACKEYACC                                                
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            GET NAME                                     
         BNE   RDPER5              ADD CODE AND NAME TO PERSON LIST             
         LA    R6,WORK+15                                                       
         BAS   RE,NAMOUT                                                        
         GOTO1 ADDNME,DMCB,WORK,APERLST                                         
         SPACE 1                                                                
RDPER5   L     R4,AREC                                                          
         BAS   RE,SEQ                                                           
         B     RDPER2                                                           
         EJECT                                                                  
*              ROUTINE TO ADD / GET NAMES FROM NAME LIST                        
         SPACE 1                                                                
         USING BIND,R5                                                          
ADDNME   NTR1                                                                   
         L     R2,0(R1)            A(RECORD TO BE ADDED)                        
         L     R5,4(R1)            A(NAME LIST)                                 
         MVC   DMCB+8(16),BININ                                                 
         LA    R3,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,0(R2)),(R3)                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         B     PCXIT                                                            
         SPACE 1                                                                
GETNME   NTR1                                                                   
         L     R2,0(R1)            A(KEY)                                       
         L     R5,4(R1)            A(NAME LIST)                                 
         MVC   DMCB+8(16),BININ                                                 
         LA    R3,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(R2),(R3)                                           
         CLI   DMCB,0                                                           
         BE    *+12                                                             
         LA    R1,SPACES           NOT FOUND                                    
         ST    R1,DMCB                                                          
         B     PCXIT                                                            
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 1                                                                
         USING ACNAMED,R4                                                       
NAMOUT   LTR   R4,R4                                                            
         BZR   RE                                                               
         MVC   0(36,R6),SPACES                                                  
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R6),ACNMNAME                                                 
         EJECT                                                                  
*              DATAMGR INTERFACE                                                
         SPACE 1                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   SAVEKEY,0(R4)                                                    
         B     GTREC                                                            
         SPACE 1                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
GTREC    NTR1                                                                   
         L     R4,AREC                                                          
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R4),(R4)                       
         B     PCXIT                                                            
         EJECT                                                                  
*              CONSTANTS                                                        
RELOTAB  DS    0A                                                               
         DC    V(SQUASHER)                                                      
         DC    V(UNDERLIN)                                                      
         DC    A(CLILST)                                                        
         DC    A(PERLST)                                                        
         DC    A(TSKLST)                                                        
         DC    A(SORTC)                                                         
         DC    A(CLILEV)                                                        
         DC    A(PERLEV)                                                        
         DC    A(RECORD)                                                        
         DC    V(SORTER)                                                        
         DC    A(ACCUMS)                                                        
         DC    A(GETVALS)                                                       
         DC    A(FORMAT)                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
SRTK1    DC    AL1(33),AL1(0),C'T',AL1(0)                                       
         DC    AL1(31),AL1(1),C'P',AL1(3)                                       
         DC    AL1(24),AL1(2),C'P',AL1(2)                                       
         DC    AL1(22),AL1(3),C'P',AL1(1)                                       
         DC    AL1(16),AL1(4),C'C',AL1(3)                                       
         DC    AL1(10),AL1(5),C'C',AL1(2)                                       
         DC    AL1(07),AL1(6),C'C',AL1(1)                                       
         DC    AL1(01),AL1(7),C'R',AL1(0)                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
SRTK2    DC    AL1(33),AL1(0),C'T',AL1(0)                                       
         DC    AL1(31),AL1(1),C'P',AL1(3)                                       
         DC    AL1(24),AL1(2),C'P',AL1(2)                                       
         DC    AL1(22),AL1(3),C'P',AL1(1)                                       
         DC    AL1(10),AL1(4),C'C',AL1(2)                                       
         DC    AL1(07),AL1(5),C'C',AL1(1)                                       
         DC    AL1(01),AL1(6),C'R',AL1(0)                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
SRTK3    DC    AL1(33),AL1(0),C'T',AL1(0)                                       
         DC    AL1(16),AL1(1),C'P',AL1(3)                                       
         DC    AL1(09),AL1(2),C'P',AL1(2)                                       
         DC    AL1(07),AL1(3),C'P',AL1(1)                                       
         DC    AL1(01),AL1(4),C'R',AL1(0)                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
SRTK4    DC    AL1(33),AL1(0),C'T',AL1(0)                                       
         DC    AL1(31),AL1(1),C'C',AL1(3)                                       
         DC    AL1(25),AL1(2),C'C',AL1(2)                                       
         DC    AL1(22),AL1(3),C'C',AL1(1)                                       
         DC    AL1(16),AL1(4),C'P',AL1(3)                                       
         DC    AL1(09),AL1(5),C'P',AL1(2)                                       
         DC    AL1(07),AL1(6),C'P',AL1(1)                                       
         DC    AL1(01),AL1(7),C'R',AL1(0)                                       
         DC    X'FF'                                                            
         EJECT                                                                  
SRTK5    DC    AL1(33),AL1(0),C'T',AL1(0)                                       
         DC    AL1(24),AL1(1),C'P',AL1(2)                                       
         DC    AL1(09),AL1(2),C'W',AL1(0)                                       
         DC    AL1(07),AL1(3),C'P',AL1(1)                                       
         DC    AL1(01),AL1(4),C'R',AL1(0)                                       
         DC    X'FF'                                                            
         SPACE 3                                                                
SRTK1A   DC    AL1(33),AL1(0),C'T',AL1(0)                                       
         DC    AL1(31),AL1(1),C'P',AL1(4)                                       
         DC    AL1(24),AL1(2),C'P',AL1(3)                                       
         DC    AL1(22),AL1(3),C'P',AL1(2)                                       
         DC    AL1(20),AL1(4),C'P',AL1(1)                                       
         DC    AL1(16),AL1(5),C'C',AL1(3)                                       
         DC    AL1(10),AL1(6),C'C',AL1(2)                                       
         DC    AL1(07),AL1(7),C'C',AL1(1)                                       
         DC    AL1(01),AL1(8),C'R',AL1(0)                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
SRTK2A   DC    AL1(33),AL1(0),C'T',AL1(0)                                       
         DC    AL1(31),AL1(1),C'P',AL1(4)                                       
         DC    AL1(24),AL1(2),C'P',AL1(3)                                       
         DC    AL1(22),AL1(3),C'P',AL1(2)                                       
         DC    AL1(20),AL1(4),C'P',AL1(1)                                       
         DC    AL1(10),AL1(5),C'C',AL1(2)                                       
         DC    AL1(07),AL1(6),C'C',AL1(1)                                       
         DC    AL1(01),AL1(7),C'R',AL1(0)                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
SRTK3A   DC    AL1(33),AL1(0),C'T',AL1(0)                                       
         DC    AL1(16),AL1(1),C'P',AL1(4)                                       
         DC    AL1(09),AL1(2),C'P',AL1(3)                                       
         DC    AL1(07),AL1(3),C'P',AL1(2)                                       
         DC    AL1(05),AL1(4),C'P',AL1(1)                                       
         DC    AL1(01),AL1(5),C'R',AL1(0)                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
SRTK4A   DC    AL1(33),AL1(0),C'T',AL1(0)                                       
         DC    AL1(31),AL1(1),C'C',AL1(3)                                       
         DC    AL1(25),AL1(2),C'C',AL1(2)                                       
         DC    AL1(22),AL1(3),C'C',AL1(1)                                       
         DC    AL1(16),AL1(4),C'P',AL1(4)                                       
         DC    AL1(09),AL1(5),C'P',AL1(3)                                       
         DC    AL1(07),AL1(6),C'P',AL1(2)                                       
         DC    AL1(05),AL1(7),C'P',AL1(1)                                       
         DC    AL1(01),AL1(8),C'R',AL1(0)                                       
         DC    X'FF'                                                            
         EJECT                                                                  
SRTK5A   DC    AL1(33),AL1(0),C'T',AL1(0)                                       
         DC    AL1(24),AL1(1),C'P',AL1(2)                                       
         DC    AL1(09),AL1(2),C'W',AL1(0)                                       
         DC    AL1(07),AL1(3),C'P',AL1(1)                                       
         DC    AL1(01),AL1(4),C'R',AL1(0)                                       
         DC    X'FF'                                                            
         EJECT                                                                  
SORTCARD DC    CL80'SORT FIELDS=(1,36,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(136,,136)'                            
         SPACE 1                                                                
TYPTAB   DS    0CL5                                                             
         DC    X'01',AL4(SRTK1)                                                 
         DC    X'02',AL4(SRTK2)                                                 
         DC    X'03',AL4(SRTK3)                                                 
         DC    X'04',AL4(SRTK4)                                                 
         DC    X'05',AL4(SRTK5)                                                 
         SPACE 1                                                                
TYPTABA  DS    0CL5                FOR 1R LEDGER FOUR DEEP                      
         DC    X'01',AL4(SRTK1A)                                                
         DC    X'02',AL4(SRTK2A)                                                
         DC    X'03',AL4(SRTK3A)                                                
         DC    X'04',AL4(SRTK4A)                                                
         DC    X'05',AL4(SRTK5A)                                                
         SPACE 1                                                                
SUBHD    DS    0CL22                                                            
         DC    C'  (PROJECT ANALYSIS)  '                                        
         DC    C' (DIVISION ANALYSIS)  '                                        
         DC    C'(DEPARTMENT ANALYSIS) '                                        
         DC    C'  (SUB-DEPT ANALYSIS) '                                        
         DC    C'   (TASK ANALYSIS)    '                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GETVALS                                               
         SPACE 1                                                                
         USING SRTKD,R5                                                         
         USING RECD,R6                                                          
GETVALS  CSECT                                                                  
         NMOD1 0,**VALS**                                                       
         L     RC,0(R1)                                                         
         L     R6,4(R1)            A(KEY) FOR THESE ACCUMS                      
         L     R8,8(R1)            A(ACCUMS)                                    
         XC    PERTOTS,PERTOTS                                                  
         XC    YTDTOTS,YTDTOTS                                                  
         XC    MNTHTOTS,MNTHTOTS                                                
         MVC   COUNT,96(R8)                                                     
         SPACE 1                                                                
         LR    R7,R8                                                            
         ZIC   R0,YTDMTHS          NUMBER OF MONTHS                             
         SR    RE,RE               YTD HOURS/COST                               
         SR    RF,RF                                                            
         SR    R1,R1               PERIOD HOURS/COST                            
         SR    R2,R2                                                            
         LA    R3,YTDPDTE                                                       
         LA    R4,PERPDTE                                                       
         SPACE 1                                                                
GETVAL2  A     RE,0(R7)            HOURS YTD                                    
         A     RF,4(R7)            COST YTD                                     
         CLC   0(3,R4),0(R3)       PERIOD MONTH VS YTD MONTH                    
         BNE   GETVAL4                                                          
         A     R1,0(R7)            HOURS FOR PERIOD                             
         A     R2,4(R7)           COST FOR PERIOD                               
         LA    R4,3(R4)                                                         
GETVAL4  LA    R3,3(R3)                                                         
         LA    R7,8(R7)                                                         
         BCT   R0,GETVAL2                                                       
         STM   RE,RF,YTDTOTS                                                    
         STM   R1,R2,PERTOTS                                                    
         SPACE 1                                                                
         LR    R7,R8                                                            
         ZIC   R0,PERMTHS                                                       
         LA    R3,YTDPDTE                                                       
         LA    R4,PERPDTE                                                       
GETVAL6  CLC   0(3,R4),0(R3)                                                    
         BE    GETVAL8                                                          
         LA    R7,8(R7)                                                         
         LA    R3,3(R3)                                                         
         B     GETVAL6                                                          
GETVAL8  ZIC   R1,FRSTBUCK         NUMBER IN FIRST BUCKET                       
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R5,MNTHTOTS                                                      
         XC    MNTHTOTS,MNTHTOTS                                                
GETVAL10 A     R2,0(R7)            HOURS                                        
         A     R3,4(R7)            COST                                         
         A     RE,0(R7)            HOURS/COST TOTALS                            
         A     RF,4(R7)                                                         
         STM   R2,R3,0(R5)         SAVE MONTH                                   
         STM   RE,RF,8(R5)         AND TOTALS                                   
         LA    R7,8(R7)                                                         
         SH    R0,=H'1'                                                         
         BCT   R1,GETVAL10         NUMBER IN FIRST BUCKET                       
GETVAL12 CH    R0,=H'0'                                                         
         BE    GETVAL13                                                         
         LA    R5,8(R5)                                                         
         MVC   0(8,R5),0(R7)                                                    
         A     RE,0(R7)                                                         
         A     RF,4(R7)                                                         
         STM   RE,RF,8(R5)                                                      
         LA    R7,8(R7)                                                         
         BCT   R0,GETVAL12                                                      
         SPACE 1                                                                
         USING RECD,R6                                                          
GETVAL13 MVI   VALS,C'N'                                                        
         ZIC   RE,RECTYP                                                        
         BCTR  RE,0                                                             
         LA    R1,PRJOPT(RE)                                                    
         CLI   0(R1),C'1'                                                       
         BE    GETVAL14                                                         
         CLI   0(R1),C'2'                                                       
         BE    GETVAL16                                                         
         CLI   0(R1),C'3'                                                       
         BE    GETVAL18                                                         
         CLI   0(R1),C'4'                                                       
         BE    GETVAL20                                                         
         DC    H'0'                                                             
         SPACE 1                                                                
GETVAL14 OC    PERTOTS,PERTOTS                                                  
         BNZ   GETVYES                                                          
         OC    YTDTOTS,YTDTOTS                                                  
         BNZ   GETVYES                                                          
         B     GETVNO                                                           
         SPACE 1                                                                
GETVAL16 OC    PERTOTS(4),PERTOTS                                               
         BNZ   GETVYES                                                          
         OC    YTDTOTS,YTDTOTS                                                  
         BNZ   GETVYES                                                          
         B     GETVNO                                                           
         SPACE 1                                                                
GETVAL18 OC    PERTOTS+4(4),PERTOTS+4                                           
         BNZ   GETVYES                                                          
         OC    YTDTOTS+4(4),YTDTOTS+4                                           
         BNZ   GETVYES                                                          
         B     GETVNO                                                           
         SPACE 1                                                                
GETVAL20 OC    MNTHTOTS,MNTHTOTS                                                
         BZ    GETVNO                                                           
         SPACE 1                                                                
GETVYES  MVI   VALS,C'Y'                                                        
GETVNO   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FORMAT VALUES                                         
         SPACE 1                                                                
         USING SRTKD,R5                                                         
         USING RECD,R6                                                          
FORMAT   CSECT                                                                  
         NMOD1 0,**FRMT**                                                       
         L     RC,0(R1)                                                         
         LA    R6,LSTKEY                                                        
         ZIC   RE,RECTYP                                                        
         BCTR  RE,0                                                             
         LA    R1,PRJOPT(RE)                                                    
         CLI   0(R1),C'1'                                                       
         BE    FORMAT1                                                          
         CLI   0(R1),C'2'                                                       
         BE    FORMAT2                                                          
         CLI   0(R1),C'3'                                                       
         BE    FORMAT3                                                          
         CLI   0(R1),C'4'                                                       
         BE    FORMAT4                                                          
         DC    H'0'                                                             
         SPACE 1                                                                
FORMAT1  LA    R3,PERTOTS                                                       
         EDIT  (B4,0(R3)),(11,P+52),2,MINUS=YES,ZERO=BLANK                      
         EDIT  (B4,4(R3)),(11,P+64),2,MINUS=YES,ZERO=BLANK                      
         LA    R3,YTDTOTS                                                       
         EDIT  (B4,0(R3)),(11,P+87),2,MINUS=YES,ZERO=BLANK                      
         EDIT  (B4,4(R3)),(11,P+99),2,MINUS=YES,ZERO=BLANK                      
         B     FORMXIT                                                          
         SPACE 1                                                                
FORMAT2  EDIT  (B4,PERTOTS),(11,P+58),2,MINUS=YES,ZERO=BLANK                    
         EDIT  (B4,YTDTOTS),(11,P+93),2,MINUS=YES,ZERO=BLANK                    
         B     FORMXIT                                                          
         SPACE 1                                                                
FORMAT3  LA    R3,PERTOTS+4                                                     
         EDIT  (B4,0(R3)),(11,P+58),2,MINUS=YES,ZERO=BLANK                      
         LA    R3,YTDTOTS+4                                                     
         EDIT  (B4,0(R3)),(11,P+93),2,MINUS=YES,ZERO=BLANK                      
         B     FORMXIT                                                          
         SPACE 1                                                                
FORMAT4  CLI   PROGPROF+1,C'Y'                                                  
         BE    FORMAT5             SHOW DECIMAL PLACES                          
         LA    R3,P+52                                                          
         LA    R4,8                                                             
         LA    R5,MNTHTOTS                                                      
FORMAT4A L     RF,0(R5)                                                         
         A     RF,=F'50'                                                        
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BZ    FORMAT4B                                                         
         EDIT  (RF),(7,0(R3)),ZERO=BLANK,FLOAT=-                                
FORMAT4B LA    R3,7(R3)                                                         
         LA    R5,8(R5)                                                         
         BCT   R4,FORMAT4A                                                      
         B     FORMXIT                                                          
         SPACE 1                                                                
FORMAT5  LA    R3,P+52                                                          
         LA    R4,8                                                             
         LA    R5,MNTHTOTS                                                      
FORMAT5A L     RF,0(R5)                                                         
         LTR   RF,RF                                                            
         BZ    FORMAT5B                                                         
         EDIT  (RF),(7,0(R3)),2,ZERO=BLANK,FLOAT=-                              
FORMAT5B LA    R3,7(R3)                                                         
         LA    R5,8(R5)                                                         
         BCT   R4,FORMAT5A                                                      
         B     FORMXIT                                                          
         SPACE 1                                                                
FORMXIT  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR WORKING STORAGE                                        
ACPC02D  DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
SQUASHER DS    V                                                                
UNDERLIN DS    V                                                                
ACLILST  DS    A                                                                
APERLST  DS    A                                                                
ATSKLST  DS    A                                                                
ASORTC   DS    A                                                                
ACLILEV  DS    A                                                                
APERLEV  DS    A                                                                
AREC     DS    A                                                                
ASORTER  DS    A                                                                
VACCUMS  DS    A                                                                
AGETVALS DS    A                                                                
AFORMAT  DS    A                                                                
         SPACE 1                                                                
ELCODE   DS    CL1                                                              
COMMAND  DS    CL6                                                              
SAVEKEY  DS    CL42                                                             
         SPACE 1                                                                
HDFLDS   DS    0CL57                                                            
HDFLD1   DS    CL57   ------JUL82------     ------Y.T.D------                   
HDFLD1A  DS    CL57   HOURS        COST     HOURS        COST                   
HDFLD2   DS    CL57   ------JUL82------     ------Y.T.D------                   
HDFLD2A  DS    CL57         HOURS                 HOURS                         
HDFLD3   DS    CL57   ---MAY82-JUL82---     ------Y.T.D------                   
HDFLD3A  DS    CL57         COST                  COST                          
HDFLD4   DS    CL57   JAN-JUN  JUL  AUG  SEP  OCT  NOV  DEC TOTAL               
HDFLD4A  DS    CL57   -------  ---  ---  ---  ---  ---  --- -----               
HDFLD5   DS    CL57                                                             
HDFLD5A  DS    CL57                                                             
         SPACE 1                                                                
PERD     DC    CL26'FOR THE PERIOD JAN82-DEC82'                                 
         SPACE 1                                                                
HDOP1    DS    CL57                                                             
HDOP2    DS    CL57                                                             
HDOP3    DS    CL57                                                             
HDOP4    DS    CL57                                                             
HDOP4A   DS    CL57                                                             
         SPACE 1                                                                
         DS    0F                                                               
THISREC  DS    CL136               RECORD FROM SORT                             
         DS    0F                                                               
SRTREC   DS    CL136                                                            
LSTKEY   DS    CL36                LAST KEY FROM SORT                           
LSTPKEY  DS    CL36                LAST KEY PRINTED                             
         SPACE 1                                                                
CDANME   DS    CL48                                                             
         ORG   CDANME                                                           
CDA      DS    CL12                CODE                                         
CDAN     DS    CL36                NAME                                         
         SPACE 1                                                                
PERPDTE  DS    CL36                12 PACKED DATES FOR PERIOD                   
PERMDTE  DS    CL24                12 MOS DATES FOR PERIOD                      
PERMTHS  DS    CL1                 NUMBER OF MONTHS                             
YTDPDTE  DS    CL36                SAME FOR Y.T.D.                              
YTDMDTE  DS    CL24                                                             
YTDMTHS  DS    XL1                                                              
END      DS    CL3                 END DATE PACKED                              
FRSTBUCK DS    CL1                 MONTHS IN FIRST BUCKET                       
SVSBAC   DS    CL60                SAVE SUBACC ELEMENT                          
         SPACE 1                                                                
YTDTOTS  DS    D                                                                
PERTOTS  DS    D                                                                
MNTHTOTS DS    CL64                16F                                          
COUNT    DS    F                                                                
         SPACE 1                                                                
COMPNAM  DS    CL36                COMPANY NAME                                 
         SPACE 1                                                                
PRJOPT   DS    CL1                                                              
DIVOPT   DS    CL1                                                              
DEPOPT   DS    CL1                                                              
SUBOPT   DS    CL1                                                              
TSKOPT   DS    CL1                                                              
         SPACE 1                                                                
SVANAL   DS    CL2                                                              
VALS     DS    CL1                                                              
FILTLEN  DS    CL1                                                              
WANT     DS    CL1                                                              
PRTSW    DS    CL1                                                              
NUMPLEV  DS    CL1                                                              
LEVEL    DS    CL1                                                              
LEVASW   DS    CL1                                                              
LEVBSW   DS    CL1                                                              
LEVCSW   DS    CL1                                                              
PKEY     DS    F                                                                
         EJECT                                                                  
*              DSECT FOR LEVEL DESCRIPTION AND CODES                            
LVD      DSECT                                                                  
LVLONG   DS    CL1                 LONGEST DESCRIPTION                          
LVNUM    DS    CL1                 NUMBER OF LEVELS                             
         DS    CL2                 SPARE                                        
LVLOW    DS    A                   A(LOWEST LEVEL)                              
LVDSP    DS    CL1                 DISP. TO THIS LEVEL                          
LVLN     DS    CL1                 LENGTH OF THIS LEVEL                         
LVTOT    DS    CL1                 TOTAL LENGTH SO FAR                          
LVDES    DS    CL15                DESCRIPTION                                  
         DS    CL2                 SPARE                                        
LVLEN    EQU   *-LVLONG                                                         
         SPACE 2                                                                
*              DSECT FOR A NAME RECORD                                          
NAMD     DSECT                                                                  
NAMCDE   DS    CL15                CODE                                         
NAME     DS    CL36                NAME                                         
NAMLEN   EQU   *-NAMCDE                                                         
         SPACE 2                                                                
*              DSECT FOR BINSRCH PARAMETERS                                     
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISP. TO KEY                                 
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAX. IN TABLE                                
BINTABLE DS    0CL1                TABLE                                        
         EJECT                                                                  
*              DSECT FOR SORTKEY FIELDS                                         
SRTKD    DSECT                                                                  
SRTKLEN  DS    CL1                 LENGTH OF FIELD                              
SRTKLEV  DS    CL1                 LEVEL NUMBER IN ACCUMS                       
SRTKTYP  DS    CL1                 R=REQUEST , P=PERSON,C=CLIENT,T=TASK         
SRTKFLD  DS    CL1                 LEVEL NUMBER IN LEDGER A=1,B=2,ETC           
SRTKLN   EQU   *-SRTKLEN                                                        
         SPACE 2                                                                
*              DSECT FOR INPUT RECORD                                           
RECD     DSECT                                                                  
RECTYP   DS    CL1                 TYPE                                         
RECACC   DS    CL15                ACCOUNT                                      
RECCON   DS    CL15                CONTRA                                       
RECTSK   DS    CL2                 TASK                                         
         DS    CL3                 SPARE                                        
RECACCUM DS    CL100               12 MONTHS  HOURS/COST                        
         ORG   RECACCUM+96                                                      
RECCOUNT DS    F                   COUNT NUMBER IN EACH LEVEL                   
RECLEN   EQU   *-RECTYP                                                         
         EJECT                                                                  
ACPC02   CSECT                                                                  
         ENTRY CLILST                                                           
CLILST   DS    0D                  CLIENT LIST                                  
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(NAMLEN)         RECORD LENGTH                                
         DC    AL4(L'NAMCDE)       DISP.TOKEY / KEY LENGTH                      
         DC    F'4000'             MAX.                                         
         DS    (4000*NAMLEN)C      CLI/DIV/PROJECT  CODE AND NAME               
         SPACE 1                                                                
         ENTRY PERLST                                                           
PERLST   DS    0D                  PERSON LIST                                  
         DC    F'0'                                                             
         DC    AL4(NAMLEN)                                                      
         DC    AL4(L'NAMCDE)                                                    
         DC    F'5000'                                                          
         DS    (5000*NAMLEN)C      OFF/DEPT/SUB/PERSON CODE AND NAME            
         SPACE 1                                                                
         ENTRY TSKLST                                                           
TSKLST   DS    0D                                                               
         DC    F'0'                                                             
         DC    AL4(NAMLEN)                                                      
         DC    AL4(L'NAMCDE)                                                    
         DC    F'450'                                                           
         DS    (450*NAMLEN)C       TASK CODE AND NAME                           
         SPACE 1                                                                
         ENTRY SORTC                                                            
SORTC    DS    0D                                                               
         DS    41000C                                                           
         SPACE 1                                                                
         ENTRY CLILEV                                                           
CLILEV   DS    0D                                                               
         DS    (4*LVLEN)C                                                       
         SPACE 1                                                                
         ENTRY PERLEV                                                           
PERLEV   DS    0D                                                               
         DS    (4*LVLEN)C                                                       
         SPACE 1                                                                
         ENTRY RECORD                                                           
RECORD   DS    0D                                                               
         DS    CL1000                                                           
         SPACE 1                                                                
         ENTRY ACCUMS                                                           
ACCUMS   DS    10CL100                                                          
         SPACE 2                                                                
*ACGENBOTH                                                                      
*ACREPWORKD                                                                     
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059ACREPPC02 03/23/15'                                      
         END                                                                    
