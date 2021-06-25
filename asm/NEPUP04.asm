*          DATA SET NEPUP04    AT LEVEL 026 AS OF 05/01/02                      
*          DATA SET NEPUP04    AT LEVEL 045 AS OF 05/06/91                      
*PHASE T32204A,*                                                                
         TITLE 'T32204 - PLAN VALUE'                                            
T32204   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32204**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   VAL2                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
VAL2     CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE 'RECORD' FOR REPORT                                     
         SPACE 3                                                                
VKEY     NTR1                                                                   
         SPACE 1                                                                
         LA    R2,PUPCLIH          CLIENT (REQUIRED)                            
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         SPACE 1                                                                
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         L     R4,AIO                                                           
         USING NPLRECD,R4                                                       
         OI    PUPGUARH+6,X'80'                                                 
         MVC   PUPGUAR,SPACES                                                   
         OC    GUARCPM,GUARCPM                                                  
         BZ    VKEY1                                                            
         LA    R2,PUPGUAR                                                       
         MVC   0(12,R2),=C'GUARANTEED -'                                        
         EDIT  (4,GUARCPM),(6,13(R2)),2,FLOAT=$,ALIGN=LEFT                      
         EJECT                                                                  
*              VALIDATE OPTIONAL DEMO AND FILTERS                               
         SPACE 3                                                                
VKEY1    LA    R2,PUPDEMOH         OPTIONAL DEMO OVERRIDE                       
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         MVC   FIRSTDEM,DEMOS      SAVE PLAN TARGET DEMO                        
         MVI   MAX,1                                                            
         GOTO1 VVALDEM             (SETS DEMOS WITH OVERRIDE)                   
         CLC   NPLNDEMS(3),DEMOS                                                
         BE    VKEY2                                                            
         MVC   PUPGUAR,SPACES      DON'T SHOW GUARANTEE IF OVERRIDE             
         MVC   NPLNDEMS(3),DEMOS                                                
         GOTO1 VEXTPLAN            (SETS TARGET AND TARGNAME)                   
         MVC   DEMOS(3),FIRSTDEM   RESTORE PLAN TARGET DEMO                     
         SPACE 1                                                                
VKEY2    MVC   WORK(10),TARGNAME                                                
         GOTO1 CENTER,DMCB,WORK,10                                              
         LA    R1,WORK                                                          
         LA    R0,10                                                            
         SPACE 1                                                                
VKEY4    CLI   0(R1),C' '          FILL TARGNAME WITH DASHES                    
         BNE   *+8                                                              
         MVI   0(R1),C'-'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,VKEY4                                                         
         MVC   PUPTITR+21(10),WORK  SHOW IT IN THE HEADINGS                     
         OI    PUPTITRH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,PUPLENH          OPTIONAL LENGTH FILTER                       
         MVI   REQLEN,0                                                         
         CLI   5(R2),0                                                          
         BE    VKEY6                                                            
         GOTO1 VALINUM                                                          
         MVC   REQLEN,ACTUAL                                                    
         SPACE 1                                                                
VKEY6    XC    ACCUMS,ACCUMS                                                    
         BAS   RE,POSTBUDG                                                      
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              HANDLE I/O FOR REPORT                                            
         SPACE 3                                                                
PREP     NTR1                                                                   
         CLI   CONACT,C'R'         IF REVALUE (BUT NOT PVALUE)                  
         BNE   PREP2                                                            
         GOTO1 VLUPUNIV            LOOK UP UNIVERSES AGAIN                      
         GOTO1 PUTREC                                                           
         SPACE 1                                                                
PREP2    LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPUKEY,R4                                                        
         MVI   NPUKTYPE,X'22'      FILL PROGRAM KEY                             
         MVC   NPUKAM,BINAGYMD                                                  
         MVC   NPUKCLT,CLTCOMP                                                  
         MVC   NPUKNET,NETWORK                                                  
         MVC   NPUKDPT,DPTCODE                                                  
         MVC   NPUKPLAN,PLANCODE                                                
         GOTO1 HIGH                                                             
         B     PREP6                                                            
         SPACE 1                                                                
PREP4    GOTO1 SEQ                                                              
         SPACE 1                                                                
PREP6    CLC   KEY(13),KEYSAVE     MUST HAVE MATCH ON PLAN                      
         BNE   PREPEND                                                          
         MVC   PROGKEY,KEY                                                      
         GOTO1 GETREC                                                           
         GOTO1 VEXTPROG                                                         
         BAS   RE,REVPROG                                                       
         BAS   RE,POSTPROG                                                      
         B     PREP4                                                            
         SPACE 1                                                                
PREPEND  BAS   RE,ADDEM                                                         
         BAS   RE,SHOWVALS                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO REVALUE PROGRAM RECORD                                
         SPACE 3                                                                
REVPROG  NTR1                                                                   
         CLI   CONACT,C'V'         IF REVALUE OR PVALUE, NOT VALUE              
         BE    XIT                                                              
         MVI   ELCODE,X'14'        DUMP VPH ELEMENTS                            
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'16'        AND NAD OVERRRIDE ELEMS                      
         GOTO1 REMELEM                                                          
         L     R4,AIO                                                           
         LA    R6,NPGDEL                                                        
         MVI   REVPER,0                                                         
         SPACE 1                                                                
REVPROG2 MVI   ELCODE,0                                                         
         BAS   RE,NEXTEL                                                        
         CLI   0(R6),X'02'         LOOK FOR OLD                                 
         BE    REVPROG3                                                         
         CLI   0(R6),X'12'              OR NEW                                  
         BNE   REVPROG6                                                         
         SPACE 1                                                                
         USING NPUAD,R6                                                         
REVPROG3 BAS   RE,REFPER           REFRESH PERIOD                               
         MVC   PERIOD,NPUAPER                                                   
         GOTO1 VLUPPROG                                                         
         L     R5,AIO                                                           
         USING NPURECD,R5                                                       
         MVC   NPGDFILT,PROGFILT                                                
         MVC   NPGDNTI,PROGNTI                                                  
         DROP  R5                                                               
         MVC   HUT,NPUAHUT         PRESET OLD HUT IN CAUSE NO REVALUE           
         MVI   LOOKUPSW,C'Y'                                                    
         CLI   CONACT,C'P'         DON'T LOOK UP HUTS FOR PVALUE                
         BNE   *+8                                                              
         MVI   LOOKUPSW,C'N'                                                    
         GOTO1 VLUPHUT                                                          
         SPACE 1                                                                
REVPRG3B TM    NPUAOVRD,X'80'      KEEP OVERRIDES                               
         BNO   *+10                                                             
         MVC   SHARE,NPUASHR                                                    
         TM    NPUAOVRD,X'40'                                                   
         BNO   *+10                                                             
         MVC   HUT,NPUAHUT                                                      
         OC    SHARE,SHARE         COMPUTE RATING                               
         BZ    REVPROG4                                                         
         TM    NPUAOVRD,X'20'      UNLESS IT HAS BEEN OVERRIDDEN                
         BO    REVPROG4                                                         
         SR    R0,R0                                                            
         ICM   R0,3,HUT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,SHARE                                                       
         MR    R0,R0                                                            
         D     R0,=F'500'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STCM  R1,3,RATING                                                      
         SPACE 1                                                                
REVPROG4 MVC   NPUASHR,SHARE       MOVE IN VALUES                               
         MVC   NPUAHUT,HUT                                                      
         TM    NPUAOVRD,X'20'                                                   
         BO    *+10                                                             
         MVC   NPUARTG,RATING                                                   
         CLI   0(R6),X'12'                                                      
         BE    REVPROG5                                                         
         ZIC   R1,NPUALEN                                                       
         SH    R1,=H'29'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NPUAVPHS(0),VPHS                                                 
         B     REVPROG2                                                         
         SPACE 1                                                                
REVPROG5 BAS   RE,SETVPH           NEW ELS MAINTAIN VPH DIFFERENTLY             
         USING NPUBD,R6                                                         
         MVC   NPUBLNK,VPHLINK                                                  
         B     REVPROG2                                                         
         SPACE 1                                                                
REVPROG6 GOTO1 VEXTPROG                                                         
         MVC   KEY,PROGKEY                                                      
         GOTO1 HIGH                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC                                                           
         B     XIT                                                              
         EJECT                                                                  
*              MAINTAIN THE VPH ELEMENTS                                        
         SPACE 3                                                                
*              INPUTS              VPHS CONTAINS VPHS!                          
*              OUTPUTS             LINK NUMBER RETURNED IN VPHLINK              
         SPACE 1                                                                
*          DATA SET NEPUP11N   AT LEVEL 012 AS OF 12/31/91                      
SETVPH   NTR1                                                                   
         LA    R1,VPHS+239                                                      
         LA    R3,240              GET L'VPHS IN R3                             
         SPACE 1                                                                
VPHLEN   CLI   0(R1),0                                                          
         BNE   VPHLEN2                                                          
         BCTR  R1,0                                                             
         BCT   R3,VPHLEN                                                        
         LA    R3,2                                                             
         SPACE 1                                                                
VPHLEN2  BCTR  R3,0                                                             
****     ZIC   R3,VPHSLEN          SET IN NEPUP00N 11/3                         
         MVI   ELCODE,X'14'                                                     
         USING NPUCD,R6                                                         
         L     R6,AIO                                                           
         SR    R2,R2               COUNT HOW MANY VPHS ELEMENTS SO FAR          
         BAS   RE,GETEL                                                         
         B     SETVPH4                                                          
         SPACE 1                                                                
SETVPH2  BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
SETVPH4  BNE   SETVPH6                                                          
         MVC   VPHLINK,NPUCLNK     RETURN LINK IF MATCH                         
         LA    R2,1(R2)                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   VPHS(0),NPUCVPHS    CHECK FOR MATCHING VPH                       
         BNE   SETVPH2                                                          
*  VPHS MATCH/ NOW CHECK NADS MATCH                                             
         LR    R1,R6               SAVE CURRENT R6 POINTER                      
         MVI   ELCODE,X'16'       .ANY NADS                                     
SETVPH4B BAS   RE,NEXTEL                                                        
         BE    SETVPH5                                                          
         CLI   NOVERS,0           .NO/ANY IN NOVERS                             
         BNE   SETVPH5A               YES/NO MATCH                              
         B     XIT                    NO/OK/XIT                                 
SETVPH5  CLC   VPHLINK,2(R6)      CHECK LINK TO NADS                            
         BNE   SETVPH4B                                                         
         ZIC   R4,1(R6)                                                         
         S     R4,=F'5'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   NOVERS(0),4(R6)                                                  
         BE    XIT                 NADS MATCH /OK/XIT                           
SETVPH5A LR    R6,R1          NADS DON'T MATCH/RESET REC ELEM POINTER           
         MVI   ELCODE,X'14'                    RESET ELCODE                     
         B     SETVPH2        GET NEXT VPH ELEM                                 
         SPACE 1                                                                
SETVPH6  LA    R2,1(R2)                                                         
         CH    R2,=H'6'            ONLY ROOM FOR 6 VPH ELEMENTS                 
         BH    XIT                 (VPHLINK IS SET TO LAST)                     
         MVC   WORK(40),ELEMENT    SAVE THE X'12' ELEMENT                       
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   NPUCEL,X'14'        ADD ANOTHER VPH ELEMENT                      
         STC   R2,NPUCLNK                                                       
         STC   R2,VPHLINK                                                       
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   NPUCVPHS(0),VPHS                                                 
         LA    R3,1+4(R3)                                                       
         STC   R3,NPUCLEN                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
* ANY NADS TO ADD                                                               
         CLI   NOVERS,0                                                         
         BE    SETVPH10                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,X'16'                                                    
         MVC   ELEMENT+2(1),VPHLINK                                             
         SR    R1,R1                                                            
         LA    R4,NOVERS                                                        
SETVPH8  CLI   0(R4),0                                                          
         BE    SETVPH9                                                          
         LA    R4,5(R4)            BUMP TO NXT NAD SET                          
         LA    R1,5(R1)                                                         
         B     SETVPH8                                                          
SETVPH9  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+4(0),NOVERS                                              
         LA    R1,5(R1)                                                         
         STC   R1,ELEMENT+1                                                     
         GOTO1 ADDELEM                                                          
*                                                                               
SETVPH10 XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(40),WORK                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO REFRESH PERIOD                                        
         SPACE 3                                                                
REFPER   NTR1                                                                   
         CLI   PLANPERT,C'W'       WEEKLY NOT SUPPORTED                         
         BE    XIT                                                              
         USING NPUAD,R6                                                         
         MVC   NPUAPER(1),PLANYEAR USER MAY HAVE CHANGED PLAN YEAR              
         ZIC   R1,PLANYEAR                                                      
         BCTR  R1,0                                                             
         CLI   PLANPERT,C'Q'       QUARTERLY                                    
         BNE   REFPER2                                                          
         CLI   NPUAPER+1,4                                                      
         BNE   XIT                                                              
         STC   R1,NPUAPER                                                       
         B     XIT                                                              
         SPACE 1                                                                
REFPER2  CLI   NPUAPER+1,9         MONTHLY                                      
         BL    REFPERX                                                          
         BE    REFPER4                                                          
         STC   R1,NPUAPER                                                       
         B     REFPERX                                                          
         SPACE 1                                                                
REFPER4  CLI   REVPER,0            SEPTEMBER - IS THIS FIRST PERIOD             
         BNE   REFPERX                                                          
         STC   R1,NPUAPER          YES - ASSUMED TO BE IN FIRST YEAR            
         SPACE 1                                                                
REFPERX  MVC   REVPER,NPUAPER                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST BUDGETS FROM PLAN                                
         SPACE 3                                                                
POSTBUDG NTR1                                                                   
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VEXTBUDG            GET BUDGETS (MAYBE FOR LENGTH)               
         LA    R2,BUDGETS                                                       
         LA    R3,ACCUMS                                                        
         LA    R0,4                                                             
         SPACE 1                                                                
PBUDG2   MVC   0(4,R3),0(R2)       MOVE BUDGETS INTO ACCUMS                     
         LA    R2,20(R2)                                                        
         LA    R3,24(R3)                                                        
         BCT   R0,PBUDG2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST PROGRAM DETAILS                                  
         SPACE 3                                                                
POSTPROG NTR1                                                                   
         LA    R2,PLANPLST         SET UP FOR PERIODS                           
         ZIC   R0,PLANNPER                                                      
         SPACE 1                                                                
PPROG2   MVC   PERIOD,0(R2)                                                     
         ZIC   R3,2(R2)            PICK UP QUARTER NUMBER                       
         MH    R3,=H'24'           FOR ACCUMULATOR SET                          
         LA    R3,ACCUMS(R3)                                                    
         USING ACCUMD,R3                                                        
         SPACE 1                                                                
PPROG4   MVC   LENGTH,REQLEN                                                    
         GOTO1 VEXTUNS                                                          
         ZIC   R1,UNITS            ADD IN UNITS                                 
         A     R1,ACUNITS                                                       
         ST    R1,ACUNITS                                                       
         SPACE 1                                                                
         MVI   GDRNDOPT,C'Y'                                                    
*        MVI   GDRAWOPT,C'Y'                                                    
         MVC   GDDEMO,=X'000001'     GO FOR HOMES FIRST                         
***      MVI   GDDEMO+2,1            GO FOR HOMES FIRST                         
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           HOMES GRPS                                   
         A     R1,ACHOMGRP                                                      
         ST    R1,ACHOMGRP                                                      
         L     R1,GDTIMP           HOMES (000)                                  
         A     R1,ACHOMIMP                                                      
         ST    R1,ACHOMIMP                                                      
         SPACE 1                                                                
         MVC   GDDEMO,TARGET       THEN THE TARGET                              
*        MVC   GDDEMO,TARGET+2     THEN THE TARGET                              
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           FOR TARGET GRP                               
         A     R1,ACTARGRP                                                      
         ST    R1,ACTARGRP                                                      
         L     R1,GDTIMP           AND (000)                                    
         A     R1,ACTARIMP                                                      
         ST    R1,ACTARIMP                                                      
         SPACE 1                                                                
         LA    R2,4(R2)                                                         
         BCT   R0,PPROG2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              NOW ADD EACH QUARTER INTO BOTTOM LINE TOTAL                      
         SPACE 3                                                                
ADDEM    NTR1                                                                   
         LA    R2,ACCUMS           ADD DETAILS INTO TOTALS                      
         LA    R3,ACCUMS+96                                                     
         LA    R0,4                                                             
         SPACE 1                                                                
ADDEM2   BAS   RE,ADDEM4                                                        
         LA    R2,24(R2)                                                        
         BCT   R0,ADDEM2                                                        
         B     XIT                                                              
         SPACE 1                                                                
ADDEM4   NTR1                                                                   
         LA    R0,6                ADD A LINE                                   
         SPACE 1                                                                
ADDEM6   L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ADDEM6                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ADJUST DEMOS BY ROUNDING FACTORS                                 
*                                                                               
RANYADJ  NTR1                                                                   
*                                                                               
         LA    R3,ACCUMS                                                        
         USING ACCUMD,R3                                                        
         LA    RF,5                                                             
         SPACE 1                                                                
ANYADJ2  LA    R4,ACTARIMP                                                      
         BAS   RE,ROUND                                                         
         SPACE 1                                                                
         LA    R4,ACHOMIMP                                                      
         BAS   RE,ROUND                                                         
*                                                                               
ANYADJ6  LA    R3,24(R3)                                                        
         BCT   RF,ANYADJ2                                                       
         B     XIT                                                              
         EJECT                                                                  
ROUND    NTR1                                                                   
         L     R1,FULL                                                          
         CLI   NETMAIN,0                                                        
         BE    *+8                                                              
         CLI   NETMEDIA,C'S'       SYNDICATION                                  
         BE    *+8                                                              
         CLI   NETMAIN,C'Y'        OR MAIN NETWORK                              
         BNE   ROUND2                                                           
         AH    R1,=H'50'                                                        
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         M     R0,=F'10'           (RETURN MAIN IN 000 TO NEAREST               
         ST    R1,FULL                                    10000)                
         B     XIT                                                              
         SPACE 1                                                                
ROUND2   AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,FULL                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY VALUES FOR PLAN                               
         SPACE 3                                                                
SHOWVALS NTR1                                                                   
         LA    R2,PUPPERH                                                       
         LA    R3,ACCUMS                                                        
         USING ACCUMD,R3                                                        
         LA    R4,5                                                             
         SPACE 1                                                                
SVAL2    CLI   8(R2),C'T'          SCREEN SHOWS TOTAL OR QUARTER                
         BE    SVAL4                                                            
         ZIC   R1,PLANYEAR                                                      
         CH    R4,=H'5'                                                         
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         EDIT  (R1),(2,12(R2))     SO EDIT YEAR                                 
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
SVAL4    BAS   RE,BUMP                                                          
         BAS   RE,SHOWLINE                                                      
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         LA    R3,24(R3)                                                        
         BCT   R4,SVAL2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EDIT A LINE OF NUMBERS                                
         SPACE 3                                                                
SHOWLINE NTR1                                                                   
         LA    R2,8(R2)            GET PAST HEADER                              
         MVC   0(71,R2),SPACES     AND CLEAR                                    
         USING ACCUMD,R3                                                        
*                                  SHOW BUDGET                                  
         EDIT  (4,ACBUDGET),(9,0(R2))                                           
         LA    R2,9(R2)            UNITS                                        
         EDIT  (4,ACUNITS),(5,0(R2))                                            
         LA    R2,5(R2)            COST PER UNIT                                
         L     R1,ACBUDGET                                                      
         LTR   R1,R1                                                            
         BZ    SLINE2                                                           
         SR    R0,R0                                                            
         OC    ACUNITS,ACUNITS                                                  
         BZ    SLINE2                                                           
         D     R0,ACUNITS                                                       
         EDIT  (R1),(7,0(R2)),FLOAT=$                                           
         CLI   0(R2),C'$'                                                       
         BNE   SLINE2                                                           
         MVI   0(R2),C' '                                                       
         SPACE 1                                                                
SLINE2   LA    R2,7(R2)            HOMES POINTS                                 
         EDIT  (4,ACHOMGRP),(7,0(R2)),1,ZERO=BLANK                              
         CLI   0(R2),C' '                                                       
         BE    SLINE4                                                           
         L     R1,ACHOMGRP                                                      
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(7,0(R2))                                                   
         SPACE 1                                                                
SLINE4   LA    R2,7(R2)            HOMES CPP                                    
         L     R1,ACBUDGET                                                      
         LTR   R1,R1                                                            
         BZ    SLINE6                                                           
         M     R0,=F'20'                                                        
         OC    ACHOMGRP,ACHOMGRP                                                
         BZ    SLINE6                                                           
         D     R0,ACHOMGRP                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(6,0(R2)),FLOAT=$                                           
         CLI   0(R2),C'$'                                                       
         BNE   SLINE6                                                           
         MVI   0(R2),C' '                                                       
         SPACE 1                                                                
SLINE6   LA    R2,6(R2)            HOMES AND HOME CPM                           
         LA    R4,ACHOMIMP                                                      
         BAS   RE,CPM                                                           
         LA    R2,15(R2)                                                        
         EDIT  (4,ACTARGRP),(7,0(R2)),1,ZERO=BLANK                              
         CLI   0(R2),C' '                                                       
         BE    SLINE8                                                           
         L     R1,ACTARGRP                                                      
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(7,0(R2))                                                   
         SPACE 1                                                                
SLINE8   LA    R2,7(R2)                                                         
         LA    R4,ACTARIMP         TARGET IMPS AND CPM                          
         BAS   RE,CPM                                                           
         B     XIT                                                              
         EJECT                                                                  
*              SUB-ROUTINE TO SHOW IMPS AND CPM                                 
         SPACE 3                                                                
*              INPUTS              R2=A(OUTPUT AREA)                            
*                                  R3=A(ACCUMS)                                 
*                                  R3=A(IMPS)                                   
         SPACE 1                                                                
         USING ACCUMD,R3                                                        
CPM      NTR1                                                                   
         L     R1,0(R4)            PICK UP IMPS                                 
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         MVC   FULL,0(R4)                                                       
         BAS   RE,ROUND                                                         
         EDIT  (4,FULL),(8,0(R2))                                               
         SPACE 1                                                                
         LA    R2,8(R2)            COMPUTE CPM IN PENNIES                       
         L     R1,ACBUDGET                                                      
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'2000'                                                      
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R2)),2,FLOAT=$                                         
         CLI   0(R2),C' '                                                       
         BE    XIT                                                              
         SPACE 1                                                                
         L     R1,ACBUDGET         TOO BIG SO COMPUTE TO $                      
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'20'                                                        
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R2)),FLOAT=$                                           
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPF4D                                                       
         SPACE 1                                                                
         DS    0D                                                               
ACCUMS   DS    CL120                                                            
REQLEN   DS    XL1                                                              
PROGKEY  DS    CL48                                                             
REVPER   DS    XL2                                                              
VPHLINK  DS    XL1                                                              
FIRSTDEM DS    XL3                                                              
         SPACE 3                                                                
*              DSECT TO COVER ACCUMULATOR LINE                                  
         SPACE 1                                                                
ACCUMD   DSECT                                                                  
ACBUDGET DS    F                                                                
ACUNITS  DS    F                                                                
ACHOMGRP DS    F                                                                
ACHOMIMP DS    F                                                                
ACTARGRP DS    F                                                                
ACTARIMP DS    F                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026NEPUP04   05/01/02'                                      
         END                                                                    
