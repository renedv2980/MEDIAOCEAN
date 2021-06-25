*          DATA SET NEPUP21S   AT LEVEL 037 AS OF 05/01/02                      
*          DATA SET NEPUP21    AT LEVEL 054 AS OF 04/25/90                      
*PHASE T32221A,*                                                                
         TITLE 'T32221 - REQUEST REPORTS'                                       
T32221   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32221**,RA,R6                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   VAL2                                                             
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
VAL2     CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD FOR REPORTS                                      
         SPACE 3                                                                
VREC     NTR1                                                                   
         SPACE 1                                                                
         LA    R2,PUPCLIH          CLIENT                                       
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
         MVC   8(8,R2),DPTNAME                                                  
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPDRQUH         QUARTER (OPTIONAL)                           
         MVI   REQQRT,0                                                         
         CLI   5(R2),0                                                          
         BE    VREC2                                                            
         GOTO1 VALINUM                                                          
         MVC   REQQRT,ACTUAL                                                    
         CLI   REQQRT,0            S/B 1-4                                      
         BE    BADQUART                                                         
         CLI   REQQRT,4                                                         
         BH    BADQUART                                                         
         SPACE 1                                                                
VREC2    LA    R2,PUPDEMOH         DEMO OVERRIDES                               
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         MVI   MAX,6                                                            
         GOTO1 VVALDEM                                                          
         SPACE 1                                                                
VREC4    LA    R2,PUPOPTH          OPTIONS                                      
         BAS   RE,EDITOPT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              EDIT OPTIONS                                                     
         SPACE 3                                                                
EDITOPT  NTR1                                                                   
         MVI   BOXOPT,C'Y'                                                      
         MVI   FIELDERR,1                                                       
         MVI   LEFTOPT,C'N'                                                     
         MVI   SPACOPT,1                                                        
         MVI   REQLEN,0                                                         
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   OPT4                                                             
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(4,R4),=C'LEFT'   LEFT OPTION                                  
         BNE   OPT6                                                             
         MVI   LEFTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(2,R4),=C'S   '   SPACING OPTION                               
         BNE   OPT8                                                             
         MVC   SPACOPT,11(R4)                                                   
         CLI   SPACOPT,1                                                        
         BL    BADOPT                                                           
         CLI   SPACOPT,3                                                        
         BH    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(3,R4),=C'LEN '   LENGTH FILTERING OPTION                      
         BNE   OPT10                                                            
         MVC   REQLEN,11(R4)                                                    
         CLI   REQLEN,1                                                         
         BL    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL REPORTS                                                  
         SPACE 3                                                                
PREP     NTR1                                                                   
         MVI   GDRNDOPT,C'Y'                                                    
*                                                                               
         XC    TOTALS,TOTALS                                                    
         MVI   QUARTER,0                                                        
         LA    R1,HEDSPECS         INITIALIZE                                   
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   HORIZ,X'BF'         SET UP UNDERLINE CHARACTER                   
         OC    ABOX,ABOX                                                        
         BNZ   *+12                                                             
         MVI   HORIZ,C'-'                                                       
         MVI   LEFTOPT,C'Y'        (OFF LINE FORCE LEFT OPTION)                 
         CLI   BOXOPT,C'N'                                                      
         BNE   *+8                                                              
         MVI   HORIZ,C'-'                                                       
         SPACE 1                                                                
         MVI   TYPE,C'S'           PLAN SUMMARY                                 
         MVC   TITLE,PUPPSTL                                                    
         CLI   PUPPSYN,C'N'                                                     
         BE    PREP2                                                            
         LA    RE,PROGACCS                                                      
         LA    RF,816                                                           
         XCEF                                                                   
***      XC    PROGACCS,PROGACCS                                                
***      XC    PLANACCS,PLANACCS                                                
***      XC    TARGACCS,TARGACCS                                                
         MVI   UNTOTS,C'Y'         UNIT TOTALS WILL FIT                         
         CLI   PLANNLEN,4                                                       
         BNE   PREP1               UNLESS THERE ARE 4 LENGTHS                   
         CLI   NDEMOS,6                                                         
         BL    PREP1               AND A FULL SET OF DEMOS                      
         MVI   UNTOTS,C'N'                                                      
         SPACE 1                                                                
PREP1    BAS   RE,COMPDISP                                                      
         BAS   RE,PLANIO                                                        
         MVI   TYPE,C'G'                                                        
         BAS   RE,GRAND                                                         
         MVI   TYPE,C'T'                                                        
         BAS   RE,COMPDISP                                                      
         BAS   RE,TARGTOTS                                                      
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
PREP2    MVC   TITLE,PUPDRTL       PLAN DETAIL                                  
         CLI   PUPDRYN,C'N'                                                     
         BE    PREP4                                                            
         LA    R2,DRQLIST          DEFAULT IS 4 QUARTERS                        
         LA    R0,4                                                             
         CLI   REQQRT,0            OPTION FOR 1 QUARTER ONLY                    
         BE    PREP3                                                            
         LA    R2,REQQRT                                                        
         LA    R0,1                                                             
         SPACE 1                                                                
PREP3    MVC   QUARTER,0(R2)                                                    
         MVI   TYPE,C'D'                                                        
         BAS   RE,COMPDISP                                                      
         BAS   RE,PLANIO           HANDLE FOR EACH QUARTER                      
         MVI   TYPE,C'G'                                                        
         BAS   RE,GRAND                                                         
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,1(R2)                                                         
         BCT   R0,PREP3                                                         
         SPACE 1                                                                
PREP4    MVI   TYPE,C'Q'           QUARTERLY SUMMARY                            
         MVI   QUARTER,0                                                        
         MVC   TITLE,PUPQSTL                                                    
         CLI   PUPQSYN,C'N'                                                     
         BE    XIT                                                              
         BAS   RE,COMPDISP                                                      
         BAS   RE,COMPACC                                                       
         XCEF  QBUFF,1000          CLEAR A BUFFER                               
         BAS   RE,PLANIO                                                        
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE I/O FOR PLAN                                              
         SPACE 3                                                                
PLANIO   NTR1                                                                   
         LA    RE,PROGACCS                                                      
         LA    RF,544                                                           
         XCEF                                                                   
***      XC    PLANACCS,PLANACCS                                                
***      XC    PROGACCS,PROGACCS                                                
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPLKEY,R4                                                        
         MVI   NPLKTYPE,X'20'      REREAD PLAN FOR BUDGET EXTRACT               
         MVC   NPLKAM,BINAGYMD                                                  
         MVC   NPLKCLT,CLTCOMP                                                  
         MVC   NPLKNET,NETWORK                                                  
         MVC   NPLKDPT,DPTCODE                                                  
         MVC   NPLKPLAN,PLANCODE                                                
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         BAS   RE,POSTBUDG                                                      
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPUKEY,R4                                                        
         MVI   NPUKTYPE,X'22'      FILL PROGRAM KEY                             
         MVC   NPUKAM,BINAGYMD                                                  
         MVC   NPUKCLT,CLTCOMP                                                  
         MVC   NPUKNET,NETWORK                                                  
         MVC   NPUKDPT,DPTCODE                                                  
         MVC   NPUKPLAN,PLANCODE                                                
         GOTO1 HIGH                                                             
         B     PLANIO6                                                          
         SPACE 1                                                                
PLANIO4  GOTO1 SEQ                                                              
         SPACE 1                                                                
PLANIO6  CLC   KEY(13),KEYSAVE     MUST HAVE MATCH ON PLAN                      
         BNE   PLANEND                                                          
         GOTO1 GETREC                                                           
         GOTO1 VEXTPROG                                                         
         BAS   RE,POSTPROG                                                      
         BAS   RE,PROGTOTS                                                      
         LA    RE,PROGACCS                                                      
         LA    RF,272                                                           
         XCEF                                                                   
*        CLI   TYPE,C'Q'           IF QUARTERLY SUMMARY                         
*        BNE   PLANIO4                                                          
*        MVC   WORK(30),KEY        DO EXTRA I/O TO RESET TIMER                  
*        MVI   KEY,4                                                            
*        GOTO1 HIGH                                                             
*        MVC   KEY,WORK                                                         
*        GOTO1 HIGH                                                             
         B     PLANIO4                                                          
         SPACE 1                                                                
PLANEND  BAS   RE,REPTOTS                                                       
         LA    RE,PLANACCS                                                      
         LA    RF,272                                                           
         XCEF                                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST BUDGETS FROM PLAN                                
         SPACE 3                                                                
POSTBUDG NTR1                                                                   
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VEXTBUDG                                                         
         LA    R3,TOTALS                                                        
         USING ACCUMD,R3                                                        
         CLI   QUARTER,0                                                        
         BNE   PBUDG4                                                           
         LA    R2,BUDGETS                                                       
         LA    R0,4                                                             
         SPACE 1                                                                
PBUDG2   L     R1,0(R2)            ADD BUDGETS INTO ACCUMS                      
         A     R1,ACBUDGET                                                      
         ST    R1,ACBUDGET                                                      
         LA    R2,20(R2)                                                        
         BCT   R0,PBUDG2                                                        
         B     XIT                                                              
         SPACE 1                                                                
PBUDG4   ZIC   R1,QUARTER          NEED BUDGET FOR 1 QUARTER                    
         CLI   QUARTER,4                                                        
         BNE   *+6                                                              
         SR    R1,R1                                                            
         MH    R1,=H'20'           DISPLACE TO BUDGET FOR QUARTER               
         LA    R1,BUDGETS(R1)                                                   
         MVC   ACBUDGET,0(R1)                                                   
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO POST PROGRAM DETAILS                                  
         SPACE 3                                                                
POSTPROG NTR1                                                                   
         LA    R2,QLIST            SET UP FOR QUARTERLY                         
         LA    R3,TOTALS                                                        
         USING ACCUMD,R3                                                        
         MVI   PERNUM,1                                                         
         LA    R0,4                                                             
         CLI   PLANPERT,C'W'       IS IT WEEKS                                  
         BNE   PPROG1                                                           
         LA    R2,PLANPLST                                                      
         CLI   PLANPRFL,C'B'       CHECK PLAN /IF WEEKS AND CALENDAR            
         BE    PPROG00                                                          
         CLI   N0PROF+3,C'C'       CHECK PROFILE/IF WEEKS AND CALENDAR          
         BNE   PPROG00                                                          
*                                                                               
         DS    0H                  ADJUST PLANPLST                              
         ZIC   R0,PLANNPER                                                      
ADJ5     GOTO1 DATCON,DMCB,(2,0(R2)),(3,WORK)                                   
         CLI   2(R2),0             ...IF 4TH QUARTER                            
         BNE   ADJ7                                                             
         ZIC   R1,WORK+1           ...SEP=0,OCT=1,ETC.                          
         S     R1,=F'9'                                                         
         STC   R1,3(R2)                                                         
         B     ADJ10                                                            
ADJ7     ZIC   R1,WORK+1        OTHER QUARTERS ADD 3                            
         A     R1,=F'3'         JAN=4,FEB=5...SEP=12                            
         STC   R1,3(R2)                                                         
ADJ10    LA    R2,4(R2)                                                         
         BCT   R0,ADJ5                                                          
         LA    R2,PLANPLST                                                      
*                                                                               
PPROG00  ZIC   R0,PLANNPER         NUMBER OF PERIODS                            
         B     PPROG2                                                           
         SPACE 1                                                                
PPROG1   CLI   PLANPERT,C'M'                                                    
         BNE   PPROG2                                                           
         LA    R2,MLIST            OR MONTHLY                                   
         LA    R0,13                                                            
         SPACE 1                                                                
PPROG2   MVC   THISQURT,2(R2)      NOTE QUARTER NUMBER                          
         CLI   QUARTER,0           IF QUARTER WAS REQUESTED                     
         BE    PPROG3                                                           
         CLI   2(R2),0             FOR WEEKS 4TH Q = 0 IN PLANPLST              
         BNE   PPROG2B                                                          
         CLI   QUARTER,4           SO TEST IT AGAINST QUARTER                   
         BE    PPROG3                                                           
         SPACE 1                                                                
PPROG2B  CLC   QUARTER,2(R2)       THEN IT MUST MATCH TABLE                     
         BNE   PPROG6                                                           
         SPACE 1                                                                
PPROG3   MVC   PERIOD,0(R2)                                                     
         CLI   PLANPERT,C'W'                                                    
         BE    PPROG4                                                           
         MVC   PERIOD(1),PLANYEAR  PERIODS                                      
         MVC   PERIOD+1(1),0(R2)                                                
         CLI   1(R2),1                                                          
         BNE   PPROG4                                                           
         ZIC   R1,PLANYEAR                                                      
         BCTR  R1,0                                                             
         STC   R1,PERIOD                                                        
         SPACE 1                                                                
PPROG4   BAS   RE,CHKPER           ANY STUFF FOR THIS PERIOD                    
         BNE   PPROG6              NO - TRY NEXT                                
*                                                                               
         MVC   GDDEMO,=X'000001'     GO FOR HOMES FIRST                         
****     MVI   GDDEMO,1            GO FOR HOMES FIRST                           
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VGETDEM                                                          
         L     R1,GDUNITS          ADD IN UNITS                                 
         A     R1,ACUNITS                                                       
         ST    R1,ACUNITS                                                       
         L     R1,GDTGRP           HOMES GRPS                                   
         A     R1,ACHOMGRP                                                      
         ST    R1,ACHOMGRP                                                      
         L     R1,GDTIMP           HOMES (000)                                  
         A     R1,ACHOMIMP                                                      
         ST    R1,ACHOMIMP                                                      
         SPACE 1                                                                
         MVC   GDDEMO,TARGET       THEN THE TARGET                              
***      MVC   GDDEMO,TARGET+2     THEN THE TARGET                              
         GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           FOR TARGET GRP                               
         A     R1,ACTARGRP                                                      
         ST    R1,ACTARGRP                                                      
         L     R1,GDTIMP           AND (000)                                    
         A     R1,ACTARIMP                                                      
         ST    R1,ACTARIMP                                                      
         CLI   PLANPERT,C'W'   .IF WEEKS                                        
         BNE   PPROG5                                                           
         ZIC   R1,3(R2)        .PLACE INTO MONTH SLOTS                          
         LA    R1,1(R1)         (ADDING 1 TO PLANPLST MONTH MAKES IT-)          
         STC   R1,PERNUM        (EQUAL TO MONTH PERNUM SLOTS.)                  
         SPACE 1                                                                
PPROG5   BAS   RE,DETPOST                                                       
         SPACE 1                                                                
PPROG6   CLI   PLANPERT,C'W'                                                    
         BNE   PPROG7                                                           
         LA    R2,4(R2)                                                         
         B     *+8                                                              
         SPACE 1                                                                
PPROG7   LA    R2,3(R2)                                                         
         AI    PERNUM,1                                                         
         SPACE 1                                                                
PPROG7A  BCT   R0,PPROG2                                                        
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
CHKPER   NTR1                                                                   
         L     R3,AIO              POINT TO RECORD                              
         LA    R1,1                SET UP FOR FAILURE                           
         MVI   ELCODE,0            SCAN ALL ELEMENTS                            
         USING NPUAD,R3                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CHKPER2  BAS   RE,NEXTEL                                                        
         BNE   CHKPERX             END - FAIL                                   
         CLI   0(R3),X'02'         CHECK OLD AND NEW STYLE ELEMENTS             
         BE    *+8                                                              
         CLI   0(R3),X'12'                                                      
         BNE   CHKPER2                                                          
         CLC   NPUAPER,PERIOD      REQUESTED PERIOD                             
         BNE   CHKPER2                                                          
         SR    R1,R1               YES - SUCCESS                                
CHKPERX  LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL DETAIL POSTING - SUMMARY REPORT                          
         SPACE 3                                                                
*                                  PERIOD IS FILLED IN AT THIS STAGE            
*                                  PERNUM HAS PERIOD NUMBER                     
         SPACE 1                                                                
DETPOST  NTR1                                                                   
         CLI   TYPE,C'S'                                                        
         BNE   DETP10                                                           
         ZIC   R1,PERNUM           POSITION TO ACCUM SET FOR PERIOD             
         BCTR  R1,0                                                             
         ZIC   RF,PLANNLEN                                                      
         SLL   RF,2                                                             
         MR    R0,RF                                                            
         LA    R3,PROGACCS(R1)     R3=A(ACCUMS FOR PERIOD)                      
         CLI   PLANPERT,C'W'                                                    
*        BNE   DETP0                                                            
         BE    DETP00                                                           
         ZIC   R1,PLANNPER                                                      
         B     DETP1                                                            
DETP0    LA    R1,4                                                             
         CLI   PLANPERT,C'M'                                                    
         BNE   *+8                                                              
DETP00   LA    R1,13                                                            
DETP1    MR    R0,RF                                                            
         LA    R4,PROGACCS(R1)     R4=A(ACCUMS FOR PLAN)                        
         LA    R2,PLANLENS                                                      
         ZIC   R0,PLANNLEN                                                      
         SPACE 1                                                                
DETP2    MVC   LENGTH,0(R2)        GET N'UNITS FOR PERIOD/LENGTH                
         CLI   REQLEN,0                                                         
         BE    DETP2B                                                           
         CLC   LENGTH,REQLEN                                                    
         BNE   DETP3                                                            
         SPACE 1                                                                
DETP2B   GOTO1 VEXTUNS                                                          
         ZIC   R1,UNITS                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         ZIC   R1,UNITS                                                         
         A     R1,0(R4)                                                         
         ST    R1,0(R4)                                                         
         SPACE 1                                                                
DETP3    LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,DETP2                                                         
         SPACE 1                                                                
         LA    R2,DEMOS            SET UP TO POST DEMOS                         
         LA    R3,TARGACCS                                                      
         ZIC   R4,NDEMOS                                                        
         MVC   LENGTH,REQLEN                                                    
         SPACE 1                                                                
DETP4    MVC   GDDEMO,0(R2)                                                     
*DETP4    MVC   GDDEMO,2(R2)                                                    
         GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           PICK UP TARGET GRPS                          
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         L     R1,GDTIMP           AND TARGET IMPS                              
         A     R1,4(R3)                                                         
         ST    R1,4(R3)                                                         
         LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,DETP4                                                         
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL DETAIL POSTING - DETAILS REPORT                          
         SPACE 3                                                                
*                                  PERIOD IS FILLED IN AT THIS STAGE            
         SPACE 1                                                                
DETP10   CLI   TYPE,C'D'                                                        
         BNE   DETQ2                                                            
         GOTO1 VEXTDEM             GET SHARE/HUT/RATING FOR PERIOD              
         LA    R3,PROGACCS                                                      
         USING DETD,R3                                                          
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VEXTUNS             GET N'UNITS FOR PERIOD                       
         ZIC   R5,UNITS                                                         
         LR    R1,R5                                                            
         A     R1,DETTUNIT         UPDATE TOTAL UNITS                           
         ST    R1,DETTUNIT                                                      
         SPACE 1                                                                
         ZICM  R1,SHARE,2                                                       
         LTR   R1,R1                                                            
         BNZ   DETP11                                                           
         ZICM  R1,RATING,2         DEDUCE SHARE IF MISSING                      
         ZICM  RF,HUT,2                                                         
         LTR   RF,RF                                                            
         BZ    DETP11B                                                          
*        BZ    DETP11                                                           
         M     R0,=F'2000'                                                      
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
DETP11   MR    R0,R5                                                            
         A     R1,DETSHR                                                        
         ST    R1,DETSHR                                                        
         SPACE 1                                                                
         ZICM  R1,HUT,2                                                         
         MR    R0,R5                                                            
         A     R1,DETHUT                                                        
         ST    R1,DETHUT                                                        
         SPACE 1                                                                
*        ZICM  R1,RATING,2                                                      
*        MR    R0,R5                                                            
*        A     R1,DETRTG                                                        
*        ST    R1,DETRTG                                                        
         SPACE 1                                                                
DETP11B  LA    R2,PLANLENS                                                      
         LA    R4,DETLUNIT                                                      
         ZIC   R0,PLANNLEN                                                      
         SPACE 1                                                                
DETP12   MVC   LENGTH,0(R2)        GET N'UNITS FOR PERIOD/LENGTH                
         CLI   REQLEN,0                                                         
         BE    DETP12B                                                          
         CLC   LENGTH,REQLEN                                                    
         BNE   DETP13                                                           
         SPACE 1                                                                
DETP12B  GOTO1 VEXTUNS                                                          
         ZIC   R1,UNITS                                                         
         A     R1,0(R4)                                                         
         ST    R1,0(R4)                                                         
         SPACE 1                                                                
DETP13   LA    R2,1(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,DETP12                                                        
         SPACE 1                                                                
         MVC   LENGTH,REQLEN                                                    
         MVC   GDDEMO,=X'000001'                                                
****     MVI   GDDEMO,1                                                         
         GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           PICK UP HOMES GRPS                           
         A     R1,DETGRPS                                                       
         ST    R1,DETGRPS                                                       
         L     R1,GDTGRP                                                        
         A     R1,DETRTG                                                        
         ST    R1,DETRTG                                                        
         L     R1,GDTIMP           AND HOMES IMPS                               
         A     R1,DETHOMES                                                      
         ST    R1,DETHOMES                                                      
         LA    R2,DEMOS            SET UP TO POST DEMOS                         
         LA    R3,DETTGRPS                                                      
         ZIC   R4,NDEMOS                                                        
         CH    R4,=H'6'                                                         
         BL    *+8                                                              
         LA    R4,6                                                             
         SPACE 1                                                                
DETP14   MVC   GDDEMO,0(R2)                                                     
*DETP14   MVC   GDDEMO,2(R2)                                                    
         GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           PICK UP TARGET GRPS                          
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         L     R1,GDTIMP           AND TARGET IMPS                              
         A     R1,4(R3)                                                         
         ST    R1,4(R3)                                                         
         LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,DETP14                                                        
         B     XIT                                                              
         EJECT                                                                  
*              DETAIL POSTING FOR QUARTERLY SUMMARY                             
         SPACE 3                                                                
*                                  PERIOD IS SET AT THIS STAGE                  
         SPACE 1                                                                
DETQ2    CLI   TYPE,C'Q'                                                        
         BNE   XIT                                                              
         MVI   GDRAWOPT,C'Y'                                                    
         ZIC   R1,THISQURT         GET QUARTER AS 0-3                           
         CLI   THISQURT,4                                                       
         BNE   *+6                                                              
         SR    R1,R1                                                            
         BAS   RE,DETQ4            POST FOR QUARTER                             
         LA    R1,4                                                             
         BAS   RE,DETQ4            THEN ADD IN FOR YEAR                         
         MVI   GDRAWOPT,C'N'                                                    
         B     XIT                                                              
         SPACE 1                                                                
DETQ4    NTR1                                                                   
         M     R0,SIZEACC          R1 HAS QUARTER NUMBER                        
         LA    R2,QBUFF(R1)        DISPLACE INTO ACCUM CHUNK                    
         MVC   GDDEMO,=X'000001'                                                
*        MVI   GDDEMO,1                                                         
         BAS   RE,DETQ10           FIRST LINE IS HOMES                          
         A     R2,SIZELINE                                                      
         LA    R3,DEMOS                                                         
         ZIC   R4,NDEMOS                                                        
         SPACE 1                                                                
DETQ6    MVC   GDDEMO,0(R3)                                                     
*DETQ6    MVC   GDDEMO,2(R3)                                                    
         BAS   RE,DETQ10           OTHER LINES ARE DEMOS                        
         A     R2,SIZELINE                                                      
         LA    R3,3(R3)                                                         
         BCT   R4,DETQ6                                                         
         B     XIT                                                              
         SPACE 1                                                                
DETQ10   NTR1                      POST FOR 1 DEMO                              
*                                  R2=A(TOTAL RAW)                              
         LA    R3,4(R2)            R3=A(LENGTH ACCUM)                           
         LR    R4,R2               R4=A(TOTAL EQUIV)                            
         A     R4,SIZELINE                                                      
         SH    R4,=H'4'                                                         
         LA    R5,PLANLENS                                                      
         ZIC   R0,PLANNLEN                                                      
         CLI   REQLEN,0                                                         
         BE    DETQ12                                                           
         LA    R5,REQLEN                                                        
         LA    R0,1                                                             
         SPACE 1                                                                
DETQ12   MVC   LENGTH,0(R5)                                                     
         MVC   SAVEPROF(1),N0PROF+1                                             
         MVC   SAVEPROF+1(1),N2PROF                                             
         CLI   N0PROF+1,0          IF NO EQUIV SET,                             
         BNE   *+8                                                              
         MVI   N0PROF+1,30         FORCE TO 30SEC EQUIV                         
         CLI   N2PROF,0                                                         
         BNE   *+8                                                              
         MVI   N2PROF,30                                                        
         GOTO1 VGETDEM                                                          
         MVC   N0PROF+1(1),SAVEPROF                                             
         MVC   N2PROF(1),SAVEPROF+1                                             
         L     R1,GDTIMP           POST RAW IMPRESSIONS                         
         CLI   PUPQGRP,C'Y'          OR RAW GRPS                                
         BNE   *+8                                                              
         L     R1,GDTGRP                                                        
         LR    RF,R1                                                            
         A     R1,0(R2)                                                         
         ST    R1,0(R2)                                                         
         A     RF,0(R3)                                                         
         ST    RF,0(R3)                                                         
         SPACE 1                                                                
         L     R1,GDEIMP           POST EQUIVALENT IMPRESSIONS                  
         CLI   PUPQGRP,C'Y'          OR EQUIVALENT GRPS                         
         BNE   *+8                                                              
         L     R1,GDEGRP                                                        
         LR    RF,R1                                                            
         A     R1,0(R4)                                                         
         ST    R1,0(R4)                                                         
         SPACE 1                                                                
         LA    R3,4(R3)            BUMP TO NEXT LENGTH                          
         LA    R5,1(R5)                                                         
         BCT   R0,DETQ12                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PROGRAM TOTALS FOR SUMMARY REPORT                                
         SPACE 3                                                                
PROGTOTS NTR1                                                                   
         CLI   TYPE,C'S'                                                        
         BNE   DETTOTS                                                          
         LA    R2,PROGACCS         ADD PROGRAM TO PLAN TOTALS                   
         LA    R3,PLANACCS                                                      
         LA    R0,56                                                            
         B     PROGT0                                                           
         CLI   PLANPERT,C'W'                                                    
         BNE   PROGT0                                                           
         ZIC   R1,PLANNPER                                                      
         LA    R1,1(R1)                                                         
         LR    R0,R1                                                            
PROGT0   BAS   RE,ADDEM                                                         
         SPACE 1                                                                
         LA    R2,P+1              SHOW UNITS FOR PROGRAM                       
         A     R2,DISP                                                          
         MVC   0(16,R2),PROGNAME                                                
         LA    R2,17(R2)                                                        
         MVC   0(6,R2),PROGCODE                                                 
         LA    R2,7(R2)                                                         
         LA    R3,PROGACCS                                                      
         MVC   SPACING,SPACOPT                                                  
         BAS   RE,DOUNITS                                                       
         B     XIT                                                              
         EJECT                                                                  
*              PROGRAM TOTALS FOR DETAIL REPORT                                 
         SPACE 3                                                                
DETTOTS  CLI   TYPE,C'D'                                                        
         BNE   XIT                                                              
         LA    R2,PROGACCS         ADD PROGRAM TO PLAN TOTALS                   
         LA    R3,PLANACCS                                                      
         LA    R0,30                                                            
         BAS   RE,ADDEM                                                         
         SPACE 1                                                                
         LA    R2,P+1              SHOW UNITS FOR PROGRAM                       
         A     R2,DISP                                                          
         CLI   QUARTER,4                                                        
         BE    DETT4                                                            
         BAS   RE,GETPRGRC                                                      
DETT4    MVC   0(16,R2),PROGNAME                                                
*        CLI   PLANPERT,C'W'                                                    
*        BNE   DETT5                                                            
*        CLC   0(16,R2),=16X'40'                                                
*        BE    XIT                                                              
DETT5    LA    R2,17(R2)                                                        
         MVC   0(6,R2),PROGCODE                                                 
         LA    R2,7(R2)                                                         
         MVC   0(3,R2),PROGDAY                                                  
         LA    R2,4(R2)                                                         
         MVC   0(11,R2),PROGTIME                                                
         LA    R2,12(R2)                                                        
         LA    R3,PROGACCS                                                      
         MVC   SPACING,SPACOPT                                                  
         MVI   TOTLEVEL,C'D'                                                    
         BAS   RE,DODETS                                                        
         B     XIT                                                              
*                                                                               
*                     READ PROGRAM REC TO GET NAME FOR THAT QUARTER             
GETPRGRC NTR1                                                                   
         MVC   OVWORK(20),KEY                                                   
         MVC   OVWORK+20(96),DMWORK                                             
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPGRECD,R4                                                       
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BINAGYMD                                                  
         MVC   NPGKNET,NETMKTN                                                  
         MVC   NPGKPROG,PROGCODE                                                
*                                                                               
         L     R3,AIO1            GET PERIOD FROM PROG ASSIGNMENT REC           
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   GPRGX                                                            
         USING NPUBD,R3                                                         
         MVC   NPGKEND,NPUBPER                                                  
         DROP  R3                                                               
*                                                                               
         GOTO1 VSETSPOT                                                         
         CLI   PLANPERT,C'W'                                                    
         BE    GPRG3                                                            
         MVC   DUB(2),NPGKEND      PRESET TO 1ST OF MONTH                       
         MVI   DUB+2,1                                                          
         CLI   PLANPERT,C'Q'       IF THIS IS QUARTERLY                         
         BNE   GPRG2                  USE START OF QUARTER                      
         ZIC   R1,NPGKEND+1         PICK UP QUARTER (1-4)                       
         BCTR  R1,0                (0-3)                                        
         MH    R1,=H'3'            (0,3,6,9)                                    
         LA    R1,1(R1)            (1,4,7,10)                                   
         STC   R1,DUB+1                                                         
         SPACE 1                                                                
GPRG2    GOTO1 DATCON,DMCB,(3,DUB),(2,NPGKEND)                                  
         SPACE 1                                                                
GPRG3    GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     SHOULD MATCH ON PROGRAM                      
         BNE   GPRGX                                                            
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,X'92'        MOST DETAILS FROM 92 ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGELEM,R3                                                       
         MVC   PROGNAME,NPGNAME                                                 
GPRGX    MVC   KEY(20),OVWORK                                                   
         MVC   DMWORK(96),OVWORK+20                                             
         GOTO1 VSETUNT                                                          
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*              PRINT REPORT TOTALS                                              
         SPACE 3                                                                
REPTOTS  NTR1                                                                   
         CLI   TYPE,C'S'                                                        
         BNE   REPDETS                                                          
         GOTO1 SPOOL,DMCB,(R8)     SPACE BEFORE PLAN TOTALS                     
         LA    R2,P+18                                                          
         A     R2,DISP                                                          
         MVC   0(6,R2),=C'TOTALS'                                               
         LA    R2,7(R2)                                                         
         LA    R3,PLANACCS                                                      
         BAS   RE,DOUNITS                                                       
         SPACE 1                                                                
CLOSEBOX L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    CLBX2                                                            
         USING BOXD,R4                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         MVI   BOXROWS+58,C' '                                                  
         MVI   BOXINIT,0                                                        
         SPACE 1                                                                
CLBX2    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              PRINT PLAN TOTALS FOR DETAIL REPORT                              
         SPACE 3                                                                
REPDETS  CLI   TYPE,C'D'                                                        
         BNE   REPQUART                                                         
         LA    R3,TOTALS           PICK UP BUDGET                               
         USING ACCUMD,R3                                                        
         MVC   SAVEBUDG,ACBUDGET                                                
         GOTO1 SPOOL,DMCB,(R8)     SPACE BEFORE PLAN TOTALS                     
         LA    R2,P+31                                                          
         A     R2,DISP                                                          
         MVC   0(6,R2),=C'TOTALS'                                               
         LA    R2,10(R2)                                                        
         LA    R3,PLANACCS                                                      
         MVI   TOTLEVEL,C'T'                                                    
         BAS   RE,DODETS                                                        
         B     CLOSEBOX                                                         
         EJECT                                                                  
*              CONTROL QUARTERLY SUMMARY PRINTING                               
         SPACE 3                                                                
REPQUART CLI   TYPE,C'Q'                                                        
         BNE   XIT                                                              
         LA    R2,QBUFF                                                         
         LA    R3,QUALPHA2                                                      
         LA    R5,BUDGETS                                                       
         LA    R0,5                                                             
         SPACE 1                                                                
RQ2      ST    R5,SAVEBUDG                                                      
         BAS   RE,RQ6                                                           
         A     R2,SIZEACC                                                       
         LA    R3,7(R3)                                                         
         CH    R0,=H'1'            IF WE ARE NOT THROUGH                        
         BE    XIT                                                              
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    RQ4                                                              
         ZIC   R1,LINE             DRAW A HORIZONTAL LINE                       
         LA    R1,BOXROWS-1(R1)                                                 
         DROP  R4                                                               
         MVI   0(R1),C'M'                                                       
         SPACE 1                                                                
RQ4      GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,20(R5)                                                        
         BCT   R0,RQ2                                                           
         SPACE 1                                                                
RQ6      NTR1                      CONTROL PRINTING FOR QUARTER                 
         LA    R4,P+1                                                           
         A     R4,DISP                                                          
         MVC   0(7,R4),0(R3)       THIS DOES 4TH. 19  ETC                       
         ZIC   R1,PLANYEAR                                                      
         CLI   0(R3),C'4'                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         AH    R1,=H'1900'          Y2K FIX                                     
         EDIT  (R1),(4,5(R4))      NOW HAVE  4TH. 1987                          
         LA    R4,10(R4)                                                        
         MVC   0(5,R4),=C'HOMES'                                                
         BAS   RE,RQ10             FIRST LINE IS HOMES                          
         A     R2,SIZELINE                                                      
         LA    R3,DEMOS                                                         
         ZIC   R5,NDEMOS                                                        
         GOTO1 VSETDB                                                           
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         SPACE 1                                                                
RQ8      GOTO1 DEMOCON,DMCB,(0,0(R3)),(10,WORK),(C'S',DBLOCK)                   
         MVC   0(10,R4),WORK                                                    
         BAS   RE,RQ10             SUBSEQUENT LINES ARE DEMOS                   
         A     R2,SIZELINE                                                      
         LA    R3,3(R3)                                                         
         BCT   R5,RQ8                                                           
         B     XIT                                                              
         EJECT                                                                  
*              PRINT LINE FOR HOMES OR 1 DEMO                                   
         SPACE 3                                                                
RQ10     NTR1                                                                   
*                                  R2=A(ACCUM LINE)                             
*                                  R4=A(PRINT AREA)                             
         LA    R4,11(R4)                                                        
         L     R5,SAVEBUDG         R5=A(BUDGETS FOR QUARTER)                    
         BAS   RE,RQ14             DO RAW TOTALS                                
         LA    R2,4(R2)                                                         
         LA    R4,15(R4)                                                        
         LA    R5,4(R5)                                                         
         ZIC   R3,PLANNLEN                                                      
         SPACE 1                                                                
RQ12     BAS   RE,RQ14             THEN LENGTH ANALYSIS                         
         LA    R2,4(R2)                                                         
         LA    R4,15(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R3,RQ12                                                          
         L     R5,SAVEBUDG                                                      
         BAS   RE,RQ14             FINALLY EQUIVALENT TOTALS                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
RQ14     NTR1                                                                   
         L     R1,0(R5)            R5=A(BUDGET)                                 
         OC    0(4,R2),0(R2)       IMPRESSION                                   
         BZ    XIT                                                              
         CLI   PUPQGRP,C'Y'                                                     
         BE    RQ16                                                             
         LTR   R1,R1                                                            
         BZ    RQ15                                                             
         M     R0,=F'2000'         COMPUTE AND SHOW CPM AND IMPS                
         D     R0,0(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(6,0(R4)),2,FLOAT=$                                         
         SPACE 1                                                                
RQ15     MVC   FULL,0(R2)                                                       
         BAS   RE,ROUND                                                         
         EDIT  (4,FULL),(7,7(R4))                                               
         B     XIT                                                              
         SPACE 1                                                                
RQ16     LTR   R1,R1                                                            
         BZ    RQ17                                                             
         M     R0,=F'20'           COMPUTE AND SHOW CPP AND GRPS                
         D     R0,0(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(6,0(R4)),FLOAT=$                                           
         SPACE 1                                                                
RQ17     EDIT  (4,0(R2)),(7,7(R4)),1                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT UNITS                                          
         SPACE 3                                                                
*              INPUTS              R2=A(PRINT AREA)                             
*                                  R3=A(ACCUMULATORS)                           
         SPACE 1                                                                
DOUNITS  NTR1                                                                   
         CLI   PLANPERT,C'W'                                                    
         BNE   DOUN1                                                            
         B     MONUNITS                                                         
         ZIC   R4,PLANNLEN                                                      
         BAS   RE,DOLENS                                                        
         LA    R2,8(R2)                                                         
         ZIC   R1,PLANNPER                                                      
         LA    R1,1(R1)                                                         
         LR    R0,R1                                                            
         B     MONUN2                                                           
DOUN1    CLI   PLANPERT,C'M'                                                    
         BE    MONUNITS                                                         
         ZIC   R4,PLANNLEN                                                      
         MH    R4,=H'5'                                                         
         SPACE 1                                                                
QUNITS   EDIT  (4,0(R3)),(3,0(R2))                                              
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,QUNITS                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
MONUNITS ZIC   R4,PLANNLEN                                                      
         BAS   RE,DOLENS           SHOW LENGTHS STACKED                         
         LA    R2,8(R2)                                                         
         LA    R0,14                                                            
         SPACE 1                                                                
MONUN2   BAS   RE,MONUN4                                                        
         LA    R2,6(R2)            (NOTE R3 UPDATED BY MONUN4)                  
         BCT   R0,MONUN2                                                        
         MVI   SPACING,2                                                        
         CLI   PLANNLEN,1                                                       
         BNE   *+10                                                             
         MVC   SPACING,SPACOPT                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
MONUN4   NTR1                                                                   
         SPACE 1                                                                
MONUN6   EDIT  (4,0(R3)),(3,0(R2))                                              
         LA    R2,132(R2)                                                       
         LA    R3,4(R3)                                                         
         BCT   R4,MONUN6                                                        
         XIT1  REGS=(R3)                                                        
         SPACE 1                                                                
DOLENS   NTR1                                                                   
         LA    R3,PLANLENS                                                      
         SPACE 1                                                                
DOLENS2  EDIT  (1,0(R3)),(4,0(R2))                                              
         LA    R2,132(R2)                                                       
         LA    R3,1(R3)                                                         
         BCT   R4,DOLENS2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT DETAILS                                        
         SPACE 3                                                                
*              INPUTS              R2=A(PRINT AREA)                             
*                                  R3=A(ACCUMULATORS)                           
*                                  TOTLEVEL = D(ETAIL) OR T(OTAL)               
         SPACE 1                                                                
DODETS   NTR1                                                                   
         USING DETD,R3                                                          
         LA    R4,DETSHR           SET UP FOR SHR/HUT/RTG                       
         LA    R5,3                                                             
         SPACE 1                                                                
DODETS2  L     R0,0(R4)                                                         
         LTR   R0,R0                                                            
         BZ    DODETS4                                                          
         SRDA  R0,31                                                            
         OC    DETTUNIT,DETTUNIT                                                
         BZ    DODETS4                                                          
         D     R0,DETTUNIT                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(4,0(R2)),1                                                 
         SPACE 1                                                                
DODETS4  LA    R2,5(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,DODETS2                                                       
         SPACE 1                                                                
         LA    R4,DETLUNIT         UNITS BY SECOND LENGTH                       
         ZIC   R5,PLANNLEN                                                      
         SPACE 1                                                                
DODETS6  EDIT  (4,0(R4)),(3,0(R2))                                              
         LA    R2,4(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,DODETS6                                                       
         CLI   UNTOTS,C'N'         USUALLY TOTAL UNITS                          
         BE    DODETS8                                                          
         EDIT  (4,DETTUNIT),(3,0(R2))     TOTAL UNITS                           
         LA    R2,4(R2)                                                         
         SPACE 1                                                                
DODETS8  CLI   TOTLEVEL,C'D'       AT THE DETAIL LEVEL                          
         BNE   DODETS20                                                         
         CLI   PUPVPH,C'N'         OPTIONAL VPH SUPPRESSION                     
         BE    *+12                                                             
         BAS   RE,DOVPHS                                                        
         LA    R2,132(R2)                                                       
         CLI   PUPIMP,C'N'         OPTIONAL IMP SUPPRESSION                     
         BE    *+12                                                             
         BAS   RE,DOIMPS                                                        
         LA    R2,132(R2)                                                       
         CLI   PUPGRP,C'Y'         OPTIONAL GRP INCLUSION                       
         BNE   *+8                                                              
         BAS   RE,DOGRPS                                                        
         CLC   P2,SPACES                                                        
         BE    *+8                                                              
         MVI   SPACING,2                                                        
         CLI   PLANPERT,C'W'       .IF IT'S WEEKS                               
         BNE   DODETS10                                                         
         LA    R1,P+41             (FROM DETTOTS RTN)                           
         A     R1,DISP                                                          
         CLC   0(20,R1),=20X'40'   .AND THERE ARE NO UNITS                      
         BH    DODETS10                                                         
         LA    R1,P+1                                                           
         A     R1,DISP                                                          
         XC    0(16,R1),0(R1)                                                   
         LA    R1,17(R1)                                                        
         XC    0(6,R1),0(R1)                                                    
         LA    R1,7(R1)                                                         
         XC    0(3,R1),0(R1)                                                    
         LA    R1,4(R1)                                                         
         XC    0(11,R1),0(R1)                                                   
         LA    R1,12(R1)                                                        
         LA    R1,132(R1)                                                       
         XC    0(4,R1),0(R1)                                                    
         B     XIT                                                              
*                                                                               
         LA    R1,P+1                                                           
         A     R1,DISP                                                          
         LA    R2,132(R1)                                                       
         XC    0(80,R1),0(R1)      .CLEAR PROG NAMES FROM P LINE                
         XC    0(80,R2),0(R2)                                                   
         B     XIT                                                              
DODETS10 GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
DODETS20 BAS   RE,DOIMPS           AT THE TOTAL LEVEL                           
         LA    R2,132(R2)          SHOW IMPS CPM GRPS CPP                       
         BAS   RE,DOCPMS                                                        
         LA    R2,132(R2)                                                       
         BAS   RE,DOGRPS                                                        
         LA    R2,132(R2)                                                       
         BAS   RE,DOCPPS                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR DETAIL REPORT                            
         SPACE 3                                                                
DOVPHS   NTR1                                                                   
         OC    DETHOMES,DETHOMES                                                
         BZ    XIT                                                              
         MVC   0(4,R2),=C'VPH '                                                 
         LA    R2,13(R2)                                                        
         LA    R4,DETTIMPS                                                      
         ZIC   R5,NDEMOS                                                        
         SPACE 1                                                                
DOVPHS2  L     R1,0(R4)            WORK OUT AVERAGE VPM                         
         LTR   R1,R1                                                            
         BZ    DOVPHS4                                                          
         M     R0,=F'2000'                                                      
         D     R0,DETHOMES                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R2))                                                   
         SPACE 1                                                                
DOVPHS4  LA    R2,8(R2)                                                         
         LA    R4,8(R4)                                                         
         BCT   R5,DOVPHS2                                                       
         B     XIT                                                              
         SPACE 1                                                                
DOIMPS   NTR1                                                                   
         MVC   0(4,R2),=C'IMPS'                                                 
         LA    R2,5(R2)                                                         
         LA    R3,DETHOMES                                                      
         ZIC   R5,NDEMOS                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
DOIMPS2  MVC   FULL,0(R3)                                                       
         BAS   RE,ROUND                                                         
         EDIT  (4,FULL),(7,0(R2))                                               
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R5,DOIMPS2                                                       
         B     XIT                                                              
         SPACE 1                                                                
DOCPMS   NTR1                                                                   
         MVC   0(4,R2),=C'CPM '                                                 
         LA    R2,5(R2)                                                         
         OC    SAVEBUDG,SAVEBUDG                                                
         BZ    XIT                                                              
         LA    R3,DETHOMES                                                      
         ZIC   R5,NDEMOS                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
DOCPMS2  OC    0(4,R3),0(R3)                                                    
         BZ    DOCPMS4                                                          
         L     R1,SAVEBUDG                                                      
         M     R0,=F'2000'                                                      
         D     R0,0(R3)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R2)),2,FLOAT=$                                         
         SPACE 1                                                                
DOCPMS4  LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R5,DOCPMS2                                                       
         B     XIT                                                              
         SPACE 1                                                                
DOGRPS   NTR1                                                                   
         MVC   0(4,R2),=C'GRPS'                                                 
         LA    R2,5(R2)                                                         
         LA    R4,DETGRPS                                                       
         ZIC   R5,NDEMOS                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
DOGRPS2  EDIT  (4,0(R4)),(7,0(R2)),1,ZERO=BLANK                                 
         LA    R2,8(R2)                                                         
         LA    R4,8(R4)                                                         
         BCT   R5,DOGRPS2                                                       
         B     XIT                                                              
         SPACE 1                                                                
DOCPPS   NTR1                                                                   
         MVC   0(4,R2),=C'CPP '                                                 
         LA    R2,5(R2)                                                         
         OC    SAVEBUDG,SAVEBUDG                                                
         BZ    XIT                                                              
         LA    R4,DETGRPS                                                       
         ZIC   R5,NDEMOS                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
DOCPPS2  OC    0(4,R4),0(R4)                                                    
         BZ    DOCPPS4                                                          
         L     R1,SAVEBUDG                                                      
         M     R0,=F'20'                                                        
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R2)),FLOAT=$                                           
         SPACE 1                                                                
DOCPPS4  LA    R2,8(R2)                                                         
         LA    R4,8(R4)                                                         
         BCT   R5,DOCPPS2                                                       
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
*-ROUTINE ROUNDS THE IMPS4                                                      
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
*        ST    R1,0(R4)                                                         
         ST    R1,FULL                                                          
         B     XIT                                                              
         EJECT                                                                  
*              TARGET TOTALS FOR SUMMARY REPORT                                 
         SPACE 3                                                                
TARGTOTS NTR1                                                                   
         CLI   LINE,48             NEED 10 LINES TO PRINT                       
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         L     R4,ABOX             SET UP BOXES IF NEEDED                       
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    TT4                                                              
         CLI   BOXOPT,C'N'                                                      
         BE    TT4                                                              
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         SPACE 1                                                                
         MVC   BOXROWS,SPACES                                                   
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'T'                                                       
         MVI   2(R1),C'M'                                                       
         MVI   7(R1),C'B'                                                       
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         A     R2,DISP                                                          
         DROP  R4                                                               
         MVI   0(R2),C'L'                                                       
         LA    R2,24(R2)                                                        
         ZIC   R3,NDEMOS                                                        
         SPACE 1                                                                
TT2      MVI   0(R2),C'C'                                                       
         LA    R2,11(R2)                                                        
         BCT   R3,TT2                                                           
         MVI   0(R2),C'R'                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
TT4      LA    R2,P+25             PUT IN DEMO NAMES                            
         A     R2,DISP                                                          
         LA    R3,DEMOS                                                         
         ZIC   R4,NDEMOS                                                        
         GOTO1 VSETDB                                                           
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         SPACE 1                                                                
TT6      GOTO1 DEMOCON,DMCB,(0,0(R3)),(10,WORK),(C'S',DBLOCK)                   
         MVC   0(10,R2),WORK                                                    
         LA    R2,11(R2)                                                        
         LA    R3,3(R3)                                                         
         BCT   R4,TT6                                                           
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EJECT                                                                  
*              TARGET TOTALS - DEAL WITH DATA LINES                             
         SPACE 3                                                                
         LA    R2,P+2                                                           
         A     R2,DISP                                                          
         MVC   0(20,R2),=CL20'TOTAL IMPRESSIONS'                                
         MVC   132(20,R2),=CL20'DEMOGRAPHIC GRPS.'                              
         MVC   264(20,R2),=CL20'DEMOGRAPHIC CPMS.'                              
         CLC   DEMOS(3),TARGET                                                  
         BNE   TT8                                                              
         OC    GUARCPM,GUARCPM                                                  
         BZ    TT8                                                              
         MVC   396(20,R2),=CL20'GUARANTEED CPM.'                                
         LA    R3,396+23(R2)                                                    
         EDIT  (4,GUARCPM),(7,0(R3)),2,FLOAT=$                                  
         SPACE 1                                                                
TT8      LA    R2,23(R2)                                                        
         LA    R3,TARGACCS                                                      
         ZIC   R4,NDEMOS                                                        
         SPACE 1                                                                
TT10     BAS   RE,TT12                                                          
         LA    R2,11(R2)                                                        
         LA    R3,8(R3)                                                         
         BCT   R4,TT10                                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
TT12     NTR1                                                                   
         MVC   FULL,4(R3)                                                       
         BAS   RE,ROUND                                                         
         EDIT  (4,FULL),(7,0(R2)) IMPS                                          
         LA    R2,132(R2)                                                       
         EDIT  (4,0(R3)),(7,0(R2)),1  GRPS                                      
         LA    R2,132(R2)                                                       
         OC    SAVEBUDG,SAVEBUDG                                                
         BZ    XIT                                                              
         OC    4(4,R3),4(R3)       IMPS                                         
         BZ    XIT                                                              
         L     R1,SAVEBUDG                                                      
         M     R0,=F'2000'                                                      
         D     R0,4(R3)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R2)),2,FLOAT=$                                         
         B     XIT                                                              
         EJECT                                                                  
*              PRINT GRAND TOTALS                                               
         SPACE 3                                                                
GRAND    NTR1                                                                   
         CLI   LINE,50             NEED 8 LINES TO PRINT                        
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         MVC   DISP,=F'30'         SET DISPLACEMENT                             
         CLI   LEFTOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   DISP+3,4                                                         
         L     R4,ABOX             SET UP BOXES IF NEEDED                       
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    GRAND2                                                           
         CLI   BOXOPT,C'N'                                                      
         BE    GRAND2                                                           
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         SPACE 1                                                                
         MVC   BOXROWS,SPACES                                                   
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'T'                                                       
         MVI   5(R1),C'B'                                                       
         MVC   BOXCOLS,SPACES                                                   
         LA    R1,BOXCOLS                                                       
         DROP  R4                                                               
         A     R1,DISP                                                          
         MVI   0(R1),C'L'                                                       
         MVI   20(R1),C'C'                                                      
         MVI   31(R1),C'R'                                                      
         LA    R1,40(R1)                                                        
         MVI   0(R1),C'L'                                                       
         MVI   20(R1),C'C'                                                      
         MVI   31(R1),C'R'                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
GRAND2   LA    R5,TOTALS                                                        
         USING ACCUMD,R5                                                        
         MVC   SAVEBUDG,ACBUDGET                                                
         LA    R2,P+2                                                           
         A     R2,DISP                                                          
         MVC   0(17,R2),=CL17'TOTAL COST:'                                      
         LA    R2,19(R2)                                                        
         EDIT  (4,ACBUDGET),(9,0(R2)),FLOAT=$                                   
         LA    R2,21(R2)                                                        
         MVC   0(17,R2),=CL17'AVERAGE RATING'                                   
         LA    R2,19(R2)                                                        
         L     R1,ACHOMGRP                                                      
         LTR   R1,R1                                                            
         BZ    GRAND4                                                           
         OC    ACUNITS,ACUNITS                                                  
         BZ    GRAND4                                                           
         M     R0,=F'2'                                                         
         D     R0,ACUNITS                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(9,0(R2)),1                                                 
         SPACE 1                                                                
GRAND4   GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P+2                                                           
         A     R2,DISP                                                          
         MVC   0(17,R2),=CL17'TOTAL GRPS:'                                      
         LA    R2,19(R2)                                                        
         EDIT  (4,ACHOMGRP),(9,0(R2)),1                                         
         LA    R2,21(R2)                                                        
         MVC   0(17,R2),=CL17'AVERAGE COST/GRP'                                 
         LA    R2,19(R2)                                                        
         L     R1,ACBUDGET                                                      
         LTR   R1,R1                                                            
         BZ    GRAND6                                                           
         OC    ACHOMGRP,ACHOMGRP                                                
         BZ    GRAND6                                                           
         M     R0,=F'20'                                                        
         D     R0,ACHOMGRP                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(9,0(R2)),FLOAT=$                                           
         SPACE 1                                                                
GRAND6   GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P+2                                                           
         A     R2,DISP                                                          
         MVC   0(17,R2),=CL17'NUMBER OF UNITS'                                  
         LA    R2,19(R2)                                                        
         EDIT  (4,ACUNITS),(9,0(R2))                                            
         LA    R2,21(R2)                                                        
         MVC   0(17,R2),=CL17'AVERAGE COST/UNIT'                                
         LA    R2,19(R2)                                                        
         L     R1,ACBUDGET                                                      
         LTR   R1,R1                                                            
         BZ    GRAND8                                                           
         OC    ACUNITS,ACUNITS                                                  
         BZ    GRAND8                                                           
         M     R0,=F'2'                                                         
         D     R0,ACUNITS                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(9,0(R2)),FLOAT=$                                           
         SPACE 1                                                                
GRAND8   GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P+2                                                           
         A     R2,DISP                                                          
         MVC   0(17,R2),=CL17'TOTAL HOMES (000)'                                
         LA    R2,19(R2)                                                        
         MVC   FULL,ACHOMIMP                                                    
         BAS   RE,ROUND                                                         
         EDIT  (4,FULL),(9,0(R2))                                               
         LA    R2,21(R2)                                                        
         MVC   0(17,R2),=CL17'AVERAGE COST/000'                                 
         LA    R2,19(R2)                                                        
         L     R1,ACBUDGET                                                      
         LTR   R1,R1                                                            
         BZ    GRAND10                                                          
         OC    ACHOMIMP,ACHOMIMP                                                
         BZ    GRAND10                                                          
         M     R0,=F'2000'                                                      
         D     R0,ACHOMIMP                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(9,0(R2)),2,FLOAT=$                                         
         SPACE 1                                                                
GRAND10  MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    TOTALS,TOTALS                                                    
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO COMPUTE DISPLACEMENT                                  
         SPACE 3                                                                
COMPDISP NTR1                                                                   
         XC    DISP,DISP                                                        
         CLI   LEFTOPT,C'Y'                                                     
         BE    XIT                                                              
         CLI   TYPE,C'T'                                                        
         BE    CDTARG                                                           
         CLI   TYPE,C'D'                                                        
         BE    CDDET                                                            
         CLI   TYPE,C'Q'                                                        
         BE    CDQUART                                                          
         CLI   TYPE,C'S'                                                        
         BE    CDSUMM                                                           
         B     XIT                                                              
         SPACE 1                                                                
CDSUMM   CLI   PLANPERT,C'W'                                                    
         BNE   CDS1                                                             
         MVI   DISP+3,8                                                         
         B     XIT                                                              
         MVI   DISP+3,0                                                         
         B     XIT                                                              
CDS1     MVI   DISP+3,8            DISPLACEMENT FOR SUMMARY                     
         CLI   PLANPERT,C'M'       MONTHLY IS FIXED                             
         BE    XIT                                                              
         ZIC   R1,PLANNLEN         QUARTERLY DEPENDS ON LENGTHS                 
         MH    R1,=H'20'                                                        
         LA    R1,25(R1)                                                        
         LA    R0,132                                                           
         SR    R0,R1                                                            
         SRL   R0,1                                                             
         ST    R0,DISP                                                          
         B     XIT                                                              
         SPACE 1                                                                
CDTARG   ZIC   R1,NDEMOS           DISPLACEMENT FOR TARGET SUMMARY              
         SLL   R1,3                                                             
         LA    R1,25(R1)                                                        
         LA    R0,132                                                           
         SR    R0,R1                                                            
         SRL   R0,1                                                             
         ST    R0,DISP                                                          
         B     XIT                                                              
         SPACE 1                                                                
CDDET    SR    R0,R0               DISPLACEMENT FOR DETAIL REPORTS              
         CLI   PLANNLEN,1                                                       
         BNE   *+8                                                              
         LA    R0,4                                                             
         CLI   PLANNLEN,2                                                       
         BNE   *+8                                                              
         LA    R0,2                                                             
         LA    R2,6                                                             
         ZIC   R1,NDEMOS                                                        
         SR    R2,R1                                                            
         BNM   *+6                                                              
         SR    R2,R2                                                            
         SLL   R2,2                                                             
         AR    R0,R2                                                            
         ST    R0,DISP                                                          
         B     XIT                                                              
         SPACE 1                                                                
CDQUART  ZIC   R1,PLANNLEN         DISPLACEMENT FOR QUARTERLY                   
         LA    R1,2(R1)                                                         
         MH    R1,=H'15'                                                        
         LA    R1,19(R1)                                                        
         LA    R0,132                                                           
         SR    R0,R1                                                            
         SRA   R0,1                                                             
         ST    R0,DISP                                                          
         B     XIT                                                              
         SPACE 1                                                                
COMPACC  NTR1                                                                   
         ZIC   R1,PLANNLEN         FIGURE OUT ACCUMUALTOR SIZES                 
         LA    R1,2(R1)                                                         
         SLL   R1,2                                                             
         ST    R1,SIZELINE                                                      
         ZIC   R0,NDEMOS                                                        
         AH    R0,=H'1'                                                         
         MR    R0,R0                                                            
         ST    R1,SIZEACC                                                       
         SPACE 1                                                                
         MVC   BASELEN,N0PROF+1    PICK UP PROFILE EQUIV                        
         CLI   PUPQGRP,C'Y'                                                     
         BNE   *+10                                                             
         MVC   BASELEN,N2PROF                                                   
         CLI   BASELEN,0                                                        
         BNE   *+8                                                              
         MVI   BASELEN,30          DEFAULT IS 30                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD AND AVERAGE LINES                                 
         SPACE 3                                                                
ADDEM    NTR1                                                                   
*                                  R2=A(FROM LINE)                              
*                                  R3=A(TO LINE)                                
*                                  R0=NUMBER OF ACCUMS                          
         SPACE 1                                                                
ADDEM2   L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ADDEM2                                                        
         B     XIT                                                              
         SPACE 1                                                                
AVERAGE  NTR1                                                                   
*                                  R2=A(LINE TO BE AVERAGED)                    
*                                  R3=DIVIDOR                                   
*                                  R4=NUMBER OF ITEMS                           
         SPACE 1                                                                
AVE2     L     R0,0(R2)                                                         
         SRDA  R0,31                                                            
         DR    R0,R3                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R4,AVE2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              SUB-ROUTINE TO SHOW CPM                                          
         SPACE 3                                                                
*              INPUTS              R2=A(OUTPUT AREA)                            
*                                  R3=A(COST)                                   
*                                  R4=A(IMPS)                                   
         SPACE 1                                                                
CPM      NTR1                                                                   
         L     R1,0(R4)            PICK UP IMPS                                 
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         SPACE 1                                                                
*                                  COMPUTE CPM IN PENNIES                       
         LR    R1,R3                                                            
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'200'                                                       
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R2)),2,FLOAT=$                                         
         CLI   0(R2),C' '                                                       
         BE    XIT                                                              
         SPACE 1                                                                
         LR    R1,R3               TOO BIG SO COMPUTE TO $                      
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'2'                                                         
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R2)),FLOAT=$                                           
         B     XIT                                                              
         EJECT                                                                  
*              SUB-ROUTINE TO SHOW CPP                                          
         SPACE 3                                                                
*              INPUTS              R2=A(OUTPUT AREA)                            
*                                  R3=A(COST)                                   
*                                  R4=A(GRPS)                                   
         SPACE 1                                                                
CPP      NTR1                                                                   
         L     R1,0(R4)            PICK UP GRPS                                 
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         SPACE 1                                                                
*                                  COMPUTE CPP IN PENNIES                       
         LR    R1,R3                                                            
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
         LR    R1,R3               TOO BIG SO COMPUTE TO $                      
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'20'                                                        
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R2)),FLOAT=$                                           
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   HEAD2+1(1),HORIZ    UNDERLINE SYSTEM NAME                        
         MVC   HEAD2+2(21),HEAD2+1                                              
         MVC   WORK(40),TITLE      REPORT TITLE                                 
         OC    WORK,SPACES                                                      
         CLI   WORK,C' '                                                        
         BH    HOOK2                                                            
         SPACE 1                                                                
         MVC   WORK(40),=CL40'PLAN SUMMARY REPORT'                              
         CLI   TYPE,C'S'                                                        
         BE    HOOK2                                                            
         MVC   WORK(40),=CL40'PLAN DETAIL REPORT'                               
         CLI   TYPE,C'D'                                                        
         BE    HOOK2                                                            
         MVC   WORK(40),=CL40'QUARTERLY SUMMARY REPORT'                         
         CLI   TYPE,C'Q'                                                        
         BE    HOOK2                                                            
         MVC   WORK(40),=CL40'GRAND TOTALS'                                     
         SPACE 1                                                                
HOOK2    GOTO1 CENTER,DMCB,WORK,40                                              
         MVC   HEAD1+41(40),WORK                                                
         GOTO1 UNDERLIN,DMCB,(40,HEAD1+40),(HORIZ,HEAD2+40)                     
         SPACE 1                                                                
         MVC   HEAD4+9(3),PUPCLI                                                
         MVC   HEAD4+15(20),PUPCLIN                                             
         MVC   HEAD5+9(4),PUPNET                                                
         MVC   HEAD4+62(8),PUPDPT                                               
         MVC   HEAD5+59(4),PUPPLAN                                              
         LA    R2,HEAD5+64                                                      
         ZIC   R1,PLANYEAR                                                      
         CLI   QUARTER,4            IF WE ARE SHOWING 4TH QUARTER               
         BNE   *+6                                                              
         BCTR  R1,0                USE PREVIOUS YEAR                            
         AH    R1,=H'1900'          Y2K FIX                                     
         EDIT  (R1),(4,0(R2))                                                   
         SPACE 1                                                                
HOOK3    CLI   QUARTER,0           MAY NEED TO SHOW QUARTER                     
         BE    HOOK4                                                            
         ZIC   R1,QUARTER                                                       
         BCTR  R1,0                                                             
         MH    R1,=H'6'                                                         
         LA    R1,QUALPHA(R1)                                                   
         MVC   HEAD6+54(6),0(R1)                                                
         MVC   HEAD6+61(7),=C'QUARTER'                                          
         GOTO1 SQUASHER,DMCB,HEAD6+54,14                                        
         SPACE 1                                                                
HOOK4    BAS   RE,HOOKHEAD                                                      
         BAS   RE,HOOKBOX                                                       
         B     XIT                                                              
         EJECT                                                                  
*              GENERATE THE HEADINGS                                            
         SPACE 3                                                                
HOOKHEAD NTR1                                                                   
         CLI   TYPE,C'S'                                                        
         BNE   HHDET                                                            
         LA    R2,HEAD10+1                                                      
         A     R2,DISP                                                          
         MVC   0(12,R2),=C'PROGRAM NAME'                                        
         LA    R2,17(R2)                                                        
         MVC   0(6,R2),=C' PROG.'                                               
         MVC   132(6,R2),=C' CODE '                                             
         LA    R2,7(R2)                                                         
         CLI   PLANPERT,C'W'                                                    
*        BE    HHWEEK                                                           
         BE    HHMON                                                            
         CLI   PLANPERT,C'M'                                                    
         BE    HHMON                                                            
         LA    R3,HHQLIST                                                       
         LA    R4,5                                                             
         SPACE 1                                                                
HHQ2     ZIC   R5,PLANNLEN                                                      
         BAS   RE,HHLENS           PUT LENGTHS ON HEAD11                        
         BCTR  R5,0                                                             
         SLL   R5,1                                                             
         AR    R5,R2                                                            
         MVC   0(3,R5),0(R3)       CENTER QUARTER IN CHUNK                      
         ZIC   R5,PLANNLEN                                                      
         SLL   R5,2                                                             
         AR    R2,R5                                                            
         LA    R3,3(R3)                                                         
         BCT   R4,HHQ2                                                          
         B     XIT                                                              
         SPACE 1                                                                
HHLENS   NTR1                                                                   
         LA    R2,132(R2)                                                       
         LA    R3,PLANLENS                                                      
         ZIC   R4,PLANNLEN                                                      
         SPACE 1                                                                
HHLENS2  EDIT  (1,0(R3)),(3,0(R2))                                              
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R2),C':'                                                       
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,HHLENS2                                                       
         B     XIT                                                              
         SPACE 1                                                                
HHMON    MVC   0(6,R2),=C'LENGTH'                                               
         LA    R2,7(R2)                                                         
         LA    R3,HHMLIST                                                       
         LA    R4,13                                                            
         ZIC   R5,PLANYEAR                                                      
         BCTR  R5,0                                                             
         SPACE 1                                                                
HHMON2   MVC   0(3,R2),0(R3)                                                    
         EDIT  (R5),(2,3(R2))                                                   
         LA    R2,6(R2)                                                         
         LA    R3,3(R3)                                                         
         CLC   0(3,R3),=C'JAN'                                                  
         BNE   *+8                                                              
         LA    R5,1(R5)                                                         
         BCT   R4,HHMON2                                                        
         MVC   0(5,R2),=C'TOTAL'                                                
         B     XIT                                                              
         SPACE 1                                                                
HHWEEK   MVC   0(6,R2),=C'LENGTH'                                               
         LA    R2,7(R2)                                                         
         LA    R3,PLANPLST                                                      
         ZIC   R4,PLANNPER                                                      
         SPACE 1                                                                
HHWEK2   GOTO1 DATCON,DMCB,(2,0(R3)),(4,0(R2))                                  
         LA    R2,6(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,HHWEK2                                                        
         A     R2,=F'-1'                                                        
         MVC   0(5,R2),=C'TOTAL'                                                
         B     XIT                                                              
         EJECT                                                                  
*              HEADINGS FOR DETAIL REPORT                                       
         SPACE 3                                                                
HHDET    CLI   TYPE,C'D'                                                        
         BNE   HHQS                                                             
         LA    R2,HEAD10+1                                                      
         A     R2,DISP                                                          
         MVC   0(12,R2),=C'PROGRAM NAME'                                        
         LA    R2,17(R2)                                                        
         MVC   0(6,R2),=C' PROG.'                                               
         MVC   132(6,R2),=C' CODE '                                             
         LA    R2,7(R2)                                                         
         MVC   0(3,R2),=C'DAY'                                                  
         LA    R2,7(R2)                                                         
         MVC   0(4,R2),=C'TIME'                                                 
         LA    R2,10(R2)                                                        
         MVC   0(3,R2),=C'SHR'                                                  
         LA    R2,5(R2)                                                         
         MVC   0(3,R2),=C'HUT'                                                  
         LA    R2,5(R2)                                                         
         MVC   0(3,R2),=C'RTG'                                                  
         LA    R2,4(R2)                                                         
         SPACE 1                                                                
         CLI   PLANNLEN,1                                                       
         BNE   HHDET1                                                           
         MVC   0(3,R2),=C'UNS'                                                  
         B     HHDET1B                                                          
         SPACE 1                                                                
HHDET1   ZIC   R1,PLANNLEN                                                      
         CLI   UNTOTS,C'Y'                                                      
         BE    *+6                                                              
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         BCTR  R1,0                                                             
         AR    R1,R2                                                            
         MVC   0(5,R1),=C'UNITS'                                                
         SPACE 1                                                                
HHDET1B  LA    R2,132(R2)                                                       
         LA    R3,PLANLENS                                                      
         ZIC   R4,PLANNLEN                                                      
         SPACE 1                                                                
HHDET2   EDIT  (1,0(R3)),(3,0(R2))                                              
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R2),C':'                                                       
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,HHDET2                                                        
         CLI   UNTOTS,C'N'                                                      
         BE    HHDET4                                                           
         MVC   0(3,R2),=C'TOT'                                                  
         LA    R2,4(R2)                                                         
         SPACE 1                                                                
HHDET4   SH    R2,=H'132'                                                       
         MVC   0(4,R2),=C'DATA'                                                 
         LA    R2,6(R2)                                                         
         MVC   0(5,R2),=C'HOMES'                                                
         LA    R2,7(R2)                                                         
         LA    R3,DEMOS                                                         
         ZIC   R4,NDEMOS                                                        
         GOTO1 VSETDB                                                           
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         SPACE 1                                                                
HHDET6   GOTO1 DEMOCON,DMCB,(0,0(R3)),(10,WORK),(C'S',DBLOCK)                   
*                                                                               
         CLI   0(R3),0                                                          
         BE    HHDET6B                                                          
         LA    RE,4                                                             
         LA    RF,WORK                                                          
HHDET6A  CLI   0(RF),C'.'                                                       
         BE    *+14                                                             
         LA    RF,1(RF)                                                         
         BCT   RE,HHDET6A                                                       
         DC    H'0'                 MUST HAVE NAD SETUP                         
         MVC   132(7,R2),0(RF)                                                  
         EDIT  (1,0(R3)),(3,2(R2)),FILL=0,ZERO=NOBLANK                          
         B     *+10                                                             
*                                                                               
*        CLI   WORK+2,C'.'         IF NAD DEMO                                  
*        BNE   HHDET6B                                                          
*        MVC   2(3,R2),WORK        PRINT ON 2 LINES                             
*        MVC   132(7,R2),WORK+3                                                 
*        B     *+10                                                             
HHDET6B  MVC   0(7,R2),WORK                                                     
         LA    R2,8(R2)                                                         
         LA    R3,3(R3)                                                         
         BCT   R4,HHDET6                                                        
         B     XIT                                                              
         EJECT                                                                  
*              HOOK HEADINGS FOR QUARTERLY SUMMARIES                            
         SPACE 3                                                                
HHQS     CLI   TYPE,C'Q'                                                        
         BNE   XIT                                                              
         LA    R2,HEAD10+2                                                      
         A     R2,DISP                                                          
         MVC   0(7,R2),=C'QUARTER'                                              
         LA    R2,10(R2)                                                        
         MVC   0(5,R2),=C'DEMO.'                                                
         LA    R2,11(R2)                                                        
         MVC   1(9,R2),=C'ALL UNITS'                                            
         BAS   RE,HHQS4                                                         
         LA    R2,15(R2)                                                        
         ZIC   R3,PLANNLEN                                                      
         LA    R4,PLANLENS                                                      
         SPACE 1                                                                
HHQS2    EDIT  (1,0(R4)),(3,1(R2))                                              
         CLI   1(R2),C' '                                                       
         BNE   *+8                                                              
         MVI   1(R2),C':'                                                       
         MVC   5(5,R2),=C'UNITS'                                                
         BAS   RE,HHQS4                                                         
         LA    R2,15(R2)                                                        
         LA    R4,1(R4)                                                         
         BCT   R3,HHQS2                                                         
         SPACE 1                                                                
         MVC   2(8,R2),=C'BASE :30'                                             
         EDIT  (1,BASELEN),(2,8(R2))                                            
         BAS   RE,HHQS4                                                         
         B     XIT                                                              
         SPACE 1                                                                
HHQS4    MVC   132(12,R2),=C'CPM.   (000)'                                      
         CLI   PUPQGRP,C'Y'                                                     
         BNER  RE                                                               
         MVC   132(12,R2),=C'CPP.   GRPS.'                                      
         BR    RE                                                               
         EJECT                                                                  
*              BOX RELATED ROUTINES                                             
         SPACE 3                                                                
HOOKBOX  NTR1                                                                   
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         CLI   BOXOPT,C'N'                                                      
         BE    XIT                                                              
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         SPACE 1                                                                
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         CLI   TYPE,C'G'                                                        
         BE    XIT                                                              
         CLI   TYPE,C'T'                                                        
         BE    XIT                                                              
         MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+11,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         SPACE 1                                                                
         LA    R2,BOXCOLS          BOXES FOR SUMMARY                            
         A     R2,DISP                                                          
         MVI   0(R2),C'L'                                                       
         CLI   TYPE,C'D'                                                        
         BE    HBDET                                                            
         CLI   TYPE,C'Q'                                                        
         BE    HBQS                                                             
         MVI   17(R2),C'C'                                                      
         LA    R2,24(R2)                                                        
         CLI   PLANPERT,C'M'                                                    
         BE    HBMON                                                            
         CLI   PLANPERT,C'W'                                                    
         BE    HBMON                                                            
         ZIC   R3,PLANNLEN                                                      
         SLL   R3,2                                                             
         LA    R4,5                                                             
         SPACE 1                                                                
HBQU2    MVI   0(R2),C'C'                                                       
         AR    R2,R3                                                            
         BCT   R4,HBQU2                                                         
         MVI   0(R2),C'R'                                                       
         B     XIT                                                              
         SPACE 1                                                                
HBMON    MVI   0(R2),C'C'                                                       
         LA    R2,7(R2)                                                         
         LA    R4,14                                                            
         SPACE 1                                                                
HBMON2   MVI   0(R2),C'C'                                                       
         LA    R2,6(R2)                                                         
         BCT   R4,HBMON2                                                        
         MVI   0(R2),C'R'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              BOXES FOR DETAIL REPORT                                          
         SPACE 1                                                                
HBDET    LA    R2,17(R2)                                                        
         MVI   0(R2),C'C'                                                       
         LA    R2,7(R2)                                                         
         MVI   0(R2),C'C'                                                       
         LA    R2,4(R2)                                                         
         MVI   0(R2),C'C'                                                       
         LA    R2,12(R2)                                                        
         MVI   0(R2),C'C'                                                       
         LA    R2,5(R2)                                                         
         MVI   0(R2),C'C'                                                       
         LA    R2,5(R2)                                                         
         MVI   0(R2),C'C'                                                       
         LA    R2,5(R2)                                                         
         MVI   0(R2),C'C'                                                       
         ZIC   R1,PLANNLEN                                                      
         CLI   UNTOTS,C'N'                                                      
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         SLL   R1,2                                                             
         AR    R2,R1                                                            
         MVI   0(R2),C'C'                                                       
         LA    R2,5(R2)                                                         
         ZIC   R1,NDEMOS                                                        
         LA    R1,1(R1)                                                         
         SPACE 1                                                                
HBDET2   MVI   0(R2),C'C'                                                       
         LA    R2,8(R2)                                                         
         BCT   R1,HBDET2                                                        
         MVI   0(R2),C'R'                                                       
         B     XIT                                                              
         EJECT                                                                  
         EJECT                                                                  
*              BOXES FOR QUARTERLY SUMMARY                                      
         SPACE 1                                                                
HBQS     LA    R2,10(R2)                                                        
         MVI   0(R2),C'C'                                                       
         LA    R2,11(R2)                                                        
         MVI   0(R2),C'C'                                                       
         LA    R2,15(R2)                                                        
         ZIC   R1,PLANNLEN                                                      
         LA    R1,1(R1)                                                         
         SPACE 1                                                                
HBQS2    MVI   0(R2),C'C'                                                       
         LA    R2,15(R2)                                                        
         BCT   R1,HBQS2                                                         
         MVI   0(R2),C'R'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         SPACE 3                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
BADQUART MVC   CONHEAD(L'MSGQUART),MSGQUART                                     
         B     MYEND                                                            
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
DRQLIST  DC    AL1(4,1,2,3)                                                     
QLIST    DC    AL1(4,1,4,1,2,1,2,2,2,3,2,3)                                     
MLIST    DC    AL1(9,1,4,10,1,4,11,1,4,12,1,4)                                  
         DC    AL1(1,2,1,2,2,1,3,2,1)                                           
         DC    AL1(4,2,2,5,2,2,6,2,2)                                           
         DC    AL1(7,2,3,8,2,3,9,2,3)                                           
         SPACE 1                                                                
MSGQUART DC    C'** ERROR ** QUARTER MUST BE 1-4'                               
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
         SPACE 1                                                                
QUALPHA  DC    C'FIRST '                                                        
         DC    C'SECOND'                                                        
         DC    C'THIRD '                                                        
         DC    C'FOURTH'                                                        
         SPACE 1                                                                
QUALPHA2 DC    C'4TH. 19'                                                       
         DC    C'1ST. 19'                                                       
         DC    C'2ND. 19'                                                       
         DC    C'3RD. 19'                                                       
         DC    C'YEAR 19'                                                       
         SPACE 1                                                                
HHQLIST  DC    C'4TH1ST2ND3RDTOT'                                               
HHMLIST  DC    C'SEPOCTNOVDECJANFEBMARAPRMAYJUNJULAUGSEP'                       
         SPACE 1                                                                
SAVEPROF DC    X'0000'                                                          
         EJECT                                                                  
*              SPECS FOR PHASE                                                  
         SPACE 3                                                                
HEDSPECS DS    0D                                                               
         SSPEC H1,2,C'NETWORK UPFRONT SYSTEM'                                   
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'NETWORK'                                                  
         SSPEC H4,55,C'DAYPART'                                                 
         SSPEC H5,55,C'PLAN      1988'                                          
         SPACE 1                                                                
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H5,115,PAGE                                                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         SPACE 1                                                                
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPD1D                                                       
         SPACE 1                                                                
TYPE     DS    CL1                                                              
LEFTOPT  DS    CL1                                                              
SPACOPT  DS    CL1                                                              
REQQRT   DS    XL1                                                              
QUARTER  DS    XL1                                                              
PERNUM   DS    XL1                                                              
HORIZ    DS    XL1                                                              
TOTLEVEL DS    CL1                                                              
UNTOTS   DS    CL1                                                              
THISQURT DS    CL1                                                              
BASELEN  DS    CL1                                                              
REQLEN   DS    XL1                                                              
DISP     DS    F                                                                
SIZEACC  DS    F                                                                
SIZELINE DS    F                                                                
SAVEBUDG DS    F                                                                
TITLE    DS    CL40                                                             
         DS    0F                                                               
TOTALS   DS    CL24                                                             
PROGACCS DS    CL272                                                            
PLANACCS DS    CL272                                                            
TARGACCS DS    CL272                                                            
QBUFF    DS    1000C                                                            
         SPACE 1                                                                
*              DSECT TO COVER TOTALS LINE                                       
         SPACE 1                                                                
ACCUMD   DSECT                                                                  
ACBUDGET DS    F                                                                
ACUNITS  DS    F                                                                
ACHOMGRP DS    F                                                                
ACHOMIMP DS    F                                                                
ACTARGRP DS    F                                                                
ACTARIMP DS    F                                                                
         SPACE 3                                                                
*              DSECT TO COVER DETAILS LINE                                      
         SPACE 1                                                                
DETD     DSECT                                                                  
DETSHR   DS    F                                                                
DETHUT   DS    F                                                                
DETRTG   DS    F                                                                
DETTUNIT DS    F                   TOTAL UNITS                                  
DETLUNIT DS    4F                  UNITS BY SECOND LENGTH                       
DETGRPS  DS    F                                                                
DETHOMES DS    F                                                                
DETTGRPS DS    F                   UP TO 6 TARGETS                              
DETTIMPS DS    F                                                                
         DS    10F                                                              
       ++INCLUDE SPGENPROGA                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037NEPUP21S  05/01/02'                                      
         END                                                                    
