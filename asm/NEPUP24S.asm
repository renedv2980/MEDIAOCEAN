*          DATA SET NEPUP24S   AT LEVEL 052 AS OF 05/01/02                      
*PHASE T32224A                                                                  
         TITLE 'T32224 - PRB2 REPORT'                                           
T32224   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32224**,RA,R6                                                 
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
         LA    RE,WORKD                                                         
         LA    RF,WORKEND-WORKD                                                 
         XCEF                                                                   
         SPACE 1                                                                
         LA    R2,PUPCLIH          CLIENT                                       
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
         OI    ALLOKS,X'04'                                                     
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
         MVC   8(7,R2),DPTNAME                                                  
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         OI    6(R2),X'80'                                                      
         OC    PLANCODE,PLANCODE                                                
         BZ    *+8                                                              
         MVI   NETYES,C'Y'                                                      
         SPACE 1                                                                
VREC2    LA    R2,PUPDEMOH         DEMO OVERRIDES                               
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         MVI   MAX,6                                                            
         GOTO1 VVALDEM                                                          
         MVC   DEMNMSV,NDEMOS      YES/SAVE 1ST PLAN'S DEMOS                    
         MVC   DEMOSV,DEMOS                                                     
         MVC   TRGNMSV,TARGNAME                                                 
         SPACE 1                                                                
VREC4    LA    R2,PUPOPTH          OPTIONS                                      
         BAS   RE,EDITOPT                                                       
         SPACE 1                                                                
         LA    R2,PUPQSTLH                                                      
         MVI   QTITLE,0                                                         
         CLI   5(R2),0                                                          
         BE    VRECX                                                            
         MVC   TITLE,PUPQSTL                                                    
         MVI   QTITLE,1                                                         
         SPACE 1                                                                
VRECX    B     XIT                                                              
         EJECT                                                                  
*              EDIT OPTIONS                                                     
         SPACE 3                                                                
EDITOPT  NTR1                                                                   
         MVI   BOXOPT,C'Y'                                                      
         MVI   FIELDERR,1                                                       
         MVI   REQLEN,0                                                         
         MVI   ROUND,C'Y'                                                       
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
OPT4     CLC   12(3,R4),=C'LEN '   LENGTH FILTERING OPTION                      
         BNE   OPT6                                                             
         MVC   REQLEN,11(R4)                                                    
         CLI   REQLEN,1                                                         
         BL    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(5,R4),=C'ROUND'  ROUNDING CPP                                 
         BNE   OPT10                                                            
         MVC   ROUND,22(R4)                                                     
         CLI   ROUND,C'Y'                                                       
         BE    OPTEND                                                           
         CLI   ROUND,C'N'                                                       
         BE    OPTEND                                                           
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
         LA    RE,QUARTER4         CLEAR TOTAL AREAS                            
         LA    RF,WORKEND-QUARTER4                                              
         XCEF                                                                   
         LA    R1,HEDSPECS         INITIALIZE                                   
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         BAS   RE,CENTERP          CENTER PRINT LINE                            
*                                                                               
         GOTO1 PUPIO,DMCB,PUPHOOK                                               
         CLI   PUPMODE,PUPIOEND    ERROR-EXIT                                   
         BE    XIT                                                              
         B     PLANEND                                                          
         SPACE 1                                                                
PUPHOOK  NTR1                                                                   
         CLI   PUPMODE,PLANFRST    PLAN REC                                     
         BNE   PH3                                                              
         MVC   PLANYRSV,PLANYEAR                                                
         BAS   RE,POSTBUDG                                                      
         LA    R5,75               25 = MAX PLANS                               
         LA    R1,PLANSAVE         SAVE NETWORK/PLAN NAME                       
PH2      CLI   0(R1),0                                                          
         BNE   PH2A                                                             
         L     R2,AIO1                                                          
         USING NPLRECD,R2                                                       
         MVC   0(4,R1),NPLKNET                                                  
         MVC   4(16,R1),PLANNAME                                                
         B     PH2B                                                             
PH2A     LA    R1,20(R1)                                                        
         BCT   R5,PH2                                                           
*                                                                               
         L     R2,AP               EXIT-TOO MANY PLANS                          
         MVC   0(35,R2),=C'*** ERROR TOO MANY PLANS 75=MAX ***'                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   PUPMODE,PUPIOEND                                                 
         B     XIT                                                              
*                                                                               
PH2B     CLI   DEMNMSV,0           IS IT FIRST PLAN                             
         BE    PH2C                                                             
         MVC   NDEMOS,DEMNMSV      NO/SO SET DEMOS FROM 1ST PLAN                
         MVC   DEMOS,DEMOSV                                                     
         MVC   TARGNAME,TRGNMSV                                                 
         BAS   RE,DEMONM           PUTS DEMO NAMES INTO DEMNAMES                
         B     PUPX                                                             
PH2C     MVC   DEMNMSV,NDEMOS      YES/SAVE 1ST PLAN'S DEMOS                    
         MVC   DEMOSV,DEMOS                                                     
         MVC   TRGNMSV,TARGNAME                                                 
         BAS   RE,DEMONM           PUTS DEMO NAMES INTO DEMNAMES                
         B     PUPX                                                             
*                                                                               
PH3      CLI   PUPMODE,PROGMD      PROGRAM REC                                  
         BNE   PUPX                                                             
         BAS   RE,POSTPROG                                                      
PUPX     B     XIT                                                              
         SPACE 1                                                                
PLANEND  BAS   RE,ROLLTOTS         ROLL DATA INTO TOTAL COLUMNS                 
         BAS   RE,PRINTIT          FLUSH DATA AREAS/PRINT                       
         CLI   NETYES,C'Y'         IF ONE PLAN/SKIP                             
         BE    XIT                                                              
         LA    R3,PLANSAVE                                                      
PND1     L     R2,AP                                                            
         LA    R4,3                                                             
PND2     MVC   0(4,R2),0(R3)                                                    
         MVC   5(16,R2),4(R3)                                                   
         LA    R3,20(R3)                                                        
         CLI   0(R3),0                                                          
         BE    PND3                                                             
         LA    R2,132(R2)                                                       
         BCT   R4,PND2                                                          
PND3     BAS   RE,WRITIT                                                        
         CLI   0(R3),0                                                          
         BNE   PND1                                                             
PNDX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROLL DATA IN QUARTER AREAS TO ROW END TOTALS IN EACH ROW                      
*                                                                               
ROLLTOTS NTR1                                                                   
         LA    R3,QUARTER4                                                      
         LA    R5,8                NUMBER OF ROWS                               
ROL1     LA    R4,3                                                             
         L     R1,0(R3)            UNITS(OR GRPS)                               
         L     R2,4(R3)            COST(OR IMPS)                                
ROL3     A     R1,8(R3)                                                         
         A     R2,12(R3)                                                        
         LA    R3,8(R3)                                                         
         BCT   R4,ROL3                                                          
         LA    R3,8(R3)            BUMP TO TOTS AREA                            
         ST    R1,0(R3)            AND STORE                                    
         ST    R2,4(R3)                                                         
         LA    R3,8(R3)            BUMP TO START OF NEXT ROW                    
         BCT   R5,ROL1                                                          
ROLX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST BUDGETS FROM PLAN                                
         SPACE 3                                                                
POSTBUDG NTR1                                                                   
         GOTO1 VEXTBUDG                                                         
         LA    R3,QUARTER4                                                      
         USING QSAVED,R3                                                        
         LA    R2,BUDGETS                                                       
         LA    R0,4                                                             
         SPACE 1                                                                
PBUDG2   L     R1,0(R2)            ADD BUDGETS INTO QUARTER SAVE AREA           
         A     R1,QCOST                                                         
         ST    R1,QCOST                                                         
         LA    R3,8(R3)           BUMP QUARTER BUDGET SAVE AREA                 
         LA    R2,20(R2)           BUMP TOP NEXT BUDGET                         
         BCT   R0,PBUDG2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST PROGRAM DETAILS                                  
         SPACE 3                                                                
POSTPROG NTR1                                                                   
         BAS   RE,SETEQIV         SET EQUIVALENCED                              
         LA    R3,QUARTER4                                                      
         USING QSAVED,R3                                                        
         LA    R2,PLANPLST                                                      
         ZIC   R0,PLANNPER         NUMBER OF PERIODS                            
         SPACE 1                                                                
PPR2     MVC   THISQURT,2(R2)      NOTE QUARTER NUMBER                          
         CLI   2(R2),0             DISPLACE TO QUARTER SAVE AREA                
         BE    PPR3                                                             
         ZIC   R1,2(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R1,*-4                                                           
*                                                                               
PPR3     ST    R3,AQUARTER                                                      
         MVC   PERIOD,0(R2)                                                     
         BAS   RE,POSTIT                                                        
*                                                                               
         LA    R2,4(R2)                                                         
         LA    R3,QUARTER4       RESET TO START OF QUARTER SAVE AREA            
         BCT   R0,PPR2                                                          
         XIT                                                                    
*                                                                               
         EJECT                                                                  
*                                                                               
*                                  R3 POINTS TO PROPER QUARTER                  
POSTIT   NTR1                                                                   
         MVC   LENGTH,REQLEN       GET UNITS                                    
         GOTO1 VEXTUNS                                                          
         ZIC   R1,UNITS                                                         
         CLI   N0PROF+1,0                                                       
         BE    PST3                                                             
         LH    R1,UNTEQV                                                        
PST3     A     R1,QUNITS                                                        
         ST    R1,QUNITS                                                        
*                                  POST DEMO DATA                               
         MVI   GDDEMO,1            GO FOR HOMES FIRST                           
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VGETDEM                                                          
         L     R1,GDEGRP           HOMES GRPS                                   
         A     R1,QGRPS                                                         
         ST    R1,QGRPS                                                         
         L     R1,GDEIMP           HOMES (000)                                  
         A     R1,QIMPS                                                         
         ST    R1,QIMPS                                                         
*                                                                               
         LA    R2,DEMOS            DO OTHER DEMOS                               
         ZIC   R5,DEMNMSV                                                       
         MVC   LENGTH,REQLEN                                                    
PST5     LA    R3,40(R3)                                                        
         MVC   GDDEMO,2(R2)                                                     
         GOTO1 VGETDEM                                                          
         L     R1,GDEGRP           GRPS                                         
         A     R1,QGRPS                                                         
         ST    R1,QGRPS                                                         
         L     R1,GDEIMP           IMPS                                         
         A     R1,QIMPS                                                         
         ST    R1,QIMPS                                                         
         LA    R2,3(R2)            BUMP DEMO                                    
         BCT   R5,PST5                                                          
PSTX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*    FLUSH DATA SAVE AREAS                                                      
*                                                                               
PRINTIT  NTR1                                                                   
         USING PLINED,R2                                                        
         USING QSAVED,R3                                                        
         LA    R3,QUARTER4                                                      
         ST    R3,AQUARTER                                                      
*                              DO FIRST THREE PRINT LINES OF FIRST ROW          
*                                           ( OF 5 PRINT LINES IN ALL)          
         L     R2,AP                                                            
         MVC   PTITLES(19),=C'NUMBER OF UNITS   :'                              
         MVC   PTITLES+132(19),=C'TOTAL COST        :'                          
         MVC   PTITLES+264(19),=C'AVG COST PER UNIT :'                          
         LA    R2,PDATA                                                         
         LA    R5,5                                                             
PRT2     OC    QUNITS,QUNITS                                                    
         BZ    PRT3                                                             
         EDIT  (B4,QUNITS),(12,0(R2))                                           
         CLI   N0PROF+1,0                                                       
         BE    PRT3                                                             
         EDIT  (B4,QUNITS),(12,0(R2)),1                                         
PRT3     OC    QCOST,QCOST                                                      
         BZ    PRT5                                                             
         EDIT  (B4,QCOST),(12,132(R2)),COMMAS=YES,FLOAT=$                       
         L     R0,QCOST                                                         
         MVC   FULL,QUNITS                                                      
         CLI   N0PROF+1,0                                                       
         BE    PRT4B                                                            
         MH    R0,=H'10'                                                        
PRT4B    BAS   RE,DIVROUND               RETURNS ROUNDED ANSWER IN FULL         
         EDIT  (B4,FULL),(12,264(R2)),COMMAS=YES,FLOAT=$                        
*                                                                               
PRT5     LA    R3,8(R3)                                                         
         LA    R2,14(R2)                                                        
         BCT   R5,PRT2                                                          
         BAS   RE,WRITIT           WRITES 1ST THREE LINES                       
         SPACE 1                                                                
*                             NOW NEXT TWO PRINT LINES OF FIRST ROW             
*                                 (HH RATING AND IMPS AVERAGES)                 
         L     R3,AQUARTER                                                      
         L     R2,AP                                                            
         LA    R5,5                                                             
         MVC   PTITLES(19),=C'AVG HH RATING     :'                              
         MVC   PTITLES+132(19),=C'AVG HH IMP (000)  :'                          
         LA    R2,PDATA                                                         
PRT9     L     R0,QGRPS                                                         
         LTR   R0,R0                                                            
         BZ    PRT9B                                                            
         MVC   FULL,0(R3)               SET UNITS IN FULL                       
         CLI   N0PROF+1,0                                                       
         BE    PRT9A                                                            
         MH    R0,=H'10'                                                        
PRT9A    BAS   RE,DIVROUND              RETURNS ANSWER IN FULL                  
         EDIT  (B4,FULL),(12,0(R2)),1                                           
PRT9B    LA    R4,132(R2)                                                       
         L     R0,QIMPS                                                         
         LTR   R0,R0                                                            
         BZ    PRT9C                                                            
         CLI   N0PROF+1,0                                                       
         BE    PRT9BB                                                           
         MH    R0,=H'10'                                                        
         MVC   FULL,0(R3)                                                       
PRT9BB   BAS   RE,DIVROUND                                                      
         EDIT  (B4,FULL),(12,0(R4)),COMMAS=YES                                  
PRT9C    LA    R3,8(R3)                                                         
         LA    R2,14(R2)                                                        
         BCT   R5,PRT9                                                          
         MVI   SPACING,2                                                        
         BAS   RE,WRITIT           WRITES LAST 2 LINES OF 1ST FIVE              
*                                                                               
         EJECT                                                                  
*                                  FLUSH OUT DEMOS                              
*                                                                               
         LA    R3,QUARTER4                                                      
         ST    R3,TEMPADR          R3 IN TEMPORARY ADR SAVE                     
         ST    R3,AQUARTER                                                      
         LA    R1,4(R3)            SAVE FIST COST ADR                           
         ST    R1,ATMPCST                                                       
         MVC   TEMPCOST,0(R1)      STORE COST                                   
PRT12    L     R2,AP                                                            
         ZIC   R1,DEMONUM          IF DEMONUM=0, THEN HOMES                     
         LTR   R1,R1                                                            
         BZ    PRT14                                                            
         BCTR  R1,0                                                             
         LA    R4,DEMNAMES                                                      
         MH    R1,=H'7'                                                         
         AR    R4,R1                                                            
         MVC   0(7,R2),0(R4)                                                    
         B     *+10                                                             
PRT14    MVC   0(5,R2),=C'HOMES'                                                
         BAS   RE,WRITIT                                                        
         L     R2,AP                                                            
         MVC   0(19,R2),=C'COST PER POINT    :'                                 
         MVC   132(19,R2),=C'TOTAL GRPS        :'                               
         LA    R4,264(R2)                                                       
         MVC   0(19,R4),=C'GROSS IMPS (000)  :'                                 
         MVC   132(19,R4),=C'CPM               :'                               
         LA    R5,5                                                             
         LA    R2,PDATA                              OUTPUT AREA                
PRT16    ST    R2,PLINESV                                                       
         LA    R4,132(R2)                                                       
         EDIT  (B4,QGRPS),(12,0(R4)),1               GRPS                       
         LA    R4,132(R4)                                                       
         EDIT  (B4,QIMPS),(12,0(R4)),COMMAS=YES      IMPS                       
         LA    R4,QGRPS            GRPS                                         
         L     R3,TEMPCOST         COST                                         
         BAS   RE,CPP                                CPP                        
         AH    R2,=H'396'                                                       
         L     R3,TEMPADR          RETURN R3 TO START                           
         LA    R4,QIMPS                                                         
         L     R3,TEMPCOST                                                      
         BAS   RE,CPM                                CPM                        
*                                                                               
         L     R3,TEMPADR          RETURN R3 TO START                           
         LA    R3,8(R3)            BUMP DATA ROW TO NEXT QUARTER                
         ST    R3,TEMPADR                                                       
         L     R2,PLINESV          RESET PRINT LINE                             
         LA    R2,14(R2)           AND BUMP TO NEXT POSITION                    
         L     R1,ATMPCST          GET QUARTER COST ADR                         
         LA    R1,8(R1)            AND BUMPT TO NEXT ONE                        
         MVC   TEMPCOST,0(R1)                                                   
         ST    R1,ATMPCST                                                       
         BCT   R5,PRT16                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,WRITIT                                                        
         AI    DEMONUM,1                                                        
         CLC   DEMNMSV,DEMONUM      HAVE WE FINISHED DEMOS                      
         BL    PRTX                                                             
         L     R3,AQUARTER         BUMP R3 TO NEXT ROW                          
         LA    R3,40(R3)                                                        
         ST    R3,TEMPADR                                                       
         ST    R3,AQUARTER                                                      
         LA    R1,QUARTER4+4       RESET TO START OF COSTS                      
         ST    R1,ATMPCST                                                       
         MVC   TEMPCOST,0(R1)                                                   
         B     PRT12                                                            
PRTX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  PUTS DEMO NAMES(CL7) INTO DEMNAMES                                           
*                                                                               
DEMONM   NTR1                                                                   
         LA    R4,DEMNAMES                                                      
         LA    R3,DEMOSV                                                        
         ZIC   R5,DEMNMSV                                                       
         GOTO1 VSETDB                                                           
DEM4     GOTO1 DEMOCON,DMCB,(0,0(R3)),(7,WORK),(C'S',DBLOCK)                    
         MVC   0(7,R4),WORK                                                     
         LA    R3,3(R3)                                                         
         LA    R4,7(R4)                                                         
         BCT   R5,DEM4                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DIVIDES / ROUNDS AND RETURNS ROUNDED ANSWER IN FULL                           
*                                                                               
*                      R0 = A(DIVIDEND)                                         
*                    FULL = DIVISOR                                             
*                                                                               
DIVROUND NTR1                                                                   
         OC    FULL,FULL                                                        
         BZ    XIT                                                              
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,FULL                                                          
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,FULL                                                          
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
         EDIT  (R1),(12,0(R2)),2,FLOAT=$                                        
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
         EDIT  (R1),(12,0(R2)),FLOAT=$                                          
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
         CLI   ROUND,C'Y'                                                       
         BE    CPPRND                                                           
         SPACE 1                                                                
*                                  COMPUTE CPP IN PENNIES                       
         LR    R1,R3                                                            
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'2000'                                                      
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(12,0(R2)),2,FLOAT=$                                        
         B     XIT                                                              
         SPACE 1                                                                
CPPRND   LR    R1,R3               COMPUTE CPP IN DOLLARS                       
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'20'                                                        
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(12,0(R2)),FLOAT=$,COMMAS=YES                               
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVI   HEAD2+1,C'_'     UNDERLINE SYSTEM NAME                           
         MVC   HEAD2+2(21),HEAD2+1                                              
         CLI   QTITLE,0                                                         
         BE    HOOK1                                                            
         MVC   WORK(40),TITLE                                                   
         B     *+10                                                             
HOOK1    MVC   WORK(40),=CL40'ANNUAL SUMMARY BY QUARTER'                        
         SPACE 1                                                                
HOOK2    GOTO1 CENTER,DMCB,WORK,40                                              
         MVC   HEAD1+41(40),WORK                                                
         GOTO1 UNDERLIN,DMCB,(40,HEAD1+40),(HORIZ,HEAD2+40)                     
         MVC   WORK(40),=CL40'(30 SEC EQUIVALENCY)'                             
         GOTO1 CENTER,DMCB,WORK,40                                              
         MVC   HEAD3+41(40),WORK                                                
         SPACE 1                                                                
         MVC   HEAD4+9(3),PUPCLI                                                
         MVC   HEAD4+15(20),PUPCLIN                                             
         CLI   NETYES,C'Y'                                                      
         BNE   HOOK2B                                                           
         MVC   H5+1(7),=C'NETWORK'                                              
         MVC   H5+53(15),=C'PLAN       1988'                                    
         MVC   HEAD5+9(4),PUPNET                                                
         MVC   HEAD5+58(5),PUPPLAN                                              
         LA    R2,HEAD5+66                                                      
         EDIT  (1,PLANYRSV),(2,0(R2))                                           
HOOK2B   MVC   HEAD4+62(7),PUPDPT                                               
*                                                                               
         LA    R2,HEAD8                                                         
         A     R2,DISP                                                          
         USING PLINED,R2                                                        
         LA    R2,PDATA                                                         
         LA    R3,QUALPHA                                                       
         LA    R5,4                                                             
         ZIC   R4,PLANYRSV                                                      
         BCTR  R4,0                                                             
HOOK4    MVC   0(10,R2),0(R3)                                                   
         EDIT  (R4),(2,10(R2))                                                  
         LA    R2,14(R2)                                                        
         ZIC   R4,PLANYRSV                                                      
         LA    R3,10(R3)                                                        
         BCT   R5,HOOK4                                                         
         MVC   0(12,R2),=C'ANNUAL TOTAL'                                        
*                                                                               
         CLI   BOXOPT,C'N'                                                      
         BE    HOOKX                                                            
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    HOOKX                                                            
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,SPACES                                                   
         LA    R3,BOXROWS                                                       
         LA    R3,6(R3)                                                         
         MVI   0(R3),C'T'                                                       
         LA    R3,2(R3)                                                         
         MVI   0(R3),C'M'                                                       
         LA    R3,65(R3)                                                        
         MVI   0(R3),C'B'                                                       
         MVC   BOXCOLS,SPACES                                                   
         LA    R3,BOXCOLS-1                                                     
         A     R3,DISP                                                          
         MVI   0(R3),C'L'                                                       
         LA    R3,1(R3)                                                         
         LA    R3,34(R3)                                                        
         MVI   0(R3),C'C'                                                       
         LA    R5,4                                                             
         LA    R3,14(R3)                                                        
         MVI   0(R3),C'C'                                                       
         BCT   R5,*-8                                                           
         MVI   0(R3),C'R'                                                       
HOOKX    B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 1                                                                
*                                                                               
*              CENTER PRINT LINE                                                
CENTERP  LA    R1,4                                                             
         MH    R1,=H'14'           EACH LENGTH IS CL14                          
         LA    R1,48(R1)           PLUS CL48                                    
         LA    R2,132                                                           
         SR    R2,R1                                                            
         SRA   R2,1                                                             
         ST    R2,DISP                                                          
         LA    R1,P                                                             
         AR    R1,R2                                                            
         ST    R1,AP                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
SETEQIV  MVC   N2SAVE,N2PROF       FORCE PROFILE TO EQUIVALENCE                 
         MVC   N0SAVE,N0PROF+1                                                  
         CLI   N2PROF,0                                                         
         BNE   *+8                                                              
         MVI   N2PROF,30                                                        
         CLI   N0PROF+1,0                                                       
         BNE   *+8                                                              
         MVI   N0PROF,30                                                        
         MVI   GDRAWOPT,0                                                       
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
SETRAW   MVC   N2SAVE,N2PROF       FORCE PROFILE TO RAW                         
         MVC   N0SAVE,N0PROF+1                                                  
         MVI   N2PROF,0                                                         
         MVI   N0PROF+1,0                                                       
         MVI   GDRAWOPT,C'Y'                                                    
         BR    RE                                                               
         SPACE 3                                                                
WRITIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
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
         SPACE 1                                                                
MSGQUART DC    C'** ERROR ** QUARTER MUST BE 1-4'                               
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
         SPACE 1                                                                
QUALPHA  DC    C'4TH QTR 19'                                                    
         DC    C'1ST QTR 19'                                                    
         DC    C'2ND QTR 19'                                                    
         DC    C'3RD QTR 19'                                                    
         SPACE 1                                                                
         SPACE 1                                                                
SAVEPROF DC    X'0000'                                                          
         EJECT                                                                  
*              SPECS FOR PHASE                                                  
         SPACE 3                                                                
HEDSPECS DS    0D                                                               
         SSPEC H1,2,C'NETWORK UPFRONT SYSTEM'                                   
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H4,55,C'DAYPART'                                                 
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
       ++INCLUDE NEPUPALL                                                       
         EJECT                                                                  
       ++INCLUDE NEPUPD4D                                                       
         SPACE 1                                                                
WORKD    DS    0CL1                WORK AREA                                    
TYPE     DS    CL1                                                              
REQLEN   DS    CL1                                                              
SPACOPT  DS    CL1                                                              
PERNUM   DS    XL1                                                              
HORIZ    DS    XL1                                                              
TOTLEVEL DS    CL1                                                              
UNTOTS   DS    CL1                                                              
THISQURT DS    CL1                                                              
DEMNMSV  DS    CL1                                                              
DEMOSV   DS    CL18                                                             
TRGNMSV  DS    CL7                                                              
DEMONUM  DS    CL1                                                              
DEMCOUNT DS    CL1                                                              
DEMNAMES DS    CL42                                                             
N2SAVE   DS    CL1                                                              
N0SAVE   DS    CL1                                                              
ROUND    DS    CL1                                                              
NETYES   DS    CL1                                                              
PLANYRSV DS    CL1                                                              
DISP     DS    F                                                                
AQUARTER DS    F                                                                
AROW     DS    F                                                                
TEMPCOST DS    F                                                                
TEMPADR  DS    F                                                                
PLINESV  DS    F                                                                
AP       DS    F                                                                
TOTSV    DS    F                                                                
ATMPCST  DS    F                                                                
TITLE    DS    CL40                                                             
QTITLE   DS    CL1                                                              
PLANSAVE DS    CL1500               FOR 75 PLANS OF 20CL EACH                   
         DS    0F                                                               
QUARTER4 DS    10F               5 X (UNIT/COST) (QUARTER TOTS)                 
         DS    70F               5 X (GRPS/IMPS) X 7 DEMO CATEGORIES            
WORKEND  EQU   *                                                                
         SPACE 1                                                                
QSAVED   DSECT                  DSECT FOR TOTAL ACCUM AREAS                     
QUNITS   DS    F                                                                
QCOST    DS    F                                                                
         DS    8F                                                               
QGRPS    DS    F                                                                
QIMPS    DS    F                                                                
*                                                                               
PLINED   DSECT                                                                  
PTITLES  DS    CL20                                                             
         DS    CL2                                                              
PDATA    DS    CL12                UP TO 6 DATA COLUMNS                         
         DS    CL2                                                              
*                                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052NEPUP24S  05/01/02'                                      
         END                                                                    
