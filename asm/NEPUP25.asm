*          DATA SET NEPUP25    AT LEVEL 008 AS OF 05/01/02                      
*          DATA SET NEPUP25    AT LEVEL 092 AS OF 04/25/90                      
*PHASE T32225A,*                                                                
         TITLE 'T32225 - PRB3 REPORT'                                           
T32225   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32225**,RA,R6                                                 
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
         LA    RE,PUPWORK          CLEAR MY WORK AREA                           
         LA    RF,PWRKLENE                                                      
         XCEF                                                                   
         SPACE 1                                                                
         LA    R2,PUPCLIH          CLIENT                                       
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
****     OI    ALLOKS,X'04'                                                     
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
***      MVC   8(7,R2),DPTNAME                                                  
***      OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         OI    6(R2),X'80'                                                      
         OC    PLANCODE,PLANCODE                                                
         BZ    *+8                                                              
         MVI   NTPLNFLG,C'Y'                                                    
         SPACE 1                                                                
VREC2    LA    R2,PUPDEMOH         DEMO OVERRIDES                               
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         MVI   MAX,6                                                            
         GOTO1 VVALDEM                                                          
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
OPT6     CLC   12(5,R4),=C'ROUND'                                               
         BNE   OPT10                                                            
         MVC   ROUND,22(R4)                                                     
         CLI   ROUND,C'Y'                                                       
         BE    OPTEND                                                           
         CLI   ROUND,C'N'                                                       
         BE    OPTEND                                                           
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
*                                                                               
         MVI   GDRNDOPT,C'Y'                                                    
*                                                                               
         LA    R1,HEDSPECS         INITIALIZE                                   
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 PUPIO,DMCB,PUPHOOK      READ THE RECORDS                         
         B     PLANEND                                                          
         SPACE 1                                                                
PUPHOOK  NTR1                                                                   
         CLI   PUPMODE,PLANFRST    PLAN FIRST                                   
         BNE   PHK5                                                             
         CLI   PLANNSV,0                                                        
         BNE   PHK2                                                             
         MVC   PLANNSV,PLANNLEN                                                 
         MVC   PLANLSV,PLANLENS                                                 
PHK2     CLC   PLANNSV,PLANNLEN                                                 
         BNE   BADLENS             ERROR-SPOT LENS NOT THE SAME                 
         CLC   PLANLENS,PLANLSV                                                 
         BNE   BADLENS             ERROR-SPOT LENS NOT INSAME ORDER             
*                                                                               
         BAS   RE,POSTBUDG         POST BUDGET                                  
         LA    R4,NETNAMSV         SAVE NET/PLANNAME                            
         LA    R5,20               20=MAX PLANS                                 
         CLI   0(R4),0                                                          
         BE    PHK3                                                             
         LA    R4,20(R4)                                                        
         BCT   R5,*-12                                                          
         DC    H'0'                CRASH-TOO MANY PLANS                         
PHK3     L     R1,AIO1                                                          
         USING NPLRECD,R1                                                       
         MVC   0(4,R4),NPLKNET                                                  
         MVC   4(16,R4),PLANNAME                                                
*                                                                               
         CLI   DEMNMSV,0           IS IT FIRST PLAN                             
         BE    PHK3B                                                            
         MVC   NDEMOS,DEMNMSV      NO/SO SET DEMOS FROM 1ST PLAN                
         MVC   DEMOS,DEMOSV                                                     
         MVC   TARGNAME,TRGNMSV                                                 
         B     PHKX                                                             
PHK3B    MVC   DEMNMSV,NDEMOS      YES/SAVE 1ST PLAN'S DEMOS                    
         MVC   DEMOSV,DEMOS                                                     
         MVC   TRGNMSV,TARGNAME                                                 
         B     PHKX                                                             
         DROP  R1                                                               
PHK5     CLI   PUPMODE,PROGMD      PROGRAM REC                                  
         BNE   PHKX                                                             
         BAS   RE,POSTPROG                                                      
PHKX     B     XIT                                                              
         SPACE 1                                                                
PLANEND  DS    0H                                                               
         BAS   RE,CENTERP          CENTER PRINT LINE                            
         BAS   RE,ROLLTOTS         ROLL DATA INTO TOTAL COLUMNS                 
         BAS   RE,PRINTIT          FLUSH DATA AREAS/PRINT                       
         CLI   NTPLNFLG,C'Y'                                                    
         BE    PLNDX                                                            
         LA    R5,20                                                            
         LA    R4,NETNAMSV                                                      
PLND3    L     R2,AP                                                            
         MVC   0(4,R2),0(R4)                                                    
         MVC   5(16,R2),4(R4)                                                   
         MVI   FORCEHED,0                                                       
         BAS   RE,WRITIT                                                        
         LA    R4,20(R4)                                                        
         CLI   0(R4),0                                                          
         BE    PLNDX                                                            
         BCT   R5,PLND3                                                         
PLNDX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROLL DATA IN QUARTER AREAS TO ROW END TOTALS IN EACH ROW                      
* ROLL QUARTER AREAS INTO ALLTOTS (ALL QUARTERS TOTAL AREA)                     
*                                                                               
ROLLTOTS NTR1                                                                   
         LA    R3,QUARTER4                                                      
         ST    R3,AQUARTER                                                      
         ST    R3,AROW                                                          
         MVI   THISQURT,0                                                       
ROL1     LA    R5,8                FOR 8 DATA ROWS                              
ROL2     L     R1,8(R3)            UNITS(OR GRPS)                               
         L     R2,12(R3)           COST(OR IMPS)                                
         CLI   PLANNSV,1                                                        
         BE    ROL3B                                                            
         ZIC   R4,PLANNSV                                                       
         BCTR  R4,0                                                             
ROL3     LA    R3,8(R3)                                                         
         A     R1,8(R3)                                                         
         A     R2,12(R3)                                                        
         BCT   R4,ROL3                                                          
ROL3B    LA    R3,8(R3)            BUMP TO TOTS AREA                            
         ST    R1,8(R3)            AND STORE                                    
         ST    R2,12(R3)                                                        
         L     R3,AROW         BUMP TO START OF NEW ROW                         
         LA    R3,48(R3)                                                        
         ST    R3,AROW                                                          
         BCT   R5,ROL2                                                          
         AI    THISQURT,1                                                       
         CLI   THISQURT,4                                                       
         BE    ROL7                                                             
         L     R3,AQUARTER                                                      
         LA    R3,QURTLENE(R3)                                                  
         ST    R3,AQUARTER                                                      
         B     ROL1                                                             
*                             NOW ROLL ALL QUARTERS INTO ALLTOTS                
ROL7     LA    R3,QUARTER4                                                      
         LA    R5,4                4 QUARTERS                                   
ROL9     LA    R2,ALLTOTS                                                       
         LA    R4,96               96 DATA FULL WORDS                           
ROL10    L     R1,0(R3)                                                         
         A     R1,0(R2)                                                         
         ST    R1,0(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R2,4(R2)                                                         
         BCT   R4,ROL10                                                         
         BCT   R5,ROL9                                                          
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
         LA    R3,QURTLENE(R3)     BUMP QUARTER BUDGET SAVE AREA                
         LA    R2,20(R2)           BUMP TOP NEXT BUDGET                         
         BCT   R0,PBUDG2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST PROGRAM DETAILS                                  
         SPACE 3                                                                
POSTPROG NTR1                                                                   
         LA    R3,QUARTER4                                                      
         USING QSAVED,R3                                                        
         LA    R2,PLANPLST                                                      
         ZIC   R0,PLANNPER         NUMBER OF PERIODS                            
         SPACE 1                                                                
PPR2     MVC   THISQURT,2(R2)      NOTE QUARTER NUMBER                          
         CLI   2(R2),0             DISPLACE TO QUARTER SAVE AREA                
         BE    PPR3                                                             
         ZIC   R1,2(R2)                                                         
         LA    R3,QURTLENE(R3)                                                  
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
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         CLI   N0PROF+1,0                                                       
         BE    PPRD2                                                            
         LH    R1,UNTEQV                                                        
PPRD2    A     R1,QUNITS                                                        
         ST    R1,QUNITS                                                        
         LA    R3,8(R3)                                                         
         LA    R2,PLANLSV          GET N'UNITS FOR PERIOD/LENGTH                
         ZIC   R0,PLANNSV                                                       
PPR4D    MVC   LENGTH,0(R2)                                                     
         CLI   REQLEN,0                                                         
         BE    PPR5                                                             
         CLC   LENGTH,REQLEN                                                    
         BNE   PPR5B                                                            
PPR5     GOTO1 VEXTUNS                                                          
         ZIC   R1,UNITS                                                         
         A     R1,QUNITS                                                        
         ST    R1,QUNITS                                                        
         LA    R3,8(R3)                                                         
PPR5B    LA    R2,1(R2)                                                         
         BCT   R0,PPR4D                                                         
*                                  POST DEMO DATA                               
         L     R3,AQUARTER                                                      
         MVC   GDDEMO,=X'000001'   GO FOR HOMES FIRST                           
**       MVI   GDDEMO,1            GO FOR HOMES FIRST                           
         BAS   RE,SETEQIV         GET EQUIVALENCED FIRST                        
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VGETDEM                                                          
         L     R1,GDEGRP           HOMES GRPS                                   
         A     R1,QGRPS                                                         
         ST    R1,QGRPS                                                         
         L     R1,GDEIMP           HOMES (000)                                  
         A     R1,QIMPS                                                         
         ST    R1,QIMPS                                                         
         LA    R3,8(R3)                                                         
         MVC   N2PROF(1),N2SAVE       RESET PROFILES                            
         MVC   N0PROF+1(1),N0SAVE                                               
*                                                                               
         BAS   RE,SETRAW           GET RAW DATA BY LENGTH                       
         LA    R2,PLANLSV                                                       
         ZIC   R0,PLANNSV                                                       
PPR9     MVC   LENGTH,0(R2)                                                     
         CLI   REQLEN,0                                                         
         BE    PPR9A                                                            
         CLC   LENGTH,REQLEN                                                    
         BNE   PPR9B                                                            
PPR9A    GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           HOMES GRPS                                   
         A     R1,QGRPS                                                         
         ST    R1,QGRPS                                                         
         L     R1,GDTIMP           HOMES (000)                                  
         A     R1,QIMPS                                                         
         ST    R1,QIMPS                                                         
         LA    R3,8(R3)                                                         
PPR9B    LA    R2,1(R2)                                                         
         BCT   R0,PPR9                                                          
         MVC   N2PROF(1),N2SAVE       RESET PROFILES                            
         MVC   N0PROF+1(1),N0SAVE                                               
         EJECT                                                                  
*                                                                               
         LA    R2,DEMOS            DO OTHER DEMOS                               
         ZIC   R1,NDEMOS                                                        
         STC   R1,DEMONUM          SAVE N'DEMOS                                 
         MVI   DEMCOUNT,1                                                       
PPR10    BAS   RE,GETROW           POINTS R3 TO PROPER DEMO TOTS ROW            
         BAS   RE,SETEQIV          FIRST EQUIVALENCED DATA                      
         MVC   LENGTH,REQLEN                                                    
         MVC   GDDEMO,0(R2)                                                     
***      MVC   GDDEMO,2(R2)                                                     
         GOTO1 VGETDEM                                                          
         L     R1,GDEGRP           GRPS                                         
         A     R1,QGRPS                                                         
         ST    R1,QGRPS                                                         
         L     R1,GDEIMP           IMPS                                         
         A     R1,QIMPS                                                         
         ST    R1,QIMPS                                                         
         MVC   N2PROF(1),N2SAVE       RESET PROFILES                            
         MVC   N0PROF+1(1),N0SAVE                                               
         SPACE 1                                                                
         BAS   RE,SETRAW           NEXT RAW DATA BY LENGTH                      
         LA    R3,8(R3)                                                         
         LA    R4,PLANLSV                                                       
         ZIC   R5,PLANNSV                                                       
PPR11    MVC   LENGTH,0(R4)                                                     
         CLI   REQLEN,0                                                         
         BE    PPR11A                                                           
         CLC   LENGTH,REQLEN                                                    
         BNE   PPR11B                                                           
PPR11A   GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           GRPS                                         
         A     R1,QGRPS                                                         
         ST    R1,QGRPS                                                         
         L     R1,GDTIMP           IMPS                                         
         A     R1,QIMPS                                                         
         ST    R1,QIMPS                                                         
         LA    R3,8(R3)                                                         
PPR11B   LA    R4,1(R4)                                                         
         BCT   R5,PPR11                                                         
         MVC   N2PROF(1),N2SAVE       RESET PROFILES                            
         MVC   N0PROF+1(1),N0SAVE                                               
         ZIC   R1,DEMONUM          HAVE WE FINISHED DEMOS                       
         BCTR  R1,0                                                             
         STC   R1,DEMONUM                                                       
         LTR   R1,R1                                                            
         BZ    PPR20                                                            
         AI    DEMCOUNT,1                                                       
         LA    R2,3(R2)            BUMP DEMO                                    
         B     PPR10                                                            
PPR20    B     XIT                                                              
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
*        POINT R3 TO DEMO TOTALS AREA                                           
*        DEMCOUNT = 1ST,2ND 3D ETC DEMO.                                        
GETROW   NTR1                                                                   
         L     R3,AQUARTER                                                      
         ZIC   R1,DEMCOUNT                                                      
         LA    R3,48(R3)                                                        
         BCT   R1,*-4                                                           
         XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*                                                                               
*    FLUSH DATA SAVE AREAS                                                      
*                                                                               
PRINTIT  NTR1                                                                   
         USING PLINED,R2                                                        
         USING QSAVED,R3                                                        
*                                                                               
         LA    R5,5                5 SAVE AREAS(4 QUARTS + TOTS)                
         MVI   THISQURT,0                                                       
         LA    R3,QUARTER4                                                      
         ST    R3,AQUARTER                                                      
PRNT3    MVI   DEMONUM,0                                                        
         BAS   RE,FLUSHIT                                                       
         L     R3,AQUARTER                                                      
         LA    R3,QURTLENE(R3)                                                  
         ST    R3,AQUARTER                                                      
         AI    THISQURT,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         BCT   R5,PRNT3                                                         
PRNTX    B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
FLUSHIT  NTR1                   DUMPS ONE SAVE AREA AT A TIME                   
         L     R2,AP                                                            
         OC    QUNITS,QUNITS                                                    
         BZ    XIT                                                              
*                              DO FIRST THREE PRINT LINES OF FIRST ROW          
*                                           ( OF 5 PRINT LINES IN ALL)          
         MVC   PTITLES(19),=C'NUMBER OF UNITS   :'                              
         EDIT  (B4,QUNITS),(12,PDATA)                                           
         CLI   N0PROF+1,0                                                       
         BE    PI8                                                              
         EDIT  (B4,QUNITS),(12,PDATA),1                                         
PI8      MVC   PTITLES+132(19),=C'TOTAL COST        :'                          
         LA    R4,PDATA+132                                                     
         EDIT  (B4,QCOST),(12,0(R4)),COMMAS=YES,FLOAT=$                         
         L     R0,QCOST                                                         
         MVC   FULL,QUNITS                                                      
         CLI   N0PROF+1,0                                                       
         BE    PI8C                                                             
         MH    R0,=H'10'                                                        
PI8C     BAS   RE,DIVROUND               RETURNS ROUNDED ANSWER IN FULL         
         MVC   PTITLES+264(19),=C'AVG COST PER UNIT :'                          
         LA    R4,PDATA+264                                                     
         EDIT  (B4,FULL),(12,0(R4)),COMMAS=YES,FLOAT=$                          
*                                                                               
         LA    R2,PDATA                                                         
         ZIC   R5,PLANNSV          GET UNITS FOR LENGTHS                        
         LA    R5,1(R5)            ADD 1 FOR TOT COL                            
         LA    R3,8(R3)                                                         
PI10     LA    R2,14(R2)                                                        
         OC    QUNITS,QUNITS                                                    
         BZ    PI12                                                             
         EDIT  (B4,QUNITS),(12,0(R2))                                           
PI12     LA    R3,8(R3)                                                         
         BCT   R5,PI10                                                          
         BAS   RE,WRITIT           WRITES 1ST THREE LINES                       
*                                                                               
         EJECT                                                                  
*                             NOW NEXT TWO PRINT LINES OF FIRST ROW             
*                                 (HH RATING AND IMPS AVERAGES)                 
         L     R3,AQUARTER                                                      
         L     R2,AP                                                            
         MVC   PTITLES(19),=C'AVG HH RATING     :'                              
         MVC   PTITLES+132(19),=C'AVG HH IMP (000)  :'                          
         LA    R2,PDATA            POINT R2 TO OUTPUT AREA                      
         L     R0,QGRPS            POINT R0 TO GRPS                             
         MVC   FULL,0(R3)          SET UNITS IN FULL                            
         CLI   N0PROF+1,0                                                       
         BE    PI17                                                             
         MH    R0,=H'10'                                                        
PI17     BAS   RE,DIVROUND         RETURNS ROUNDED ANS IN FULL                  
         EDIT  (B4,FULL),(12,0(R2)),1                                           
         LA    R2,132(R2)                                                       
         L     R0,QIMPS            POINT R0 TO IMPS                             
         MVC   FULL,0(R3)                                                       
         CLI   N0PROF+1,0                                                       
         BE    PI18                                                             
         MH    R0,=H'10'                                                        
PI18     BAS   RE,DIVROUND                                                      
         BAS   RE,IMPROUND                                                      
         EDIT  (B4,FULL),(12,0(R2)),COMMAS=YES                                  
*                                                                               
*                                 DO RTG/IMP FOR LENGTHS                        
         L     R3,AQUARTER                                                      
         LA    R3,8(R3)                                                         
         L     R2,AP                                                            
         LA    R2,PDATA                                                         
         LA    R2,14(R2)                                                        
         ZIC   R5,PLANNSV                                                       
         LA    R5,1(R5)            1 FOR TOT COL                                
PI20     OC    0(4,R3),0(R3)                                                    
         BZ    PI24                                                             
         MVC   FULL,0(R3)                                                       
         L     R0,QGRPS                                                         
         LTR   R0,R0                                                            
         BZ    PI21                                                             
         BAS   RE,DIVROUND              RETURNS ROUNDED ANS IN FULL             
         EDIT  (B4,FULL),(12,0(R2)),1                                           
PI21     MVC   FULL,0(R3)                                                       
         L     R0,QIMPS                                                         
         LTR   R0,R0                                                            
         BZ    PI24                                                             
         BAS   RE,DIVROUND                                                      
         BAS   RE,IMPROUND                                                      
         LA    R4,132(R2)                                                       
         EDIT  (B4,FULL),(12,0(R4)),COMMAS=YES                                  
PI24     LA    R2,14(R2)                                                        
         LA    R3,8(R3)                                                         
         BCT   R5,PI20                                                          
         MVI   SPACING,2                                                        
         BAS   RE,WRITIT           WRITES LAST 2 LINES OF 1ST FIVE              
*                                                                               
         EJECT                                                                  
*                                  FLUSH OUT DEMOS                              
*                                                                               
         BAS   RE,DEMONM           PUTS DEMO NAMES INTO DEMNAMES                
         L     R3,AQUARTER                                                      
         ST    R3,TEMPADR          R3 IN TEMPORARY ADR SAVE                     
         MVC   TEMPCOST,4(R3)      STORE COST                                   
PI27     L     R2,AP                                                            
         ZIC   R1,DEMONUM          IF DEMONUM=0, THEN HOMES                     
         LTR   R1,R1                                                            
         BZ    PI28                                                             
         BCTR  R1,0                                                             
         LA    R4,DEMNAMES                                                      
         MH    R1,=H'7'                                                         
         AR    R4,R1                                                            
         MVC   0(7,R2),0(R4)                                                    
         B     *+10                                                             
PI28     MVC   0(10,R2),=C'HOUSEHOLDS'                                          
         MVI   ALLOWLIN,7                                                       
         BAS   RE,WRITIT                                                        
         L     R2,AP                                                            
         MVC   0(19,R2),=C'COST PER POINT    :'                                 
         MVC   132(19,R2),=C'TOTAL GRPS        :'                               
         LA    R4,264(R2)                                                       
         MVC   0(19,R4),=C'GROSS IMPS (000)  :'                                 
         MVC   132(19,R4),=C'CPM               :'                               
         LA    R2,PDATA                              OUTPUT AREA                
         LA    R4,132(R2)                                                       
         EDIT  (B4,QGRPS),(12,0(R4)),1               GRPS                       
         LA    R4,132(R4)                                                       
         MVC   FULL,QIMPS                                                       
         BAS   RE,IMPROUND                                                      
         EDIT  (B4,FULL),(12,0(R4)),COMMAS=YES      IMPS                        
         LA    R4,QGRPS            GRPS                                         
         L     R3,TEMPCOST         COST                                         
         BAS   RE,CPP                                CPP                        
         AH    R2,=H'396'                                                       
         L     R3,TEMPADR          RETURN R3 TO START                           
         LA    R4,QIMPS                                                         
         L     R3,TEMPCOST                                                      
         BAS   RE,CPM                                CPM                        
         L     R3,TEMPADR          RETURN R3 TO START                           
*                                                                               
         LA    R3,8(R3)            BUMP DATA ROW TO LENGTH GRPS/IMPS            
         ZIC   R5,PLANNSV                                                       
         LA    R5,1(R5)            ADD 1 FOR TOTS                               
         L     R2,AP               SET UP R2                                    
         LA    R2,132(R2)                                                       
         LA    R2,PDATA                                                         
PI30     LA    R2,14(R2)                                                        
         CLC   QGRPS,=F'0'                                                      
         BE    PI31                                                             
         EDIT  (B4,QGRPS),(12,0(R2)),1                                          
PI31     LA    R4,132(R2)                                                       
         CLC   QIMPS,=F'0'                                                      
         BE    PI32                                                             
         MVC   FULL,QIMPS                                                       
         BAS   RE,IMPROUND                                                      
         EDIT  (B4,FULL),(12,0(R4)),COMMAS=YES                                  
PI32     LA    R3,8(R3)                                                         
         BCT   R5,PI30                                                          
         MVI   SPACING,2                                                        
         BAS   RE,WRITIT                                                        
         AI    DEMONUM,1                                                        
         CLC   NDEMOS,DEMONUM      HAVE WE FINISHED DEMOS                       
         BL    PIX                                                              
         L     R3,TEMPADR          BUMP R3 TO NEXT ROW                          
         LA    R3,48(R3)                                                        
         ST    R3,TEMPADR                                                       
         B     PI27                                                             
PIX      B     XIT                                                              
         EJECT                                                                  
*-ROUTINE ROUNDS THE IMPS4                                                      
IMPROUND NTR1                                                                   
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
         ST    R1,0(R4)                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  PUTS DEMO NAMES(CL7) INTO DEMNAMES                                           
*                                                                               
DEMONM   NTR1                                                                   
         MVC   DEMOS,DEMOSV                                                     
         MVC   NDEMOS,DEMNMSV                                                   
         MVC   TARGNAME,TRGNMSV                                                 
         LA    R4,DEMNAMES                                                      
         LA    R3,DEMOS                                                         
         ZIC   R5,NDEMOS                                                        
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
         M     R0,=F'2000'                                                      
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
         M     R0,=F'20'                                                        
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
         CLI   0(R2),C' '                                                       
         BE    XIT                                                              
         SPACE 1                                                                
CPPRND   LR    R1,R3               ROUND TO DOLLAS                              
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'20'                                                        
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(12,0(R2)),COMMAS=YES,FLOAT=$                               
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVI   HEAD2+1,C'_'     UNDERLINE SYSTEM NAME                           
         MVC   HEAD2+2(21),HEAD2+1                                              
         CLI   QTITLE,0                                                         
         BE    *+14                                                             
         MVC   WORK(40),TITLE                                                   
         B     HOOK2                                                            
         MVC   WORK(40),=CL40'QUARTERLY EVALUATION'                             
         SPACE 1                                                                
HOOK2    GOTO1 CENTER,DMCB,WORK,40                                              
         MVC   HEAD1+41(40),WORK                                                
         GOTO1 UNDERLIN,DMCB,(40,HEAD1+40),(HORIZ,HEAD2+40)                     
         SPACE 1                                                                
         MVC   HEAD4+9(3),PUPCLI                                                
         MVC   HEAD4+15(20),PUPCLIN                                             
         MVC   HEAD4+62(7),PUPDPT                                               
         CLI   NTPLNFLG,C'Y'                                                    
         BNE   HOOK4                                                            
         MVC   H5+1(7),=C'NETWORK'                                              
         MVC   H5+53(15),=C'PLAN       1988'                                    
         MVC   HEAD5+9(4),PUPNET                                                
         MVC   HEAD5+58(4),PUPPLAN                                              
         LA    R2,HEAD5+64                                                      
         ZIC   R1,PLANYEAR                                                      
         CLI   THISQURT,0           IF WE ARE SHOWING 4TH QUARTER               
         BNE   *+6                                                              
         BCTR  R1,0                USE PREVIOUS YEAR                            
         AH    R1,=H'1900'          Y2K FIX                                     
         EDIT  (R1),(4,0(R2))                                                   
         SPACE 1                                                                
HOOK4    LA    R3,QUALPHA                                                       
         ZIC   R1,THISQURT                                                      
         MH    R1,=H'6'                                                         
         AR    R3,R1                                                            
         MVC   HEAD6+54(6),0(R3)                                                
         CLI   THISQURT,4                                                       
         BNE   HOOK5                                                            
         MVC   HEAD6+54(11),=C'YEAR TOTALS'                                     
         B     *+10                                                             
HOOK5    MVC   HEAD6+61(7),=C'QUARTER'                                          
         GOTO1 SQUASHER,DMCB,HEAD6+54,14                                        
         LA    R2,HEAD9                                                         
         A     R2,DISP                                                          
         USING PLINED,R2                                                        
         MVC   21(11,R2),=C'EQUIVALENCY'                                        
         ZIC   R1,PLANNSV                                                       
         LA    R1,1(R1)                                                         
         MH    R1,=H'14'                                                        
         LA    R3,PDATA+14                                                      
         LR    R4,R3                                                            
         AR    R3,R1                                                            
         SR    R3,R4               R3 = LENGTH OF OUT AREA                      
         LA    R4,PDATA+14                                                      
         MVC   0(19,R4),=C'ANNOUNCEMENT  BASIS'                                 
         PRINT GEN                                                              
         GOTO1 CENTER,DMCB,0(R4),(R3)                                           
         PRINT NOGEN                                                            
         LA    R2,132(R2)                                                       
         LA    R2,PDATA                                                         
         LA    R2,14(R2)                                                        
         ZIC   R4,PLANNSV                                                       
         LA    R3,PLANLSV                                                       
HOOK6    EDIT  (B1,0(R3)),(2,6(R2)),                                            
         MVC   9(3,R2),=C'SEC'                                                  
         MVI   12(R2),0                                                         
         LA    R2,14(R2)                                                        
         LA    R3,1(R3)                                                         
         BCT   R4,HOOK6                                                         
         MVC   6(5,R2),=C'TOTAL'                                                
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
         LA    R3,7(R3)                                                         
         MVI   0(R3),C'T'                                                       
         LA    R3,3(R3)                                                         
         MVI   0(R3),C'M'                                                       
         LA    R3,60(R3)                                                        
         MVI   0(R3),C'B'                                                       
         MVC   BOXCOLS,SPACES                                                   
         LA    R3,BOXCOLS-1                                                     
         A     R3,DISP                                                          
         MVI   0(R3),C'L'                                                       
         LA    R3,1(R3)                                                         
         LA    R3,34(R3)                                                        
         MVI   0(R3),C'C'                                                       
         ZIC   R1,PLANNSV                                                       
         LA    R1,1(R1)                                                         
         MH    R1,=H'14'                                                        
         AR    R3,R1                                                            
         MVI   0(R3),C'R'                                                       
HOOKX    B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 1                                                                
*                                                                               
*              CENTER PRINT LINE                                                
CENTERP  NTR1                                                                   
         CLI   REQLEN,0            IF LENGTH REQUESTED                          
         BE    CT2                                                              
         MVI   PLANNSV,1          SET ACCORDINGLY                               
         MVC   PLANLSV(1),REQLEN                                                
*                                                                               
CT2      ZIC   R1,PLANNSV                                                       
         MH    R1,=H'14'           EACH LENGTH IS CL14                          
         LA    R1,48(R1)           PLUS CL48                                    
         LA    R2,132                                                           
         SR    R2,R1                                                            
         SRA   R2,1                                                             
         ST    R2,DISP                                                          
         LA    R1,P                                                             
         AR    R1,R2                                                            
         ST    R1,AP                                                            
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
SETEQIV  MVC   N2SAVE,N2PROF       FORCE PROFILE TO EQUIVALENCE                 
         MVC   N0SAVE,N0PROF+1                                                  
         CLI   N2PROF,0                                                         
         BNE   *+8                                                              
         MVI   N2PROF,30                                                        
         CLI   N0PROF+1,0                                                       
         BNE   *+8                                                              
         MVI   N0PROF+1,30                                                      
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
BADLENS  MVC   CONHEAD(L'LENERR),LENERR                                         
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
LENERR   DC    C'** ERROR ** PLANS MUST HAVE SAME SPOT LENS'                    
         SPACE 1                                                                
QUALPHA  DC    C'FOURTH'                                                        
         DC    C'FIRST '                                                        
         DC    C'SECOND'                                                        
         DC    C'THIRD '                                                        
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
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPD5D                                                       
         SPACE 1                                                                
TYPE     DS    CL1                                                              
REQLEN   DS    CL1                                                              
SPACOPT  DS    CL1                                                              
PERNUM   DS    XL1                                                              
HORIZ    DS    XL1                                                              
TOTLEVEL DS    CL1                                                              
UNTOTS   DS    CL1                                                              
THISQURT DS    CL1                                                              
DEMONUM  DS    CL1                                                              
DEMCOUNT DS    CL1                                                              
DEMNAMES DS    CL42                                                             
DEMNMSV  DS    CL1                                                              
DEMOSV   DS    CL18                                                             
TRGNMSV  DS    CL7                                                              
N2SAVE   DS    CL1                                                              
N0SAVE   DS    CL1                                                              
ROUND    DS    CL1                                                              
NTPLNFLG DS    CL1                                                              
PLANNSV  DS    CL1                                                              
PLANLSV  DS    CL4                                                              
DISP     DS    F                                                                
AQUARTER DS    F                                                                
AROW     DS    F                                                                
TEMPCOST DS    F                                                                
TEMPADR  DS    F                                                                
AP       DS    F                                                                
TOTSV    DS    F                                                                
NETNAMSV DS    CL400               MAX OF 20X20 NET/PLANNAMESA                  
TITLE    DS    CL40                                                             
QTITLE   DS    CL1                                                              
         DS    0F                                                               
QUARTER4 DS    12F               6 X (UNIT/COST) (QUARTER TOTS)                 
         DS    84F               6 X (GRPS/IMPS) X 7 DEMO CATEGORIES            
QURTLENE EQU   *-QUARTER4                                                       
QUARTER1 DS    96F                                                              
QUARTER2 DS    96F                                                              
QUARTER3 DS    96F                                                              
ALLTOTS  DS    96F                                                              
PWRKLENE EQU   *-PUPWORK                                                        
         SPACE 1                                                                
QSAVED   DSECT                  DSECT FOR TOTAL ACCUM AREAS                     
QUNITS   DS    F                                                                
QCOST    DS    F                                                                
         DS    10F                                                              
QGRPS    DS    F                                                                
QIMPS    DS    F                                                                
         DS    82F                                                              
*                                                                               
PLINED   DSECT                                                                  
PTITLES  DS    CL20                                                             
         DS    CL2                                                              
PDATA    DS    CL12                UP TO 6 DATA COLUMNS                         
         DS    CL2                                                              
*                                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008NEPUP25   05/01/02'                                      
         END                                                                    
