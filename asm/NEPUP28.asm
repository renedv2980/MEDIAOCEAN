*          DATA SET NEPUP28    AT LEVEL 006 AS OF 05/01/02                      
*          DATA SET NEPUP28    AT LEVEL 020 AS OF 01/02/91                      
*PHASE T32228A,*                                                                
         TITLE 'T32228 - PRB6 REPORT'                                           
T32228   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32228**,RA,R6                                                 
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
         OI    ALLOKS,X'04'        ALLOW ALL NETWORKS                           
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
         MVC   8(7,R2),DPTNAME                                                  
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         OI    ALLOKS,X'01'        ALLOW ALL PLANS                              
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
VREC2    LA    R2,PUPDEMOH         DEMO OVERRIDES                               
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         MVI   MAX,6                                                            
         GOTO1 VVALDEM                                                          
         MVC   NDEMOV,NDEMOS                                                    
         MVC   DEMOV,DEMOS                                                      
         GOTO1 VSETDB                                                           
         GOTO1 DEMOCON,DMCB,(0,DEMOS),(7,WORK),(C'S',DBLOCK)                    
         MVC   TRGNMOV,WORK                                                     
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
         MVI   PUPQFLG,0                                                        
         MVI   NETTOTS,0                                                        
         MVI   RAWOPT,0                                                         
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
         CLC   12(1,R4),=C'Q'      QUARTER OPTION                               
         BNE   OPT14                                                            
         MVI   PUPQFLG,C'Y'                                                     
         CLI   11(R4),4                                                         
         BH    BADOPT                                                           
         CLI   11(R4),1                                                         
         BL    BADOPT                                                           
         MVC   PUPQFLT,11(R4)                                                   
         CLI   11(R4),4                                                         
         BNE   OPTEND                                                           
         MVI   PUPQFLT,0                                                        
         B     OPTEND                                                           
         SPACE 1                                                                
OPT14    DS    0H                                                               
         CLC   12(3,R4),=C'NET'    COMBINE SAME NETWORK OPTION                  
         BNE   OPT16                                                            
         MVC   NETTOTS,22(R4)                                                   
         CLI   NETTOTS,C'Y'                                                     
         BE    OPTEND                                                           
         CLI   NETTOTS,C'N'                                                     
         BE    OPTEND                                                           
         B     BADOPT                                                           
         SPACE 1                                                                
OPT16    DS    0H                                                               
         CLC   12(3,R4),=C'RAW'    FORCE RAW NUMBERS/NO EQUIVALENCE             
         BNE   OPT18                                                            
         MVC   RAWOPT,22(R4)                                                    
         CLI   RAWOPT,C'Y'                                                      
         BE    OPTEND                                                           
         CLI   RAWOPT,C'N'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPT18    DS    0H                                                               
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
         MVI   HORIZ,C'_'                                                       
         SPACE 1                                                                
         GOTO1 PUPIO,DMCB,PUPHOOK                                               
         CLI   NETTOTS,C'Y'                                                     
         BNE   ENDIT                                                            
         BAS   RE,PRINTIT                                                       
         BAS   RE,ROLLTOTS                                                      
ENDIT    BAS   RE,FINLTOT                                                       
         B     XIT                                                              
         SPACE 1                                                                
PUPHOOK  NTR1                                                                   
         CLI   PUPMODE,PLANFRST    PLAN FIRST                                   
         BNE   PH3                                                              
         CLI   DEMONUM,0           IS IT FIRST PLAN                             
         BE    PH2                                                              
         CLI   NETTOTS,C'Y'        NO/COMBINE SAME NET OPTION                   
         BNE   PH2B                                                             
         L     R2,AIO                                                           
         USING NPLRECD,R2                                                       
         CLC   NPLKNET,NETSAVE                                                  
         BE    PH2B                                                             
         BAS   RE,PRTNROLL                                                      
         B     PH2D                                                             
         DROP  R2                                                               
*                                                                               
PH2      MVC   DEMONUM,NDEMOS      YES/SAVE 1ST PLAN'S DEMOS                    
         MVC   DEMOSV,DEMOS                                                     
         CLI   NDEMOV,0            ARE THERE DEMO OVERRIDES                     
         BE    PH2G                                                             
         MVC   NDEMOS,NDEMOV                                                    
         MVC   DEMOS,DEMOV                                                      
         MVC   DEMONUM,NDEMOV                                                   
         MVC   DEMOSV,DEMOV                                                     
         MVC   TARGNAME,TRGNMOV                                                 
PH2G     BAS   RE,DEMONM                                                        
         CLI   NETTOTS,C'Y'        COMBINE SAME NET OPTION                      
         BNE   PH2A                                                             
         L     R2,AIO                                                           
         USING NPLRECD,R2                                                       
PH2D     MVC   NETSAVE,NPLKNET                                                  
         DROP  R2                                                               
PH2A     BAS   RE,CENTERP          CENTER PRINT LINE                            
PH2B     BAS   RE,SETEQIV          FORCE EQUIVALENCE                            
         CLI   RAWOPT,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,SETRAW           FORCE RAW                                    
         LA    R5,BUDGETS          GET COST                                     
         LA    R2,4                                                             
BUDG     L     R1,0(R5)                                                         
         A     R1,QCOST                                                         
         ST    R1,QCOST                                                         
         LA    R5,20(R5)                                                        
         BCT   R2,BUDG                                                          
         B     PUPX                                                             
         SPACE 1                                                                
PH3      CLI   PUPMODE,PLANLST     END OF PLAN                                  
         BNE   PH5                                                              
         CLI   NETTOTS,C'Y'        COMBINE SAME NET OPTION                      
         BE    PUPX                YES/EXIT                                     
         BAS   RE,PRTNROLL                                                      
         B     PUPX                                                             
         SPACE 1                                                                
PH5      CLI   PUPMODE,PROGMD      PROGRAM REC                                  
         BNE   PUPX                                                             
         BAS   RE,POSTIT                                                        
         SPACE 1                                                                
PUPX     B     XIT                                                              
*                                                                               
PRTNROLL NTR1                                                                   
         BAS   RE,PRINTIT                                                       
         BAS   RE,ROLLTOTS                                                      
         LA    RE,QSAVE                                                         
         LA    RF,QSAVEND-QSAVE                                                 
         XCEF                                                                   
         CLI   NETTOTS,C'Y'                                                     
         BNE   PUPX                                                             
         L     R2,AIO                                                           
         USING NPLRECD,R2                                                       
         MVC   NETSAVE,NPLKNET                                                  
         B     PUPX                                                             
         DROP  R2                                                               
         EJECT                                                                  
FINLTOT  NTR1                                                                   
         BAS   RE,WRITIT                                                        
         MVI   NETTOTS,0                                                        
         MVI   GDRAWOPT,0          TO TURN OFF MULT BY 10 IN TOTALS             
         XC    PLANNAME,PLANNAME                                                
         MVC   PLANNAME(14),=C'*** TOTALS ***'                                  
         LA    R5,(QSAVEND-QSAVE)/4                                             
         LA    R4,QSAVE                                                         
         LA    R3,TOTSAVE                                                       
MVLOOP   MVC   0(4,R4),0(R3)                                                    
         LA    R4,4(R4)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,MVLOOP                                                        
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROLL DATA IN SAVE AREA TO TOTALS                                              
*                                                                               
ROLLTOTS NTR1                                                                   
         LA    R2,QSAVE                                                         
         LA    R3,TOTSAVE                                                       
         LA    R5,(QSAVEND-QSAVE)/4                                             
RL2      L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,RL2                                                           
ROLX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST PROGRAM DETAILS                                  
         SPACE 3                                                                
*                                                                               
POSTIT   NTR1                                                                   
         MVC   LENGTH,REQLEN       GET UNITS                                    
         MVI   MYBYTE,0                                                         
         LA    R4,PLANPLST                                                      
PST2     MVC   PERIOD,0(R4)                                                     
         GOTO1 VEXTUNS                                                          
         ZIC   R1,UNITS                                                         
         LTR   R1,R1                                                            
         BZ    PST7                                                             
         CLI   N0PROF+1,0                                                       
         BE    PST3                                                             
         LH    R1,UNTEQV                                                        
PST3     A     R1,QUNITS                                                        
         ST    R1,QUNITS                                                        
*                                  POST DEMO DATA                               
         MVC   GDDEMO,=X'000001'   GO FOR HOMES FIRST                           
***      MVI   GDDEMO,1            GO FOR HOMES FIRST                           
         MVC   LENGTH,REQLEN                                                    
         GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           HOMES GRPS                                   
         A     R1,QGRPS                                                         
         ST    R1,QGRPS                                                         
         L     R1,GDTIMP           HOMES (000)                                  
         A     R1,QIMPS                                                         
         ST    R1,QIMPS                                                         
*                                                                               
*        LA    R2,DEMOS            DO OTHER DEMOS                               
*        ZIC   R5,NDEMOS                                                        
         LA    R2,DEMOSV           DO OTHER DEMOS                               
         ZIC   R5,DEMONUM                                                       
         LA    R3,QIMPS+4                                                       
         MVC   LENGTH,REQLEN                                                    
PST5     MVC   GDDEMO,0(R2)                                                     
*PST5     MVC   GDDEMO,2(R2)                                                    
         GOTO1 VGETDEM                                                          
*                                                                               
         ZIC   R0,DEMONUM          FIND DEMO ROW                                
         LA    R1,DEMOSV                                                        
PST6     CLC   0(3,R2),0(R1)                                                    
         BE    PST6B                                                            
         LA    R1,3(R1)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,PST6                                                          
         B     PST6D               DEMO NOT FOUND/GET NEXT DEMO                 
*                                                                               
PST6B    L     R1,GDTGRP           GRPS                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         L     R1,GDTIMP           IMPS                                         
         A     R1,4(R3)                                                         
         ST    R1,4(R3)                                                         
*                                                                               
PST6D    LA    R2,3(R2)            BUMP DEMO                                    
         LA    R3,QIMPS+4          RESET TO START OF DEMO SAVE AREA             
         BCT   R5,PST5                                                          
PST7     LA    R4,4(R4)                                                         
         AI    MYBYTE,1                                                         
         CLC   MYBYTE,PLANNPER                                                  
         BNE   PST2                                                             
PSTX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*    FLUSH DATA SAVE AREAS                                                      
*                                                                               
PRINTIT  NTR1                                                                   
         USING LINED,R2                                                         
         L     R2,AP                                                            
         OC    QUNITS,QUNITS                                                    
         BZ    XIT                                                              
         CLI   GDRAWOPT,C'Y'       IF IT'S RAW                                  
         BNE   PRT4                                                             
         L     R1,QUNITS                                                        
         MH    R1,=H'10'           MULTIPLY BY 10 SO ITS SAME AS EQUIV          
         ST    R1,QUNITS                                                        
PRT4     MVC   LPLAN(4),NETSAVE                                                 
         CLI   NETTOTS,C'Y'                                                     
         BE    *+10                                                             
         MVC   LPLAN,PLANNAME                                                   
         MVC   LTITLES(14),=C'COST PER POINT'                                   
         MVC   LTITLES+132(10),=C'TOTAL GRPS'                                   
         MVC   LTITLES+264(10),=C'GROSS IMPS'                                   
         MVC   LTITLES+396(3),=C'CPM'                                           
         LA    R4,QGRPS                                                         
         LA    R2,LHOMES                                                        
         ST    R2,APSV                                                          
         ZIC   R5,DEMONUM                                                       
         LA    R5,1(R5)            ADD 1 FOR HOMES                              
PRT5     L     R0,QCOST                                                         
         LR    R1,R4                                                            
         BAS   RE,CPP                              CPP                          
         LA    R3,132(R2)                                                       
         EDIT  (B4,0(R4)),(10,0(R3)),1             GRPS                         
         LA    R3,264(R2)                                                       
         MVC   FULL,4(R4)                                                       
         BAS   RE,IMPROUND                                                      
         EDIT  (B4,FULL),(10,0(R3)),COMMAS=YES    IMPS                          
         A     R2,=F'396'                                                       
         L     R0,QCOST                                                         
         LA    R1,4(R4)            RAW IMPS                                     
         BAS   RE,CPM                                                           
         L     R2,APSV             BUMP PRINT LINE                              
         LA    R2,11(R2)                                                        
         ST    R2,APSV                                                          
         LA    R4,8(R4)            BUMP DATA AREA                               
         BCT   R5,PRT5                                                          
         MVI   SPACING,2                                                        
         MVI   ALLOWLIN,4                                                       
         CLI   PLANNAME,C'*'       IS IT TOTALS                                 
         BNE   *+8                                                              
         MVI   ALLOWLIN,10                                                      
         BAS   RE,WRITIT                                                        
*                                                                               
         L     R2,AP                                                            
         MVC   LPLAN+14(15),=C'NUMBER OF UNITS'                                 
         LA    R2,LHOMES                                                        
***      CLI   N0PROF+1,0                                                       
***      BE    PRT7                                                             
         EDIT  (B4,QUNITS),(10,0(R2)),1                                         
         B     PRT7B                                                            
PRT7     EDIT  (B4,QUNITS),(10,0(R2))                                           
PRT7B    MVI   ALLOWLIN,5                                                       
         BAS   RE,WRITIT                                                        
*                                                                               
         L     R2,AP                                                            
         LA    R2,LPLAN+14                                                      
         MVC   0(10,R2),=C'TOTAL COST'                                          
         MVC   132(17,R2),=C'AVG COST PER UNIT'                                 
         MVC   264(13,R2),=C'AVG HH RATING'                                     
         MVC   396(10,R2),=C'AVG HH IMP'                                        
         L     R2,AP                                                            
         LA    R2,LHOMES                                                        
         EDIT  (B4,QCOST),(10,0(R2)),COMMAS=YES,FLOAT=$                         
         L     R0,QCOST                                                         
         MVC   FULL,QUNITS                                                      
         BAS   RE,DIVROUND                                                      
         EDIT  (B4,FULL),(10,132(R2)),COMMAS=YES,FLOAT=$                        
         L     R0,QGRPS                                                         
         MVC   FULL,QUNITS                                                      
         BAS   RE,DIVROUND                                                      
         EDIT  (B4,FULL),(10,264(R2)),1                                         
         L     R0,QIMPS                                                         
         MVC   FULL,QUNITS                                                      
         BAS   RE,DIVROUND                                                      
         BAS   RE,IMPROUND                                                      
         EDIT  (B4,FULL),(10,396(R2)),COMMAS=YES                                
         MVI   SPACING,3                                                        
         BAS   RE,WRITIT                                                        
*                                                                               
PRTX     B     XIT                                                              
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
         LA    R4,DEMNAMES                                                      
         LA    R3,DEMOS                                                         
         ZIC   R5,NDEMOS                                                        
         GOTO1 VSETDB                                                           
         MVC   DBFILE,=C'NAD'                                                   
         MVC   DBSELMED,C'T'                                                    
DEM4     GOTO1 DEMOCON,DMCB,(0,0(R3)),(10,WORK),(C'S',DBLOCK)                   
         MVC   0(10,R4),WORK                                                    
         LA    R3,3(R3)                                                         
         LA    R4,10(R4)                                                        
         BCT   R5,DEM4                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DIVIDES / ROUNDS AND RETURNS ROUNDED ANSWER IN FULL                           
*                                                                               
*                      R0 = DIVIDEND                                            
*                    FULL = DIVISOR                                             
*                                                                               
DIVROUND NTR1                                                                   
**       CLI   N0PROF+1,0                                                       
**       BE    *+4                                                              
         MH    R0,=H'10'      RAW AND EQIV UNIT COUNT IS TO 1 DECIMAL           
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
*                                  R0=R3=A(COST)                                
*                                  R1=R4=A(IMPS)                                
         SPACE 1                                                                
CPM      NTR1                                                                   
         LR    R3,R0                                                            
         LR    R4,R1                                                            
*                                                                               
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
         EDIT  (R1),(10,0(R2)),2,FLOAT=$                                        
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
         EDIT  (R1),(10,0(R2)),FLOAT=$                                          
         B     XIT                                                              
         EJECT                                                                  
*              SUB-ROUTINE TO SHOW CPP                                          
         SPACE 3                                                                
*              INPUTS              R2=A(OUTPUT AREA)                            
*                                  R0=R3=A(COST)                                
*                                  R1=R4=A(GRPS)                                
         SPACE 1                                                                
CPP      NTR1                                                                   
         LR    R3,R0                                                            
         LR    R4,R1                                                            
*                                                                               
         LR    R1,R4                                                            
         L     R1,0(R1)            PICK UP GRPS                                 
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
         EDIT  (R1),(10,0(R2)),2,FLOAT=$                                        
         B     XIT                                                              
         SPACE 1                                                                
CPPRND   LR    R1,R3               COMPUTE CPP IN DOLLARS                       
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'20'                                                        
         D     R0,0(R4)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(10,0(R2)),FLOAT=$,COMMAS=YES                               
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
HOOK1    MVC   WORK(40),=CL40'NETWORK SUMMARY'                                  
         SPACE 1                                                                
HOOK2    GOTO1 CENTER,DMCB,WORK,40                                              
         MVC   HEAD1+41(40),WORK                                                
         GOTO1 UNDERLIN,DMCB,(40,HEAD1+40),(HORIZ,HEAD2+40)                     
         MVC   HEAD4+9(3),PUPCLI                                                
         MVC   HEAD4+15(20),PUPCLIN                                             
*                                                                               
         LA    R2,HEAD7                                                         
         A     R2,DISP                                                          
         USING LINED,R2                                                         
         MVC   LPLAN(9),=C'PLAN NAME'                                           
         ZIC   R5,DEMONUM                                                       
         LA    R3,DEMNAMES                                                      
         MVC   LHOMES(10),=C'HOUSEHOLDS'                                        
         LA    R2,LDEMOS                                                        
HOOK3    MVC   0(10,R2),0(R3)                                                   
         LA    R3,10(R3)                                                        
         LA    R2,11(R2)                                                        
         BCT   R5,HOOK3                                                         
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
         LA    R3,5(R3)                                                         
         MVI   0(R3),C'T'                                                       
         LA    R3,2(R3)                                                         
         MVI   0(R3),C'M'                                                       
         LA    R3,65(R3)                                                        
         MVI   0(R3),C'B'                                                       
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         ZIC   R5,DEMONUM                                                       
         LA    R5,1(R5)            ADD HOMES                                    
         A     R2,DISP                                                          
         MVI   0(R2),C'L'                                                       
         LA    R2,LTITLES+15                                                    
         MVI   0(R2),C'C'                                                       
         LA    R2,11(R2)                                                        
         BCT   R5,*-8                                                           
         MVI   0(R2),C'R'                                                       
HOOKX    B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 1                                                                
*                                                                               
*              CENTER PRINT LINE                                                
CENTERP  ZIC   R1,DEMONUM                                                       
         MH    R1,=H'11'           EACH DEMO = CL11                             
         LA    R1,45(R1)           + CL45 FOR TITLES                            
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
SETEQIV  DS    0H                  FORCE PROFILE TO EQUIVALENCE                 
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
SETRAW   DS    0H                  FORCE PROFILE TO RAW                         
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
       ++INCLUDE NEPUPD8D                                                       
         SPACE 1                                                                
WORKD    DS    0CL1                WORK AREA                                    
TYPE     DS    CL1                                                              
REQLEN   DS    CL1                                                              
HORIZ    DS    XL1                                                              
MYBYTE DS      CL1                                                              
DEMONUM  DS    CL1                                                              
DEMCOUNT DS    CL1                                                              
DEMOSV   DS    CL18                                                             
NDEMOV   DS    CL1                                                              
DEMOV    DS    CL18                                                             
TRGNMOV  DS    CL7                                                              
TRGNMSV  DS    CL7                                                              
DEMNAMES DS    CL70                                                             
ROUND    DS    CL1                                                              
NETTOTS  DS    CL1                                                              
RAWOPT   DS    CL1                                                              
NETSAVE  DS    CL4                                                              
DISP     DS    F                                                                
AP       DS    F                                                                
APSV     DS    F                                                                
TITLE    DS    CL40                                                             
QTITLE   DS    CL1                                                              
         SPACE 1                                                                
QSAVE    DS    0F                                                               
QUNITS   DS    F                                                                
QCOST    DS    F                                                                
QGRPS    DS    F                                                                
QIMPS    DS    F                                                                
         DS    12F                                                              
QSAVEND  EQU   *                                                                
         SPACE 1                                                                
TOTSAVE  DS    0F                                                               
TUNITS   DS    F                                                                
TCOST    DS    F                                                                
TGRPS    DS    F                                                                
TIMPS    DS    F                                                                
         DS    12F                                                              
TSAVEND  EQU   *                                                                
         SPACE 1                                                                
WORKEND  EQU   *                                                                
*                                                                               
LINED    DSECT                                                                  
         DS    CL1                                                              
LPLAN    DS    CL16                                                             
         DS    CL2                                                              
LTITLES  DS    CL14                                                             
         DS    CL2                                                              
LHOMES   DS    CL10                                                             
         DS    CL1                                                              
LDEMOS   DS    CL66                UP TO 6 DATA COLUMNS                         
LEND     DS    CL1                                                              
*                                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEPUP28   05/01/02'                                      
         END                                                                    
