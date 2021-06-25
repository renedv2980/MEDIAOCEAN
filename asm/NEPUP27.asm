*          DATA SET NEPUP27    AT LEVEL 034 AS OF 05/01/02                      
*          DATA SET NEPUP27    AT LEVEL 137 AS OF 07/09/90                      
*PHASE T32227A,*                                                                
*INCLUDE BINSRCH2                                                               
         TITLE 'T32227 - PWEE REPORT'                                           
T32227   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32227**,RA,R6,RR=R2                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         ST    R2,RELO                                                          
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
         LA    RE,WORKD                                                         
         LA    RF,WORKDEND-WORKD                                                
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
***      MVC   8(7,R2),DPTNAME                                                  
**       OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         OI    ALLOKS,X'01'                                                     
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         OI    6(R2),X'80'                                                      
         OC    PLANCODE,PLANCODE   IF PLAN                                      
         BZ    *+8                                                              
         MVI   NTPLNFLG,C'Y'       PRINT NET/PLAN IN HEADS                      
         SPACE 1                                                                
         MVI   PUPQFLG,0                                                        
         LA    R2,PUPQRTH          QUARTER                                      
         CLI   5(R2),0                                                          
         BE    OPTCHK                                                           
         CLI   PLANPERT,C'W'       ONLY FOR WEEKLY PLANS                        
         BE    VQRT                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(19),=C'** ERROR ** INVALID'                              
         B     MYCURSOR                                                         
VQRT     MVI   PUPQFLG,C'Y'                                                     
         CLI   PUPQRT,C'1'                                                      
         BL    QRTERR                                                           
         CLI   PUPQRT,C'4'                                                      
         BH    QRTERR                                                           
         PACK  DUB,PUPQRT                                                       
         CVB   R0,DUB                                                           
         STC   R0,PUPQFLT                                                       
         CLI   PUPQFLT,4           CHANGE 4TH Q TO 0                            
         BNE   *+8                                                              
         MVI   PUPQFLT,0                                                        
         B     VDEMOS                                                           
QRTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MSGQUART),MSGQUART                                     
         B     MYCURSOR                                                         
OPTCHK   DS    0H                                                               
         CLI   PUPQFLG,C'Y'        QUARTER OPTION                               
         BE    VDEMOS                                                           
         CLI   PLANPERT,C'W'       IF WEEKLY PLAN                               
         BNE   VDEMOS                                                           
         LA    R2,PUPQRTH          NEED QUARTER OPTION                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'** ERROR ** QUARTER REQUIRED'                     
         B     MYCURSOR                                                         
         SPACE 1                                                                
VDEMOS   LA    R2,PUPDEMOH                                                      
         CLI   5(R2),0                                                          
         BE    VOPTS                                                            
         GOTO1 VVALDEM                                                          
         MVI   DEMSREQD,0          PXZPXZ                                       
         MVI   DEMOVRID,C'Y'                                                    
         MVC   DEMSV,DEMOS                                                      
         SPACE 1                                                                
VOPTS    LA    R2,PUPOPTH          OPTIONS                                      
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
         MVI   REQOPT,C'R'         RTG IS DEFAULT                               
         MVI   EQVOPT,C'N'         NO EQUIVALENCE IS DEFAULT                    
         MVI   LGOPT,C'N'          NO LENGHT TOTS                               
         CLI   5(R2),0                                                          
         BE    OPTX                                                             
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
OPT6     CLC   12(1,R4),=C'Q'      QUARTER OPTION                               
         BNE   OPT8                                                             
         SPACE 1                                                                
OPT8     CLC   12(3,R4),=C'OPT'    RTG/UNT OPTION                               
         BNE   OPT10                                                            
         MVC   REQOPT,22(R4)                                                    
         CLI   REQOPT,C'R'                                                      
         BE    OPTEND                                                           
         CLI   REQOPT,C'U'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
OPT10    DS    0H                                                               
         CLC   12(3,R4),=C'EQV'    EQUIVALENCE OPTION                           
         BNE   OPT12                                                            
         MVC   EQVOPT,22(R4)                                                    
         CLI   EQVOPT,C'Y'                                                      
         BE    OPTEND                                                           
         CLI   EQVOPT,C'N'                                                      
         BE    OPTEND                                                           
OPT12    DS    0H                                                               
         CLC   12(3,R4),=C'TOT'    LENGTH TOTALS OPTION                         
         BNE   OPT14                                                            
         MVC   LGOPT,22(R4)                                                     
         CLI   LGOPT,C'Y'                                                       
         BE    OPTEND                                                           
         CLI   LGOPT,C'N'                                                       
         BE    OPTEND                                                           
         B     BADOPT                                                           
OPT14    DS    0H                                                               
         B     BADOPT                                                           
         SPACE 3                                                                
OPTEND   LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,OPT2                                                          
OPTX     B     XIT                                                              
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYCURSOR                                                         
         EJECT                                                                  
*              CONTROL REPORTS                                                  
         SPACE 3                                                                
PREP     NTR1                                                                   
*                                                                               
         LA    R1,HEDSPECS         INITIALIZE                                   
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         CLI   EQVOPT,C'Y'         EQUIVALENCE OPTION                           
         BNE   *+8                                                              
         BAS   RE,SETEQV                                                        
         CLI   PLANPERT,C'W'       IF WEEKLY PLAN                               
         BNE   *+8                                                              
         MVI   PUPQFLG,C'Y'        SET FLAG(DEFAULT IS 4TH QUARTER)             
         GOTO1 PUPIO,DMCB,PUPHOOK                                               
         CLI   LGOPT,C'Y'          LENGTH SUB TOTALS                            
         BNE   *+8                                                              
         BAS   RE,SUBTOTS                                                       
         BAS   RE,ENDTOTS          GRAND TOTALS                                 
         B     XIT                                                              
         SPACE 1                                                                
*                                                                               
PUPHOOK  NTR1                                                                   
         CLI   PUPMODE,PLANFRST                                                 
         BE    PH20                                                             
         CLI   PUPMODE,PLANLST                                                  
         BE    PH30                                                             
         CLI   PUPMODE,PROGMD                                                   
         BNE   PHX                                                              
         BAS   RE,PROGBIN          SET PROGS TO BINSRCH                         
         B     PHX                                                              
PH20     BAS   RE,CLEARBIN                                                      
         CLI   PPERTSV,0                                                        
         BE    PH20E                                                            
         CLC   PPERTSV,PLANPERT                                                 
         BE    PH20E                                                            
         MVI   FORCEHED,C'Y'                                                    
PH20E    MVC   PPERTSV,PLANPERT                                                 
         L     R1,AIO1                                                          
         USING NPLRECD,R1                                                       
         MVC   PCODESV,NPLKPLAN                                                 
         CLI   PLANPERT,C'W'       IF WEEKLY PLAN                               
         BNE   PH20F                                                            
         CLI   PUPQFLG,C'Y'        NEEDS QUARTER FILTER                         
         BE    PH20X                                                            
         MVI   PUPQFLG,C'Y'        SET TO 4TH QUARTER IF NOT SET                
         MVI   PUPQFLT,0           (AS COULD BE THE CASE IF PLAN=ALL)           
         B     PH20X                                                            
PH20F    MVI   PUPQFLG,0                                                        
PH20X    B     PHX                                                              
         DROP  R1                                                               
PH30     OC    BINDMCB+8(4),BINDMCB+8                                           
         BZ    XIT                                                              
         MVC   NPERIODS,PLANNPER                                                
         LA    R1,PLANPLST                                                      
         ST    R1,APLST                                                         
         CLI   PLANPERT,C'W'       IF WEEKLY                                    
         BNE   PH30G                                                            
         CLI   PROGPERQ,0          AND FOR 1ST,2ND,3D QUARTER                   
         BE    PH30G                                                            
         MVI   NPERIODS,13         SKIP 1ST 3 WEEKS OF 16 OF PLIST              
         L     R1,APLST                                                         
         LA    R1,12(R1)                                                        
         ST    R1,APLST                                                         
*                                                                               
         CLI   PROGPERQ,1          ,,IF FIRST                                   
         BNE   SKPJDX                                                           
         CLI   PLANYEAR,101        ,, AND 2001                                  
         BNE   SKPJDX                                                           
         CLI   PLANPRFL,C'C'        ,,AND CALENDAR                              
         BNE   *+12                                                             
         MVI   NPERIODS,13         ,,IT'S 13 WEEKS                              
         B     SKPJDX                                                           
         MVI   NPERIODS,12         ,,ELSE 12 WEEKS                              
         L     R1,APLST                                                         
         LA    R1,4(R1)                                                         
         ST    R1,APLST                                                         
SKPJDX   EQU   *                                                                
*                                                                               
         CLI   PROGPERQ,3          IF THIRD                                     
         BNE   SKPJDX2                                                          
         CLI   PLANYEAR,101         AND 2001                                    
         BNE   SKPJDX2                                                          
         MVI   NPERIODS,14         14 WEEKS                                     
         L     R1,APLST                                                         
         A     R1,=F'-4'                                                        
         ST    R1,APLST                                                         
SKPJDX2  EQU   *                                                                
*                                                                               
         CLI   PROGPERQ,3       *  HARD CODED FUDGE  IF 3D PERIOD               
         BNE   PH30A               *                                            
         CLI   PLANYEAR,100        *            AND 2000                        
         BNE   PH30A               *                                            
*        CLI   PROGPERQ,2       *  HARD CODED FUDGE  IF 2D PERIOD               
*        BNE   PH30A               *                                            
*        CLI   PLANYEAR,96         *            AND 1996                        
*        BNE   PH30A               *                                            
         CLI   PLANPRFL,C'B'       *            AND IF BROADCAST                
         BE    PH30B               *                                            
         CLI   PLANPRFL,C'C'       *                                            
         BE    PH30A               *                                            
         CLI   N0PROF+3,C'B'       *            DO 14 WEEKS                     
         BE    PH30B               *                                            
*                                                                               
PH30A    CLI   PROGPERQ,3       *  HARD CODED FUDGE  IF 3D PERIOD               
         BNE   PH30G               *                                            
         CLI   PLANYEAR,X'5B'      *            AND 1991                        
         BNE   PH30G               *                                            
         CLI   PLANPRFL,C'B'       *            AND IF CALENDAR                 
         BE    PH30G               *                                            
         CLI   PLANPRFL,C'C'       *                                            
         BE    PH30B               *                                            
         CLI   N0PROF+3,C'C'       *  CHK PROF IF PLANPRFL = 0                  
         BNE   PH30G               *                                            
PH30B    MVI   NPERIODS,13         *                                            
         L     R1,APLST            *                                            
         A     R1,=F'-4'           *                                            
         ST    R1,APLST            *                                            
*                                                                               
PH30G    L     R1,AIO1                                                          
         USING NPURECD,R1                                                       
         MVC   P+1(4),NPUKNET                                                   
         MVC   P2+1(3),PCODESV                                                  
         MVC   P2+6(16),PLANNAME                                                
         DROP  R1                                                               
         BAS   RE,WRITIT                                                        
         BAS   RE,DOPROGS          READ/PRINT PROGS FROM BINSRCH                
         BAS   RE,PLANTOTS         PLAN TOTS/ROLL TO GRAND TOTS                 
PHX      B     XIT                                                              
         EJECT                                                                  
         SPACE                                                                  
* READ PROGRAM RECS AND PUT TO BINSRCH                                          
*                                                                               
PROGBIN  NTR1                                                                   
         LA    R3,WORK             SET INTO BINSRCH                             
         USING BIND,R3                                                          
         XC    0(BINRLENE,R3),0(R3)                                             
         MVC   BINDAY,PROGDAYC                                                  
         MVC   BINTIME,PROGMIL                                                  
         MVC   BINPROG,PROGCODE                                                 
         MVC   BINDATA,KEY                                                      
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R3)),RR=RELO                             
         CLI   0(R1),X'01'        IF REC FOUND                                  
         BE    PRGX                                                             
         DC    H'0'                TAKE A HIT                                   
PRGX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*    STEP THROUGH BINTABLE AND RE-READ THE PROGRAM RECS(NOW                     
*    IN DAY/TIME ORDER)                                                         
*                                                                               
DOPROGS  NTR1                                                                   
         USING BIND,R3                                                          
         LA    R3,BUFF                                                          
DPR1     OC    0(5,R3),0(R3)                                                    
         BZ    DPRX                                                             
         MVC   KEY(20),BINDATA                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         GOTO1 VEXTPROG                                                         
         ZIC   R2,PLANNLEN                                                      
         LA    R4,PLANLENS                                                      
         CLI   REQLEN,0                                                         
         BE    DPR2                                                             
         MVC   LENGTH,REQLEN                                                    
         LA    R2,1                                                             
         B     *+10                                                             
DPR2     MVC   LENGTH,0(R4)                                                     
         BAS   RE,GETDEMS          GETS DEMOS FOR LENGTH                        
         CLI   LGOPT,C'Y'          LENGTH TOTALS OPTION                         
         BNE   *+8                                                              
         BAS   RE,ADDLENS          ADD TO LENGTH TOTS                           
         BAS   RE,PUTOP            PUT TO PRINT LINE                            
         LA    R4,1(R4)            GET NEXT LENGTH                              
         BCT   R2,DPR2                                                          
         LA    R3,BINRLENE(R3)                                                  
         B     DPR1                                                             
DPRX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
* SETS RTG/UNITS FOR REQUESTED LENGTH INTO LWEEK AREA                           
*                   LENGTH IS SET AT THIS POINT                                 
*                                                                               
GETDEMS  NTR1                                                                   
         ZIC   R2,NPERIODS                                                      
         L     R3,APLST                                                         
         LA    R4,LWEEKS1                                                       
GTD3     CLI   PUPQFLG,C'Y'       ARE WE FILTERING ON QUARTER                   
         BNE   GTD5                                                             
         CLC   PUPQFLT,2(R3)                                                    
         BNE   GTD10                                                            
*                                                                               
GTD5     MVC   PERIOD,0(R3)                                                     
         GOTO1 VEXTUNS             GET UNITS                                    
         ZIC   R1,UNITS                                                         
         A     R1,0(R4)            WEEK UNITS                                   
         ST    R1,0(R4)                                                         
         ZIC   R1,UNITS                                                         
         A     R1,LUNT                                                          
         ST    R1,LUNT                                                          
         MVC   GDDEMO,TARGET       DEMO                                         
**       MVC   GDDEMO,TARGET+2     DEMO                                         
         CLI   DEMOVRID,C'Y'       IS THERE AN OVERRIDE                         
         BNE   *+10                                                             
         MVC   GDDEMO,DEMSV                                                     
***      MVC   GDDEMO,DEMSV+2                                                   
         GOTO1 VGETDEM                                                          
         L     R1,GDTGRP                                                        
         A     R1,64(R4)           WEEK HHRTG                                   
         ST    R1,64(R4)                                                        
         L     R1,GDTGRP                                                        
         A     R1,LHHRTG           WEEK HHRTG                                   
         ST    R1,LHHRTG                                                        
         LA    R4,4(R4)            BUMP WEEK LIST                               
GTD10    LA    R3,4(R3)            BUMP PERIOD LIST                             
         BCT   R2,GTD3                                                          
         B     XIT                                                              
*                                                                               
*                                                                               
ADDLENS  NTR1                                                                   
         LA    R2,LINTOTS          ADD TO LENGTH TOTALS                         
         LA    R3,LGTOTS                                                        
         LA    R5,10                                                            
ADL11    OC    0(4,R3),0(R3)                                                    
         BZ    ADL11B                                                           
         CLC   LENGTH,3(R3)                                                     
         BNE   ADL14                                                            
ADL11B   MVC   3(1,R3),LENGTH                                                   
         LA    R3,4(R3)                                                         
         LA    R4,34                                                            
ADL12    L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,ADL12                                                         
         B     ADLX                                                             
ADL14    LA    R3,LGTOTLN(R3)                                                   
         BCT   R5,ADL11                                                         
         DC    H'0'                TOO MANY LENGTHS                             
ADLX     B     XIT                                                              
         EJECT                                                                  
* WRITE WEEK DATA TO PRINTLINE                                                  
*                                                                               
PUTOP    NTR1                          0(R4) HAS SPOT LENGTH                    
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         LA    R3,LINTOTS                                                       
         USING LINTOTS,R3                                                       
         OC    LUNT,LUNT                                                        
         BZ    XIT                                                              
         ZIC   R5,NPERIODS                                                      
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         MVC   PIPROG,PROGNAME                                                  
         EDIT  LHHRTG,(5,PIHHRTG),1                                             
         EDIT  LUNT,(3,PIUNT)                                                   
         CLI   REQLEN,0                                                         
         BE    *+8                                                              
         LA    R4,REQLEN                                                        
         EDIT  (B1,0(R4)),(2,PILEN)                                             
         LA    R4,LWEEKS1                                                       
         LA    R2,PICOST+11                                                     
PTP2     CLI   REQOPT,C'R'         IS IT RTG                                    
         BNE   PTP4                                                             
         OC    64(4,R4),64(R4)                                                  
         BNZ   PTP2B                                                            
         MVI   3(R2),C'-'                                                       
         B     PTP5                                                             
PTP2B    EDIT  (B4,64(R4)),(5,0(R2)),1                                          
         CLI   0(R2),X'40'                                                      
         BNH   PTP5                                                             
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         CLI   0(R1),X'40'                                                      
         BNH   PTP5                                                             
         MVC   0(5,R2),=5X'40'                                                  
         EDIT  (B4,64(R4)),(5,132(R2)),1                                        
         B     PTP5                                                             
PTP4     OC    0(4,R4),0(R4)                                                    
         BNZ   PTP4B                                                            
         MVI   3(R2),C'-'                                                       
         B     PTP5                                                             
PTP4B    EDIT  (B4,0(R4)),(5,0(R2))                                             
PTP5     LA    R2,5(R2)                                                         
         CLI   PLANPERT,C'W'                                                    
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,PTP2                                                          
         BAS   RE,WRITIT                                                        
         DROP  R2,R3                                                            
*                                                                               
         LA    R4,LHHRTG           ROLL WEEK TO PLAN TOTS                       
         LA    R1,PHHRTG                                                        
         SR    R1,R4                                                            
         SRA   R1,2                                                             
         LA    R5,PHHRTG                                                        
PTP7     L     R2,0(R4)                                                         
         A     R2,0(R5)                                                         
         ST    R2,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R1,PTP7                                                          
*                                                                               
         LA    R4,LHHRTG           CLEAR WEEK AREAS                             
         LA    R1,PHHRTG                                                        
         SR    R1,R4                                                            
         MVI   0(R4),0                                                          
         LA    R4,1(R4)                                                         
         BCT   R1,*-8                                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
PLANTOTS NTR1                                                                   
         LA    R4,PHHRTG           ROLL PLAN TO GRAND TOTOS                     
         LA    R1,GHHRTG                                                        
         SR    R1,R4                                                            
         SRA   R1,2                                                             
         LA    R5,GHHRTG                                                        
PLT3     L     R2,0(R5)                                                         
         A     R2,0(R4)                                                         
         ST    R2,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R1,PLT3                                                          
*        LA    R4,PHHRTG                                                        
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         BAS   RE,WRITIT                         SKIP A LINE                    
         MVC   PIPROG(14),=C'PLAN GRP TOTAL'                                    
         MVI   P3+1,0                                                           
         MVC   PIPROG+399(10),=C'UNIT TOTAL'                                    
         LA    R4,PIHHRTG                                                       
         BCTR  R4,0                                                             
         EDIT  PHHRTG,(6,0(R4)),1                                               
         EDIT  PUNT,(3,PIUNT)                                                   
         BAS   RE,GETBUDG                                                       
         LA    R4,PLANRTG                                                       
         ZIC   R5,NPERIODS                                                      
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         LA    R2,PICOST+11                                                     
PLT5     CLI   MYBYT,0                   PUT RTG ON ALTERNATE LINES             
         BNE   PLT5B                                                            
         OC    64(4,R4),64(R4)                                                  
         BNZ   *+12                                                             
         MVI   3(R2),C'-'                                                       
         B     PLT5A                                                            
         EDIT  (B4,64(R4)),(5,0(R2)),1                                          
PLT5A    MVI   MYBYT,1                                                          
         B     PLT7                                                             
PLT5B    OC    64(4,R4),64(R4)                                                  
         BNZ   *+12                                                             
         MVI   135(R2),C'-'                                                     
         B     PLT5C                                                            
         EDIT  (B4,64(R4)),(5,132(R2)),1                                        
PLT5C    MVI   MYBYT,0                                                          
PLT7     OC    0(4,R4),0(R4)                                                    
         BNZ   *+12                                                             
         MVI   399(R2),C'-'                                                     
         B     PLT7B                                                            
         EDIT  (B4,0(R4)),(5,396(R2))                                           
PLT7B    LA    R2,5(R2)                                                         
         CLI   PLANPERT,C'W'                                                    
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,PLT5                                                          
         BAS   RE,WRITIT                                                        
         LA    R4,PHHRTG           CLEAR PLAN TOTS                              
         LA    R1,GHHRTG                                                        
         SR    R1,R4                                                            
         MVI   0(R4),0                                                          
         LA    R4,1(R4)                                                         
         BCT   R1,*-8                                                           
         LA    R3,P                                                             
         SR    R2,R3                                                            
         BCTR  R2,0                                                             
         LA    R3,P                                                             
         MVI   1(R3),C'-'          DRAW A LINE BETWEEN PLANS                    
         LA    R3,1(R3)                                                         
         BCT   R2,*-8                                                           
         BAS   RE,WRITIT                                                        
PLTX     B     XIT                                                              
         SPACE                                                                  
*                                                                               
*  GET BUDGET FROM PLAN RECORD                                                  
GETBUDG  NTR1                                                                   
         LA    R3,BUDGETS                                                       
         CLI   PLANPERT,C'W'                                                    
         BE    GTB5                                                             
         CLI   PUPQFLG,C'Y'                                                     
         BE    GTB5                                                             
         LA    R1,4                                                             
         SR    R4,R4                                                            
GTB3     L     R5,0(R3)                                                         
         AR    R4,R5                                                            
         LA    R3,20(R3)                                                        
         BCT   R1,GTB3                                                          
         L     R1,GCOST                                                         
         AR    R1,R4                                                            
         ST    R1,GCOST                                                         
         EDIT (R4),(10,PICOST),COMMAS=YES                                       
         B     GTBX                                                             
GTB5     ZIC   R1,PUPQFLT          GET BUDGET FOR THIS QUARTER                  
         MH    R1,=H'20'                                                        
         AR    R3,R1                                                            
         L     R1,GCOST                                                         
         MVC   FULL,0(R3)                                                       
         A     R1,FULL                                                          
         ST    R1,GCOST                                                         
         EDIT (B4,0(R3)),(10,PICOST),COMMAS=YES                                 
GTBX     B     XIT                                                              
         EJECT                                                                  
*  FINAL GRAND TOTALS                                                           
*                                                                               
ENDTOTS  NTR1                                                                   
         LA    R4,GHHRTG                                                        
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         BAS   RE,WRITIT                         SKIP A LINE                    
         MVC   PIPROG(14),=C'** GRAND TOTAL'                                    
         MVI   P3+1,0                                                           
         MVC   PIPROG+399(5),=C'UNITS'                                          
         LA    R5,PIHHRTG-1                                                     
         EDIT  GHHRTG,(6,0(R5)),1                                               
         EDIT  GUNT,(3,PIUNT)                                                   
         EDIT  GCOST,(10,PICOST),COMMAS=YES                                     
         LA    R4,GRTGS                                                         
         ZIC   R5,NPERIODS                                                      
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         LA    R2,PICOST+11                                                     
         MVI   MYBYT,0                                                          
END5     CLI   MYBYT,0                   PUT RTG ON ALTERNATE LINES             
         BNE   END5B                                                            
         OC    64(4,R4),64(R4)                                                  
         BNZ   *+12                                                             
         MVI   3(R2),C'-'                                                       
         B     END5A                                                            
         EDIT  (B4,64(R4)),(5,0(R2)),1                                          
END5A    MVI   MYBYT,1                                                          
         B     END7                                                             
END5B    OC    64(4,R4),64(R4)                                                  
         BNZ   *+12                                                             
         MVI   135(R2),C'-'                                                     
         B     END6                                                             
         EDIT  (B4,64(R4)),(5,132(R2)),1                                        
END6     MVI   MYBYT,0                                                          
END7     OC    0(4,R4),0(R4)                                                    
         BNZ   *+12                                                             
         MVI   399(R2),C'-'                                                     
         B     END7B                                                            
         EDIT  (B4,0(R4)),(5,396(R2))                                           
END7B    LA    R2,5(R2)                                                         
         CLI   PPERTSV,C'W'                                                     
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,END5                                                          
         BAS   RE,WRITIT                                                        
ENDX     B     XIT                                                              
         EJECT                                                                  
*  LENGTH SUBTOTALS                                                             
*                                                                               
SUBTOTS  NTR1                                                                   
         LA    R4,LGTOTS                                                        
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         BAS   RE,WRITIT                         SKIP A LINE                    
         MVC   PIPROG(13),=C'** SUB TOTALS'                                     
SUB2     MVC   PIPROG+267(5),=C'UNITS'                                          
         EDIT  (B4,0(R4)),(3,PILEN)                                             
         EDIT  (B4,4(R4)),(5,PIHHRTG),1,COMMAS=YES                              
         EDIT  (B4,8(R4)),(3,PIUNT)                                             
         LA    R3,12(R4)                                                        
         ZIC   R5,NPERIODS                                                      
         LTR   R5,R5                                                            
         BZ    SUB9                                                             
         LA    R2,PICOST+11                                                     
SUB5     CLI   MYBYT,0                   PUT RTG ON ALTERNATE LINES             
         BNE   SUB5B                                                            
         OC    64(4,R3),64(R3)                                                  
         BNZ   *+12                                                             
         MVI   3(R2),C'-'                                                       
         B     SUB5A                                                            
         EDIT  (B4,64(R3)),(5,0(R2)),1                                          
SUB5A    MVI   MYBYT,1                                                          
         B     SUB7                                                             
SUB5B    OC    64(4,R3),64(R3)                                                  
         BNZ   *+12                                                             
         MVI   135(R2),C'-'                                                     
         B     SUB6                                                             
         EDIT  (B4,64(R3)),(5,132(R2)),1                                        
SUB6     MVI   MYBYT,0                                                          
SUB7     OC    0(4,R3),0(R3)                                                    
         BNZ   *+12                                                             
         MVI   267(R2),C'-'                                                     
         B     SUB7B                                                            
         EDIT  (B4,0(R3)),(5,264(R2))                                           
SUB7B    LA    R2,5(R2)                                                         
         CLI   PPERTSV,C'W'                                                     
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,SUB5                                                          
SUB9     BAS   RE,WRITIT                                                        
         LA    R2,P                                                             
         LA    R4,LGTOTLN(R4)                                                   
         OC    0(4,R4),0(R4)                                                    
         BNZ   SUB2                                                             
SUBX     B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVI   HEAD2+1,C'_'     UNDERLINE SYSTEM NAME                           
         MVC   HEAD2+2(21),HEAD2+1                                              
         MVC   WORK(40),TITLE      REPORT TITLE                                 
         OC    WORK,SPACES                                                      
         CLI   WORK,C' '                                                        
         BH    HOOK2                                                            
         SPACE 1                                                                
         CLI   QTITLE,0                                                         
         BE    *+14                                                             
         MVC   WORK(40),TITLE                                                   
         B     HOOK2                                                            
         MVC   WORK(40),=CL40'PRE EVALUATION FLOWCHART'                         
         SPACE 1                                                                
HOOK2    GOTO1 CENTER,DMCB,WORK,40                                              
         MVC   HEAD1+41(40),WORK                                                
         CLI   EQVOPT,C'Y'                                                      
         BNE   HOOK2A                                                           
         MVC   WORK(40),=CL40'(EQUIVALENCED)'                                   
         GOTO1 CENTER,DMCB,WORK,40                                              
         MVC   HEAD2+41(40),WORK                                                
         SPACE 1                                                                
HOOK2A   MVC   HEAD4+9(3),PUPCLI                                                
         MVC   HEAD4+15(20),PUPCLIN                                             
         CLI   NTPLNFLG,C'Y'                                                    
         BNE   HOOK2B                                                           
         MVC   H5+53(15),=C'PLAN       1988'                                    
         MVC   H5+1(7),=C'NETWORK'                                              
         MVC   HEAD5+9(4),PUPNET                                                
         MVC   HEAD5+58(5),PUPPLAN                                              
         LA    R2,HEAD5+64                                                      
         ZIC   R1,PLANYEAR                                                      
         AH    R1,=H'1900'          Y2K FIX                                     
         EDIT  (R1),(4,0(R2))                                                   
HOOK2B   MVC   HEAD4+62(7),PUPDPT                                               
         MVC   HEAD6+98(6),=C'TARGET'                                           
         CLI   DEMOVRID,C'Y'                                                    
         BE    HOOK2C                                                           
         MVC   HEAD6+105(10),TARGNAME                                           
         B     HOOK2F                                                           
HOOK2C   LA    R3,DEMSV                                                         
         GOTO1 VSETDB                                                           
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         GOTO1 DEMOCON,DMCB,(0,0(R3)),(10,WORK),(C'S',DBLOCK)                   
         MVC   HEAD6+105(11),WORK                                               
HOOK2F   LA    R2,HEAD9                                                         
         USING PLINED,R2                                                        
         MVC   PIPROG+132,=C'PROGRAM NAME----'                                  
         MVC   PIHHRTG,=C'TOTAL'                                                
         MVC   PIHHRTG+132(5),=C' RTG '                                         
         MVC   PIUNT(2),=C'NO'                                                  
         MVC   PIHHRTG+138(4),=C'UNTS'                                          
         MVC   PILEN+132,=C'LEN'                                                
         MVC   PICOST+3(4),=C'PLAN'                                             
         MVC   PICOST+135(4),=C'COST'                                           
         L     R3,APLST                                                         
         LA    R2,PIDATA                                                        
         ZIC   R5,NPERIODS                                                      
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         LA    R4,QUALPHA                                                       
         CLI   PPERTSV,C'Q'                                                     
         BE    HK3                                                              
         LA    R4,MOALPHA                                                       
HK2D     CLI   PPERTSV,C'W'                                                     
         BNE   HK3                                                              
         GOTO1 DATCON,DMCB,(2,0(R3)),(0,MYWORK)                                 
         MVC   2(2,R2),MYWORK+2                                                 
         MVC   134(2,R2),MYWORK+4                                               
         B     HK10                                                             
*                                                                               
HK3      ZIC   R1,0(R3)                                                         
         AH    R1,=H'1900'          Y2K FIX                                     
         EDIT  (R1),(4,132(R2))                                                 
                                                                                
***      EDIT  (B1,0(R3)),(4,132(R2))                                           
         MVC   1(3,R2),0(R4)                                                    
         LA    R4,3(R4)                                                         
HK10     LA    R2,5(R2)                                                         
         CLI   PPERTSV,C'W'                                                     
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,HK2D                                                          
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
         LA    R3,65(R3)                                                        
         MVI   0(R3),C'B'                                                       
         MVC   BOXCOLS,SPACES                                                   
         LA    R3,BOXCOLS                                                       
         USING PLINED,R3                                                        
         MVI   0(R3),C'L'                                                       
         LA    R3,PICOST+11                                                     
         ZIC   R5,NPERIODS                                                      
HK13     LA    R3,5(R3)                                                         
         CLI   PPERTSV,C'W'                                                     
         BE    *+8                                                              
         LA    R3,1(R3)                                                         
         BCT   R5,HK13                                                          
         MVI   0(R3),C'R'                                                       
HOOKX    B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 1                                                                
*                                                                               
CLEARBIN NTR1                                                                   
*                               * SET UP BINSRCH PARAMETERS                     
         SR    R0,R0               A OF REC TO BE ADDED                         
         LA    R1,BUFF             A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,BINRLENE         LENGTH OF REC                                
         LA    R4,BINKLENE         DISP OF KEY INTO REC                         
         L     R5,=F'100'        MAX RECS IN BINTBL                             
         STM   R0,R5,BINDMCB                                                    
*                                                                               
         LA    RE,BUFF          CLEAR BINTABLE                                  
         L     RF,=F'3100'                                                      
         XCEF                                                                   
         B     XIT                                                              
*                                                                               
         SPACE 3                                                                
WRITIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
SETEQV   CLI   N2PROF,0                                                         
         BNE   *+8                                                              
         MVI   N2PROF,30                                                        
         CLI   N0PROF+1,0                                                       
         BNE   *+8                                                              
         MVI   N0PROF,30                                                        
         MVI   GDRAWOPT,0                                                       
         BR    RE                                                               
*                                                                               
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
QUALPHA  DC    C'Q4 Q1 Q2 Q3 '                                                  
MOALPHA  DC    C'SEPOCTNOVDECJANFEBMARAPRMAYJUNJULAUGSEP'                       
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
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPD7D                                                       
         EJECT                                                                  
         SPACE 1                                                                
*                             WORK AREA                                         
WORKD    DS    0CL1                                                             
DATA     DS    CL1                                                              
REQLEN   DS    CL1                                                              
REQOPT   DS    CL1                                                              
EQVOPT   DS    CL1                                                              
LGOPT    DS    CL1                                                              
MYBYT    DS    CL1                                                              
NTPLNFLG DS    CL1              N=MULT PLAN/NO NET/PLAN IN HEADS                
RELO     DS    F                                                                
BINDMCB  DS    6F                                                               
APLST    DS    F                   ADDRESS OF PLANPLST                          
NPERIODS DS    CL1                 PLANNPER                                     
MYHALF   DS    CL2                                                              
MYWORK   DS    CL20                                                             
TITLE    DS    CL40                                                             
QTITLE   DS    CL1                                                              
DEMOVRID DS    CL1                                                              
PPERTSV  DS    CL1                                                              
DEMSV    DS    CL3                                                              
PCODESV  DS    CL3                                                              
         DS    0F                                                               
LINTOTS  DS    0CL136                                                           
LHHRTG   DS    F                                                                
LUNT     DS    F                                                                
LWEEKS1  DS    16F                                                              
LWEEKS2  DS    16F                                                              
*                                                                               
PLANTOT  DS    0CL136                                                           
PHHRTG   DS    F                                                                
PUNT     DS    F                                                                
PLANRTG  DS    16F                                                              
PLANUNT  DS    16F                                                              
*                                                                               
GRANDTOT DS    0CL140                                                           
GHHRTG   DS    F                                                                
GUNT     DS    F                                                                
GRTGS    DS    16F                                                              
GUNTS    DS    16F                                                              
GCOST    DS    F                                                                
*                                                                               
LGTOTS   DS    0F                  TOTALS BY SPOT LENGTH                        
LGLENTH  DS    F                   ROOM FOR 10 SPOT-LENGTH TOTS                 
LGHHRTG  DS    F                                                                
LGUNT    DS    F                                                                
LGWEEK1  DS    16F                                                              
LGWEEK2  DS    16F                                                              
LGTOTLN  EQU   *-LGTOTS                                                         
         DS    CL1400                                                           
WORKDEND EQU   *                                                                
*                                                                               
         SPACE 3                                                                
*                                                                               
BIND     DSECT                  DSECT FOR BINSRCH RECORDS                       
BINDAY   DS    CL1                                                              
BINTIME  DS    CL4                                                              
BINPROG  DS    CL6                                                              
BINKLENE EQU   *-BINDAY                                                         
BINDATA  DS    CL20                PUP PROGRAM REC KEY                          
BINRLENE EQU   *-BINDAY                                                         
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
         DS    CL1                                                              
PIPROG   DS    CL16                                                             
         DS    CL1                                                              
PIHHRTG  DS    CL5                                                              
         DS    CL2                                                              
PIUNT    DS    CL3                                                              
         DS    CL1                                                              
PILEN    DS    CL3                                                              
         DS    CL2                                                              
PICOST   DS    CL10                                                             
         DS    CL2                                                              
PIDATA   DS    CL4                                                              
         DS    CL1                                                              
         DS    CL75                                                             
*                                                                               
*                                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034NEPUP27   05/01/02'                                      
         END                                                                    
