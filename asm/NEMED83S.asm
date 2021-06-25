*          DATA SET NEMED83S   AT LEVEL 174 AS OF 05/01/02                      
*PHASE T31E83A                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'T31E83 - WEEKLY GOAL PERFORMANCE'                               
T31E83   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PRBY**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         LA    R8,4095(RB)                                                      
         LA    R8,1(R8)                                                         
         USING T31E83+4096,R8                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2                                                       
         LA    R7,500(R7)                                                       
         USING WORKD,R7                                                         
         L     R6,ANETWS3                                                       
         USING NDDEMBLK,R6         R6(ANETWS3)-NDDEMBLK,DEDBLOCK                
         ST    R6,NBADEM                                                        
         ST    R2,RELO                                                          
         EJECT                                                                  
*HIPO******************************************************************         
*  TITLE: NEMED83 (T31E83) PRE-BUY GOAL REPORT                        *         
*                                                                     *         
*  COMMENTS: WRITES A REPORT THAT COMPARES EST GRPS FROM GOAL REC     *         
*            TO EST GRPS FROM UNIT REC                                *         
*                                                                     *         
*  CALLS TO: NETIO,NETGOAL                                            *         
*                                                                     *         
*                                                                     *         
*  LOCALS: ANETWS4 TO END USED FOR RECORD LIST                        *         
*          ANETWS2+500 USED FOR MYWORKD                               *         
*          R7-MYWORKD                                                 *         
*                                                                     *         
***********************************************************************         
*  LOGIC:  FIRST, PRGM GETS WEEKLIST THROUGH NVWKLST                  *         
*          AND SETS UP WORKRECS USING THOSE DATES                     *         
*                                                                     *         
*          SECOND, READS GOAL RECS THROUGH NETGOAL AND ADDS GOAL      *         
*          DATA TO WORKRECS USING NGOALWK(START OF GOAL WEEK)         *         
*                                                                     *         
*          THIRD, READS UNIT RECS THROUGH NETIO AND ADDS UNIT         *         
*          DATA TO WORKRECS.                                          *         
*                                                                     *         
*                                                                     *         
*ENDHIPO***************************************************************         
         SPACE 3                                                                
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY *                                                                
         SPACE                                                                  
VK       DS    0H                                                               
         L     RF,=A(VALRTN)                                                    
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
         B     EXIT                                                             
         SPACE                                                                  
         EJECT                                                                  
         SPACE                                                                  
* LIST RECORDS *                                                                
LR       DS    0H                                                               
*                                      GET WEEKLIST                             
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'          READ IN DATE SEQUENCE                        
         MVI   NBRESUME,NBPROCPK                                                
         MVI   NBHUNOPT,C'Y'       PROGRAM DEALS IN HUNDREDS                    
         OI    NBSPLOPT,X'80'      AND PIGGYBACKS                               
         SPACE 1                                                                
LR02     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALDAT                                                  
         BNE   LR02                                                             
         MVI   PERTYPE,C'W'        WEEKS ONLY                                   
         LA    R3,53               FOR 53 WEEKS MAX                             
         ST    R3,NUMPER                                                        
         L     R3,ANETWS1          WKLIST IN ANETWS1                            
         NETGO NVWKLST,DMCB,NUMPER,(R3),PERTYPE                                 
         SPACE                                                                  
* NOW USE WEEKLIST DATES TO SET UP WORKRECDS                                    
         L     R2,NUMPER           NUMPER HAS NUM OF PERIODS RETURNED           
         ST    R2,BINDMCB+8        SET FOR BINSRCH                              
         L     R3,ANETWS4          PUT REC INTO ANETWS4                         
         USING WORKRECD,R3                                                      
         L     R4,ANETWS1          HAS WEEKLIST                                 
LR04     MVC   WSTRT(4),0(R4)      MOVE STRT + END DATE                         
         LA    R4,4(R4)                                                         
         LA    R3,WRECLENE(R3)                                                  
         BCT   R2,LR04                                                          
         DROP  R3                                                               
         EJECT                                                                  
*              HANDLE GOAL RECORDS (NETGOAL)                                    
         SPACE 3                                                                
         L     R5,ANETWS1                                                       
         USING NETGOALD,R5                                                      
         XC    0(150,R5),0(R5)     AT3/27/85 BLOCK=CL100                        
         LA    R2,NETBLOCK                                                      
         ST    R2,NGANTBLK                                                      
         L     R2,ANETWS1          LIST=100 MAX (3/27/85)                       
         LA    R2,150(R2)                                                       
         ST    R2,NGAPLIST                                                      
         LA    R2,NETGOLHK                                                      
         ST    R2,NGAHOOK                                                       
         L     R2,ANETWS2          USING R2 AS WORKAREA                         
         USING WORKRECD,R2                                                      
         XC    0(WRECLENE,R2),0(R2)                                             
***      MVC   NGSELSL,SPOTLEN                                                  
         MVC   NGSELSL,NBSELLEN                                                 
         GOTO1 ANETGOAL,DMCB,NGBLOCK                                            
         B     LR05                                                             
         SPACE                                                                  
NETGOLHK NTR1                                                                   
         CLI   PRDFLG,C'Y'         IS IT FOR SPECIFIC PRD                       
         BNE   NGL5                                                             
         OC    NGOALPRD,=3X'404040'                                             
         CLC   NGOALPRD,NBSELPRD     YES/CHK IT                                 
         BNE   NGLHKX                                                           
NGL5     MVC   WSTRT,NGOALWK                                                    
         L     R2,ANETWS4          /BUT TEST IF DATES ALREADY THERE             
NGL5A    CLC   NGOALWK,6(R2)       TEST AGAINST END                             
         BH    NGL6                                                             
         CLC   NGOALWK,4(R2)       AND START                                    
         BL    NGL6A               NOT THERE/GO TO BINSRCH                      
         L     R4,12(R2)         GOAL WEEK WITHIN START/END DATES               
         A     R4,NGOALGRP             SO ADD TO THIS WEEK                      
         ST    R4,12(R2)                                                        
         B     NGLHKX                                                           
NGL6     LA    R2,WRECLENE(R2)     BUMP TO NEXT WEEK IN TABLE                   
         OC    0(6,R2),0(R2)       IF NO MORE, ADD WITH BINSRCH                 
         BNZ   NGL5A                                                            
*                                                                               
NGL6A    L     R2,ANETWS2          RESTORE R2                                   
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R2)),RR=RELO                             
         CLI   0(R1),X'01'         TEST IF REC FOUND(X'01'=NOT FOUND)           
         BNE   NGL7                                                             
         L     R3,0(R1)                                                         
         MVC   12(4,R3),NGOALGRP   NOT FOUND / MOVE IN GRP                      
*                                                                               
NGL7     L     R3,0(R1)            REC FOUND/ADD TO GOALS                       
         L     R4,12(R3)                                                        
         A     R4,NGOALGRP                                                      
         ST    R4,12(R3)                                                        
NGLHKX   B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*              NOW READ UNIT RECORDS                                            
         SPACE 3                                                                
LR05     LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         L     R3,ANETWS4                                                       
         USING WORKRECD,R3                                                      
         MVI   NBSELUOP,C'A'       ACTUAL SCHEDULE                              
         NETGO NVDEMOPT,DMCB       RETURN EST DEMOS FOR MAKE GOOD               
*                                  BUT NOT FOR PFB                              
         CLI   ACTFLG,C'Y'         IS IS ACTUAL DEMOS                           
         BNE   *+8                                                              
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBSPLOPT,X'C0'      TURN ON SPLIT OPTION                         
         SPACE                                                                  
LR06     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    LREXIT                                                           
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LR06                                                             
         SPACE                                                                  
LR07     CLC   NBACTDAT,WEND       IS AIRDATE WITHIN WEEK DATES                 
         BH    LR10                                                             
         CLC   NBACTDAT,WSTRT                                                   
         BNL   *+6                                                              
         DC    H'0'               (UNIT REC CAN'T BE LESS THAN WSTRT)           
* GET DEMO                                                                      
         LA    R4,NDESTDEM                                                      
         CLI   ACTFLG,C'Y'        IS IT ACT DEMO OPTION                         
         BNE   *+8                                                              
         LA    R4,NDACTDEM         YES                                          
         SR    R1,R1                                                            
         ICM   R1,3,2(R4)          ADD TO GRP TOTS                              
*        CLI   NDDEMOS+1,C'R'      IS IT GRPS    FORCE TARGET TO RTG            
*        BE    *+8                                                              
*        ICM   R1,15,4(R4)          NO/USE IMPS                                 
         A     R1,WEST                                                          
         ST    R1,WEST                                                          
         ICM   R1,15,NBACTUAL      ADD TO UNIT COST                             
         A     R1,WCOST                                                         
         ST    R1,WCOST                                                         
         ZIC   R1,WUNITS           ADD TO NUMBER OF UNITS                       
         LA    R1,1(R1)                                                         
         STC   R1,WUNITS                                                        
         SPACE                                                                  
         BAS   RE,ADDGRPS                                                       
         B     LR06                GET NXT UNT REC                              
         SPACE                                                                  
LR10     DS    0H            LINE FOR 1 WEEK COMPLETED                          
         BAS   RE,SETPRTLN                                                      
         BAS   RE,PRINTIT                                                       
         LA    R3,WRECLENE(R3)     BUMP TO NEXT REC                             
         B     LR07                                                             
         SPACE 2                                                                
LREXIT   DS    0H                                                               
         OC    WSTRT,WSTRT         IS THERE A REC TO PRINT                      
         BZ    LRX5                IF NOT/SKIP TO TABLEALL ROUTINE              
         BAS   RE,SETPRTLN                                                      
         BAS   RE,PRINTIT                                                       
         LA    R3,WRECLENE(R3)     BUMP TO NEXT REC                             
LRX5     BAS   RE,TABLEALL                                                      
         BAS   RE,FINLTOTS                                                      
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************                                  
*  AFTER LAST UNIT REC THIS ROUTINE STEPS    *                                  
*  DOWN BINTABLE AND PRINTS ANY REMAINING    *                                  
*  BINRECS(THESE WOULD BE GOAL RECS WITH NO  *                                  
*  MATCHING UNITS).                          *                                  
*                                            *                                  
*  INPUT R3-POINTS TO BINREC                 *                                  
**********************************************                                  
         SPACE                                                                  
TABLEALL NTR1                                                                   
         USING WORKRECD,R3                                                      
TBL5     CLI   WEND,0                                                           
         BE    TBLX                                                             
         BAS   RE,SETPRTLN                                                      
         BAS   RE,PRINTIT                                                       
         LA    R3,WRECLENE(R3)                                                  
         B     TBL5                                                             
TBLX     B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
******************************                                                  
* SET UP PRINT LINE          *                                                  
* ADD TO  TOTALS/WRITE LINE  *                                                  
* R3 POINTS TO WORKRECD-     *                                                  
* -DO NOT USE                *                                                  
******************************                                                  
SETPRTLN NTR1                                                                   
         USING WORKRECD,R3                                                      
         LA    R2,P                                                             
         USING LISTD,R2                                                         
         GOTO1 DATCON,DMCB,(2,WSTRT),(4,LDATES)                                 
         MVI   LDATES+5,C'-'                                                    
         GOTO1 DATCON,DMCB,(2,WEND),(4,LDATES+6)                                
         EDIT  WUNITS,(5,LUNITS)                                                
         EDIT  WGOAL,(8,LGOAL),1                                                
         CLI   NDDEMOS+1,C'I'                                                   
         BE    SET2                                                             
         EDIT  WEST,(8,LEST),1   GRP HAS 1 DEC                                  
         B     SET2A                                                            
SET2     NETGO NVPRDEM,DMCB,(C'I',0),WEST,LEST+1                                
SET2A    EDIT  WCOST,(11,LCOST),2                                               
         LA    R5,LRTG1            GRP PRINT OUT FIELD                          
         LA    R4,5                BCT GRP LIMIT                                
         LA    RE,WRPGS            GRP WORKREC FIELD                            
         LA    RF,NDDEMOS+3                                                     
SET3     L     R1,0(RE)                                                         
         LTR   R1,R1                                                            
         BZ    SET3A                                                            
         CLI   1(RF),C'I'                                                       
         BE    SET3AA                                                           
         EDIT  (R1),(8,(R5)),1                                                  
         B     SET3A                                                            
SET3AA   STM   RE,RF,SAVEREF                                                    
         MVC   DUB(4),0(RE)                                                     
         NETGO NVPRDEM,DMCB,(C'I',0),DUB,1(R5)                                  
         LM    RE,RF,SAVEREF                                                    
SET3A    LA    R5,10(R5)                                                        
         LA    RE,4(RE)                                                         
         LA    RF,3(RF)            BUMP DEMO CODES                              
         BCT   R4,SET3                                                          
         SPACE                                                                  
         LA    R1,WRPGS            ADD UP GRP TOTS                              
         LA    R4,GRPTOTS                                                       
         LA    R5,5                BCT GRP LIMIT                                
SET4     L     R0,0(R1)                                                         
         A     R0,0(R4)                                                         
         ST    R0,0(R4)                                                         
         LA    R1,4(R1)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SET4                                                          
         SPACE                                                                  
         L     R1,WGOAL            ADD TO GOALTOT                               
         A     R1,GOALTOT                                                       
         ST    R1,GOALTOT                                                       
         L     R1,WEST             ADD TO ESTTOT                                
         A     R1,ESTTOT                                                        
         ST    R1,ESTTOT                                                        
         L     R1,WCOST            ADD TO UNIT COST TOT                         
         A     R1,COSTTOT                                                       
         ST    R1,COSTTOT                                                       
         ZIC   R1,WUNITS           ADD TO UNIT TOTAL                            
         A     R1,UNITTOT                                                       
         ST    R1,UNITTOT                                                       
         SPACE                                                                  
         L     R4,WEST             EST-GOAL=DIFF                                
         S     R4,WGOAL                                                         
         L     R5,DIFFTOT          ADD TO DIFFTOT                               
         AR    R5,R4                                                            
         ST    R5,DIFFTOT                                                       
         EDIT  (R4),(8,LDIFF),1,FLOAT=-   PUT DIFF TO PRINT LINE                
         SPACE                                                                  
         SR    R5,R5                                                            
         OC    WGOAL,WGOAL                                                      
         BZ    SET5                                                             
         L     R5,WEST            EST/GOAL=INDEX                                
         M     R4,=F'1000'                                                      
         D     R4,WGOAL                                                         
SET5     EDIT  (R5),(8,LINDEX),1        PUT INDEX TO PRINT LINE                 
         SPACE                                                                  
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
FINLTOTS NTR1                                                                   
         LA    R2,P                                                             
         USING LISTD,R2                                                         
         BAS   RE,PRINTIT                                                       
         MVC   P(13),=C'*** TOTAL ***'                                          
         CLI   CPPMFLG,C'Y'         IS CPM/CPP OPTION ON                        
         BNE   FINL1A                                                           
         MVI   P2,0                                                             
         MVC   P3+26(7),=C'CPP/CPM'                                             
FINL1A   CLI   NDDEMOS+1,C'R'                                                   
**       BE    FINL1                                                            
**       NETGO NVPRDEM,DMCB,(C'I',0),ESTTOT,LEST+1  (IMPS)                      
**       B     FINL1B                                                           
FINL1    EDIT  ESTTOT,(8,LEST),1                                                
FINL1B   CLI   CPPMFLG,C'Y'         IS CPM/CPP OPTION ON                        
         BNE   FINL2                                                            
         L     R5,COSTTOT                                                       
         LTR   R5,R5                                                            
         BZ    FINL2                                                            
         L     R1,ESTTOT                                                        
         LTR   R1,R1                                                            
         BZ    FINL2                                                            
         LA    R4,1                                                             
         CLI   NDDEMOS+1,C'R'                                                   
         BE    *+8                                                              
         LA    R4,10                                                            
         MR    R4,R4                                                            
         D     R4,ESTTOT                                                        
         LA    R4,LEST+132                                                      
         LA    R4,132(R4)                                                       
         EDIT  (R5),(8,(R4)),2,FLOAT=$     CPP/CPM                              
FINL2    EDIT  GOALTOT,(8,LGOAL),1                                              
FINL2A   CLI   SPLCPT,C'Y'               IS CPM/CPP OPTION ON                   
         BNE   FINL2B                                                           
         OC    GOALTOT,GOALTOT           IF NO GOALS                            
         BZ    FINL3                        THEN SKIP CPP AND INDEX             
         L     R5,COSTTOT                                                       
         LTR   R5,R5                                                            
         BZ    FINL2B                                                           
         M     R4,=F'1'                                                         
         D     R4,GOALTOT                                                       
         LA    R4,LGOAL+132                                                     
         LA    R4,132(R4)                                                       
         EDIT  (R5),(8,(R4)),2,FLOAT=$     CPP/CPM                              
FINL2B   EDIT  COSTTOT,(11,LCOST),2                                             
         EDIT  DIFFTOT,(8,LDIFF),1,FLOAT=-                                      
         OC    GOALTOT,GOALTOT                                                  
         BZ    FINL3                                                            
         L     R5,ESTTOT          EST/GOAL=INDEX                                
         LTR   R5,R5                                                            
         BZ    FINL3                                                            
         M     R4,=F'1000'                                                      
         D     R4,GOALTOT                                                       
         EDIT  (R5),(8,LINDEX),1                                                
FINL3    EDIT  UNITTOT,(5,LUNITS)                                               
         LA    R3,GRPTOTS                                                       
         LA    R5,NDDEMOS+3                                                     
         LA    R2,LRTG1                                                         
         XC    FULL,FULL                                                        
FINL5    L     R1,0(R3)                                                         
         LTR   R1,R1                                                            
         BZ    FINL5A                                                           
         CLI   1(R5),C'R'                                                       
         BE    FINL5AA                                                          
         NETGO NVPRDEM,DMCB,(C'I',0),0(R3),1(R2)   (IMPS)                       
         B     FINL5AB                                                          
FINL5AA  EDIT  (R1),(8,0(R2)),1                                                 
FINL5AB  L     RF,COSTTOT                                                       
         LTR   RF,RF                                                            
         BZ    FINL5A                                                           
         L     R1,0(R3)                                                         
         LTR   R1,R1                                                            
         BZ    FINL5A                                                           
         LA    RE,1                                                             
         CLI   1(R5),C'R'                                                       
         BE    *+8                                                              
         LA    RE,10                                                            
         MR    RE,RE                                                            
         D     RE,0(R3)                                                         
         LA    RE,132(R2)                                                       
         LA    RE,132(RE)                                                       
         CLI   SPLCPT,C'Y'         IS CPM/CPP OPT ON                            
         BNE   FINL5A                                                           
         EDIT  (RF),(8,0(RE)),2,FLOAT=$                                         
FINL5A   LA    R3,4(R3)                                                         
         LA    R2,10(R2)                                                        
         LA    R5,3(R5)                                                         
         L     R1,FULL                                                          
         LA    R1,1(R1)                                                         
         ST    R1,FULL                                                          
         C     R1,=F'5'                                                         
         BNE   FINL5                                                            
         BAS   RE,PRINTIT                                                       
FNLX     B     EXIT                                                             
         EJECT                                                                  
*************************************                                           
* ROUTINE ADDS GRPS TO WORKREC      *                                           
*  FOR DEMOS AFTER TARGET DEMO      *                                           
*************************************                                           
ADDGRPS  NTR1                                                                   
         CLI   NDNDEMOS,0                                                       
         BE    ADX                                                              
         LA    R2,5                BCT DEMO LIMIT                               
         CLI   ACTFLG,C'Y'         IS IT ACT DEMOS                              
         BE    *+12                                                             
         LA    R1,NDESTDEM+8          USE EST DEMOS                             
         B     *+8                                                              
         LA    R1,NDACTDEM+8          USE ACT DEMOS                             
         LA    R3,WRPGS            (1ST SET IN EST FIELD)                       
         LA    R5,NDDEMOS+3                                                     
AD5      XC    FULL,FULL                                                        
         MVC   FULL+2(2),2(R1)     SET GRPS                                     
         CLI   1(R5),C'R'          IS IT GRPS                                   
         BE    *+10                                                             
         MVC   FULL,4(R1)          NO/USE IMPS                                  
         L     R4,0(R3)                                                         
         A     R4,FULL                                                          
         ST    R4,0(R3)                                                         
         LA    R1,8(R1)            BUMP DEMOS                                   
         LA    R5,3(R5)            BUMP DEMO CODES                              
         LA    R3,4(R3)            BUMP DEMO TOTAL FIELD                        
         BCT   R2,AD5                                                           
ADX      B     EXIT                                                             
         EJECT                                                                  
HDRTN    NTR1                                                                   
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+15(20),SPLCLIN                                                
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),SPLPRO                                                  
         MVC   H4+15(20),SPLPRON                                                
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(8),SPLEST                                                  
         CLI   NBSELEST,0          SHOW ESTIMATE NAME IF ONLY ONE               
         BE    HDHK1                                                            
         CLI   NBSELESE,0                                                       
         BNE   HDHK1                                                            
         MVC   H5+15(20),SPLESTN                                                
         SPACE 1                                                                
HDHK1    MVC   H6(11),=C'SPOT LENGTH'                                           
         LA    R3,H6+15                                                         
         CLI   NBSELLEN,0                                                       
         BNE   *+14                                                             
         MVC   H6+15(3),=C'ALL'                                                 
         B     SPT5                                                             
         ZIC   R2,NBSELLEN                                                      
         EDIT  (R2),(2,(R3))                                                    
         SPACE                                                                  
SPT5     LA    R2,H10                                                           
         USING LISTD,R2                                                         
         MVC   LDATES(5),=C'DATES'                                              
         MVC   LUNITS(5),=C'UNITS'                                              
         MVC   LCOST+7(4),=C'COST'                                              
         MVC   LEST-1(18),=C'----------------'                                  
         MVC   LDIFF+4(14),=C'--------------'                                   
         LA    R2,H11                                                           
         MVC   LEST+5(3),=C'EST'                                                
         CLI   ACTFLG,C'Y'                                                      
         BNE   *+10                                                             
         MVC   LEST+5(3),=C'ACT'                                                
         MVC   LGOAL+4(4),=C'GOAL'                                              
         MVC   LDIFF+4(4),=C'DIFF'                                              
         MVC   LINDEX+3(5),=C'INDEX'                                            
         SPACE                                                                  
* GET DEMO NAMES TO HEADER *                                                    
         CLI   BOXSET,C'Y'                                                      
         BNE   HDHK2                                                            
         MVC   H10,HEADSV          SET HEADLINES                                
         MVC   H11,HEADSV2                                                      
         B     HDBOXES                                                          
HDHK2    MVC   DBCOMFCS,ACOMFACS   *SET FOR DEMOCON                             
         MVC   DBFILE,=C'NTI'      *                                            
         MVI   DBSELMED,C'N'       *                                            
         MVI   DBSELSRC,C'N'       *                                            
         SPACE                                                                  
         LA    R3,H10                                                           
         USING LISTD,R3                                                         
         LA    R5,6                BCT LIMIT FOR DEMO CATEGORIES                
         SR    R2,R2               COUNTER FOR DEMO NUMBER(POSITION)            
         SPACE                                                                  
*        LA    R1,5                *SET FOR VPHS IN NDDEMOS                     
*        LA    R6,NDDEMOS          *                                            
* HDHK05   MVI   1(R6),C'V'          *                                          
*        LA    R6,3(R6)            *                                            
*        BCT   R1,HDHK05           *                                            
         SPACE                                                                  
GETDEMO  NETGO NVDEMCON,DMCB,((R2),NDDEMBLK),DBLOCK,(7,WORK)                    
         CH    R5,=H'6'            IS IT FIRST DEMO                             
         BNE   HD3                                                              
         LA    R3,LEST+5          SET PRINT LINE FOR GOAL DEMO                  
         MVC   0(12,R3),WORK                                                    
***      CLI   WORK+7,C'*'        IS IT IMPS                                    
***      BNE   *+10                                                             
         MVC   7(7,R3),=C' (RTG) '                                              
         LA    R3,H10              RESET R3 TO START OF PRINT LINE              
         LA    R3,LRTG1-9          SET FOR REMAINING DEMOS LOOP                 
         B     HD5                                                              
         SPACE                                                                  
HD3      MVC   0(7,R3),WORK                                                     
         MVC   133(5,R3),WORK+7                                                 
         CLI   WORK+7,C'*'                                                      
         BNE   *+10                                                             
         MVC   132(6,R3),=C'(IMPS)'                                             
HD5      LA    R2,1(R2)                                                         
         LA    R3,10(R3)                                                        
         BCT   R5,GETDEMO                                                       
         MVC   HEADSV,H10          SAVE HEADLINES                               
         MVC   HEADSV2,H11                                                      
         SPACE                                                                  
*  SET UP BOXES PARAMETERS *                                                    
         SPACE                                                                  
HDBOXES  CLI   BOXSET,C'Y'                                                      
         BE    HDX                                                              
         MVI   BOXSET,C'Y'                                                      
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         MVI   0(R5),C'L'                                                       
         LA    R5,13(R5)           DATES                                        
         MVI   0(R5),C'C'                                                       
         LA    R5,7(R5)            UNITS                                        
         MVI   0(R5),C'C'                                                       
         LA    R5,13(R5)           COST                                         
         MVI   0(R5),C'C'                                                       
         LA    R5,10(R5)           ESTIMATED                                    
         MVI   0(R5),C'C'                                                       
         LA    R5,10(R5)           GOAL GRP                                     
         MVI   0(R5),C'C'                                                       
         LA    R5,10(R5)            DIFFERENCE                                  
         MVI   0(R5),C'C'                                                       
         LA    R5,10(R5)           INDEX                                        
         MVI   0(R5),C'C'                                                       
         LA    R5,10(R5)           GRP                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,10(R5)           GRP                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,10(R5)           GRP                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,10(R5)           GRP                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,10(R5)           GRP                                          
         MVI   0(R5),C'R'                                                       
         SPACE                                                                  
         LA    R5,BOXROWS                                                       
         LA    R5,8(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,3(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,46(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,55,C'WEEKLY GOAL PERFORMANCE'                                 
         SSPEC H2,55,C'-----------------------'                                 
         SSPEC H3,53,PERIOD                                                     
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
VALRTN   NMOD1 0,VALRTN                                                         
         L     RC,0(R1)                                                         
* GET NETGOAL ADDRS                                                             
         GOTO1 NBCALLOV,DMCB,0,X'D9000A35'                                      
         L     RF,DMCB             PICK UP ADDRESS OF NETGOAL                   
         ST    RF,ANETGOAL                                                      
         SPACE                                                                  
* SET UP BINSRCH PARAMETERS                                                     
         SR    R0,R0               A OF REC TO BE ADDED                         
         L     R1,ANETWS4          A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,WRECLENE         LENGTH OF REC                                
         LA    R4,WKEYLENE         DISP OF KEY INTO REC                         
         LA    R5,55               MAX RECS IN BINTBL                           
         STM   R0,R5,BINDMCB                                                    
         L     RE,ANETWS4          CLEAR BINTBL AREA                            
         LA    RF,4000                                                          
         LA    RF,1000(RF)                                                      
         XCEF                                                                   
         SPACE                                                                  
* VALIDATE SCREEN INPUT FIELDS                                                  
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
         LA    R2,SPLCLIH                                                       
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
         MVI   FTERMFLG,1               SET OPTIONAL FLAG                       
         LA    R2,SPLPROH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK10                                                             
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         CLC   8(3,R2),=C'POL'                                                  
         BE    VK10                                                             
         MVI   PRDFLG,C'Y'                                                      
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
VK10     LA    R2,SPLESTH                                                       
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         CLC   NDDEMOS(3),=3X'00'          TEST IF ANY DEMOS                    
         BNE   VK12                   IF NOT/MEANS RANGE                        
         L     R3,NBAIO               SO GET THEM                               
         USING ESTHDR,R3                                                        
         MVC   NDDEMOS,EDEMLST                                                  
         SPACE 1                                                                
VK12     OI    SPLESTNH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
         LA    R2,SPLNETH                                                       
         NETGO NVNET,DMCB,SPLNETN                                               
         OI    SPLNETNH+6,X'80'                                                 
*                                                                               
VK20     LA    R2,SPLPAKH                                                       
         NETGO NVPAK,DMCB,SPLPAKN                                               
         OI    SPLPAKNH+6,X'80'                                                 
*                                                                               
VK30     DS    0H                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPTALL,DMCB                                                    
*                                                                               
VK40     LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
VK50     LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB                                                    
         MVC   DEMSV,NDDEMOS       SHOULD NOW HAVE THE TARGET                   
*                                                                               
VK60     LA    R2,SPLDEMH                                                       
         NETGO NVGETFLD,DMCB       ARE THERE OVERRIDE DEMOS                     
         BZ    VK70                                                             
         LA    R3,DBLOCK           YES/                                         
         LA    R4,NDDEMBLK                                                      
         NETGO NVDEM,DMCB,(R3),(R4)    GET THEM                                 
         MVC   WORK,NDDEMOS                                                     
         MVC   NDDEMOS(3),DEMSV        SET SAVED TARGET DEM FIRST               
         MVC   NDDEMOS+3,WORK          THEN SET OVERRIDE DEMOS                  
         SPACE                                                                  
VK70     LA    R2,SPLIMPH            IS DEM OPTION SET                          
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK80                                                             
         LA    R3,NDDEMOS+3                                                     
         LA    R4,5                                                             
         CLI   8(R2),C'I'                                                       
         BNE   VK75                                                             
VK72     MVI   1(R3),C'I'   SET ALL DEMS AFTER 1ST FOR IMP                      
         LA    R3,3(R3)                                                         
         BCT   R4,VK72                                                          
         B     VK80                                                             
VK75     CLI   8(R2),C'R'                                                       
         BE    VK77                                                             
         MVI   ERROR,INVALID       IF NOT I/R GOTO ERROR                        
         B     TRAPERR                                                          
VK77     MVI   1(R3),C'R'   SET ALL DEMS AFTER 1ST FOR RTG                      
         LA    R3,3(R3)                                                         
         BCT   R4,VK75                                                          
         SPACE                                                                  
VK80     LA    R2,SPLCPTH                                                       
         CLI   8(R2),C'Y'          IS IT CPP/CPM OPTION                         
         BNE   VK85                                                             
         MVI   CPPMFLG,C'Y'                                                     
         SPACE                                                                  
VK85     LA    R2,SPLACTH          IS ACTUALS OPTION ON                         
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK90                                                             
         CLI   8(R2),C'Y'                                                       
         BE    VK87                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
VK87     MVI   ACTFLG,C'Y'                                                      
         SPACE                                                                  
VK90     DS    0H                   FOR FUTURE FIELDS                           
         SPACE                                                                  
VKEXIT   DS    0H                                                               
         XIT1                                                                   
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
*                                                                               
*                                                                               
         LTORG                                                                  
       EJECT                                                                    
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2+500                     
         DS    0F                                                               
BINDMCB  DS    6F                                                               
ANETGOAL DS    V                                                                
*                                                                               
RELO     DS    F                                                                
SAVEREF  DS    D                                                                
ESTTOT   DS    F                                                                
GOALTOT  DS    F                                                                
COSTTOT  DS    F                                                                
DIFFTOT  DS    F                                                                
UNITTOT  DS    F                                                                
GRPTOTS  DS    5F                                                               
NUMPER   DS    F                                                                
         DS    0D                                                               
PAKWRK   DS    CL16                                                             
PERTYPE  DS    CL3                                                              
DEMSV    DS    CL3                                                              
*                                                                               
WRKDAT   DS    CL6                                                              
WRKDAT2  DS    CL6                                                              
PRDFLG   DS    CL1                                                              
CPPMFLG  DS    CL1                                                              
ACTFLG   DS    CL1                                                              
BOXSET   DS    CL1                                                              
SPOTLEN  DS    CL1                                                              
HEADSV   DS    CL132                                                            
HEADSV2  DS    CL132                                                            
         SPACE 2                                                                
*                                                                               
WORKRECD DSECT             *** WPRDN AND WESTN NOT USED AT PRESENT              
         DS    0F          *** REPORT GOES ACROSS PRD AND EST                   
WPRDN    DS    CL3                                                              
WESTN    DS    CL1                                                              
WSTRT    DS    CL2                                                              
WKEYLENE EQU   *-WPRDN                                                          
WEND     DS    CL2                                                              
WCOST    DS    F                                                                
WGOAL    DS    F                                                                
WEST     DS    F                                                                
WRPGS    DS    6F                                                               
WUNITS   DS    CL1                                                              
WRECLENE EQU   *-WPRDN                                                          
         SPACE 2                                                                
*                                                                               
LISTD    DSECT                                                                  
         DS    CL2                                                              
LDATES   DS    CL11                                                             
         DS    CL2                                                              
LUNITS   DS    CL5                                                              
         DS    CL2                                                              
LCOST    DS    CL11                                                             
         DS    CL2                                                              
LGOAL    DS    CL8                                                              
         DS    CL2                                                              
LEST     DS    CL8                                                              
         DS    CL2                                                              
LDIFF    DS    CL8                                                              
         DS    CL2                                                              
LINDEX   DS    CL8                                                              
         DS    CL2                                                              
LRTG1    DS    CL8                                                              
         DS    CL2                                                              
LRTG2    DS    CL8                                                              
         DS    CL2                                                              
LRTG3    DS    CL8                                                              
         DS    CL2                                                              
LRTG4    DS    CL8                                                              
         DS    CL2                                                              
LRTG5    DS    CL8                                                              
         DS    CL2                                                              
LRTG6    DS    CL8                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NENETGOALD                                                     
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NEMEDFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDD3D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
*                                                                               
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'174NEMED83S  05/01/02'                                      
         END                                                                    
