*          DATA SET NEWRI23X   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEWRI23    AT LEVEL 093 AS OF 03/17/99                      
*PHASE T32023X                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE KHDUMMY                                                                
         TITLE 'T32023 - N5  REPORT  PHASE'                                     
*******************************************************                         
*  N5 REPORT IS DISCONTINUED                                                    
*  SCREEN = NEWRIE2X EDIT = NEWRI22X                                            
*******************************************************                         
T32023   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NWN5**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T32023,RB,RA      RA = 2ND BASE REG                              
         L     R8,ASPOOLD        * ANETWS1   = OPEN WORK AREAS                  
         USING SPOOLD,R8                                                        
         L     R9,ASYSD          * ANETWS2 = FIXED WORK AREA                    
         USING NETSYSD,R9                                                       
         L     R6,ANETWS4        * ANETWS3 = CLIST                              
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         L     R7,ANETWS2        * ANETWS4 = NDDEMBLK                           
         USING MYD,R7                                                           
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         L     R1,ANETWS4                                                       
         A     R1,=F'1000'                                                      
         ST    R1,ARCTOT                                                        
         BAS   R5,GETDISP                                                       
         MVC   ORDER,=C'NSCZ'      SET MEDIA ORDER/Z=OTHER                      
         L     R1,=A(BINTABLE)                                                  
         A     R1,RELO                                                          
         ST    R1,ABINTABL                                                      
* GET NETGOAL ADDRS                                                             
         GOTO1 NBCALLOV,DMCB,0,X'D9000A35'                                      
         L     RF,DMCB             PICK UP ADDRESS OF NETGOAL                   
         ST    RF,ANETGOAL                                                      
* GET DEMOCON ADDRS                                                             
         GOTO1 NBCALLOV,DMCB,0,X'D9000AE0'                                      
         L     RF,DMCB             PICK UP ADDRESS OF DEMOCON                   
         ST    RF,ADEMOCON                                                      
         SPACE                                                                  
* SET UP BINSRCH PARAMETERS                                                     
*                                                                               
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BAS   RE,REPMOD                                                        
XIT      XIT1                                                                   
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
EDERR    GOTO1 ERREX                                                            
         EJECT                                                                  
*              REPORT INITIALIZATION                                            
         SPACE 3                                                                
REPMOD   NTR1                                                                   
*                               * SET UP BINSRCH PARAMETERS                     
         SR    R0,R0               A OF REC TO BE ADDED                         
         L     R1,ABINTABL         A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,BRECLENE         LENGTH OF REC                                
         LA    R4,BKEYLENE         DISP OF KEY INTO REC                         
         L     R5,=F'1000'        MAX RECS IN BINTBL                            
         STM   R0,R5,BINDMCB                                                    
         L     RE,ABINTABL      CLEAR BINTABLE                                  
         L     RF,=F'120000'                                                    
         XCEF                                                                   
         EJECT                                                                  
*                                      GET WEEKLIST                             
WK02     NETGO NSNETIO,DMCB,NETBLOCK                                            
**       CLI   NBERROR,0               I NEED THE SPACE                         
**       BE    *+6                                                              
**       DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALDAT                                                  
         BNE   WK02                                                             
         MVI   PERTYPE,C'W'        WEEKS ONLY                                   
         LA    R3,53               FOR 53 WEEKS MAX                             
         ST    R3,NUMPER                                                        
         L     R3,ANETWS1                                                       
         NETGO NVWKLST,DMCB,NUMPER,(R3),PERTYPE                                 
**                                                                              
** DO NOT USE WEEKLIST, BUT NEED ROUTINE TO SET UP NBCMPSTRT/END                
** AND ANYTHING ELSE IT MIGHT DO - COMPATIBLE WITH OTHER GOAL PROG              
         EJECT                                                                  
*                               ****  READ GOAL RECS ****                       
GETGOALS DS    0H                                                               
         L     R5,ANETWS1                                                       
         USING NETGOALD,R5                                                      
         XC    0(150,R5),0(R5)                                                  
         LA    R2,NETBLOCK                                                      
         ST    R2,NGANTBLK                                                      
         L     R2,ANETWS1                                                       
         LA    R2,150(R2)                                                       
         ST    R2,NGAPLIST                                                      
         MVI   NGMAXPRD,100        LIST=100 PRODS MAX                           
         LA    R2,NETGOLHK                                                      
         ST    R2,NGAHOOK                                                       
         LA    R2,MYWORK                                                        
         USING BINRECD,R2                                                       
         XC    0(BRECLENE,R2),0(R2)                                             
         GOTO1 ANETGOAL,DMCB,NGBLOCK                                            
         B     REP5                                                             
*                                                                               
NETGOLHK NTR1                                                                   
         SPACE                                                                  
         MVC   BPRD,NGOALPRD       PRODUCT CODE                                 
         MVI   BMEDIA,X'FF'      MEDIA = X'FF'                                  
         MVI   BMEDX,X'FF'      MEDIA = X'FF'                                   
         MVC   BEST,NGOALEST       ESTIMATE                                     
         MVC   BDPT,NGOALDP        DAYPART                                      
         MVC   BGOAL,NGOALTRG      GOAL TARGET DEMO                             
         MVC   BGGRP,NGOALGRP      GOAL GRP                                     
         MVC   BGDOL,NGOALDOL     GOAL DOLLARS                                  
         DROP  R2                                                               
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R2)),RR=RELO                             
         CLI   0(R1),X'01'         TEST IF REC FOUND(X'01'=NOT FOUND)           
         BE    NGX                                                              
         L     R3,0(R1)            REC FOUND/ADD TO GOALS                       
         USING BINRECD,R3                                                       
         L     R4,BGGRP                                                         
         A     R4,NGOALGRP                                                      
         ST    R4,BGGRP                                                         
         L     R4,BGDOL                                                         
         A     R4,NGOALDOL                                                      
         ST    R4,BGDOL                                                         
NGX      DS    0H                                                               
         XC    0(BRECLENE,R2),0(R2)                                             
         B     XIT                                                              
         DROP  R5,R3                                                            
         EJECT                                                                  
*                                                                               
REP5     DS    0H            SET UP SORTER                                      
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         B      REP7                                                            
         SPACE                                                                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,4,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=40'                                    
         SPACE                                                                  
*                                                                               
REP7     DS    0H                                                               
         XC    UNIV,UNIV           PXZ                                          
         MVI   NBRESUME,NBPROCPK                                                
         MVI   NBDATA,C'U'                                                      
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBSPLOPT,X'C0'      OPTION TO SPLIT EVEN IF POOL                 
         MVI   NBHUNOPT,C'Y'                                                    
         NETGO NVDEMOPT,DMCB       ACT SCHED/EST DEMO FOR MAKE GOOD             
*                                                         NOT FOR PFB           
         L     R1,=A(NETBUFFA)     SETUP FOR NBEFFMED                           
         A     R1,RELO                                                          
         ST    R1,NBANBUFF                                                      
         LR    RE,R1                                                            
         LA    RF,1000                                                          
         XCEF                                                                   
         CLC   NBSELPRD,=C'POL'                                                 
         BE    GETUNIT                                                          
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    GETUNIT                                                          
         OC    NBSELPRD,NBSELPRD                                                
         BZ    GETUNIT                                                          
*                                  PROD SELECTED/GET 1 CHAR CODE                
         L     R1,ACLIST                                                        
LOOP     CLC   0(3,R1),NBSELPRD                                                 
         BE    REP7A                                                            
         LA    R1,4(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   LOOP                                                             
         DC    H'0'                                                             
REP7A    MVC   PRD1SV,3(R1)                                                     
*                                                                               
GETUNIT  LA    R2,MYWORK                                                        
         USING BINRECD,R2                                                       
         XC    0(BRECLENE,R2),0(R2)                                             
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BE    UNIT                                                             
**       MVC   WORK(3),=C'BY ' %%%%%%%%$$$$$$$%%%%%%%%%TESTING                  
**       BAS   RE,NEWEST       $$$$$$$$$$$$$$$$$$$$$$$$$$$$                     
         CLI   NBMODE,NBREQLST                                                  
         BE    REP10                                                            
         B     GETUNIT                                                          
*                                                                               
UNIT     CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    UNIV,UNIV                                                        
         BNZ   UNIT1                                                            
         MVC   NBAIOSV,NBAIO                                                    
         L     R1,=A(UNIVWORK)                                                  
         A     R1,RELO                                                          
         ST    R1,NBAIO                                                         
         LA    R4,UNIV                                                          
         NETGO NVUNIV,DMCB,(0,NDDEMBLK),(C'E',DBLOCK),0(R4)                     
         MVC   NBAIO,NBAIOSV                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
***      BAS   RE,PRNTU        ****TESTING****                                  
UNIT1    OC    NBPROGNM,=16XL1'40'    REPLACE BINARY ZEROES W/ SPACES           
*                                     FOR SORTING BY PROG NAME                  
         CLI   PRD1SV,0            IF PRD REQ FILTER ON PRD                     
         BE    UNT3                                                             
         CLC   NBSPLPRN,PRD1SV                                                  
         BNE   GETUNIT                                                          
UNT3     MVC   0(1,R2),NBSPLPRN  PASS UNITS TO SORTER IN PROD/DPT ORDER         
         MVC   1(1,R2),NBACTDP                                                  
         MVC   3(1,R2),NBEFFMED                                                 
         CLI   3(R2),C'N'                                                       
         BNE   *+12                                                             
         MVI   2(R2),1                                                          
         B     UNT5                                                             
         CLI   3(R2),C'S'                                                       
         BNE   *+12                                                             
         MVI   2(R2),2                                                          
         B     UNT5                                                             
         CLI   3(R2),C'C'                                                       
         BNE   *+12                                                             
         MVI   2(R2),3                                                          
         B     UNT5                                                             
         MVI   2(R2),4             OTHER                                        
UNT5     MVC   7(32,R2),NBKEY                                                   
         GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
         B     GETUNIT                                                          
         EJECT                                                                  
REP10    DS    0H                                                               
REP11    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R4,4(R1)                                                         
         LTR   R4,R4                                                            
         BNZ   REP12                                                            
         CLI   SRT1,C'N'                                                        
         BE    REP20               END OF RECORDS                               
         L     R2,AP1                                                           
         MVC   0(18,R2),=C'NO RECORDS RECEIVED FROM SORT'                       
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
*                                                                               
REP12    CLI   SRT1,C'N'                                                        
         BE    REP13                                                            
         MVI   SRT1,C'N'                                                        
         B     REP13A                                                           
REP13    DS    0H                                                               
         CLC   SRTPRD1,0(R4)        IS IT SAME PRD                              
         BE    REP15                                                            
REP13A   MVC   WORK(1),0(R4)                                                    
         BAS   R5,PRDCD                                                         
         MVC   SRTPRD3,WORK                                                     
         BAS   RE,NEWEST                                                        
         MVC   SRTPRD1,0(R4)                                                    
*                                                                               
REP15    MVC   NBKEY,7(R4)         SET KEY FOR NETIO                            
         MVC   MEDSV,3(R4)         SET MEDIA                                    
         MVC   MEDXSV,2(R4)                                                     
         LA    R2,MYWORK                                                        
         USING BINRECD,R2                                                       
         XC    0(BRECLENE,R2),0(R2)                                             
         MVI   NBFUNCT,NBFGET                                                   
         MVI   NBSPLPRN,0                                                       
         NETGO NSNETIO,DMCB,NETBLOCK                                            
****     BAS   RE,PRNTU                                                         
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NBSPLPRN,0(R4)            ***                                    
         MVI   NBWHERE,0                 ***                                    
         GOTO1 NBNETVAL,DMCB,NETBLOCK    ***                                    
*****    BAS   RE,PRNTU                                                         
         LA    R1,5                BCT DEMO LIMIT                               
         LA    RF,B1DEMO                                                        
         LA    R3,NDDEMOS                                                       
         LA    R4,NDESTDEM                                                      
         LA    R5,NDACTDEM                                                      
DEMLOOP  MVC   0(3,RF),0(R3)       DEMO SELECTION                               
         MVC   5(2,RF),2(R4)       EST GRP                                      
         MVC   9(2,RF),2(R5)       ACT GRP                                      
         MVC   11(4,RF),4(R4)       EST IMP                                     
         MVC   15(4,RF),4(R5)      ACT IMP                                      
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         LA    R3,3(R3)                                                         
         LA    RF,BDEMLENE(RF)                                                  
         BCT   R1,DEMLOOP                                                       
*                                                                               
         L     RE,NBACTUAL         ACTUAL DOLLARS                               
         SRDA  RE,32(0)                                                         
         D     RE,=F'100'                                                       
         STCM  RF,15,BACTDOL                                                    
******   BAS   RE,PRDCD                                                         
         MVC   BPRD,SRTPRD3       PRODUCT                                       
         MVC   BEST,NBACTEST      ESTIMATE                                      
         MVC   BDPT,NBACTDP       DAYPART                                       
         MVC   BMEDIA,MEDSV       MEDIA                                         
         MVC   BMEDX,MEDXSV       MEDIA                                         
         CLI   BMEDIA,0                                                         
         BNE   *+8                                                              
         MVI   BMEDIA,C'Z'         SET TO OTHER                                 
         MVC   BGOAL,NDDEMOS+2     1ST DEMO ON EST HEADER                       
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R2)),RR=RELO                             
         CLI   0(R1),X'01'         X'01'=NOT FOUND                              
         BE    REP11                                                            
         L     R3,0(R1)            REC FOUND/SO ADD TO IT                       
*                                                                               
         LA    R3,16(R3)     *** BE CAREFUL (SEE DSECT)                         
         L     RF,BACTDOL          ACTDOLS                                      
         A     RF,0(R3)                                                         
         ST    RF,0(R3)                                                         
         LA    R3,7(R3)            DEMOS                                        
         LA    R2,B1DEME                                                        
         LA    R1,5                BCT DEMO LIMIT                               
ADDLOOP  DS    0H                                                               
         ICM   R4,15,0(R3)                                                      
         MVC   FULL,0(R2)                                                       
         A     R4,FULL                                                          
         STCM  R4,15,0(R3)                                                      
*                                                                               
         ICM   R4,15,4(R3)                                                      
         MVC   FULL,4(R2)                                                       
         A     R4,FULL                                                          
         STCM  R4,15,4(R3)                                                      
*                                                                               
         ICM   R4,15,8(R3)                                                      
         MVC   FULL,8(R2)                                                       
         A     R4,FULL                                                          
         STCM  R4,15,8(R3)                                                      
*                                                                               
         ICM   R4,15,12(R3)                                                     
         MVC   FULL,12(R2)                                                      
         A     R4,FULL                                                          
         STCM  R4,15,12(3)                                                      
*                                                                               
         LA    R3,BDEMLENE(R3)                                                  
         LA    R2,BDEMLENE(R2)                                                  
         BCT   R1,ADDLOOP                                                       
*                                                                               
         B     REP11                                                            
         EJECT                                                                  
**************************************************                              
* STEP THROUGH BINTABLE.SHOULD BE UNIT RECS BY PROD/DPT/MED                     
*                       FOLLOWED BY THE CORRESPONDING GOAL REC                  
*                                                                               
REP20    DS    0H                                                               
         L     R2,ABINTABL                                                      
         USING BINRECD,R2                                                       
         OC    0(5,R2),0(R2)                                                    
         BE    XIT                 NO DATA                                      
         CLI   FRST,C'N'                                                        
         BE    REP22                                                            
         BAS   RE,ROUT1                                                         
         MVI   FRST,C'N'                                                        
*                                                                               
REP22    CLC   CURPRD,BPRD         CHK PROD CHANGE                              
         BE    REP26                                                            
         CLI   PREVREC,C'G'        PREVIOUS BINREC 'GOAL'                       
         BNE   REP24                                                            
         BAS   RE,RECAP                                                         
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,ROUT1                                                         
         B     REP30                                                            
REP24    BAS   RE,TOTL2                                                         
         BAS   RE,RECAP                                                         
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,ROUT1                                                         
         B     REP30                                                            
*                                                                               
REP26    CLC   BDPT,CURDPT         CHK DPT CHANGE                               
         BE    REP30                                                            
         CLI   PREVREC,C'U'                                                     
         BNE   *+8                                                              
         BAS   RE,TOTL2                                                         
         BAS   RE,PRINTIT          SKIP LINE BETWEEN DAYPARTS                   
         BAS   RE,GETDPTNM                                                      
         BAS   RE,WRTDPT                                                        
*                                                                               
REP30    CLI   BMEDIA,X'FF'        GOAL BINREC                                  
         BNE   *+12                                                             
         BAS   RE,TOTLINE          YES                                          
         B     *+8                                                              
         BAS   RE,MEDLINE          NO                                           
         LA    R2,BRECLENE(R2)                                                  
         CLC   0(3,R2),=3X'00'     ARE THERE MORE BINRECS                       
         BNE   REP22                                                            
         CLI   PREVREC,C'U'        NO/END OF FILE                               
         BNE   *+8                                                              
         BAS   RE,TOTL2                                                         
         BAS   RE,RECAP                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************                                 
* BINREC IS UNITS                                                               
MEDLINE  NTR1                                                                   
         BAS   RE,PRINTIT          SKIP LINE                                    
         L     R5,AP1                                                           
         ST    R2,ACURBIN                                                       
         USING PLINED,R5                                                        
         MVI   PREVREC,C'U'                                                     
*                                                                               
         CLI   BMEDIA,C'N'         MEDIA                                        
         BNE   *+14                                                             
         MVC   PMED,=C'NTWRK'                                                   
         B     MED7                                                             
         CLI   BMEDIA,C'C'                                                      
         BNE   *+14                                                             
         MVC   PMED,=C'CABLE'                                                   
         B     MED7                                                             
         MVC   PMED,=C'SYND '                                                   
         CLI   BMEDIA,C'S'                                                      
         BE    *+10                                                             
         MVC   PMED,=C'OTHER'                                                   
MED7     DS    0H                                                               
*                                                                               
         L     RF,BACTDOL                                                       
         EDIT  (RF),(9,PACTDOL)    ACTUAL                                       
         MVC   FULL,B1DEME         TARGET EST                                   
         LA    R3,PESTGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
         MVC   FULL,B1DEMA         TARGET ACT                                   
         LA    R3,PACTGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   DUB(4),B1DEMA       GRP INDEX                                    
         MVC   DUB+4(4),B1DEME                                                  
         LA    RF,PESTIND                                                       
         BAS   RE,EDIND                                                         
*                                                                               
         ZIC   R1,DEMONUM          BCT DEMO LIMIT FOR REST OF DEMOS             
         LA    R2,B2DEME                                                        
         LA    R3,PDEMEST1                                                      
DEMOLOOP MVC   FULL,0(R2)                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
         MVC   FULL,4(R2)                                                       
         LA    RE,6(R3)                                                         
         ST    RE,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   DUB(4),4(R2)         GRP INDEX                                   
         MVC   DUB+4(4),0(R2)                                                   
         LA    RF,16(R3)                                                        
         BAS   RE,EDIND                                                         
         B     DEML2               ** NEW N5 REP                                
*                                                                               
         LA    R2,BDEMLENE(R2)                                                  
         LA    R3,PDEMLENE(R3)                                                  
         BCT   R1,DEMOLOOP                                                      
*                                                                               
DEML2    BAS   RE,ROLLDPT         ROLL OVER TO DPTTOT LINE                      
         BAS   RE,ROLLMED         ROLL OVER TO MEDIA RECAP LINE                 
*                                  NOW DO CPM LINE                              
*                                                                               
         L     R5,AP1                                                           
         LA    R5,132(R5)                                                       
         L     R2,ACURBIN          SET R2 TO START OF BINREC                    
         MVC   PMED(3),=C'CPM'                                                  
         OC    B1IMPE,B1IMPE                                                    
         BZ    SKIP1                                                            
         MVC   DUB(4),BACTDOL                                                   
         MVC   DUB+4(4),B1IMPE                                                  
         LA    RF,PESTGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,ESTIMP                                                        
SKIP1    OC    B1IMPA,B1IMPA                                                    
         BZ    SKIP2                                                            
         MVC   DUB(4),BACTDOL                                                   
         MVC   DUB+4(4),B1IMPA                                                  
         LA    RF,PACTGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,ACTIMP                                                        
*                                                                               
SKIP2    B     SKIP3           *** GRP INDX ON MEDIA LINE ABOVE                 
         L     RE,ACTIMP                                                        
         SRDA  RE,32(R0)                                                        
         OC    ESTIMP,ESTIMP                                                    
         BZ    SKIP3                                                            
         M     RE,=F'100'                                                       
         D     RE,ESTIMP                                                        
         EDIT  (RF),(3,PESTIND)                                                 
*                                                                               
SKIP3    ZIC   R1,DEMONUM                                                       
         LA    R5,PDEMEST1                                                      
         LA    R4,B2IMPE                                                        
         MVC   ACTDOLSV,BACTDOL                                                 
IMPLOOP  XC    ESTIMP,ESTIMP                                                    
         XC    ACTIMP,ACTIMP                                                    
         OC    0(4,R4),0(4)                                                     
         BZ    SKIP4                                                            
         MVC   DUB(4),ACTDOLSV                                                  
         MVC   DUB+4(4),0(R4)                                                   
         LR    RF,R5                                                            
         BAS   RE,EDCPM                                                         
         ST    RF,ESTIMP                                                        
SKIP4    OC    4(4,R4),4(R4)                                                    
         BZ    SKIP5                                                            
         MVC   DUB(4),ACTDOLSV                                                  
         MVC   DUB+4(4),4(R4)                                                   
         LA    RF,6(R5)            SET NXT PRINT POSITION                       
         BAS   RE,EDCPM                                                         
         ST    RF,ACTIMP                                                        
*                                                                               
         B     SKIP5               GRP INDX ON MEDIA LINE ABOVE                 
         OC    ESTIMP,ESTIMP                                                    
         BZ    SKIP5                                                            
         L     RE,ACTIMP                                                        
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         ICM   R2,15,ESTIMP                                                     
         DR    RE,R2                                                            
         LA    R2,12(R5)                                                        
         BAS   RE,EDIND                                                         
*                                                                               
SKIP5    LA    R5,PDEMLENE(R5)     BUMPT TO NXT PRINT POSITION                  
         LA    R4,BDEMLENE(R4)                                                  
         BCT   R1,IMPLOOP                                                       
         XC    ACTCPM(20),ACTCPM                                                
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
*                                                                               
EDITGRP  NTR1                                                                   
         L     R3,APOUT                                                         
         OC    FULL,FULL                                                        
         BZ    EDGX                                                             
         CLC   FULL,=F'9999'                                                    
         BNH   EDT5                                                             
         L     RF,FULL                                                          
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         EDIT  (RF),(5,0(R3))                                                   
         B     EDGX                                                             
EDT5     EDIT  (B4,FULL),(5,0(R3)),1                                            
EDGX     B     XIT                                                              
         EJECT                                                                  
***************************************                                         
* GOAL REC - DO TOTAL FOR DAYPART                                               
*                                                                               
TOTLINE  NTR1                                                                   
*                                                                               
         BAS   RE,PRINTIT          SKIP LINE                                    
         ST    R2,ACURBIN          STORE ADDRESS OF BINREC                      
         L     R5,AP1                                                           
         USING PLINED,R5                                                        
         MVC   FULL,BGGRP                                                       
         BAS   R3,GIMPS            GET GOAL IMPS                                
         MVI   PREVREC,C'G'                                                     
         MVC   PDPT+5(5),=C'TOTAL'                                              
         EDIT  (B4,BGDOL),(9,PGOLDOL)       GOAL DOLLARS                        
         MVC   FULL,BGGRP                   GOAL GRP                            
         LA    R3,PGOLGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         EDIT  (B4,DACTDOL),(9,PACTDOL)                                         
*                                                                               
         MVC   FULL,D1DEME                                                      
         LA    R3,PESTGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   FULL,D1DEMA                                                      
         LA    R3,PACTGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   DUB(4),D1DEMA      GRP INDX                                      
         MVC   DUB+4(4),BGGRP                                                   
         LA    RF,PGOLIND                                                       
         BAS   RE,EDIND                                                         
         MVC   DUB(4),D1DEMA                                                    
         MVC   DUB+4(4),D1DEME                                                  
         LA    RF,PESTIND                                                       
         BAS   RE,EDIND                                                         
*                                                                               
         ZIC   R1,DEMONUM          BCT LIMIT                                    
         LA    R4,PDEMEST1                                                      
         LA    R3,D2DEME                                                        
EDADD    MVC   FULL,0(R3)                                                       
         ST    R4,APOUT                                                         
         BAS   RE,EDITGRP                                                       
         LA    RE,6(R4)                                                         
         ST    RE,APOUT                                                         
         MVC   FULL,4(R3)                                                       
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   DUB(4),4(R3)       GRP INDX                                      
         MVC   DUB+4(4),0(R3)                                                   
         LA    RF,16(R4)                                                        
         BAS   RE,EDIND                                                         
         B     EDAD5               ** NEW N5                                    
*                                                                               
         LA    R4,PDEMLENE(R4)                                                  
         LA    R3,DDEMLENE(R3)                                                  
EDAD5    BCT   R1,EDADD                                                         
*                                                                               
TOTCPM   DS    0H                  * DO TOT CPM LINE                            
         L     R5,AP1                                                           
         LA    R5,132(R5)                                                       
         MVC   PDPT+6(3),=C'CPM'                                                
*                                                                               
         OC    GOALIMP,GOALIMP                                                  
         BZ    TC3                                                              
         OC    BGDOL,BGDOL                                                      
         BZ    TC3                                                              
         MVC   DUB(4),BGDOL            GOAL CPM                                 
*        MVC   DUB+4(4),GOALIMP                                                 
         ICM   R1,15,GOALIMP                                                    
         SR    R0,R0                                                            
         M     R0,=F'10'                                                        
         ST    R1,DUB+4                                                         
         LA    RF,PGOLGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,GOALCPM                                                       
TC3      OC    D1IMPE,D1IMPE                                                    
         BZ    TC5                                                              
         MVC   DUB(4),DACTDOL          EST CPM                                  
         MVC   DUB+4(4),D1IMPE                                                  
         LA    RF,PESTGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,ESTCPM                                                        
TC5      OC    D1IMPA,D1IMPA                                                    
         BZ    TC7                                                              
         MVC   DUB(4),DACTDOL          ACT CPM                                  
         MVC   DUB+4(4),D1IMPA                                                  
         LA    RF,PACTGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,ACTCPM                                                        
TC7      OC    GOALCPM,GOALCPM    DO TARGET INDEX                               
         BZ    TC8                IF GOALCPM=0/SKIP                             
         OC    ESTCPM,ESTCPM                                                    
         BZ    TC10                                                             
         L     RE,GOALCPM                                                       
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,ESTCPM                                                        
         LA    R2,PGOLIND                                                       
******   BAS   RE,EDIND                                                         
*                                                                               
TC8      OC    ESTCPM,ESTCPM      DO ACTUAL INDEX                               
         BZ    TC10               IF ESTCPM OR ACTCPM = 0/SKIP                  
         OC    ACTCPM,ACTCPM                                                    
         BZ    TC10                                                             
         L     RE,ACTCPM                                                        
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,ESTCPM                                                        
         LA    R2,PESTIND                                                       
******   BAS   RE,EDIND                                                         
*                                                                               
TC10     DS    0H                  LOOP TO DO DEMOS AFTER TARGET                
         ZIC   R3,DEMONUM          BCT LIMIT                                    
         LA    R2,PDEMEST1                                                      
         LA    R4,D2IMPE                                                        
TCLOOP   XC    ACTCPM(8),ACTCPM                                                 
         OC    0(4,R4),0(R4)       CHK EST IMP                                  
         BNZ   TC12                                                             
         LA    R2,6(R2)                                                         
         B     TC15                                                             
TC12     MVC   DUB(4),DACTDOL                                                   
         MVC   DUB+4(4),0(R4)                                                   
         LR    RF,R2                                                            
         BAS   RE,EDCPM                                                         
         ST    RF,ESTCPM                                                        
         LA    R2,6(R2)                                                         
         OC    4(4,R4),4(R4)                                                    
         BZ    TC15                                                             
         MVC   DUB(4),DACTDOL                                                   
         MVC   DUB+4(4),4(R4)                                                   
         LR    RF,R2                                                            
         BAS   RE,EDCPM                                                         
         ST    RF,ACTCPM                                                        
TC15     LA    R2,6(R2)            BUMP TO INDEX OUT FIELD                      
         OC    ESTCPM,ESTCPM                                                    
         BZ    TC17                                                             
         OC    ACTCPM,ACTCPM                                                    
         BZ    TC17                                                             
         L     RE,ACTCPM                                                        
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,ESTCPM                                                        
****     BAS   RE,EDIND                                                         
TC17     LA    R2,5(R2)                                                         
         LA    R4,DDEMLENE(R4)                                                  
         BCT   R3,TCLOOP                                                        
TC20     DS    0H                                                               
         L     R2,ACURBIN                                                       
         BAS   RE,ROLLMED                                                       
         XC    DPTTOT(DRECLENE),DPTTOT    CLEAR DPTTOT LINE                     
         XC    ACTCPM(20),ACTCPM                                                
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         EJECT                                                                  
****************************************                                        
* INPUT:  DIVIDEND IN DUB, DIVISOR IN DUB+4                                     
*         OUTAREA IN RF                                                         
* OUTPUT: ROUNDED EDITED NUMBER IN OUTAREA, BINARY VALUE IN RF                  
*                                                                               
EDCPM    NTR1                                                                   
         LR    R2,RF                                                            
         L     RE,DUB                                                           
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         M     RE,=F'1000'                                                      
         D     RE,DUB+4                                                         
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         LTR   RF,RF                                                            
         BZ    EDCX                                                             
         EDIT  (RF),(5,0(R2)),2                                                 
         CLI   0(R2),X'40'                                                      
         BH    *+8                                                              
         MVI   0(R2),C'$'                                                       
EDCX     XIT1  REGS=(RF)                                                        
*                                                                               
****************************************                                        
* INPUT:  DIVIDEND IN DUB, DIVISOR IN DUB+4                                     
*         OUTAREA IN RF                                                         
* OUTPUT: +/- GRP INDEX IN OUTAREA, BINARY VALUE IN RF                          
*                                                                               
EDIND    NTR1                                                                   
         OC    DUB+4(4),DUB+4                                                   
         BZ    EDIX                                                             
         LR    R2,RF                                                            
         L     RE,DUB                                                           
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,DUB+4                                                         
         S     RF,=F'100'                                                       
         EDIT  (RF),(4,0(R2)),FLOAT=-                                           
         LTR   RF,RF                                                            
         BM    EDIX                                                             
         BNZ   EDI5                                                             
         MVC   2(2,R2),=C'+0'                                                   
         B     EDIX                                                             
EDI5     LA    R3,3(R2)                                                         
         LA    R4,4                                                             
PLUS     CLI   0(R3),C' '                                                       
         BE    EDI9                                                             
         BCTR  R3,0                                                             
         BCT   R4,PLUS                                                          
         B     *+8                                                              
EDI9     MVI   0(R3),C'+'                                                       
EDIX     XIT1  REGS=(RF)                                                        
         EJECT                                                                  
****************************************                                        
* ROLL OVER MEDIA LINE TO DPTTOT LINE                                           
*                                                                               
ROLLDPT  NTR1                                                                   
         USING BINRECD,R2                                                       
         L     R2,ACURBIN                                                       
         LA    R3,DPTTOT                                                        
         USING DPTTOT,R3                                                        
         L     R4,DACTDOL          ACTUAL DOLLARS                               
         A     R4,BACTDOL                                                       
         ST    R4,DACTDOL                                                       
*                                                                               
         ZIC   R1,DEMONUM         ADD GRPS/IMPS TO DPTTOT LINE                  
         LA    R1,1(R1)                                                         
         LA    R2,B1DEME                                                        
         LA    R3,D1DEME                                                        
ADDGRP   MVC   FULL,0(R2)                                                       
         ICM   R4,15,0(R3)                                                      
         A     R4,FULL                                                          
         STCM  R4,15,0(R3)                                                      
*                                                                               
         MVC   FULL,4(R2)                                                       
         ICM   R4,15,4(R3)                                                      
         A     R4,FULL                                                          
         STCM  R4,15,4(R3)                                                      
*                                                                               
         MVC   FULL,8(R2)                                                       
         ICM   R4,15,8(R3)                                                      
         A     R4,FULL                                                          
         STCM  R4,15,8(R3)                                                      
*                                                                               
         MVC   FULL,12(R2)                                                      
         ICM   R4,15,12(R3)                                                     
         A     R4,FULL                                                          
         STCM  R4,15,12(R3)                                                     
         LA    R2,BDEMLENE(R2)                                                  
         LA    R3,DDEMLENE(R3)                                                  
         BCT   R1,ADDGRP                                                        
         DROP  R3                                                               
         B     XIT                                                              
*                                                                               
**********************************************                                  
* ROLL OVER MEDIA LINE TO RECAP MEDIA TOT LINE                                  
*                                                                               
ROLLMED  NTR1                                                                   
         L     R2,ACURBIN                                                       
         USING BINRECD,R2                                                       
         LA    R3,RMAREA           MATCH THE MEDIA                              
         USING RMTOTD,R3                                                        
         CLI   BMEDIA,X'FF'                                                     
         BNE   *+8                                                              
         MVI   BMEDIA,C'N'                                                      
MED1     CLC   RMMEDIA,BMEDIA                                                   
         BE    MED2                                                             
         OC    0(10,R3),0(R3)                                                   
         BZ    *+12                                                             
         LA    R3,RMRECLNE(R3)                                                  
         B     MED1                                                             
         MVC   RMMEDIA,BMEDIA      NO MATCH,SO ADD NEW MEDIA REC                
*                                                                               
MED2     L     R4,RMACTDOL         ACTUAL DOLLARS                               
         A     R4,BACTDOL                                                       
         ST    R4,RMACTDOL                                                      
*********                                                                       
         L     R4,RMGDOL           *******DOLLARS                               
         A     R4,BGDOL                                                         
         ST    R4,RMGDOL                                                        
         L     R4,RMGGRP           *******DOLLARS                               
         A     R4,BGGRP                                                         
         ST    R4,RMGGRP                                                        
***************                                                                 
         ZIC   R1,DEMONUM         ADD GRPS/IMPS TO DPTTOT LINE                  
         LA    R1,1(R1)                                                         
         LA    R2,B1DEME                                                        
         LA    R3,RM1DEME                                                       
MED5     MVC   FULL,0(R2)                                                       
         ICM   R4,15,0(R3)                                                      
         A     R4,FULL                                                          
         STCM  R4,15,0(R3)                                                      
*                                                                               
         MVC   FULL,4(R2)                                                       
         ICM   R4,15,4(R3)                                                      
         A     R4,FULL                                                          
         STCM  R4,15,4(R3)                                                      
*                                                                               
         MVC   FULL,8(R2)                                                       
         ICM   R4,15,8(R3)                                                      
         A     R4,FULL                                                          
         STCM  R4,15,8(R3)                                                      
*                                                                               
         MVC   FULL,12(R2)                                                      
         ICM   R4,15,12(R3)                                                     
         A     R4,FULL                                                          
         STCM  R4,15,12(R3)                                                     
         LA    R2,BDEMLENE(R2)                                                  
         LA    R3,RMDLENE(R3)                                                   
         BCT   R1,MED5                                                          
         DROP  R3                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
****************************************                                        
* ROLL OVER DPTTOT TO RECAP TOTAL                                               
*                                                                               
ROLLRC2  NTR1                                                                   
         USING RMMEDIA,R2                                                       
         L     R3,ARCTOT                                                        
         USING RCTOTD,R3                                                        
         L     R4,RMACTDOL                                                      
         A     R4,RCACTDOL                                                      
         ST    R4,RCACTDOL                                                      
*                                                                               
         L     R4,RMGGRP                                                        
         A     R4,RCGGRP                                                        
         ST    R4,RCGGRP                                                        
         L     R4,RMGDOL                                                        
         A     R4,RCGDOL                                                        
         ST    R4,RCGDOL                                                        
*                                                                               
         ZIC   R1,DEMONUM         ADD GRPS/IMPS TO DPTTOT LINE                  
         LA    R1,1(R1)                                                         
         LA    R2,RM1DEME                                                       
         LA    R3,RC1DEME                                                       
RC2A     MVC   FULL,0(R2)                                                       
         ICM   R4,15,0(R3)                                                      
         A     R4,FULL                                                          
         STCM  R4,15,0(R3)                                                      
*                                                                               
         MVC   FULL,4(R2)                                                       
         ICM   R4,15,4(R3)                                                      
         A     R4,FULL                                                          
         STCM  R4,15,4(R3)                                                      
*                                                                               
         MVC   FULL,8(R2)                                                       
         ICM   R4,15,8(R3)                                                      
         A     R4,FULL                                                          
         STCM  R4,15,8(R3)                                                      
*                                                                               
         MVC   FULL,12(R2)                                                      
         ICM   R4,15,12(R3)                                                     
         A     R4,FULL                                                          
         STCM  R4,15,12(R3)                                                     
         LA    R2,RMDLENE(R2)                                                   
         LA    R3,RCDLENE(R3)                                                   
         BCT   R1,RC2A                                                          
         DROP  R3                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***************************************                                         
* DO TOTAL LINE- BUT DO NOTUSE BINREC                                           
* TAKE DATA ONLY FROM DPTREC                                                    
*                                                                               
TOTL2    NTR1                                                                   
*                                                                               
         BAS   RE,PRINTIT          SKIP LINE                                    
         L     R5,AP1                                                           
         USING PLINED,R5                                                        
         MVC   PDPT+5(5),=C'TOTAL'                                              
*                                                                               
         EDIT  (B4,DACTDOL),(9,PACTDOL)                                         
*                                                                               
         MVC   FULL,D1DEME                                                      
         LA    R3,PESTGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   FULL,D1DEMA                                                      
         LA    R3,PACTGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   DUB(4),D1DEMA                                                    
         MVC   DUB+4(4),D1DEME                                                  
         LA    RF,PESTIND                                                       
         BAS   RE,EDIND                                                         
*                                                                               
         ZIC   R1,DEMONUM          BCT LIMIT                                    
         LA    R4,PDEMEST1                                                      
         LA    R3,D2DEME                                                        
EDADD2   MVC   FULL,0(R3)                                                       
         ST    R4,APOUT                                                         
         BAS   RE,EDITGRP                                                       
         LA    RE,6(R4)                                                         
         ST    RE,APOUT                                                         
         MVC   FULL,4(R3)                                                       
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   DUB(4),4(R3)                                                     
         MVC   DUB+4(4),0(R3)                                                   
         LA    RF,16(R4)                                                        
         BAS   RE,EDIND                                                         
         B     EDADD5              ** NEW N5                                    
*                                                                               
         LA    R4,PDEMLENE(R4)                                                  
         LA    R3,DDEMLENE(R3)                                                  
EDADD5   BCT   R1,EDADD2                                                        
*                                                                               
TOTCPM2  DS    0H                  * DO TOT CPM LINE                            
         L     R5,AP1                                                           
         LA    R5,132(R5)                                                       
         MVC   PDPT+6(3),=C'CPM'                                                
*                                                                               
         OC    D1IMPE,D1IMPE                                                    
         BZ    L25                                                              
         MVC   DUB(4),DACTDOL          EST CPM                                  
         MVC   DUB+4(4),D1IMPE                                                  
         LA    RF,PESTGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,ESTCPM                                                        
L25      OC    D1IMPA,D1IMPA                                                    
         BZ    L27                                                              
         MVC   DUB(4),DACTDOL          ACT CPM                                  
         MVC   DUB+4(4),D1IMPA                                                  
         LA    RF,PACTGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,ACTCPM                                                        
L27      OC    GOALCPM,GOALCPM    DO TARGET INDEX                               
         BZ    L28                IF GOALCPM=0/SKIP                             
         OC    ESTCPM,ESTCPM                                                    
         BZ    L210                                                             
         L     RE,GOALCPM                                                       
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,ESTCPM                                                        
         LA    R2,PGOLIND                                                       
******   BAS   RE,EDIND                                                         
*                                                                               
L28      OC    ESTCPM,ESTCPM      DO ACTUAL INDEX                               
         BZ    L210               IF ESTCPM OR ACTCPM = 0/SKIP                  
         OC    ACTCPM,ACTCPM                                                    
         BZ    L210                                                             
         L     RE,ACTCPM                                                        
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,ESTCPM                                                        
         LA    R2,PESTIND                                                       
*****    BAS   RE,EDIND                                                         
*                                                                               
L210     DS    0H                  LOOP TO DO DEMOS AFTER TARGET                
         ZIC   R3,DEMONUM          BCT LIMIT                                    
         LA    R2,PDEMEST1                                                      
         LA    R4,D2IMPE                                                        
L2LOOP   XC    ACTCPM(8),ACTCPM                                                 
         OC    0(4,R4),0(R4)       CHK EST IMP                                  
         BNZ   L212                                                             
         LA    R2,6(R2)                                                         
         B     L215                                                             
L212     MVC   DUB(4),DACTDOL                                                   
         MVC   DUB+4(4),0(R4)                                                   
         LR    RF,R2                                                            
         BAS   RE,EDCPM                                                         
         ST    RF,ESTCPM                                                        
         LA    R2,6(R2)                                                         
         OC    4(4,R4),4(R4)                                                    
         BZ    L215                                                             
         MVC   DUB(4),DACTDOL                                                   
         MVC   DUB+4(4),4(R4)                                                   
         LR    RF,R2                                                            
         BAS   RE,EDCPM                                                         
         ST    RF,ACTCPM                                                        
L215     LA    R2,6(R2)            BUMP TO INDEX OUT FIELD                      
         OC    ESTCPM,ESTCPM                                                    
         BZ    L217                                                             
         OC    ACTCPM,ACTCPM                                                    
         BZ    L217                                                             
         L     RE,ACTCPM                                                        
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,ESTCPM                                                        
*****    BAS   RE,EDIND                                                         
L217     LA    R2,5(R2)                                                         
         LA    R4,DDEMLENE(R4)                                                  
         BCT   R3,L2LOOP                                                        
L220     DS    0H                                                               
***      L     R2,ACURBIN (NOTE THAT IN THIS CASE BINREC IS NEXT PRD)           
***      BAS   RE,ROLLMED(IN TOTLINE BINREC IS GOAL DATA OF CURRENT PRD         
         XC    DPTTOT(DRECLENE),DPTTOT    CLEAR DPTTOT LINE                     
         XC    ACTCPM(20),ACTCPM                                                
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
* TAKES DPT CODE FROM BINREC                                                    
GETDPTNM NTR1                                                                   
         USING BINRECD,R2                                                       
         LA    R3,DPTTBL                                                        
GD3      CLC   BDPT,0(R3)                                                       
         BE    GD5                                                              
         LA    R3,9(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   GD3                                                              
         MVC   DPTNAME,=C'UNKNOWN'                                              
         B     *+10                                                             
GD5      MVC   DPTNAME,1(R3)                                                    
         MVC   CURDPT,BDPT                                                      
GDX      B     XIT                                                              
         SPACE 2                                                                
*        DAYPART NAME TABLE                                                     
DPTTBL   DC    CL1'D',CL8'DAYTIME'                                              
         DC    CL1'F',CL8'FRINGE'                                               
         DC    CL1'P',CL8'PRIME'                                                
         DC    CL1'K',CL8'KIDS'                                                 
         DC    CL1'S',CL8'SPORTS'                                               
         DC    CL1'N',CL8'NEWS'                                                 
         DC    CL1'L',CL8'LATE'                                                 
         DC    CL1'Y',CL8'YOUTH'                                                
         DC    CL1'E',CL8'EARLY'                                                
         DC    CL1'T',CL8'TEENS'                                                
         DC    CL1'C',CL8'CABLE'                                                
         DC    CL1'X',CL8'SYND'                                                 
         DC    CL1'I',CL8'SPECIAL'                                              
         DC    XL1'FF',CL8' '      END OF TABLE                                 
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
********************************                                                
GETPRDNM NTR1                                                                   
*                                                                               
         USING BINRECD,R2                                                       
         CLC   BPRD(3),=C'UNA'                                                  
         BNE   GET5                                                             
         XC    PRODNAME,PRODNAME                                                
         MVC   PRODNAME(11),=C'UNALLOCATED'                                     
         XC    CURPRD,CURPRD                                                    
         B     GPX                                                              
GET5     XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   KEY+4(3),BPRD           3 CHAR PRD CODE                          
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         USING PRDHDR,R4                                                        
         MVC   PRODNAME,PNAME                                                   
         MVC   CURPRD,BPRD                                                      
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
*                                                                               
GPX      DS    0H                                                               
         B     XIT                                                              
         DROP  R2                                                               
         SPACE                                                                  
**********************************                                              
*  GET 3CHAR CODE FROM C LIST                                                   
*                                                                               
PRDCD    DS    0H                                                               
         L     R3,ACLIST                                                        
PCD3     CLC   3(1,R3),WORK                                                     
         BE    PCD5                                                             
         LA    R3,4(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   PCD3                                                             
         MVC   WORK(3),=C'UNA'                                                  
         B     *+10                                                             
PCD5     MVC   WORK(3),0(R3)                                                    
         BR    R5                                                               
         EJECT                                                                  
*                                                                               
**********************************                                              
*                                                                               
ROUT1    NTR1                                                                   
         USING BINRECD,R2                                                       
         MVC   WORK(3),BPRD                                                     
         BAS   RE,NEWEST           GET EST/NDDEMOS                              
         BAS   RE,GETPRDNM                                                      
         BAS   RE,GETDPTNM                                                      
         BAS   RE,WRTDPT                                                        
         XC    RMAREA(250),RMAREA            CLEAR TOT AREA                     
         XC    RMAREA+250(250),RMAREA+250                                       
         L     RE,ARCTOT                                                        
         XC    0(150,RE),0(RE)                                                  
         XC    DPTTOT(DRECLENE),DPTTOT                                          
         XC    APOUT(28),APOUT                                                  
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 3                                                                
*                                                                               
WRTDPT   NTR1                                                                   
         L     R3,AP1                                                           
         USING PLINED,R3                                                        
         MVC   PDPT,DPTNAME                                                     
         LA    R2,132(R3)                                                       
WDP5     MVI   0(R2),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    R2,1(R2)                                                         
         CLI   0(R3),X'40'                                                      
         BH    WDP5                                                             
         DS    0H                                                               
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*******************************************                                     
* COMBINED MEDIA RECAP                                                          
*                                                                               
RECAP    NTR1                                                                   
         L     R5,AP1                                                           
         USING PLINED,R5                                                        
         BAS   RE,PRINTIT          SKIP LINE                                    
         BAS   RE,PRINTIT          SKIP LINE                                    
         L     R2,AP1                                                           
         MVC   PDPT(17),=C'COMBINED DAYPARTS'                                   
         MVC   PDPT+132(17),=17C'-'                                             
         LA    R2,RMAREA                                                        
         USING RMMEDIA,R2                                                       
         MVI   ORDNUM,0                                                         
RMED3    DS    0H                                                               
         BAS   RE,PRINTIT          SKIP LINE                                    
***      ST    R2,ARMAREA                                                       
         BAS   RE,SETMED                                                        
         OC    0(10,R2),0(R2)                                                   
         BZ    RMEDTOT                                                          
         BAS   RE,ROLLRC2         ROLL TO TOTAL RECAP LINE                      
         CLI   RMMEDIA,C'N'         MEDIA                                       
         BNE   *+14                                                             
         MVC   PMED,=C'NTWRK'                                                   
         B     RMED7                                                            
         CLI   RMMEDIA,C'C'                                                     
         BNE   *+14                                                             
         MVC   PMED,=C'CABLE'                                                   
         B     RMED7                                                            
         CLI   RMMEDIA,C'S'                                                     
         BNE   *+14                                                             
         MVC   PMED,=C'SYND '                                                   
         B     RMED7                                                            
         MVC   PMED,=C'OTHER'                                                   
RMED7    DS    0H                                                               
*                                                                               
         EDIT  (B4,RMACTDOL),(9,PACTDOL)    ACTUAL                              
         MVC   FULL,RM1DEME        TARGET EST                                   
         LA    R3,PESTGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
         MVC   FULL,RM1DEMA        TARGET ACT                                   
         LA    R3,PACTGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   DUB(4),RM1DEMA                                                   
         MVC   DUB+4(4),RM1DEME                                                 
         LA    RF,PESTIND                                                       
         BAS   RE,EDIND                                                         
*                                                                               
         ZIC   R1,DEMONUM          BCT DEMO LIMIT FOR REST OF DEMOS             
         LA    R2,RM2DEME                                                       
         LA    R3,PDEMEST1                                                      
RMED10   MVC   FULL,0(R2)                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
         MVC   FULL,4(R2)                                                       
         LA    RE,6(R3)                                                         
         ST    RE,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   DUB(4),4(R2)                                                     
         MVC   DUB+4(4),0(R2)                                                   
         LA    RF,16(R3)                                                        
         BAS   RE,EDIND                                                         
         B     RMED11              ** NEW N5                                    
*                                                                               
         LA    R2,RMDLENE(R2)                                                   
         LA    R3,PDEMLENE(R3)                                                  
RMED11   BCT   R1,RMED10                                                        
*                                                                               
RMED12   DS    0H                  RECAP CPM LINE                               
*                                  NOW DO CPM LINE                              
*                                                                               
         L     R5,AP1                                                           
         LA    R5,132(R5)                                                       
         L     R2,ARMAREA          SET R2 TO START OF RM RECORD                 
         MVC   PMED(3),=C'CPM'                                                  
         OC    RM1IMPE,RM1IMPE                                                  
         BZ    RMED14                                                           
         MVC   DUB(4),RMACTDOL                                                  
         MVC   DUB+4(4),RM1IMPE                                                 
         LA    RF,PESTGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,ESTIMP                  FOR CPM INDEX                         
RMED14   OC    RM1IMPA,RM1IMPA                                                  
         BZ    RMED16                                                           
         MVC   DUB(4),RMACTDOL                                                  
         MVC   DUB+4(4),RM1IMPA                                                 
         LA    RF,PACTGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,ACTIMP                                                        
RMED16   L     RE,ACTIMP                                                        
*        SRDA  RE,32(R0)                                                        
*        OC    ESTIMP,ESTIMP                                                    
*        BZ    RMED18                                                           
*        M     RE,=F'100'                                                       
*        D     RE,ESTIMP                                                        
*        EDIT  (RF),(3,PESTIND)                                                 
RMED18   ZIC   R1,DEMONUM                                                       
         LA    R5,PDEMEST1                                                      
         LA    R4,RM2IMPE                                                       
         MVC   ACTDOLSV,RMACTDOL                                                
RMED20   XC    ESTIMP,ESTIMP                                                    
         XC    ACTIMP,ACTIMP                                                    
         OC    0(4,R4),0(4)                                                     
         BZ    RMED22                                                           
         MVC   DUB(4),ACTDOLSV                                                  
         MVC   DUB+4(4),0(R4)                                                   
         LR    RF,R5                                                            
         BAS   RE,EDCPM                                                         
         ST    RF,ESTIMP                                                        
RMED22   OC    4(4,R4),4(R4)                                                    
         BZ    RMED24                                                           
         MVC   DUB(4),ACTDOLSV                                                  
         MVC   DUB+4(4),4(R4)                                                   
         LA    RF,6(R5)            SET NXT PRINT POSITION                       
         BAS   RE,EDCPM                                                         
         ST    RF,ACTIMP                                                        
         OC    ESTIMP,ESTIMP                                                    
         BZ    RMED24                                                           
         L     RE,ACTIMP                                                        
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         ICM   R2,15,ESTIMP                                                     
         DR    RE,R2                                                            
         LA    R2,12(R5)                                                        
****     BAS   RE,EDIND                                                         
RMED24   LA    R5,PDEMLENE(R5)     BUMP TO NXT PRINT POSITION                   
         LA    R4,RMDLENE(R4)                                                   
         BCT   R1,RMED20                                                        
         XC    ACTCPM(20),ACTCPM                                                
         BAS   RE,PRINTIT                                                       
***      L     R2,ARMAREA                                                       
*****    LA    R2,RMRECLNE(R2)                                                  
         L     R5,AP1                                                           
         B     RMED3                                                            
*                                                                               
RMEDTOT  DS    0H                  RECAP TOTAL LINE                             
         L     R5,AP1                                                           
         USING PLINED,R5                                                        
         L     R2,ARCTOT                                                        
         USING RCTOTD,R2                                                        
         MVC   FULL,RCGGRP                                                      
         BAS   R3,GIMPS            GET GOAL IMPS                                
         MVC   PDPT+5(5),=C'TOTAL'                                              
         EDIT  (B4,RCGDOL),(9,PGOLDOL)       GOAL DOLLARS                       
         MVC   FULL,RCGGRP                  GOAL GRP                            
         LA    R3,PGOLGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   DUB(4),RC1DEMA                                                   
         MVC   DUB+4(4),RCGGRP                                                  
         LA    RF,PGOLIND                                                       
         BAS   RE,EDIND                                                         
*                                                                               
         EDIT  (B4,RCACTDOL),(9,PACTDOL)                                        
*                                                                               
         MVC   FULL,RC1DEME                                                     
         LA    R3,PESTGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   FULL,RC1DEMA                                                     
         LA    R3,PACTGRP                                                       
         ST    R3,APOUT                                                         
         BAS   RE,EDITGRP                                                       
*                                                                               
         MVC   DUB(4),RC1DEMA                                                   
         MVC   DUB+4(4),RC1DEME                                                 
         LA    RF,PESTIND                                                       
         BAS   RE,EDIND                                                         
*                                                                               
         ZIC   R1,DEMONUM          BCT LIMIT                                    
         LA    R4,PDEMEST1                                                      
         LA    R3,RC2DEME                                                       
RM5      MVC   FULL,0(R3)                                                       
         ST    R4,APOUT                                                         
         BAS   RE,EDITGRP                                                       
         LA    RE,6(R4)                                                         
         ST    RE,APOUT                                                         
         MVC   FULL,4(R3)                                                       
         BAS   RE,EDITGRP                                                       
         MVC   DUB(4),4(R3)                                                     
         MVC   DUB+4(4),0(R3)                                                   
         LA    RF,16(R4)                                                        
****     B     RMNEWN5             ** NEW N5                                    
         BAS   RE,EDIND                                                         
         LA    R4,PDEMLENE(R4)                                                  
         LA    R3,RCDLENE(R3)                                                   
RMNEWN5  BCT   R1,RM5                                                           
*                                                                               
         L     R5,AP1                                                           
         LA    R5,132(R5)                                                       
         MVC   PDPT+6(3),=C'CPM'                                                
*                                                                               
         OC    GOALIMP,GOALIMP                                                  
         BZ    RM7                                                              
         OC    RCGDOL,RCGDOL                                                    
         BZ    RM7                                                              
         MVC   DUB(4),RCGDOL           GOAL CPM                                 
***      MVC   DUB+4(4),GOALIMP                                                 
         ICM   R1,15,GOALIMP                                                    
         SR    R0,R0                                                            
         M     R0,=F'10'                                                        
         ST    R1,DUB+4                                                         
         LA    RF,PGOLGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,GOALCPM                                                       
RM7      OC    RC1IMPE,RC1IMPE                                                  
         BZ    RM9                                                              
         MVC   DUB(4),RCACTDOL         EST CPM                                  
         MVC   DUB+4(4),RC1IMPE                                                 
         LA    RF,PESTGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,ESTCPM                                                        
RM9      OC    RC1IMPA,RC1IMPA                                                  
         BZ    RM11                                                             
         MVC   DUB(4),RCACTDOL         ACT CPM                                  
         MVC   DUB+4(4),RC1IMPA                                                 
         LA    RF,PACTGRP                                                       
         BAS   RE,EDCPM                                                         
         ST    RF,ACTCPM                                                        
RM11     OC    GOALCPM,GOALCPM    DO TARGET INDEX                               
         BZ    RM13               IF GOALCPM=0/SKIP                             
         OC    ESTCPM,ESTCPM                                                    
         BZ    RM15                                                             
         L     RE,GOALCPM                                                       
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,ESTCPM                                                        
         LA    R2,PGOLIND                                                       
****     BAS   RE,EDIND                                                         
*                                                                               
RM13     OC    ESTCPM,ESTCPM      DO ACTUAL INDEX                               
         BZ    RM15               IF ESTCPM OR ACTCPM = 0/SKIP                  
         OC    ACTCPM,ACTCPM                                                    
         BZ    RM15                                                             
         L     RE,ACTCPM                                                        
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,ESTCPM                                                        
         LA    R2,PESTIND                                                       
****     BAS   RE,EDIND                                                         
*                                                                               
RM15     DS    0H                  LOOP TO DO DEMOS AFTER TARGET                
         ZIC   R3,DEMONUM          BCT LIMIT                                    
         L     R2,ARCTOT           RESET ADDR OF TOTREC                         
         MVC   ACTDOLSV,RCACTDOL                                                
         LA    R4,RC2IMPE                                                       
         LA    R2,PDEMEST1                                                      
RMLOOP   XC    ACTCPM(8),ACTCPM                                                 
         OC    0(4,R4),0(R4)       CHK EST IMP                                  
         BNZ   RM17                                                             
         LA    R2,6(R2)                                                         
         B     RM19                                                             
RM17     MVC   DUB(4),ACTDOLSV                                                  
         MVC   DUB+4(4),0(R4)                                                   
         LR    RF,R2                                                            
         BAS   RE,EDCPM                                                         
         ST    RF,ESTCPM                                                        
         LA    R2,6(R2)                                                         
         OC    4(4,R4),4(R4)                                                    
         BZ    RM19                                                             
         MVC   DUB(4),ACTDOLSV                                                  
         MVC   DUB+4(4),4(R4)                                                   
         LR    RF,R2                                                            
         BAS   RE,EDCPM                                                         
         ST    RF,ACTCPM                                                        
RM19     LA    R2,6(R2)            BUMP TO INDEX OUT FIELD                      
         OC    ESTCPM,ESTCPM                                                    
         BZ    RM21                                                             
         OC    ACTCPM,ACTCPM                                                    
         BZ    RM21                                                             
         L     RE,ACTCPM                                                        
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,ESTCPM                                                        
****     BAS   RE,EDIND                                                         
RM21     LA    R2,5(R2)                                                         
         LA    R4,RCDLENE(R4)                                                   
         BCT   R3,RMLOOP                                                        
*                                                                               
         BAS   RE,PRINTIT                                                       
RMEDX    B     XIT                                                              
         DROP  R5,R2                                                            
*                                                                               
**********************************                                              
* PRINT COMBINED MEDIA IN ORDER (NCSZ/Z=OTHER)                                  
*                                                                               
SETMED   NTR1                                                                   
         LA    R4,4                                                             
SETM1    LA    R2,RMAREA                                                        
         USING RMMEDIA,R2                                                       
         ZIC   R1,ORDNUM           POINTS TO MEDIA                              
         LA    R3,ORDER            MEDIA LIST                                   
         AR    R3,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,ORDNUM                                                        
SETM5    CLC   0(1,R3),RMMEDIA                                                  
         BE    SETMX                                                            
         LA    R2,RMRECLNE(R2)                                                  
         OC    0(10,R2),0(R2)                                                   
         BNZ   SETM5                                                            
         BCT   R4,SETM1                                                         
SETMX    ST    R2,ARMAREA                                                       
         XIT1  REGS=(R2)                                                        
         DROP  R2                                                               
         EJECT                                                                  
*************************************                                           
* GET NEW ESTIMATE HEADER                                                       
*     AND STORE EDEMLST IN NDDEMOS                                              
*********************************                                               
* INPUT     3 CL PROD CODE IN WORK                                              
* OUTPUT    NDDEMOS                                                             
*                                                                               
NEWEST   NTR1                                                                   
**       GOTO1 =A(NEST),DMCB,(R9),(R7),(R6),RR=RELO                             
***NEST     CSECT                                                               
*         NMOD1 0,EST                                                           
*         L     R9,0(R1)                                                        
*         USING NETSYSD,R9                                                      
*         L     R7,4(R1)                                                        
*         USING MYD,R7                                                          
*         L     R6,8(R1)                                                        
*         USING NDDEMBLK,R6                                                     
*                                                                               
         OC    DEMOSV,DEMOSV       IF DEMO OVERRIDES                            
         BNZ   NEX                 THEN SKIP                                    
         CLC   WORK(3),=C'UNA'     IF UNALLOCATED PROD                          
         BNE   NW2                    CLEAR NDDEMOS                             
         XC    NDDEMOS,NDDEMOS        AND EXIT                                  
         B     NEX                                                              
NW2      NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   KEY+4(3),WORK               SET 3CHAR PROD CODE                  
         CLI   NBSELEST,0                                                       
         BE    *+10                                                             
         MVC   KEY+7(1),NBSELEST                                                
         MVC   FILENAME,=C'SPTDIR  '                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     NW4                                                              
NW3      MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
NW4      LA    R1,6                                                             
         CLI   NBSELEST,0                                                       
         BE    *+8                                                              
         LA    R1,7                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+8(5),KEYSAVE+8                                               
         BNE   NW3                                                              
NW6      MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,ANETWS1,DMWORK           
         L     R4,ANETWS1                                                       
         USING ESTHDR,R4                                                        
         CLI   NBSELEST,0                                                       
         BNE   NW5                                                              
         CLC   ESTART,NBSELEND                                                  
         BH    NW3                                                              
         CLC   EEND,NBSELSTR                                                    
         BL    NW3                                                              
NW5      MVC   NDDEMOS,EDEMLST                                                  
*                                                                               
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         XC    FILENAME,FILENAME                                                
NEX      B     XIT                                                              
*                                                                               
*        LTORG                                                                  
*        XIT1                                                                   
*                                                                               
         EJECT                                                                  
*****************************************                                       
* GETS DISPLACEMENT OF PRINT LINE                                               
* TO CENTER OUTPUT                                                              
*                                                                               
* OUTPUT AP1,AH13,ABOXCOLS                                                      
*****************************************                                       
         SPACE                                                                  
GETDISP  DS    0H                                                               
         ZIC   R1,DEMONUM          DEMONUM CONTAINS NO OF WEEKS                 
         MH    R1,=H'16'           16=LENGTH OF GRP FIELDS                      
         AH    R1,=H'60'           60=REQUIRED FIELDSL FIELD                    
         LA    R2,132                                                           
         SR    R2,R1                                                            
         LTR   R2,R2                                                            
         BZ    GTD5                                                             
         BP    *+6                                                              
         DC    H'0'                MUST NOT  BE NEGATIVE                        
         SRA   R2,1                DIV BY 2(SRA IGNORES REMAINDER)              
         SPACE                                                                  
* R2 NOW CONTAINS NUMBER TO BE ADDED TO P LINES                                 
GTD5     LTR   R2,R2               IF R2=0,ADD 1 FOR BOXES                      
         BNZ   *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R1,P1                                                            
         AR    R1,R2                                                            
         ST    R1,AP1              SET START OF P LINE                          
         LA    R1,H13                                                           
         AR    R1,R2                                                            
         ST    R1,AH13             SET START OF HEADLINE                        
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         LA    R1,BOXCOLS                                                       
         BCTR  R1,0                SUBTRACT ONE FOR BOX                         
         AR    R1,R2                                                            
         ST    R1,ABOXCOLS         SET START OF BOXES                           
         BR    R5                                                               
         DROP  R3                                                               
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
**********************                                                          
*  TEST PURPOSES                                                                
*                                                                               
*PRNTU    NTR1                                                                  
*         GOTO1 HEXOUT,DMCB,NBACTAM,P,20                                        
*         GOTO1 HEXOUT,DMCB,NDESTDEM,P+42,24                                    
*         BAS   RE,PRINTIT                                                      
*         B     XIT                                                             
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+17(20),SPLCLIN                                                
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),CURPRD                                                  
         MVC   H4+17(20),PRODNAME                                               
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(6),SPLEST                                                  
         MVC   H5+17(20),SPLESTN                                                
         DROP  R5                                                               
         SPACE                                                                  
         L     R2,AH13                                                          
         USING PLINED,R2                                                        
         MVC   PDPT+132(7),=C'DAYPART'                                          
         MVC   PMED+132(5),=C'MEDIA'                                            
         MVC   PGOLDOL+6(7),=C'DOLLARS'                                         
         MVC   PGOLDOL+135(4),=C'GOAL'                                          
         MVC   PACTDOL+136(3),=C'ACT'                                           
         MVC   PGOLGRP+133(4),=C'GOAL'                                          
         MVC   PESTGRP+134(3),=C'EST'                                           
         MVC   PACTGRP+134(3),=C'ACT'                                           
         MVC   PGOLIND+2(3),=C'IND'                                             
         MVC   PGOLIND+143(4),=C'GOAL'   ** NEW N5                              
         MVC   PDEMACT1+139(7),=C'GOL EST' ** NEW N5                            
         MVC   PDEMACT1+9(3),=C'IND'                                            
         MVC   PGOLIND+133(3),=C'GOL'                                           
         MVC   PESTIND+132(3),=C'EST'                                           
         ZIC   R1,DEMONUM                                                       
         LTR   R1,R1                                                            
         BZ    DEMNAME                                                          
         LA    R2,PDEMEST1+132                                                  
HDLOOP   MVC   2(3,R2),=C'EST'                                                  
         MVC   8(3,R2),=C'ACT'                                                  
         B     DEMNAME             ** NEW N5                                    
*        MVC   12(3,R2),=C'IND'                                                 
*        LA    R3,12(R2)                                                        
*        S     R3,=F'132'                                                       
*        MVC   0(3,R3),=C'GRP'                                                  
*        LA    R2,PDEMLENE(R2)                                                  
*        BCT   R1,HDLOOP                                                        
*                                                                               
DEMNAME  L     R2,AH13             GET DEMO NAMES                               
         LA    R4,DBLOCK                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBCOMFCS,ACOMFACS   *SET FOR DEMOCON                             
         MVC   DBFILE,=C'NTI'      *                                            
         MVI   DBSELMED,C'N'       *                                            
         LA    R3,NDDEMOS                                                       
         L     R1,ANETWS1        ****                                           
         MVC   0(10,R1),0(R3)    ****                                           
         LA    R5,PGOLGRP+4                                                     
         GOTO1 ADEMOCON,DMCB,(1,(R3)),(2,(R5)),(0,(R4))                         
*        LA    R5,27(R5)                                                        
         LA    R5,33(R5)                                                        
         LA    R3,3(R3)                                                         
         ZIC   R2,DEMONUM                                                       
         LTR   R2,R2                                                            
         BZ    HDBOX                                                            
DEMLOP   GOTO1 ADEMOCON,DMCB,(1,(R3)),(2,(R5)),(0,(R4))                         
         LA    R5,PDEMLENE(R5)                                                  
         LA    R3,3(R3)                                                         
         BCT   R2,DEMLOP                                                        
         SPACE                                                                  
HDBOX    DS    0H                   SET PARAMS FOR BOXES                        
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
         DROP  R2                                                               
         L     R5,ABOXCOLS                                                      
         USING PLINED,R5                                                        
         MVI   PDPT,C'L'                                                        
         MVI   PMED,C'C'                                                        
         MVI   PGOLDOL,C'C'                                                     
         MVI   PGOLGRP,C'C'                                                     
         ZIC   R4,DEMONUM                                                       
         LTR   R4,R4                                                            
         BNZ   BOX5                                                             
         MVI   PDEMEST1,C'R'                                                    
         B     BOX7                                                             
BOX5     LA    R2,PDEMEST1-7       NEW N5                                       
         MVI   0(R2),C'C'                                                       
         LA    R2,PDEMLENE(R2)                                                  
         BCT   R4,*-8                                                           
         MVI   0(R2),C'R'                                                       
         SPACE                                                                  
BOX7     LA    R5,BOXROWS                                                       
         LA    R5,11(R5)                                                        
         MVI   0(R5),C'T'                                                       
         LA    R5,3(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,46(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,50,C'PRODUCT POST ANALYSIS'                                   
         SSPEC H2,50,C'---------------------'                                   
         SSPEC H3,50,PERIOD                                                     
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,98,REPORT                                                     
         SSPEC H4,98,RUN                                                        
         SSPEC H5,125,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
***********************************                                             
* GET IMPRESSION FOR GOALS -(UNIV X GRP = IMPS (ROUNDED))                       
* INPUT: GOALGRP IN FULL                                                        
GIMPS    DS    0H                                                               
         USING BINRECD,R2                                                       
         XC    GOALIMP,GOALIMP                                                  
         OC    FULL,FULL           NO GOAL GRP/SKIP                             
         BZ    GIX                                                              
         MVC   HALF,FULL+2                                                      
         L     R0,UNIV                                                          
         MH    R0,HALF             GOAL GRP                                     
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,GOALIMP                                                       
GIX      BR    R3                                                               
         SPACE                                                                  
*                                                                               
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BINTABLE DS    CL50000                                                          
         DS    CL50000                                                          
         DS    CL20000                                                          
NETBUFFA DS    CL1000                                                           
UNIVWORK DS    CL2000                                                           
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
BINDMCB  DS    6F                                                               
RELO     DS    A                                                                
ACLIST   DS    A                                                                
ANETGOAL DS    A                                                                
ADEMOCON DS    A                                                                
ACURBIN  DS    A                   ADDRESS OF CURRENT BINREC                    
UNIV     DS    F                                                                
GOALIMP  DS    F                                                                
DEMOSV   DS    CL15                                                             
DEMONUM  DS    CL1          NUMBER OF DEMOS REQUESTED BEYOND TARGET             
FRST     DS    CL1                                                              
PEROPT   DS    CL1                                                              
PREVREC  DS    CL1                                                              
CURDPT   DS    CL1                                                              
CURMED   DS    CL1                                                              
CURPRD   DS    CL3                                                              
CUREST   DS    CL1                                                              
PERTYPE  DS    CL4                                                              
PRDCDSV  DS    CL3                                                              
DPTNAME  DS    CL8                                                              
MYWORK   DS    CL150                                                            
PRODNAME DS    CL20                                                             
EDIN     DS    CL4                                                              
EDOUT    DS    CL5                                                              
*                                                                               
ABOXCOLS DS    F                                                                
AH13     DS    F                                                                
AP1      DS    F                                                                
ABINTABL DS    F                                                                
BOXSET   DS    CL1                                                              
PRODCODE DS    CL3                                                              
APOUT    DS    F                                                                
ACTCPM   DS    F                                                                
ESTCPM   DS    F                                                                
GOALCPM  DS    F                                                                
ACTIMP   DS    F                                                                
ESTIMP   DS    F                                                                
ACTDOLSV DS    F                                                                
ARMAREA  DS    F                                                                
ARCTOT   DS    F                                                                
NBAIOSV  DS    F                   PXZ                                          
NUMPER   DS    CL1                                                              
SRTPRD1  DS    CL1                                                              
SRTPRD3  DS    CL3                                                              
SRT1     DS    CL1                                                              
MEDSV    DS    CL1                                                              
MEDXSV   DS    CL1                                                              
ORDNUM   DS    CL1                                                              
ORDER    DS    CL4                                                              
PRD1SV   DS    CL1                                                              
*                                                                               
*                                                                               
DPTTOT   DS    0CL1           TOTAL LINE FOR DAYPART                            
DPRD     DS    CL3                 PRODUCT                                      
DDPT     DS    CL1                 DAYPART                                      
DMEDIA   DS    CL1                 MEDIA                                        
DKEYLENE EQU   *-DPRD                                                           
DGOAL    DS    CL1                 TARGET DEMO                                  
DEST     DS    CL1                 EST                                          
DGGRP    DS    F                   GOAL GRP                                     
DGDOL    DS    F                   GOAL DOLLARS                                 
DACTDOL  DS    F                   ACTUAL DOLLARS                               
D1DEMO   DS    CL3                 DEMO                                         
D1DEME   DS    CL4                 EST DEM                                      
D1DEMA   DS    CL4                 ACT DEM                                      
D1IMPE   DS    CL4                 EST IMP                                      
D1IMPA   DS    CL4                 ACT IMP                                      
DDEMLENE EQU   *-D1DEMO                                                         
D2DEMO   DS    CL3                                                              
D2DEME   DS    CL4                                                              
D2DEMA   DS    CL4                                                              
D2IMPE   DS    CL4                                                              
D2IMPA   DS    CL4                                                              
D3DEMO   DS    CL3                                                              
D3DEME   DS    CL4                                                              
D3DEMA   DS    CL4                                                              
D3IMPE   DS    CL4                                                              
D3IMPA   DS    CL4                                                              
D4DEMO   DS    CL3                                                              
D4DEME   DS    CL4                                                              
D4DEMA   DS    CL4                                                              
D4IMPE   DS    CL4                                                              
D4IMPA   DS    CL4                                                              
D5DEMO   DS    CL3                                                              
D5DEME   DS    CL4                                                              
D5DEMA   DS    CL4                                                              
D5IMPE   DS    CL4                                                              
D5IMPA   DS    CL4                                                              
DRECLENE EQU   *-DPRD                                                           
*                                                                               
RMAREA   DS    CL500                                                            
*                                                                               
MYDLENE  EQU   *-BINDMCB                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
BINRECD  DSECT                                                                  
BPRD     DS    CL3                 PRODUCT CODE                                 
BDPT     DS    CL1                 DAYPART                                      
BMEDX    DS    CL1                                                              
BMEDIA   DS    CL1                 MEDIA                                        
BKEYLENE EQU   *-BPRD                                                           
BGOAL    DS    CL1                 TARGET DEMO                                  
BEST     DS    CL1                 EST                                          
BGGRP    DS    F                   GOAL GRP                                     
BGDOL    DS    F                   GOAL DOLLARS                                 
BACTDOL  DS    F                   ACTUAL DOLLARS                               
B1DEMO   DS    CL3                 DEMO                                         
B1DEME   DS    CL4                 EST DEM                                      
B1DEMA   DS    CL4                 ACT DEM                                      
B1IMPE   DS    CL4                 EST IMP                                      
B1IMPA   DS    CL4                 ACT IMP                                      
BDEMLENE EQU   *-B1DEMO                                                         
B2DEMO   DS    CL3                                                              
B2DEME   DS    CL4                                                              
B2DEMA   DS    CL4                                                              
B2IMPE   DS    CL4                                                              
B2IMPA   DS    CL4                                                              
B3DEMO   DS    CL3                                                              
B3DEME   DS    CL4                                                              
B3DEMA   DS    CL4                                                              
B3IMPE   DS    CL4                                                              
B3IMPA   DS    CL4                                                              
B4DEMO   DS    CL3                                                              
B4DEME   DS    CL4                                                              
B4DEMA   DS    CL4                                                              
B4IMPE   DS    CL4                                                              
B4IMPA   DS    CL4                                                              
B5DEMO   DS    CL3                                                              
B5DEME   DS    CL4                                                              
B5DEMA   DS    CL4                                                              
B5IMPE   DS    CL4                                                              
B5IMPA   DS    CL4                                                              
BRECLENE EQU   *-BPRD                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
RMTOTD   DSECT              TOTALS FOR RECAP BY MEDIA                           
RMMEDIA  DS    CL1                 MEDIA                                        
RMKLENE  EQU   *-RMMEDIA                                                        
RMGOAL   DS    CL1                 TARGET DEMO                                  
RMEST    DS    CL1                 EST                                          
RMGGRP   DS    F                   GOAL GRP                                     
RMGDOL   DS    F                   GOAL DOLLARS                                 
RMACTDOL DS    F                   ACTUAL DOLLARS                               
RM1DEMO  DS    CL3                 DEMO                                         
RM1DEME  DS    CL4                 EST DEM                                      
RM1DEMA  DS    CL4                 ACT DEM                                      
RM1IMPE  DS    CL4                 EST IMP                                      
RM1IMPA  DS    CL4                 ACT IMP                                      
RMDLENE  EQU   *-RM1DEMO                                                        
RM2DEMO  DS    CL3                                                              
RM2DEME  DS    CL4                                                              
RM2DEMA  DS    CL4                                                              
RM2IMPE  DS    CL4                                                              
RM2IMPA  DS    CL4                                                              
         DS    CL3                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL3                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL3                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
RMRECLNE EQU   *-RMMEDIA                                                        
         EJECT                                                                  
*                                                                               
RCTOTD   DS    0CL1         TOTALS FOR RECAP                                    
RCPRD    DS    CL3                 PRODUCT                                      
RCDPT    DS    CL1                 DAYPART                                      
RCMEDIA  DS    CL1                 MEDIA                                        
RCKLENE  EQU   *-RCPRD                                                          
RCGOAL   DS    CL1                 TARGET DEMO                                  
RCEST    DS    CL1                 EST                                          
RCGGRP   DS    F                   GOAL GRP                                     
RCGDOL   DS    F                   GOAL DOLLARS                                 
RCACTDOL DS    F                   ACTUAL DOLLARS                               
RC1DEMO  DS    CL3                 DEMO                                         
RC1DEME  DS    CL4                 EST DEM                                      
RC1DEMA  DS    CL4                 ACT DEM                                      
RC1IMPE  DS    CL4                 EST IMP                                      
RC1IMPA  DS    CL4                 ACT IMP                                      
RCDLENE  EQU   *-RC1DEMO                                                        
RC2DEMO  DS    CL3                                                              
RC2DEME  DS    CL4                                                              
RC2DEMA  DS    CL4                                                              
RC2IMPE  DS    CL4                                                              
RC2IMPA  DS    CL4                                                              
         DS    CL3                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL3                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL3                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
RCRECLNE EQU   *-RCPRD                                                          
         EJECT                                                                  
*                                                                               
PLINED   DSECT                    PRINT LINE DSECT                              
PDPT     DS    CL7                                                              
         DS    CL1                                                              
PMED     DS    CL5                                                              
         DS    CL1                                                              
PGOLDOL  DS    CL9                                                              
         DS    CL1                                                              
PACTDOL  DS    CL9                                                              
         DS    CL1                                                              
PGOLGRP  DS    CL5                                                              
         DS    CL1                                                              
PESTGRP  DS    CL5                                                              
         DS    CL1                                                              
PACTGRP  DS    CL5                                                              
         DS    CL1                                                              
PGOLIND  DS    CL4                                                              
         DS    CL1                                                              
PESTIND  DS    CL4                                                              
         DS    CL1                                                              
         DS    CL7                 ***                                          
PDEMEST1 DS    CL5                                                              
         DS    CL1                                                              
PDEMACT1 DS    CL5                                                              
         DS    CL11                ***                                          
         DS    CL1                                                              
PDEMIND1 DS    CL4                                                              
         DS    CL1                                                              
PDEMLENE EQU   *-PDEMEST1                                                       
PDEMEST2 DS    CL5                                                              
         DS    CL1                                                              
PDEMACT2 DS    CL5                                                              
         DS    CL1                                                              
PDEMIND2 DS    CL4                                                              
         DS    CL1                                                              
PDEMEST3 DS    CL5                                                              
         DS    CL1                                                              
PDEMACT3 DS    CL5                                                              
         DS    CL1                                                              
PDEMIND3 DS    CL4                                                              
         DS    CL1                                                              
PDEMEST4 DS    CL5                                                              
         DS    CL1                                                              
PDEMACT4 DS    CL5                                                              
         DS    CL1                                                              
PDEMIND4 DS    CL4                                                              
         EJECT                                                                  
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE2D                                                       
****** ++INCLUDE DRDICFILE                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEWRI23X  05/01/02'                                      
         END                                                                    
