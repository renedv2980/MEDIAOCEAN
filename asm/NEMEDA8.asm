*          DATA SET NEMEDA8    AT LEVEL 039 AS OF 05/01/02                      
*PHASE T31EA8A                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'T31EA8 - WEEKLY PROGRAM FLOW '                                  
T31EA8   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEA8**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING T31EA8,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          R7-WORKING STORAG (ANETWS2+500)              
         LA    R7,500(R7)                                                       
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R6,ANETWS3                                                       
         USING NDDEMBLK,R6         R6-ANETWS3/NDDEMBLK,DEDBLOCK                 
         ST    R6,NBADEM                                                        
         L     R1,ANETWS1                                                       
         ST    R1,ACLISTSV         ANETWS1=CLISTSV                              
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                  ANETWS4 (5000CL) FOR BINTABLE                
*                                  ANETWS2 FOR 500CL IS FREE                    
         EJECT                                                                  
*HIPO******************************************************************         
*  TITLE: NEMEDA8 (T31EA8) PROGRAM FLOWCHART                         *          
*                                                                     *         
*  COMMENTS: WRITES A REPORT IN PROGRAM ORDER SHOWING UNITS           *         
*            UNDER WEEKS                                                        
*                                                                     *         
*  CALLS TO: NETIO                                                              
*                                                                     *         
*  GLOBAL: R7-MYWORKD (ANETWS2+500)                                   *         
*                                                                     *         
***********************                                               *         
*  LOGIC:  READ UNITS IN PROGRAM ORDER THROUGH NETIO                  *         
*          USE BINSRCH TO SET IN BINTABLE                             *         
*          AT PROGRAM BREAK WRITE TO REPORT                           *         
*          THEN CONTINUE SAME WITH NEXT PROGRAM'S UNITS               *         
*          OVERALL TOTALS AT END                                      *         
*                                                                     *         
*                                                                     *         
*ENDHIPO***************************************************************         
         SPACE 3                                                                
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************                                           
* VALIDATE REQUEST SCREEN DATA                                                  
*                                                                               
VK       DS    0H                                                               
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
*                                                                               
         LA    R2,SPLCLIH              CLIENT                                   
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
         L     R3,NBAIO                                                         
         USING CLTHDR,R3                                                        
         L     RF,ACLISTSV                                                      
         MOVE  ((RF),880),CLIST                                                 
         DROP  R3                                                               
*                                                                               
         MVI   FTERMFLG,1          SET OPTIONAL FLAG                            
*                                                                               
         LA    R2,SPLPROH               PRODUCT                                 
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK10                                                             
         CLC   8(3,R2),=C'POL'                                                  
         BE    VK10                                                             
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
VK10     LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
*        MVC   DEMSV,NDDEMOS       SAVE TARGET DEMO                             
         CLI   NDDEMOS,X'00'          TEST IF ANY DEMOS                         
         BNE   VK12                   IF NOT/MEANS RANGE                        
         L     R3,NBAIO               SO GET THEM                               
         USING ESTHDR,R3                                                        
*        MVC   DEMSV,EDEMLST                                                    
         MVC   NDDEMOS,EDEMLST                                                  
VK12     OI    SPLESTNH+6,X'80'                                                 
*                                                                               
VK20     LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
VK25     LA    R2,SPLPAKH                                                       
         NETGO NVPAKLOK,DMCB,SPLPAKN                                            
         OI    SPLPAKNH+6,X'80'                                                 
*                                                                               
VK30     DS    0H                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPTALL,DMCB                                                    
*                                                                               
VK40     LA    R2,SPLSDTH                                                       
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
VK50     LA    R2,SPLEDTH                                                       
         NETGO NVENDDAT,DMCB,98           MAX IS 14 WEEKS                       
*                                                                               
VK60     LA    R2,SPLACTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK70                                                             
         MVI   ERROR,INVALID                                                    
         CLI   SPLACT,C'Y'                                                      
         BNE   TRAPERR                                                          
         MVI   ACTFLG,C'Y'                                                      
*                                                                               
VK70     LA    R2,SPLPGEH          PAGE BREAK ON NEW NTWK                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK72                                                             
         CLI   SPLPGE,C'Y'                                                      
         BNE   TRAPERR                                                          
         MVI   NTWKPGE,C'Y'                                                     
*                                                                               
VK72     LA    R2,SPLSKPH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK75                                                             
         CLI   8(R2),C'D'                                                       
         BE    VKEXIT                                                           
         CLI   8(R2),C'T'                                                       
         BZ    VKEXIT                                                           
*                                                                               
VK75     DS    0H                                                               
*                                  DIG OUT ANY OPTIONS                          
         LA    R2,SPLOPTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    VKEXIT                                                           
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
         SPACE 1                                                                
VK77     CLC   12(4,R3),=C'NOCOST'      OPTION TO SUPPRESS COST FIELDS          
         BNE   VK77A                                                            
         MVI   NOCOST,C'Y'                                                      
         B     VK77X                                                            
VK77A    CLC   12(4,R3),=C'ASSIGNED'    OPTION FOR ASSIGNED DOLLARS             
         BNE   EDINV                                                            
         MVI   ASSCOST,C'Y'                                                     
VK77X    LA    R3,32(R3)                                                        
         BCT   R0,VK77                                                          
         SPACE 1                                                                
VKEXIT   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         DROP  R5                                                               
         EJECT                                                                  
*****************************************                                       
* READ/CALCULATE/EDIT/PRINT THE UNIT RECS                                       
*                                                                               
LR       DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   LR1                                                              
         L     R2,=A(OFTABLE)          FUDGE ANETWS4 IF OFFLINE                 
         A     R2,RELO                                                          
         ST    R2,ANETWS4                                                       
LR1      BAS   RE,GTWKLIST         WEEKLIST                                     
         BAS   RE,CHKDUPS          CHK FOR DUP DATES IN PERLIST                 
         L     R2,NUMPER           SET PERLIST TO WEEKTOTS                      
         LTR   R2,R2               MAKE SURE AT LEAST ONE WEEK                  
         BNP   EXIT                                                             
         LA    R3,PERLIST                                                       
         LA    R4,WEEKTOTS                                                      
LR5      MVC   0(4,R4),0(R3)                                                    
         LA    R4,12(R4)                                                        
         LA    R3,4(R3)                                                         
         BCT   R2,LR5                                                           
         BAS   RE,GETDISP          CENTER OUTPUT                                
         BAS   RE,SETBIN           SET UP BINTABLE/PERLIST TO TABLE             
*                                                                               
         MVI   NBDATA,C'U'         * GET UNIT RECS                              
         MVI   NBSPLOPT,C'Y'       SPLIT OPTION                                 
         MVI   NBSEQ,C'Q'                                                       
         NETGO NVDEMOPT,DMCB      ACTUAL SCHED/EST DEMO FOR MAKE GOOD           
         CLI   ACTFLG,C'Y'                                                      
         BNE   LR10                                                             
         MVI   NBESTOPT,0                                                       
         MVI   NBACTOPT,C'Y'                                                    
LR10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BE    PROCUN                                                           
         CLI   NBMODE,NBREQLST                                                  
         BNE   LR10                                                             
         BAS   RE,PRINTIT         END OF RECORDS                                
         MVI   SPACING,2                                                        
         BAS   RE,SPOOLIT                                                       
         BAS   RE,NTWKTOTS                                                      
         MVI   SPACING,2                                                        
         BAS   RE,SPOOLIT                                                       
         L     R3,AP1                                                           
         USING PLINED,R3                                                        
         MVC   PNET(26),=C'*** ALL NETWORK TOTALS ***'                          
         BAS   RE,SPOOLIT                                                       
         EDIT  ALLUNITS,(4,PUTOT)                                               
         LA    R1,ALLTOTS                                                       
         LA    R5,PUTOT+5                                                       
         LA    R3,132(R5)                                                       
         L     R4,NUMPER                                                        
LR10A    CLI   130(R5),X'40'       IS PREV SPACE FILLED                         
         BE    LR10B                                                            
         EDIT  (B4,0(R1)),(7,132(R3))    YES/PRINT ON ALTERNATE LINE            
         EDIT  (B4,4(R1)),(4,132(R5))                                           
         B     LR10C                                                            
LR10B    EDIT  (B4,0(R1)),(7,0(R3))                                             
         EDIT  (B4,4(R1)),(4,0(R5))                                             
LR10C    LA    R5,7(R5)                                                         
         LA    R3,7(R3)                                                         
         LA    R1,8(R1)                                                         
         BCT   R4,LR10A                                                         
         BAS   RE,SPOOLIT                                                       
         B     LRX                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
PROCUN   DS    0H                                                               
         BAS   RE,GETDAT           GET WEEK START DATE                          
         CLI   WORK,X'FF'          CHK DATE OUT OF RANGE                        
         BE    LR10                    .YES/SKIP AND GET NXT UNIT               
         CLI   PROGSV,0            IS IT FIRST TIME                             
         BE    LR11                                                             
*                                                                               
         CLC   PROGSV,NBACTPRG     IS IT SAME PROGRAM                           
         BNE   LR10D                                                            
         CLC   NETSV,NBACTNET      AND SAME NET                                 
         BE    LR12                   .YES                                      
LR10D    BAS   RE,PRINTIT             .NO/PRINT BINTABLE                        
         MVI   SPACING,2                                                        
         BAS   RE,SPOOLIT                /PRINT BLANK LINE                      
         BAS   RE,SETBIN                 /CLEAR N RESET BINTABLE                
*                                                                               
         CLC   NETSV,NBACTNET      IS IT SAME NETWORK                           
         BE    LR11                                                             
         BAS   RE,NTWKTOTS         NO/PRINT NTWK TOTS                           
         CLI   NTWKPGE,C'Y'        CHK IF PAGEBRK ON NEW NTWK                   
         BNE   *+12                                                             
         MVI   FORCEHED,C'Y'       YES/FORCE HEAD                               
         B     LR11                                                             
         MVI   SPACING,3                                                        
         BAS   RE,SPOOLIT                                                       
*                                                                               
LR11     MVC   PROGSV,NBACTPRG              SET PROGRAM CODE                    
         MVC   PROGNAM,NBPROGNM                 AND NAME                        
         MVC   NETSV,NBACTNET                   NETWORK                         
         XC    RTGTOT(8),RTGTOT            CLEAR PROG TOTS                      
*                                                                               
LR12     L     R2,ANETWS2                                                       
         USING BINRECD,R2                                                       
         XC    0(BINRECLN,R2),0(R2)                                             
         MVC   BINDATE,NBACTDAT      SET DATE                                   
         MVC   BINPRD,NBSPLPRN       SET PRD                                    
         CLI   NOCOST,C'Y'                                                      
         BE    LR12A                                                            
         MVC   BINCOST,NBACTUAL      SET ACTUAL COST                            
         CLI   ASSCOST,C'Y'        BUT IS IT ASSIGNED                           
         BNE   *+10                                                             
         MVC   BINCOST,NBASSIGN                                                 
LR12A    L     R1,UNITOT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,UNITOT           ADD TO UNIT COUNT                            
         LA    R4,NDESTDEM                                                      
         CLI   ACTFLG,C'Y'                                                      
         BNE   *+8                                                              
         LA    R4,NDACTDEM                                                      
         MVC   HALF,2(R4)                                                       
         L     R1,RTGTOT                                                        
         AH    R1,HALF                                                          
         ST    R1,RTGTOT           ADD TO RTG TOT                               
*                                                                               
LR13     GOTO1 =V(BINSRCH),BINDMCB,(1,(R2)),RR=RELO                             
         L     R3,0(R1)                                                         
         LTR   R3,R3               TEST BINTABLE FULL                           
         BNZ   LR15                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                IF YES/BOMB                                  
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=C'*** ERROR TOO MANY RECORDS ***'                   
         GOTO1 ERREX2                                                           
         DROP  R5                                                               
LR15     CLI   0(R1),X'01'     TEST IF REC NOT FOUND(SO INSERTED)               
         BE    LR10                YES/ SO GET NEXT UNIT                        
         DROP  R2                                                               
         USING BINRECD,R3                                                       
         ZIC   R4,BINNUM       *IF BINRECORD FOUND, GET CNTR NUMBER             
         LA    R4,1(R4)         *OF BINREC, BUMP IT N ZIC INTO NEW REC          
         DROP  R3                                                               
         USING BINRECD,R2                                                       
         STC   R4,BINNUM        *THEN GO ADD NEW REC TO BINTBL                  
         B     LR13                                                             
         DROP  R2                                                               
*                                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* STEPS THROUGH BINTABLE PRINTING DATA                                          
*                                                                               
*   BINTABLE CONSISTS OF DUMMY BINRECS WITH PERLIST START DATE                  
*   PLUS (UNIT)BINRECS WITH DATES THAT FALL WITHIN PERLIST DATES                
*     BINREC=DATE-PRD-COST-SUBLINE NUMBER(ADDED ONLY FOR DUPS)                  
*                                                                               
*   BINDISP=DISP INTO BINTABLE TO POINT TO DESIRED REC                          
*           SET AT START TO BINRECLN TO SKIP DUMMY WEEK START RECS.             
*           AT END OF EACH PASS THROUGH BINRECS, BUMPED BY LENGTH               
*           OF BINREC TO POINT TO NEXT REC IN EACH WEEK GROUP                   
*                                                                               
* LOCALS: R2- POINTS TO START OF CURRENT WEEK IN BINTABLE                       
*         R3- PRINT LINE                                                        
*         R4-PERLIST                                                            
***********************************************                                 
PRINTIT  NTR1                                                                   
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         MVI   FRST,0                                                           
         LA    R1,BINRECLN         SET DISP INTO BINTABLE TO SKIP               
         ST    R1,BINDISP          DUMMY WEEK START BINRECS                     
PRT10    L     R2,ANETWS4                                                       
         USING BINRECD,R2                                                       
         L     R3,AP1                                                           
         USING PLINED,R3                                                        
         LA    R4,PERLIST                                                       
         CLI   FRST,0                                                           
         BNE   PRT15                                                            
         MVI   FRST,1            FIRST TIME                                     
         CLC   PNETSV,NETSV          CHK IF NET ALREADY SET                     
         BE    PRT12                                                            
         MVC   PNET,NETSV            SET NETWORK                                
         MVC   PNETSV,NETSV                                                     
PRT12    MVC   PPROG,PROGSV          SET PROGRAM                                
         CLI   SPLNAM,C'Y'         IF OPTION ON                                 
         BNE   *+10                                                             
         MVC   PPROG+132(16),PROGNAM                                            
         EDIT  UNITOT,(3,PUTOT)      SET UNIT TOTAL                             
         L     R1,UNITOT             ROLL PROGRAM UNITS TO NTWKUNITS            
         LTR   R1,R1                                                            
         BZ    PRT15                                                            
         A     R1,NTWKUNIT                                                      
         ST    R1,NTWKUNIT                                                      
         L     R1,RTGTOT           GET RTG AVERAGE                              
         M     R0,=F'1'                                                         
         D     R0,UNITOT                                                        
         ST    R1,RTGTOT           SET RTG AV IN RTGTOT                         
         EDIT  (R1),(4,PAVRTG),1                                                
PRT15    BAS   RE,INDATES          CHK IF BINREC IN PERLIST WEEK                
         BNE   PRT20                  .NO/SKIP TO NXT WEEK                      
         BAS   RE,EDITFLDS        EDIT BINREC WEEK DATA TO PRINTLINE            
PRT20    LA    R4,4(R4)            BUMP PERLIST                                 
         CLI   0(R4),0             CHK IF EOF PERLIST                           
         BE    PRT25                  .YES/PLINE AT END-PRINT                   
         BAS   RE,NXTBINWK            .NO/GET NXT BINWEEK REC                   
         LA    R3,L'PWKFLD(R3)       BUMP TO NEXT PLINE FIELD                   
         B     PRT15                                                            
PRT25    CLC   P,SPACES            IS P LINE SPACES                             
         BE    PRTX                  .YES/END IT                                
         CLI   SPLSKP,C'D'           .NO/ PRINT IT                              
         BNE   PRT27                                                            
         MVI   SPACING,2                                                        
         B     PRT30                                                            
PRT27    CLI   SPLSKP,C'T'                                                      
         BNE   PRT30                                                            
         MVI   SPACING,3                                                        
PRT30    GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R1,BINDISP          SET BINTABLE DISP TO                         
         LA    R1,BINRECLN(R1)     POINT TO NXT BINREC IN WEEK GROUP            
         ST    R1,BINDISP                                                       
         B     PRT10                                                            
PRTX     B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************                                  
* EDITS BINREC TO PRINT LINE                                                    
* INPUT R2-BINREC WEEK                                                          
*       R3-PRINT LINE                                                           
* OUTPUT BINREC EDITED TO PRINT LINE                                            
*                                                                               
EDITFLDS NTR1                                                                   
         USING BINRECD,R2                                                       
         USING PLINED,R3                                                        
         A     R2,BINDISP         ADD DISP TO POINT TO DESIRED BINREC           
         GOTO1 DATCON,DMCB,(2,BINDATE),(4,WORK)                                 
         MVC   PDATE,WORK+3        MOVE DAY/NOT MONTH                           
         MVC   WORK(1),BINPRD                                                   
         BAS   RE,GETPRD        GET PRINTABLE PRD RETURNED IN WORK+10           
         MVC   PPRD,WORK+10                                                     
         LA    R4,PDATE+132                                                     
         L     R1,BINCOST                                                       
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         EDIT  (R1),(6,0(R4))   SET COST TO PRINT LINNE                         
         LA    R3,WEEKTOTS      ADD COST TO APPROPRIATE WEEKTOT                 
EDT5     CLC   BINDATE,2(R3)                                                    
         BNH   EDT7                                                             
         LA    R3,12(R3)                                                        
         CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     EDT5                                                             
EDT7     A     R1,4(R3)                                                         
         ST    R1,4(R3)                                                         
         L     R1,8(R3)       ADD TO UNITS OF APPROPRIATE WEEKTOT               
         LA    R1,1(R1)                                                         
         ST    R1,8(R3)                                                         
         B     EXIT                                                             
         DROP  R2,R3                                                            
         SPACE 3                                                                
*******************************                                                 
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
****************************************************                            
*    ROUTINE CLEARS BINTABLE                                                    
*    SETS UP BINTABLE PARAMETERS                                                
*    ADDS THE DUMMY BINRECS WITH PERLIST WEEK START DATES                       
*                                                                               
*    CALLED AT INITIALIZATION AND AFTER EACH PROGRAM BREAK                      
*                                                                               
SETBIN   NTR1                                                                   
* CLEAR BINTABLE                                                                
         L     R1,BINDMCB+8        GET NUMB OF RECS IN BINTABLE                 
         LTR   R1,R1                                                            
         BZ    PB7                                                              
         L     R2,ANETWS4                                                       
PB5      XC    0(BINRECLN,R2),0(R2)                                             
         LA    R2,BINRECLN(R2)                                                  
         BCT   R1,PB5                                                           
         SPACE                                                                  
* SET UP BINTABLE PARAMETERS                                                    
PB7      DS    0H                                                               
         SR    R0,R0               A OF REC TO BE ADDED                         
         L     R1,ANETWS4          A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,BINRECLN         LENGTH OF REC                                
         LA    R4,BINKLEN          DISP OF KEY INTO REC                         
         L     R5,=F'700'          MAX RECS = 700                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   PB8                                                              
         L     R5,=F'2000'         MAX OFFLINE=2000                             
PB8      STM   R0,R5,BINDMCB                                                    
         SPACE                                                                  
* SET PERLIST DUMMY RECS WITH WEEK START TO BINTABLE                            
         L     R2,NUMPER                                                        
         LTR   R2,R2                                                            
         BZ    PBX                                                              
         L     R3,ANETWS2                                                       
         XC    0(BINRECLN,R3),0(R3)                                             
         LA    R4,PERLIST                                                       
PB10     MVC   0(2,R3),0(R4)                                                    
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R3)),RR=RELO                             
         LA    R4,4(R4)                                                         
         BCT   R2,PB10                                                          
PBX      B     EXIT                                                             
         SPACE 3                                                                
***************************************                                         
         EJECT                                                                  
*******************************************                                     
* POINTS R2 TO NEXT WEEK IN BINTABLE                                            
*                                                                               
* INPUT R2=CURRENT WEEK IN BINTALBE                                             
*       R4=DESIRED WEEK IN PERLIST                                              
* OUTPUT R2=NEXT WEEK IN BINTABLE                                               
********************************************                                    
NXTBINWK NTR1                                                                   
NXT5     LA    R2,BINRECLN(R2)                                                  
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R2),0(R4)                                                    
         BNE   NXT5                                                             
         XIT1  REGS=(R2)                                                        
         SPACE 3                                                                
***************************************                                         
* DO NETWORK TOTALS                                                             
*                                                                               
NTWKTOTS NTR1                                                                   
         L     R3,AP1                                                           
         USING PLINED,R3                                                        
         MVC   PNET(16),=C'*UNIT/COST TOTS*'                                    
         CLI   NOCOST,C'Y'                                                      
         BNE   *+10                                                             
         MVC   PNET(16),=C'*UNIT TOTS*     '                                    
         EDIT  NTWKUNIT,(4,PUTOT)         PRINT UNITS                           
         LA    R2,WEEKTOTS                                                      
         LA    R4,PUTOT+5                                                       
         LA    R3,132(R4)                                                       
         L     R1,NUMPER                                                        
NET3     CLI   130(R4),X'40'       IS PREV SPACE FILLED                         
         BE    NET3A                                                            
         EDIT  (B4,4(R2)),(7,132(R3))                                           
         EDIT  (B4,8(R2)),(4,132(R4))                                           
         B     NET3B                                                            
NET3A    EDIT  (B4,4(R2)),(7,0(R3))                                             
         EDIT  (B4,8(R2)),(4,0(R4))                                             
NET3B    LA    R3,7(R3)                                                         
         LA    R4,7(R4)                                                         
         LA    R2,12(R2)                                                        
         BCT   R1,NET3                                                          
         BAS   RE,SPOOLIT                                                       
* ROLL AND CLEAR NUMBER OF UNITS                                                
         L     R1,ALLUNITS                                                      
         A     R1,NTWKUNIT                                                      
         ST    R1,ALLUNITS                                                      
         XC    NTWKUNIT,NTWKUNIT                                                
* ROLL WEEKTOTS TO ALLTOTS AND CLEAR WEEKTOTS                                   
         L     R4,NUMPER                                                        
         LA    R1,ALLTOTS                                                       
         LA    R2,WEEKTOTS                                                      
ROLL     L     R3,0(R1)                                                         
         A     R3,4(R2)                                                         
         ST    R3,0(R1)                                                         
         L     R3,4(R1)            ROLL OVER UNITS                              
         A     R3,8(R2)                                                         
         ST    R3,4(R1)                                                         
         XC    4(8,R2),4(R2)       CLEAR THAT WEEK TOT                          
         LA    R1,8(R1)                                                         
         LA    R2,12(R2)                                                        
         BCT   R4,ROLL                                                          
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
****************************************                                        
* GET WEEKLIST INTO PERLIST                                                     
*                                                                               
GTWKLIST NTR1                                                                   
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBRESUME,NBPROCPK                                                
GTWK5    NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALDAT                                                  
         BNE   GTWK5                                                            
         MVI   PERTYPE,C'W'        WEEKS ONLY                                   
         LA    R3,14               FOR 14 WEEKS MAX                             
         ST    R3,NUMPER                                                        
         LA    R3,PERLIST                                                       
         NETGO NVWKLST,DMCB,NUMPER,(R3),PERTYPE                                 
         B     EXIT                                                             
         SPACE 3                                                                
***************************************************                             
* CHKS IF BINREC FALLS WITHIN CURRENT PERLIST WEEK                              
*                                                                               
* INPUT R2-START OF CURRENT BINWEEK                                             
*       R4-CURRENT PERLIST WEEK                                                 
* OUTPUT COND CODE=0 IF OK                                                      
**************************************************                              
INDATES  NTR1                                                                   
         A     R2,BINDISP          POINT R2 TO BINREC                           
         CLI   0(R2),0             TEST IF END OF RECS                          
         BE    INX                    .YES/SET CC=UNEQUAL                       
         CLC   0(2,R2),2(R4)       CHK IF DATE OF REC HIGHER                    
         BH    INX                 THAN END OF PERLIST WEEK                     
         SR    R2,R2                  .NO/SET CC=EQUAL                          
INX      LTR   R2,R2                                                            
         B     EXIT                                                             
         EJECT                                                                  
**********************************                                              
*  TO GET PRD CODE FROM C LIST   *                                              
*  INPUT   WORK HAS PRDNO        *                                              
*  OUTPUT  PRDCODE IN WORK+10    *                                              
**********************************                                              
GETPRD   NTR1                                                                   
         L     R2,ACLISTSV                                                      
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                                                             
         MVC   WORK+10(3),=C'***'   .SET TO UNDEFINED                           
         B     GPX                                                              
GP12     CLC   3(1,R2),WORK                                                     
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   WORK+10(3),0(R2)      SET 3 CHAR PRINTABLE PRD CODE              
*                                                                               
GPX      B     EXIT                                                             
         SPACE 2                                                                
********************************************                                    
* READ PRODUCT HEADER                      *                                    
*      INPUT : WORK CONTAINS 3CL PRD CODE  *                                    
*      OUTPUT: PRDNAME IN WORK             *                                    
*                                          *                                    
********************************************                                    
         SPACE                                                                  
GETPRDNM NTR1                                                                   
         LA    R3,KEY                                                           
         USING PKEY,R3                                                          
         XC    KEY,KEY                                                          
         MVC   PKEYAM,NBACTAM                                                   
         MVC   PKEYCLT,NBACTCLI                                                 
         MVC   PKEYPRD,WORK                                                     
         NETGO NVSETSPT,DMCB                                                    
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R3,NBAIO                                                         
         USING PRDHDR,R3                                                        
         MVC   WORK(20),PNAME                                                   
         SPACE                                                                  
GPNX     NETGO NVSETUNT,DMCB       RESET TO UNIT FILE                           
         XC    FILENAME,FILENAME   CLEAR FILENAME                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***************************************                                         
* CHECK PERLIST FOR START/END DUPLICATE DATE                                    
* THIS CAN HAPPEN WHEN START/END INPUT                                          
* DATES DO NOT COINCIDE WITH START/                                             
* END OF WEEK                                                                   
*                                                                               
*************************************                                           
         SPACE                                                                  
CHKDUPS  NTR1                                                                   
         LA    R3,PERLIST       * DOES PERLIST BEGIN WITH WEEK DATES            
         CLC   0(2,R3),2(R3)       CHK START/END EQUAL                          
         BNE   DUP2                                                             
         MVC   0(L'PERLIST-4,R3),4(R3)    IF EQUAL/MEANS NOT WEEK               
         L     R3,NUMPER                  SO GET RID OF THIS DATE               
         BCTR  R3,0                       BEFORE START/END WEEK DATES           
         ST    R3,NUMPER                                                        
         LA    R3,PERLIST                                                       
         LA    R3,L'PERLIST-4(R3)         CLEAR LAST 4 BYTES                    
         XC    0(4,R3),0(R3)                                                    
         SPACE                                                                  
DUP2     L     R3,NUMPER                                                        
         BCTR  R3,0                                                             
         MH    R3,=H'4'                                                         
         LA    R2,PERLIST                                                       
         AR    R2,R3               R2 POINTS TO LAST WEEK                       
         CLC   0(2,R2),2(R2)       ARE LAST TWO DATES EQUAL                     
         BNE   DUPX                                                             
         XC    0(4,R2),0(R2)       YES ZERO OUT                                 
         L     R3,NUMPER                                                        
         BCTR  R3,0                                                             
         ST    R3,NUMPER           SET NUMPER TO ONE LESS                       
DUPX     B     EXIT                                                             
         EJECT                                                                  
********************************************************************            
* SEARCH PERLIST TO FIND WEEK STRT/END PARAMS OF NBACTDAT                       
* OUTPUT: WEEK START IN WORK                                                    
*         WORK=FF IF NBACTDAT(AIR DATE) LESS THAN START OF PERLIST              
********************************************************************            
         SPACE                                                                  
GETDAT   NTR1                                                                   
         LA    R2,PERLIST                                                       
         CLC   NBACTDAT,0(R2)                                                   
         BE    GDIT             IF NBACTDAT LESS THAN START OF PERLIST          
         BNL   GD7                 SET WORK TO FF AND EXIT                      
GD5      MVI   WORK,X'FF'                                                       
         B     GDX                                                              
         SPACE                                                                  
GD7      CLC   NBACTDAT,2(R2)                                                   
         BNH   GDIT                                                             
         LA    R2,4(R2)            BUMP PERLIST                                 
         CLI   0(R2),0             IF END OF PERLIST THEN NBACTDAT              
         BE    GD5                 GREATER THAN END OF LIST                     
         CLC   NBACTDAT,0(R2)                                                   
         BNE   GD7                                                              
GDIT     MVC   WORK(2),0(R2)       SET STRT TO WORK                             
GDX      B     EXIT                                                             
         EJECT                                                                  
*****************************************                                       
* GETS DISPLACEMENT OF PRINT LINE                                               
* TO CENTER OUTPUT                                                              
*                                                                               
* OUTPUT AP1,AH10,ABOXCOLS                                                      
*****************************************                                       
         SPACE                                                                  
GETDISP  NTR1                                                                   
         L     R1,NUMPER           NUMPER CONTAINS NO OF WEEKS                  
         MH    R1,=H'7'           15=LENGTH OF REPEATED FIELDS                  
         AH    R1,=H'20'           20=NET/PROD/TOT FIELDS                       
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
         LA    R1,H10                                                           
         AR    R1,R2                                                            
         ST    R1,AH10             SET START OF HEADLINE                        
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         LA    R1,BOXCOLS                                                       
         BCTR  R2,0                SUBTRACT 1 FOR BOXES START                   
         AR    R1,R2                                                            
         ST    R1,ABOXCOLS         SET START OF BOXES                           
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
         PRINT GEN                                                              
TRAPERR  GOTO1 ERREX                                                            
         PRINT NOGEN                                                            
         SPACE                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+15(20),SPLCLIN                                                
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),SPLPRO                                                  
         MVC   H4+15(20),SPLPRON                                                
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(3),SPLEST                                                  
         MVC   H5+15(20),SPLESTN                                                
         SPACE                                                                  
         L     R2,AH10                                                          
         USING PLINED,R2                                                        
         L     R1,ABOX             IS ABOX ZERO                                 
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         MVI   ONLINESW,C'Y'       YES/SO ONLINE                                
         MVC   PNET+132(3),=C'NET'                                              
         MVC   PPROG+132(3),=C'PRG'                                             
         MVC   PAVRTG(3),=C' AV'                                                
         MVC   PAVRTG+132(3),=C'RTG'                                            
         MVC   PUTOT+132(5),=C'UNITS'                                           
         CLI   ONLINESW,C'Y'                                                    
         BNE   SKIPIT                                                           
         MVC   PNET+264(3),=C'-------------'                                    
         MVC   PPROG+264(3),=C'-------------'                                   
         MVC   PAVRTG+264(3),=C'-------------'                                  
         MVC   PUTOT+264(5),=C'-------------'                                   
SKIPIT   DS 0H                                                                  
         LA    R2,PWKFLD                                                        
         USING PWKFLD,R2                                                        
         LA    R3,PERLIST          PRINT DATE HEADINGS                          
HDHK1    CLI   0(R3),0                                                          
         BE    HDHK1A                                                           
         GOTO1 DATCON,DMCB,(2,0(R3)),(4,0(R2))                                  
         MVC   132(6,R2),=C'DT/PRD'                                             
         CLI   ONLINESW,C'Y'                                                    
         BNE   *+10                                                             
         MVC   264(6,R2),=C'-------------'                                      
         LA    R2,L'PWKFLD(R2)                                                  
         LA    R3,4(R3)                                                         
         B     HDHK1                                                            
         DROP  R2                                                               
*                                                                               
HDHK1A   DS    0H                                                               
         CLI   BOXSET,C'Y'          SET PARAMS FOR BOXES                        
         BE    HDX                                                              
         MVI   BOXSET,C'Y'                                                      
         CLI   ONLINESW,C'Y'       IF ON-LINE                                   
         BE    HDX                 THEN SKIP BOXES                              
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         L     R4,ABOXCOLS                                                      
         USING PLINED,R4                                                        
         MVI   0(R4),C'L'                                                       
         MVI   PPROG,C'C'                                                       
         CLI   SPLNAM,C'Y'         (PROGRAM NAME OPTION)                        
         BE    *+12                                                             
         MVI   PAVRTG,C'C'                                                      
         MVI   PUTOT,C'C'                                                       
         L     R3,NUMPER                                                        
BOX5     MVI   PDATE,C'C'                                                       
*        MVI   PPRD,C'C'                                                        
*        MVI   PCOST,C'C'                                                       
         LA    R4,L'PWKFLD(R4)                                                  
         BCT   R3,BOX5                                                          
         MVI   PDATE,C'R'                                                       
         SPACE                                                                  
         LA    R4,BOXROWS                                                       
         LA    R4,8(R4)                                                         
         MVI   0(R4),C'T'                                                       
         LA    R4,3(R4)                                                         
         MVI   0(R4),C'M'                                                       
         LA    R4,49(R4)                                                        
         MVI   0(R4),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C' PROGRAM FLOWCHART  '                                    
         SSPEC H2,52,C' -----------------  '                                    
         SSPEC H3,50,PERIOD                                                     
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
OFTABLE  DS    CL9000                                                           
         DS    CL9000                                                           
       EJECT                                                                    
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2+500                     
BINDMCB  DS    6F                                                               
*                                                                               
RELO     DS    F                                                                
BINDISP  DS    F                                                                
AP1      DS    F                                                                
AH10     DS    F                                                                
ABOXCOLS DS    F                                                                
ACLISTSV DS    F                                                                
NUMPER   DS    F                                                                
NETNUM   DS    F                                                                
*                        TOTAL FIELDS                                           
RTGTOT   DS    F                   RTG AV                                       
UNITOT   DS    F                   NUM OF UNITS                                 
NTWKUNIT DS    F                                                                
ALLUNITS DS    F                                                                
WEEKTOTS DS    CL168              14X12 DATE(CL4)+COST(CL4)+UNITS(CL4)          
         DS    F                                                                
ALLTOTS  DS    CL112              14X8  COST(CL4)+UNITS(CL4)                    
*                                                                               
TOTRTG   DS    F                   ACROSS NETWORK TOTALS                        
TOTUNIT  DS    F                                                                
TOTCOST  DS    F                                                                
*                                                                               
PERLIST  DS    CL60                                                             
PERTYPE  DS    CL1                                                              
FRST     DS    CL1                                                              
ONLINESW DS    CL1                                                              
NTWKPGE  DS    CL1                                                              
ACTFLG   DS    CL1                                                              
BOXSET   DS    CL1                                                              
NOCOST   DS    CL1                                                              
PROGSV   DS    CL6                                                              
PROGNAM  DS    CL16                                                             
NETSV    DS    CL4                                                              
PNETSV   DS    CL4                                                              
ASSCOST  DS    CL1                                                              
*                                                                               
         SPACE 2                                                                
*                                                                               
BINRECD  DSECT                                                                  
BINDATE  DS    CL2                 DATE                                         
BINPRD   DS    CL1                 PRD                                          
BINCOST  DS    F                   ACTUAL COST                                  
BINNUM   DS    CL1                 SUB-LINE TO ADD DUPS AS SEPERATES            
BINKLEN  EQU   *-BINRECD                                                        
BINRECLN EQU   *-BINRECD                                                        
         SPACE 2                                                                
*                                                                               
PLINED   DSECT                                                                  
PNET     DS    CL4                                                              
         DS    CL1                                                              
PPROG    DS    CL6                                                              
         DS    CL1                                                              
PAVRTG   DS    CL4                                                              
         DS    CL1                                                              
PUTOT    DS    CL3                                                              
         DS    CL3                                                              
PWKFLD   DS    0CL7            A MAXIMUM OF 14 PWKFLDS                          
PDATE    DS    CL2                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
         SPACE                                                                  
*              NETINCLS                                                         
*              NENETGOALD                                                       
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NENETGOALD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDD8D                                                       
         SPACE 1                                                                
*              DDBIXBOX                                                         
*              NETDEMOD                                                         
*              DEDBLOCK                                                         
*              SPGENPRD                                                         
*              SPGENEST                                                         
*              SPGENCLT                                                         
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039NEMEDA8   05/01/02'                                      
         END                                                                    
