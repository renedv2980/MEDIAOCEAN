*          DATA SET NEMED26T   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEMED26    AT LEVEL 101 AS OF 02/06/86                      
*PHASE T31E26A,+0                                                               
*INCLUDE GETBROAD                                                               
*INCLUDE MOBILE                                                                 
*INCLUDE NETCOM                                                                 
         TITLE 'T31E26 - NETWORK OVERNIGHTS'                                    
         PRINT NOGEN                                                            
T31E26   NMOD1 0,**OVPR**                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T31E26+4096,R9                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R6,ASYSD                                                         
         USING NETSYSD,R6                                                       
         L     R7,ANETWS2                                                       
         USING OVERD,R7                                                         
         EJECT                                                                  
*                                                                               
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         ST    R1,RELO                                                          
         L     R1,=A(BUFFALOC)                                                  
         A     R1,RELO                                                          
         ST    R1,ABUFFC                                                        
*                                                                               
         L     R1,=A(HOOK)                                                      
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         L     R1,=V(NETCOM)                                                    
         A     R1,RELO                                                          
         ST    R1,ANETCOM                                                       
*                                                                               
         MVI   PIGFLAG,0                                                        
         EJECT                                                                  
*              CONTROL OF SUMMARIES                                             
         SPACE 3                                                                
         MVI   NBDATA,C'U'         READ UNIT RECORDS                            
         OI    NBSPLOPT,X'80'      TURN ON SPLIT OPTION                         
         MVI   NBRESUME,NBPROCPK   RESUME READING PACKAGES                      
         SPACE 1                                                                
INITIO   NETGO NSNETIO,DMCB,NETBLOCK      INITIALIZE NETBLOCK                   
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST                                                  
         B     INI2                                                             
*                                  BRANCH ROUND THIS BLOW FOR NOW               
         DC    H'0'                SHOULDN'T HAPPEN                             
INI2     CLI   NBMODE,NBVALDAT                                                  
         BNE   INITIO                                                           
*                                                                               
         MVI   NBESTOPT,C'Y'                                                    
         BAS   RE,SETMENU                                                       
         BAS   RE,SETDATE                                                       
         BAS   RE,SETGROUP                                                      
         L     R1,ABUFFC           ADJUST COLUMNS TO OPTIMIZE                   
         MVI   43(R1),9                                                         
         CLI   SPLFLAV,C'P'                                                     
         BNE   *+8                                                              
         MVI   43(R1),33                                                        
         CLI   SPLFLAV,C'V'                                                     
         BNE   IN2                                                              
         MVI   43(R1),16                                                        
         MVI   NBSELUOP,C'E'                                                    
         CLI   SPLFLAV+1,C'2'      FOR V2 USE ACT SKED                          
         BNE   IN2                                                              
         MVI   NBSELUOP,C'A'                                                    
         MVI   NBESTOPT,C'A'       GET EST DEMOS FOR MGS                        
*                                  IF MISSED UNIT WAS NOT PFB                   
         B     IN2                                                              
*                                                                               
IN2      GOTO1 BUFFALO,DMCB,=C'SET',ABUFFC                                      
         CLC   SPLFLAV(2),=C'V3'                                                
         BNE   IN3                                                              
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBSELUOP,C'A'                                                    
IN3      CLI   SPLFLAV,C'P'                                                     
         BNE   IN4                                                              
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBESTOPT,C'A'       GET EST DEMOS FOR MGS                        
*                                  IF MISSED UNIT WAS NOT PFB                   
         CLI   SPLFLAV+1,C'M'                                                   
         BNE   *+8                 IF FLAVOR=PM                                 
         MVI   NBESTOPT,C'Y'       DONT GET DEMOS FOR MISSED,PFBS               
         CLI   SPLFLAV+1,C'A'                                                   
         BNE   *+8                 IF FLAVOR=PA                                 
         MVI   NBESTOPT,C'Y'       DONT GET DEMOS FOR MISSED,PFBS               
         SPACE 1                                                                
IN4      CLI   SPLFLAV,C'E'                                                     
         BNE   LOOP                                                             
         MVI   NBUSER+13,C'N'      OVERRIDE PROF. DONT FILT PRE-EMPTS           
         MVI   NBSELUOP,C'A'       USE ACTUAL SCHEDULE UNLESS ALLU=Y            
         CLI   SPLALLU,C'Y'                                                     
         BNE   LOOP                                                             
         MVI   NBSELUOP,0          RESET TO 0                                   
**************************************************                              
*********** TEMPORARILY? DISABLED ****************                              
*                                                                               
*******  MVC   NBREVOPT,NBUSER+11  REVALUE OPTION                               
*************                                                                   
         EJECT                                                                  
*              MAIN NETIO LOOP                                                  
         SPACE 3                                                                
LOOP     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BE    GOTUN                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    LASTONE                                                          
         B     LOOP                                                             
         SPACE 1                                                                
GOTUN    MVC   CURPRD,NBSPLPRN     CURRENT PRODUCT NUMBER                       
         CLC   NBSELPRD,=C'POL'    FOR POOL                                     
         BNE   *+10                                                             
         MVC   CURPRD,NBPRD        USE FIRST ALLOCATED BRAND                    
         L     RF,=A(PROCCOST)     POSSIBLE ACCOUNTING ADJUSTMENTS              
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         OC    NBPROGNM,=16XL1'40' REPLACE BINARY ZEROS WITH SPACES             
*                                  FOR SORTING BY PROGNAME.                     
*                                  EVENTUALLY SHOULD BE IN NETVALUE.            
*                                                                               
         L     R1,NBASSIGN         SAVE ASSIGNED DOLLARS                        
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         ST    R1,ASSDOLS                                                       
         L     R1,NBACTUAL         SAVE ACTUAL DOLLARS                          
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         ST    R1,ACTDOLS                                                       
         CLI   SPLFLAV,C'E'     **** FUDGE TO FORCE ZERO RECORDS TO             
         BNE   LOOP1                PRINT. FORCE UNITS TO 1                     
         MVC   SAVESTUN,NBESTUN                                                 
         MVC   SAVACTUN,NBACTUN                                                 
         LA    R1,1                                                             
         STH   R1,NBESTUN                                                       
         STH   R1,NBACTUN                                                       
         SPACE 1                                                                
LOOP1    CLI   SPLFLAV,C'P'        IGNORE SOME UNITS ON POSTS                   
         BNE   LOOP6                                                            
         MVC   SAVESTUN,NBESTUN                                                 
         MVC   SAVACTUN,NBACTUN                                                 
         CLI   SPLFLAV+1,C'A'      AUDIT NEEDS ALL                              
         BE    LOOP6                                                            
         CLI   SPLFLAV+1,C'P'                                                   
         BE    FLAVP                                                            
         CLI   SPLFLAV+1,C'M'                                                   
         BE    FLAVM                                                            
         CLI   SPLFLAV+1,C'F'      F NEEDS PFB                                  
         BE    FLAVF                                                            
         CLI   SPLFLAV+1,C'X'                                                   
         BE    FLAVX                                                            
         TM    NBUNITST,X'02'      DEFAULT IGNORES MISSED                       
         BO    LOOP                                                             
         B     LOOP6                                                            
*                                                                               
FLAVF    TM    NBUNITST,X'04'      F NEEDS PFBS                                 
         BNO   LOOP                                                             
         TM    NBUNITST,X'03'        IGNORES MISSED AND MAKEGOODS               
         BNZ   LOOP                                                             
         B     LOOP6                                                            
*                                                                               
FLAVM    TM    NBUNITST,X'03'      M NEEDS ONLY MISSED AND MAKEGOODS            
         BZ    LOOP                                                             
         B     LOOP6                                                            
*                                                                               
FLAVP    TM    NBUNITST,X'40'      P NEEDS ONLY PRE-EMPTS                       
         BNO   LOOP                                                             
         TM    NBUNITST,X'02'                                                   
         BO    LOOP                                                             
         B     LOOP6                                                            
*                                                                               
FLAVX    TM    NBUNITST,X'02'      IGNORE MISSED UNITS                          
         BO    LOOP                                                             
         TM    NBUNITST,X'04'      IGNORE PFBS THAT ARENT MAKEGOODS             
         BNO   LOOP6                                                            
         TM    NBUNITST,X'01'                                                   
         BNO   LOOP                                                             
*                                                                               
LOOP6    BAS   RE,FILTGRUP                                                      
         BNE   LOOP                                                             
         BAS   RE,BUFFPOST                                                      
         B     LOOP                                                             
*                                                                               
*                                                                               
LASTONE  BAS   RE,OVREPS                                                        
         B     XIT                                                              
*                                                                               
PROCERR  DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
*              ROUTINE TO SET UP MENUS AND GENERATE COLUMNS                     
         SPACE 3                                                                
SETMENU  NTR1                                                                   
         LA    R3,PROGPROF         ADDRESS APPROPRIATE PROFILE                  
         OC    PROGPROF,PROGPROF                                                
         BNZ   *+10                                                             
         MVC   PROGPROF,DEFPROF                                                 
         CLI   SPLFLAV,C'P'                                                     
         BL    SMB                                                              
         LA    R3,5(R3)                                                         
         BE    SMB                                                              
         LA    R3,5(R3)                                                         
         SPACE 1                                                                
SMB      OC    COLS,COLS           USER SELECTED COLUMNS                        
         BNZ   SM14                                                             
         CLI   MENU,0              USER SELECTED MENU                           
         BNE   SM8                                                              
         CLI   SPLFORM,C'-'                                                     
         BE    SM14                                                             
         SPACE 1                                                                
SM2      MVC   MENU,0(R3)                                                       
         SPACE 1                                                                
SM8      LA    R2,MENUTAB                                                       
         SPACE 1                                                                
SM10     CLC   0(1,R2),MENU                                                     
         BE    SM12                                                             
         LA    R2,10(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   SM10                                                             
         B     SM2                                                              
         SPACE 1                                                                
SM12     CLC   1(1,R2),SPLFLAV                                                  
         BNE   SM2                                                              
         XC    COLS,COLS           MAX 8 COLS IN MENU (LEFT FROM OLD)           
         MVC   COLS(8),2(R2)                                                    
         SPACE 1                                                                
SM14     CLI   DETMENU,0           USER SELECTED DETAIL MENU                    
         BNE   SM18                                                             
         SPACE 1                                                                
SM16     MVC   DETMENU,1(R3)       DEFAULT                                      
         CLI   SPLDETH+5,0                                                      
         BE    SM18                                                             
         LA    R1,SPLDET           USER LIST                                    
         LA    R2,DETAILS                                                       
         ZIC   R0,SPLDETH+5                                                     
         XC    DETAILS,DETAILS                                                  
         SPACE 1                                                                
SM17     CLI   0(R1),C','          MOVE IN NON COMMAS                           
         BE    *+14                                                             
         MVC   0(1,R2),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,SM17                                                          
         B     SM24                                                             
         SPACE 1                                                                
SM18     LA    R2,DETMTAB          LOOK UP MENU TABLE                           
         SPACE 1                                                                
SM20     CLC   0(1,R2),DETMENU                                                  
         BE    SM22                                                             
         LA    R2,L'DETMTAB(R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BNE   SM20                                                             
         B     SM16                                                             
         SPACE 1                                                                
SM22     MVC   DETAILS,2(R2)                                                    
         SPACE 1                                                                
SM24     MVC   SUBTOT,2(R3)        SUBTOTAL OPTION FROM PROFILE                 
         CLI   SUBTOT,C'*'                                                      
         BNE   *+8                                                              
         MVI   SUBTOT,C'-'                                                      
         CLI   SPLSUBH+5,0                                                      
         BE    SM26                                                             
         MVC   SUBTOT,SPLSUB                                                    
         SPACE 1                                                                
SM26     CLI   RECMENU,0           USER SELECTED RECAP MENU                     
         BNE   SM30                                                             
         SPACE 1                                                                
SM28     MVC   RECMENU,3(R3)       DEFAULT                                      
         CLI   SPLRECH+5,0                                                      
         BE    SM30                                                             
         LA    R1,SPLREC           USER LIST                                    
         LA    R2,RECAP                                                         
         ZIC   R0,SPLRECH+5                                                     
         XC    RECAP,RECAP                                                      
         SPACE 1                                                                
SM29     CLI   0(R1),C','                                                       
         BE    *+14                                                             
         MVC   0(1,R2),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,SM29                                                          
         B     SM36                                                             
         SPACE 1                                                                
SM30     LA    R2,RECMTAB          LOOK UP MENU TABLE                           
         SPACE 1                                                                
SM32     CLC   0(1,R2),RECMENU                                                  
         BE    SM34                                                             
         LA    R2,6(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   SM32                                                             
         B     SM28                                                             
         SPACE 1                                                                
SM34     MVC   RECAP,2(R2)                                                      
         SPACE 1                                                                
SM36     MVC   SUBREC,4(R3)        RECAP SUB. FROM PROFILE                      
         CLI   SUBREC,C'*'                                                      
         BNE   *+8                                                              
         MVI   SUBREC,C'-'                                                      
         CLI   SPLRSUBH+5,0                                                     
         BE    SM38                                                             
         MVC   SUBREC,SPLRSUB                                                   
         SPACE 1                                                                
         SPACE 1                                                                
SM38     BAS   RE,TRIMCOLS                                                      
         BAS   RE,SETLNO                                                        
         BAS   RE,CHECKSUB                                                      
         L     RF,=A(COMPDISP)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     XIT                                                              
         EJECT                                                                  
*              MENU TABLE                                                       
         SPACE 3                                                                
MENUTAB  DS    0CL10                                                            
         DC    AL1(01),C'E',AL1(25,3,7,10,0,0,0,0)                              
         DC    AL1(02),C'E',AL1(25,3,7,10,11,0,0,0)                             
         DC    AL1(03),C'E',AL1(25,1,3,6,7,0,0,0)                               
         DC    AL1(04),C'E',AL1(25,2,4,6,7,0,0,0)                               
         DC    AL1(05),C'E',AL1(25,0,5,0,0,0,0,0)                               
         DC    AL1(06),C'E',AL1(25,0,5,0,13,0,0,0)                              
         DC    AL1(07),C'E',AL1(25,0,5,0,7,0,9,0)                               
         DC    AL1(08),C'E',AL1(25,5,7,9,14,0,0,0)                              
         DC    AL1(09),C'E',AL1(25,0,5,0,12,0,13,0)                             
         DC    AL1(10),C'E',AL1(25,5,7,9,14,11,0,0)                             
         DC    AL1(11),C'E',AL1(25,0,7,0,0,0,0,0)                               
         SPACE 1                                                                
         DC    AL1(31),C'V',AL1(31,32,33,25,51,44,47,50)                        
         DC    AL1(21),C'P',AL1(34,25,41,40,42,45,0,0)                          
         DC    X'FF'                                                            
         SPACE 2                                                                
DETMTAB  DS    0CL10                                                            
         DC    AL1(01),C'*',C'DPYTBL',AL2(0)                                    
         DC    AL1(02),C'*',C'DPYTBN',AL2(0)                                    
         DC    AL1(03),C'*',C'DPYTNL',AL2(0)                                    
         DC    AL1(04),C'*',C'DPYTEL',AL2(0)                                    
         DC    AL1(05),C'*',C'PDYTBL',AL2(0)                                    
         DC    AL1(06),C'*',C'NPDYTB',AL2(0)                                    
         DC    AL1(07),C'*',C'NPDYTL',AL2(0)                                    
         DC    AL1(08),C'*',C'PDYTEL',AL2(0)                                    
         DC    X'FF'                                                            
         SPACE 1                                                                
RECMTAB  DS    0CL6                                                             
         DC    AL1(01),C'*',C'NM  '                                             
         DC    AL1(02),C'*',C'N4  '                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
DEFPROF  DC    X'0101D401D8'       DEFAULTS FOR MENUS                           
         DC    X'1501E601D4'                                                    
         DC    X'1F01E601D4'                                                    
         EJECT                                                                  
*              ROUTINE TO TRIM UNNEEDED COLUMNS                                 
         SPACE 3                                                                
TRIMCOLS NTR1                                                                   
         OC    NDDEMOS+3(3),NDDEMOS+3                                           
         BZ    TRIM1                                                            
         OC    NDDEMOS+6(3),NDDEMOS+6                                           
         BZ    TRIM2                                                            
         B     XIT                                                              
         SPACE 1                                                                
TRIM1    MVI   WORK,45             ONLY 1 DEMO                                  
         BAS   RE,TRIM4                                                         
         MVI   WORK,46                                                          
         BAS   RE,TRIM4                                                         
         MVI   WORK,47                                                          
         BAS   RE,TRIM4                                                         
         SPACE 1                                                                
TRIM2    MVI   WORK,48             2 DEMOS                                      
         BAS   RE,TRIM4                                                         
         MVI   WORK,49                                                          
         BAS   RE,TRIM4                                                         
         MVI   WORK,50                                                          
         BAS   RE,TRIM4                                                         
         B     XIT                                                              
         SPACE 1                                                                
TRIM4    LA    R2,COLS             CHECK TO SEE IF ONE OF                       
         LA    R3,L'COLS           SELECTED COLUMNS IS NOT NEEDED               
         SPACE 1                                                                
TRIM6    CLC   0(1,R2),WORK                                                     
         BNE   *+8                                                              
         MVI   0(R2),0                                                          
         LA    R2,1(R2)                                                         
         BCT   R3,TRIM6                                                         
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINES SET NUMBERS OF LISTS                                    
         SPACE 3                                                                
SETLNO   NTR1                                                                   
         LA    R2,COLS             COLUMNS                                      
         LA    R3,L'COLS                                                        
         AR    R2,R3               GO BACK THRU LIST                            
         BCTR  R2,0                                                             
SNLOOP   CLI   0(R2),0                                                          
         BNE   SN1                                                              
         BCTR  R2,0                                                             
         BCT   R3,SNLOOP                                                        
SN1      STC   R3,NCOLS                                                         
         SPACE 1                                                                
         LA    R2,DETAILS          DETAILS                                      
         MVI   MAX,8                                                            
         BAS   RE,SN2                                                           
         MVC   NDETAILS,ACTUAL                                                  
         SPACE 1                                                                
         LA    R2,RECAP            RECAP                                        
         MVI   MAX,4                                                            
         BAS   RE,SN2                                                           
         MVC   NRECAP,ACTUAL                                                    
         B     XIT                                                              
         SPACE 1                                                                
SN2      LR    R1,R2                                                            
         ZIC   R0,MAX                                                           
         SPACE 1                                                                
SN4      CLI   0(R1),C' '          SUBSTITUTE 0 FOR SPACE                       
         BNE   *+8                                                              
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,SN4                                                           
         SPACE 1                                                                
         ZIC   R3,MAX              NOW DETERMINE L'LIST                         
         AR    R2,R3                                                            
         BCTR  R2,0                                                             
         SPACE 1                                                                
SN6      STC   R3,ACTUAL                                                        
         CLI   0(R2),0                                                          
         BNER  RE                                                               
         BCTR  R2,0                                                             
         BCT   R3,SN6                                                           
         BR    RE                                                               
         EJECT                                                                  
*              CHECK IF SUBTOTALS ARE CONSISTENT WITH DETAILS                   
         SPACE 3                                                                
CHECKSUB NTR1                                                                   
         LA    R2,DETAILS                                                       
         MVI   WORK,C'P'           MUST HAVE PROGRAM                            
         CLI   SUBTOT,C'P'                                                      
         BE    *+8                                                              
         MVI   WORK,C'D'                OR DATE SPECIFIED                       
         LA    R3,6                                                             
         SPACE 1                                                                
CS2      CLC   0(1,R2),WORK                                                     
         BE    CS4                                                              
         LA    R2,1(R2)                                                         
         BCT   R3,CS2                                                           
         MVI   SUBTOT,C'-'         OTHERWISE CUT OUT RECAP                      
         SPACE 1                                                                
CS4      CLI   SUBTOT,C'P'         IF PROGRAM IS SELECTED                       
         BNE   XIT                                                              
         CLI   WORK,C'D'           THEN DATE MUST ALSO BE SPECIFIED             
         BE    XIT                                                              
         MVI   WORK,C'D'                                                        
         B     CS2                                                              
         EJECT                                                                  
*              ROUTINES TO SET UP DATE STACKS                                   
         SPACE 3                                                                
SETDATE  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(4),=C'S000'     GET SPOT PROFILE FOR SPEC PERS               
         MVC   KEY+4(2),NBEFFAGY                                                
         MVI   KEY+6,C'N'                                                       
         GOTO1 NBCLUNPK,DMCB,NBACTCLI,KEY+7                                     
         GOTO1 NBGTPROF,DMCB,KEY,SPOTPROF,NBDM   INTO SPOTPROF                  
*                                                                               
         LA    R2,NBUSER+3         USE ACCTG MONTH TYPE FOR EST FLAV            
         CLI   SPLFLAV,C'E'                                                     
         BNE   *+8                                                              
         LA    R2,NBUSER+2                                                      
*                                                                               
         LA    R3,2                SELECTED MONTH TYPE                          
         CLI   0(R2),C'C'                                                       
         BE    SD2                 CALENDAR                                     
*                                                                               
*******  LA    R3,0                BRODCAST                                     
*******  CLI   0(R2),C'B'          COMMENTED OUT UNTIL SPEC PERS                
*******  BE    SD2                  ALLOWED SYSTEM WIDE                         
*                                                                               
         ZIC   R3,SPOTPROF+2       OR ONE OF THE SPECIAL FLAVORS                
         SPACE 1                                                                
SD2      L     R4,=V(GETBROAD)                                                  
         A     R4,RELO                                                          
         ST    R4,WORK                                                          
         MVC   WORK+4(4),ADDAY                                                  
         MVC   WORK+8(4),GETDAY                                                 
         MVC   WORK+12(4),DATCON                                                
         MVC   MOBLIST,WORK                                                     
         L     RF,=V(MOBILE)                                                    
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(24,USERQSTR),((R3),MLIST),WORK,SPOTPROF               
         GOTO1 (RF),DMCB,(105,USERQSTR),(4,WLIST)                               
         LA    R2,MLIST                                                         
         LA    R3,QLIST                                                         
         LA    R4,8                                                             
         SPACE 1                                                                
SD4      MVC   0(4,R3),0(R2)       BUILD QUARTER LIST FROM MONTHS               
         CLI   4(R2),X'FF'                                                      
         BE    SD6                                                              
         MVC   2(2,R3),6(R2)                                                    
         CLI   8(R2),X'FF'                                                      
         BE    SD6                                                              
         MVC   2(2,R3),10(R2)                                                   
         CLI   12(R2),X'FF'                                                     
         BE    SD6                                                              
         LA    R2,12(R2)                                                        
         LA    R3,4(R3)                                                         
         BCT   R4,SD4                                                           
         SH    R3,=H'4'                                                         
         SPACE 1                                                                
SD6      MVI   4(R3),X'FF'                                                      
         BAS   RE,EDLIST                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXPAND DATE LISTS TO EDITED FORMAT                    
         SPACE 3                                                                
EDLIST   NTR1                                                                   
         LA    R2,WLIST                                                         
         LA    R3,EWLIST                                                        
         MVI   DUB,C'W'                                                         
         BAS   RE,EL2                                                           
         LA    R2,MLIST                                                         
         LA    R3,EMLIST                                                        
         MVI   DUB,C'M'                                                         
         BAS   RE,EL2                                                           
         LA    R2,QLIST                                                         
         LA    R3,EQLIST                                                        
         MVI   DUB,C'Q'                                                         
         BAS   RE,EL2                                                           
         B     XIT                                                              
         SPACE 1                                                                
EL2      NTR1                                                                   
         SPACE 1                                                                
EL4      CLI   DUB,C'M'            WEEK AND QUARTER                             
         BE    EL6                                                              
         GOTO1 DATCON,DMCB,(2,0(R2)),(4,0(R3))                                  
         LA    R3,5(R3)                                                         
         CLI   DUB,C'W'            QUARTER ONLY                                 
         BE    EL8                                                              
         MVI   0(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,2(R2)),(4,1(R3))                                  
         LA    R3,6(R3)                                                         
         B     EL8                                                              
         SPACE 1                                                                
EL6      GOTO1 DATCON,DMCB,(2,0(R2)),(0,WORK)     MONTH                         
         CLI   SPLFLAV,C'E'        IF FLAV=EST USE NBUSER+2                     
         BNE   EL65                                                             
         CLI   NBUSER+2,C'C'       IF CALENDAR MONTH WE HAVE MONTH              
         BE    EL7                 ELSE MAKE SURE BY BUMPING END                
         B     EL66                                                             
*                                  ELSE USE NBUSER+3                            
EL65     CLI   NBUSER+3,C'C'       IF CALENDAR MONTH WE HAVE MONTH              
         BE    EL7                 ELSE MAKE SURE BY BUMPING END                
*                                                                               
EL66     GOTO1 DATCON,DMCB,(2,2(R2)),(0,WORK)                                   
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         ZIC   R4,DMCB             TO THE FOLLOWING SUNDAY                      
         LA    R5,7                                                             
         SR    R5,R4                                                            
         BZ    EL7                                                              
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R5)                                      
         MVC   WORK(6),WORK+6                                                   
         SPACE 1                                                                
EL7      PACK  WORK+8(8),WORK+2(2)                                              
         CVB   R1,WORK+8                                                        
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS-3(R1)                                                  
         MVC   0(3,R3),0(R1)                                                    
         MVC   3(2,R3),WORK                                                     
         LA    R3,5(R3)                                                         
         SPACE 1                                                                
EL8      LA    R2,4(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   EL4                                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP PRODUCT GROUP                                  
         SPACE 3                                                                
SETGROUP NTR1                                                                   
         CLI   SPLPRO+1,C'='                                                    
         BNE   SGXIT                                                            
         NETGO NVSETSPT,DMCB                                                    
         USING PRDHDR,R4                                                        
         LA    R4,KEY                                                           
         L     R2,=A(GROUPLST)                                                  
         A     R2,RELO                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),NBACTAM                                                 
         SPACE 1                                                                
SG2      LA    R4,KEY                                                           
         AI    PKEYPRD+2,1         SKIP TO NEXT PRODUCT                         
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   SG4                                                              
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         MVC   0(1,R2),PCODE+1     PRODUCT CODE                                 
         MVC   1(3,R2),PGRP1       FIND MATCH OF PRODUCT GROUP SCHEME           
         CLC   SPLPRO(1),1(R2)     MAY BE FIRST                                 
         BE    SG3                                                              
         MVC   1(3,R2),PGRP2       OR SECOND                                    
         CLC   SPLPRO(1),1(R2)                                                  
         BE    SG3                                                              
         MVC   1(3,R2),PGRP3       OR THIRD                                     
         CLC   SPLPRO(1),1(R2)                                                  
         BE    SG3                                                              
         XC    1(3,R2),1(R2)                                                    
         SPACE 1                                                                
SG3      LA    R2,4(R2)                                                         
         B     SG2                                                              
         SPACE 1                                                                
SG4      MVI   0(R2),X'FF'                                                      
         LA    R4,KEY              GET SCHEME RECORD                            
         XC    KEY,KEY                                                          
         USING PRGKEY,R4                                                        
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD(3),NBACTAM                                              
         MVC   PRGKID,SPLPRO                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 READ                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         LA    R2,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING PRGEL01,R2                                                       
         MVC   BREAK,PRGBK1        DIG OUT BREAK NAME AND LENGTH                
         MVC   BRLEN,PRGBK1LN                                                   
         CLI   PRGBK2LN,0                                                       
         BE    SGXIT                                                            
         MVC   BREAK,PRGBK2                                                     
         AC    BRLEN,PRGBK2LN                                                   
         B     SGXIT                                                            
SGXIT    XC    FILENAME,FILENAME   RESET TO UNT FILE                            
         NETGO NVSETUNT,DMCB                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ESTABLISH PRODUCT GROUP FOR UNIT                      
*              AND TO FILTER ON GROUP WHERE APPLICABLE                          
         SPACE 3                                                                
FILTGRUP NTR1                                                                   
         CLI   SPLPRO+1,C'='                                                    
         BNE   YES                                                              
         CLI   CURPRD,X'FF'         UNALLOCATED MISSES                          
         BE    NO                                                               
         CLI   CURPRD,0                                                         
         BE    NO                                                               
         L     R2,=A(GROUPLST)                                                  
         A     R2,RELO                                                          
         SPACE 1                                                                
FG2      CLC   CURPRD,0(R2)                                                     
         BE    FG6                                                              
         SPACE 1                                                                
FG4      LA    R2,4(R2)                                                         
         B     FG2                                                              
         SPACE 1                                                                
FG6      MVC   BTABGRP+2(3),1(R2)  GROUP ESTABLISHED                            
         CLC   SPLPRO+2(3),=C'ALL'                                              
         BE    YES                                                              
         UNPK  WORK(5),2(3,R2)                                                  
         LA    R2,WORK                                                          
         LA    R1,SPLPRO+2                                                      
         LA    R0,3                                                             
         SPACE 1                                                                
FG8      CLI   0(R1),C'A'          FILTER SPECIFIC GROUP                        
         BL    FG10                                                             
         CLC   0(1,R1),0(R2)                                                    
         BNE   NO                                                               
         SPACE 1                                                                
FG10     LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,FG8                                                           
         MVC   SINGROUP,BTABGRP+2                                               
         B     YES                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         B     *+6                                                              
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO CONTROL BUFFALO KEYS                                 
         SPACE 3                                                                
BUFFPOST NTR1                                                                   
         BAS   RE,BUFFVALS         FILL IN THE TABLE                            
         BAS   RE,BUFFPAGE         FILL IN PAGE DETAILS IN KEY                  
         BAS   RE,BUFFCOLS         FILL NUMERIC COLUMNS                         
         SPACE 1                                                                
         XC    BUFFKEY,BUFFKEY     DETAIL RECORD                                
         MVI   BUFFTYPE,1                                                       
         MVI   BUFFSUB,0                                                        
         LA    R2,DETAILS                                                       
         CLI   DETAILS,C'-'                                                     
         BE    BP1                                                              
         MVC   BUFFCHES(1),NBACTEST                                             
         MVC   BUFFCHPK(1),NBPACK                                               
         MVC   BUFFCHST,NBUNITST   PASS UNIT STATUS FOR SUBSEQUENT TEST         
         MVC   BUFFCHRS(1),NBRESULT                                             
         CLI   NBRESULT,0                                                       
         BNE   BPP2                                                             
         MVI   BUFFCHRS,C'E'       TEST FOR BUFFALO                             
         SPACE 1                                                                
BPP2     MVC   BUFFPNUM,CURPRD     SET PNUM TO PROD EXCEPT FOR POL              
         CLC   NBSELPRD,=C'POL'                                                 
         BE    BPP4                                                             
         MVI   BUFFPNUM,0                                                       
         SPACE 1                                                                
BPP4     ZIC   R3,NDETAILS                                                      
         BAS   RE,BFILL                                                         
         MVC   BUFFLEFT(72),SPACES                                              
         CLI   COMMOPT,C'Y'        OPTION TO SUPPORT COMMENTS                   
         BNE   BPD                                                              
         LA    R2,IO                                                            
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   BPD                                                              
         USING NUCOMEL,R2                                                       
         SPACE 1                                                                
BPB      ZIC   R1,NUCOMLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUFFLEFT(0),NUCOMMNT                                             
         MVI   BUFFSUB,1                                                        
         LR    R0,R2                                                            
         LA    R2,DETAILS                                                       
         BAS   RE,BFILL                                                         
         LR    R2,R0                                                            
         MVC   BUFFLEFT(72),SPACES                                              
         AI    BUFFKEY+L'BUFFKEY-1,1                                            
         BAS   RE,NEXTEL                                                        
         BE    BPB                                                              
         SPACE 1                                                                
BPD      LA    R2,DETAILS                                                       
         CLI   SUBTOT,C'-'                                                      
         BE    BP0                                                              
         SPACE 1                                                                
         MVI   BUFFKEY,X'FF'       SUB TOTALS                                   
         MVC   BUFFKEY+1(L'BUFFKEY-1),BUFFKEY                                   
         MVI   BUFFSUB,2                                                        
         BAS   RE,BFILL                                                         
         SPACE 1                                                                
BP0      MVI   BUFFKEY,X'FF'                                                    
         MVC   BUFFKEY+1(L'BUFFKEY-1),BUFFKEY                                   
         MVI   BUFFSUB,3                                                        
         BAS   RE,BUFFPUT                                                       
         BAS   RE,BALLPUT                                                       
         SPACE 1                                                                
BP1      CLI   RECAP,C'-'                                                       
         BE    XIT                                                              
         MVI   BUFFSUB,4                                                        
         LA    R2,RECAP            SET UP FOR MULTIPLE LEVEL RECAPS             
         ZIC   R3,NRECAP                                                        
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         SPACE 1                                                                
BP2      MVI   BUFFTYPE,2                                                       
         BAS   RE,BUFFPAGE                                                      
         MVI   BUFFKEY,X'FF'                                                    
         MVC   BUFFKEY+1(L'BUFFKEY-1),BUFFKEY                                   
         BAS   RE,BFILL                                                         
         BAS   RE,BALLPUT                                                       
         MVI   BUFFSUB,6                                                        
         BCT   R3,BP2                                                           
         BAS   RE,BUFFPAGE                                                      
         MVC   BUFFKEY(4),=4X'FF'  AND THEN A FINAL TOTAL                       
         MVI   BUFFSUB,7                                                        
         BAS   RE,BUFFPUT                                                       
         BAS   RE,BALLPUT                                                       
         CLI   SUBREC,C'-'                                                      
         BNH   XIT                                                              
         BAS   RE,BUFFPAGE                                                      
         MVI   BUFFSUB,5           RECAP SUBTOT                                 
         LA    R2,RECAP                                                         
         ZIC   R3,NRECAP                                                        
         BAS   RE,BFILL                                                         
         BAS   RE,BALLPUT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              FILL TABLE WITH UNIT VALUES                                      
         SPACE 3                                                                
BUFFVALS NTR1                                                                   
         L     R2,ANETWS1          CLIENT RECORD IS AT W/S AREA 1               
         USING CLTHDR,R2                                                        
         LA    R2,CLIST                                                         
         DROP  R2                                                               
         MVI   BTABBRND+5,0                                                     
         CLI   CURPRD,0                                                         
         BE    BV1                                                              
         CLI   CURPRD,X'FF'                                                     
         BNE   BV2                                                              
         SPACE 1                                                                
BV1      MVC   BTABBRND+2(3),=C'UNA'                                            
         B     BV4                                                              
         SPACE 1                                                                
BV2      CLC   CURPRD,3(R2)         LOOK UP BRAND                               
         BE    *+12                                                             
         LA    R2,4(R2)                                                         
         B     BV2                                                              
         MVC   BTABBRND+2(3),0(R2)                                              
         CLC   SPLPRO(3),=C'POL'   IF WE'RE DOING POL                           
         BNE   BV4                                                              
         MVC   BTABBRND+5(1),NBPRD2     POP IN SECOND BRAND CODE                
         SPACE 1                                                                
BV4      MVC   BTABNET+2(4),NBACTNET                                            
         LA    R2,BTABEST+2                                                     
         MVC   BTABCLI+2(3),NBCLICOD                                            
         EDIT  (1,NBACTEST),(3,(R2)),FILL=0                                     
         MVC   BTABTIME+2(4),NBTIME                                             
         LA    R2,BTABLEN+2                                                     
         EDIT  (1,NBLEN),(3,(R2))                                               
         MVC   BTABDAY+2(1),NBDAY                                               
         MVC   BTABDAY+3(3),NBDAYNAM                                            
         MVC   BTABPROG+2(16),NBPROGNM                                          
         BAS   RE,ANALDATE                                                      
         MVC   BTABDATE+4(2),NBACTDAT                                           
         LA    R2,SUBTOT                                                        
         LA    R3,BTABDATE+2                                                    
         BAS   RE,BV6                                                           
         SPACE 1                                                                
         LA    R2,SUBREC           WEEK                                         
         LA    R3,BTABWEEK+2                                                    
         MVI   2(R3),C'W'                                                       
         MVC   3(1,R3),WORK                                                     
         BAS   RE,BV6                                                           
         SPACE 1                                                                
         LA    R3,BTABMNTH+2       MONTH                                        
         MVI   2(R3),C'M'                                                       
         MVC   3(1,R3),WORK+1                                                   
         BAS   RE,BV6                                                           
         SPACE 1                                                                
         LA    R3,BTABFOUR+2       ODD MONTH                                    
         MVI   2(R3),C'4'                                                       
         MVC   3(1,R3),WORK+1                                                   
         BAS   RE,BV6                                                           
         SPACE 1                                                                
         MVI   BTABQURT+4,C'Q'                                                  
         MVC   BTABQURT+5(1),WORK+2                                             
         SPACE 1                                                                
         MVC   BTABDPT+2(1),NBACTDP   DAYPART CODE                              
         B     XIT                                                              
         SPACE 1                                                                
BV6      XC    0(2,R3),0(R3)                                                    
         CLI   0(R2),C'-'                                                       
         BER   RE                                                               
         CLI   0(R2),C'P'                                                       
         BER   RE                                                               
         MVI   0(R3),C'W'                                                       
         MVC   1(1,R3),WORK                                                     
         CLI   0(R2),C'W'                                                       
         BER   RE                                                               
         MVI   0(R3),C'M'                                                       
         MVC   1(1,R3),WORK+1                                                   
         CLI   0(R2),C'M'                                                       
         BER   RE                                                               
         MVI   0(R3),C'4'                                                       
         CLI   0(R2),C'4'                                                       
         BER   RE                                                               
         MVI   0(R3),C'Q'                                                       
         MVC   1(1,R3),WORK+2                                                   
         BR    RE                                                               
         SPACE 2                                                                
BTABBRND DC    C'B',AL1(4),XL4'00'                                              
BTABEST  DC    C'E',AL1(4),XL4'00'                                              
BTABNET  DC    C'N',AL1(4),XL4'00'                                              
BTABDATE DC    C'D',AL1(4),XL4'00'                                              
BTABWEEK DC    C'W',AL1(4),XL4'00'                                              
BTABMNTH DC    C'M',AL1(4),XL4'00'                                              
BTABFOUR DC    C'4',AL1(4),XL4'00'                                              
BTABQURT DC    C'Q',AL1(4),XL4'00'                                              
BTABDAY  DC    C'Y',AL1(4),XL4'00'                                              
BTABTIME DC    C'T',AL1(4),XL4'00'                                              
BTABLEN  DC    C'L',AL1(4),XL4'00'                                              
BTABCLI  DC    C'C',AL1(4),XL4'00'                                              
BTABGRP  DC    C'G',AL1(4),XL4'00'                                              
BTABDPT  DC    C'S',AL1(4),XL4'00'                                              
BTABPROG DC    C'P',AL1(16),CL16' '                                             
         EJECT                                                                  
*              PUT PAGE DETAILS INTO KEY                                        
         SPACE 3                                                                
BUFFPAGE NTR1                                                                   
         XC    BUFFKPAG,BUFFKPAG                                                
         CLI   TOGOPT,C'Y'         FOR CLIENTS TOGETHER                         
         BE    BUP2                                                             
         CLC   SPLCLI(3),=C'ALL'                                                
         BE    BUP1                                                             
         CLI   SPLCLI,C'*'         FOR CLI BY OFFICE                            
         BNE   BUP2                                                             
BUP1     MVC   BUFFCLI,BTABCLI+2                                                
         SPACE 1                                                                
BUP2     CLI   SPLPRO+1,C'='                                                    
         BNE   *+10                                                             
         MVC   BUFFGRP,BTABGRP+2                                                
         SPACE 1                                                                
         CLC   SPLPRO(3),=C'ALL'                                                
         BNE   *+10                                                             
         MVC   BUFFPRD,BTABBRND+2                                               
         SPACE 1                                                                
         CLC   SPLEST(3),=C'ALL'                                                
         BNE   BUP6                                                             
         MVC   BUFFEST,BTABEST+2   FOR ALL OR RANGE                             
         SPACE 1                                                                
BUP6     CLC   SPLNET(3),=C'ALL'                                                
         BNE   *+10                                                             
         MVC   BUFFNET,BTABNET+2                                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              PUT OUT ALL TOTAL RECORDS                                        
         SPACE 3                                                                
BALLPUT  NTR1                                                                   
         OC    BUFFNET,BUFFNET     ALL NETWORK TOTALS                           
         BZ    *+14                                                             
         MVC   BUFFNET,FF                                                       
         BAS   RE,BUFFPUT                                                       
         SPACE 1                                                                
         OC    BUFFEST,BUFFEST     ALL ESTIMATE TOTALS                          
         BZ    *+14                                                             
         MVC   BUFFEST,FF                                                       
         BAS   RE,BUFFPUT                                                       
         SPACE 1                                                                
         OC    BUFFPRD,BUFFPRD     ALL PRODUCT TOTALS                           
         BZ    *+14                                                             
         MVC   BUFFPRD,FF                                                       
         BAS   RE,BUFFPUT                                                       
         SPACE 1                                                                
         OC    BUFFGRP,BUFFGRP     ALL GROUP TOTALS                             
         BZ    *+14                                                             
         MVC   BUFFGRP,FF                                                       
         BAS   RE,BUFFPUT                                                       
         SPACE 1                                                                
         OC    BUFFCLI,BUFFCLI     ALL CLIENT TOTALS                            
         BZ    *+14                                                             
         MVC   BUFFCLI,FF                                                       
         BAS   RE,BUFFPUT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ANALYZE UNITDATE BY WEEK/MONTH/QUARTER                
         SPACE 3                                                                
ANALDATE NTR1                                                                   
         LA    R2,WLIST                                                         
         LA    R3,WORK                                                          
         BAS   RE,AD2                                                           
         LA    R2,MLIST                                                         
         LA    R3,WORK+1                                                        
         BAS   RE,AD2                                                           
         LA    R2,QLIST                                                         
         LA    R3,WORK+2                                                        
         BAS   RE,AD2                                                           
         B     XIT                                                              
         SPACE 1                                                                
AD2      LA    R1,1                                                             
         SPACE 1                                                                
AD3      STC   R1,0(R3)            LOOK UP DATE NUMBER                          
         CLI   4(R2),X'FF'                                                      
         BER   RE                                                               
         CLC   NBACTDAT,2(R2)      DOES UNIT FIT HERE                           
         BH    AD4                                                              
         BR    RE                                                               
         SPACE 1                                                                
AD4      LA    R2,4(R2)                                                         
         LA    1,1(R1)                                                          
         B     AD3                                                              
         EJECT                                                                  
*              ROUTINE TO FILL BUFFALO COLUMNS                                  
         SPACE 3                                                                
BUFFCOLS NTR1                                                                   
         MVC   BUFFLEFT(72),SPACES                                              
         XC    BUFFUNIT(132),BUFFUNIT                                           
         MVI   BUFFORCE+3,1                                                     
         CLI   SPLFLAV,C'E'                                                     
         BNE   BC2                                                              
         LH    R0,NBACTUN                                                       
         ST    R0,BUFFUNIT                                                      
*                                                                               
BC1      MVC   BUFFASS,ASSDOLS                                                  
         MVC   BUFFACT,ACTDOLS                                                  
         MVC   BUFFINT,NBINTEG                                                  
         SPACE 1                                                                
         L     R1,NBPAYTGR         PAID                                         
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         ST    R1,BUFFPAID                                                      
         MVC   BUFFIPD,NBPAYIGR                                                 
         L     R1,NBBILTGR         BILLED                                       
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         ST    R1,BUFFBILL                                                      
         MVC   BUFFIBL,NBBILIGR                                                 
         TM    NBUNITST,X'42'      IF PRE-EMPT OR MISSED                        
         BZ    BCF                                                              
         XC    BUFFASS,BUFFASS     SET COSTS TO 0                               
         XC    BUFFINT,BUFFINT     BUT LEAVE BILLING AND PAYING                 
         XC    BUFFACT,BUFFACT                                                  
BCF      CLI   SPLFILT,C'1'                                                     
         BL    XIT                                                              
         XC    BUFFCUT,BUFFCUT                                                  
         BAS   RE,FILT                                                          
         B     XIT                                                              
         EJECT                                                                  
*              BUFFALO COLUMNS - POST                                           
         SPACE 3                                                                
BC2      CLI   SPLFLAV,C'P'        POST - ESTIMATE                              
         BNE   BC8                                                              
         LH    R0,NBESTUN                                                       
         ST    R0,BUFFUNIT                                                      
         LA    R1,0                                                             
         ST    R1,BUFFASS           COSTS=0 IF ESTUN=0                          
         ST    R1,BUFFCOST                                                      
         OC    SAVESTUN,SAVESTUN                                                
         BZ    BC3                                                              
*                                                                               
         L     R1,NBINTEG                    ESTIMATED                          
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CLI   NBUSER+15,C'Y'              (OPTIONALLY + INTEG)                 
         BE    *+6                                                              
         SR    R1,R1                                                            
         LR    R0,R1                                                            
         A     R0,ASSDOLS                                                       
         ST    R0,BUFFASS                                                       
         A     R1,ACTDOLS                                                       
         ST    R1,BUFFCOST                                                      
BC3      MVC   BUFFRH+2(2),NBESTHOM+2                                           
         MVC   BUFFR1+2(2),NDESTDEM+2                                           
         MVC   BUFFR2+2(2),NDESTDEM+10                                          
         MVC   BUFFR3+2(2),NDESTDEM+18                                          
         MVC   BUFFIH,NBESTHOM+4                                                
         MVC   BUFFI1,NDESTDEM+4                                                
         MVC   BUFFI2,NDESTDEM+12                                               
         MVC   BUFFI3,NDESTDEM+20                                               
         MVC   BUFFHT+2(2),NBESTHUT                                             
         MVC   BUFFSH+2(2),NBESTSHR                                             
         MVC   BUFFV1+2(2),NDESTDEM                                             
         MVC   BUFFV2+2(2),NDESTDEM+8                                           
         MVC   BUFFV3+2(2),NDESTDEM+16                                          
         SPACE 1                                                                
BC4      DS    0H                                                               
         LH    R0,NBACTUN                                                       
         ST    R0,BUFFAUN                                                       
         LA    R1,0                                                             
         ST    R1,BUFFAASS          COSTS=0 IF ACTUN=0                          
         ST    R1,BUFFACST                                                      
         OC    SAVACTUN,SAVACTUN                                                
         BZ    BC5                                                              
         L     R1,NBINTEG                                                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CLI   NBUSER+15,C'Y'                                                   
         BE    *+6                                                              
         SR    R1,R1                                                            
         LR    R0,R1                                                            
         A     R0,ASSDOLS                                                       
         ST    R0,BUFFAASS                                                      
         A     R1,ACTDOLS                                                       
         ST    R1,BUFFACST                                                      
BC5      MVC   BUFFARH+2(2),NBACTHOM+2                                          
         MVC   BUFFAR1+2(2),NDACTDEM+2                                          
         MVC   BUFFAR2+2(2),NDACTDEM+10                                         
         MVC   BUFFAR3+2(2),NDACTDEM+18                                         
         MVC   BUFFAIH,NBACTHOM+4                                               
         MVC   BUFFAI1,NDACTDEM+4                                               
         MVC   BUFFAI2,NDACTDEM+12                                              
         MVC   BUFFAI3,NDACTDEM+20                                              
         MVC   BUFFAHT+2(2),NBACTHUT                                            
         MVC   BUFFASH+2(2),NBACTSHR                                            
         MVC   BUFFAV1+2(2),NDACTDEM                                            
         MVC   BUFFAV2+2(2),NDACTDEM+8                                          
         MVC   BUFFAV3+2(2),NDACTDEM+16                                         
         CLI   NBUSER+8,C'Y'       OPTION TO USE ASSIGNED COSTS                 
         BNE   BC6                                                              
         MVC   BUFFCOST,ASSDOLS                                                 
         MVC   BUFFACST,ASSDOLS                                                 
*                                                                               
BC6      CLI   SPLFLAV+1,C'A'      AUDIT TRAIL                                  
         BE    BC7                                                              
         CLI   SPLFLAV+1,C'M'                                                   
         BE    BC7                                                              
         CLI   SPLFLAV+1,C'P'                                                   
         BNE   XIT                                                              
         SPACE 1                                                                
BC7      L     RF,=A(MAKEGOOD)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     XIT                                                              
         EJECT                                                                  
*              BUFFALO COLUMNS - EVALUATION                                     
         SPACE 3                                                                
BC8      CLI   SPLFLAV+1,C'2'      ESTIMATE                                     
         BE    BC10                                                             
         CLI   SPLFLAV+1,C'3'                                                   
         BE    BC10                                                             
         MVC   BUFFCOST,ACTDOLS                                                 
         MVC   BUFFASS,ASSDOLS                                                  
         CLI   NBUSER+8,C'Y'       OPTION TO USE ASSIGNED COSTS                 
         BNE   BC9                                                              
         MVC   BUFFCOST,ASSDOLS                                                 
*                                                                               
BC9      LH    R0,NBESTUN                                                       
         ST    R0,BUFFUNIT                                                      
         MVC   BUFFRH+2(2),NBESTHOM+2                                           
         MVC   BUFFR1+2(2),NDESTDEM+2                                           
         MVC   BUFFR2+2(2),NDESTDEM+10                                          
         MVC   BUFFR3+2(2),NDESTDEM+18                                          
         MVC   BUFFHT+2(2),NBESTHUT                                             
         MVC   BUFFSH+2(2),NBESTSHR                                             
         MVC   BUFFV1+2(2),NDESTDEM                                             
         MVC   BUFFV2+2(2),NDESTDEM+8                                           
         MVC   BUFFV3+2(2),NDESTDEM+16                                          
         MVC   BUFFIH,NBESTHOM+4                                                
         MVC   BUFFI1,NDESTDEM+4                                                
         MVC   BUFFI2,NDESTDEM+12                                               
         MVC   BUFFI3,NDESTDEM+20                                               
         CLI   SPLFLAV+1,C'2'      FUDGE FOR V2                                 
         BNE   XIT                                                              
         LA    R0,1                                                             
         ST    R0,BUFFUNIT                                                      
         B     XIT                                                              
         SPACE 1                                                                
BC10     LH    R0,NBACTUN          ACTUAL                                       
         ST    R0,BUFFUNIT                                                      
         MVC   BUFFCOST,ACTDOLS                                                 
         MVC   BUFFASS,ASSDOLS                                                  
         CLI   NBUSER+8,C'Y'       OPTION TO USE ASSIGNED COSTS                 
         BNE   BC11                                                             
         MVC   BUFFCOST,ASSDOLS                                                 
BC11     MVC   BUFFRH+2(2),NBACTHOM+2                                           
         MVC   BUFFR1+2(2),NDACTDEM+2                                           
         MVC   BUFFR2+2(2),NDACTDEM+10                                          
         MVC   BUFFR3+2(2),NDACTDEM+18                                          
         MVC   BUFFHT+2(2),NBACTHUT                                             
         MVC   BUFFSH+2(2),NBACTSHR                                             
         MVC   BUFFV1+2(2),NDACTDEM                                             
         MVC   BUFFV2+2(2),NDACTDEM+8                                           
         MVC   BUFFV3+2(2),NDACTDEM+16                                          
         MVC   BUFFIH,NBACTHOM+4                                                
         MVC   BUFFI1,NDACTDEM+4                                                
         MVC   BUFFI2,NDACTDEM+12                                               
         MVC   BUFFI3,NDACTDEM+20                                               
         CLI   SPLFLAV+1,C'2'      FLAVOR 2 USES ESTIMATED DEMOS                
         BE    BC9                                                              
         B     XIT                                                              
         EJECT                                                                  
*              DYNAMICALLY FILL KEY FROM DATA LIST                              
         SPACE 3                                                                
BFILL    NTR1                                                                   
         CLI   BUFFSUB,2           ADJUST DATES FOR SUB-TOTS                    
         BNE   *+10                                                             
         MVC   BTABDATE+4(2),=X'FFFF'                                           
         CLI   BUFFSUB,5                                                        
         BNE   BF1                                                              
         MVC   BTABWEEK+4(2),=X'FFFF'                                           
         MVC   BTABMNTH+4(2),=X'FFFF'                                           
         MVC   BTABFOUR+4(2),=X'FFFF'                                           
         SPACE 1                                                                
BF1      CLI   0(R2),0                                                          
         BE    XIT                                                              
         LA    R4,BUFFKEY                                                       
         SPACE 1                                                                
BF2      LA    R1,BTABBRND                                                      
         SPACE 1                                                                
BF4      CLC   0(1,R1),0(R2)       LOCATE VALUE IN TABLE                        
         BE    BF6                                                              
         LA    R1,6(R1)                                                         
         B     BF4                                                              
         SPACE 1                                                                
BF6      ZIC   R5,1(R1)            GET LENGTH                                   
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),2(R1)       AND MOVE TO KEY                              
         SPACE 1                                                                
         CLI   BUFFSUB,2           ARE WE DOING SUB-TOTS                        
         BNE   BF10                                                             
         CLI   SUBTOT,C'P'         BY PROGRAM                                   
         BNE   BF8                                                              
         CLI   0(R1),C'P'          RECORD IS FINISHED WHEN PROGRAM              
         BE    BF12                IS IN THE SUB-RECORD                         
         B     BF10                                                             
         SPACE 1                                                                
BF8      CLI   0(R1),C'D'          BY WEEK/MONTH- RECORD IS FINISHED            
         BE    BF12                WHEN DATE IS IN.                             
         SPACE 1                                                                
BF10     LA    R2,1(R2)            OTHERWISE GO ONTO NEXT BIT                   
         LA    R4,1(R4,R5)                                                      
         BCT   R3,BF2                                                           
         CLI   SEPOPT,C'Y'         OPTION TO FORCE UNITS APART                  
         BNE   BF12                                                             
         CLI   BUFFSUB,1                                                        
         BH    BF12                                                             
         MVC   BUFFKEY+L'BUFFKEY-1(1),SEPSPOT                                   
         AI    SEPSPOT,1                                                        
         SPACE 1                                                                
BF12     BAS   RE,BUFFPUT                                                       
         B     XIT                                                              
         SPACE 1                                                                
BUFFPUT  NTR1                                                                   
         CLI   MAXOPT,C'Y'                                                      
         BNE   BUFFPUT2                                                         
         CP    MAXCOUNT,MAXMAX                                                  
         BE    XIT                                                              
         AP    MAXCOUNT,=P'1'                                                   
         SPACE 1                                                                
BUFFPUT2 GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,BUFFIO                               
         TM    TRACOPT,X'80'                                                    
         BNO   XIT                                                              
         L     RF,=A(BUFFTRCE)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     XIT                                                              
         SPACE 1                                                                
TRACOPT  DC    X'00'               X'80' TO TRACE PUTS                          
*                                  X'40' TO TRACE GETS                          
MAXOPT   DC    C'N'                OPTION TO LIMIT BUFFALO PUTS                 
MAXCOUNT DC    PL4'0'                                                           
MAXMAX   DC    PL4'100'                                                         
         EJECT                                                                  
*              ROUTINES TO FILTER BILLING / PAYING                              
         SPACE 3                                                                
FILT     NTR1                                                                   
         IF    SPLFILT,EQ,C'1',FILT1                                            
         IF    SPLFILT,EQ,C'2',FILT2                                            
         IF    SPLFILT,EQ,C'3',FILT3                                            
         IF    SPLFILT,EQ,C'4',FILT4                                            
         IF    SPLFILT,EQ,C'5',FILT5                                            
         B     XIT                                                              
         SPACE 1                                                                
FILT1    OC    NBPAYTGR,NBPAYTGR   CLEARED                                      
         BNZ   *+10                                                             
         XC    ACUN(12),ACUN                                                    
         OC    NBPAYIGR,NBPAYIGR                                                
         BNZ   XIT                                                              
         XC    ACINT,ACINT                                                      
         B     XIT                                                              
         SPACE 1                                                                
FILT2    OC    NBPAYTGR,NBPAYIGR   UNCLEARED                                    
         BZ    *+10                                                             
         XC    ACUN(12),ACUN                                                    
         OC    NBPAYIGR,NBPAYIGR                                                
         BZ    XIT                                                              
         XC    ACINT,ACINT                                                      
         B     XIT                                                              
         SPACE 1                                                                
FILT3    OC    NBBILTGR,NBBILTGR   BILLED                                       
         BNZ   *+10                                                             
         XC    ACUN(12),ACUN                                                    
         OC    NBBILIGR,NBBILIGR                                                
         BNZ   XIT                                                              
         XC    ACINT,ACINT                                                      
         B     XIT                                                              
         SPACE 1                                                                
FILT4    OC    NBBILTGR,NBBILTGR   BILLABLE                                     
         BZ    *+10                                                             
         XC    ACUN(12),ACUN                                                    
         OC    NBBILIGR,NBBILIGR                                                
         BZ    XIT                                                              
         XC    ACINT,ACINT                                                      
         B     XIT                                                              
         SPACE 1                                                                
FILT5    TM    NBUNITST,X'20'      UNALLOCATED                                  
         BNO   XIT                                                              
         XC    ACUN(28),ACUN                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL REPORTS                                       
         SPACE 3                                                                
OVREPS   NTR1                                                                   
         XC    ACTSUBS,ACTSUBS                                                  
         MVI   LASTRES,0                                                        
         MVI   LASTTYPE,0                                                       
         MVC   LASTP,SPACES                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         XC    BUFFIO(40),BUFFIO                                                
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFFC,BUFFIO,1                            
         B     OV4                                                              
         SPACE 1                                                                
OV2      GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFFC,BUFFIO,1                             
         SPACE 1                                                                
OV4      CLI   DMCB+8,X'80'                                                     
         BE    OVXIT                                                            
         TM    TRACOPT,X'40'       OPTION TO TRACE GETS                         
         BNO   OV4A                                                             
         L     RF,=A(BUFFTRCE)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         SPACE 1                                                                
OV4A     OC    SUBDISP,SUBDISP                                                  
         BNZ   *+12                                                             
         LA    R1,P+4                                                           
         ST    R1,SUBDISP                                                       
         CLI   BUFFTYPE,2                                                       
         BE    OV22                                                             
         CLI   LASTTYPE,2                                                       
         BNE   OV4B                                                             
         MVC   LASTP,SPACES                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         SPACE 1                                                                
OV4B     MVI   LASTTYPE,1                                                       
         CLI   BUFFSUB,2                                                        
         BE    OV6                                                              
         BH    OV10                                                             
         CLI   BUFFSUB,1                                                        
         BE    OV5                                                              
         L     RF,=A(ANYACT)                                                    
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         LA    R2,DETAILS          DETAILS                                      
         ZIC   R3,NDETAILS                                                      
         BAS   RE,CLS                                                           
         BAS   RE,FORM                                                          
         B     OV2                                                              
         SPACE 1                                                                
OV5      GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R1,SUBDISP          COMMENTS ARE INDENTED                        
         MVC   0(72,R1),BUFFLEFT                                                
******   MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     OV2                                                              
         SPACE 1                                                                
OV6      GOTO1 SPOOL,DMCB,(R8)     SUBTOTALS                                    
         L     R1,SUBDISP                                                       
         MVC   0(11,R1),=C'WEEK TOTALS'                                         
         CLI   SUBTOT,C'W'                                                      
         BE    OV8                                                              
         MVC   0(12,R1),=C'MONTH TOTALS'                                        
         CLI   SUBTOT,C'M'                                                      
         BE    OV8                                                              
         MVC   0(14,R1),=C'PROGRAM TOTALS'                                      
         CLI   SUBTOT,C'P'                                                      
         BE    OV8                                                              
         MVC   0(7,R1),=C'QUARTER'                                              
         CLI   SUBTOT,C'Q'                                                      
         BE    OV8                                                              
         MVC   0(14,R1),=C'PERIOD TOTALS '                                      
         SPACE 1                                                                
OV8      MVI   SPACING,2                                                        
         BAS   RE,FORM                                                          
         B     OV2                                                              
         EJECT                                                                  
*              ROUTINES TO CONTROL PAGE TOTALS AND RECAPS                       
         SPACE 3                                                                
OV10     MVI   FORCEHED,C'N'                                                    
         XC    ACTSUBS,ACTSUBS                                                  
         MVI   LASTRES,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SPACING,2                                                        
         L     R1,SUBDISP                                                       
         MVC   0(13,R1),=C'REPORT TOTALS'                                       
         OC    BUFFKPAG,BUFFKPAG                                                
         BZ    OV18                                                             
         MVC   0(13,R1),SPACES                                                  
         CLI   BUFFNET,0                                                        
         BE    OV12                                                             
         MVC   0(15,R1),=C'NETWORK TOTALS '                                     
         CLI   BUFFNET,X'FF'                                                    
         BNE   OV18                                                             
         MVC   0(15,R1),=C'ALL NETWORKS   '                                     
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
OV12     CLI   BUFFEST,0                                                        
         BE    OV14                                                             
         MVC   0(15,R1),=C'ESTIMATE TOTALS'                                     
         CLI   BUFFEST,X'FF'                                                    
         BNE   OV18                                                             
         MVC   0(15,R1),=C'ALL ESTIMATES  '                                     
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
OV14     CLI   BUFFPRD,0                                                        
         BE    OV15                                                             
         MVC   0(15,R1),=C'BRAND TOTALS   '                                     
         CLI   BUFFPRD,X'FF'                                                    
         BNE   OV18                                                             
         MVC   0(15,R1),=C'ALL BRANDS     '                                     
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
OV15     CLI   BUFFGRP,0                                                        
         BE    OV16                                                             
         MVC   0(15,R1),=C'GROUP TOTALS   '                                     
         CLI   BUFFGRP,X'FF'                                                    
         BNE   OV18                                                             
         MVC   0(15,R1),=C'ALL GROUPS     '                                     
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
OV16     CLI   BUFFCLI,0                                                        
         BE    OV18                                                             
         MVC   0(15,R1),=C'CLIENT TOTALS  '                                     
         CLI   BUFFCLI,X'FF'                                                    
         BNE   OV18                                                             
         MVC   0(15,R1),=C'ALL CLIENTS    '                                     
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
OV18     L     R4,COLDISP          CHECK WE HAD ROOM                            
         BCTR  R4,0                                                             
         CLC   0(3,R4),SPACES                                                   
         BE    OV20                                                             
         MVC   P2,P                                                             
         MVC   P,SPACES                                                         
         SPACE 1                                                                
OV20     BAS   RE,FORM                                                          
         MVI   FORCEHED,C'Y'                                                    
         B     OV2                                                              
         SPACE 1                                                                
OV22     MVI   LASTTYPE,2                                                       
         CLI   BUFFSUB,4                                                        
         BE    OV24                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SPACING,2                                                        
         SPACE 1                                                                
OV24     LA    R2,RECAP                                                         
         ZIC   R3,NRECAP                                                        
         BAS   RE,CLS                                                           
         BAS   RE,FORM                                                          
         B     OV2                                                              
*                                                                               
OVXIT    BAS   RE,ENDCOMM                                                       
         B     XIT                                                              
*        EJECT                                                                  
*  PRINT END COMMENTS                                                           
****                                                                            
ENDCOMM  NTR1                                                                   
         LA    R4,COMMAREA                                                      
         USING NCOMBLKD,R4                                                      
         MVI   NCBBOT,C'Y'         LAST PAGE COMMENTS                           
         MVI   NCBKDFL,C'Y'        DEFAULT TO NEXT KEY                          
         MVI   NCBMULT,C'Y'        PASS MULTIPLE COMMENTS                       
         MVC   NCBAIO,NBAIO                                                     
*                                                                               
         MVI   NBFUNCT,NBFRDHI     NEXT NETIO CALL MUST DO READ HI 1ST          
*                                  (SEQ WON'T WORK DUE TO COMMENT I/O)          
         LA    R2,NETBLOCK                                                      
         ST    R2,NCBNETB                                                       
         LA    R2,ENDCOMHK                                                      
         ST    R2,NCBAHOOK                                                      
*                                                                               
         MVC   NCBAM,NBACTAM                                                    
         MVC   NCBID(2),SPLCOM        COMMENT FROM SCREEN                       
         CLI   SPLCOM+2,0             CK IF TOP COMMENTS REQUESTED              
         BE    ECOM2                                                            
         CLI   SPLCOM+2,C'*'                                                    
         BE    ECOM2                                                            
         CLI   SPLCOM+2,C'4'                                                    
         BNE   ECOMXIT                                                          
ECOM2    MVI   NCBID+2,C'4'                                                     
         MVC   NCBCLT,NBACTCLI                                                  
         MVC   NCBPRD,NBEFFPNM                                                  
         MVC   NCBEST,NBSELEST                                                  
         MVC   NCBNTWK,NBSELNET                                                 
         GOTO1 ANETCOM,DMCB,NCOMBLKD                                            
         SPACE                                                                  
ECOMXIT  B     XIT                                                              
         SPACE                                                                  
*                                                                               
ENDCOMHK NTR1                                                                   
         L     R3,CURCOMM                                                       
         SPACE                                                                  
         L     R2,NBAIO                                                         
         USING NCOMELEM,R2                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   EHOOKX                                                           
ECHKLOP  ZIC   R1,NCOMELEN                                                      
         SH    R1,=H'4'                                                         
         EXMVC R1,P1,NCOMETXT                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,NEXTEL           GET NEXT                                     
         BE    ECHKLOP                                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)     PRINT 2 BLANK LINES                          
EHOOKX   B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
FORM     NTR1                                                                   
         L     RF,=A(FORMC)                                                     
         B     CLS2                                                             
         SPACE 1                                                                
CLS      NTR1                                                                   
         L     RF,=A(COLSC)                                                     
         SPACE 1                                                                
CLS2     A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     XIT                                                              
         SPACE 1                                                                
         GETEL (R2),DATADISP,ELCODE                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG/CONSTANTS/CSECTS                                           
         SPACE 3                                                                
RELOC    DC    A(*)                                                             
FF       DC    X'FFFFFFFFFFFF'                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO HANDLE DATA COLUMNS                                   
         SPACE 3                                                                
COLSC    CSECT                                                                  
         USING *,RF                                                             
COLSNTR  NTR1                                                                   
         LA    RB,COLSC                                                         
         DROP  RF                                                               
         USING COLSC,RB                                                         
         LA    R4,P                                                             
         L     R1,DETINSET                                                      
         CLI   BUFFTYPE,1                                                       
         BE    *+8                                                              
         L     R1,RECINSET                                                      
         AR    R4,R1                                                            
         XC    SUBDISP,SUBDISP                                                  
         LA    R5,BUFFKEY                                                       
         SPACE 1                                                                
CL2      L     R1,=A(COLTAB)                                                    
         A     R1,RELO                                                          
         SPACE 1                                                                
CL4      CLC   0(1,R1),0(R2)                                                    
         BE    CL6                                                              
         LA    R1,32(R1)                                                        
         B     CL4                                                              
         SPACE 1                                                                
CL6      ZIC   R0,1(R1)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   CL7                                                              
         MVC   0(5,R4),=C'*TOT*'                                                
         CLI   1(R1),4                                                          
         BNE   COLSXIT                                                          
         MVC   0(5,R4),=C'TOT  '                                                
         B     COLSXIT                                                          
         SPACE 1                                                                
         SPACE 1                                                                
CL7      CLI   0(R2),C'C'          CLIENT                                       
         BNE   CL8                                                              
         MVC   2(3,R4),0(R5)                                                    
         B     CL20                                                             
         SPACE 1                                                                
CL8      CLI   0(R2),C'B'          BRAND                                        
         BNE   CL9                                                              
         BAS   RE,COLBRAND                                                      
         B     CL20                                                             
         SPACE 1                                                                
CL9      CLI   0(R2),C'G'          GROUP                                        
         BNE   CL10                                                             
         CLI   BRLEN,0                                                          
         BE    CL20                                                             
         MVC   0(1,R4),0(R5)                                                    
         UNPK  WORK(5),1(3,R5)                                                  
         ZIC   R1,BRLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),WORK                                                     
         GOTO1 CENTER,DMCB,(R4),5                                               
         B     CL20                                                             
         SPACE 1                                                                
CL10     CLI   0(R2),C'4'                         ODD MONTHS                    
         BNE   CL12                                                             
         MVC   0(5,R4),=C'*TOT*'                                                
         CLI   2(R5),X'FF'                                                      
         BE    CL20                                                             
         ZIC   R1,3(R5)                                                         
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,MLIST(R1)                                                     
         MVC   WORK(4),0(R1)                                                    
         GOTO1 DATCON,DMCB,(2,WORK),(4,0(R4))                                   
         GOTO1 DATCON,DMCB,(2,WORK+2),(4,6(R4))                                 
         MVI   5(R4),C'-'                                                       
         B     CL20                                                             
         SPACE 1                                                                
CL12     CLI   0(R2),C'W'                         WEEKS                         
         BNE   CL14                                                             
         CLI   2(R5),X'FF'                                                      
         BE    CL13                                                             
         ZIC   R1,3(R5)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'5'                                                         
         LA    R1,EWLIST(R1)                                                    
         MVC   0(5,R4),0(R1)                                                    
         B     CL20                                                             
         SPACE 1                                                                
CL13     MVC   0(5,R4),=C'*TOT*'                  WEEK SUB-TOTS                 
         CLI   2(R2),C'M'                                                       
         BNE   CL20                                                             
         ZIC   R1,1(R5)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'5'                                                         
         LA    R1,EMLIST(R1)                                                    
         MVC   0(5,R4),0(R1)                                                    
         B     CL20                                                             
         SPACE 1                                                                
CL14     CLI   0(R2),C'M'                                                       
         BNE   CL15                               MONTH                         
         MVC   0(5,R4),=C'*TOT*'                                                
         CLI   2(R5),X'FF'                                                      
         BE    CL20                                                             
         ZIC   R1,3(R5)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'5'                                                         
         LA    R1,EMLIST(R1)                                                    
         MVC   0(5,R4),0(R1)                                                    
         B     CL20                                                             
         SPACE 1                                                                
CL15     CLI   0(R2),C'Q'                         QUARTER                       
         BNE   CL16                                                             
         ZIC   R1,3(R5)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'11'                                                        
         LA    R1,EQLIST(R1)                                                    
         MVC   0(11,R4),0(R1)                                                   
         B     CL20                                                             
         SPACE 1                                                                
CL16     CLI   0(R2),C'D'                                                       
         BNE   CL17                                                             
         GOTO1 DATCON,DMCB,(2,2(R5)),(4,0(R4))    DATE                          
         B     CL20                                                             
         SPACE 1                                                                
CL17     CLI   0(R2),C'T'                         TIME                          
         BNE   CL18                                                             
         GOTO1 UNTIME,DMCB,0(R5),(R4)                                           
         B     CL20                                                             
         SPACE 1                                                                
CL18     CLI   0(R2),C'Y'                         DAY                           
         BNE   CL19                                                             
         MVC   0(3,R4),1(R5)                                                    
         B     CL20                                                             
         SPACE 1                                                                
CL19     CLI   0(R2),C'S'                         DAYPART                       
         BNE   CLOTH                                                            
         MVC   0(1,R4),0(R5)                                                    
         LA    R1,DPTTAB                                                        
         LA    RF,DAYPARTS                                                      
         SPACE 1                                                                
CL19B    CLC   0(1,R5),0(R1)                                                    
         BE    CL19D                                                            
         LA    R1,L'DPTTAB(R1)                                                  
         BCT   RF,CL19B                                                         
         B     CL20                                                             
         SPACE 1                                                                
CL19D    MVC   0(7,R4),1(R1)                                                    
         B     CL20                                                             
         SPACE 1                                                                
* TABLE OF DAYPART CODES AND NAMES                                              
*                                                                               
DPTTAB   DS    0CL8                                                             
         DC    C'D',CL7'DAYTIME'                                                
         DC    C'F',CL7'FRINGE'                                                 
         DC    C'P',CL7'PRIME'                                                  
         DC    C'K',CL7'KIDS'                                                   
         DC    C'T',CL7'TEENS'                                                  
         DC    C'Y',CL7'YOUTH'                                                  
         DC    C'S',CL7'SPORTS'                                                 
         DC    C'N',CL7'NEWS'                                                   
         DC    C'E',CL7'EARLY'                                                  
         DC    C'L',CL7'LATE'                                                   
         DC    C'C',CL7'CABLE'                                                  
         DC    C'X',CL7'SYND.'                                                  
         DC    C'X',CL7'X'                                                      
         DC    C'I',CL7'SPECIAL'                                                
         DC    C'I',CL7'I'                                                      
DAYPARTS EQU   (*-DPTTAB)/L'DPTTAB                                              
         SPACE 1                                                                
CLOTH    LR    R1,R0                              OTHERS                        
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     CL20                                                             
         MVC   0(0,R4),0(R5)                                                    
         SPACE 1                                                                
CL20     AR    R4,R0                                                            
         OC    SUBDISP,SUBDISP                                                  
         BNZ   *+8                                                              
         ST    R4,SUBDISP                                                       
         LA    R5,4(R5)                                                         
         CLI   0(R2),C'P'                                                       
         BNE   *+8                                                              
         LA    R5,12(R5)                                                        
         LA    R2,1(R2)                                                         
         BCT   R3,CL2                                                           
         ST    R4,COLDISP                                                       
         CLI   BUFFTYPE,1                                                       
         BE    COLSXIT                                                          
         MVC   SAVEP,P                                                          
         LA    R2,P                TRIM DOWN DUPLICATE FIELDS ON RECAPS         
         LA    R3,LASTP                                                         
         ZIC   R4,NRECAP                                                        
         SPACE 1                                                                
CL22     LR    R1,R2                                                            
         SPACE 1                                                                
CL24     CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         LA    R1,1(R1)                                                         
         B     CL24                                                             
         SPACE 1                                                                
CL26     CLI   0(R1),C' '                                                       
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     CL26                                                             
         SR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,CLCLC                                                         
         BNE   CL28                                                             
         EX    R1,CLMVC                                                         
         LA    R1,1(R1)                                                         
         AR    R2,R1                                                            
         AR    R3,R1                                                            
         BCT   R4,CL22                                                          
         SPACE 1                                                                
CL28     MVC   LASTP,SAVEP                                                      
         B     COLSXIT                                                          
         SPACE 1                                                                
CLCLC    CLC   0(0,R2),0(R3)                                                    
CLMVC    MVC   0(0,R2),SPACES                                                   
         EJECT                                                                  
*              HANDLE BRAND(S) EDITING                                          
         SPACE 3                                                                
COLBRAND NTR1                                                                   
*                                  R4=A(OUTPUT) WILL BE AAA-BBB                 
*                                  R5=A(INPUT)  IS AAAB                         
         MVC   0(3,R4),0(R5)       OUTPUT FIRST BRAND                           
         CLI   3(R5),0             IS THERE A SECOND BRAND                      
         BE    COLSXIT                                                          
         L     R2,ANETWS1          CLIENT RECORD IS AT W/S AREA 1               
         USING CLTHDR,R2                                                        
         LA    R2,CLIST                                                         
         DROP  R2                                                               
         LA    R4,2(R4)            BUMP PAST 2/3 CHARACTER CODE                 
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'-'          DASH BETWEEN BRANDS                          
         SPACE 1                                                                
COLBR2   CLC   3(1,R5),3(R2)       LOOK UP BRAND                                
         BE    *+12                                                             
         LA    R2,4(R2)                                                         
         B     COLBR2                                                           
         MVC   1(3,R4),0(R2)       NOW OUTPUT SECOND BRAND                      
         SPACE 1                                                                
COLSXIT  XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO TRACE BUFFALO PUTS                                    
         SPACE 3                                                                
BUFFTRCE CSECT                                                                  
         USING *,RF                                                             
BFTRC    NTR1                                                                   
         LA    RB,BUFFTRCE                                                      
         DROP  RF                                                               
         USING BUFFTRCE,RB                                                      
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         MVC   P,BUFFIO                                                         
         GOTO1 HEXOUT,DMCB,BUFFIO,P2,132,=C'SEP'                                
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,P2                                                             
         BASR  RE,RF                                                            
         MVC   P,P3                                                             
         BASR  RE,RF                                                            
         MVC   P,BUFFIO+132                                                     
         GOTO1 HEXOUT,DMCB,BUFFIO+132,P2,132,=C'SEP'                            
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,P2                                                             
         BASR  RE,RF                                                            
         MVC   P,P3                                                             
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         BASR  RE,RF                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
****************************************                                        
* ROUTINE TO ADJUST COSTS BY A PERCENTAGE.                                      
*                                                                               
*   INPUT - ACCPCTG - PERCENTAGE TO ADJUST BY                                   
*   OUTPUT - NBACTUAL,NBASSIGN,NBINTEG,NBBILIGR,NBBILTGR,                       
*            NBPAYTGR,NBPAYIGR - COSTS WHICH ARE ADJUSTED                       
******************************************                                      
PROCCOST CSECT                                                                  
         USING *,RF                                                             
PCST     NTR1                                                                   
         LA    RB,PROCCOST                                                      
         DROP  RF                                                               
         USING PROCCOST,RB                                                      
         MVC   HOLDPCTG,ACCPCTG     ADJUST FOR ACCT PCTG                        
*                                                                               
         OC    HOLDPCTG,HOLDPCTG   IF 0 OR 100 DONT ADJUST                      
         BZ    XITAPCT                                                          
         CLC   HOLDPCTG,=F'10000'      (TWO DEC PLACES)                         
         BE    XITAPCT                                                          
*                                                                               
         L     R5,NBACTUAL         ADJUST ACTUAL                                
         M     R4,HOLDPCTG                                                      
         A     R5,=F'5000'         MULTIPLY AND ROUND                           
         D     R4,=F'10000'        RESULT IN R5                                 
         ST    R5,NBACTUAL                                                      
*                                                                               
         L     R5,NBASSIGN         ADJUST ASSIGNED                              
         M     R4,HOLDPCTG                                                      
         A     R5,=F'5000'         MULTIPLY AND ROUND                           
         D     R4,=F'10000'        RESULT IN R5                                 
         ST    R5,NBASSIGN                                                      
*                                                                               
         L     R5,NBINTEG          ADJUST INTEG                                 
         M     R4,HOLDPCTG                                                      
         A     R5,=F'5000'         MULTIPLY AND ROUND                           
         D     R4,=F'10000'        RESULT IN R5                                 
         ST    R5,NBINTEG                                                       
*                                                                               
         L     R5,NBPAYIGR         ADJUST PAID INT GROSS                        
         M     R4,HOLDPCTG                                                      
         A     R5,=F'5000'         MULTIPLY AND ROUND                           
         D     R4,=F'10000'        RESULT IN R5                                 
         ST    R5,NBPAYIGR                                                      
*                                                                               
         L     R5,NBPAYTGR         ADJUST PAID TIME GROSS                       
         M     R4,HOLDPCTG                                                      
         A     R5,=F'5000'         MULTIPLY AND ROUND                           
         D     R4,=F'10000'        RESULT IN R5                                 
         ST    R5,NBPAYTGR                                                      
*                                                                               
         L     R5,NBBILIGR         ADJUST BILLED GROSS                          
         M     R4,HOLDPCTG                                                      
         A     R5,=F'5000'         MULTIPLY AND ROUND                           
         D     R4,=F'10000'        RESULT IN R5                                 
         ST    R5,NBBILIGR                                                      
*                                                                               
         L     R5,NBBILTGR         ADJUST BILLED TIME                           
         M     R4,HOLDPCTG                                                      
         A     R5,=F'5000'         MULTIPLY AND ROUND                           
         D     R4,=F'10000'        RESULT IN R5                                 
         ST    R5,NBBILTGR                                                      
*                                                                               
XITAPCT  XIT1                                                                   
         LTORG                                                                  
********************************************                                    
         EJECT                                                                  
*              ROUTINE TO CONTROL COLUMN EDITING                                
         SPACE 3                                                                
FORMC    CSECT                                                                  
         USING *,RF                                                             
FORMCNTR NTR1                                                                   
         LA    RB,FORMC                                                         
         DROP  RF                                                               
         USING FORMC,RB                                                         
         LA    R2,COLS                                                          
         ZIC   R3,NCOLS                                                         
         LTR   R3,R3                                                            
         BZ    FEND                                                             
         L     R4,COLDISP                                                       
         BCTR  R4,0                                                             
         CLI   SPLFLAV,C'E'                                                     
         BNE   F10                                                              
         BAS   RE,ESTPACK                                                       
         SPACE 1                                                                
F2       CLI   0(R2),0             GAP                                          
         BNE   F3                                                               
         LA    R2,1(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,F2                                                            
         B     FEND                                                             
         SPACE 1                                                                
F3       CLI   0(R2),25            UNITS                                        
         BNE   F4                                                               
         EDIT  (4,BUFFUNIT),(4,(R4))                                            
         LA    R4,6(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,F2                                                            
         B     FEND                                                             
         SPACE 1                                                                
F4       CLI   0(R2),11            SHOW EST-PAK                                 
         BNE   F6                                                               
         CLI   BUFFSUB,1                                                        
         BH    F8                                                               
         EDIT  (1,BUFFCHES),(3,1(R4)),FILL=0                                    
         MVI   4(R4),C'-'                                                       
         ZIC   R1,BUFFCHPK                                                      
         EDIT  (R1),(3,5(R4)),FILL=0                                            
         B     F8                                                               
         SPACE 1                                                                
F6       ZIC   R5,0(R2)                                                         
         BCTR  R5,0                                                             
         MH    R5,=H'6'                                                         
         LA    R5,ESTASS1(R5)                                                   
         CP    0(6,R5),=P'0'                                                    
         BE    F7                                                               
         EDIT  (P6,0(R5)),(12,0(R4)),2,FLOAT=-                                  
         AP    ESTTOT,0(6,R5)                                                   
         B     F8                                                               
         SPACE 1                                                                
F7       ZIC   R5,0(R2)            VALUE IS ZERO                                
         BCTR  R5,0                                                             
         LA    R5,ZERFLAGS(R5)                                                  
         CLI   0(R5),C'0'          DO WE SHOW AS 0 OR SPACE?                    
         BNE   F8                                                               
         CLI   BUFFSUB,0           AND, IF A DETAIL                             
         BNE   F8                                                               
         MVC   9(3,R4),=C'.00'                                                  
         SPACE 1                                                                
F8       LA    R2,1(R2)                                                         
         LA    R4,12(R4)                                                        
         BCT   R3,F2                                                            
         MVC   ZERFLAGS,SPACES                                                  
         B     FEND                                                             
         EJECT                                                                  
*              ROUTINE BUILDS ESTIMATE PACKED                                   
         SPACE 3                                                                
ESTPACK  NTR1                                                                   
         LA    R2,ESTASS1                                                       
         LA    R3,20                                                            
         MVC   ZERFLAGS,SPACES     PRESET FLAGS                                 
         MVI   ZEROACT,C' '                                                     
         TM    BUFFCHST,X'20'                                                   
         BNO   *+8                                                              
         MVI   ZEROACT,C'0'                                                     
         MVI   ZEROASS,C' '                                                     
         TM    BUFFCHST,X'08'                                                   
         BNO   *+8                                                              
         MVI   ZEROASS,C'0'                                                     
         SPACE 1                                                                
EP2      ZAP   0(6,R2),=P'0'       PRE-CLEAR                                    
         LA    R2,6(R2)                                                         
         BCT   R3,EP2                                                           
         SPACE 1                                                                
         L     R1,BUFFASS          ASSIGNED                                     
         CVD   R1,DUB                                                           
         MP    DUB,=P'100'                                                      
         AP    ESTASS1,DUB                                                      
         AP    ESTASS2,DUB                                                      
         AP    ESTDIFF,DUB                                                      
         MVC   ZERASS1,ZEROASS                                                  
         MVC   ZERASS2,ZEROASS                                                  
         MVC   ZERDIFF,ZEROASS                                                  
         SPACE 1                                                                
         L     R1,BUFFACT          ACTUAL                                       
         CVD   R1,DUB                                                           
         MP    DUB,=P'100'                                                      
         AP    ESTACT1,DUB                                                      
         AP    ESTACT2,DUB                                                      
         AP    ESTGROS,DUB                                                      
         AP    ESTTOT1,DUB                                                      
         AP    ESTUNPD,DUB                                                      
         AP    ESTUNBL,DUB                                                      
         SP    ESTDIFF,DUB                                                      
         MVC   ZERACT1,ZEROACT                                                  
         MVC   ZERACT2,ZEROACT                                                  
         MVC   ZERGROS,ZEROACT                                                  
         MVC   ZERTOT1,ZEROACT                                                  
         MVC   ZERUNPD,ZEROACT                                                  
         MVC   ZERUNBL,ZEROACT                                                  
         OC    ZERDIFF,ZEROACT                                                  
         SPACE 1                                                                
         L     R1,BUFFINT          INTEGRATION                                  
         CVD   R1,DUB                                                           
         AP    ESTINTG,DUB                                                      
         AP    ESTTOT1,DUB                                                      
         AP    ESTNETP,DUB                                                      
         AP    ESTACT2,DUB                                                      
         AP    ESTASS2,DUB                                                      
         AP    ESTUNPD,DUB                                                      
         AP    ESTUNBL,DUB                                                      
         SPACE 1                                                                
         M     R0,=F'85'           NET INT                                      
         D     R0,=F'100'                                                       
         CVD   R1,DUB                                                           
         AP    ESTINTN,DUB                                                      
         AP    ESTTOT2,DUB                                                      
         SPACE 1                                                                
         L     R1,BUFFCUT          CUT-IN                                       
         CVD   R1,DUB                                                           
         AP    ESTCUTN,DUB                                                      
         SPACE 1                                                                
         L     R1,BUFFACT          NET                                          
         CVD   R1,DUB                                                           
         MP    DUB,=P'85'                                                       
         AP    ESTNETT,DUB                                                      
         AP    ESTNETP,DUB                                                      
         AP    ESTTOT2,DUB                                                      
         MVC   ZERNETT,ZEROACT                                                  
         MVC   ZERNETP,ZEROACT                                                  
         MVC   ZERTOT2,ZEROACT                                                  
         SPACE 1                                                                
         L     R1,BUFFACT          COMM                                         
         CVD   R1,DUB                                                           
         MP    DUB,=P'15'                                                       
         AP    ESTCOMM,DUB                                                      
         L     R1,BUFFINT                                                       
         M     R0,=F'15'                                                        
         D     R0,=F'100'                                                       
         CVD   R1,DUB                                                           
         AP    ESTCOMM,DUB                                                      
         SPACE 1                                                                
         L     R1,BUFFPAID         PAID                                         
         CVD   R1,DUB                                                           
         MP    DUB,=P'100'                                                      
         L     R1,BUFFIPD                                                       
         CVD   R1,DUB2                                                          
         AP    DUB,DUB2                                                         
         AP    ESTPAID,DUB                                                      
         SP    ESTUNPD,DUB                                                      
         SPACE 1                                                                
         L     R1,BUFFBILL         BILLED                                       
         CVD   R1,DUB                                                           
         MP    DUB,=P'100'                                                      
         L     R1,BUFFIBL                                                       
         CVD   R1,DUB2                                                          
         AP    DUB,DUB2                                                         
         AP    ESTBILL,DUB                                                      
         SP    ESTUNBL,DUB                                                      
         B     FXIT                                                             
         EJECT                                                                  
*              ROUTINE TO CONTROL EVAL/POST                                     
         SPACE 3                                                                
F10      LTR   R3,R3               IF R3=0 SKIP LOOP                            
         BZ    F125                                                             
         XC    ACTDISP,ACTDISP     POST ESTIMATED                               
         BAS   RE,F20              AND EVALUATION                               
         CLC   SPLFLAV(2),=C'P2'                                                
         BNE   F12                                                              
         MVI   ACTDISP+3,64        ALTERNATING POST                             
         BAS   RE,F20                                                           
         SPACE 1                                                                
F12      LA    R2,1(R2)                                                         
         BCT   R3,F10                                                           
F125     CLI   SPLFLAV,C'P'                                                     
         BNE   FEND                                                             
         CLI   SPLFLAV+1,C'2'                                                   
         BE    FEND                                                             
         LA    R2,COLS                                                          
         ZIC   R3,NCOLS                                                         
         LTR   R3,R3                                                            
         BZ    F15                                                              
         MVI   ACTDISP+3,64                                                     
         CLI   SPLFLAV+1,C'A'                                                   
         BE    F13                                                              
         CLI   SPLFLAV+1,C'M'                                                   
         BNE   F14                                                              
         SPACE 1                                                                
F13      LA    R4,P-1                                                           
         A     R4,LFTWIDTH                                                      
         A     R4,COLWIDTH                                                      
         SPACE 1                                                                
F14      BAS   RE,F20              POST - ACTUAL                                
         LA    R2,1(R2)                                                         
         BCT   R3,F14                                                           
F15      BAS   RE,MGCHAT                                                        
         SPACE 1                                                                
FEND     CLC   P2,SPACES                                                        
         BNE   FEND2                                                            
         MVC   P2,P3                                                            
         MVC   P3,P4                                                            
         MVC   P4,SPACES                                                        
         SPACE 1                                                                
FEND2    CLI   BUFFSUB,0                                                        
         BNE   FEND4                                                            
         MVC   SPACING,SPACOPT                                                  
         SPACE 1                                                                
FEND4    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     FXIT                                                             
         EJECT                                                                  
*              CONTROL COLUMN HANDLING ADDRESSES                                
         SPACE 3                                                                
F20      NTR1                                                                   
         LA    R3,ADDTAB                                                        
         SPACE 1                                                                
F22      CLC   0(1,R3),0(R2)       MATCH ON COLUMN NO                           
         BE    F24                                                              
         LA    R3,8(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   F22                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
F24      ZIC   R1,1(R3)            POSSIBLE PARAMETER                           
         SLL   R1,3                                                             
         L     RF,4(R3)            PICK UP BRANCH ADDRESS                       
         A     RF,RELO                                                          
         BR    RF                  OFF WE GO                                    
         SPACE 1                                                                
F26      L     R3,=A(HEADTAB)      COME BACK TO HERE                            
         A     R3,RELO                                                          
         MVI   ZEROFLAG,C' '                                                    
         SPACE 1                                                                
F28      CLC   0(1,R3),0(R2)       FIND COLUMN DISPLACEMENT                     
         BE    *+12                                                             
         LA    R3,32(R3)                                                        
         B     F28                                                              
         ZIC   R1,1(R3)                                                         
         AR    R4,R1                                                            
         XIT1  REGS=(R4)                                                        
         SPACE 1                                                                
ADDTAB   DS    0D                                                               
         DC    AL1(25,0,0,0),A(F30)     UNITS                                   
         DC    AL1(31,0,0,0),A(F32)     HUT                                     
         DC    AL1(32,0,0,0),A(F34)     SHR                                     
         DC    AL1(33,0,0,0),A(F36)     RTG                                     
         DC    AL1(34,0,0,0),A(F38)     COST                                    
         DC    AL1(35,0,0,0),A(F39)     ACOST                                   
         DC    AL1(40,0,0,0),A(F40)     HOMES                                   
         DC    AL1(41,0,0,0),A(F42)     RTG                                     
         DC    AL1(42,1,0,0),A(F40)     D1                                      
         DC    AL1(43,1,0,0),A(F42)     R1                                      
         DC    AL1(44,0,0,0),A(F44)     V1                                      
         DC    AL1(45,2,0,0),A(F40)     D2                                      
         DC    AL1(46,2,0,0),A(F42)     R2                                      
         DC    AL1(47,1,0,0),A(F44)     V2                                      
         DC    AL1(48,3,0,0),A(F40)     D3                                      
         DC    AL1(49,3,0,0),A(F42)     R3                                      
         DC    AL1(50,2,0,0),A(F44)     V3                                      
         DC    AL1(51,0,0,0),A(F46)     IRH                                     
         DC    AL1(52,0,0,0),A(F48)     CPPH                                    
         DC    AL1(53,1,0,0),A(F48)     CPP1                                    
         DC    AL1(54,2,0,0),A(F48)     CPP2                                    
         DC    AL1(55,3,0,0),A(F48)     CPP3                                    
         DC    AL1(56,0,0,0),A(F50)     CPMH                                    
         DC    AL1(57,1,0,0),A(F50)     CPM1                                    
         DC    AL1(58,2,0,0),A(F50)     CPM2                                    
         DC    AL1(59,3,0,0),A(F50)     CPM3                                    
         DC    AL1(60,0,0,0),A(F52)     VPH1                                    
         DC    AL1(61,1,0,0),A(F52)     VPH2                                    
         DC    AL1(62,2,0,0),A(F52)     VPH3                                    
         DC    AL1(63,1,0,0),A(F46)     IR1                                     
         DC    AL1(64,2,0,0),A(F46)     IR2                                     
         DC    AL1(65,3,0,0),A(F46)     IR3                                     
         DC    AL1(00,0,0,0),A(F26)     GAP                                     
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN ROUTINES - SECTION 1                                      
         SPACE 3                                                                
F30      LA    R1,BUFFUNIT         UNITS                                        
         A     R1,ACTDISP                                                       
         L     R1,0(R1)                                                         
         EDIT  (R1),(5,(R4))                                                    
         CLI   ACTDISP+3,0                                                      
         BE    F26                           ON ACTUAL POSTS                    
         CLI   ACTOPT,C'Y'                                                      
         BE    F26                                                              
         CLI   BUFFSUB,0                                                        
         BNE   F26                           AT THE DETAIL LEVEL                
         CLI   SPLFLAV+1,C'A'                                                   
         BNE   F26                           FOR AUDIT FLAVOR                   
         MVC   4(1,R4),BUFFCHRS              SHOW RESULT INSTEAD                
         B     F26                           OF UNITS                           
         SPACE 1                                                                
F32      LA    R1,BUFFHT                     AVE HUT                            
         BAS   RE,AVE                                                           
         B     F26                                                              
         SPACE 1                                                                
F34      LA    R1,BUFFSH                     AVE SHARE                          
         BAS   RE,AVE                                                           
         B     F26                                                              
         SPACE 1                                                                
F36      LA    R1,BUFFRH                     AVE RTG                            
         BAS   RE,AVE                                                           
         B     F26                                                              
         SPACE 1                                                                
F38      LA    R1,BUFFCOST         ACTUAL COST                                  
         TM    BUFFCHST,X'20'         CHECK FOR ZERO                            
         BNO   F39B                                                             
         MVI   ZEROFLAG,C'0'                                                    
         B     F39B                                                             
         SPACE 1                                                                
F39      LA    R1,BUFFASS          OR ASSIGNED                                  
         TM    BUFFCHST,X'08'         CHECK FOR ZERO                            
         BNO   F39B                                                             
         MVI   ZEROFLAG,C'0'                                                    
         SPACE 1                                                                
F39B     A     R1,ACTDISP                                                       
         L     R1,0(R1)                                                         
         EDIT  (R1),(9,(R4))                                                    
         CLI   BUFFSUB,0           AND, IF A DETAIL                             
         BNE   *+10                                                             
         OC    8(1,R4),ZEROFLAG    SHOW ZERO IF REALLY ZERO                     
         CLI   0(R4),C' '                                                       
         BE    F26                                                              
         BCTR  R4,0                                                             
         CLI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         BE    F26                                                              
         MVC   132(9,R4),0(R4)                                                  
         MVC   0(9,R4),SPACES                                                   
         B     F26                                                              
         SPACE 1                                                                
F40      LA    R1,BUFFIH(R1)                 IMPRESSIONS                        
         ST    R1,SAVER1                                                        
         BAS   RE,IMP6                                                          
         CLI   BUFFSUB,7                     ON RECAP TOTALS                    
         BE    F41                                                              
         CLI   ACTOPT,C'Y'         OR ON ACTUAL SUB-TOTALS                      
         BE    F41                                                              
         CLI   BUFFSUB,4           OR ON NETWORK TOTALS IN RECAPS               
         BL    F26                                                              
         CLC   BUFFKEY+4(4),=4X'FF'                                             
         BNE   F26                                                              
         CLI   RECAP,C'B'                                                       
         BE    F41                                                              
         CLI   RECAP,C'C'                                                       
         BE    F41                                                              
         CLI   RECAP,C'G'                                                       
         BE    F41                                                              
         CLI   RECAP,C'E'                                                       
         BE    F41                                                              
         CLI   RECAP,C'N'                                                       
         BNE   F26                                                              
         SPACE 1                                                                
F41      L     R1,SAVER1                                                        
         LA    R4,264(R4)                                                       
         BAS   RE,CPM                        SHOW CPM                           
         SH    R4,=H'264'                                                       
         CLI   ACTDISP+3,0                   AND, ON ACTUAL POST                
         BE    F26                                                              
         L     R1,SAVER1                                                        
         LA    R4,396(R4)                                                       
         BAS   RE,INDEX                      SHOW INDEX AS WELL                 
         SH    R4,=H'396'                                                       
         B     F26                                                              
         SPACE 1                                                                
F42      LA    R1,BUFFRH(R1)                 GRPS                               
         ST    R1,SAVER1                                                        
         BAS   RE,GRP6                                                          
         CLI   BUFFSUB,7                     ON RECAP TOTALS                    
         BE    F43                                                              
         CLI   ACTOPT,C'Y'                                                      
         BE    F43                                                              
         CLI   BUFFSUB,4           OR ON NETWORK TOTALS IN RECAPS               
         BL    F26                                                              
         CLC   BUFFKEY+4(4),=4X'FF'                                             
         BNE   F26                                                              
         CLI   RECAP,C'B'                                                       
         BE    F43                                                              
         CLI   RECAP,C'C'                                                       
         BE    F43                                                              
         CLI   RECAP,C'G'                                                       
         BE    F43                                                              
         CLI   RECAP,C'E'                                                       
         BE    F43                                                              
         CLI   RECAP,C'N'                                                       
         BNE   F26                                                              
         SPACE 1                                                                
F43      L     R1,SAVER1                                                        
         LA    R4,264(R4)                                                       
         BAS   RE,CPP                        SHOW CPP                           
         SH    R4,=H'264'                                                       
         CLI   ACTDISP+3,0                   AND, ON ACTUAL POST                
         BE    F26                                                              
         L     R1,SAVER1                                                        
         LA    R4,396(R4)                                                       
         BAS   RE,INDEX                      SHOW INDEX                         
         SH    R4,=H'396'                                                       
         B     F26                                                              
         EJECT                                                                  
*              COLUMN ROUTINES - SECTION 2                                      
         SPACE 3                                                                
F44      ST    R1,SAVER1                                                        
         BAS   RE,VPH                                                           
         L     R1,SAVER1                     IMPS                               
         LA    R1,BUFFI1(R1)                                                    
         LA    R4,3(R4)                                                         
         BAS   RE,IMP6                                                          
         L     R1,SAVER1                     AND GRPS                           
         LA    R1,BUFFR1(R1)                                                    
         LA    R4,7(R4)                                                         
         BAS   RE,GRP4                                                          
         SH    R4,=H'10'                                                        
         B     F26                                                              
         SPACE 1                                                                
F46      LA    R1,BUFFIH(R1)                   HOMES IMPS                       
         BAS   RE,IMP6                                                          
         ZIC   R1,1(R3)                                                         
         SLL   R1,3                                                             
         LA    R1,BUFFRH(R1)                                                    
         LA    R4,7(R4)                                                         
         BAS   RE,GRP4                                                          
         SH    R4,=H'7'                                                         
         B     F26                                                              
         SPACE 1                                                                
F48      LA    R1,BUFFRH(R1)                 CPP                                
         BAS   RE,CPP                                                           
         B     F26                                                              
         SPACE 1                                                                
F50      LA    R1,BUFFIH(R1)                 CPM                                
         BAS   RE,CPM                                                           
         B     F26                                                              
         SPACE 1                                                                
F52      BAS   RE,VPH              VPH                                          
         B     F26                                                              
         EJECT                                                                  
*              SUBSIDIARY EDITING AIDS                                          
         SPACE 3                                                                
VPH      LA    R1,BUFFI1(R1)       VPH HANDLING                                 
         A     R1,ACTDISP                                                       
         L     R1,0(R1)                      PICK UP IMPS                       
         LTR   R1,R1                                                            
         BZR   RE                                                               
         M     R0,=F'200'                                                       
         LA    RF,BUFFIH                                                        
         A     RF,ACTDISP                                                       
         L     RF,0(RF)                      AND HOMES IMPS                     
         LTR   RF,RF                                                            
         BZR   RE                                                               
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(4,(R4))                                                    
         CLI   1(R4),C' '                                                       
         BNER  RE                                                               
         MVC   1(3,R4),2(R4)                                                    
         BR    RE                                                               
         SPACE 1                                                                
IMP6     A     R1,ACTDISP          IMPRESSIONS TO 6 CHARS                       
         L     R1,0(R1)                                                         
         EDIT  (R1),(7,DMCB)                                                    
         MVC   1(6,R4),DMCB+1                                                   
         CLI   DMCB,C' '                                                        
         BER   RE                                                               
         SH    R4,=H'2'                                                         
         CLC   0(2,R4),SPACES                                                   
         BNE   IMP62                                                            
         EDIT  (R1),(9,(R4))                                                    
         LA    R4,2(R4)                                                         
         BR    RE                                                               
         SPACE 1                                                                
IMP62    MVC   2(7,R4),SPACES                                                   
         EDIT  (R1),(9,132(R4))                                                 
         LA    R4,2(R4)                                                         
         BR    RE                                                               
         SPACE 1                                                                
GRP4     A     R1,ACTDISP          GRPS TO 4 CHARS                              
         L     R1,0(R1)                                                         
         LTR   R1,R1                                                            
         BZR   RE                                                               
         EDIT  (R1),(5,(R4)),1     TRY 1 DEC                                    
         CLI   0(R4),C' '                                                       
         BER   RE                                                               
         LA    R1,5(R1)            TRY 0 DEC                                    
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(5,(R4))                                                    
         CLI   0(R4),C' '                                                       
         BER   RE                                                               
         SH    R4,=H'2'            STILL WONT FIT                               
         CLC   0(2,R4),SPACES                                                   
         BNE   GRP42                                                            
         EDIT  (R1),(7,(R4))                                                    
         LA    R4,2(R4)                                                         
         BR    RE                                                               
         SPACE 1                                                                
GRP42    MVC   2(5,R4),SPACES                                                   
         EDIT  (R1),(7,132(R4))                                                 
         LA    R4,2(R4)                                                         
         BR    RE                                                               
         SPACE 1                                                                
GRP6     A     R1,ACTDISP          GRPS TO 6 CHARS                              
         L     R1,0(R1)                                                         
         LTR   R1,R1                                                            
         BZR   RE                                                               
         EDIT  (R1),(7,(R4)),1     TRY 1 DEC PLACE                              
         CLI   0(R4),C' '                                                       
         BER   RE                                                               
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(7,(R4))                                                    
         BR    RE                                                               
         SPACE 1                                                                
AVE      A     R1,ACTDISP                                                       
         L     R1,0(R1)            AVERAGE HUT/SHR/RTG                          
         LA    RF,BUFFUNIT                                                      
         A     RF,ACTDISP                                                       
         L     RF,0(RF)                                                         
         LTR   RF,RF                                                            
         BZR   RE                                                               
         SR    R0,R0                                                            
         DR    R0,RF                                                            
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(4,(R4))                                                    
         BR    RE                                                               
         SPACE 1                                                                
CPM      A     R1,ACTDISP          COST PER THOUSAND                            
         L     R1,0(R1)                                                         
         M     R0,=F'10'                                                        
         B     CPP2                                                             
         SPACE 1                                                                
CPP      A     R1,ACTDISP          COST PER POINT                               
         L     R1,0(R1)                                                         
         SPACE 1                                                                
CPP2     LTR   RF,R1                                                            
         BZR   RE                                                               
         LA    R1,BUFFCOST         GET COST                                     
         A     R1,ACTDISP                                                       
         L     R1,0(R1)                                                         
         M     R0,=F'2000'                                                      
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                NOW HAVE CPP/CPM IN PENNIES                  
         ST    R4,SAVER4                                                        
         CLC   0(7,R4),SPACES                                                   
         BE    *+12                                                             
         LA    R4,132(R4)                                                       
         B     *-14                                                             
         LR    RF,R1                                                            
         EDIT  (R1),(7,(R4)),2,FLOAT=$                                          
         CLI   0(R4),C' '                                                       
         BE    CPP4                                                             
         LA    R1,50(RF)                                                        
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(7,(R4)),FLOAT=$                                            
         SPACE 1                                                                
CPP4     L     R4,SAVER4                                                        
         BR    RE                                                               
         SPACE 1                                                                
INDEX    LR    RF,R1                                                            
         L     R1,BUFFCOST         EST COST                                     
         M     R0,=F'1000'         X 1000  (FOR ROUNDING)                       
         OC    0(4,RF),0(RF)                                                    
         BZR   RE                                                               
         D     R0,0(RF)            / EST POINTS                                 
         SR    R0,R0                                                            
         M     R0,64(RF)           X ACTS POINTS                                
         OC    BUFFACST,BUFFACST                                                
         BZR   RE                                                               
         D     R0,BUFFACST         / ACT COST                                   
         SR    R0,R0                                                            
         AH    R1,=H'5'            NOW DO ROUNDING                              
         D     R0,=F'10'                                                        
         ST    R4,SAVER4                                                        
         CLC   0(7,R4),SPACES                                                   
         BE    *+12                                                             
         LA    R4,132(R4)                                                       
         B     *-14                                                             
         EDIT  (R1),(7,(R4))                                                    
         L     R4,SAVER4                                                        
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO PUT IN MAKE GOOD INFO                                 
         SPACE 1                                                                
MGCHAT   NTR1                                                                   
         CLI   ACTOPT,C'Y'                                                      
         BE    FXIT                                                             
         L     R4,COLDISP                                                       
         CLC   BUFFLEFT,SPACES                                                  
         BE    MGCHAT4                                                          
         CLC   0(36,R4),SPACES     SEE IF ANY CONFLICT                          
         BNE   MGCHAT2                                                          
         MVC   0(36,R4),BUFFLEFT                                                
         B     MGCHAT4                                                          
         SPACE 1                                                                
MGCHAT2  MVC   132(36,R4),BUFFLEFT YES - POSITION ON NEXT LINE                  
         SPACE 1                                                                
MGCHAT4  A     R4,COLWIDTH                                                      
         CLC   BUFFRT,SPACES                                                    
         BE    FXIT                                                             
         MVC   0(36,R4),BUFFRT                                                  
         B     FXIT                                                             
         SPACE 1                                                                
FXIT     XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINES TO ENSURE THAT TOO MUCH DATA IS NOT REQUESTED           
*              AND TO WORK OUT DISPLACEMENTS FOR PRINTING                       
         SPACE 3                                                                
COMPDISP CSECT                                                                  
         USING *,RF                                                             
COMPDNTR NTR1                                                                   
         LA    RB,COMPDISP                                                      
         DROP  RF                                                               
         USING COMPDISP,RB                                                      
         SPACE 1                                                                
         LA    R2,DETAILS          COMPUTE WIDTH OF DETAILS                     
         L     R3,=A(COLTAB)                                                    
         A     R3,RELO                                                          
         LA    R4,DETWIDTH                                                      
         ZIC   R5,NDETAILS                                                      
         BAS   RE,HOWWIDE                                                       
         SPACE 1                                                                
         LA    R2,RECAP            COMPUTE WIDTH OF RECAP                       
         LA    R4,RECWIDTH                                                      
         ZIC   R5,NRECAP                                                        
         BAS   RE,HOWWIDE                                                       
         SPACE 1                                                                
         MVC   LFTWIDTH,DETWIDTH   SELECT THE GREATER                           
         CLC   RECWIDTH,DETWIDTH                                                
         BL    *+10                                                             
         MVC   LFTWIDTH,RECWIDTH                                                
         SPACE 1                                                                
         L     R1,LFTWIDTH         COMPUTE INSET FOR DETAILS                    
         S     R1,DETWIDTH                                                      
         ST    R1,DETINSET                                                      
         L     R1,LFTWIDTH                       AND RECAP                      
         S     R1,RECWIDTH                                                      
         ST    R1,RECINSET                                                      
         CLI   BOXOPT,C'Y'         ADJUST IF ANY BOXES ARE PRESENT              
         BNE   CD2                                                              
         LA    R1,1(R1)                                                         
         ST    R1,RECINSET                                                      
         L     R1,DETINSET                                                      
         LA    R1,1(R1)                                                         
         ST    R1,DETINSET                                                      
         L     R1,LFTWIDTH                                                      
         LA    R1,1(R1)                                                         
         ST    R1,LFTWIDTH                                                      
         MVI   MAXLINES,59                                                      
         SPACE 1                                                                
CD2      LA    R2,COLS             COMPUTE WIDTH OF COLUMNS                     
         L     R3,=A(HEADTAB)                                                   
         A     R3,RELO                                                          
         LA    R4,COLWIDTH                                                      
         ZIC   R5,NCOLS                                                         
         BAS   RE,HOWWIDE                                                       
         SPACE 1                                                                
         L     R1,COLWIDTH                                                      
         CLI   SPLFLAV,C'P'                                                     
         BNE   CD6                                                              
         CLI   SPLFLAV+1,C'A'      POST AUDIT NEEDS MINIMUM OF 36               
         BE    CD3                                                              
         CLI   SPLFLAV+1,C'M'                                                   
         BNE   CD4                                                              
         SPACE 1                                                                
CD3      CH    R1,=H'36'                                                        
         BH    CD4                                                              
         LA    R1,36                                                            
         ST    R1,COLWIDTH                                                      
         SPACE 1                                                                
CD4      SLL   R1,1                POSTS HAVE TWO CHUNKS                        
         SPACE 1                                                                
CD6      A     R1,LFTWIDTH                                                      
         CH    R1,=H'132'          ALLOW UP TO 132 CHARACTERS                   
         BNH   CDXIT                                                            
         ZIC   R1,NCOLS            TRIM DOWN THE DATA COLUMNS                   
         LA    R2,COLS-1(R1)                                                    
         MVI   0(R2),0                                                          
         BCTR  R1,0                                                             
         STC   R1,NCOLS                                                         
         LTR   R1,R1                                                            
         BNZ   CD2                 GO BACK AND SEE IF IT WILL FIT               
         DC    H'0'                                                             
         SPACE 1                                                                
CDXIT    XIT1                                                                   
         EJECT                                                                  
*              FIGURE OUT WIDTH OF AN EXPRESSION                                
         SPACE 3                                                                
HOWWIDE  NTR1                                                                   
         XC    0(4,R4),0(R4)                                                    
         CLI   0(R2),C'-'                                                       
         BE    XIT                                                              
         LTR   R5,R5               IF NO LENGTH THEN EXIT                       
         BZ    XIT                                                              
*                                                                               
HW2      BAS   RE,HW4                                                           
         LA    R2,1(R2)                                                         
         BCT   R5,HW2                                                           
         B     CDXIT                                                            
         SPACE 1                                                                
HW4      NTR1                                                                   
         SPACE 1                                                                
HW6      CLC   0(1,R3),0(R2)       MATCH AGAINST TABLE                          
         BE    HW8                                                              
         LA    R3,32(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   HW6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
HW8      ZIC   R1,1(R3)            WIDTH OF THIS ITEM                           
         A     R1,0(R4)                                                         
         ST    R1,0(R4)                                                         
         B     CDXIT                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SHOW MAKE-GOOD/MISSED DETAILS                         
         SPACE 3                                                                
MAKEGOOD CSECT                                                                  
         USING *,RF                                                             
MAKEGNTR NTR1                                                                   
         LA    RB,MAKEGOOD                                                      
         DROP  RF                                                               
         USING MAKEGOOD,RB                                                      
         MVC   BUFFLEFT(72),SPACES                                              
         SPACE 1                                                                
         TM    NBUNITST,X'40'                                                   
         BNO   MKB                                                              
         TM    NBUNITST,X'02'                                                   
         BO    MKB                                                              
         MVC   BUFFRT(12),=C'(PRE-EMPTED)'                                      
         B     MGXIT                                                            
         SPACE 2                                                                
MKB      MVC   WORK,SPACES                                                      
         CLI   NBUNITST,1                                                       
         BL    MGXIT                                                            
         TM    NBUNITST,X'01'      MAKE GOOD                                    
         BNO   MKD                                                              
         MVC   WORK(8),=C'(M/G FOR'                                             
         MVC   WORK+9(16),NBMGFPNM                                              
         MVC   WORK+26(09),=C'ON MMMDD)'                                        
         GOTO1 DATCON,DMCB,(2,NBMGFDAT),(4,WORK+29)                             
         GOTO1 SQUASHER,DMCB,WORK,36                                            
         MVC   BUFFLEFT,WORK                                                    
         SPACE 2                                                                
MKD      TM    NBUNITST,X'02'      MISSED                                       
         BNO   MGXIT                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(7),=C'(M/G BY'                                              
         MVC   WORK+9(16),NBMGBPNM                                              
         MVC   WORK+26(09),=C'ON MMMDD)'                                        
         GOTO1 DATCON,DMCB,(2,NBMGBDAT),(4,WORK+29)                             
         GOTO1 SQUASHER,DMCB,WORK,36                                            
         MVC   BUFFRT,WORK                                                      
         SPACE 1                                                                
MGXIT    XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO HANDLE ACTUAL SUBTOTALS                               
         SPACE 3                                                                
ANYACT   CSECT                                                                  
         USING *,RF                                                             
ANYANTR  NTR1                                                                   
         LA    RB,ANYACT                                                        
         DROP  RF                                                               
         USING ANYACT,RB                                                        
         SPACE 1                                                                
         CLI   SPLFLAV,C'P'                                                     
         BNE   ANYXIT                                                           
         CLI   BUFFCHRS,C'E'       IF ITS NOT ESTIMATED                         
         BE    ANYACT4                                                          
         CLI   BUFFCHRS,C'L'       OR LATEST ESTIMATED                          
         BE    ANYACT4                                                          
         LA    R2,BUFFUNIT         ADD INTO ACTUAL SUBS                         
         LA    R3,ACTSUBS                                                       
         LA    R4,33                                                            
         SPACE 1                                                                
ANYACT2  L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,ANYACT2                                                       
         B     ANYXIT                                                           
         SPACE 1                                                                
ANYACT4  MVC   WORK(1),LASTRES     IF IT IS ESTIMATED, WAS THE LAST             
         MVC   LASTRES,BUFFCHRS                        ACTUAL                   
         CLI   WORK,0                                                           
         BE    ANYXIT                                                           
         CLI   WORK,C'E'                                                        
         BE    ANYXIT                                                           
         CLI   WORK,C'L'                                                        
         BE    ANYXIT                                                           
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)     YES SO ITS TIME FOR ACTUAL SUBTOTS           
         L     R1,SUBDISP                                                       
         MVC   0(13,R1),=C'ACTUAL TOTALS'                                       
         MVC   BUFFSAVE,BUFFUNIT                                                
         MVC   BUFFUNIT(132),ACTSUBS                                            
         MVI   ACTOPT,C'Y'                                                      
         MVI   SPACING,2                                                        
         L     RF,=A(FORMC)                                                     
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         MVI   ACTOPT,0                                                         
         MVC   BUFFUNIT(132),BUFFSAVE                                           
         XC    ACTSUBS,ACTSUBS                                                  
         SPACE 1                                                                
ANYXIT   XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     CSECT                                                                  
         ENTRY HEADTAB                                                          
         ENTRY COLTAB                                                           
         USING *,RF                                                             
HOOKNTR  NTR1                                                                   
         LA    RB,HOOK                                                          
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         DROP  RF                                                               
         USING HOOK,RB,R9                                                       
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(5),SPLPRO                                                  
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+16(20),SPLCLIN                                                
         MVC   H5+16(20),SPLPRON                                                
         CLC   SPLPRO(3),=C'POL'   TEST FOR POOL                                
         BNE   *+10                                                             
         MVC   H5+16(20),SPACES    YES-CLEAR PRODUCT NAME                       
         MVC   H6+16(24),SPLESTN                                                
*                                                                               
         LA    R1,H7               CURCOMM IS NEXT AVAILABLE PRINT LINE         
         ST    R1,CURCOMM                                                       
         OC    ACCPCTG,ACCPCTG      DONT PRINT PCT IF 0 OR 100.00               
         BZ    HOOK1                                                            
         CLC   ACCPCTG,=F'10000'                                                
         BE    HOOK1                                                            
         MVC   H7(7),=C'PERCENT'                                                
         EDIT  ACCPCTG,(6,H7+10),2                                              
         LA    R1,H8                                                            
         ST    R1,CURCOMM                                                       
         SPACE 1                                                                
HOOK1    DS    0H                                                               
         BAS   RE,HOOKALL                                                       
         BAS   RE,BILDTITL                                                      
         BAS   RE,HOOKEST                                                       
         BAS   RE,HLSUBS                                                        
         BAS   RE,COMENT                                                        
         CLI   BOXOPT,C'Y'         SET UP BOXES IF REQUESTED                    
         BNE   HOOK2                                                            
         CLI   SPLFLAV,C'P'                                                     
         BE    *+10                                                             
         MVC   MYROWS+11(2),=C'M ' ONLY POST USES H12                           
         L     R1,ABOX                                                          
         MVC   0(200,R1),MYBOX                                                  
         MVC   200(200,R1),MYBOX+200                                            
         SPACE 1                                                                
HOOK2    MVC   H6+76(13),=C'DAYPART - ALL'                                      
         CLI   SPLDPTH+5,0                                                      
         BE    *+10                                                             
         MVC   H6+86(8),SPLDPTN                                                 
         CLI   SPLPAKH+5,0                                                      
         BE    SHOOK2                                                           
         CLI   NBSELEST,0                                                       
         BE    SHOOK2                                                           
         CLI   NBSELESE,0                                                       
         BNE   SHOOK2                                                           
         MVC   H6+76(34),SPLPAKN                                                
         SPACE 1                                                                
SHOOK2   CLI   SPLFLAV,C'E'                                                     
         BNE   HXIT                                                             
         IC    R1,SPLFILT                                                       
         SLL   R1,28                                                            
         SRL   R1,28               0-5                                          
         MH    R1,=H'12'                                                        
         LA    R1,FILTTITL(R1)                                                  
         MVC   H6+49(12),0(R1)                                                  
         B     HXIT                                                             
         SPACE 2                                                                
FILTTITL DC    CL12'(ORDERED) '                                                 
         DC    CL12'(CLEARED) '                                                 
         DC    CL12'UNCLEARED '                                                 
         DC    CL12' (BILLED) '                                                 
         DC    CL12' BILLABLE '                                                 
         DC    CL12'UNALLOCATED'                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              GET THE COMMENTS                                                 
         SPACE 3                                                                
COMENT   NTR1                                                                   
         LA    R4,COMMAREA                                                      
         USING NCOMBLKD,R4                                                      
         MVI   NCBTOP,C'Y'         TOP OF PAGE COMMENTS                         
         MVI   NCBKDFL,C'Y'        DEFAULT TO NEXT KEY                          
         MVI   NCBMULT,C'Y'        PASS MULTIPLE COMMENTSA                      
         MVC   NCBAIO,NBAIO                                                     
         LA    R2,NETBLOCK                                                      
         ST    R2,NCBNETB                                                       
         LA    R2,COMHOOK                                                       
         ST    R2,NCBAHOOK                                                      
         SPACE 1                                                                
         MVC   NCBAM,NBACTAM                                                    
         MVC   NCBID(2),SPLCOM        COMMENT FROM SCREEN                       
         CLI   SPLCOM+2,0             CK IF TOP COMMENTS REQUESTED              
         BE    COM1                                                             
         CLI   SPLCOM+2,C'*'                                                    
         BE    COM1                                                             
         CLI   SPLCOM+2,C'1'                                                    
         BNE   COMXIT                                                           
         SPACE 1                                                                
COM1     MVI   NCBID+2,C'1'        TOP COMMENTS                                 
         MVC   NCBCLT,NBACTCLI                                                  
         MVC   NCBPRD,BUFFPNUM                                                  
         MVC   NCBEST,BUFFCHES     CURRENT ESTIMATE                             
         MVC   NCBNTWK,BUFFNET                                                  
         CLI   BUFFNET,0           FOR ALL NETWORKS                             
         BNE   COM2                                                             
         MVC   NCBNTWK,NBSELNET                                                 
         SPACE 1                                                                
COM2     GOTO1 ANETCOM,DMCB,NCOMBLKD                                            
         MVI   NBFUNCT,NBFRESTR    RESTORE UNIT ON NEXT NETIO                   
         SPACE 1                                                                
COMXIT   XIT1                                                                   
         SPACE 1                                                                
COMHOOK  NTR1                                                                   
         L     R3,CURCOMM                                                       
         LA    R1,H11                                                           
         CR    R1,R3                                                            
         BE    HOOKX               NO MORE ROOM                                 
         SPACE                                                                  
         L     R2,NBAIO                                                         
         USING NCOMRECD,R2                                                      
         LA    R2,NCOMELEM         GET FIRST 02 ELEMENT                         
         USING NCOMELEM,R2                                                      
         CLI   NCOMELEM,X'02'                                                   
         BNE   HOOKX                                                            
         SPACE 1                                                                
HOOK5    ZIC   R1,NCOMELEN                                                      
         SH    R1,=H'4'                                                         
         CH    R1,=H'35'                                                        
         BL    HOOK6               MAXIMUM COMM LENGTH = 35                     
         LA    R1,35                                                            
         SPACE 1                                                                
HOOK6    EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),NCOMETXT                                                 
         LA    R3,132(R3)                                                       
         ST    R3,CURCOMM                                                       
         LA    R1,H11                                                           
         CR    R1,R3                                                            
         BE    HOOKX               NO MORE ROOM                                 
         ZIC   R1,NCOMELEN                                                      
         LA    R2,0(R1,R2)         NEXT ELEMENT                                 
         CLI   NCOMELEM,X'02'                                                   
         BE    HOOK5                                                            
         SPACE 1                                                                
HOOKX    XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO BUILD TITLE                                           
         SPACE 3                                                                
BILDTITL NTR1                                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(8),=C'POST-BUY'                                             
         CLI   SPLFLAV,C'P'                                                     
         BE    BT2                                                              
         MVC   WORK(8),=C'ESTIMATE'                                             
         CLI   SPLFLAV,C'E'                                                     
         BE    BT2                                                              
         MVC   WORK(10),=C'EVALUATION'                                          
         SPACE 2                                                                
BT2      MVC   WORK+12(6),=C'REPORT'                                            
         CLI   BUFFTYPE,1                                                       
         BE    *+10                                                             
         MVC   WORK+12(6),=C'RECAP '                                            
         GOTO1 SQUASHER,DMCB,WORK,36                                            
         GOTO1 CENTER,DMCB,WORK,30                                              
         MVC   H1+38(30),WORK                                                   
         GOTO1 UNDERLIN,DMCB,(30,H1+38),H2+38                                   
         B     HXIT                                                             
         SPACE 3                                                                
*              HEADLINE ROUTINES - ESTIMATE                                     
         SPACE 3                                                                
HOOKEST  NTR1                                                                   
         CLC   SPLEST(3),=C'NO,'                                                
         BNE   HKE2                                                             
         MVC   H6+10(5),SPLEST+3                                                
         MVC   H6+16(24),=CL24'ALL FILTERED'                                    
         B     HXIT                                                             
         SPACE 1                                                                
HKE2     CLI   NBSELESE,0                                                       
         BE    HXIT                                                             
         MVC   WORK(29),=C'ESTIMATES 101 TO 102 COMBINED'                       
         EDIT  (1,NBSELEST),(3,WORK+10),WRK=DMCB                                
         EDIT  (1,NBSELESE),(3,WORK+17),WRK=DMCB                                
         GOTO1 SQUASHER,DMCB,WORK,29                                            
         MVC   H6(40),SPACES                                                    
         MVC   H6(29),WORK                                                      
         B     HXIT                                                             
         SPACE 2                                                                
HXIT     XIT1                                                                   
         EJECT                                                                  
*              ROUTINES TO HANDLE ALL CONDITIONS IN HEADLINES                   
         SPACE 3                                                                
HOOKALL  NTR1                                                                   
         CLI   SPLNETH+5,0                                                      
         BE    HA2                                                              
         MVC   H5+76(9),=C'NETWORK -'                                           
         CLC   SPLNET(3),=C'ALL'                                                
         BE    HA1                                                              
         MVC   H5+86(4),SPLNET                                                  
         B     HA2                                                              
         SPACE 1                                                                
HA1      MVC   H5+86(4),BUFFNET                                                 
         CLI   BUFFNET,X'FF'                                                    
         BE    HA2                                                              
         MVC   H5+86(4),=C'ALL '                                                
         SPACE 1                                                                
HA2      CLI   BUFFEST,0                                                        
         BE    HA4                                                              
         CLI   BUFFEST,X'FF'                                                    
         BE    HA4                                                              
         MVC   H6+10(3),BUFFEST                                                 
         MVC   H6+16(24),SPACES                                                 
         SPACE 1                                                                
HA4      CLI   BUFFPRD,0                                                        
         BE    HA6                                                              
         CLI   BUFFPRD,X'FF'                                                    
         BE    HA6                                                              
         MVC   H5+10(3),BUFFPRD                                                 
         BAS   RE,HOOKPRD                                                       
         MVC   H5+16(20),SAVEPRDN                                               
         SPACE 1                                                                
HA6      CLI   BUFFCLI,0                                                        
         BE    HA8                                                              
         CLI   BUFFCLI,X'FF'                                                    
         BE    HA8                                                              
         MVC   H4+10(3),BUFFCLI                                                 
         BAS   RE,HOOKCLI                                                       
         MVC   H4+16(20),SAVECLIN                                               
         SPACE 1                                                                
HA8      CLI   BUFFSUB,4           SHOW SINGLE GROUP ON RECAPS                  
         BL    HA10                                                             
         OC    SINGROUP,SINGROUP                                                
         BZ    HXIT                                                             
         MVC   HEADGRP,SINGROUP                                                 
         B     HA12                                                             
         SPACE 1                                                                
HA10     CLI   BUFFGRP,0                                                        
         BE    HXIT                                                             
         CLI   BUFFGRP,X'FF'                                                    
         BE    HXIT                                                             
         MVC   HEADGRP,BUFFGRP                                                  
         SPACE 1                                                                
HA12     MVC   H5(38),SPACES                                                    
         MVC   H5(5),=C'GROUP'                                                  
         MVC   H5+10(1),SPLPRO                                                  
         UNPK  WORK(5),HEADGRP+1(3)                                             
         ZIC   R1,BRLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   H5+11(0),WORK                                                    
         BAS   RE,HOOKGRP                                                       
         MVC   H5+16(24),SAVEGRPN                                               
         B     HXIT                                                             
         EJECT                                                                  
*              ROUTINES TO LOOK UP CLI/PRD/GROUP NAMES                          
         SPACE 3                                                                
HOOKCLI  NTR1                                                                   
         CLC   BUFFCLI,SAVECLI                                                  
         BE    HXIT                                                             
         MVC   SAVECLI,BUFFCLI                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         USING CLTHDR,R4                                                        
         MVC   CKEYAM,NBACTAM                                                   
         GOTO1 NBCLPACK,DMCB,BUFFCLI,CKEYCLT                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 READ                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         MVC   SAVECLIN,CNAME                                                   
         B     HXIT                                                             
         SPACE 2                                                                
HOOKPRD  NTR1                                                                   
         CLC   BUFFPRD,SAVEPRD                                                  
         BE    HXIT                                                             
         MVC   SAVEPRD,BUFFPRD                                                  
         CLC   SAVEPRD,=C'UNA'                                                  
         BNE   HOOKPRDB                                                         
         MVC   SAVEPRDN,=CL20'UNALLOCATED'                                      
         B     HXIT                                                             
         SPACE 1                                                                
HOOKPRDB XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRDHDR,R4                                                        
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         MVC   PKEYAM(3),NBACTAM                                                
         CLI   BUFFCLI,0           IF ONLY ONE CLIENT                           
         BNE   HOOKPRD1                                                         
         MVC   PKEYCLT,NBACTCLI    THEN USE CURRENT CLIENT                      
         MVC   PKEYPRD,BUFFPRD                                                  
         B     HOOKPRD2                                                         
         SPACE 1                                                                
HOOKPRD1 GOTO1 NBCLPACK,DMCB,BUFFCLI,PKEYCLT                                    
         MVC   PKEYPRD,BUFFPRD                                                  
         SPACE 1                                                                
HOOKPRD2 MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    HOOKPRD4                                                         
         MVC   SAVEPRDN,SPACES                                                  
         SPACE 1                                                                
HOOKPRD4 MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         MVC   SAVEPRDN,PNAME                                                   
         B     HXIT                                                             
         SPACE 1                                                                
HOOKGRP  NTR1                                                                   
         CLC   HEADGRP,SAVEGRP                                                  
         BE    HXIT                                                             
         MVC   SAVEGRP,HEADGRP                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD(3),NBACTAM                                              
         MVC   PRGKID(3),HEADGRP                                                
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         LA    R2,PRGEL                                                         
         SPACE 1                                                                
HOOKGRP2 CLI   0(R2),X'10'                                                      
         BE    HOOKGRP4                                                         
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     HOOKGRP2                                                         
         SPACE 1                                                                
         USING PRGEL10,R2                                                       
HOOKGRP4 MVC   SAVEGRPN,PRGNAM1                                                 
         CLC   PRGNAM2,SPACES                                                   
         BNH   HXIT                                                             
         MVC   SAVEGRPN,PRGNAM2                                                 
         B     HXIT                                                             
         EJECT                                                                  
*              ROUTINES TO CONTROL H10-H12 FILLING                              
         SPACE 3                                                                
HLSUBS   NTR1                                                                   
         LA    R2,DETAILS                                                       
         ZIC   R3,NDETAILS                                                      
         LA    R4,H10-1                                                         
         A     R4,DETINSET                                                      
         MVC   MYCOLS(132),SPACES                                               
         MVC   MYCOLS+132(132),SPACES                                           
         LA    R6,MYCOLS-1                                                      
         A     R6,DETINSET                                                      
         MVI   0(R6),C'L'                                                       
         MVI   COLCHAR,C'C'                                                     
         ST    R6,THISCOL                                                       
         CLI   BUFFTYPE,1                                                       
         BE    HL4                                                              
*        CLC   P,SPACES                                                         
*        BE    *+10                                                             
*        OC    P(40),LASTP                                                      
         LA    R2,RECAP                                                         
         ZIC   R3,NRECAP                                                        
         LA    R4,H10-1                                                         
         A     R4,RECINSET                                                      
         MVI   0(R6),C' '                                                       
         LA    R6,MYCOLS-1                                                      
         A     R6,RECINSET                                                      
         MVI   0(R6),C'L'                                                       
         ST    R6,THISCOL                                                       
         SPACE 1                                                                
HL4      BAS   RE,HLCOL                                                         
         LA    R2,COLS                                                          
         ZIC   R3,NCOLS                                                         
         CLI   SPLFLAV,C'P'                                                     
         BE    HL6                                                              
         BAS   RE,HLCOL                                                         
         B     HL12                                                             
         SPACE 1                                                                
HL6      CLI   SPLFLAV+1,C'2' REGULAR POST                                      
         BE    HL8                                                              
         LR    R5,R4                                                            
         LA    R4,132(R4)                                                       
         MVI   COLCHAR,C' '                                                     
         BAS   RE,HLCOL                                                         
         L     R6,THISCOL                                                       
         MVI   0(R6),C'C'                                                       
         MVC   WORK(8),=C'ESTIMATE'                                             
         BAS   RE,HLOVER                                                        
         SPACE 1                                                                
         LA    R4,H11-1                                                         
         A     R4,LFTWIDTH                                                      
         A     R4,COLWIDTH                                                      
         LR    R5,R4                                                            
         SH    R5,=H'132'                                                       
         BAS   RE,HLCOL                                                         
         MVC   WORK(8),=C'-ACTUAL-'                                             
         BAS   RE,HLOVER                                                        
         B     HL12                                                             
         SPACE 1                                                                
HL8      LR    R5,R4          ALTERNATING POST                                  
         LR    R0,R3          SAVE N'COLS                                       
         LA    R4,132(R4)                                                       
         LTR   R0,R0                                                            
         BZ    HL12                                                             
         SPACE 1                                                                
HL10     LA    R3,1                ESTIMATE                                     
         MVI   COLCHAR,C' '                                                     
         BAS   RE,HLCOL                                                         
         MVC   WORK(8),=C'ESTIMATE'                                             
         BAS   RE,HLOVER                                                        
         LR    R5,R4                                                            
         SH    R5,=H'132'                                                       
         SPACE 1                                                                
         BAS   RE,HLCOL       THEN ACTUAL                                       
         L     R6,THISCOL                                                       
         MVI   0(R6),C'C'                                                       
         MVC   WORK(8),=C'-ACTUAL-'                                             
         BAS   RE,HLOVER                                                        
         LR    R5,R4                                                            
         SH    R5,=H'132'                                                       
         LA    R2,1(R2)                                                         
         BCT   R0,HL10                                                          
         SPACE 1                                                                
HL12     L     R6,THISCOL                                                       
         MVI   0(R6),C'R'                                                       
         B     HXIT                                                             
         EJECT                                                                  
*              ROUTINE TO HANDLE A LIST OF HEADLINE COLUMNS                     
         SPACE 3                                                                
HLCOL    NTR1                                                                   
         LTR   R3,R3               IF NO COLUMNS                                
         BZ    HXIT4                                                            
HC2      BAS   RE,HC4                                                           
         LA    R2,1(R2)                                                         
         BCT   R3,HC2                                                           
         B     HXIT4                                                            
         SPACE 1                                                                
HC4      NTR1                                                                   
         LA    R5,HEADTAB                                                       
         SPACE 1                                                                
HC6      CLC   0(1,R5),0(R2)  FIND COLUMNS ENTRY                                
         BE    HC8                                                              
         LA    R5,32(R5)                                                        
         CLI   0(R5),X'FF'                                                      
         BNE   HC6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
HC8      ZIC   R1,1(R5)                                                         
         L     R6,THISCOL                                                       
         AR    R6,R1                                                            
         MVC   0(1,R6),COLCHAR                                                  
         ST    R6,THISCOL                                                       
         CLI   0(R5),C'P'                                                       
         BNE   *+8                                                              
         SH    R1,=H'2'                                                         
         SH    R1,=H'2'                                                         
         CLI   BUFFTYPE,2                                                       
         BNE   HC8B                                                             
         CLI   0(R5),11            SKIP LIST ON RECAPS                          
         BE    HC9                                                              
         SPACE 1                                                                
HC8B     EX    R1,HC10                                                          
         EX    R1,HC12                                                          
         BAS   RE,DEMSCAN                                                       
         SPACE 1                                                                
HC9      LA    R4,2(R1,R4)                                                      
         CLI   0(R5),C'P'                                                       
         BNE   *+8                                                              
         LA    R4,2(R4)                                                         
         SPACE 1                                                                
HXIT4    XIT1  REGS=(R4)                                                        
         SPACE 1                                                                
HC10     MVC   1(0,R4),3(R5)                                                    
HC12     MVC   133(0,R4),18(R5)                                                 
         EJECT                                                                  
*              HEADLINE TABLE                                                   
         SPACE 3                                                                
HEADTAB  DS    0CL32                                                            
         DC    AL1(00,04),C'                              '                     
         DC    AL1(01,12),C'    ASSIGNED       --------   '                     
         DC    AL1(02,12),C'    ASSIGNED       --------   '                     
         DC    AL1(03,12),C'      ACTUAL         ------   '                     
         DC    AL1(04,12),C'      ACTUAL         ------   '                     
         DC    AL1(05,12),C'       GROSS          -----   '                     
         DC    AL1(06,12),C'  DIFFERENCE     ----------   '                     
         DC    AL1(07,12),C' INTEGRATION    -----------   '                     
         DC    AL1(08,12),C'      CUT-IN         ------   '                     
         DC    AL1(09,12),C'       TOTAL          GROSS   '                     
         DC    AL1(10,12),C'       TOTAL          (NET)   '                     
         DC    AL1(11,08),C' EST-PAK        -------       '                     
         DC    AL1(12,12),C'  COMMISSION     ----------   '                     
         DC    AL1(13,12),C'         NET            ---   '                     
         DC    AL1(14,12),C'   NET TOTAL      ---------   '                     
         DC    AL1(15,12),C'     CLEARED        -------   '                     
         DC    AL1(16,12),C'   UNCLEARED      ---------   '                     
         DC    AL1(17,12),C'      BILLED         ------   '                     
         DC    AL1(18,12),C'    UNBILLED       --------   '                     
         DC    AL1(19,12),C' INTEGRATION       (NET)      '                     
         DC    AL1(20,12),C'       TOTAL          -----   '                     
         DC    AL1(25,06),C' UNITS          -----         '                     
         DC    AL1(31,04),C' AVE            HUT           '                     
         DC    AL1(32,04),C' SHR            ---           '                     
         DC    AL1(33,04),C' AVE            RTG           '                     
         DC    AL1(34,09),C'     COST           ----      '                     
         DC    AL1(35,09),C' ASSIGNED         COST        '                     
         DC    AL1(40,07),C'  HOMES          (000)        '                     
         DC    AL1(41,07),C'  HOMES          (GRP)        '                     
         DC    AL1(42,07),C' DEMO.1          (000)        '                     
         DC    AL1(43,07),C' DEMO.1          (GRP)        '                     
         DC    AL1(44,15),C' ----DEMO.1---- VPH  IMPS GRPS'                     
         DC    AL1(45,07),C' DEMO.2          (000)        '                     
         DC    AL1(46,07),C' DEMO.2          (GRP)        '                     
         DC    AL1(47,15),C' ----DEMO.2---- VPH  IMPS GRPS'                     
         DC    AL1(48,07),C' DEMO.3          (000)        '                     
         DC    AL1(49,07),C' DEMO.3          (GRP)        '                     
         DC    AL1(50,15),C' ----DEMO.3---- VPH  IMPS GRPS'                     
         DC    AL1(51,12),C'   --HOMES--      IMPS GRPS   '                     
         DC    AL1(52,07),C'  HOMES          (CPP)        '                     
         DC    AL1(53,07),C' DEMO.1          (CPP)        '                     
         DC    AL1(54,07),C' DEMO.2          (CPP)        '                     
         DC    AL1(55,07),C' DEMO.3          (CPP)        '                     
         DC    AL1(56,07),C'  HOMES          (CPM)        '                     
         DC    AL1(57,07),C' DEMO.1          (CPM)        '                     
         DC    AL1(58,07),C' DEMO.2          (CPM)        '                     
         DC    AL1(59,07),C' DEMO.3          (CPM)        '                     
         DC    AL1(60,07),C' DEMO.1          (VPH)        '                     
         DC    AL1(61,07),C' DEMO.2          (VPH)        '                     
         DC    AL1(62,07),C' DEMO.3          (VPH)        '                     
         DC    AL1(63,12),C'   -DEMO.1--      IMPS GRPS   '                     
         DC    AL1(64,12),C'   -DEMO.2--      IMPS GRPS   '                     
         DC    AL1(65,12),C'   -DEMO.3--      IMPS GRPS   '                     
         EJECT                                                                  
COLTAB   DC    C'B',AL1(08),C' BRAND          -----         '                   
         DC    C'G',AL1(06),C' GROUP          -----         '                   
         DC    C'C',AL1(07),C' CLIENT         ------        '                   
         DC    C'T',AL1(12),C' TIME           ----          '                   
         DC    C'L',AL1(04),C' LEN            ---           '                   
         DC    C'N',AL1(05),C' NET            ---           '                   
         DC    C'P',AL1(17),C' PROGRAM        -------       '                   
         DC    C'D',AL1(06),C' DATE           ----          '                   
         DC    C'E',AL1(04),C' EST            ---           '                   
         DC    C'Y',AL1(04),C' DAY            ---           '                   
         DC    C'W',AL1(06),C' WEEK           ----          '                   
         DC    C'M',AL1(06),C' MONTH          -----         '                   
         DC    C'4',AL1(12),C' PERIOD         ------        '                   
         DC    C'Q',AL1(12),C' QUARTER        -------       '                   
         DC    C'S',AL1(08),C' DAYPART        -------       '                   
         DC    X'FF'                                                            
         SPACE 1                                                                
MYBOX    DC    C'Y'                                                             
         DC    AL1(1)                                                           
         DC    6X'00'                                                           
MYCOLS   DC    264C' '                                                          
MYROWS   DC    C'        T   M                 '                                
         DC    C'                            B '                                
         DC    40C' '                                                           
         DC    28X'00'                                                          
         EJECT                                                                  
*              ROUTINE TO PUT ESTIMATED/ACTUAL INTO H10                         
         SPACE 3                                                                
HLOVER   NTR1                                                                   
         LA    R3,132(R5)          FIRST FILL WITH DASHES                       
         LR    R2,R4                                                            
         SR    R2,R3               R2=LENGTH                                    
         GOTO1 UNDERLIN,DMCB,((R2),(R3)),(R5)                                   
         SPACE 1                                                                
HOV2     CLI   0(R5),C'-'          FIND FIRST DASH                              
         BE    HOV4                                                             
         LA    R5,1(R5)                                                         
         B     HOV2                                                             
         SPACE 1                                                                
HOV4     LR    R4,R5               FIND LAST                                    
         SPACE 1                                                                
HOV6     CLI   0(R4),C' '                                                       
         BE    HOV8                                                             
         LA    R4,1(R4)                                                         
         B     HOV6                                                             
         SPACE 1                                                                
HOV8     SR    R4,R5               H'DASHES = R4                                
         CH    R4,=H'8'                                                         
         BH    HOV10                                                            
*                                  ONLY ROOM FOR EST/ACT                        
         SH    R4,=H'3'                                                         
         SRL   R4,1                                                             
         AR    R5,R4                                                            
         MVC   0(3,R5),=C'EST'                                                  
         CLI   WORK,C'E'                                                        
         BE    HXIT                                                             
         MVC   0(3,R5),=C'ACT'                                                  
         B     HXIT                                                             
         SPACE 1                                                                
HOV10    SH    R4,=H'8'            ROOM FOR ESTIMATED/-ACTUAL-/                 
         SRL   R4,1                                                             
         AR    R5,R4                                                            
         MVC   0(8,R5),WORK                                                     
         B     HXIT                                                             
         EJECT                                                                  
*              ROUTINE TO LOOK FOR DEMOS IN HEADLINES                           
         SPACE 3                                                                
DEMSCAN  NTR1                                                                   
         SPACE 1                                                                
DS2      CLC   0(5,R4),=C'DEMO.'                                                
         BE    DS4                                                              
         LA    R4,1(R4)                                                         
         BCT   R1,DS2                                                           
         B     HXIT                                                             
         SPACE 2                                                                
DS4      ZIC   R2,5(R4)            1-3                                          
         SLL   R2,28                                                            
         SRL   R2,28                                                            
         BCTR  R2,0                0-2 BINARY                                   
         L     R6,ASYSD                                                         
         USING NETSYSD,R6                                                       
         NETGO NVDEMCON,DMCB,((R2),NDDEMBLK),(C'C',DBLOCK),(7,WORK)             
         DROP  R6                                                               
         MVC   0(6,R4),WORK        ONLY NEED AGE/SEX                            
         CLI   6(R4),C'-'                                                       
         BNE   HXIT                                                             
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R4),C'-'                                                       
         CLI   5(R4),C' '                                                       
         BNE   *+8                                                              
         MVI   5(R4),C'-'                                                       
         B     HXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         BUFF  LINES=500,ROWS=1,COLUMNS=33,FLAVOR=BINARY,              X        
               COMMENT=80,KEYLIST=(64,A)                                        
         SPACE 1                                                                
GROUPLST CSECT                                                                  
         DC    880X'00'                                                         
         EJECT                                                                  
*              DSECTS FOR OVERNIGHT                                             
         SPACE 3                                                                
OVERD    DSECT                     COMMON WITH EDIT                             
*              NETDEMOT AND                                                     
*              DEDBLOCK HERE                                                    
         PRINT OFF                                                              
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         SPACE 1                                                                
         PRINT ON                                                               
*                                  FIELDS SHARED WITH EDIT                      
DPFILT   DS    CL1                                                              
MENU     DS    CL1                                                              
COLS     DS    CL10                                                             
DETMENU  DS    CL1                                                              
RECMENU  DS    CL1                                                              
SPACOPT  DS    CL1                                                              
COMMOPT  DS    CL1                                                              
SEPOPT   DS    CL1                                                              
BOXOPT   DS    CL1                                                              
TOGOPT   DS    CL1                                                              
ACCPCTG  DS    F                                                                
         SPACE 1                                                                
*                                  LOCAL WORKING STORAGE                        
ACTDOLS  DS    F                   ACTUAL DOLLARS SAVED HERE                    
ASSDOLS  DS    F                   ASSIGNED DOLLARS SAVED HERE                  
SEPSPOT  DS    CL1                                                              
SINGROUP DS    CL3                                                              
HEADGRP  DS    CL3                                                              
COMMSW   DS    CL1                                                              
NCOLS    DS    CL1                                                              
NDETAILS DS    CL1                                                              
NRECAP   DS    CL1                                                              
SUBTOT   DS    CL1                                                              
DETAILS  DS    CL8                                                              
RECAP    DS    CL4                                                              
RELO     DS    A                                                                
ABUFFC   DS    A                                                                
SAVEMG   DS    CL148                                                            
SAVEBNO  DS    CL3                                                              
SAVEBDSB DS    CL1                                                              
MYKEY    DS    CL13                                                             
MYSAVE   DS    CL13                                                             
MYMODE   DS    CL1                                                              
SAVESMON DS    CL1                                                              
SAVESUB  DS    CL1                                                              
SAVMKT   DS    CL6                                                              
SAVPRD   DS    CL1                                                              
         SPACE 1                                                                
SAVETYPE DS    CL1                                                              
SAVERDEM DS    CL1                                                              
SAVEMSDT DS    CL2                                                              
SAVEMGDT DS    CL2                                                              
SAVEMSNM DS    CL16                                                             
SAVEMGNM DS    CL16                                                             
SAVPROG  DS    CL16                                                             
SAVLINE  DS    CL1                                                              
SAVERES  DS    CL1                                                              
SAVECLI  DS    CL3                                                              
SAVECLIN DS    CL20                                                             
SAVEPRD  DS    CL3                                                              
SAVEPRDN DS    CL20                                                             
SAVEGRP  DS    CL3                                                              
SAVEGRPN DS    CL24                                                             
SAVESTUN DS    H                                                                
SAVACTUN DS    H                                                                
HOLDPCTG DS    F                                                                
TEMPPNM  DS    CL1                                                              
CURPRD   DS    CL1                                                              
PIGFLAG  DS    CL1                                                              
LASTTYPE DS    CL1                                                              
BREAK    DS    CL12                                                             
BRLEN    DS    CL1                                                              
WLIST    DS    421C                                                             
MLIST    DS    CL97                                                             
QLIST    DS    CL33                                                             
MOBLIST  DS    CL16                                                             
LASTP    DS    CL40                                                             
SAVEP    DS    CL40                                                             
SUBREC   DS    CL1                                                              
EWLIST   DS    105CL5                                                           
EMLIST   DS    24CL5                                                            
EQLIST   DS    8CL11                                                            
ACTDISP  DS    F                                                                
SAVER4   DS    F                                                                
SAVER1   DS    F                                                                
SUBDISP  DS    A                                                                
COLDISP  DS    A                                                                
         SPACE 1                                                                
ESTASS1  DS    PL6                                                              
ESTASS2  DS    PL6                                                              
ESTACT1  DS    PL6                                                              
ESTACT2  DS    PL6                                                              
ESTGROS  DS    PL6                                                              
ESTDIFF  DS    PL6                                                              
ESTINTG  DS    PL6                                                              
ESTCUTN  DS    PL6                                                              
ESTTOT1  DS    PL6                                                              
ESTTOT2  DS    PL6                                                              
ESTGAP   DS    PL6                                                              
ESTCOMM  DS    PL6                                                              
ESTNETT  DS    PL6                                                              
ESTNETP  DS    PL6                                                              
ESTPAID  DS    PL6                                                              
ESTUNPD  DS    PL6                                                              
ESTBILL  DS    PL6                                                              
ESTUNBL  DS    PL6                                                              
ESTINTN  DS    PL6                                                              
ESTTOT   DS    PL6                                                              
         SPACE 1                                                                
ZERFLAGS DS    0CL20                                                            
ZERASS1  DS    CL1                                                              
ZERASS2  DS    CL1                                                              
ZERACT1  DS    CL1                                                              
ZERACT2  DS    CL1                                                              
ZERGROS  DS    CL1                                                              
ZERDIFF  DS    CL1                                                              
ZERINTG  DS    CL1                                                              
ZERCUTN  DS    CL1                                                              
ZERTOT1  DS    CL1                                                              
ZERTOT2  DS    CL1                                                              
ZERGAP   DS    CL1                                                              
ZERCOMM  DS    CL1                                                              
ZERNETT  DS    CL1                                                              
ZERNETP  DS    CL1                                                              
ZERPAID  DS    CL1                                                              
ZERUNPD  DS    CL1                                                              
ZERBILL  DS    CL1                                                              
ZERUNBL  DS    CL1                                                              
ZERINTN  DS    CL1                                                              
ZERTOT   DS    CL1                                                              
         SPACE 1                                                                
RECWIDTH DS    F                                                                
DETWIDTH DS    F                                                                
LFTWIDTH DS    F                                                                
COLWIDTH DS    F                                                                
RECINSET DS    F                                                                
DETINSET DS    F                                                                
THISCOL  DS    A                                                                
COLCHAR  DS    CL1                                                              
         EJECT                                                                  
*              BUFFALO I/O AREA                                                 
         SPACE 2                                                                
BUFFIO   DS    0D                                                               
BUFFKPAG DS    0CL16                                                            
BUFFCLI  DS    CL3                                                              
BUFFGRP  DS    CL3                                                              
BUFFPRD  DS    CL3                                                              
BUFFEST  DS    CL3                                                              
BUFFNET  DS    CL4                                                              
BUFFTYPE DS    CL1                                                              
BUFFKEY  DS    CL46                                                             
BUFFSUB  DS    CL1                                                              
         SPACE 1                                                                
BUFFCHES DS    XL1                 ESTIMATE NUMBER                              
BUFFCHPK DS    XL1                 PACKAGE NUMBER                               
BUFFCHST DS    XL1                 UNIT STATUS                                  
BUFFCHRS DS    CL1                 RESULT CODE                                  
         SPACE 1                                                                
BUFFPNUM DS    CL1                 PROD NUMBER (FOR COMMENTS)                   
         DS    CL3                                                              
BUFFLEFT DS    CL36                                                             
BUFFRT   DS    CL36                                                             
         SPACE 1                                                                
BUFFUNIT DS    F                   ESTIMATES                                    
BUFFASS  DS    F                                                                
BUFFACT  DS    F                                                                
BUFFINT  DS    F                                                                
BUFFCUT  DS    F                                                                
BUFFPAID DS    F                                                                
BUFFBILL DS    F                                                                
BUFFIBL  DS    F                                                                
BUFFIPD  DS    F                                                                
         SPACE 1                                                                
ACUN     EQU   BUFFUNIT                                                         
ACINT    EQU   BUFFINT                                                          
         ORG   BUFFACT                                                          
BUFFCOST DS    F                   EVALUATION & POST EST                        
BUFFRH   DS    F                                                                
BUFFIH   DS    F                                                                
BUFFR1   DS    F                                                                
BUFFI1   DS    F                                                                
BUFFR2   DS    F                                                                
BUFFI2   DS    F                                                                
BUFFR3   DS    F                                                                
BUFFI3   DS    F                                                                
BUFFHT   DS    F                   EVALUATION                                   
BUFFSH   DS    F                                                                
BUFFV1   DS    F                                                                
BUFFV2   DS    F                                                                
BUFFV3   DS    F                                                                
         SPACE 1                                                                
BUFFAUN  DS    F                   POST ACTUAL                                  
BUFFAASS DS    F                                                                
BUFFACST DS    F                                                                
BUFFARH  DS    F                                                                
BUFFAIH  DS    F                                                                
BUFFAR1  DS    F                                                                
BUFFAI1  DS    F                                                                
BUFFAR2  DS    F                                                                
BUFFAI2  DS    F                                                                
BUFFAR3  DS    F                                                                
BUFFAI3  DS    F                                                                
BUFFAHT  DS    F                                                                
BUFFASH  DS    F                                                                
BUFFAV1  DS    F                                                                
BUFFAV2  DS    F                                                                
BUFFAV3  DS    F                                                                
BUFFORCE DS    F                                                                
BUFFSAVE DS    CL132                                                            
ACTSUBS  DS    CL132                                                            
LASTRES  DS    CL1                                                              
ACTOPT   DS    CL1                                                              
DUB2     DS    D                                                                
ZEROFLAG DS    CL1                                                              
ZEROACT  DS    CL1                                                              
ZEROASS  DS    CL1                                                              
         SPACE 1                                                                
ESTLIST  DS    CL256                                                            
         SPACE 1                                                                
ANETCOM  DS    A                   STUFF FOR COMMENTS                           
CURCOMM  DS    A                                                                
COMMAREA DS    CL50                                                             
         SPACE 1                                                                
*              SPGENEST            INCLUDES HERE                                
*              SPGENPRG                                                         
*              SPGENCLT                                                         
*              SPGENPRD                                                         
*              NEGENUNIT                                                        
*              NEMEDFFD                                                         
*              NEMEDF6D                                                         
*              NETINCLS                                                         
*              NECOMBLOK                                                        
*              NEGENCOM                                                         
         PRINT OFF                                                              
DUMMYD   DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENPRG                                                       
CLIDSECT DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PDDUMMY  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF6D                                                       
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NECOMBLOK                                                      
       ++INCLUDE NEGENCOM                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED26T  05/01/02'                                      
         END                                                                    
