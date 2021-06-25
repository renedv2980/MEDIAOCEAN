*          DATA SET NERES20    AT LEVEL 175 AS OF 05/01/02                      
*PHASE T32120A,*                                                                
T32120   TITLE '-   GRID REPORT'                                                
T32103   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GRD**,RA,RR=R2                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
         SPACE 3                                                                
VKEY     DS    0C                                                               
         MVI   OPTION,X'FF'                                                     
         MVI   OPTION,X'FF'        ALL ALLOWED                                  
         LA    RF,DBLOCKA                                                       
         USING DBLOCKD,RF                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,AGENCY     FOR LOCKOUTS                                 
         MVC   DBAUTH,TWAAUTH                                                   
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
         SPACE 1                                                                
         MVI   DBSELSRC,C'N'       NTI VALIDATION                               
         MVI   DBSELMED,C'N'                                                    
         MVI   DBFUNCT,DBGETNTI                                                 
         MVC   DBFILE,=C'NTI'                                                   
         MVC   SVFILE,DBFILE                                                    
         MVC   SVSOURCE,DBSELSRC                                                
         MVC   SVMEDIA,DBSELMED                                                 
         DROP  RF                                                               
         MVC   TPSBK,=X'5F24'                                                   
         MVC   PRSBK,=X'5F24'                                                   
         LA    R2,GRDTPBKH         VALIDATE BOOK                                
         CLI   5(R2),0                                                          
         BE    VALPRBK                                                          
         MVI   MAX,1                                                            
         GOTO1 VVALBOOK                                                         
         CLC   BOOKS+1(2),=X'5F24'                                              
         BL    BADBOOK                                                          
         MVC   TPSBK,BOOKS+1                                                    
         SPACE 1                                                                
VALPRBK  LA    R2,GRDPRBKH         VALIDATE BOOK                                
         CLI   5(R2),0                                                          
         BE    VALBBK                                                           
         MVI   MAX,1                                                            
         GOTO1 VVALBOOK                                                         
         CLC   BOOKS+1(2),=X'5F24'                                              
         BL    BADBOOK                                                          
         MVC   PRSBK,BOOKS+1                                                    
         SPACE 1                                                                
VALBBK   LA    R2,GRDBBKH          VALIDATE BOOK                                
         MVI   MAX,1                                                            
         GOTO1 VVALBOOK                                                         
         CLC   BOOKS+1(2),=X'5F24'                                              
         BL    BADBOOK                                                          
         CLC   BOOKS+1(2),TPSBK                                                 
         BL    BADBOOK                                                          
         CLC   BOOKS+1(2),PRSBK                                                 
         BL    BADBOOK                                                          
         SPACE 1                                                                
         LA    R2,GRDDEMOH         VALIDATE DEMOS                               
         MVI   MAX,8                                                            
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
*        MVC   NUMDEMS,ACTUAL      SAVE NUMBER OF DEMOS                         
         MVI   NUMDEMS,2           OVERRIDE TO R1,S1                            
*        MVC   DEMOS,DEMOSET                                                    
         MVI   DEMOS+1,C'R'        FORCE TO RATING                              
         MVC   DEMOS+3(3),DEMOS                                                 
         MVI   DEMOS+6,X'FF'                                                    
         MVI   DEMOS+4,C'S'         AND SHARE                                   
         MVC   DEMOS2,DEMOS                                                     
         MVI   DEMOS2+4,C'P'       SHARE = RATING/PUT                           
         SPACE 1                                                                
         LA    R2,GRDNETH          VALIDATE NETWORK                             
         LA    R3,6                                                             
         LA    R4,NETSAVE                                                       
         XC    NETSAVE,NETSAVE                                                  
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY20   GOTO1 VVALNET                                                          
         MVC   0(7,R4),ACTNET                                                   
         LA    R4,7(R4)                                                         
VKEY25   BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY30                                                           
         CLI   5(R2),0             ANOTHER NETWORK?                             
         BNE   VKEY20                                                           
         B     VKEY25                                                           
         SPACE 1                                                                
VKEY30   LA    R2,GRDDAYH          DAY                                          
         MVI   DAYNUM,X'FF'                                                     
         CLC   8(3,R2),=C'ALL'                                                  
         BE    BADDAY                                                           
         GOTO1 VVALDAY                                                          
         MVC   DAYNUM,ACTUAL                                                    
         SPACE 1                                                                
VKEY40   LA    R2,GRDSTRTH         START TIME                                   
         GOTO1 VVALTIM                                                          
         MVC   STARTNUM,ACTUAL                                                  
         MVC   SAVSTART,STARTNUM                                                
         MVC   MILST,WORK                                                       
         LA    R2,GRDENDH          END TIME                                     
         GOTO1 VVALTIM                                                          
         MVC   ENDNUM,ACTUAL                                                    
         MVC   MILEND,WORK                                                      
         ZIC   RE,STARTNUM                                                      
         ZIC   RF,ENDNUM                                                        
         SR    RF,RE                                                            
         CH    RF,=H'12'                                                        
         BH    TIMERR                                                           
         SR    R1,R1                                                            
         LA    R0,6                                                             
         ICM   R1,3,MILST                                                       
         LA    R2,H6+17                                                         
HLTIME1  BAS   RE,CNVTIM                                                        
         AH    R1,=H'30'                                                        
         LR    RF,R1                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         CH    RE,=H'60'                                                        
         BL    *+12                                                             
         SH    R1,=H'60'                                                        
         AH    R1,=H'100'                                                       
         LA    R2,18(R2)                                                        
         BCT   R0,HLTIME1                                                       
         MVC   SVH6,H6                                                          
         SPACE 1                                                                
         LA    R2,GRDOPTH          OPTIONS                                      
         BAS   RE,VALOPT                                                        
         LA    R2,GRDTITLH         OWN TITLE                                    
         GOTO1 VVALTITL                                                         
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         SPACE 2                                                                
CNVTIM   STH   R1,HALF                                                          
         MVC   0(17,R2),=C'-------------------'                                 
         CH    R1,=H'2499'                                                      
         BL    *+8                                                              
         SH    R1,=H'2400'                                                      
         MVC   10(3,R2),=C' AM'                                                 
         CH    R1,=H'1200'                                                      
         BL    *+10                                                             
         MVC   10(3,R2),=C' PM'                                                 
         CH    R1,=H'1300'                                                      
         BL    *+8                                                              
         SH    R1,=H'1200'                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB+5(3)                                                 
         MVC   5(2,R2),WORK                                                     
         MVI   7(R2),C':'                                                       
         MVC   8(2,R2),WORK+2                                                   
         LH    R1,HALF                                                          
         BR    RE                                                               
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VALOPT   NTR1                                                                   
         MVI   SPACOPT,1           PRESET VALUES FOR OPTIONS                    
         MVI   DAYOPT,C'P'         P=POCKETPIECE                                
         SPACE 1                                                                
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(8,BLOCK)                                      
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    BADOPT                                                           
         LA    R3,BLOCK                                                         
         SPACE 1                                                                
OPT2     CLC   12(2,R3),=C'S  '                                                 
         BNE   OPT4                                                             
         MVI   SPACOPT,2                                                        
         CLI   22(R3),C'2'                                                      
         BE    OPTEND                                                           
         MVI   SPACOPT,3                                                        
         CLI   22(R3),C'3'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
         SPACE 1                                                                
OPT4     CLC   12(3,R3),=C'DAY'                                                 
         BNE   OPT9                                                             
         CLI   22(R3),C'I'         I=INDIVIDUAL DAYS                            
         BNE   BADOPT                                                           
         MVC   DAYOPT,22(R3)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT9     B     BADOPT                                                           
         SPACE 1                                                                
OPTEND   LA    R3,32(R3)                                                        
         BCT   R4,OPT2                                                          
         B     XIT                                                              
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYEND                                                            
BADBOOK  MVC   CONHEAD(L'INVBOK),INVBOK                                         
         B     MYEND                                                            
BADDAY   MVC   CONHEAD(L'INVDAY),INVDAY                                         
         B     MYEND                                                            
TIMERR   MVC   CONHEAD(L'MAXTIM),MAXTIM                                         
         B     MYEND                                                            
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
*              INPUT               R4=A(MAIN DBLOCK)                            
         SPACE 1                                                                
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         SPACE 1                                                                
*              CONTROL OF MULTIPLE/SINGLE DAY                                   
         SPACE 3                                                                
SDAY     CLI   DAYNUM,X'FF'                                                     
         BE    SD2                                                              
         BAS   RE,REPDAY                                                        
         B     XIT                                                              
         SPACE 1                                                                
SD2      LA    R2,1                                                             
         LA    R3,7                                                             
         MVC   SAVSTART,STARTNUM                                                
         SPACE 1                                                                
SD4      STC   R2,DAYNUM                                                        
         MVC   STARTNUM,SAVSTART                                                
         BAS   RE,REPDAY                                                        
         LA    R2,1(R2)                                                         
         BCT   R3,SD4                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO CONTROL REPORT                                       
         SPACE 3                                                                
REPDAY   NTR1                                                                   
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         MVC   DBSELBK,BOOKS+1     SELECT BOOK                                  
         ZIC   R1,DAYNUM           SELECT DAY                                   
         LA    R1,DAYLIST(R1)      (NEED SPOT-TYPE)                             
         MVC   DBSELDAY,0(R1)                                                   
         MVI   DBPRGDUR,C'Y'       SET FOR TOTAL PROGRAM                        
         MVI   DBBEST,0                                                         
         CLI   DBSELDAY,X'7C'      FOR M-F                                      
         BNE   *+8                                                              
         MVI   DBBEST,C'L'         ONLY WANT ROTATORS                           
         CLI   DBSELDAY,X'7F'      ALSO FOR M-S                                 
         BNE   *+8                                                              
         MVI   DBBEST,C'L'                                                      
         DROP  RF                                                               
         SPACE 1                                                                
REPDAY2  MVI   FORCEHED,C'Y'                                                    
         MVI   FIRST,C'Y'                                                       
         SPACE 1                                                                
REPLOOP  BAS   RE,PROCESS                                                       
         B     XIT                                                              
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVI   FIRST,C'Y'                                                       
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
REPLOOP4 BAS   RE,ALLSTARS                                                      
         GOTO1 SPOOL,PARAS,(R8)                                                 
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL I/O                                                      
         SPACE 3                                                                
PROCESS  NTR1                                                                   
         XC    MYNETS,MYNETS                                                    
         MVC   MYNETS(L'NETSAVE),NETSAVE                                        
         LA    R2,MYNETS                                                        
         ZIC   R3,NUMNETS                                                       
         CLI   DEMOS+2,1           NO NON-NET IF NOT HOMES                      
         BNE   PROCESS1                                                         
         MH    R3,=H'7'                                                         
         AR    R3,R2                                                            
         MVC   0(7,R3),0(R2)                                                    
         MVC   0(3,R3),=C'IND'                                                  
         MVC   7(7,R3),0(R2)                                                    
         MVC   7(3,R3),=C'CAB'                                                  
         MVC   14(7,R3),0(R2)                                                   
         MVC   14(3,R3),=C'PAY'                                                 
         ZIC   R3,NUMNETS                                                       
         LA    R3,3(R3)                                                         
         STC   R3,NUMNETS                                                       
         SPACE 2                                                                
PROCESS1 LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         MVC   DBSELTIM(2),MILST                                                
         MVC   DBSELTIM+2(2),MILEND                                             
         MVC   WORKAREA,DBLOCKA                                                 
         SPACE 1                                                                
PROCESS2 LA    RF,DBLOCKA                                                       
         MVC   DBLOCKA(256),WORKAREA                                            
         XC    TPAVG,TPAVG                                                      
         XC    TPSAVG,TPSAVG                                                    
         MVC   BUFF(2),=X'FFFF'                                                 
         MVC   DBSELBK,BOOKS+1                                                  
         MVC   DBSELSTA,0(R2)      SELECT NETWORK                               
         MVC   DBDAYOPT,DAYOPT     P (POCKETPIECE) OR I (INDIVIDUAL)            
         MVI   DBFUNCT,DBGETNTI                                                 
         XC    STARTNUM,STARTNUM                                                
         GOTO1 VADJSEL             ADJUST FOR PEOPLE-METERS                     
         CLC   DBSELSTA(3),=C'IND'                                              
         BE    PRS5                                                             
         CLC   DBSELSTA(3),=C'CAB'                                              
         BE    PRS5                                                             
         CLC   DBSELSTA(3),=C'PAY'                                              
         BE    PRS5                                                             
         GOTO1 DEMAND,DMCB,DBLOCKA,PROCPR                                       
         LA    RF,DBLOCKA                                                       
         OC    DBDIVSOR,DBDIVSOR                                                
         BZ    PRS5                                                             
         CLC   DBSELSTA(3),=C'   '                                              
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
         LA    RF,DBLOCKA                                                       
         MVC   DBSELBK,PRSBK                                                    
PRS1     LA    R5,BUFF                                                          
         MVC   DBSELPRG,0(R5)                                                   
PRS2     GOTO1 DEMAND,DMCB,DBLOCKA,PROCPRS                                      
         LA    RF,DBLOCKA                                                       
         LA    R5,PRBLEN(R5)                                                    
         MVC   DBSELPRG,0(R5)                                                   
         CLC   0(2,R5),=X'FFFF'                                                 
         BNE   PRS2                                                             
         ZIC   RE,DBSELBK+1                                                     
         LA    RE,1(RE)                                                         
         STC   RE,DBSELBK+1                                                     
         CH    RE,=H'52'                                                        
         BL    PRS4                                                             
         MVI   DBSELBK+1,1                                                      
         ZIC   RE,DBSELBK                                                       
         LA    RE,1(RE)                                                         
         STC   RE,DBSELBK                                                       
PRS4     CLC   BOOK+1(2),DBSELBK                                                
         BNL   PRS1                                                             
         DROP  RF                                                               
*                                                                               
PRS5     LA    R5,BUFF                                                          
         LA    R5,1000(R5)                                                      
         XC    0(250,R5),0(R5)                                                  
PROCESS3 ST    R5,COST                                                          
         MVC   DBLOCKA(256),WORKAREA                                            
         XC    0(PRBHLEN,R5),0(R5)                                              
         MVI   QHNUM,0                                                          
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         MVC   DBSELSTA,0(R2)      SELECT NETWORK                               
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELBK(2),BOOKS+1                                               
         XC    DBSELPRG,DBSELPRG                                                
         GOTO1 VADJSEL             ADJUST FOR PEOPLE-METERS                     
         GOTO1 DEMAND,DMCB,DBLOCKA,PROCTP                                       
         OC    DBDIVSOR,DBDIVSOR                                                
         BZ    PRNTLOP6                                                         
         CLC   DBSELSTA(3),=C'IND'                                              
         BE    PROCSTD3                                                         
         CLC   DBSELSTA(3),=C'CAB'                                              
         BE    PROCSTD3                                                         
         CLC   DBSELSTA(3),=C'PAY'                                              
         BE    PROCSTD3                                                         
         MVC   DBLOCKA(256),WORKAREA                                            
         MVI   QHNUM,0                                                          
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         MVC   DBSELSTA,0(R2)      SELECT NETWORK                               
         MVI   DBFUNCT,DBGETDEM                                                 
         GOTO1 VADJSEL             ADJUST FOR PEOPLE-METERS                     
         LA    RF,DBLOCKA                                                       
         MVC   DBSELBK,TPSBK                                                    
         LA    R5,BUFF                                                          
         LA    R5,2000(R5)                                                      
         XC    0(250,R5),0(R5)                                                  
         ST    R5,COST                                                          
PROCSTD1 MVI   QHNUM,0                                                          
         LA    RF,DBLOCKA                                                       
         MVI   DBSTYPE,0                                                        
         GOTO1 DEMAND,DMCB,DBLOCKA,PROCTPS                                      
         LA    RF,DBLOCKA                                                       
         ZIC   RE,DBSELBK+1                                                     
         LA    RE,1(RE)                                                         
         STC   RE,DBSELBK+1                                                     
         CH    RE,=H'52'                                                        
         BL    PROCSTD2                                                         
         MVI   DBSELBK+1,1                                                      
         ZIC   RE,DBSELBK                                                       
         LA    RE,1(RE)                                                         
         STC   RE,DBSELBK                                                       
PROCSTD2 CLC   BOOK+1(2),DBSELBK                                                
         BNL   PROCSTD1                                                         
         DROP  RF                                                               
PROCSTD3 LA    RF,P5+17                                                         
         ST    RF,DETDISP                                                       
         MVC   P5,SPACES                                                        
         LA    RF,P+17                                                          
         ST    RF,COST                                                          
         LA    R5,BUFF                                                          
         USING PRBD,R5                                                          
PRNTLOP1 CLC   0(2,R5),=X'FFFF'                                                 
         BE    PRNTLOP2                                                         
         B     REAL1                                                            
         SR    RE,RE                                                            
         ICM   RE,3,PRBNTI                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P(5),DUB+5(3)                                                    
         MVC   P+6(25),PRBPROG                                                  
         MVC   P+32(25),PRBEPIS                                                 
         ICM   RE,3,PRBSCNT                                                     
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+58(3),DUB+6(2)                                                 
         ICM   RE,3,PRBCOVER                                                    
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+62(3),DUB+6(2)                                                 
         MVC   P+66(4),PRBTYPE                                                  
         ICM   RE,3,PRBD1                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+71(3),DUB+6(2)                                                 
         ICM   RE,3,PRBD1+2                                                     
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+75(3),DUB+6(2)                                                 
         B     REALPR1                                                          
REAL1    LA    RF,DBLOCKA                                                       
         USING DBLOCKD,RF                                                       
         MVC   P(3),DBSELSTA                                                    
         CLC   DBSELSTA(3),=C'IND'                                              
         BE    PRNTLOP2A                                                        
         CLC   DBSELSTA(3),=C'CAB'                                              
         BE    PRNTLOP2A                                                        
         CLC   DBSELSTA(3),=C'PAY'                                              
         BE    PRNTLOP2A                                                        
         MVC   P(3),=C'   '                                                     
         MVC   P+4(4),=C'NAME'                                                  
         MVC   P2+4(4),=C'EPIS'                                                 
         MVC   P3+4(4),=C'S/C '                                                 
         MVC   P4+4(4),=C'PTD '                                                 
         MVC   P5+4(4),=C'PSTD'                                                 
         CLC   PRBQHS,SAVSTART                                                  
         BH    *+10                                                             
         MVC   PRBQHS,SAVSTART                                                  
         ZIC   RE,PRBQHS                                                        
         ZIC   RF,SAVSTART                                                      
         SR    RE,RF                                                            
         SRL   RE,1                                                             
         MH    RE,=H'18'                                                        
         STH   RE,HALF                                                          
         A     RE,COST                                                          
         MVC   0(17,RE),PRBPROG                                                 
         CLI   19(RE),C' '                                                      
         BH    *+10                                                             
         MVC   0(25,RE),PRBPROG                                                 
         AH    RE,=H'132'                                                       
         MVC   0(17,RE),PRBEPIS                                                 
         CLI   19(RE),C' '                                                      
         BH    *+10                                                             
         MVC   0(25,RE),PRBEPIS                                                 
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   0(RE),0                                                          
         AH    RE,=H'132'                                                       
         SR    R1,R1                                                            
         ICM   R1,3,PRBSCNT                                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,RE),DUB+6(2)                                                 
         MVI   3(RE),C'/'                                                       
         EDIT  (B2,PRBCOVER),(3,4(RE)),ALIGN=LEFT                               
         LA    R1,7(RE)                                                         
         CLI   PRBPREM,C'Y'                                                     
         BNE   *+14                                                             
         MVC   0(3,R1),=C'(P)'                                                  
         LA    R1,3(R1)                                                         
         CLI   PRBTYPE+2,C'S'                                                   
         BNE   *+14                                                             
         MVC   0(3,R1),=C'(S)'                                                  
         LA    R1,3(R1)                                                         
         CLI   PRBTYPE+3,C'R'                                                   
         BNE   *+10                                                             
         MVC   0(3,R1),=C'(R)'                                                  
         AH    RE,=H'132'                                                       
         EDIT  (B2,PRBD1),(4,0(RE)),1                                           
         MVI   4(RE),C'/'                                                       
         ST    RE,FULL                                                          
         LH    RF,PRBD1+2                                                       
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         STH   RF,PRBD1+2                                                       
         L     RE,FULL                                                          
         EDIT  (B2,PRBD1+2),(4,5(RE)),ALIGN=LEFT,ZERO=BLANK                     
         OC    PRBD2(4),PRBD2                                                   
         BZ    UNWPR                                                            
         OC    PRBD2+8(4),PRBD2+8                                               
         BZ    REALZ2                                                           
         SR    RE,RE                                                            
         L     RF,PRBD2+4          CALCULATE THE SHARE                          
         M     RE,=F'200'                                                       
         D     RE,PRBD2+8                                                       
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,PRBD2+8                                                       
REALZ2   SR    RE,RE                                                            
         L     RF,PRBD2+4                                                       
         M     RE,=F'2'                                                         
         D     RE,PRBD2                                                         
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,PRBD2+4                                                       
*        SR    RE,RE                                                            
*        L     RF,PRBD2+8                                                       
*        D     RE,PRBD2                                                         
*        ST    RF,PRBD2+8                                                       
UNWPR    L     RE,DETDISP                                                       
         AH    RE,HALF                                                          
         MVI   0(RE),0                                                          
         EDIT  (B4,PRBD2+4),(4,0(RE)),1,ZERO=BLANK                              
         MVI   4(RE),C'/'                                                       
         EDIT  (B4,PRBD2+8),(4,5(RE)),ZERO=BLANK,ALIGN=LEFT                     
         LA    R5,PRBLEN(R5)                                                    
         B     PRNTLOP1                                                         
REALPR1  GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
         ICM   RE,15,PRBD2                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+1(7),DUB+4(4)                                                  
         ICM   RE,15,PRBD2+4                                                    
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+10(7),DUB+4(4)                                                 
         ICM   RE,15,PRBD2+8                                                    
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+19(7),DUB+4(4)                                                 
         GOTO1 SPOOL,PARAS,(R8)                                                 
         LA    R5,PRBLEN(R5)                                                    
         B     PRNTLOP1                                                         
PRNTLOP2 DS    0C                                                               
         MVC   P3(3),0(R2)                                                      
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   P,P5                                                             
         GOTO1 SPOOL,PARAS,(R8)                                                 
PRNTLOP2A LA    R5,BUFF                                                         
         LA    R5,1000(R5)                                                      
         LA    R1,6                                                             
         LA    RF,DBLOCKA                                                       
         USING DBLOCKD,RF                                                       
         CLC   DBSELSTA(3),=C'IND'                                              
         BE    *+10                                                             
         CLC   DBSELSTA(3),=C'CAB'                                              
         BE    *+10                                                             
         CLC   DBSELSTA(3),=C'PAY'                                              
         BNE   *+10                                                             
         MVC   P(3),DBSELSTA                                                    
         OC    TPAVG(4),TPAVG                                                   
         BZ    PRAVWX                                                           
         L     RF,TPAVG+8          UNWEIGHT SHARE                               
         SR    RE,RE                                                            
         M     RE,=F'2'                                                         
         D     RE,TPAVG                                                         
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         SR    RE,RE                                                            
         AH    RF,=H'5'                                                         
         D     RE,=F'10'                                                        
         ST    RF,FULL                                                          
         EDIT  (B4,FULL),(4,P+12),ZERO=BLANK                                    
         L     RF,TPAVG+4          UNWEIGHT RATING                              
         SR    RE,RE                                                            
         M     RE,=F'2'                                                         
         D     RE,TPAVG                                                         
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,FULL                                                          
         EDIT  (B4,FULL),(4,P+8),1,ZERO=BLANK                                   
PRAVWX   L     RF,COST                                                          
         MVC   P+4(4),=C'HTD '                                                  
PRNTLOP3 DS    0C                                                               
         EDIT  (B4,0(R5)),(4,0(RF)),1,ALIGN=LEFT,ZERO=BLANK                     
         LA    RF,09(RF)                                                        
         ST    RF,FULL                                                          
         L     RF,4(R5)                                                         
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         ST    RF,4(R5)                                                         
         L     RF,FULL                                                          
         EDIT  (B4,4(R5)),(4,0(RF)),ALIGN=LEFT,ZERO=BLANK                       
         LA    RF,09(RF)                                                        
         LA    R5,8(R5)                                                         
         BCT   R1,PRNTLOP3                                                      
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
         LA    RF,DBLOCKA                                                       
         USING DBLOCKD,RF                                                       
         CLC   DBSELSTA(3),=C'IND'                                              
         BE    PRNTLOP6                                                         
         CLC   DBSELSTA(3),=C'CAB'                                              
         BE    PRNTLOP6                                                         
         CLC   DBSELSTA(3),=C'PAY'                                              
         BE    PRNTLOP6                                                         
         MVC   P+4(4),=C'HSTD'                                                  
         OC    TPSAVG(4),TPSAVG                                                 
         BZ    PRSAVWX                                                          
         OC    TPSAVG+8(4),TPSAVG+8                                             
         BZ    TPSAVGZ                                                          
         L     RF,TPSAVG+4         CALULATE SHARE                               
         SR    RE,RE                                                            
         M     RE,=F'200'                                                       
         D     RE,TPSAVG+8                                                      
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,FULL                                                          
         EDIT  (B4,FULL),(4,P+12),ZERO=BLANK                                    
TPSAVGZ  L     RF,TPSAVG+4         UNWEIGHT RATING                              
         SR    RE,RE                                                            
         M     RE,=F'2'                                                         
         D     RE,TPSAVG                                                        
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,FULL                                                          
         EDIT  (B4,FULL),(4,P+8),1,ZERO=BLANK                                   
PRSAVWX  LA    R5,BUFF                                                          
         LA    R5,2000(R5)                                                      
         LA    R1,6                                                             
UNW4     OC    0(4,R5),0(R5)                                                    
         BZ    UNW4Z2                                                           
         OC    8(4,R5),8(R5)                                                    
         BZ    UNW4Z                                                            
         L     RF,4(R5)            CALCULATE THE SHARE                          
         SR    RE,RE                                                            
         M     RE,=F'200'                                                       
         D     RE,8(R5)                                                         
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,8(R5)                                                         
*                                                                               
UNW4Z    L     RF,4(R5)                                                         
         SR    RE,RE                                                            
         M     RE,=F'2'                                                         
         D     RE,0(R5)                                                         
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,4(R5)                                                         
*        L     RF,8(R5)                                                         
*        SR    RE,RE                                                            
*        D     RE,0(R5)                                                         
*        ST    RF,8(R5)                                                         
UNW4Z2   LA    R5,16(R5)                                                        
         BCT   R1,UNW4                                                          
*                                                                               
         LA    R5,BUFF                                                          
         LA    R5,2000(R5)                                                      
         LA    R1,6                                                             
         L     RF,COST                                                          
PRNTLOP4 DS    0C                                                               
         EDIT  (B4,4(R5)),(4,0(RF)),1,ALIGN=LEFT,ZERO=BLANK                     
         LA    RF,09(RF)                                                        
         EDIT  (B4,8(R5)),(4,0(RF)),ALIGN=LEFT,ZERO=BLANK                       
         LA    RF,09(RF)                                                        
         LA    R5,16(R5)                                                        
         BCT   R1,PRNTLOP4                                                      
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVI   P,C'*'                                                           
         MVC   P+1(131),P                                                       
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
PRNTLOP6 LA    R2,7(R2)                                                         
         BCT   R3,PROCESS2                                                      
         MVC   BUFF(2),=X'FFFF'                                                 
         B     PROCESS6                                                         
PROCESS6 B     XIT                                                              
         SPACE 1                                                                
PROCPRS  NTR1                                                                   
         GOTO1 DEFINE,PARAS,=C'PREM',DBLOCKA,DUB                                
         CLI   DUB,C'Y'                                                         
         BNE   *+10                                                             
         XC    PRBD2,PRBD2                                                      
         LA    R2,DEMOS2                                                        
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCKA,DUB                              
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         LH    RE,DBFACTOR                                                      
         LR    R0,RE                                                            
         A     RE,PRBD2                                                         
         ST    RE,PRBD2                                                         
         L     RE,DUB                                                           
         LR    RF,R0                                                            
         MR    RE,RE                                                            
         A     RF,PRBD2+4                                                       
         ST    RF,PRBD2+4                                                       
         L     RE,DUB+4                                                         
         LR    RF,R0                                                            
         MR    RE,RE                                                            
         A     RF,PRBD2+8                                                       
         ST    RF,PRBD2+8                                                       
         B     XIT                                                              
         DROP  R5                                                               
         DROP  RF                                                               
         SPACE 2                                                                
PROCPR   NTR1                                                                   
*        MVC   FILTER,DUB                                                       
*        GOTO1 VCHEFILT,PARAS,DUB                                               
*        BNE   XIT                                                              
         ZIC   RE,STARTNUM                                                      
         LA    RF,PRBLEN                                                        
         MR    RE,RE                                                            
         LA    R5,BUFF(RF)                                                      
         USING PRBD,R5                                                          
         XC    PRBD(PRBLEN),PRBD                                                
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         MVC   PRBNTI,DBACTPRG                                                  
         GOTO1 DEFINE,PARAS,=C'TYPE',DBLOCKA,PRBTYPE                            
         GOTO1 DEFINE,PARAS,=C'PROG25',DBLOCKA,PRBPROG                          
         GOTO1 DEFINE,PARAS,=C'EPIS25',DBLOCKA,PRBEPIS                          
         GOTO1 DEFINE,PARAS,=C'SCOUNT',DBLOCKA,PRBSCNT                          
         GOTO1 DEFINE,PARAS,=C'COVERAGE',DBLOCKA,PRBCOVER                       
         GOTO1 DEFINE,PARAS,=C'TIME',DBLOCKA,PRBTIME                            
         GOTO1 DEFINE,PARAS,=C'PREM',DBLOCKA,PRBPREM                            
         LA    R2,DEMOS                                                         
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCKA,DUB                              
         MVC   PRBD1(2),DUB+2                                                   
         MVC   PRBD1+2(2),DUB+6                                                 
         LA    R5,PRBLEN(R5)                                                    
         MVC   0(2,R5),=X'FFFF'                                                 
         SPACE 1                                                                
         ZIC   RE,STARTNUM                                                      
         LA    RE,1(RE)                                                         
         STC   RE,STARTNUM                                                      
         B     XIT                                                              
         DROP  RF                                                               
         DROP  R5                                                               
         SPACE 2                                                                
PROCTP   NTR1                                                                   
         GOTO1 DEFINE,PARAS,=C'TIME',DBLOCKA,DMCB                               
         CLC   DMCB(1),SAVSTART                                                 
         BH    *+10                                                             
         MVC   DMCB(1),SAVSTART                                                 
         ZIC   RE,DMCB                                                          
         ZIC   RF,SAVSTART                                                      
         SR    RE,RF                                                            
         SRL   RE,1                                                             
         STC   RE,QHNUM                                                         
         LA    R2,DEMOS                                                         
         ZIC   RF,QHNUM                                                         
         SLL   RF,3                                                             
         A     RF,COST                                                          
         PRINT GEN                                                              
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCKA,(RF)                             
* ACCUMULATE DEMOS FOR AVERAGE                                                  
         L     RF,8(R1)            RESTORE OUTPUT POINTER                       
         LA    RE,1                                                             
         A     RE,TPAVG                                                         
         ST    RE,TPAVG            COUNT                                        
         L     RE,0(RF)                                                         
         A     RE,TPAVG+4                                                       
         ST    RE,TPAVG+4          RATING                                       
         L     RE,4(RF)                                                         
         A     RE,TPAVG+8                                                       
         ST    RE,TPAVG+8          PUT                                          
         PRINT NOGEN                                                            
         SPACE 1                                                                
*        ZIC   RE,QHNUM                                                         
*        LA    RE,1(RE)                                                         
*        STC   RE,QHNUM                                                         
         B     XIT                                                              
         SPACE 2                                                                
PROCTPS  NTR1                                                                   
         GOTO1 DEFINE,PARAS,=C'TIME',DBLOCKA,DMCB                               
         CLC   DMCB,SAVSTART                                                    
         BH    *+10                                                             
         MVC   DMCB,SAVSTART                                                    
         ZIC   RE,DMCB                                                          
         ZIC   RF,SAVSTART                                                      
         SR    RE,RF                                                            
         SRL   RE,1                                                             
         STC   RE,QHNUM                                                         
         LA    R2,DEMOS2                                                        
         ZIC   R5,QHNUM                                                         
         SLL   R5,4                                                             
         A     R5,COST                                                          
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCKA,DUB                              
         SPACE 1                                                                
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         L     R1,DBAREC                                                        
         USING PRKEY,R1                                                         
         MVC   HALF(1),PRDW                                                     
         NI    HALF,X'F0'                                                       
         SR    RF,RF                                                            
         LA    RF,1                                                             
         CLI   HALF,0                                                           
         BNE   *+8                                                              
         LA    RF,5                                                             
         TM    HALF,X'80'                                                       
         BZ    *+8                                                              
         LA    RF,7                                                             
         STH   RF,HALF                                                          
         LA    RF,DBLOCKA                                                       
         L     RE,DBAQUART                                                      
         USING PHELEM,RE                                                        
         SR    RF,RF                                                            
PRDUR    CLI   PHCODE,PHCODEQ                                                   
         BH    PRDUR9                                                           
         BE    PRDUR1                                                           
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     PRDUR                                                            
PRDUR1   CLI   PHDURTOT,0          MISSING                                      
         BE    PRDUR3                                                           
*                                                                               
         CLI   PRSTAT+4,C'C'       CABLE TP HAS QTR HOURS HERE                  
         BE    PRDUR3                                                           
         CLI   PRSTAT+4,C'Q'                                                    
         BE    PRDUR3                                                           
*                                                                               
         ZIC   R1,PHDURTOT                                                      
         MH    R1,HALF                                                          
         B     PRDUR8                                                           
PRDUR3   LA    RF,DBLOCKA                                                       
         LH    R1,DBFACTOR         MULTIPLY BY 15                               
         MH    R1,=H'15'                                                        
PRDUR8   LA    RF,DBLOCKA                                                       
         STH   R1,DBFACTOR                                                      
PRDUR9   LA    RF,DBLOCKA                                                       
         LH    RE,DBFACTOR                                                      
         LR    R0,RE                                                            
         A     RE,0(R5)                                                         
         ST    RE,0(R5)            WEIGHT HALF HOUR                             
         LR    RE,R0                                                            
         A     RE,TPSAVG                                                        
         ST    RE,TPSAVG           WEIGHT TOTAL                                 
         LR    RE,R0                                                            
         L     RF,DUB                                                           
         MR    RE,RE                                                            
         LR    RE,RF                                                            
         A     RE,TPSAVG+4                                                      
         ST    RE,TPSAVG+4         RATING TOTAL                                 
         A     RF,4(R5)                                                         
         ST    RF,4(R5)            RATING HALF HOUR                             
         LR    RE,R0                                                            
         L     RF,DUB+4                                                         
         MR    RE,RE                                                            
         LR    RE,RF                                                            
         A     RE,TPSAVG+8                                                      
         ST    RE,TPSAVG+8         PUT TOTAL                                    
         A     RF,8(R5)                                                         
         ST    RF,8(R5)            PUT HALF HOUR                                
         B     XIT                                                              
         DROP  RF                                                               
         DROP  R1                                                               
         EJECT                                                                  
*              PRINTING AIDS                                                    
         SPACE 3                                                                
SPLAT    NTR1                                                                   
         BAS   RE,ALLSTARS                                                      
         CLI   FIRST,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,CLEARSUM                                                      
         MVI   FIRST,C'N'                                                       
         LA    R2,BUFF                                                          
         ZIC   R3,NUMDEMS                                                       
         LA    R3,1(R3)                                                         
         SRL   R3,1                                                             
         LA    R3,1(R3)                                                         
         GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 2                                                                
SPLAT2   MVC   P,0(R2)                                                          
         BAS   RE,SUMSTARS                                                      
         GOTO1 SPOOL,PARAS,(R8)                                                 
         LA    R2,132(R2)                                                       
         BCT   R3,SPLAT2                                                        
         B     XIT                                                              
         SPACE 2                                                                
ALLSTARS NTR1                                                                   
         LA    R2,P                                                             
         A     R2,DETDISP                                                       
         MVC   0(14,R2),STARS                                                   
         LA    R2,14(R2)                                                        
         ZIC   R3,NUMNETS                                                       
         SPACE 2                                                                
ALL2     MVC   0(19,R2),STARS                                                   
         LA    R2,19(R2)                                                        
         BCT   R3,ALL2                                                          
         B     XIT                                                              
         SPACE 2                                                                
SUMSTARS NTR1                                                                   
         LA    R2,P+14                                                          
         A     R2,DETDISP                                                       
         ZIC   R3,NUMNETS                                                       
         LA    R1,P                                                             
         A     R1,DETDISP                                                       
         MVI   0(R1),C'*'                                                       
         MVI   13(R1),C'*'                                                      
         SPACE 2                                                                
SUM2     MVI   18(R2),C'*'                                                      
         LA    R2,19(R2)                                                        
         BCT   R3,SUM2                                                          
         B     XIT                                                              
         SPACE 2                                                                
CLEARSUM NTR1                                                                   
         LA    R2,BUFF+14                                                       
         A     R2,DETDISP                                                       
         LA    R3,P+14                                                          
         A     R3,DETDISP                                                       
         ZIC   R4,NUMNETS                                                       
         SPACE 2                                                                
CLEAR2   CLC   0(18,R2),SPACES     IF A PROGRAMS MISSING                        
         BNE   *+10                                                             
         MVC   0(18,R3),SPACES     TAKE OUT PRECEDING STARS                     
         LA    R2,19(R2)                                                        
         LA    R3,19(R3)                                                        
         BCT   R4,CLEAR2                                                        
         B     XIT                                                              
         SPACE 2                                                                
STARS    DC    20C'*'                                                           
DAYALPH  DC    C'M-FMONTUEWEDTHUFRISATSUNM-S'                                   
         SPACE 2                                                                
DAYLIST  DC    X'7C402010080402017F8003'                                        
         EJECT                                                                  
*              HOOK ROUTINES FOR HEADLINES                                      
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H1+30(24),=C'NATIONAL NIELSEN RATINGS'                           
         MVC   H3+30(08),=C'WEEK OF '                                           
         MVC   H3+39(08),GRDBBK                                                 
         LA    R2,GRDTPBKH                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   H4+30(08),=C'TP START'                                           
         MVC   H4+39(08),GRDTPBK                                                
         LA    R2,GRDPRBKH                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   H5+30(08),=C'PR START'                                           
         MVC   H5+39(08),GRDPRBK                                                
*                                                                               
         MVC   H3(4),=C'DEMO'                                                   
         MVC   H3+10(8),GRDDEMO                                                 
         MVC   H4(3),=C'DAY'                                                    
         MVC   H4+10(8),GRDDAY                                                  
         MVC   H6(132),SVH6                                                     
         MVC   H6+8(8),=C' AVERAGE'                                             
         MVC   H7+8(8),=C' RTG SHR'                                             
         MVC   H7(3),=C'NET'                                                    
         MVC   H7+4(4),=C'DATA'                                                 
         LA    RF,H6+17                                                         
*        MVC   0(19,RF),=C'------08:00 PM-----'                                 
         MVC   132(13,RF),=C'RTG       SHR'                                     
         LA    RF,18(RF)                                                        
*        MVC   0(19,RF),=C'------08:30 PM-----'                                 
         MVC   132(13,RF),=C'RTG       SHR'                                     
         LA    RF,18(RF)                                                        
*        MVC   0(19,RF),=C'------09:00 PM-----'                                 
         MVC   132(13,RF),=C'RTG       SHR'                                     
         LA    RF,18(RF)                                                        
*        MVC   0(19,RF),=C'------09:30 PM-----'                                 
         MVC   132(13,RF),=C'RTG       SHR'                                     
         LA    RF,18(RF)                                                        
*        MVC   0(19,RF),=C'------10:00 PM-----'                                 
         MVC   132(13,RF),=C'RTG       SHR'                                     
         LA    RF,18(RF)                                                        
*        MVC   0(19,RF),=C'------10:30 PM-----'                                 
         MVC   132(13,RF),=C'RTG       SHR'                                     
         B     XIT                                                              
         SPACE 1                                                                
HOOKX    GOTO1 VRESHEAD                                                         
         B     XIT                                                              
         SPACE 2                                                                
DASHES   DC    40C'-'                                                           
         EJECT                                                                  
*              COMMON ROUTINES                                                  
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
ERREND   GOTO1 VERRXIT                                                          
         B     XIT                                                              
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         EJECT                                                                  
*                                                                               
*              LITERAL POOL AND CONSTANTS                                       
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 1                                                                
*                                  MY OWN ERROR MESSAGES                        
         SPACE 1                                                                
MANYBKS  DC    C'* ERROR * TOO MANY BOOKS - LIMIT IS 8'                         
MANYDEM  DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 1'                         
TOOBIG   DC    C'* ERROR * PLEASE SHORTEN REQUEST OR RUN OVERNIGHT'             
PAVONLY  DC    C'* ERROR * PAV OPTION ONLY'                                     
NOFOUND  DC    C'* ERROR * RECORD NOT FOUND - '                                 
OPTERR   DC    C'* ERROR * INVALID OPTION'                                      
INVBOK   DC    C'* ERROR * INVALID BOOK'                                        
INVDAY   DC    C'* ERROR * INVALID DAY'                                         
MAXTIM   DC    C'* ERROR * MAXIMUM TIME IS 3 HOURS'                             
         SPACE 2                                                                
DEMOSET  DS    0CL9                                                             
         DC    X'00',C'R',X'01',X'00',C'S',X'01',X'FFFFFF'                      
         SPACE 3                                                                
*                                  REPORT HEADLINE SPECS                        
         SPACE 1                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V'                                    
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,NETREP                                                     
         SSPEC H5,77,PAGE                                                       
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER RECORDS IN BUFF                                   
         SPACE 3                                                                
BUFFD    DSECT                                                                  
BUFFREC  DS    0CL220                                                           
BUFFBK   DS    CL4                                                              
BUFFPROG DS    6CL20        16 CHAR PROG NAME, 4 CHAR WEEK INDICATOR            
BUFFDEM  DS    6CL16               8 DEMOS, 2 BYTES EACH                        
         SPACE 2                                                                
*              SPGENPROG HERE                                                   
*              NERESALL                                                         
         PRINT OFF                                                              
       ++INCLUDE SPGENPROGA                                                     
       ++INCLUDE NERESALL1                                                      
         PRINT ON                                                               
         EJECT                                                                  
*              DSECT TO COVER SCREEN                                            
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NERESF9D                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
*              LOCAL WORKING STORAGE                                            
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
SPACOPT  DS    XL1                                                              
STARTNUM DS    XL1                                                              
ENDNUM   DS    XL1                                                              
SAVSTART DS    XL1                                                              
FIRST    DS    CL1                                                              
QHNUM    DS    CL1                                                              
TPSBK    DS    CL2                                                              
PRSBK    DS    CL2                                                              
COST     DS    F                                                                
DETDISP  DS    F                                                                
MILST    DS    H                                                                
MILEND   DS    H                                                                
BYTEA    DS    C                                                                
BYTEB    DS    C                                                                
P5       DS    CL132                                                            
SVH6     DS    CL132                                                            
DEMOS2   DS    CL20                                                             
MYNETS   DS    CL91                ROOM FOR 21 NETS                             
         DS    0F                                                               
TPAVG    DS    CL16                                                             
TPSAVG   DS    CL16                                                             
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         SPACE 1                                                                
PRBD     DSECT                                                                  
PRBNTI   DS    CL2                                                              
PRBTIME  DS    CL7                                                              
         ORG   PRBTIME                                                          
PRBQHS   DS    C                                                                
PRBQHE   DS    C                                                                
PRBMILS  DS    CL2                                                              
PRBMILE  DS    CL2                                                              
PRBQHDUR DS    C                                                                
PRBPREM  DS    C                                                                
PRBPROG  DS    CL25                                                             
PRBEPIS  DS    CL25                                                             
PRBSCNT  DS    XL2                                                              
PRBCOVER DS    XL2                                                              
PRBTYPE  DS    XL4                                                              
PRBD1    DS    XL4                 PROGRAM DEMOS                                
PRBD2    DS    XL12                PROG STD DEMOS (WEIGHT DEMO1 DEMO2)          
PRBLEN   EQU   *-PRBNTI                                                         
PRBHLEN  EQU   48                                                               
*              DDCOMFACTS, FAFACTS, & NEGETNUND                                 
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE NEGETNUND                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'175NERES20   05/01/02'                                      
         END                                                                    
