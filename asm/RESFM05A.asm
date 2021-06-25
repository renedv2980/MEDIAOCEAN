*          DATA SET RESFM05A   AT LEVEL 038 AS OF 05/01/02                      
*PHASE T81805B,*                                                                
*INCLUDE BINSRCH                                                                
         TITLE 'T81805 - DAYPART RECORD'                                        
**********************************************************************          
*                                                                    *          
*        RESFM05 (T81805) --- DAYPART RECORD                         *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* ???????  (???) --- HISTORY LOST                                    *          
*                                                                    *          
* 08OCT90  (EFJ) --- TOMBSTONE ADDED, PHASE CARD CHANGED TO 'A'      *          
*                                                                    *          
* 11AUG95  (BU ) --- ADD 'CM' TO MEDIA TABLE                         *          
*                                                                    *          
* 03OCT96  (SEP) --- ALLOW LOW POWER TV STATION ENTRY                *          
*                                                                    *          
* 20APR99  (AST) --- SUMMARY DAYPART FOR AUR                         *          
*                                                                    *          
* 03SEP99  (BU ) --- RESET START TIME VALIDATION TO 6A               *          
*         NOTE:  THIS VERSION DOESN'T WORK!!!  THE FINAL VALIDATION  *          
*             IS TOTALLY SCREWED UP, AND MUST BE REWRITTEN WHEN      *          
*             AND IF SOMEONE CAN FIGURE OUT WHAT THE HELL IT'S       *          
*             ACTUALLY SUPPOSED TO DO!!  BILL - SEP 3/99             *          
*                                                                    *          
*                    ***  END TOMBSTONE  ***                         *          
**********************************************************************          
T81805   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1805**,RR=R2                                                 
         L     RC,0(R1)                                                         
         ST    R2,RELO                                                          
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE AND DISPLAY KEY                                         
         SPACE 3                                                                
VKEY     DS    0H                                                               
         LA    R2,SDDSTAH                                                       
         LA    R4,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         USING RSDDKEY,R4                                                       
         MVI   RSDDKTYP,X'26'                                                   
         MVC   RSDDKREP,AGENCY                                                  
         CLI   ACTNUM,ACTLIST      IF ACTION LIST OR                            
         BE    VK20                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT, THEN                          
         BNE   *+12                                                             
VK20     CLI   5(R2),0             OK IF BLANK                                  
         BE    VK200                                                            
*                                                                               
         MVC   RSDDKSTA(4),=X'FFFFFFFF' DEFAULT STATION                         
         CLI   5(R2),2             AT LEAST 3 CHAR FOR DEFAULT                  
         BL    VK100                                                            
         BNE   *+18                                                             
         MVI   RSDDKSTA+4,C' '                                                  
         CLC   8(2,R2),=C'TV'                                                   
         BE    VK200                                                            
         MVI   RSDDKSTA+4,C'L'                                                  
         CLI   8(R2),C'L'                                                       
         BE    VK200                                                            
         MVI   RSDDKSTA+4,C'R'                                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,CLCRAD                                                        
         BE    VK200                                                            
*                                                                               
VK100    GOTO1 VALISTA                                                          
         MVC   RSDDKSTA,WORK                                                    
*                                                                               
VK200    MVC   KEY,SVKEY                                                        
VKEXT    B     XIT                                                              
CLCRAD   CLC   8(0,R2),=C'RADIO'                                                
         EJECT                                                                  
*                                  DISPLAY KEY                                  
DKEY     DS    0H                                                               
         LA    R4,KEY                                                           
         LA    R2,SDDSTAH          DISPLAY STATION                              
         OI    6(R2),X'80'                                                      
         XC    SDDSTA,SDDSTA                                                    
         CLC   =F'-1',RSDDKSTA                                                  
         BNE   DK100                                                            
         MVC   8(2,R2),=C'TV'                                                   
         CLI   RSDDKSTA+4,C' '                                                  
         BE    XIT                                                              
         MVI   8(R2),C'L'                                                       
         CLI   RSDDKSTA+4,C'L'                                                  
         BE    XIT                                                              
         MVC   8(5,R2),=C'RADIO'                                                
         B     XIT                                                              
*                                                                               
DK100    MVC   8(4,R2),RSDDKSTA                                                 
         LA    RE,11(R2)                                                        
         CLI   0(RE),C'L'          LOW POWER TV                                 
         BE    DK150                                                            
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
DK150    MVI   0(RE),C'-'                                                       
         LA    RF,MEDTBL                                                        
         LA    R0,5                                                             
DK200    CLC   RSDDKSTA+4(1),0(RF)                                              
         BE    DK220                                                            
         LA    RF,3(RF)                                                         
         BCT   R0,DK200                                                         
         DC    H'0'                                                             
DK220    MVC   1(2,RE),1(RF)                                                    
*                                                                               
         B     XIT                                                              
*                                                                               
MEDTBL   DC    CL3' TV'                                                         
         DC    CL3'LL '                                                         
         DC    CL3'AAM'                                                         
         DC    CL3'FFM'                                                         
         DC    CL3'CCM'                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                  VALIDATE AND DISPLAY RECORD                  
VREC     DS    0H                                                               
         L     R6,AIO1                                                          
         USING RSDDRECD,R6                                                      
         LA    R2,SDDDPTH                                                       
         GOTO1 ANY                                                              
         MVC   RSDDCODE(2),=X'0108'                                             
*                                                                               
         MVI   ELCODE,X'02'                                                     
         GOTO1 HELLO,DMCB,(C'D',=C'REPFIL  '),(ELCODE,0(R6)),0                  
*                                                                               
* BUILD ELEMENTS FOR SECONDARY DAYPARTS & DAY/TIMES                             
* ALSO BUILD A LIST OF DPT DY/TMS TO DO ERROR CHECKING - R4                     
         LA    R5,16                                                            
         LA    R4,WORKTBL                                                       
         LR    RE,R4                                                            
         LA    RF,1682                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   NXELMN,1                                                         
VR100    CLI   5(R2),0                                                          
         BNE   VR200                                                            
         LA    R0,6                3 DAYS - 3 TIMES PER LINE                    
VR120    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CLI   5(R2),0                                                          
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         BCT   R0,VR120                                                         
         ZIC   R0,0(R2)            POINT NEXT DAYART                            
         AR    R2,R0                                                            
         B     VR320                                                            
*                                                                               
VR200    XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'0206'                                                 
         GOTO1 VALIDPT                                                          
         MVC   0(2,R4),8(R2)                                                    
         LR    R3,R4                                                            
         LA    R4,3(R4)                                                         
         MVC   ELEM+3(2),8(R2)                                                  
         LA    R7,ELEM+6           START OF DAY/TIMES IN ELEMENT                
*                                                                               
VR220    LA    R8,3                                                             
         ZIC   R0,0(R2)            POINT TO 1ST DAY - MUST BE 1 IF DPT          
         AR    R2,R0                                                            
         GOTO1 ANY                                                              
VR240    CLI   5(R2),0                                                          
         BNE   VR260                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BE    VR300                                                            
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR260    ZIC   R0,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R0),8(R2)),HALF,WORK                               
         CLI   HALF,0              TEST FOR ERROR                               
         BNE   *+12                NO                                           
         MVI   ERROR,INVDAY                                                     
         B     TRAPERR                                                          
         MVC   0(1,R7),HALF                                                     
         MVC   0(1,R4),HALF                                                     
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ICM   R0,1,5(R2)                                                       
         BZ    TIMERRTN                                                         
         GOTO1 TIMVAL,DMCB,((R0),8(R2)),FULL                                    
         CLI   0(R1),X'FF'         TEST FOR ERROR                               
         BE    TIMERRTN                                                         
         CLC   FULL,=C'NONE'       SCREEN OUT SPECIAL CHARS.                    
         BE    TIMERRTN                                                         
         CLC   FULL,=C'VARY'                                                    
         BNE   *+12                                                             
TIMERRTN MVI   ERROR,INVTIME                                                    
         B     TRAPERR                                                          
         CLC   FULL+2(2),=C'CC'    MAKE CONCLUSSION 2A                          
         BNE   *+10                                                             
         MVC   FULL+2(2),=H'200'                                                
         MVC   1(4,R7),FULL                                                     
         MVC   1(4,R4),FULL                                                     
*                                                                               
* SET TABLE FOR DAY/TIME CHECKS                                                 
         ICM   RE,3,3(R4)          IF NO END DATE SAVE START                    
         BNZ   *+10                                                             
         MVC   3(2,R4),FULL                                                     
         LA    R4,5(R4)                                                         
         LA    R7,5(R7)                                                         
         ZIC   RE,ELEM+5           COUNT OF DAY/TIMES                           
         LA    RE,1(RE)                                                         
         STC   RE,ELEM+5                                                        
         IC    RE,ELEM+1           ELEMENT LENGTH                               
         LA    RE,5(RE)                                                         
         STC   RE,ELEM+1                                                        
*                                                                               
VR300    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R8,VR240                                                         
*                                                                               
         CH    R5,=H'1'                                                         
         BE    *+14                                                             
         CLC   ELEM+3(2),8(R2)                                                  
         BNE   *+8                                                              
         BCT   R5,VR220                                                         
         MVC   2(1,R3),ELEM+5      # DAYTIMES                                   
         ZIC   RE,NXELMN                                                        
         STC   RE,ELEM+2                                                        
         LA    RE,1(RE)                                                         
         STC   RE,NXELMN                                                        
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL  '),0(R6),ELEM                        
         LTR   R5,R5                                                            
         BZ    *+8                                                              
VR320    BCT   R5,VR100                                                         
*                                                                               
* CHECK FOR DUPLICATE DAYPART                                                   
VR400    LA    R6,WORKTBL                                                       
VR420    LR    R3,R6                                                            
VR440    ZIC   RE,2(R3)                                                         
         MH    RE,=H'5'                                                         
         AR    R3,RE                                                            
         LA    R3,3(R3)                                                         
         CLI   0(R3),0                                                          
         BE    VR460               NEXT DAYPART                                 
         CLC   0(2,R6),0(R3)       DUP DAYPART                                  
         BNE   VR440                                                            
         MVC   CONHEAD(7),=C'DAYPART'                                           
         MVC   CONHEAD+8(2),0(R6)                                               
         MVC   CONHEAD+11(14),=C'ALREADY EXISTS'                                
         B     MYERR                                                            
*                                                                               
VR460    ZIC   RE,2(R6)                                                         
         MH    RE,=H'5'                                                         
         AR    R6,RE                                                            
         LA    R6,3(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   VR420                                                            
*                                                                               
* BUILD A SORTED LIST BY INDIVIDUAL DAY & TIME (BINSRCH)                        
VR500    L     R6,AIO1                                                          
         LA    RE,WORKTBL                                                       
         LA    RF,1682                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         XC    WORK,WORK                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VR520    BAS   RE,NEXTEL                                                        
         BNE   VR700                                                            
*                                                                               
* PUT SINGLE DAY/TIME RECORDS INTO A TABLE IN ORDER                             
         ZIC   R5,5(R6)            # DAY/TIMES THIS ELEMENT                     
         LA    R2,6(R6)            POINT TO 1ST TIME                            
VR540    LA    R3,X'40'                                                         
         LA    R4,1                STORE MONDAY AS 1 FOR SORT                   
VR560    ZIC   RE,0(R2)            BUILD DAY/TIME TABLE                         
         NR    RE,R3               IS THIS DAY PART OF THIS ELEMENT             
         BZ    VR580                                                            
         STC   R4,WORK             YES - ADD A DAY/TIME RECORD                  
         MVC   WORK+1(4),1(R2)                                                  
         ICM   RF,3,WORK+3                                                      
         BNZ   *+10                                                             
         MVC   WORK+3(2),WORK+1                                                 
         SR    RE,RE               AM TIMES BEFORE 6AM ARE UPDATED              
*                                                                               
*   DON'T CHANGE TIMES PRIOR TO 6AM FOR START TIME!!!                           
*                                                                               
***      CLC   =H'600',WORK+1                                                   
***      BNH   *+16                                                             
***      ICM   RE,3,WORK+1                                                      
***      AH    RE,=H'2400'                                                      
***      STCM  RE,3,WORK+1                                                      
         CLC   =H'600',WORK+3                                                   
         BNH   *+16                                                             
         ICM   RE,3,WORK+3                                                      
         AH    RE,=H'2400'                                                      
         STCM  RE,3,WORK+3                                                      
*                                                                               
         LA    R7,WORKTBL                                                       
         MVC   DMCB+12(4),=F'5'                                                 
         MVC   DMCB+16(4),=F'5'                                                 
         MVC   DMCB+20(4),=F'336'                                               
         ICM   R0,15,WORK+10                                                    
         GOTO1 =V(BINSRCH),DMCB,(1,WORK),(R7),(R0),RR=RELO                      
         MVC   WORK+10(4),DMCB+8                                                
VR580    AH    R4,=H'1'                                                         
         SRA   R3,1                                                             
         BNZ   VR560                                                            
         LA    R2,5(R2)                                                         
         BCT   R5,VR540                                                         
*&&DO                                                                           
*                                                                               
* CHECK FOR OVERLAP                                                             
         ZIC   R5,5(R6)            # DAY/TIMES THIS ELEMENT                     
         LA    R2,6(R6)            POINT TO 1ST TIME                            
VR600    LR    R7,R6                                                            
         MVC   FULL,1(R2)                                                       
         SR    RE,RE               AM TIMES BEFORE 6AM ARE UPDATED              
         CLC   =H'600',FULL                                                     
         BNH   *+16                                                             
         ICM   RE,3,FULL                                                        
         AH    RE,=H'2400'                                                      
         STCM  RE,3,FULL                                                        
         CLC   =H'600',FULL+2                                                   
         BNH   *+16                                                             
         ICM   RE,3,FULL+2                                                      
         AH    RE,=H'2400'                                                      
         STCM  RE,3,FULL+2                                                      
         ZIC   RE,1(R7)                                                         
         AR    R7,RE                                                            
VR620    CLI   0(R7),X'02'                                                      
         BNE   VR680                                                            
*                                                                               
*** NEW FOR AUR                                                                 
         XC    DPTS,DPTS                                                        
         MVC   DPTS,3(R7)          GET DAYPARTS FROM ELEM FOR CMPARE            
***                                                                             
         ZIC   R0,5(R7)                                                         
         LA    R7,6(R7)                                                         
VR640    ZIC   RE,0(R2)                                                         
         EX    RE,DAYTEST                                                       
         BZ    VR660                                                            
         MVC   WORK(4),1(R7)                                                    
         SR    RE,RE               AM TIMES BEFORE 6AM ARE UPDATED              
         CLC   =H'600',WORK                                                     
         BNH   *+16                                                             
         ICM   RE,3,WORK                                                        
         AH    RE,=H'2400'                                                      
         STCM  RE,3,WORK                                                        
         CLC   =H'600',WORK+2                                                   
         BNH   *+16                                                             
         ICM   RE,3,WORK+2                                                      
         AH    RE,=H'2400'                                                      
         STCM  RE,3,WORK+2                                                      
         CLC   FULL+2(2),WORK      END BEFORE START                             
         BNH   VR660                                                            
         CLC   FULL(2),WORK+2      START AFTER END                              
         BNL   VR660                                                            
*                                                                               
*** NEW FOR AUR                                                                 
         CLC   3(1,R6),DPTS        SAME FIRST DAYPART?                          
         BE    VR660               YES, SKIP OVERLAP                            
*        BNE   VR650               NO, OVERLAP ERROR                            
*        CLC   4(1,R6),DPTS+1      SEC DAYPART?                                 
*        BNE   VR660               DIFFERENT SEC DPT, SKIP OVERLAP              
***                                                                             
VR650    MVC   CONHEAD(7),=C'DAYPART'                                           
         MVC   CONHEAD+8(2),3(R6)                                               
         MVC   CONHEAD+11(21),=C'HAS OVERLAPPING TIMES'                         
         B     MYERR               DONE THIS ELEM - GET NEXT                    
*                                                                               
VR660    LA    R7,5(R7)                                                         
         BCT   R0,VR640                                                         
         B     VR620                                                            
*                                                                               
VR680    LA    R2,5(R2)                                                         
         BCT   R5,VR600                                                         
*&&                                                                             
         B     VR520                                                            
*                                                                               
* MAKE SURE DAY/TIMES COVER ALLDAYS M-SU/6-2AM - NO OVERLAPS EITHER             
VR700    MVI   WORK,0              1ST TIME INDICATOR                           
         LA    R3,WORKTBL                                                       
         LA    R1,1                                                             
         CLI   0(R3),1             1ST DAY MONDAY                               
         BNE   NODAYER                                                          
         B     VR760                                                            
*                                                                               
VR720    CLI   0(R3),0                                                          
         BE    VR730                                                            
*** NEW AUR CODE                                                                
         CLI   WORK,0                                                           
         BE    VR780                                                            
***                                                                             
         ZIC   R1,WORK                                                          
         CLC   0(1,R3),WORK        DAY BREAK                                    
         BE    VR780                                                            
*   TEST DUMP                                                                   
****>>>> MVC   VR720(2),=X'0000'                                                
*   TEST DUMP                                                                   
VR730    CLC   =H'2600',WORK+3                                                  
         B     VR740               NOW SKIPS END TIME CHECK                     
****     BNH   VR740                                                            
         BAS   RE,FINDDAY                                                       
         MVC   CONHEAD+4(24),=C'END TIMES MUST EQUAL 2AM'                       
MYERR    MVI   GENSTAT2,USMYOK                                                  
         LA    R2,SDDDPTH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
VR740    CLI   0(R3),0                                                          
         BE    VR900                                                            
         LA    R1,1(R1)                                                         
         ZIC   RE,0(R3)                                                         
         CR    RE,R1                                                            
         BE    VR760                                                            
NODAYER  BAS   RE,FINDDAY                                                       
         MVC   CONHEAD+4(12),=C'HAS NO TIMES'                                   
         B     MYERR                                                            
*                                                                               
VR760    CLC   =H'600',1(R3)                                                    
         BE    VR800                                                            
         ZIC   R1,0(R3)                                                         
         BAS   RE,FINDDAY                                                       
         MVC   CONHEAD+4(25),=C'START TIME MUST EQUAL 6AM'                      
         B     MYERR                                                            
*                                                                               
VR780    CLI   WORK,0                                                           
         BZ    VR800                                                            
         CLC   1(2,R3),WORK+3      OLD END = NEW START OK                       
         BE    VR800                                                            
         SR    RE,RE               ALSO OK OLD END 759/NEW START 800            
         SR    RF,RF                                                            
         ICM   RF,3,WORK+3                                                      
         LA    RF,1(RF)                                                         
         LR    R1,RF                                                            
         D     RE,=F'100'          CHECK FOR END OF AN HOUR                     
         CH    RE,=H'60'                                                        
         BNE   *+8                                                              
         AH    R1,=H'40'                                                        
         LR    RF,R1                                                            
*                                                                               
*** NEW CODE FOR AUR                                                            
****     CLC   =X'02580A28',1(R3)      START TIME 6AM? END TIME 2AM?            
****     BE    VR800                                                            
         B     VR800               NOW, ALWAYS SKIP CODE BELOW                  
***                                                                             
         SR    RE,RE                                                            
         ICM   RE,3,1(R3)                                                       
         CR    RE,RF                                                            
         BE    VR800                                                            
         ZIC   R1,0(R3)                                                         
         BAS   RE,FINDDAY                                                       
         MVC   CONHEAD+4(20),=C'.IME NOT CONSECUTIVE'                           
         MVC   WORK+5(2),1(R3)                                                  
         SR    RE,RE               AM TIMES BEFORE 6AM ARE UPDATED              
         CLC   =H'2400',WORK+3                                                  
         BNL   *+16                                                             
         ICM   RE,3,WORK+3                                                      
         SH    RE,=H'2400'                                                      
         STCM  RE,3,WORK+3                                                      
         CLC   =H'2400',WORK+5                                                  
         BNL   *+16                                                             
         ICM   RE,3,WORK+5                                                      
         SH    RE,=H'2400'                                                      
         STCM  RE,3,WORK+5                                                      
         GOTO1 UNTIME,DMCB,WORK+3,CONHEAD+28                                    
         B     MYERR                                                            
*                                                                               
VR800    MVC   WORK(5),0(R3)                                                    
*                                                                               
*** NEW CODE FOR AUR                                                            
         CLC   =X'02580A28',1(R3)      START TIME 6AM? END TIME 2AM?            
         BNE   *+10                                                             
         XC    WORK,WORK           A SUMMARY DPT, SO CLEAR WORK                 
***                                                                             
         CLC   1(2,R3),=H'600'     START TIME PRE-6AM?                          
         BNL   VR820               NO  - BUMP TO NEXT ENTRY                     
         ZIC   R1,0(R3)                                                         
         BAS   RE,FINDDAY                                                       
         MVC   CONHEAD+4(25),=C'START TIME MUST.EQUAL 6AM'                      
         B     MYERR                                                            
VR820    EQU   *                                                                
         LA    R3,5(R3)                                                         
         B     VR720                                                            
*                                                                               
VR900    ZIC   R1,WORK             MAKE SURE LAST DAY FRIDAY                    
         LA    R1,1(R1)                                                         
         CLI   WORK,7                                                           
         BNE   NODAYER                                                          
*                                                                               
* RE-READ KEY SO GENCON DOESN'T BLOW UP                                         
VR1000   CLI   ACTNUM,ACTADD                                                    
         BE    VR1020                                                           
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(27),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DMINBTS,X'80'                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'REPFIL  ',         X        
               KEY+28,AIO2,DMWORK                                               
VR1020   MVC   AIO,AIO1                                                         
VREXT    B     XIT                                                              
DAYTEST  TM    0(R7),0                                                          
         EJECT                                                                  
* FIND DAY                                                                      
FINDDAY  MH    R1,=H'3'                                                         
         LA    R1,DAYTBL-3(R1)                                                  
         MVC   CONHEAD(3),0(R1)                                                 
         BR    RE                                                               
         SPACE 2                                                                
DAYTBL   DC    CL3'MON'                                                         
         DC    CL3'TUE'                                                         
         DC    CL3'WED'                                                         
         DC    CL3'THU'                                                         
         DC    CL3'FRI'                                                         
         DC    CL3'SAT'                                                         
         DC    CL3'SUN'                                                         
         EJECT                                                                  
*                                  DISPLAY RECORD                               
DREC     DS    0H                                                               
         L     R6,AIO1                                                          
         LA    R2,SDDDPTH                                                       
         BAS   RE,CLRSCRN                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR100    BAS   RE,NEXTEL                                                        
         BNE   DREXT                                                            
         ZIC   R5,5(R6)                                                         
         LA    R4,6(R6)                                                         
         SR    R3,R3               SET R3 TO OUTPUT DAYPART                     
DR200    LTR   R3,R3                                                            
         BNZ   DR220                                                            
         LA    R3,3                                                             
         MVC   8(2,R2),3(R6)                                                    
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
DR220    GOTO1 UNDAY,DMCB,0(R4),8(R2)                                           
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         GOTO1 UNTIME,DMCB,1(R4),8(R2)                                          
         OI    6(R2),X'80'                                                      
         LA    R4,5(R4)                                                         
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCTR  R3,0                                                             
         BCT   R5,DR200                                                         
*                                                                               
         LTR   R3,R3               INCREMENT SCREEN IF NEED BE                  
         BZ    DR100                                                            
         SLL   R3,1                MULTIPLY BY 2                                
DR300    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R3,DR300                                                         
         B     DR100                                                            
*                                                                               
DREXT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                  LIST RECORDS                                 
         SPACE 3                                                                
LIST     DS    0H                                                               
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         LA    R2,LSDSELH                                                       
         BAS   RE,CLRSCRN                                                       
         LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LS220                                                            
         USING RSDDKEY,R4                                                       
         MVI   RSDDKTYP,X'26'                                                   
         MVC   RSDDKREP,AGENCY     REP                                          
         MVC   RSDDKSTA(5),SVKEY+22 STATION                                     
*                                                                               
         GOTO1 HIGH                                                             
         B     LS220                                                            
         DROP  R4                                                               
*                                                                               
LS200    GOTO1 SEQ                                                              
LS220    CLC   KEY(22),KEYSAVE     CHECKMAIN C/B                                
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES       SHOW SCREEN                                  
         LA    R3,LISTAR                                                        
         USING LISTD,R3                                                         
         L     R6,AIO                                                           
         USING RSDDRECD,R6                                                      
         CLC   =F'-1',RSDDKSTA                                                  
         BNE   LS240                                                            
         MVC   LISTSTA(2),=C'TV'                                                
         CLI   RSDDKSTA+4,C' '                                                  
         BE    LS300                                                            
         MVC   LISTSTA(2),=C'L '                                                
         CLI   RSDDKSTA+4,C'L'                                                  
         BE    LS300                                                            
         MVC   LISTSTA(5),=C'RADIO'                                             
         B     LS300                                                            
*                                                                               
LS240    LA    R4,LISTSTA                                                       
         MVC   0(4,R4),RSDDKSTA                                                 
         LA    RE,3(R4)                                                         
         CLI   0(RE),C' '                                                       
         BE    LS250                                                            
         CLI   0(RE),C'L'                                                       
         BE    LS250                                                            
         LA    RE,1(RE)                                                         
LS250    MVI   0(RE),C'-'                                                       
         LA    RF,MEDTBL                                                        
         LA    R0,5                                                             
LS260    CLC   RSDDKSTA+4(1),0(RF)                                              
         BE    LS280                                                            
         LA    RF,3(RF)                                                         
         BCT   R0,LS260                                                         
         DC    H'0'                                                             
LS280    MVC   1(2,RE),1(RF)                                                    
*                                                                               
* EDIT DAY TIME TO WORK THAN FORMAT LINE TO FIT ON SCREEN                       
LS300    LA    R3,LISTDPT                                                       
         SR    R4,R4                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LS320    BAS   RE,NEXTEL                                                        
         BNE   LS500                                                            
         LTR   R4,R4                                                            
         BZ    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         MVC   0(2,R3),3(R6)                                                    
         LA    R3,1(R3)                                                         
         CLI   0(R3),0                                                          
         BE    *+8                                                              
         LA    R3,1(R3)                                                         
         LR    R4,R6               NOT 1ST TIME                                 
         B     LS320                                                            
         SPACE                                                                  
LS500    MVC   DMDSKADD,KEY+28                                                  
         GOTO1 LISTMON                                                          
         B     LS200                                                            
         SPACE                                                                  
         DROP  R3,R6,R8                                                         
         EJECT                                                                  
*                                  PRINT REPORT                                 
PREP     DS    0H                                                               
*                                                                               
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R2,BUFF             CLEAR BUFF TO SPACES                         
         LA    R3,10                                                            
PR30     MVC   0(132,R2),SPACES                                                 
         LA    R2,132(R2)                                                       
         BCT   R3,PR30                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSDDKEY,R4                                                       
         MVI   RSDDKTYP,X'26'                                                   
         MVC   RSDDKREP,AGENCY     REP                                          
         MVC   RSDDKSTA,SVKEY+22   STATION, IF FILTERED ON                      
         GOTO1 HIGH                                                             
         B     PR50                                                             
         DROP  R4                                                               
         SPACE 1                                                                
PR40     GOTO1 SEQ                                                              
PR50     CLC   KEY(22),KEYSAVE     CHECK MAIN C/B                               
         BNE   XIT                                                              
         MVI   FORCEHED,C'Y'       NEW PAGE PER STATION                         
         SPACE 1                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO1                                                          
         LA    R2,BUFF                                                          
         USING PRINTD,R2                                                        
         SPACE 1                                                                
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     PR60                                                             
PR55     BAS   RE,NEXTEL                                                        
         BE    PR60                                                             
         OC    SVKEY+22(5),SVKEY+22                                             
         BNZ   XIT                 ONLY DO STATION REQUESTED                    
         B     PR40                DO ALL STATIONS                              
         SPACE 1                                                                
PR60     ZIC   R5,5(R6)            NUMBER OF DAY/TIMES                          
         MVC   PDPT,3(R6)                                                       
         LA    R4,6(R6)            DAY/TIMES                                    
PR65     LA    R7,PDAY                                                          
         LA    R3,3                                                             
PR70     GOTO1 UNDAY,DMCB,0(R4),0(R7)                                           
         GOTO1 UNTIME,DMCB,1(R4),11(R7)                                         
         LA    R4,5(R4)            NEXT DAY/TIME                                
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BZ    PR80                                                             
         LA    R7,31(R7)           PRINT ON SAME LINE                           
         BCT   R5,PR70                                                          
         B     PR90                                                             
*                                                                               
PR80     LA    R2,132(R2)          PRINT ON NEXT LINE                           
         BCT   R5,PR65                                                          
*                                                                               
PR90     BAS   RE,SPLAT                                                         
         LA    R2,BUFF                                                          
         B     PR55                                                             
         DROP  R2,R8                                                            
*                                                                               
*                                                                               
*                                                                               
*                                                                               
SPLAT    NTR1                                                                   
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         LA    R2,BUFF                                                          
         LA    R3,10                                                            
SPLAT20  CLC   0(132,R2),SPACES                                                 
         BE    SPLAT40                                                          
         MVC   P(132),0(R2)                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   0(132,R2),SPACES                                                 
         LA    R2,132(R2)                                                       
         BCT   R3,SPLAT20                                                       
         SPACE 1                                                                
SPLAT40  GOTO1 SPOOL,DMCB,(R8)     SPACING LINE                                 
         B     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,46,C'DAYPART DEFINITION RECORDS'                              
         SSPEC H2,46,C'--------------------------'                              
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         SSPEC H7,1,C'STATION -'                                                
         SSPEC H12,1,C'DAYPART'                                                 
         SSPEC H13,1,C'-------'                                                 
         SSPEC H12,13,C'DAY        TIME'                                        
         SSPEC H13,13,C'--------   -------------'                               
         SSPEC H12,44,C'DAY        TIME'                                        
         SSPEC H13,44,C'--------   -------------'                               
         SSPEC H12,75,C'DAY        TIME'                                        
         SSPEC H13,75,C'--------   -------------'                               
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         CLC   =F'-1',KEY+22       DEFAULT STATION                              
         BNE   HD5                                                              
         MVC   H7+10(10),=C'TV DEFAULT'                                         
         CLI   KEY+26,C' '                                                      
         BE    HDX                                                              
         CLI   KEY+26,C'L'                                                      
         BE    HDX                                                              
         MVC   H7+10(13),=C'RADIO DEFAULT'                                      
         B     HDX                                                              
         SPACE 1                                                                
HD5      MVC   H7+10(4),KEY+22     STATION                                      
         LA    R3,H7+13                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         MVC   1(2,R3),=C'-L'                                                   
         CLI   KEY+26,C'L'                                                      
         BE    HDX                                                              
         MVC   1(3,R3),=C'-TV'                                                  
         CLI   KEY+26,C' '                                                      
         BE    HDX                                                              
         MVC   2(2,R3),=C'AM'                                                   
         CLI   KEY+26,C'A'                                                      
         BE    HDX                                                              
         MVI   2(R3),C'F'                                                       
         CLI   KEY+26,C'F'                                                      
         BE    HDX                                                              
         MVI   2(R3),C'C'                                                       
HDX      B     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
CLRSCRN  NTR1                                                                   
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     XIT                                                              
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         DROP  R8                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
*                                                                               
*** NEW FOR AUR                                                                 
DPTS     DS    CL2                 DAYPART OF ONE '02' ELEM                     
***                                                                             
         LTORG                                                                  
WORKTBL  DS    1862C                                                            
         EJECT                                                                  
LISTD    DSECT                                                                  
LISTSTA  DS    CL7                                                              
         DS    CL1                                                              
LISTDPT  DS    CL48                                                             
         SPACE 4                                                                
PRINTD   DSECT                                                                  
         DS    0CL132                                                           
         DS    CL2                                                              
PDPT     DS    CL2                 DAYPART                                      
         DS    CL8                                                              
PDAY     DS    CL8                 DAY                                          
         DS    CL3                                                              
         DS    CL13                TIME                                         
         DS    CL7                                                              
         DS    CL8                 DAY                                          
         DS    CL3                                                              
         DS    CL13                TIME                                         
         DS    CL7                                                              
         DS    CL8                 DAY                                          
         DS    CL3                                                              
         DS    CL13                TIME                                         
         DS    CL34                                                             
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RESFMFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* RESFMF5D                                                                      
       ++INCLUDE RESFMF5D                                                       
         ORG   CONTAGH                                                          
* RESFME5D                                                                      
       ++INCLUDE RESFME5D                                                       
         EJECT                                                                  
* REGENSDD                                                                      
         EJECT                                                                  
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
* RESFMWORKD                                                                    
       ++INCLUDE RESFMWORKD                                                     
         ORG   SYSSPARE                                                         
SVKEY    DS    CL34                                                             
NXELMN   DS    XL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038RESFM05A  05/01/02'                                      
         END                                                                    
