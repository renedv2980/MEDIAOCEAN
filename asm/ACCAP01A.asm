*          DATA SET ACCAP01A   AT LEVEL 010 AS OF 03/07/03                      
*PHASE T61D01A                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE COVAIL                                                                 
*INCLUDE ACRAPPER                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP01 -- SALARY HISTORY MOVE/DELETE                *         
*                                                                     *         
*  COMMENTS:     MOVES AND/OR DELETES HISTORY RECORDS BASED ON PERSON *         
*                LOCATION ACTIONS (ENDING LOCATION OR CHANGING LOC'S) *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPD4                                      *         
*                                                                     *         
*  OUTPUTS:      UPDATED SALARY HISTORY RECS                          *         
*                UPDATED PERSON RECORD                                *         
*                UPDATED 1R RECORD(S)                                 *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- SYSSPARE                                       *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
* DCUR LVL 004 - USE GENERATED SAL LOCK DTE OR SAL LOCK DTE PASSED    *         
*                FROM PERSON PGM-WHICHEVER IS HIGHER TO CHK FOR SAL   *         
* DCUR LVL 005 - CHECK START DATE OF ENDING/DELETING LOC TO CHECK FOR *         
*                RIGHT LOCATION WHEN UPDATING PERSON RECORD           *         
* DCUR LVL 007 - FIXED BUG WHEN USER IS MOVING/DELETING SALARY % THE  *         
*                PC1/PC2 % NO LONGER EXISTS (IT'S SET AT ZERO)        *         
* DCUR LVL 009 - ADD UK ONLY CODE FOR BUILDING 1R NAMELD              *         
***********************************************************************         
         TITLE 'T61D01 - SALARY HISTORY RECORD DELETE/MOVE'                     
T61D01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D01**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         APPLICATION STORAGE AREA                     
         USING BLOCKSD,R5                                                       
         ST    R3,RELO                                                          
         ST    RC,SAVERC                                                        
*                                                                               
         GOTO1 =A(SETUP),DMCB,RR=RELO  ANY INITIALIZING                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
*                                                                               
* IF PGMCALL BIT IS NOT ON THAT MEANS THAT USER TYPED IN HISTORY DELETE         
* OR MOVE INSTEAD OF GOING THROUGH THE PERSON PROGRAM.                          
*                                                                               
VK       TM    PRGSTAT,PGMCALL                                                  
         BO    *+12                                                             
         LA    R2,CONACTH                                                       
         B     ERRACT                                                           
         MVI   BIT,0                                                            
         MVI   BIT2,0                                                           
         MVC   OFFICE,SPACES                                                    
         MVC   DEPT,SPACES                                                      
         MVC   SUBDPT,SPACES                                                    
         MVC   PERSON,SPACES                                                    
         MVC   ACCNT,SPACES                                                     
         MVC   OFFICE2,SPACES                                                   
         MVC   DEPT2,SPACES                                                     
         MVC   SUBDPT2,SPACES                                                   
         MVI   SEQNUM,0                                                         
         GOTO1 GETLDG,DMCB,C'1R'   GET LENGTHS                                  
         MVC   LEVELLN,ABCDLEN                                                  
*                                                                               
         LA    R2,CONOPTH          NO VALID OPTIONS                             
         CLI   CONOPTH+5,0                                                      
         BNE   EINVOPT                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE LOCATION BEING ENDED PASSED FROM PERSON PROGRAM.                     
* IF NO OFFICE/DEPT/SUBDEPT/PERSON THEN USER MANUALLY TYPED IN HISTORY          
* DELETE OR MOVE - NOT ALLOWED.  CAN ONLY USE THIS FROM THE PERSON              
* PROGRAM.                                                                      
***********************************************************************         
*                                                                               
VK10     LA    R2,HMVOFFH          OFFICE                                       
         CLI   5(R2),0             ANY DATA?                                    
         BNE   *+12                                                             
         LA    R2,CONACTH                                                       
         B     ERRACT                                                           
*                                                                               
         LA    R3,ACCNT                                                         
         MVC   ACCNT,SPACES                                                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,OFFICE,HMVOFF    SAVE OFFICE                                  
         OC    OFFICE,SPACES                                                    
         ZIC   R1,LEVELLNA         BUILD ACCOUNT IN ACCNT                       
         BCTR  R1,0                TO VALIDATE 1R                               
         EXMVC R1,0(R3),HMVOFF                                                  
         LA    R3,1(R1,R3)                                                      
*                                                                               
         LA    R2,HMVDEPTH                                                      
         CLI   5(R2),0                                                          
         BE    ERRINV                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,DEPT,HMVDEPT     SAVE DEPARTMENT                              
         OC    DEPT,SPACES                                                      
         ZIC   R1,LEVELLNB                                                      
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),HMVDEPT                                                 
         LA    R3,1(R1,R3)                                                      
*                                                                               
         LA    R2,HMVSDPTH                                                      
         CLI   5(R2),0                                                          
         BE    ERRINV                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,SUBDPT,HMVSDPT   SAVE SUB-DEPT                                
         OC    SUBDPT,SPACES                                                    
         ZIC   R1,LEVELLNC                                                      
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),HMVSDPT                                                 
         LA    R3,1(R1,R3)                                                      
*                                                                               
         LA    R2,HMVPERSH                                                      
         CLI   5(R2),0                                                          
         BE    ERRINV                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,PERSON,HMVPERS   SAVE PERSON                                  
         OC    PERSON,SPACES                                                    
         ZIC   R1,LEVELLND                                                      
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),HMVPERS                                                 
         OC    ACCNT,SPACES                                                     
         MVC   SVACCNT,ACCNT       SAVE OLD LOCATION ACCOUNT                    
*                                                                               
         LA    R2,HMVOFFH          CURSOR AT OFFICE ON ERROR                    
         GOTO1 =A(VALLOC),DMCB,RR=RELO  VALIDATE LOCATION GIVEN                 
         BNE   EINVLOCD                                                         
         EJECT                                                                  
***********************************************************************         
*        VALIDATE LOCATION END DATE                                             
***********************************************************************         
*                                                                               
         LA    R2,HMVEDTEH         LOCATION END DATE                            
         CLI   5(R2),0             MUST HAVE THIS                               
         BE    ERRINV                                                           
         XC    DMCB,DMCB                                                        
         LA    R1,HMVEDTE                                                       
         ST    R1,DMCB                                                          
         ZIC   R1,5(R2)                                                         
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         MVC   ENDATE,PVALPSTA                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SALARY LOCK DATE IF PASSED ON OLD LOCATION                    
***********************************************************************         
*                                                                               
         XC    SALDATE,SALDATE                                                  
         LA    R2,HMVSLDTH                                                      
         CLI   5(R2),0                                                          
         BE    VK15                                                             
         XC    DMCB,DMCB                                                        
         LA    R1,HMVSLDT                                                       
         ST    R1,DMCB                                                          
         ZIC   R1,5(R2)                                                         
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         MVC   SALDATE,PVALPSTA                                                 
         B     VK20                                                             
*                                                                               
VK15     LA    R2,HMVSLDWH         IF NO SALLOCK DATE HIDE 'SALLOC'             
         OI    1(R2),X'20'+X'0C'                                                
         LA    R2,HMVSLDTH                                                      
         OI    1(R2),X'20'+X'0C'                                                
*                                                                               
VK20     XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(2),=C'1R'   1R                                           
         MVC   ACTKACT,ACCNT       MOVE IN ACCOUNT                              
         LA    R2,HMVOFFH          CURSOR AT OFFICE ON ERROR                    
         GOTO1 TSTSEC,0            SECURITY CHECK OF 1R IN BIGKEY               
         EJECT                                                                  
***********************************************************************         
* VALIDATE NEW LOCATION PASSED FROM PERSON PROGRAM. (IF TRANSFERRING)           
* LOCATION PASSED IF MOVING SALARY (LOC SHOWS ON SCREEN) OR DELETING            
* SALARY (HIDE LOCATION).                                                       
* IF NO OFFICE MEANS ENDING A LOCATION SO PROTECT AND HIDE SOME FIELDS          
***********************************************************************         
*                                                                               
         LA    R2,HMVOFF2H         OFFICE                                       
         CLI   5(R2),0             ANY DATA                                     
         BNE   VK30                YES GO VALIDATE                              
         BAS   RE,PROTECT          PROTECT AND HIDE SOME VALUES                 
         OI    BIT,DELHIS                                                       
         B     VK96                                                             
*                                                                               
VK30     CLI   ACTEQU,ACTDEL       DELETING HISTORY WHILE TRANSFERRING          
         BNE   VK40                                                             
         LA    R2,HMVHEDWH         THEN DON'T SHOW 'AMT TO TRANS' COL           
         OI    1(R2),X'20'+X'0C'                                                
         BAS   RE,PROTECT          PROTECT AND HIDE 2ND LOCATION                
*        B     VK96                                                             
*                                                                               
VK40     LA    R2,HMVOFF2H         OFFICE                                       
         LA    R3,ACCNT                                                         
         MVC   ACCNT,SPACES                                                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,OFFICE2,HMVOFF2   SAVE OFFICE                                 
         OC    OFFICE2,SPACES                                                   
         ZIC   R1,LEVELLNA         BUILD ACCOUNT IN ACCNT                       
         BCTR  R1,0                TO VALIDATE 1R                               
         EXMVC R1,0(R3),HMVOFF2                                                 
         LA    R3,1(R1,R3)                                                      
*                                                                               
         LA    R2,HMVDPT2H                                                      
         CLI   5(R2),0                                                          
         BE    ERRINV                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,DEPT2,HMVDPT2     SAVE DEPARTMENT                             
         OC    DEPT2,SPACES                                                     
         ZIC   R1,LEVELLNB                                                      
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),HMVDPT2                                                 
         LA    R3,1(R1,R3)                                                      
*                                                                               
         LA    R2,HMVSDP2H                                                      
         CLI   5(R2),0                                                          
         BE    ERRINV                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,SUBDPT2,HMVSDP2   SAVE SUB-DEPT                               
         OC    SUBDPT2,SPACES                                                   
         ZIC   R1,LEVELLNC                                                      
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),HMVSDP2                                                 
         OC    ACCNT,SPACES                                                     
         LA    R3,1(R1,R3)                                                      
         ST    R3,SVADDR         SVE ADDR WHERE PERS SHOULD BE MOVED IN         
*                                                                               
         LA    R2,HMVOFF2H         CURSOR AT OFFICE ON ERROR                    
         GOTO1 =A(VALLOC),DMCB,RR=RELO  VALIDATE LOCATION GIVEN                 
         BNE   EINVLOCD                                                         
*                                                                               
         L     R3,SVADDR           POINT TO RIGT SPOT FOR PERSON                
         ZIC   R1,LEVELLND                                                      
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),PERSON     MOVE IN PERSON TO COMPLETE                   
         OC    ACCNT,SPACES                                                     
         MVC   SVACCNT2,ACCNT      SAVE NEW LOCATION ACCOUNT                    
         EJECT                                                                  
***********************************************************************         
*        VALIDATE LOCATION START DATE PASSED FROM PERSON                        
***********************************************************************         
*                                                                               
         LA    R2,HMVSDTEH                                                      
         CLI   5(R2),0                                                          
         BE    ERRINV                                                           
         XC    DMCB,DMCB                                                        
         LA    R1,HMVSDTE                                                       
         ST    R1,DMCB                                                          
         ZIC   R1,5(R2)                                                         
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         MVC   STDATE,PVALPSTA                                                  
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD KEY                                                              
***********************************************************************         
*                                                                               
*                                  PROTECT LOC(S) PASSED FROM PERSON            
VK96     GOTO1 =A(PROTLOCS),DMCB,RR=RELO                                        
         GOTO1 =A(PAYCDS),DMCB,RR=RELO    MAKE TABLE OF PAYCODES                
*                                                                               
         USING PHIRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    SALARY HISTORY RECORD                        
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CMPY        COMPANY                                      
         MVC   PHIKOFC,OFFICE                                                   
         MVC   PHIKDPT,DEPT                                                     
         MVC   PHIKSBD,SUBDPT                                                   
         MVC   PHIKPER,PERSON                                                   
         MVC   PHIKMOA,X'0000'     READ HIGHEST                                 
         MVC   PHIKSEQ,SEQNUM                                                   
*                                                                               
         CLC   BIGKEY(L'ACTKEY),SAVEKEY     KEY CHANGE                          
         BE    VKX                                                              
*                                                                               
VK100    BAS   RE,CLRSCRN                   CLEAR SCREEN                        
         MVC   SAVEKEY,BIGKEY                                                   
         XC    STDISP,STDISP                                                    
         XC    PRVSTDSP,PRVSTDSP                                                
         MVI   FIRSTIME,C'Y'       SET FIRSTIME IN                              
         XC    PEBLK,PEBLK         CLEAR BLOCK TO HOLD PERSON INFO              
         LA    R4,PEBLK                                                         
         USING PEBLKD,R4                                                        
         LA    R4,ELNAME           POINT TO BLOCK PASSED FROM PERSON            
         CLC   PENAME,=C'*PERBLK*' THIS BLOCK BETTER HAVE BEEN PASSED           
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,PETABLN          LENGTH OF TABLE ENTERIES                     
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EXMVC R1,PEBLK,0(R4)      SAVE BLOCK                                   
*                                                                               
VKX      MVI   IOOPT,C'Y'          DOING MY OWN IO                              
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RECORD                                              *         
***********************************************************************         
*                                                                               
VR       MVI   IOOPT,C'Y'          DISABLE CHANGES TO REC                       
         NI    BIT,X'FF'-VLAMT1                                                 
         GOTO1 =A(PAYCDS),DMCB,RR=RELO                                          
         CLI   FIRSTIME,C'N'      NOT THE FIRST TIME THROUGH                    
         BE    VR10                                                             
         GOTO1 =A(MAKETAB),DMCB,RR=RELO  MAKE TABLE FROM HISTORY REC            
*                                                                               
* ----- UPDATE AND DISPLAY ----- *                                              
*                                                                               
VR10     LA    R4,ELEMBLK                                                       
         USING ELEMTABD,R4                                                      
         USING DSPLINED,R2                                                      
         LA    R2,HMVLIN1H         FIRST DISPLAY LINE                           
*                                                                               
         CLI   FIRSTIME,C'N'                                                    
         BNE   VR20                                                             
*                                                                               
         LA    R3,DSPAMT1H         POINT TO 'KEEP' AMOUNT                       
         OI    BIT,VLAMT1                                                       
         GOTO1 =A(VALAMT),DMCB,RR=RELO VALIDATE AMOUNT                          
         NI    BIT,X'FF'-VLAMT1                                                 
         CLI   ACTEQU,ACTDEL                                                    
         BE    VR15                ONLY 1 AMT FOR ACTION DELETE                 
         LA    R3,DSPAMT2H                                                      
         GOTO1 =A(VALAMT),DMCB,RR=RELO                                          
*                                                                               
VR15     BAS   RE,UPTAB            UPDATE TABLE BEFORE DISPLAYING               
         BAS   RE,TABPCT           NOW UPDATE THE PC1/PC2 TABLE ENTRIES         
         BAS   RE,CLRSCRN          CLEAR SCREEN FIELDS                          
*                                                                               
VR20     MVI   FIRSTIME,C'N'                                                    
         CLI   PFKEY,3             PF3=QUIT (DO NOT UPDATE)                     
         BE    VR150               GO BACK TO PERSON                            
         CLI   PFKEY,6             PF6 TRIGGERS UPDTE TO HISTORY REC(S)         
         BE    VR100                                                            
         CLI   PFKEY,0                                                          
         BNE   VR32                                                             
         B     VR38                                                             
*                                                                               
VR32     CLI   PFKEY,7             UP                                           
         BNE   VR34                                                             
         MVC   STDISP,PRVSTDSP                                                  
         LA    R1,ELLEN                                                         
         MH    R1,=H'8'            8 LINES                                      
         LH    R0,PRVSTDSP                                                      
         SR    R0,R1                                                            
         CH    R0,=H'0'                                                         
         BNL   *+6                 DISP FROM TOP                                
         SR    R0,R0                                                            
         STH   R0,PRVSTDSP                                                      
         B     VR38                                                             
*                                                                               
VR34     CLI   PFKEY,8             DOWN                                         
         BNE   VR38                                                             
         MVC   PRVSTDSP,STDISP                                                  
         MVC   STDISP,DLINE1                                                    
         B     VR38                                                             
*                                                                               
VR37     MVC   STDISP,=H'0'        DEFAULT TO BEGINNING                         
         MVC   PRVSTDSP,=H'0'                                                   
*                                                                               
VR38     LA    R0,ELLEN            LENGTH OF ONE ENTRY                          
         MH    R0,TABCOUNT         NUMBER OF ENTRIES                            
         LH    R1,STDISP                                                        
         CR    R0,R1                                                            
         BH    VR39                                                             
         LA    R1,0                                                             
         STH   R1,STDISP                                                        
VR39     AR    R4,R1                                                            
*                                                                               
VR40NX   LA    R1,ELEMBLK                                                       
         LR    R0,R4               R4 POINTS TO ELEMENT BEING DISPLAYED         
         SR    R0,R1                                                            
         STH   R0,DLINE1           SAVE DISPLACEMENT INTO TABLE                 
*                                                                               
         OC    0(ELCHKENT,R4),0(R4)        ANY MORE ENTRIES?                    
         BNZ   VR42                                                             
         NI    GENSTAT2,X'FF'-RETEQSEL     GET NEXT SELECTION                   
         LA    R0,0                START FROM TOP NEXT ENTER                    
         STH   R0,DLINE1                                                        
         B     VRX                                                              
*                                                                               
VR42     GOTO1 =A(PAYCDS),DMCB,RR=RELO     MAKE TABLE OF PAYCDS                 
         GOTO1 DATCON,DMCB,(1,ELDATE),(17,DSPDATE)                              
         MVI   DSPDATEH+5,8        SET LENGTH                                   
         OI    DSPDATEH+6,X'80'                                                 
*                                                                               
VR45A    MVC   PCDNUM,ELNUM                                                     
         BAS   RE,DISPCD          DISPLAY CODE FROM PAYROLL CODE REC            
         MVC   DSPIND,SPACES                                                    
         TM    ELSTAT2,PDESHRTE    HOURLY RATE?                                 
         BZ    *+10                                                             
         MVC   DSPIND,=C'*H'                                                    
         TM    ELSTAT2,PDESADJ     ADJUSTED RATE?                               
         BZ    *+10                                                             
         MVC   DSPIND,=C'*A'                                                    
         MVI   DSPINDH+5,2         SET LENGTH                                   
*                                                                               
         LA    R3,ELTOT            DISPLAY TOTAL                                
         LA    R6,DSPTOTH                                                       
         OC    DSPTOT,SPACES                                                    
         GOTO1 =A(DISTOT),DMCB,RR=RELO                                          
         OI    DSPTOTH+6,X'80'    XMIT                                          
*                                                                               
         LA    R3,ELAMT1                                                        
         LA    R6,DSPAMT1H                                                      
         OC    DSPAMT1,SPACES                                                   
         GOTO1 =A(DISAMT),DMCB,RR=RELO                                          
         NI    DSPAMT1H+1,X'FF'-X'20'   UNPROTECT                               
         TM    ELSTAT,PDEPCS       IS THIS ENTRY PC1/PC2 GENERATED?             
         BNZ   *+12                                                             
         TM    ELSTAT,PDERVRSL     IS THIS ENTRY A REVERSAL?                    
         BZ    *+8                                                              
         OI    DSPAMT1H+1,X'20'    THEN PROTECT                                 
         OI    DSPAMT1H+6,X'80'                                                 
*                                                                               
         LA    R3,ELAMT2                                                        
         LA    R6,DSPAMT2H                                                      
         OC    DSPAMT2,SPACES                                                   
         GOTO1 =A(DISAMT),DMCB,RR=RELO                                          
         CLI   ACTEQU,ACTDEL       IF DELETING SALARY DO NOT WANT TO            
         BNE   *+12                SHOW TRANSFER AMOUNT COLUMN                  
         OI    DSPAMT2H+1,X'20'+X'0C'  PROTECT AND HIDE (LOW INTENSITY)         
         B     VR58                                                             
*                                                                               
         OI    DSPAMT2H+6,X'80'                                                 
         NI    DSPAMT2H+1,X'FF'-X'20'   UNPROTECT                               
         TM    ELSTAT,PDEPCS       IS THIS ENTRY PC1/PC2 GENERATED?             
         BNZ   *+12                                                             
         TM    ELSTAT,PDERVRSL     IS THIS ENTRY A REVERSAL?                    
         BZ    *+8                                                              
         OI    DSPAMT2H+1,X'20'    THEN PROTECT                                 
*                                                                               
VR58     LA    R2,DSPLLEN(R2)      NEXT SCREEN LINE                             
         LA    R1,HMVPFKYH         END OF LIST                                  
         CR    R2,R1                                                            
         BNL   VRX                                                              
         LA    R4,ELLEN(R4)        NEXT TABLE ENTRY                             
         B     VR40NX                                                           
*                                                                               
* COME HERE TO UPDATE HISTORY REC(S) WHEN PF6=UPDATE                            
*                                                                               
VR100    GOTO1 =A(UPOLDLOC),DMCB,RR=RELO                                        
         CLI   ACTEQU,ACTDEL       FOR ACTION DELETE ONLY UPDATING OLD          
         BE    VR105               LOCATION HISTORY                             
         GOTO1 =A(UPNEWLOC),DMCB,RR=RELO                                        
         B     VR110                                                            
*                                                                               
VR105    MVC   TEMPOFF,OFFICE       MAKE SURE NO MORE SALARY IN OLD             
         MVC   TEMPDEPT,DEPT        LOCATION PAST ENDATE/SAL LOCK DTE           
         MVC   TEMPSDPT,SUBDPT      EXCEPT FOR ADJ SALARY                       
         MVC   TEMPPER,PERSON                                                   
         GOTO1 =A(CHKSAL),DMCB,RR=RELO                                          
         BE    *+6                                                              
         DC    H'0'                DID NOT DELETE ALL SALARY                    
*                                                                               
VR110    GOTO1 =A(UPPER),DMCB,RR=RELO  UPDATE PERSON REC                        
         GOTO1 =A(UP1R),DMCB,RR=RELO UPDATE/ADD 1R                              
*                                                                               
VR150    CLI   PFKEY,3             IF QUITTING GO BACK TO PERSON/CHANGE         
         BNE   VR155                                                            
         MVI   PFKEY,1                                                          
         B     *+8                                                              
VR155    MVI   PFKEY,2             ELSE GO BACK TO PERSON/DISPLAY               
         BAS   RE,CALLPF                                                        
         DC    H'0'                SHOULDNT RETURN                              
*                                                                               
VRX      CLI   PFKEY,6                                                          
         BE    VRXX                                                             
         OI    GENSTAT2,USMYOK     SET MY OWN MESSAGE                           
         LA    R2,HMVLIN1H         SET CURSOR TO FIRST LINE                     
         B     INFUPD                                                           
VRXX     B     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
*    PROTECT AND HIDE (LOW INTENSITY) NEW LOCATION COLUMNS AND VALUES           
*    WHEN USER IS ENDING A LOCATION OR DELETING SALARY                          
***********************************************************************         
*                                                                               
PROTECT  NTR1                                                                   
*                                                                               
         LA    R2,HMVTOWH                                                       
         OI    1(R2),X'20'+X'0C'                                                
         LA    R2,HMVOFFWH                                                      
         OI    1(R2),X'20'+X'0C'                                                
         LA    R2,HMVOFF2H                                                      
         OI    1(R2),X'20'+X'0C'                                                
         LA    R2,HMVDPTWH                                                      
         OI    1(R2),X'20'+X'0C'                                                
         LA    R2,HMVDPT2H                                                      
         OI    1(R2),X'20'+X'0C'                                                
         LA    R2,HMVSDPWH                                                      
         OI    1(R2),X'20'+X'0C'                                                
         LA    R2,HMVSDP2H                                                      
         OI    1(R2),X'20'+X'0C'                                                
         LA    R2,HMVSDTWH                                                      
         OI    1(R2),X'20'+X'0C'                                                
         LA    R2,HMVSDTEH                                                      
         OI    1(R2),X'20'+X'0C'                                                
         LA    R2,HMVHEDWH                                                      
         OI    1(R2),X'20'+X'0C'                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*    MAKE TABLE OF '86' ELEMENTS FROM LOCATION END DATE PASSED                  
*    FROM PERSON RECORD FORWARD (ONLY CALL THIS FIRST TIME IN)                  
***********************************************************************         
*                                                                               
MAKETAB  NTR1                                                                   
*                                                                               
         LA    R0,ELEMBLK          CLEAR BLOCK FOR TABLE                        
         L     R1,=A(ELEMLNQ)                                                   
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    TABCOUNT,TABCOUNT   COUNT OF TABLE ENTRIES                       
         MVI   SEQNUM,0                                                         
*                                                                               
         USING ELEMTABD,R4                                                      
         LA    R4,ELEMBLK          MAKE TABLE OF ELEMENTS                       
*                                                                               
         USING PHIRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    X'3E05' PAYROLL HISTORY RECORD               
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CMPY        COMPANY                                      
         MVC   PHIKOFC,OFFICE                                                   
         MVC   PHIKDPT,DEPT                                                     
         MVC   PHIKSBD,SUBDPT                                                   
         MVC   PHIKPER,PERSON                                                   
         MVC   PHIKMOA,X'0000'     GET MOST CURRENT                             
         MVC   PHIKSEQ,SEQNUM                                                   
         MVC   KEYSAVE,BIGKEY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'ACCDIR  ',BIGKEY,BIGKEY,0            
         B     MT20                                                             
MT10NX   GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'ACCDIR  ',BIGKEY,BIGKEY,0            
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ ',=C'ACCDIR  ',BIGKEY,BIGKEY,0            
*                                                                               
MT20     CLC   BIGKEY(PHIKMOA-PHIKEY),KEYSAVE                                   
         BNE   MT100                                                            
*                                                                               
         LA    R6,BIGKEY                                                        
         ZICM  R1,PHIKMOA,2        MOA                                          
         LNR   R1,R1                                                            
         STH   R1,WORK                                                          
         CLC   WORK(2),ENDATE                                                   
         BL    MT100               READ UNTIL RECS ARE BEFORE END DATE          
*                                                                               
         MVC   AIO,AIO1                                                         
         LA    R2,BIGKEY                                                        
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC ',=C'ACCMST  ',(R2),AIO,WORK              
*                                                                               
         USING PDEELD,R6                                                        
         L     R6,AIO1             GET ELEMENTS                                 
MT22     MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         B     MT23                                                             
MT22NX   MVI   ELCODE,PDEELQ                                                    
         BAS   RE,NEXTEL                                                        
*                                                                               
MT23     BNE   MT10NX                                                           
         CLC   PDEDTE,ENDATE       PAYDATE > ENDATE                             
         BNH   MT22NX                                                           
         OC    SALDATE,SALDATE     ANY SALARY LOCK DATE                         
         BZ    *+14                                                             
         CLC   PDEDTE,SALDATE                                                   
         BNH   MT22NX                                                           
*                                                                               
*        ZAP   WORK(8),PDEAMT      ANY $$'S                                     
*        AP    WORK(8),PDEADJ                                                   
*        CP    WORK(8),=P'0'                                                    
*        BE    MT22NX                                                           
*                                                                               
         LH    R1,TABCOUNT         COUNT ENTRIES IN TABLE                       
         LA    R1,1(R1)                                                         
         STH   R1,TABCOUNT                                                      
*                                                                               
         CLC   TABCOUNT,=Y(MAXCOUNT) END REACHED                                
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   ELSEQ,SEQNUM        SAVE SEQ NUM OF RECORD                       
         MVC   ELDATE,PDEDTE       SAVE ENTRY IN TABLE                          
         MVC   ELNUM,PDENUM                                                     
         ZAP   ELTOT(8),PDEAMT(6)   TOTAL OF ORIGINAL AND ADJ                   
         AP    ELTOT(8),PDEADJ(6)                                               
         MVC   ELSTAT,PDESTAT      STATUS BYTE                                  
         MVC   ELSTAT2,PDESTAT2                                                 
*                                                                               
* HANDLE 'KEEP' AND 'TRANSFER' AMOUNTS BASED ON ACTION AND PAYCODE              
*                                                                               
         ZAP   ELAMT1(6),=P'0'     DEFAULT TO ZERO'S (KEEP AMT)                 
         ZAP   ELAMT2(6),=P'0'     DEFAULT TO ZERO'S (TRANSFER AMT)             
         CLI   ACTEQU,ACTDEL                                                    
         BNE   MT25                                                             
         TM    PDESTAT2,PDESADJ    ADJUSTED SALARY?                             
         BZ    *+10                                                             
         ZAP   ELAMT1(6),ELTOT+2(6) THEN COPY IN TOTAL AMOUNT TO KEEP           
         ZAP   ELAMT2(6),ELTOT+2(6) COPY TOTAL AMOUNT FOR TRANSFER              
         B     MT80                                                             
*                                                                               
MT25     TM    PDESTAT2,PDESADJ    ADJUSTED SALARY?                             
         BO    MT25A                                                            
*                                                                               
* IF REVERSAL-CHECK TO SEE IF MOA IS THE MONTH AFTER THE END DATE OF            
* OLD LOCATION.  YES, THEN CAN'T TRANSFER THE REVERSAL TO NEW LOCATION          
* BECAUSE WILL NOT MATCH THE LOCATION OF THE DEFINING REVERSAL                  
*                                                                               
         TM    PDESTAT,PDERVRSL    REVERSAL?                                    
         BZ    MT25B                                                            
         MVC   NXTDATE,ENDATE                                                   
         GOTO1 =A(NXTMTH),DMCB,RR=RELO   ADD 1 MONTH TO LOC ENDDATE             
         CLC   ELDATE(2),INDATE      INDATE=NEXT MONTHS DATE                    
         BNE   MT25B                                                            
MT25A    ZAP   ELAMT1(6),ELTOT+2(6) THEN COPY IN TOTAL AMOUNT                   
         ZAP   ELAMT2,=P'0'        AND FORCE IN 0'S IN TRANFER                  
         B     MT80                                                             
*                                                                               
* WHEN TRANSFERRING LOCATIONS MUST CHECK IF ANY PC1/PC2 GENERATED               
* ENTRIES SINCE THE 2 LOCATIONS CAN HAVE DIFFERENT PERCENTS MUST ADJUST         
*                                                                               
MT25B    DS    0H                                                               
         TM    PDESTAT,PDEPC1+PDEPC2 GENRATED BY PC1/PC2?                       
         BNZ   MT25C                                                            
         ZAP   ELAMT2(6),ELTOT+2(6)  NO THEN JUST COPY THE TOTAL                
         B     MT80                                                             
*                                                                               
MT25C    DS    0H                                                               
         XC    SVPCODE,SVPCODE                                                  
         MVC   SVPCODE,ELNUM       SAVE THE % PCODE                             
         BAS   RE,RECPCT           BUMP THROUGH ELEMS TO FIND %'S               
         NI    BIT,X'FF'-UPDKEEP                                                
         MVC   KEY2,BIGKEY         SAVE KEY BEFORE CHECKING PROFILES            
         GOTO1 =A(GETPCTS),DMCB,RR=RELO                                         
         MVC   BIGKEY,KEY2         RESTORE KEY                                  
         USING PCTSTABD,R3         BUMP THROUGH PERCENT TABLE AND               
         L     R3,APCTSTAB         ADD ANY PERCENTAGE ELEM                      
         B     MT30                                                             
MT25NX   LA    R3,PCTSLEN(R3)                                                   
         CLC   =X'FFFF',0(R3)                                                   
         BNE   *+6                                                              
         DC    H'0'                IF DIDN'T FIND IT SOMETHINGS WRONG           
*                                                                               
MT30     MVC   BYTE,PDESTAT                                                     
         NC    BYTE,PCTSBIT        SAME NUMBER?                                 
         BZ    MT25NX                                                           
         LA    R1,STARTWRK                                                      
         ZICM  R0,PCTSPCT,2        DISP TO PERCENTAGE                           
         AR    R1,R0                                                            
         ZAP   PCTAMT,0(6,R1)                                                   
         ZAP   WORK(12),SVAMT(8)     AMOUNT                                     
         MP    WORK(12),PCTAMT                                                  
         CP    WORK(12),=P'0'                                                   
         BE    MT80                                                             
         DP    WORK(12),=P'1000000'                                             
         ZAP   ELAMT2,WORK(8)                                                   
*                                                                               
MT80     ZIC   R1,PDELN                                                         
         LA    R0,PDELNQ                                                        
         SR    R1,R0                                                            
         LTR   R1,R1                                                            
         BZ    MT90                                                             
         STC   R1,ELDESCLN                                                      
         SH    R1,=H'1'                                                         
         BM    MT90                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELDESC(0),PDEDESC                                                
*                                                                               
MT90     LA    R4,ELLEN(R4)        BUMP TABLE                                   
         B     MT22NX                                                           
*                                                                               
MT100    LH    R2,TABCOUNT         SORT ON DATE THEN PAYCODE                    
         GOTO1 VQSORT,DMCB,(0,ELEMBLK),(R2),ELLEN,L'ELDATE+L'ELNUM,    X        
               ELDATE-ELEMTABD                                                  
         MVC   ELNAME,=C'*HIDMTAB'          TABLE HEADER                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        BUMP THROUGHT ELEMENTS FOR THE MONTH AND CHECK FOR ANY                 
*        PAYCODES W/ PC1/PC2 ATTACHEMENTS                                       
*        NTRY - AIO1 POINTS TO REC                                              
*               SVPCODE CONTAINS PC1/PC2 PAYCODE                                
***********************************************************************         
*                                                                               
RECPCT   NTR1                                                                   
         ZAP   SVAMT,=P'0'                                                      
         GOTO1 =A(PAYCDS),DMCB,RR=RELO     MAKE TABLE OF PAYCODES               
*                                                                               
         USING PDEELD,R6                                                        
         L     R6,AIO1                                                          
         MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         B     RECP10                                                           
RECP5NX  MVI   ELCODE,PDEELQ                                                    
         BAS   RE,NEXTEL                                                        
*                                                                               
RECP10   BNE   RECPX                                                            
         CLC   PDENUM,SVPCODE     DON'T CHECK THE ONE BEING CHECKED             
         BE    RECP5NX                                                          
*                                                                               
         USING PAYCDTAB,R3                                                      
         L     R3,ADISPBLK                                                      
         B     RECP20                                                           
RECP15NX LA    R3,PAYCDLEN(R3)                                                  
         L     R1,PAYEND                                                        
         CR    R3,R1               CHECK FOR END OF TABLE                       
         BL    *+6                                                              
         DC    H'0'                CODE MUST BE THERE                           
*                                                                               
RECP20   CLC   PAYCDNUM,PDENUM     MATCH ON PAYROLL CODE                        
         BNE   RECP15NX                                                         
         CLC   PAYCDPC1,SVPCODE    ANY PERCENTAGES TO ADD                       
         BE    *+14                                                             
         CLC   PAYCDPC2,SVPCODE    ANY PERCENTAGES TO ADD                       
         BNE   RECP5NX                                                          
         AP    SVAMT,PDEAMT        ADD AMOUNT                                   
         AP    SVAMT,PDEADJ        AND ADJUSTMENTS                              
         B     RECP5NX                                                          
*                                                                               
RECPX    B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        CHECK IF ANY SALARY STILL EXISTS PAST END DATE                         
*        USING LOCATION IN TEMPOFF, TEMPDEPT AND TEMPSDPT                       
***********************************************************************         
*                                                                               
CHKSAL   NTR1                                                                   
         L     RC,SAVERC                                                        
*                                                                               
         USING PHIRECD,R6                                                       
CS01     LA    R6,KEY2             CHECK FOR ANY SALARY AT LOCATION             
         XC    KEY2,KEY2                                                        
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    X'3E05' PAYROLL HISTORY RECORD               
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CMPY        COMPANY                                      
         MVC   PHIKOFC,TEMPOFF                                                  
         MVC   PHIKDPT,TEMPDEPT                                                 
         MVC   PHIKSBD,TEMPSDPT                                                 
         MVC   PHIKPER,PERSON                                                   
         MVC   PHIKMOA,X'0000'     GET MOST CURRENT                             
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         B     CS10                                                             
CS10SEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2                    
CS10     CLC   KEY2(PHIKMOA-PHIKEY),KEYSAVE                                     
         BNE   CSYES                                                            
*                                                                               
CS14     LA    R2,KEY2             GET RECORD                                   
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',(R2),AIO2,WORK               
*                                                                               
         USING PDEELD,R6                                                        
         L     R6,AIO2             CHECK DATES                                  
         MVI   ELCODE,PDEELQ       X'86' PAYROLL DETAIL ELEM                    
         BAS   RE,GETEL                                                         
         B     CS16                                                             
CS16NX   BAS   RE,NEXTEL                                                        
CS16     BNE   CS10SEQ                                                          
*                                                                               
         CLC   PDEDTE,ENDATE                                                    
         BNH   CS16NX                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(L'SALLOCK),SALLOCK   GENERATED SAL LOCK DATE                
         CLC   WORK(L'SALLOCK),SALDATE   COMPARE TO PASSED SAL LOCK DTE         
         BNL   *+10                                                             
         MVC   WORK(L'SALDATE),SALDATE   USE THE HIGHER ONE TO COMPARE          
         OC    WORK(L'SALDATE),WORK    ANY SALARY LOCK DATE                     
         BZ    *+14                                                             
         CLC   PDEDTE,WORK                                                      
         BNH   CS16NX                                                           
*                                                                               
         TM    PDESTAT2,PDESADJ    ONLY ADJ SALARY CAN BE PAST ENDATE           
         BO    CS16NX                                                           
*                                                                               
CSNO     B     XNO                 CAN'T DELETE - SALARY EXISTS                 
CSYES    B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD OR UPDATE 1R RECORD                                                
***********************************************************************         
*                                                                               
         USING PEBLKD,R3                                                        
         USING ACTRECD,R6                                                       
UP1R     NTR1                                                                   
*                                                                               
         LA    R3,PEBLK            PERSON INFO BLOCK                            
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         MVC   ACTKACT,SVACCNT     1R ACCOUNT OF OLD LOCATION                   
         OI    BIT2,UPOLD1R        SET BIT FOR OLD LOC UPDATE                   
UP1RNX   L     RE,AIO              PRE-CLEAR AIO                                
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    UP1R10              FOUND SO UPDATE                              
         TM    BIT2,UPNEW1R        UPDATING NEW LOC-THEN OKAY                   
         BO    *+6                                                              
         DC    H'0'                THEN SHOULD HAVE FOUND IT                    
         TM    BIT,DUPLOC          IS THIS LOCATION A DUPLICATE?                
         BZ    UP1R20                                                           
         DC    H'0'                THEN SHOULD HAVE FOUND THE 1R                
*                                                                               
UP1R10   MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         GOTO1 =A(DELOFF),DMCB,RR=RELO  TURN OFF DELETE BITS                    
         USING EMPELD,R6                                                        
         L     R6,AIO1                                                          
         MVI   ELCODE,EMPELQ       X'56' EMPLOYEE HISTORY ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   UP1R15                                                           
*                                                                               
         LA    R4,PELOCS                                                        
         USING PELOCS,R4                                                        
         TM    BIT2,UPOLD1R        OLD LOCATION?                                
         BZ    UP1R11                                                           
         TM    BIT,DELHIS          ACTION DELETE?                               
         BO    UP1R11              THEN ONLY ONE LOCATION ENTRY                 
         LA    R4,PELOCNQ(R4)      MUST POINT TO OLD LOCATION ENTRY             
UP1R11   MVC   EMPCSTAT,PESTAT     SET STATUS                                   
         TM    BIT2,UPOLD1R        IF UPDATING THE NEW LOCATION                 
         BZ    UP1R12              THEN NO SALARY LOCK DATE                     
         MVC   EMPSALKD,SALDATE                                                 
         OC    SALLOCK,SALLOCK                                                  
         BZ    UP1R12                                                           
         CLC   SALLOCK,SALDATE     SAVE WHICHEVER DATE IS HIGHER                
         BNH   *+10                                                             
         MVC   EMPSALKD,SALLOCK                                                 
UP1R12   GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         DROP  R4                                                               
*                                                                               
UP1R15   TM    BIT,DELHIS          DELETING HISTORY?                            
         BO    UP1RX               THEN DONE                                    
         TM    BIT2,UPNEW1R        DID WE JUST UPD NEW 1R                       
         BO    UP1RX               THEN DONE                                    
         NI    BIT2,X'FF'-UPOLD1R                                               
         OI    BIT2,UPNEW1R                                                     
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         MVC   ACTKACT,SVACCNT2      MOVE IN NEW LOC TO KEY                     
         B     UP1RNX                                                           
*                                                                               
         USING ACTRECD,R6                                                       
UP1R20   MVC   AIO,AIO1                                                         
         L     R6,AIO1             SET UP KEY TO ADD 1R                         
         MVC   0(L'ACTKEY,R6),KEYSAVE                                           
         MVC   ACTRLEN,=AL2(ACTRFST-ACTKEY)                                     
         OI    ACTRSTAT,ACTSABLP                                                
*                                                                               
         LA    R3,PEBLK                                                         
         LA    R4,PELOCS                                                        
         USING PELOCS,R4                                                        
         TM    PELSTAT,PENEWLOC    THIS MUST BE THE NEW LOC                     
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*&&US*&& GOTO1 ELM20,DMCB,(PENLNME,PELNAME),(PENFNME,PEFNAME) ADD X'20'         
*&&UK*&& GOTO1 =A(ELEM20UK),DMCB,RR=RELO    UPDATE 20 FOR UK                    
         BAS   RE,ELEM5A           NAME ELEMENTS                                
         BAS   RE,ELEM32           BALANCE ELEMENT                              
         BAS   RE,ELEM33           PEEL ELEMENT                                 
         BAS   RE,ELEM56           EMPLOYEE HISTORY ELEMENT                     
         BAS   RE,ELEM30           STATUS ELEMENT                               
         GOTO1 ADDREC                                                           
*                                                                               
UP1RX    B     XIT                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
*  ADD X'5A' NAME ELEMENTS                                                      
***********************************************************************         
*                                                                               
         USING PEBLKD,R3                                                        
         USING GPNELD,R6                                                        
ELEM5A   NTR1                                                                   
*                                                                               
         MVI   ELCODE,GPNELQ       X'5A'                                        
         GOTO1 REMELEM             REMOVE ANY IF THERE                          
*                                                                               
         LA    R3,PEBLK                                                         
         LA    R6,ELEM             ADD LAST NAME                                
         XC    ELEM,ELEM                                                        
         MVI   GPNEL,GPNELQ                                                     
         ZIC   R1,PENLNME          LENGTH OF LAST NAME                          
         AH    R1,=Y(GPNLNQ)                                                    
         STC   R1,GPNLN                                                         
         ZIC   R1,PENLNME                                                       
         SH    R1,=H'1'                                                         
         BM    EL5A10                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GPNNME(0),PELNAME                                                
         MVI   GPNTYP,GPNTLST                                                   
         GOTO1 ADDELEM                                                          
*                                                                               
EL5A10   LA    R6,ELEM             ADD FIRST NAME                               
         XC    ELEM,ELEM                                                        
         MVI   GPNEL,GPNELQ                                                     
         ZIC   R1,PENFNME          LENGTH OF FIRST NAME                         
         AH    R1,=Y(GPNLNQ)                                                    
         STC   R1,GPNLN                                                         
         ZIC   R1,PENFNME                                                       
         SH    R1,=H'1'                                                         
         BM    EL5AX                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GPNNME(0),PEFNAME                                                
         MVI   GPNTYP,GPNTFST                                                   
         GOTO1 ADDELEM                                                          
EL5AX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*  ADD X'32' ACCOUNT BALANCE ELEMENT                                            
***********************************************************************         
*                                                                               
         USING ABLELD,R6                                                        
ELEM32   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,ABLELQ                                                    
         BAS   RE,GETEL            DO NOTHING IF X'32' EXISTS                   
         BE    EL32X                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   ABLEL,ABLELQ        X'32'                                        
         MVI   ABLLN,ABLLN2Q                                                    
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         ZAP   ABLURG,=P'0'                                                     
         GOTO1 ADDELEM                                                          
EL32X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*  ADD X'33' ACCOUNT PEEL ELEMENT                                               
***********************************************************************         
*                                                                               
         USING APOELD,R6                                                        
ELEM33   NTR1                                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,APOELQ                                                    
         BAS   RE,GETEL            DO NOTHING IF X'33' EXISTS                   
         BE    EL33X                                                            
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   APOEL,APOELQ        X'33'                                        
         MVI   APOLN,APOLN2Q                                                    
         ZAP   APODR,=P'0'                                                      
         ZAP   APOCR,=P'0'                                                      
         GOTO1 ADDELEM                                                          
EL33X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*  ADD X'56' EMPLOYEE HISTORY ELEMENT FROM SAVED 56 ELEMENT FROM                
*  PERSON RECORD                                                                
***********************************************************************         
*                                                                               
         USING EMPELD,R6                                                        
         USING PEBLKD,R3                                                        
ELEM56   NTR1                                                                   
*                                                                               
         LA    R3,PEBLK                                                         
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   EMPEL,EMPELQ        X'56'                                        
         MVI   EMPLN,EMPLNQ                                                     
         MVC   EMPHIR,PEHIREDT     HIRE DATE                                    
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*  ADD X'30' RECORD STATUS ELEMENT (FILLING IN WHAT I CAN)                      
***********************************************************************         
*                                                                               
         USING PEBLKD,R3                                                        
         USING RSTELD,R6                                                        
ELEM30   NTR1                                                                   
*                                                                               
         LA    R3,PELOCS           POINT TO NEW LOCATION ENTRY                  
         USING PELOCS,R3                                                        
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN3Q                                                    
         GOTO1 DATCON,DMCB,(5,0),(1,RSTBDATE)                                   
         MVC   RSTTDATE,RSTBDATE                                                
         MVC   RSTFILT1,PEFILT1    UPDATE FILTERS                               
         MVC   RSTFILT2,PEFILT2                                                 
         MVC   RSTFILT3,PEFILT3                                                 
         MVC   RSTFILT4,PEFILT4                                                 
         MVC   RSTFILT5,PEFILT5                                                 
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        UPDATE PERSON RECORD BY ENDING THE ACTIVE LOCATION AND ADD A           
*        NEW LOCATION IF TRANSFERRING                                           
***********************************************************************         
*                                                                               
         USING PERRECD,R6                                                       
         USING PEBLKD,R3                                                        
UPPER    NTR1                                                                   
*                                                                               
         LA    R3,PEBLK            SAVED PERSON INFO                            
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    PERSON RECORD TYPE                           
         MVC   PERKCPY,CMPY       COMPANY                                       
         MVC   PERKCODE,PERSON     PERSON                                       
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'PERKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         USING EMPELD,R6                                                        
UPDP05   L     R6,AIO1                                                          
         MVI   ELCODE,EMPELQ       X'56' EMPLOYEE STATUS ELEM                   
         BAS   RE,GETEL                                                         
         BNE   UPDP10                                                           
         MVC   EMPHIR,PEHIREDT     HIRE DATE                                    
         MVC   EMPTRM,PETERMDT     TERMINATION DATE IF ANY                      
         MVC   EMPCSTAT,PESTAT     STATUS                                       
         TM    BIT,DELHIS          IF ADDING A NEW LOCATION                     
         BZ    UPDP10              THEN NO SAL LOCK DATE                        
*                                                                               
* SALDATE IS THE SALARY LOCK DATE USER FILLED IN ON THE PERSON SCREEN           
* FOR THE OLD LOCATION.  SALLOCK IS THE LATEST DATE THAT THE USER               
* WANTS TO KEEP HISTORY IN THE OLD LOCATION.  WHICHEVER DATE IS HIGHER          
* WILL GO IN THE '56' ELEMENT                                                   
*                                                                               
         MVC   EMPSALKD,SALDATE                                                 
         OC    SALLOCK,SALLOCK                                                  
         BZ    UPDP10                                                           
         CLC   SALLOCK,SALDATE                                                  
         BNH   *+10                                                             
         MVC   EMPSALKD,SALLOCK                                                 
*                                                                               
UPDP10   LA    R4,PELOCS           POINT TO LOCATION ENTRY                      
         USING PELOCS,R4                                                        
         TM    PELSTAT,PENEWLOC    MUST POINT TO THE OLD LOCATION               
         BZ    UPDP12                                                           
         LA    R4,PELOCNQ(R4)                                                   
*                                                                               
         USING LOCELD,R6                                                        
UPDP12   L     R6,AIO1                                                          
         MVI   ELCODE,LOCELQ       X'83' ELEMENT                                
         BAS   RE,GETEL                                                         
         B     *+8                                                              
UPDNXT   BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                THIS LOCATION MUST EXIST                     
*                                                                               
         CLC   OFFICE,LOCOFF                                                    
         BNE   UPDNXT                                                           
         CLC   DEPT,LOCDEPT                                                     
         BNE   UPDNXT                                                           
         CLC   SUBDPT,LOCSUB                                                    
         BNE   UPDNXT                                                           
*        OC    LOCEND,LOCEND       IS THIS THE ACTIVE LOCATION                  
*        BNZ   UPDNXT                                                           
         CLC   LOCSTART,PESTART    CHK IF THIS IS SAME LOC                      
         BNE   UPDNXT                                                           
*                                                                               
         MVC   LOCEND,ENDATE       END THE LOCATION                             
         OC    SALLOCK,SALLOCK                                                  
         BZ    *+10                                                             
         MVC   LOCSALKD,SALLOCK    ADD SALARY LOCK DATE                         
         OC    SALDATE,SALDATE                                                  
         BZ    UPDP15                                                           
         CLC   SALDATE,SALLOCK                                                  
         BNH   *+10                                                             
         MVC   LOCSALKD,SALDATE    KEEP WHATEVER DATE IS HIGHER                 
UPDP15   MVC   LOCSTAT,PESTAT      SET STATUS                                   
         DROP  R4                                                               
*                                                                               
         TM    BIT,DELHIS          DELETING HISTORY?                            
         BO    UPD100              DONE-WRITE BACK REC                          
*                                                                               
         LA    R3,PEBLK            POINT TO BEGINNING AGAIN                     
         LA    R4,PELOCS                                                        
         USING PELOCS,R4                                                        
         TM    PELSTAT,PENEWLOC    THIS BETTER BE THE NEW LOC                   
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,CHKDUP           CHECK IS NEW LOC IS A DUPLICATE              
*                                                                               
         USING LOCELD,R6                                                        
         LA    R6,ELEM             BUILD NEW ACTIVE LOCATION                    
         XC    ELEM,ELEM                                                        
         MVI   LOCEL,LOCELQ                                                     
         MVI   LOCLN,LOCLNQ                                                     
         MVC   LOCOFF,OFFICE2      OFFICE                                       
         MVC   LOCDEPT,DEPT2       DEPT                                         
         MVC   LOCSUB,SUBDPT2      SUBDEPT                                      
         MVC   LOCSTART,STDATE     START DATE                                   
         GOTO1 ADDELEM                                                          
*                                                                               
UPD100   DS    0H                                                               
         GOTO1 =A(ADDPTREL),DMCB,AIO1,RAPKRPER,RR=RELO                          
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 =A(ADDRAPTR),DMCB,AIO,RAPKRPER,RR=RELO                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR A DUPLICATE LOCATION AND MARK AS DUPLICATE                   
***********************************************************************         
*                                                                               
         USING LOCELD,R6                                                        
CHKDUP   NTR1                                                                   
         NI    BIT,X'FF'-DUPLOC                                                 
         L     R6,AIO1                                                          
         MVI   ELCODE,LOCELQ       X'83' ELEMENT                                
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CHKNXT   BAS   RE,NEXTEL                                                        
         BNE   CHKX                                                             
*                                                                               
         CLC   OFFICE2,LOCOFF                                                   
         BNE   CHKNXT                                                           
         CLC   DEPT2,LOCDEPT                                                    
         BNE   CHKNXT                                                           
         CLC   SUBDPT2,LOCSUB                                                   
         BNE   CHKNXT                                                           
         OI    LOCSTAT2,LOCSDUP                                                 
         OI    BIT,DUPLOC                                                       
CHKX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SCREEN FIELDS BEFORE UPDATING TABLE                           
*        R2 POINTS TO SCREEN                                                    
*        R3 POINTS TO AMOUNT FIELD                                              
*        R4 POINTS TO TABLE                                                     
***********************************************************************         
*                                                                               
         USING ELEMTABD,R4                                                      
         USING DSPLINED,R2                                                      
VALAMT   NTR1                                                                   
*                                                                               
         ST    R2,SVADDR           SAVE ADDRESS OF SCREEN LINE                  
VA02     CLI   DSPDATEH+5,0        ANYTHING ON LINE                             
         BE    VA100                                                            
         CLI   DSPCODEH+5,0                                                     
         BE    VA100                                                            
*                                                                               
         CLI   5(R3),0             ANY AMOUNT THERE                             
         BNE   *+8                                                              
         B     ERRMISS             BETTER BE                                    
         TM    1(R3),X'20'         AMOUNT PROTECTED                             
         BO    VA100               SKIP                                         
*                                                                               
         ZAP   WORK(8),=P'0'                                                    
         LR    R2,R3                                                            
         ZIC   R0,0(R2)            NEED TO CHECK FOR /HR                        
         SH    R0,=H'8'                                                         
         TM    1(R2),X'02'         XHEADER                                      
         BNO   *+8                                                              
         SH    R0,=H'8'            R0=MAX LEN OF FIELD                          
*                                                                               
         LA    R1,8(R2)                                                         
         SR    R3,R3                                                            
VA10     CLC   0(L'AC@PERHR,R1),AC@PERHR      /HR                               
         BE    VA30                                                             
         LA    R3,1(R3)            LENGTH TO VALIDATE NUMBEER                   
         LA    R1,1(R1)                                                         
         BCT   R0,VA10                                                          
         ZIC   R3,5(R2)             VALIDATE AMOUNT                             
VA30     GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R3)                                  
         CLI   DMCB,X'FF'                                                       
         BE    EINVAMT                                                          
         TM    ELSTAT2,PDESHRTE    HOURLY RATE                                  
         BO    VA40                                                             
         CP    DMCB+4(8),=P'9999999.99'                                         
         BH    EINVAMT                                                          
         CP    DMCB+4(8),=P'-9999999.99'                                        
         BL    EINVAMT                                                          
         B     VA100                                                            
*                                                                               
VA40     CP    DMCB+4(8),=P'999.99'                                             
         BH    EINVAMT                                                          
         CP    DMCB+4(8),=P'-999.99'                                            
         BL    EINVAMT                                                          
*                                                                               
VA100    L     R2,SVADDR           POINT TO BEGINNING OF LINE                   
         LA    R2,DSPLLEN(R2)      NEXT SCREEN LINE                             
         ST    R2,SVADDR                                                        
         LA    R4,ELLEN(R4)        NEXT TABLE ENTRY                             
         LA    R1,HMVPFKYH         END OF SCREEN                                
         CR    R2,R1                                                            
         BNL   VAX                                                              
         LA    R3,DSPAMT1H                                                      
         TM    BIT,VLAMT1          VALIDATING 1ST AMOUNT                        
         BO    VA02                                                             
         LA    R3,DSPAMT2H         VALIDATING 2ND AMOUNT                        
         B     VA02                                                             
VAX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE TABLE ENTRIES FROM SCREEN                                       
*        R4 POINTS TO BEGINNING OF TABLE                                        
*        R2 POINTS TO SCREEN                                                    
***********************************************************************         
*                                                                               
         USING ELEMTABD,R4                                                      
UPTAB    NTR1                                                                   
         AH    R4,STDISP           ADD DISPLACEMENT                             
         GOTO1 =A(PAYCDS),DMCB,RR=RELO    MAKE TABLE OF PAYCODES                
*                                                                               
UPD10    OC    0(ELCHKENT,R4),0(R4)   ANY MORE ENTRIES                          
         BZ    UPDX                                                             
*                                                                               
         TM    ELSTAT,PDEPCS       IS THIS ENTRY PC1/PC2 GENERATED?             
         BNZ   UPD30               SKIP THIS FOR NOW                            
*                                                                               
         CLC   DSPCODE,SVREVCD  LOOK FOR MATCH ON SAVED REV CODE                
         BNE   UPD10A                                                           
         ZAP   ELAMT1(6),REVAMT1   MOVE IN SAVED REVERSAL AMOUNT(S)             
         ZAP   ELAMT2(6),REVAMT2                                                
         XC    SVREVCD,SVREVCD     CLEAR SAVED REVERSAL CODE                    
         B     UPD25                                                            
*                                                                               
UPD10A   DS    0H                                                               
*        CLC   ELNUM,SVPCODE       MUST BE SAME PCODE                           
*        BNE   UPD12                                                            
*        ZAP   ELAMT1(6),PCTAMT1   MOVE IN SAVED PCT AMOUNT                     
*        ZAP   ELAMT2(6),PCTAMT2                                                
*        XC    SVPCODE,SVPCODE                                                  
*        B     UPD25                                                            
*                                                                               
UPD12    TM    DSPAMT1H+1,X'20'    KEEP AMOUNT PROTECTED                        
         BO    UPD30               THEN SKIP                                    
         XC    ELAMT1,ELAMT1       CLEAR AMOUNTS IN TABLE                       
         XC    ELAMT2,ELAMT2                                                    
UPD14    LA    R3,DSPTOTH                                                       
         OI    BIT,TOTALS                                                       
         BAS   RE,UPDAMT           UPDATE AMOUNT                                
         NI    BIT,X'FF'-TOTALS                                                 
         ZAP   ELTOT(8),WORK(8)  TOTAL AMOUNT                                   
         LA    R3,DSPAMT1H                                                      
         BAS   RE,UPDAMT           UPDATE AMOUNT                                
         ZAP   ELAMT1(6),WORK+2(6)   KEEP AMOUNT                                
*                                                                               
UPD15    CLI   ACTEQU,ACTDEL       ACTION DELETE                                
         BE    UPD20                                                            
         LA    R3,DSPAMT2H                                                      
         BAS   RE,UPDAMT           UPDATE AMOUNT                                
         ZAP   ELAMT2(6),WORK+2(6)   TRANSFER AMT                               
         B     *+10                                                             
*                                                                               
UPD20    MVC   ELAMT2(6),ELTOT+2   COPY TOTAL AMT IF DELETE ACTION              
UPD25    XC    WORK,WORK                                                        
         ZIC   R1,DSPCODEH+5       LENGTH OF PAYCODE                            
         BCTR  R1,0                                                             
         EXMVC R1,WORK,DSPCODE                                                  
         OC    WORK,SPACES                                                      
         BAS   RE,CHKPCODE         CHECK FOR PC1/PC2                            
*                                                                               
UPD30    LA    R4,ELLEN(R4)        NEXT ENTRY IN TABLE                          
         LA    R2,DSPLLEN(R2)      NEXT SCREEN LINE                             
         LA    R1,HMVPFKYH         END OF SCREEN                                
         CR    R2,R1                                                            
         BL    UPD10               UPDATE NEXT LINE                             
*                                                                               
UPDX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        UPDATE PC1/PC2 GENERATE ENTRIES                                        
***********************************************************************         
*                                                                               
REG      USING ELEMTABD,R6                                                      
PCT      USING ELEMTABD,R4                                                      
         USING PAYCDTAB,R3                                                      
TABPCT   NTR1                                                                   
*                                                                               
         LA    R4,ELEMBLK          R4 WILL POINT TO PC1/PC2 ENTRY               
         LA    R6,ELEMBLK          R6 WILL BUMP THROUGH TABLE                   
         GOTO1 =A(PAYCDS),DMCB,RR=RELO                                          
         B     TAB10                                                            
*                                                                               
TAB5NX   LA    R4,ELLEN(R4)        BUMP TABLE                                   
TAB10    OC    0(ELCHKENT,R4),0(R4)   ANY MORE ENTRIES                          
         BZ    TABX                                                             
*                                                                               
         TM    ELSTAT,PDEPCS       IS THIS ENTRY PC1/PC2 GENERATED              
         BZ    TAB5NX              NO CONTINUE LOOKING                          
         ZAP   PCT.ELAMT1,=P'0'    CLEAR THE AMOUNTS                            
         ZAP   PCT.ELAMT2,=P'0'                                                 
         MVC   SVPCODE,PCT.ELNUM   SAVE THE CODE                                
         MVC   SVPCDTE,PCT.ELDATE                                               
         B     TAB17                                                            
*                                                                               
TAB15NX  LA    R6,ELLEN(R6)                                                     
TAB17    OC    0(ELCHKENT,R6),0(R6)                                             
         BNZ   TAB20                                                            
         LA    R6,ELEMBLK          RESET TABLE                                  
         B     TAB5NX              AND KEEP ON LOOKING FOR PC1/PC2'S            
*                                                                               
TAB20    CLC   REG.ELNUM,SVPCODE   DON'T CHECK THE ONE BEING CHECKED            
         BE    TAB15NX                                                          
         CLC   REG.ELDATE,SVPCDTE  AND MUST BE SAME DATE AT PC1/PC2 ONE         
         BNE   TAB15NX                                                          
         L     R3,ADISPBLK                                                      
         B     TAB30                                                            
TAB25NX  LA    R3,PAYCDLEN(R3)                                                  
         L     R1,PAYEND                                                        
         CR    R3,R1               CHECK FOR END OF TABLE                       
         BL    *+6                                                              
         DC    H'0'                CODE MUST BE THERE                           
*                                                                               
TAB30    CLC   PAYCDNUM,REG.ELNUM  MATCH ON PAYCODE                             
         BNE   TAB25NX                                                          
         CLC   PAYCDPC1,SVPCODE    DOES THIS HAVE A PC1/PC2 ATTACHED?           
         BE    *+14                                                             
         CLC   PAYCDPC2,SVPCODE                                                 
         BNE   TAB15NX                                                          
*                                                                               
         CP    REG.ELAMT1,=P'0'                                                 
         BNZ   *+16                                                             
         TM    ACTEQU,ACTDEL                                                    
         BO    TAB15NX                                                          
         B     TAB40                                                            
         ZAP   SVAMT,REG.ELAMT1                                                 
         OI    BIT,UPDKEEP         UPDATE KEEP AMOUNT                           
         MVC   KEY2,BIGKEY         SAVE KEY BEFORE CHECKING PROFILES            
         GOTO1 =A(GETPCTS),DMCB,RR=RELO                                         
         MVC   BIGKEY,KEY2         RESTORE KEY                                  
         BAS   RE,RDPCTS                                                        
         AP    PCT.ELAMT1,PCTAMT1                                               
         TM    ACTEQU,ACTDEL                                                    
         BO    TAB15NX                                                          
TAB40    CP    REG.ELAMT2,=P'0'                                                 
         BZ    TAB15NX                                                          
         ZAP   SVAMT,REG.ELAMT2                                                 
         NI    BIT,X'FF'-UPDKEEP                                                
         MVC   KEY2,BIGKEY         SAVE KEY BEFORE CHECKING PROFILES            
         GOTO1 =A(GETPCTS),DMCB,RR=RELO                                         
         MVC   BIGKEY,KEY2         RESTORE KEY                                  
         BAS   RE,RDPCTS                                                        
         AP    PCT.ELAMT2,PCTAMT2                                               
         B     TAB15NX                                                          
*                                                                               
TABX     B     XIT                                                              
         DROP  REG,PCT,R3                                                       
         EJECT                                                                  
***********************************************************************         
*        READ PERCENTS                                                          
*        NTRY - R6 POINTS TO TABLE ENTRY                                        
*        NTRY - R3 POINTS TO PAYROLL CODE TABLE ENTRY                           
*               AMOUNT IS IN SVAMT                                              
*        ON EXIT:  UPDATED AMOUNT IN WORK                                       
***********************************************************************         
*                                                                               
REG      USING ELEMTABD,R6                                                      
         USING PCTSTABD,R2         BUMP THROUGH PERCENT TABLE A                 
RDPCTS   NTR1                                                                   
         L     R2,APCTSTAB         ADD ANY PERCENTAGE ELEM                      
         B     RDP10                                                            
RDP5NX   LA    R2,PCTSLEN(R2)                                                   
         CLC   =X'FFFF',0(R2)                                                   
         BNE   *+6                                                              
         DC    H'0'                IF DIDN'T FIND IT SOMETHINGS WRONG           
*                                                                               
RDP10    ZICM  RF,PCTSNUMB,2       DISP TO PCT PAYROLL NUMBER IN TABLE          
         AR    RF,R3                                                            
         OC    0(1,RF),0(RF)       ANY NUMBER                                   
         BZ    RDP5NX              NO, SKIP                                     
         LA    R1,STARTWRK                                                      
         ZICM  R0,PCTSPCT,2        DISP TO PERCENTAGE                           
         AR    R1,R0                                                            
         ZAP   PCTAMT,0(6,R1)                                                   
         ZAP   WORK(12),SVAMT(8)     AMOUNT                                     
         MP    WORK(12),PCTAMT                                                  
         CP    WORK(12),=P'0'                                                   
         BE    XIT                                                              
         DP    WORK(12),=P'1000000'                                             
         TM    BIT,UPDKEEP         UPDATING KEEP AMOUNT                         
         BZ    RDP20                                                            
         ZAP   PCTAMT1,WORK(8)                                                  
         B     RDPX                                                             
RDP20    ZAP   PCTAMT2,WORK(8)                                                  
RDPX     B     XIT                                                              
         DROP  R2,REG                                                           
***********************************************************************         
*        UPDATE AMOUNT IN TABLE FROM SCREEN                                     
*        R3 POINTS TO AMOUNT HEADER                                             
*        ON EXIT:  UPDATED AMOUNT IN WORK                                       
***********************************************************************         
*                                                                               
         USING ELEMTABD,R4                                                      
         USING DSPLINED,R2                                                      
UPDAMT   NTR1                                                                   
*                                                                               
         ZAP   WORK(8),=P'0'                                                    
         LR    R2,R3                                                            
         ZIC   R0,0(R2)            NEED TO CHECK FOR /HR                        
         SH    R0,=H'8'                                                         
         TM    1(R2),X'02'         XHEADER                                      
         BNO   *+8                                                              
         SH    R0,=H'8'            R0=MAX LEN OF FIELD                          
*                                                                               
         LA    R1,8(R2)                                                         
         SR    R3,R3                                                            
UPAM10   CLC   0(L'AC@PERHR,R1),AC@PERHR      /HR                               
         BE    UPAM30                                                           
         LA    R3,1(R3)            LENGTH TO VALIDATE NUMBEER                   
         LA    R1,1(R1)                                                         
         BCT   R0,UPAM10                                                        
         ZIC   R3,5(R2)             VALIDATE AMOUNT                             
         TM    BIT,TOTALS          UPDATING TOTAL                               
         BZ    UPAM30                                                           
         ZIC   R3,7(R2)            THEN USE OUTPUT LENGTH                       
UPAM30   GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R3)                                  
         CLI   DMCB,X'FF'                                                       
         BNE   *+8                                                              
         B     EINVAMT                                                          
         ZAP   WORK(8),DMCB+4(8)                                                
UPAMX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECKS FOR ANY PC1/PC2 ATTACHMENTS ON PAYCODE AND REVERSALS            
*        WORK CONTAINS PAYCODE NAME FROM SCREEN                                 
***********************************************************************         
*                                                                               
CHKPCODE NTR1                                                                   
         USING ELEMTABD,R4                                                      
         USING DSPLINED,R2                                                      
         USING PAYCDTAB,R3                                                      
*                                                                               
         ZAP   PCTAMT1,=P'0'                                                    
         ZAP   PCTAMT2,=P'0'                                                    
*                                                                               
         L     R3,ADISPBLK                                                      
         B     CTP20                                                            
CTPNX    LA    R3,PAYCDLEN(R3)                                                  
         L     R1,PAYEND                                                        
         CR    R3,R1               CHECK FOR END OF TABLE                       
         BNL   CTPNO                                                            
*                                                                               
CTP20    OC    PAYCDNUM(PAYCDLEN),PAYCDNUM                                      
         BZ    CTPNO                                                            
         CLC   PAYCDNME,WORK        MATCH ON PAYROLL CODE NAME                  
         BNE   CTPNX                                                            
         LA    R6,ELAMT1           POINT R6 TO KEEP AMOUNT                      
         OI    BIT,UPDKEEP                                                      
         B     CTP30                                                            
CTP25    NI    BIT,X'FF'-UPDKEEP                                                
         LA    R6,ELAMT2           POINT R6 TO 2ND AMOUNT                       
*                                                                               
CTP30    OC    PAYCDPCS,PAYCDPCS   ANY PERCENTAGES TO ADD                       
         BZ    CTP40               NO THEN CHECK FOR REVERSAL                   
         GOTO1 =A(PERCENT),DMCB,RR=RELO GO GET PERCENT                          
         BNE   CTP40                                                            
         CLI   ACTEQU,ACTDEL                                                    
         BE    CTP40                                                            
         TM    BIT,UPDKEEP                                                      
         BO    CTP25               KNOW CHECK FOR TRANSFER AMOUNT               
         B     CTPYES                                                           
*                                                                               
CTP40    CLC   PAYCDREV,SPACES     ANY REVERSAL CODE                            
         BNH   CTPYES              NO                                           
         MVC   SVREVCD,PAYCDREV    SAVE REVERSAL CODE                           
         CP    ELAMT1,=P'0'                                                     
         BE    *+16                                                             
         OI    BIT,UPDKEEP                                                      
         LA    R6,ELAMT1           POINT TO OLD LOC COLUMN FIRST                
         B     CTP50                                                            
         ZAP   REVAMT1,=P'0'                                                    
         ZAP   REVAMT2,=P'0'                                                    
         CLI   ACTEQU,ACTDEL                                                    
         BE    CTPYES              DONE                                         
CTP45    NI    BIT,X'FF'-UPDKEEP                                                
         CP    ELAMT2,=P'0'                                                     
         BNE   *+14                                                             
         ZAP   REVAMT2,=P'0'                                                    
         B     CTPYES                                                           
         LA    R6,ELAMT2                                                        
*                                                                               
CTP50    TM    BIT,UPDKEEP         UPDATING KEEP AMOUNT                         
         BZ    CTP55                                                            
         ZAP   REVAMT1,0(6,R6)     R6 POINTS TO AMOUNT                          
         OI    REVAMT1+5,X'0D'      MAKE NEGATIVE                               
         B     CTP60                                                            
CTP55    ZAP   REVAMT2,0(6,R6)                                                  
         OI    REVAMT2+5,X'0D'     MAKE NEGATIVE                                
*                                                                               
CTP60    CLI   ACTEQU,ACTDEL       DELETING?                                    
         BNE   *+14                THEN DONE                                    
         ZAP   REVAMT2,=P'0'                                                    
         B     CTPYES                                                           
         TM    BIT,UPDKEEP         UPDATING KEEP COLUMN                         
         BO    CTP45               THEN MUST CHECK FOR TRANSFER AMOUNT          
*                                                                               
CTPYES   B     XYES                                                             
CTPNO    B     XNO                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
*        GETS MATCHING CODE NAME FROM PAYCDBLK TABLE                            
*        USES PCDNUM FOR INPUT CODE : OUTPUTS TO SCREEN                         
***********************************************************************         
*                                                                               
DISPCD   NTR1                                                                   
         USING DSPLINED,R2                                                      
         USING PAYCDTAB,R3                                                      
         L     R3,ADISPBLK                                                      
         B     DS20                                                             
DS10NX   LA    R3,PAYCDLEN(R3)                                                  
         L     R1,PAYEND                                                        
         CR    R3,R1               CHECK FOR END OF TABLE                       
         BNL   DSXNO                                                            
*                                                                               
DS20     OC    PAYCDNUM(PAYCDLEN),PAYCDNUM                                      
         BZ    DSXNO                                                            
         CLC   PCDNUM,PAYCDNUM     MATCH ON PAYROLL CODE NUMBER                 
         BNE   DS10NX                                                           
         MVC   DSPCODE,PAYCDNME     DISP CODE                                   
         OI    DSPCODEH+6,X'80'                                                 
         MVI   DSPCODEH+5,5        GIVE A LENGTH                                
DSXYES   B     XYES                                                             
DSXNO    B     XNO                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        CLEAR SOME FIELDS                                                      
***********************************************************************         
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,HMVLIN1H         CLEAR ALL FIELDS                             
         LA    R3,HMVPFKYH                                                      
CSCLR    DS    0H                                                               
         ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         MVI   5(R2),X'00'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CSCLR               NO                                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CALL PF KEY                                                  *         
***********************************************************************         
*                                                                               
CALLPF   NTR1                                                                   
         L     R2,=A(MPFTABLE)                                                  
         A     R2,RELO                                                          
         LA    R3,HMVPFKYH                                                      
         GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)  INITIALIZE THE PFKEYS            
CALLX    B     XIT                                                              
***********************************************************************         
*        RANDOM STUFF                                                 *         
***********************************************************************         
*                                  GENERAL MESSAGES                             
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
ERRX     MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
*                                                                               
EYUPD    MVC   GERROR,=AL2(ACEYUPD)    ENTER Y TO UPDATE                        
         B     ACCERRX                                                          
EINVDATE MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX                                                          
EINVAMT  MVC   GERROR,=AL2(ACEAMNT)                                             
         B     ACCERRX                                                          
EINVOPT  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ACCERRX                                                          
EINVLOCD MVC   GERROR,=AL2(ACEILOCD)                                            
         B     ACCERRX                                                          
INFUPD   MVC   GERROR,=AL2(ACIDISUP)                                            
         MVI   GMSGTYPE,C'I'                                                    
         B     ACCERRX                                                          
ERRACT   MVC   GERROR,=AL2(ACERCACT)                                            
         B     ACCERRX                                                          
*                                                                               
ACCERRX  MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         GETEL2 R2,DATADISP,ELCODE                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        PERCENTAGE INFO TABLE                                                  
***********************************************************************         
*                                                                               
* PC1                                                                           
*                                                                               
PCTSTAB  DC    AL2(PAYPC1-PAYELD)      DISP TO CODE IN '84' ELEM                
         DC    AL1(PDEPC1)             BIT ON IN '86' ELEM                      
         DC    AL2(PAYCDPC1-PAYCDTAB)  DISP TO PAYNUM FOR PCT IN TABLE          
         DC    AL2(PC1PCT-STARTWRK)    DISP TO PERCENTAGE                       
         DC    AL2(COPC1-COBLOCK)      DISP TO PERCENT IN PROFILE TAB           
*                                                                               
* PC2                                                                           
*                                                                               
         DC    AL2(PAYPC2-PAYELD)                                               
         DC    AL1(PDEPC2)                                                      
         DC    AL2(PAYCDPC2-PAYCDTAB)                                           
         DC    AL2(PC2PCT-STARTWRK)                                             
         DC    AL2(COPC2-COBLOCK)                                               
*                                                                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
*        MAINTENANCE SCREEN PFKEY TABLE DEFINITIONS                             
***********************************************************************         
MPFTABLE DS    0C                                                               
*                                                                               
*        INTERNAL PF1 TO GO BACK TO PERSON/CHANGE WHEN QUITTING                 
*        FROM HISTORY SCREEN                                                    
*                                                                               
         DC    AL1(LPF01X-*,01,PFTCPROG,(LPF01X-LPF01)/KEYLNQ,0)                
         DCDD  AC#CPRSN,3                                                       
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#CHG,8                                                         
LPF01    DC    AL1(KEYTYTWA,L'HMVPERS-1),AL2(HMVPERS-T61DFFD)                   
LPF01X   EQU   *                                                                
*                                                                               
*        INTERNAL PF2 TO GO BACK TO PERSON/DISPLAY AFTER UPDATING               
*        HISTORY AND PERSON                                                     
*                                                                               
         DC    AL1(LPF02X-*,02,PFTCPROG,(LPF02X-LPF02)/KEYLNQ,0)                
         DCDD  AC#CPRSN,3                                                       
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#DSP,8                                                         
LPF02    DC    AL1(KEYTYTWA,L'HMVPERS-1),AL2(HMVPERS-T61DFFD)                   
LPF02X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        ADD X'FA' POINTER ELEMENTS                                   *         
*        R1 SHOULD POINT TO RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RAPPERD,R6                                                       
ADDPTREL NMOD1 0,*PTREL*                                                        
         L     RC,SAVERC                                                        
         L     R2,0(R1)                                                         
         L     R6,AIO3                                                          
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM    'FA' POINTER ELEM                            
         MVC   RAPCPY,CMPY                                                      
         MVI   RAPEMU,C'N'                                                      
         MVC   RAPACOM,ACOMFACS                                                 
         ST    R2,RAPAREC                                                       
         L     R2,4(R1)                                                         
         STCM  R2,1,RAPRTYP                                                     
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    ADDPTRX                                                          
         DC    H'0'                                                             
ADDPTRX  XIT1                                                                   
         DROP  R6                                                               
***********************************************************************         
*     LITERALS                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADD RECORD ACTIVITY PASSIVE POINTER                          *         
*        R1 SHOULD POINT TO RECORD                                    *         
***********************************************************************         
         USING RAPPERD,R6                                                       
ADDRAPTR NMOD1 0,*RAPTR*                                                        
         L     RC,SAVERC                                                        
         L     R2,0(R1)                                                         
         L     R6,AIO3                                                          
         MVI   RAPACTN,RAPAPTR     TYPE '14' PASSIVE POINTER                    
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    ADDRAPX                                                          
         DC    H'0'                                                             
ADDRAPX  XIT1                                                                   
         DROP  R6                                                               
***********************************************************************         
*     LITERALS                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*     PROTECT LOCATIONS PASSED FROM PERSON RECORD                     *         
***********************************************************************         
*                                                                               
PROTLOCS NMOD1 0,*PROLOC*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         LA    R2,HMVPERSH                                                      
         OI    1(R2),X'20'                                                      
         LA    R2,HMVOFFH                                                       
         OI    1(R2),X'20'                                                      
         LA    R2,HMVDEPTH                                                      
         OI    1(R2),X'20'                                                      
         LA    R2,HMVSDPTH                                                      
         OI    1(R2),X'20'                                                      
         LA    R2,HMVEDTEH                                                      
         OI    1(R2),X'20'                                                      
         LA    R2,HMVSLDTH                                                      
         OI    1(R2),X'20'                                                      
*                                                                               
         LA    R2,HMVOFF2H                                                      
         OI    1(R2),X'20'                                                      
         LA    R2,HMVDPT2H                                                      
         OI    1(R2),X'20'                                                      
         LA    R2,HMVSDP2H                                                      
         OI    1(R2),X'20'                                                      
         LA    R2,HMVSDTEH                                                      
         OI    1(R2),X'20'                                                      
*                                                                               
PROTX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*     UPDATE OLD LOCATION HISTORY RECORDS FROM TABLE                  *         
*     ACTIONS:  MOVE AND DELETE                                       *         
***********************************************************************         
*                                                                               
         USING ELEMTABD,R4                                                      
         USING PHIRECD,R6                                                       
UPOLDLOC NMOD1 0,*UPOLD**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         NI    BIT,X'FF'-(HISTELEM+BMPTAB)                                      
         XC    SALLOCK,SALLOCK                                                  
         LA    R4,ELEMBLK                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    X'3E05' PAYROLL HISTORY RECORD               
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CMPY        COMPANY                                      
         MVC   PHIKOFC,OFFICE                                                   
         MVC   PHIKDPT,DEPT                                                     
         MVC   PHIKSBD,SUBDPT                                                   
         MVC   PHIKPER,PERSON                                                   
         XC    WORK,WORK                                                        
         ZICM  R1,ELDATE,2         GET NEXT RECORD                              
         LNR   R1,R1                                                            
         STH   R1,WORK                                                          
         MVC   PHIKMOA,WORK                                                     
         MVC   PHIKSEQ,SEQNUM                                                   
OL10SEQ  GOTO1 HIGH                                                             
OL10     CLC   BIGKEY(PHIKMOA-PHIKEY),KEYSAVE                                   
         BNE   OLDX                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         USING PDEELD,R6                                                        
         L     R6,AIO1                                                          
         MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   OL30                                                             
*                                                                               
OL15     CLI   0(R6),X'86'                                                      
         BNE   OL30                                                             
         CLC   PDEDTE,ELDATE                                                    
         BNE   OLNXT                                                            
         CLC   PDENUM,ELNUM                                                     
         BNE   OLNXT                                                            
         ST    R6,SVADDR                                                        
         MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             REMOVE ELEMENT                               
*                                                                               
         CP    ELAMT1,=P'0'        IF AMOUNT TO SAVE=ZERO                       
         BNE   OL18                DO NOT REBUILD                               
         CLI   0(R6),X'86'         AFTER REMELEM R6 ALREADY POINTS TO           
         BNE   OL30                NEXT ELEM.                                   
         LA    R4,ELLEN(R4)                                                     
         B     OL15                                                             
OL18     OI    BIT,HISTELEM        WILL KEEP HISTORY IN LOCATION                
*                                                                               
         TM    ELSTAT2,PDESADJ     IF JUST ADJUSTED SALARY IS LEFT              
         BO    OL22                THEN DON'T ADD SALLOCK DATE                  
         CLC   SALLOCK,ELDATE      IS HIGHEST SALLOCK DTE SAVED ALREADY         
         BNL   *+10                                                             
         MVC   SALLOCK,ELDATE                                                   
*                                                                               
OL22     XC    ELEM,ELEM           REBUILD ELEM                                 
         LA    R6,ELEM                                                          
         USING PDEELD,R6                                                        
         MVI   PDEEL,PDEELQ                                                     
         MVC   PDEDTE,ELDATE                                                    
         MVC   PDENUM,ELNUM                                                     
         MVC   PDESTAT,ELSTAT                                                   
         MVC   PDESTAT2,ELSTAT2                                                 
         ZAP   PDEAMT(6),=P'0'     ZERO OUT ORIGINAL AMOUNT                     
         TM    ELSTAT,PDERVRSL     REVERSAL?                                    
         BZ    *+8                                                              
         OI    PDEAMT+5,X'0D'      MAKE NEGATIVE                                
         ZAP   PDEADJ(6),ELAMT1(6) STORE UPDATED AMT IN ADJUSTMENT              
         LA    R0,PDELNQ            DESCRIPTION AND LENGTH                      
         ZIC   R1,ELDESCLN                                                      
         AR    R0,R1                                                            
         STC   R0,PDELN                                                         
         SH    R1,=H'1'                                                         
         BM    OL25                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDEDESC(0),ELDESC                                                
         OC    PDEDESC,SPACES                                                   
*                                                                               
OL25     GOTO1 ADDELEM             ADDS ELEMENT                                 
         OI    BIT,BMPTAB                                                       
         MVI   ELCODE,X'86'        RESET ELEMENT                                
         L     R6,SVADDR                                                        
*                                                                               
OLNXT    BAS   RE,NEXTEL                                                        
         BNE   OL30                                                             
         TM    BIT,BMPTAB          SHOULD I BUMP TABLE                          
         BZ    OL15                NO                                           
         LA    R4,ELLEN(R4)        BUMP TO NEXT TABLE ENTRY                     
         NI    BIT,X'FF'-BMPTAB                                                 
         B     OL15                                                             
*                                                                               
OL30     DS    0H                                                               
         BAS   RE,ANYELEMS         MARK DELETED IF NO ELEMS EXIST               
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         NI    BIT,X'FF'-HISTELEM                                               
         LA    R4,ELLEN(R4)        BUMP TABLE                                   
         OC    0(ELCHKENT,R4),0(R4) ANY MORE ENTRIES                            
         BZ    OLDX                                                             
*                                                                               
         USING PHIRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    WORK,WORK                                                        
         ZICM  R1,ELDATE,2         GET NEXT RECORD                              
         LNR   R1,R1                                                            
         STH   R1,WORK                                                          
         MVC   PHIKMOA,WORK                                                     
         B     OL10SEQ                                                          
*                                                                               
OLDX     XIT1                                                                   
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        ANY ELEMENTS ON RECORD                                                 
***********************************************************************         
*                                                                               
ANYELEMS NTR1                                                                   
         USING PHIRECD,R6                                                       
         L     R6,AIO                                                           
         NI    PHIRSTA,X'FF'-X'80'    MAKE SURE NOT DELETED                     
         MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         BE    XYES                                                             
*                                                                               
         L     R6,AIO                                                           
         OI    PHIRSTA,X'80'       DELETE IF NO ELEMS                           
         LA    R6,BIGKEY                                                        
         OI    PHIKSTA,X'80'       DELETE IF NO ELEMS                           
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADD NEW HISTORY RECORDS FOR NEW LOCATION                               
*        ACTIONS:  MOVE                                                         
***********************************************************************         
*                                                                               
         USING ELEMTABD,R4                                                      
         USING PHIRECD,R6                                                       
UPNEWLOC NMOD1 0,*UPNEW**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVC   AIO,AIO1                                                         
         L     RE,AIO              PRE-CLEAR AIO                                
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         NI    BIT,X'FF'-HISTELEM                                               
         LA    R4,ELEMBLK          POINT TO BEGINNING OF TABLE                  
NW10     CP    ELAMT2,=P'0'        ANY AMOUNT TO MOVE                           
         BNE   NW20                                                             
NW15     LA    R4,ELLEN(R4)        NO THEN BUMP TABLE                           
         OC    0(ELCHKENT,R4),0(R4)   ANY MORE ENTRIES                          
         BZ    NWX                                                              
         B     NW10                                                             
*                                                                               
NW20     OI    BIT,HISTELEM                                                     
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    X'3E05' PAYROLL HISTORY RECORD               
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CMPY        COMPANY                                      
         MVC   PHIKOFC,OFFICE2                                                  
         MVC   PHIKDPT,DEPT2                                                    
         MVC   PHIKSBD,SUBDPT2                                                  
         MVC   PHIKPER,PERSON                                                   
         XC    WORK,WORK                                                        
         ZICM  R1,ELDATE,2                                                      
         LNR   R1,R1                                                            
         STH   R1,WORK                                                          
         MVC   PHIKMOA,WORK                                                     
         MVC   PHIKSEQ,SEQNUM                                                   
         OI    DMINBTS,X'08'       READ FOR DELETES                             
NW20NXT  L     RE,AIO              PRE-CLEAR AIO                                
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         NI    BIT,X'FF'-NEWREC                                                 
         GOTO1 HIGH                                                             
         CLC   BIGKEY(PHIKSEQ-PHIKEY),KEYSAVE                                   
         BNE   NW30                                                             
         GOTO1 GETREC                                                           
         GOTO1 =A(DELOFF),DMCB,RR=RELO  TURN OFF DELETE BITS                    
*                                                                               
         USING PDEELD,R6                                                        
         L     R6,AIO              REMOVE ANY '86' ELEMENTS                     
         MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   NW40                                                             
*                                                                               
NW25     MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         CLI   0(R6),PDEELQ        AFTER REMELEM R6 POINTS TO NEXT ELEM         
         BE    NW25                                                             
         B     NW40                                                             
*                                                                               
         USING PHIRECD,R6                                                       
NW30     L     R6,AIO1             SET UP KEY TO ADD HISTORY                    
         MVC   0(L'PHIKEY,R6),KEYSAVE                                           
         MVC   PHIRLEN,=AL2(PHIRFST-PHIKEY)                                     
         OI    BIT,NEWREC                                                       
*                                                                               
NW40     CP    ELAMT2,=P'0'        ANY AMOUNT TO SAVE                           
         BE    NW50                                                             
         OI    BIT,HISTELEM                                                     
         MVC   SVDATE,ELDATE       SAVE MOA (YYMM)                              
         XC    ELEM,ELEM           BUILD ELEM                                   
         LA    R6,ELEM                                                          
         USING PDEELD,R6                                                        
         MVI   PDEEL,PDEELQ                                                     
         MVC   PDEDTE,ELDATE                                                    
         MVC   PDENUM,ELNUM                                                     
         MVC   PDESTAT,ELSTAT                                                   
         MVC   PDESTAT2,ELSTAT2                                                 
         ZAP   PDEAMT(6),ELAMT2(6) STORE NEW AMT IN ORIGINAL                    
         ZAP   PDEADJ(6),=P'0'     CLEAR OUT ADJUSTMENT                         
         TM    ELSTAT,PDERVRSL     REVERSAL?                                    
         BZ    *+8                                                              
         OI    PDEADJ+5,X'0D'      MAKE NEGATIVE                                
         LA    R0,PDELNQ            DON'T CARRY OVER DESCRIPTION                
         STC   R0,PDELN                                                         
         GOTO1 ADDELEM             ADDS ELEMENT                                 
*                                                                               
NW50     LA    R4,ELLEN(R4)        BUMP TABLE                                   
         OC    0(ELCHKENT,R4),0(R4) ANYMORE ENTRIES?                            
         BZ    NW100                                                            
         CLC   SVDATE,ELDATE       COMPARE YYMM MOA                             
         BE    NW40                                                             
*                                                                               
NW100    NI    BIT,X'FF'-UPDKEEP   NOT UPDATING OLD LOC HISTORY ANYMORE         
         TM    BIT,NEWREC                                                       
         BO    NW110                                                            
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         B     NW120                                                            
*                                                                               
NW110    GOTO1 ADDREC                                                           
*                                                                               
NW120    OC    0(ELCHKENT,R4),0(R4) ANYMORE ENTRIES                             
         BNZ   NW10                                                             
*                                                                               
NWX      XIT1                                                                   
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        TURN OFF DELETE BITS OF HISTORY RECORDS                                
***********************************************************************         
*                                                                               
         USING PHIRECD,R6                                                       
DELOFF   NMOD1 0,*DELOFF*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R6,AIO                                                           
         NI    PHIRSTA,X'FF'-X'80'                                              
         LA    R6,BIGKEY                                                        
         NI    PHIKSTA,X'FF'-X'80'                                              
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY AMOUNT (PL6 AT R3) IN FIELD AT R6                              
***********************************************************************         
*                                                                               
         USING ELEMTABD,R4                                                      
DISAMT   NMOD1 0,*DISAMT*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         CURED (P6,(R3)),(11,WORK),2,FLOAT=-,ZERO=NOBLANK,ALIGN=LEFT            
         TM    ELSTAT2,PDESHRTE    HOURLY RATE                                  
         BNO   DA100                                                            
         LA    R1,WORK                                                          
         AR    R1,R0                                                            
         MVC   0(L'AC@PERHR,R1),AC@PERHR        /HR                             
         LR    R1,R0                                                            
         LA    R1,3(R1)                                                         
         STC   R1,5(R6)            LENGTH                                       
         ZIC   R0,0(R6)                                                         
         SH    R0,=H'8'                                                         
         TM    1(R6),X'02'         XHEADER                                      
         BNO   *+8                                                              
         SH    R0,=H'8'                                                         
         SR    R0,R1                                                            
         LA    R3,8(R6)                                                         
         AR    R3,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),WORK                                                     
         B     DAX                                                              
DA100    STC   R0,5(R6)            LENGTH                                       
         CURED (P6,(R3)),(11,8(R6)),2,FLOAT=-,ZERO=NOBLANK                      
DAX      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY TOTAL OF ORIGINAL AMOUNT + MANUAL ADJUSTMENT AMOUNT            
*        PL8 AT R3 IN FIELD AT R6                                               
***********************************************************************         
*                                                                               
         USING ELEMTABD,R4                                                      
DISTOT   NMOD1 0,*DISTOT*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         CURED (P8,(R3)),(11,WORK),2,FLOAT=-,ZERO=NOBLANK,ALIGN=LEFT            
         TM    ELSTAT2,PDESHRTE    HOURLY RATE                                  
         BNO   DT100                                                            
         LA    R1,WORK                                                          
         AR    R1,R0                                                            
         MVC   0(L'AC@PERHR,R1),AC@PERHR        /HR                             
         LR    R1,R0                                                            
         LA    R1,3(R1)                                                         
         STC   R1,5(R6)            LENGTH                                       
         ZIC   R0,0(R6)                                                         
         SH    R0,=H'8'                                                         
         TM    1(R6),X'02'         XHEADER                                      
         BNO   *+8                                                              
         SH    R0,=H'8'                                                         
         SR    R0,R1                                                            
         LA    R3,8(R6)                                                         
         AR    R3,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),WORK                                                     
         B     DTX                                                              
DT100    STC   R0,5(R6)            LENGTH                                       
         CURED (P8,(R3)),(11,8(R6)),2,FLOAT=-,ZERO=NOBLANK                      
DTX      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE 1R ACCOUNT                                                    
*        INPUT                     OFFICE = OFFICE CODE               *         
*        INPUT                     DEPT   = DEPATMENT CODE            *         
*        INPUT                     SUBDPT = SUB-DEPARTMENT CODE       *         
*        OUTPUT                    CC = EQUAL IF VALID, ELSE NOT      *         
***********************************************************************         
         USING ACTRECD,R4                                                       
VALLOC   NMOD1 0,*VALOC**          VALIDATE LOCATION ENTEREDD                   
         L     RC,SAVERC                                                        
*                                                                               
         LA    R4,KEY2             BUILD KEY                                    
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         MVC   ACTKACT,ACCNT                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,AIO2                    
         L     R4,AIO2                                                          
         CLC   KEY2(L'ACTKEY),0(R4)                                             
         BNE   VLNO                                                             
         B     VLYES                                                            
*                                                                               
VLYES    B     XYES                                                             
VLNO     B     XNO                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NMOD1 0,**SETUP*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         OI    GENSTAT2,RETEQSEL   RETURN SAME SELECTION (FOR PAGING)           
         MVI   SEQNUM,X'00'                                                     
         L     R1,=A(PCTSTAB)                                                   
         A     R1,RELO                                                          
         ST    R1,APCTSTAB                                                      
*                                                                               
         L     R1,=V(ACRAPPER)                                                  
         A     R1,RELO                                                          
         ST    R1,VRAPPER                                                       
*                                                                               
         SR    R2,R2                                                            
         LA    R3,HMVPFKYH                                                      
         GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)  INITIALIZE THE PFKEYS            
                                                                                
*                                                                               
SUX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GET ANY PERCENTAGES FROM PROFILES                                      
***********************************************************************         
*                                                                               
GETPCTS  NMOD1 0,*GETPCT*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R0,AIO2             USE AIO2 FOR COBLOCK                         
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING COBLOCK,R6                                                       
         L     R6,AIO2                                                          
         MVC   COADM,DATAMGR       PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,CMPY                                                      
         MVC   COKMTHD,SPACES                                                   
         MVC   COKPER,PERSON                                                    
         TM    BIT,UPDKEEP         ARE WE UPDATING THE KEEP AMOUNT?             
         BZ    GP05                NO THEN USE THE TRANSFER LOCATION            
         MVC   COKOFC,OFFICE                                                    
         MVC   COKDPT(L'DEPT),DEPT                                              
         MVC   COKSDT(L'SUBDPT),SUBDPT                                          
         B     GP10                                                             
*                                                                               
GP05     MVC   COKOFC,OFFICE2                                                   
         MVC   COKDPT(L'DEPT),DEPT                                              
         MVC   COKSDT(L'SUBDPT),SUBDPT2                                         
*                                                                               
GP10     GOTO1 VGETCAP,DMCB,COBLOCK                                             
         CLI   COSTATUS,X'00'      ANY ERRORS                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PCTSTABD,RF                                                      
         L     RF,APCTSTAB         PERCENT TABLE                                
         LA    R3,COBLOCK          PROFILE TABLE                                
*                                                                               
GP20     LA    R2,STARTWRK                                                      
         ZICM  R1,PCTSPCT,2        DISP TO PERCENT SAVE VALUE                   
         AR    R2,R1               ADD TO A(STARTWRK)                           
         ZAP   0(6,R2),=P'0'                                                    
*                                                                               
         ZICM  R1,PCTSPCTD,2       DISP TO PROFILE PERCENT                      
         BZ    GP40                                                             
         AR    R1,R3               ADD TO A(COBLOCK)                            
         ZAP   0(6,R2),0(4,R1)                                                  
*                                                                               
GP40     LA    RF,PCTSLEN(RF)                                                   
         CLC   =X'FF',0(RF)                                                     
         BNE   GP20                                                             
         XIT1                                                                   
         DROP  RF,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*     GET NEXT MONTH'S DATE                                           *         
***********************************************************************         
*                                                                               
         USING ELEMTABD,R4                                                      
NXTMTH   NMOD1 0,*NXTMTH*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,NXTDATE),(0,WORK) ADD 1 MONTH TO DATE             
         GOTO1 ADDAY,DMCB,(C'M',WORK),(X'40',WORK+10),1                         
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,INDATE)                               
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*  READS PAYROLL CODE RECORD(S) AND MAKES TABLE OF CODES IN PAYCDBLK            
***********************************************************************         
*                                                                               
PAYCDS   NMOD1 0,*PAYCDS*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R0,ADISPBLK                                                      
         LA    R1,PCTABLN                                                       
         AR    R0,R1                                                            
         ST    R0,PAYEND           SAVE END OF TABLE                            
*                                                                               
         L     R0,ADISPBLK         CLEAR BLOCK FOR TABLE                        
         LA    R1,PCTABLN                                                       
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   PSEQNUM,0                                                        
*                                                                               
         USING PAYCDTAB,R3                                                      
         L     R3,ADISPBLK                                                      
         USING PAYRECD,R6                                                       
PC10     LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    PAYROLL CODE RECORD TYPE                     
         MVI   PAYKSUB,PAYKSUBQ    PAYROLL CODE SUB TYPE                        
         MVC   PAYKCPY,CMPY        COMPANY                                      
         MVC   PAYKSEQ,PSEQNUM     SEQUENCE NUMBER                              
         MVC   SAVEKEY2,KEY2                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         CLC   KEY2(L'ACTKEY),SAVEKEY2                                          
         BNE   PCX                                                              
         MVC   AIO,AIO2                                                         
         LA    R2,KEY2                                                          
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC ',=C'ACCMST  ',(R2),AIO,WORK              
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,PAYELQ                                                    
         USING PAYELD,R6                                                        
         BAS   RE,GETEL                                                         
         B     PC15                                                             
PC15NX   BAS   RE,NEXTEL                                                        
PC15     BNE   PC30                                                             
         MVC   PAYCDNUM,PAYNUM                                                  
         MVC   PAYCDNME,PAYCODE                                                 
         MVC   PAYCDREV,PAYREV                                                  
         MVC   PAYCDPCS,PAYPCS                                                  
         MVC   PAYCDST,PAYSTAT                                                  
*                                                                               
         LA    R3,PAYCDLEN(R3)                                                  
         L     R1,PAYEND                                                        
         CR    R3,R1                                                            
         BL    PC15NX                                                           
         DC    H'0'                TOO MANY PAYCODES-INCREASE TABLE             
*                                                                               
PC30     ZIC   R1,PSEQNUM          CHECK FOR NEXT REC                           
         LA    R1,1(R1)                                                         
         STC   R1,PSEQNUM                                                       
         B     PC10                                                             
*                                                                               
PCX      XIT1                                                                   
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        RECALC ANY PERRCENTAGES                                                
*        R3 POINTS TO PAYROLL CODE TABLE ENTRY                                  
*        R4 POINT TO TABLE ENTRY OF THE CALLING PERCENTAGE                      
*        R6 POINT TO AMOUNT IN TABLE                                            
*        ON EXIT -- KEEP AMOUNT IN PCTAMT1, TRANSFER AMOUNT IN PCTAMT2          
*                   PAYCODE IN SVPCODE                                          
***********************************************************************         
*                                                                               
PERCENT  NMOD1 0,**PCT***                                                       
         L     RC,SAVERC                                                        
*                                                                               
         GOTO1 =A(PAYCDS),DMCB,RR=RELO    MAKE TABLE OF PAYCODES                
         GOTO1 =A(GETPCTS),DMCB,RR=RELO                                         
*                                                                               
         USING PCTSTABD,R1         BUMP THROUGH PERCENT TABLE AND               
         L     R1,APCTSTAB         ADD ANY PERCENTAGE ELEM                      
         B     PCT30                                                            
PCT30NX  LA    R1,PCTSLEN(R1)                                                   
         CLC   =X'FFFF',0(R1)                                                   
         BE    PERCENTN                                                         
*                                                                               
PCT30    ZICM  RF,PCTSNUMB,2       DISP TO PCT PAYROLL NUMBER IN TABLE          
         AR    RF,R3                                                            
         OC    0(1,RF),0(RF)       ANY NUMBER                                   
         BZ    PCT30NX             NO, SKIP                                     
         MVC   BYTE,0(RF)          PCT NUMBER                                   
         MVC   SVPCODE,BYTE                                                     
*                                                                               
         CP    0(6,R6),=P'0'       ANY AMOUNT                                   
         BE    PERCENTY                                                         
*                                                                               
         LA    RF,STARTWRK                                                      
         ZICM  R0,PCTSPCT,2        DISP TO PERCENTAGE                           
         AR    RF,R0                                                            
         ZAP   PCTAMT,0(6,RF)                                                   
         ZAP   WORK(12),0(6,R6)      AMOUNT                                     
         MP    WORK(12),PCTAMT                                                  
         CP    WORK(12),=P'0'                                                   
         BE    PERCENTY                                                         
         DP    WORK(12),=P'1000000'                                             
         TM    BIT,UPDKEEP         UPDATING KEEP AMOUNT                         
         BZ    PCT35                                                            
         ZAP   PCTAMT1,WORK(8)                                                  
         B     PERCENTY                                                         
PCT35    ZAP   PCTAMT2,WORK(8)                                                  
*                                                                               
PERCENTY B     XYES                                                             
PERCENTN B     XNO                                                              
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
*&&UK                                                                           
***********************************************************************         
*        UPDATE X'20' -  GENERAL NAME ELEMENT (FROM SCREEN) FOR UK              
*        SAVED AS LAST FIRST IN UK (NO COMMA)                                   
***********************************************************************         
*                                                                               
         USING NAMELD,R6                                                        
         USING PEBLKD,R3                                                        
ELEM20UK NMOD1 0,**EL20UK                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVI   ELCODE,NAMELQ       X'20'                                        
         GOTO1 REMELEM             REMOVE ANY IF THERE                          
*                                                                               
         MVC   WORK,SPACES         BUILD NAME IN WORK                           
         LA    R4,WORK                                                          
         LA    R2,0                LEN SO FAR                                   
*                                                                               
         ZIC   R1,PENLNME          LAST NAME                                    
         AHI   R1,-1                                                            
         BM    ERRMISS                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PELNAME     MOVE IN LAST NAME                            
*                                                                               
         LA    R2,1(R1,R2)         R2=LENGTH SO FAR                             
         AR    R4,R2               BUMP R4 POINTER IN WORK                      
         LA    R4,1(R4)            LEAVE A SPACE BTN FIRST & LA                 
         LA    R2,1(R2)            ADD 1 TO LEN                                 
*                                                                               
         ZIC   R1,PENFNME                                                       
         AHI   R1,-1                                                            
         BM    ERRMISS                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PEFNAME     MOVE IN FIRST NAME                           
         B     EL25UK                                                           
*                                                                               
EL25UK   LA    R2,1(R1,R2)         ADD TO LEN                                   
         CHI   R2,36               MAX LEN                                      
         BNH   *+8                                                              
         LA    R2,36                                                            
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NAMEL,NAMELQ        X'20'                                        
         AHI   R2,NAMLN1Q          ADD HEADER TO LEN                            
         STC   R2,NAMLN                                                         
         SH    R2,=Y(NAMLN1Q+1)                                                 
         BM    EL20UKX                                                          
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   NAMEREC(0),WORK                                                  
         GOTO1 ADDELEM                                                          
EL20UKX  B     XIT                                                              
         DROP  R6,R3                                                            
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                     *         
***********************************************************************         
*                                                                               
*        DDSPOOLD                                                               
*        DDSPLWORKD                                                             
*        FASECRETD                                                              
*        ACCAPWORKD                                                             
*        ACCAPDSECT                                                             
*        ACGENFILE                                                              
*        ACDDEQUS                                                               
*        DDPERVALD                                                              
*        ACRAPPERD                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE DDPERVALD                                                      
COBLOCKD DSECT                                                                  
       ++INCLUDE ACCAPBLOCK                                                     
RAPPERD  DSECT                                                                  
       ++INCLUDE ACRAPPERD                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SCREENS                                                      *         
***********************************************************************         
*                                                                               
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPD4D                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                          *         
***********************************************************************         
*                                                                               
STARTWRK DS    0F                                                               
RELO     DS    A                                                                
SAVERC   DS    F                   SAVED RC                                     
APCTSTAB DS    F                   A(PCTSTAB)                                   
VRAPPER  DS    F                   V(ACRAPPER)                                  
PAYEND   DS    F                   END OF PAYROLL CODE TABLE                    
SVADDR   DS    F                   SAVE ADDRESS                                 
TABCOUNT DS    H                   COUNT OF TABLE ENTRIES                       
STDISP   DS    H                   START DISPLACEMENT                           
PRVSTDSP DS    H                   PREVIOUS START DISPLACEMENT                  
DLINE1   DS    H                   DISPLACEMENT TO LINE 1                       
*                                                                               
SALDATE  DS    XL3                                                              
NXTDATE  DS    XL3                                                              
STDATE   DS    PL3                                                              
ENDATE   DS    PL3                                                              
INDATE   DS    CL3                 PWOS YEAR AND MONTH                          
SVDATE   DS    XL2                 SAVED MOA FROM TABLE                         
SVPCDTE  DS    XL3                 SAVED DATE FROM PC1/PC2 ENTRY                
SALLOCK  DS    XL3                 SALARY LOCK DATE OF OLD LOCATION             
TEMPOFF  DS    CL2                 TEMPORARY OFFICE FIELD                       
TEMPDEPT DS    CL6                     "     DEPT     "                         
TEMPSDPT DS    CL6                     "     SUBDPT   "                         
TEMPPER  DS    CL8                     "     PERSON   "                         
PERSON   DS    CL8                 PERSON CODE                                  
OFFICE   DS    CL2                 OFFICE CODE FROM OLD LOC                     
DEPT     DS    CL3                 DEPARTMENT CODE FROM OLD LOC                 
SUBDPT   DS    CL3                 SUB DEPARTMENT CODE FROM OLD LOC             
OFFICE2  DS    CL2                 OFFICE CODE FROM NEW LOC                     
DEPT2    DS    CL6                 DEPARTMENT CODE FROM NEW LOC                 
SUBDPT2  DS    CL6                 SUB DEPARTMENT CODE FROM NEW LOC             
*                                                                               
PCDNUM   DS    XL1                 PAYROLL CODE NUMBER                          
PCTAMT   DS    PL6                                                              
PCTAMT1  DS    PL6                                                              
PCTAMT2  DS    PL6                                                              
REVAMT1  DS    PL6                                                              
REVAMT2  DS    PL6                                                              
SVAMT    DS    PL8                 SAVED TOTAL AMOUNT                           
SVPCODE  DS    CL1                                                              
SVREVCD  DS    CL5                 SAVED REVERSAL CODE                          
FIRSTIME DS    CL1                 INDICATES FIRST TIME IN PROGRAM              
*                                                                               
BIT      DS    XL1                                                              
VLAMT1   EQU   X'80'               VALIDATING KEEP AMOUNT                       
DELHIS   EQU   X'40'               DELETING HISTORY(HIDE SOME FIELDS)           
HISTELEM EQU   X'20'               KEEPING SOME HISTORY IN OLD LOCATION         
BMPTAB   EQU   X'10'               BUMP TABLE TO NEXT ENTRY                     
NEWREC   EQU   X'08'               BIT TO ADD A NEW RECORD                      
DUPLOC   EQU   X'04'               NEW LOCATION IS A DUPLICATE                  
UPDKEEP  EQU   X'02'               UPDATING KEEP AMOUNT AT THIS TIME            
TOTALS   EQU   X'01'               UPDATING TOTAL AMOUNT                        
*                                                                               
BIT2     DS    XL1                                                              
UPOLD1R  EQU   X'80'               UPDATING OLD LOC'S 1R                        
UPNEW1R  EQU   X'40'               UPDATING/ADDING NEW LOC'S 1R                 
*                                                                               
PC1PCT   DS    PL6                                                              
PC2PCT   DS    PL6                                                              
*                                                                               
SEQNUM   DS    XL1                                                              
PSEQNUM  DS    XL1                                                              
LEVELLN  DS    0CL4                LENGTHS OF ALL LEVELS                        
LEVELLNA DS    CL1                 LENGTH OF A                                  
LEVELLNB DS    CL1                 LENGTH OF B                                  
LEVELLNC DS    CL1                 LENGTH OF C                                  
LEVELLND DS    CL1                 LENGTH OF D                                  
ACCNT    DS    CL12                MY TEMP ACCOUNT FIELD                        
SVACCNT  DS    CL12                SAVE 1R ACCOUNT OF OLD LOCATION              
SVACCNT2 DS    CL12                SAVE 1R ACCOUNT OF NEW LOCATION              
SAVEKEY  DS    XL42                ACCFILE KEY                                  
SAVEKEY2 DS    XL42                ACCFILE KEY                                  
KEY2     DS    XL70                ACCFILE KEY                                  
MAXCOUNT EQU   75                  MAX NUMBER OF TABLE ENTRIES                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
* 4,080 BYTES AVAILABLE IN SYSSPARE                                             
BLOCKSD  DSECT                     BLOCKS PUT IN DISPBLK                        
ELNAME   DS    CL8                                                              
ELEMBLK  DS    75CL(ELLEN)                                                      
ELEMLNQ  EQU   *-ELNAME                                                         
*                                                                               
* PEBLK IS USED TO HOLD THE INFORMATION PASSED BY THE PERSON PROGRAM            
*                                                                               
PEBLK    DS    CL150                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY LINE DSECT                                                     
***********************************************************************         
*                                                                               
DSPLINED DSECT                     DISPLAY LINE DSECT                           
DSPDATEH DS    CL8                                                              
DSPDATE  DS    CL8                 DATE                                         
DSPCODEH DS    CL8                                                              
DSPCODE  DS    CL5                 PAY CODE                                     
DSPINDH  DS    CL8                                                              
DSPIND   DS    CL2                 TYPE INDICATOR                               
DSPTOTH  DS    CL8                                                              
DSPTOT   DS    CL11                AMOUNT+ADJUSTMENT                            
DSPAMT1H DS    CL8                                                              
DSPAMT1  DS    CL11                AMOUNT TO KEEP(ACTION DEL AND MOVE)          
DSPAMT2H DS    CL8                                                              
DSPAMT2  DS    CL11                AMOUNT TO TRANSFER (MOVE)                    
DSPLLEN  EQU   *-DSPLINED                                                       
         EJECT                                                                  
***********************************************************************         
*        PERCENTAGE TABLE DSECT                                                 
***********************************************************************         
*                                                                               
PCTSTABD DSECT                                                                  
PCTSNUM  DS    XL2                 DISP TO PCT NUMBER IN '84' ELEM              
PCTSBIT  DS    XL1                 BIT ON IN PDESTAT IN '86' ELEM               
PCTSNUMB DS    XL2                 DISP TO PCT NUMBER IN PAYCDTAB               
PCTSPCT  DS    XL2                 DISP TO PERCENTAGE VALUE                     
PCTSPCTD DS    XL2                 DISP TO PERCENT VALUE IN PROFILES            
PCTSLEN  EQU   *-PCTSTABD                                                       
         EJECT                                                                  
***********************************************************************         
*        PAYROLL CODES AND NUMBERS DSECT                                        
***********************************************************************         
*                                                                               
PAYCDTAB DSECT                     TABLE OF '85' ELEMS                          
PAYCDNUM DS    XL1                 PAY CODE NUMBER                              
PAYCDNME DS    XL5                 PAYROLL CODE                                 
PAYCDREV DS    XL5                 PAYROLL REVERSAL CODE                        
PAYCDPCS DS    0XL2                PCTS  ******* CHANGE WHEN ADDING PCS         
PAYCDPC1 DS    XL1                 PAYROLL PC1 NUMBER                           
PAYCDPC2 DS    XL1                 PAYROLL PC2 NUMBER                           
PAYCDST  DS    XL1                 STATUS (EQUS AS IN ELEM)                     
PAYCDLEN EQU   *-PAYCDTAB                                                       
PCTABLN  EQU   255*PAYCDLEN                                                     
         EJECT                                                                  
***********************************************************************         
*        TABLE OF 86 ELEMS DSECT                                                
***********************************************************************         
*                                                                               
ELEMTABD DSECT                     TABLE OF '86' ELEMS                          
ELSEQ    DS    XL1                 SEQ NUMBER OF RECORD FOR THIS ELEM           
ELDATE   DS    XL3                 DATE                                         
ELNUM    DS    XL1                 PAYROLL CODE NUMBER                          
ELTOT    DS    PL8                 TOTAL AMT OF ORIGINAL & ADJUSTMENT           
ELAMT1   DS    PL6                 UPDATE AMOUNT (DEL AND MOVE)                 
ELAMT2   DS    PL6                 TRANSFER AMOUNT (MOVE)                       
ELCHKENT EQU   *-ELEMTABD          CHECK FOR ENTRY UP TO THIS POINT             
ELSTAT   DS    XL1                 DATE STATUS                                  
ELDESCLN DS    XL1                 DESC LENGTH                                  
ELDESC   DS    CL21                DESCRIPTION                                  
ELSTAT2  DS    XL1                 DATE STATUS                                  
ELLEN    EQU   *-ELEMTABD                                                       
*                                                                               
***********************************************************************         
*        INFO BLOCK PASSED FROM PERSON PROGRAM                                  
***********************************************************************         
PEBLKD   DSECT                                                                  
PENAME   DS    CL8                                                              
PETABLN  DS    XL1                 TABLE LENGTH                                 
PENLNME  DS    XL1                 LENGTH OF LAST NAME                          
PELNAME  DS    CL36                LAST NAME                                    
PENFNME  DS    XL1                 LENGTH OF FIRST NAME                         
PEFNAME  DS    CL36                FIRST NAME                                   
PEHIREDT DS    XL3                 HIRE DATE                                    
PETERMDT DS    XL3                 TERMINATION DATE                             
PESTART  DS    XL3                 START DATE                                   
PELOCS   DS    0C                                                               
PESTAT   DS    XL1                 STATUS                                       
PEFILTS  DS    0XL5                                                             
PEFILT1  DS    XL1                 FILTER 1                                     
PEFILT2  DS    XL1                 FILTER 2                                     
PEFILT3  DS    XL1                 FILTER 3                                     
PEFILT4  DS    XL1                 FILTER 4                                     
PEFILT5  DS    XL1                 FILTER 5                                     
PELSTAT  DS    XL1                                                              
PENEWLOC EQU   X'80'               THIS ENTRY IS THE NEW LOCATION               
PELOCNQ  EQU   *-PELOCS                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACCAP01A  03/07/03'                                      
         END                                                                    
