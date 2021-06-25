*          DATA SET CTSFM16    AT LEVEL 061 AS OF 05/01/02                      
*PHASE TA0A16A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A16 - ON-LINE DARE REP TABLE MAINT/LIST                  *         
*                                                                     *         
*  COMMENTS: MAINTAINS DARE REP TABLE RECORDS                         *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMC1 (TA0AC1) -- MAINTENANCE                    *         
*                  CTSFMC2 (TA0AC2) -- LIST                           *         
*                                                                     *         
*  OUTPUTS: UPDATED DARE REP TABLE RECORDS                            *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - 2ND BASE                                              *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
         TITLE 'TA0A16 DARE REP TABLE RECORDS'                                  
TA0A16   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A16*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         OI    GENSTAT5,NODLST     NO DELETING FROM THE LIST SCREEN             
*                                                                               
         CLI   MODE,RECDEL         DELETE THE RECORD?                           
         BNE   MAIN10                                                           
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
         DROP  RF                                                               
         MVC   GERROR,=AL2(216)    RECORD/ACTION COMBO INVALID                  
         LA    R2,CONACTH                                                       
         B     SFMERROR                                                         
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VK                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+16                                                             
         BAS   RE,VR                                                            
         BAS   RE,DR                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BAS   RE,DR                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DK                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BNE   *+12                                                             
         BAS   RE,LR                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,PRINTREP       ONLINE LIST RECORDS                          
         BNE   *+12                                                             
         BAS   RE,PR                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   VALIDATE KEY                                                                
*                                                                               
VK       NTR1                                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TODAYDTE)                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STAKEYD,R4                                                       
*                                                                               
         CLI   ACTNUM,ACTLIST      DO TESTS IF LIST                             
         BE    VK2                                                              
         CLI   ACTNUM,ACTREP       SKIP TESTS IF REPORT                         
         BNE   VK5                                                              
*                                                                               
VK2      OC    SFLMEDA,SFLMEDA                                                  
         BNZ   VK50                                                             
         B     VK10                                                             
*                                                                               
VK5      CLI   SFMMEDAH+5,1        IS THE MEDIA 1 CHAR?                         
         BNE   VK10                                                             
         CLI   SFMMEDAH+8,C'T'     IS THE MEDIA T?                              
         BE    VK15                                                             
         CLI   SFMMEDAH+8,C'R'        OR  MEDIA R?                              
         BE    VK15                                                             
*                                                                               
VK10     MVC   GERROR,=AL2(300)                                                 
         LA    R2,SFMMEDAH                                                      
         B     SFMERROR                                                         
*                                                                               
VK15     LA    R2,SFMSTATH                                                      
         XC    BLOCK1,BLOCK1                                                    
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK1),C',=,-'                             
         LA    R6,BLOCK1                                                        
*                                                                               
         CLI   0(R6),0                                                          
         BE    VK20                                                             
         CLI   0(R6),3                                                          
         BL    VK20                                                             
         CLI   0(R6),4                                                          
         BH    VK20                                                             
         TM    2(R6),X'40'                                                      
         BZ    VK20                                                             
         MVC   STAKSTIN(4),12(R6)                                               
*                                                                               
         CLI   1(R6),0             ANY BAND?                                    
         BNE   VK18                YES, CHECK IT                                
         CLI   SFMMEDA,C'R'        NO, ARE WE DOING RADIO?                      
         BE    VK20                    YES, NEED THE BAND                       
         MVC   STAKSTIN+4(1),SFMMEDA   NO, BAND IS THE MEDIA                    
         B     VK50                                                             
*                                                                               
VK18     CLI   SFMMEDA,C'R'        RADIO?                                       
         BE    VK19                                                             
         CLI   22(R6),C'T'                                                      
         BE    *+12                                                             
         CLI   22(R6),C'L'                                                      
         BNE   VK20                                                             
         MVC   STAKSTIN+4(1),22(R6)                                             
         B     VK50                                                             
*                                                                               
VK19     CLI   22(R6),C'A'                                                      
         BE    *+12                                                             
         CLI   22(R6),C'F'                                                      
         BNE   VK20                                                             
         MVC   STAKSTIN+4(1),22(R6)                                             
         B     VK50                                                             
*                                                                               
VK20     MVC   GERROR,=AL2(308)                                                 
         LA    R2,SFMSTATH                                                      
         B     SFMERROR                                                         
*                                                                               
VK50     MVI   STAKSYS,STAKSYSQ    BUILD KEY...                                 
         MVI   STAKTYP,STAKTYPQ                                                 
         MVC   STAKMEDA,SFMMEDA    ...FOR MAINTENANCE                           
         MVC   TEMPEXT,STAKSTIN+4  CALL LETTER EXTENSION                        
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK55                                                             
         CLI   ACTNUM,ACTREP                                                    
         BNE   EXIT                                                             
*                                                                               
         XC    LUPDATE,LUPDATE     ACTIVITY DATE FILTER FOR REPORT ONLY         
         CLI   SFRDATEH+5,0                                                     
         BE    VK51                NO                                           
*                                                                               
         ZIC   R2,SFRDATEH+5        INPUT LENGTH                                
         GOTO1 PERVAL,DMCB,((R2),SFRDATE),(X'20',PERVALST)                      
         TM    4(R1),X'01'+X'02'   VALID DATE?                                  
         BNZ   VKDERR              NO                                           
         MVC   LUPDATE(6),PERVALST+28                                           
         B     VK51                                                             
VKDERR   MVC   GERROR,=AL2(330)    INVALID DATE OR DATE RANGE                   
         LA    R2,SFRDATEH                                                      
         B     SFMERROR                                                         
*                                                                               
VK51     CLI   SFRSORTH+5,0        ANY SORT?                                    
         BE    VK55                NO                                           
*                                                                               
         ZIC   R5,SFRSORTH+5                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   SFRSORT(0),=C'REP'  SORT ON CURRENT REP?                         
         BNE   VK52                                                             
         OI    MYFLAG,SORTREP      YES-SORT ON REP                              
         MVC   SAVESORT,=C'REP   '                                              
         B     VK55                                                             
VK52     EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   SFRSORT(0),=C'STATUS'   SORT ON STATUS?                          
         BNE   VK54                                                             
         OI    MYFLAG,SORTSTAT     YES-SORT ON STATUS                           
         MVC   SAVESORT,=C'STATUS'                                              
         B     VK55                                                             
*                                                                               
VK54     MVC   GERROR,=AL2(INVALID)                                             
         LA    R2,SFRSORTH                                                      
         B     SFMERROR                                                         
*                                                                               
VK55     MVC   STAKMEDA,SFLMEDA    ..OR FOR LIST                                
         XC    STAKSTIN,STAKSTIN                                                
         MVC   STAKSTIN,SFLSTAT    CHECK FILTERS                                
*                                                                               
         TM    SFLFILTH+4,X'20'                                                 
         BNZ   EXIT                                                             
         LA    R2,SFLFILTH                                                      
         XC    REP,REP                                                          
         XC    PREP,PREP                                                        
         XC    BLOCK1,BLOCK1                                                    
         CLI   SFLFILTH+5,0                                                     
         BE    EXIT                                                             
         GOTO1 SCANNER,DMCB,(R2),BLOCK1                                         
         LA    R5,BLOCK1                                                        
         ZIC   R6,DMCB+4                                                        
*                                                                               
VK60     CLC   =C'REP',12(R5)                                                   
         BNE   *+14                                                             
         MVC   REP,22(R5)                                                       
         B     VK65                                                             
         CLC   =C'PREP',12(R5)                                                  
         BNE   *+14                                                             
         MVC   PREP,22(R5)                                                      
         B     *+14                                                             
         MVC   GERROR,=AL2(210)                                                 
         B     SFMERROR                                                         
*                                                                               
VK65     LA    R5,32(R5)                                                        
         BCT   R6,VK60             GET OUT WHEN NO MORE FILTERS                 
*                                                                               
         OC    REP,REP                                                          
         BZ    VK80                                                             
         LA    R2,REP                                                           
*                                                                               
VK69     LA    R5,REPIDS           DARE TABLE OF CODES                          
*                                                                               
VK70     CLC   0(3,R2),0(R5)                                                    
         BE    VK80                                                             
         LA    R5,L'REPIDS(R5)                                                  
         CLC   X'FF',0(R5)         GOTO ERROR IF END OF TABLE                   
         BNE   VK70                                                             
         LA    R2,SFLFILTH                                                      
         MVC   GERROR,=AL2(BADREP)                                              
         B     SFMERROR                                                         
*                                                                               
VK80     CLC   0(3,R2),PREP                                                     
         BE    VKX                                                              
         OC    PREP,PREP                                                        
         BZ    VKX                                                              
         LA    R2,PREP                                                          
         B     VK69                                                             
*                                                                               
VKX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*   VALIDATE RECORD                                                             
*                                                                               
VR       NTR1                                                                   
*                                                                               
         XC    OLDREP,OLDREP                                                    
         XC    OLDPREV,OLDPREV                                                  
         L     R3,AIO                                                           
         USING STAREPD,R3                                                       
         MVI   ELCODE,STAREPCQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VR2                 MUST BE A REP ELEMENT                        
         USING STAREPD,R3          REP ELEMENT DSECT                            
         MVC   OLDREP,STAREPCR     SAVE OLD REP VALUES                          
         MVC   OLDPREV,STAREPPR                                                 
*                                                                               
         L     R3,AIO                                                           
         USING STAKEYD,R3                                                       
         MVI   ELCODE,STAREPCQ     REMOVE REP ELEMENT TO REBUILD IT             
         GOTO1 REMELEM                                                          
         MVI   ELCODE,STASTACQ     REMOVE NEW STATION ELEMENT                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,STAHOMCQ     REMOVE MARKET ELEMENT CODE                   
         GOTO1 REMELEM                                                          
*                                                                               
VR2      XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING STAREPD,R3          REP ELEMENT DSECT                            
         MVI   STAREPC,STAREPCQ                                                 
         MVI   STAREPLN,STAREPLQ                                                
*                                                                               
* AN 'X' IN DATE MEANS CLEAR PREVIOUS REP FIELD                                 
*                                                                               
         LA    R2,SFMCREPH                                                      
         CLI   SFMDATE,C'X'        TEST DATE INPUT IS 'X'                       
         BNE   VR10                NO                                           
         MVC   GERROR,=AL2(NOPRVREP)                                            
         OC    OLDPREV,OLDPREV     TEST ANY PREVIOUS REP                        
         BZ    SFMERROR            NO - ERROR                                   
         MVC   STAREPCR,OLDPREV    MOVE PREVIOUS TO CURRENT                     
         B     VR80                                                             
*                                                                               
VR10     CLI   ACTNUM,ACTADD                                                    
         BE    VR20                                                             
*=======================================================*                       
* ACTION IS CHANGE                                                              
*=======================================================*                       
         SPACE 1                                                                
         CLC   OLDREP,SFMCREP      TEST REP HAS CHANGED                         
         BNE   VR12                YES                                          
         MVC   STAREPCR,OLDREP     RESTORE OLD REP VALUES                       
         MVC   STAREPPR,OLDPREV                                                 
         B     VR30                                                             
         SPACE 1                                                                
*=========================================================*                     
* REP HAS CHANGED - EFFECTIVE DATE MUST BE INPUT                                
*=========================================================*                     
         SPACE 1                                                                
VR12     CLI   SFMDATEH+5,0        TEST DATE INPUT                              
         BE    VR50                NO - ERROR                                   
         LA    R2,SFMCREPH                                                      
         BAS   RE,CHECK                                                         
         MVC   STAREPCR,SFMCREP                                                 
         MVC   STAREPPR,OLDREP                                                  
         B     VR30                                                             
         EJECT                                                                  
*=======================================================*                       
* ACTION IS ADD                                                                 
*=======================================================*                       
         SPACE 1                                                                
VR20     LA    R2,SFMCREPH         VALIDATE CURRENT REP                         
         BAS   RE,CHECK                                                         
         MVC   STAREPCR,SFMCREP                                                 
*                                                                               
VR30     CLI   SFMDATEH+5,0        TEST DATE INPUT                              
         BE    VR80                NO                                           
*                                                                               
VR40     LA    R2,SFMDATEH                                                      
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         GOTO1 PERVAL,DMCB,(SFMDATEH+5,SFMDATE),(X'60',PERVALST)                
         LA    R6,PERVALST                                                      
         USING PERVALD,R6                                                       
         TM    DMCB+4,X'02'                                                     
         BZ    VR60                                                             
*                                                                               
VR50     MVC   GERROR,=AL2(BADDATE)   INVALID DATE                              
         LA    R2,SFMDATEH                                                      
         B     SFMERROR                                                         
*                                                                               
VR60     GOTO1 DATCON,DMCB,(0,PVALESTA),(15,STAREPED)                           
         EJECT                                                                  
*===============================================================                
* NEED TO SET DARE/NOT DARE CODES FOR CURRENT/PREVIOUS REPS                     
*===============================================================                
         SPACE 1                                                                
VR80     DS    0H                                                               
         LA    R1,STAREPCR         POINT TO CURRENT REP                         
         BAS   RE,GETSTAT                                                       
         MVC   STAREPST,STATUS     SET CURRENT REP DARE STATUS                  
*                                                                               
         LA    R1,STAREPPR         POINT TO PREVIOUS REP                        
         BAS   RE,GETSTAT                                                       
         TM    STATUS,X'80'                                                     
         BZ    *+8                                                              
         OI    STAREPST,X'40'      SET PREVIOUS NOT A DARE REP                  
*                                                                               
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*=============================================================                  
* EDIT NEW STATION CALL LETTERS AND EFFECTIVE DATE                              
*=============================================================                  
         SPACE 1                                                                
VR90     DS    0H                                                               
         LA    R2,SFMNSTAH                                                      
         CLI   5(R2),0                                                          
         BE    VR110                                                            
         LA    R2,SFMNSDTH         NO CALL LETTERS, NO DATE ALLOWED             
         CLI   5(R2),0                                                          
         BNE   VR92                                                             
         MVC   GERROR,=AL2(BADDATE)   INVALID DATE                              
         B     SFMERROR                                                         
*                                                                               
VR92     MVC   GERROR,=AL2(INVSTA)                                              
         LA    R2,SFMNSTAH                                                      
         CLI   5(R2),3             SHOULD HAVE 3 OR 4 CALL LETTERS              
         BL    SFMERROR                                                         
* BUILD NEW STATION ELEMENT                                                     
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING STASTAD,R3          REP ELEMENT DSECT                            
         MVI   STASTAC,STASTACQ                                                 
         MVI   STASTALN,STASTALQ                                                
         MVC   STASTA,8(R2)                                                     
         CLI   12(R2),C'-'                                                      
         BNE   VR92A                                                            
         MVC   STASTA+4(1),13(R2)                                               
         B     VR93                                                             
*                                                                               
VR92A    DS    0H                                                               
         CLI   11(R2),C'-'                                                      
         BNE   VR93                                                             
         MVI   STASTA+3,C' '                                                    
         MVC   STASTA+4(1),12(R2)                                               
*                                                                               
VR93     DS    0H                                                               
         CLI   STASTA+4,0                                                       
         BNE   *+10                                                             
         MVC   STASTA+4(1),TEMPEXT      CALL LETTER EXTENSION                   
         OC    STASTA,SPACES                                                    
*                                                                               
* MAKE SURE NEW CALL LETTERS ARE NOT THE SAME AS STATION CALL LETTERS           
         CLC   STASTA(4),SFMSTAT     SAME CALL LETTERS W/O THE POWER?           
         BNE   VR93C                                                            
         CLI   SFMMEDA,C'R'            RADIO?                                   
         BNE   VR93A                                                            
         CLC   STASTA+4(1),SFMSTAT+5   YES, SAME BAND?                          
         BE    SFMERROR                                                         
         B     VR93C                                                            
*                                      NO, TELEVISION                           
VR93A    CLI   SFMSTAT+5,C'L'              LOW POWER?                           
         BNE   VR93B                                                            
         CLI   STASTA+4,C'L'                                                    
         BE    SFMERROR                ERROR IF BOTH STA ARE THE SAME           
         B     VR93C                                                            
*                                                                               
VR93B    CLI   STASTA+4,C'T'       NO, TELEVISION                               
         BE    SFMERROR                ERROR IF BOTH STA ARE THE SAME           
*                                                                               
VR93C    LA    R2,SFMNSDTH         VALIDATE DATE (REQUIRED)                     
         CLI   5(R2),0                                                          
         BE    VR94                                                             
         MVC   GERROR,=AL2(BADDATE)                                             
         GOTO1 PERVAL,DMCB,(SFMNSDTH+5,SFMNSDT),(X'60',PERVALST)                
         LA    R6,PERVALST                                                      
         USING PERVALD,R6                                                       
         TM    DMCB+4,X'02'                                                     
         BO    SFMERROR                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,PVALESTA),(15,STASTADT)                           
*                                                                               
VR94     DS    0H                  ADD ELEMENT TO RECORD                        
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*=================================================================              
* EDIT HOME MARKET ELEMENT                                                      
*=================================================================              
                                                                                
VR110    DS    0H                                                               
*                                                                               
         CLI   SFMRIDH+5,0         ANY RECEIVING ID?                            
         BNE   VR110A                                                           
         CLI   SFMCITYH+5,0        ANY HOME MARKET CITY?                        
         BE    VR100                                                            
         MVC   GERROR,=AL2(MISSING)                                             
         LA    R2,SFMRIDH                                                       
         B     SFMERROR                                                         
*                                                                               
VR110A   CLI   SFMCITYH+5,0        ANY HOME MARKET CITY?                        
         BNE   VR111                                                            
         MVC   GERROR,=AL2(MISSING)                                             
         LA    R2,SFMCITYH                                                      
         B     SFMERROR                                                         
*                                                                               
VR111    CLI   SFMCITYH+5,2        2 CHARACTERS LONG?                           
         BE    VR112                                                            
         MVC   GERROR,=AL2(INVALID)                                             
         LA    R2,SFMCITYH                                                      
         B     SFMERROR                                                         
*                                                                               
VR112    TM    SFMCITYH+4,X'04'    ALPHABETIC?                                  
         BO    VR114                                                            
         MVC   GERROR,=AL2(NOTALPHA)                                            
         LA    R2,SFMCITYH                                                      
         B     SFMERROR                                                         
*                                                                               
VR114    XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING STAHOMD,R3          HOME MARKET ELEMENT DSECT                    
         MVI   STAHOMC,STAHOMCQ                                                 
         MVI   STAHOMLN,STAHOMLQ                                                
         MVC   STAHOMCT,SFMCITY                                                 
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SFMRID                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,MYIO                  
         LA    R4,MYIO                                                          
         OC    SFMRID,=C'          '                                            
         CLC   SFMRID,CTIKID                                                    
         BE    VR116                                                            
         MVC   GERROR,=AL2(INVALID)                                             
         LA    R2,SFMRIDH                                                       
         B     SFMERROR                                                         
*                                                                               
VR116    MVC   STAHOMIC,CTIKID                                                  
*                                                                               
         LA    R4,CTIDATA                                                       
VRID     CLI   0(R4),X'00'         END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R4),X'02'                                                      
         BE    VRIDYES                                                          
         ZIC   R5,1(R4)            LENGTH                                       
         AR    R4,R5                                                            
         B     VRID                                                             
*                                                                               
VRIDYES  MVC   STAHOMIB,2(R4)      HEXADECIMAL ID                               
*                                                                               
VR120    DS    0H                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SAVEKEY                                                      
*                                                                               
VR100    DS    0H                                                               
         MVC   CONHEAD,=C'ACTION COMPLETED  ***CALL COMPUTER ROOM TO RE+        
               FRESH DARE***  '                                                 
         OI    CONHEADH+6,X'80'    TRANSMIT                                     
         B     EXIT                                                             
         EJECT                                                                  
*================================================================               
* LOOK UP STATUS BYTE FOR DARE/NOT DARE                                         
*================================================================               
         SPACE 1                                                                
GETSTAT  MVI   STATUS,0                                                         
         LA    R5,REPIDS                                                        
*                                                                               
GETST2   CLC   0(3,R1),0(R5)                                                    
         BE    GETST4                                                           
         LA    R5,L'REPIDS(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   GETST2                                                           
         BR    RE                                                               
*                                                                               
GETST4   TM    13(R5),X'20'        TEST NOT A DARE REP                          
         BZ    *+8                                                              
         MVI   STATUS,STAREPST_ND  SET STATUS TO NOT DARE                       
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   DISPLAY RECORD                                                              
*                                                                               
DR       NTR1                                                                   
*                                                                               
         L     R4,AIO              CHECK KEY SAME AS RECORD                     
         USING STAKEYD,R4                                                       
*                                                                               
         XC    SFMCREP,SFMCREP                                                  
         OI    SFMCREPH+6,X'80'                                                 
         XC    SFMPREP,SFMPREP                                                  
         OI    SFMPREPH+6,X'80'                                                 
         XC    SFMDATE,SFMDATE                                                  
         OI    SFMDATEH+6,X'80'                                                 
         XC    SFMDND,SFMDND                                                    
         OI    SFMDNDH+6,X'80'                                                  
         XC    SFMNSTA,SFMNSTA                                                  
         OI    SFMNSTAH+6,X'80'                                                 
         XC    SFMNSDT,SFMNSDT                                                  
         OI    SFMNSDTH+6,X'80'                                                 
         XC    SFMCITY,SFMCITY                                                  
         OI    SFMCITYH+6,X'80'                                                 
         XC    SFMRID,SFMRID                                                    
         OI    SFMRIDH+6,X'80'                                                  
*                                                                               
         CLC   KEY(STAKLENQ),STAKEY                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO                                                           
         USING STAREPD,R3                                                       
         MVI   ELCODE,STAREPCQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE A REP ELEMENT                        
         DC    H'0'                                                             
*                                                                               
         MVC   SFMCREP,STAREPCR    DISPLAY REP ELEMENT                          
         OI    SFMCREPH+6,X'80'                                                 
*                                                                               
         MVC   SFMPREP,STAREPPR    PUT RECORD TO SCREEN                         
         OI    SFMPREPH+6,X'80'                                                 
         OC    STAREPED,STAREPED                                                
         BZ    DR20                                                             
         GOTO1 DATCON,DMCB,(6,STAREPED),(5,SFMDATE)                             
         OI    SFMDATEH+6,X'80'                                                 
*                                                                               
DR20     OI    SFMDNDH+6,X'80'                                                  
         MVC   SFMDND,=CL8'* DARE *'                                            
         TM    STAREPST,STAREPST_ND  TEST NOT DARE REP                          
         BZ    *+10                                                             
         MVC   SFMDND,=CL8'NOT DARE'                                            
         SPACE 1                                                                
         EJECT                                                                  
*==========================================================                     
* DISPLAY NEW CALL LETTER DATA IF ELEMENT PRESENT                               
*==========================================================                     
         SPACE 1                                                                
         L     R3,AIO                                                           
         USING STAREPD,R3                                                       
         MVI   ELCODE,STASTACQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DR30                                                             
         USING STASTAD,R3                                                       
*                                                                               
         MVC   SFMNSTA,STASTA                                                   
         OI    SFMNSTAH+6,X'80'                                                 
*                                                                               
         OC    STASTADT,STASTADT                                                
         BZ    DRX                                                              
         GOTO1 DATCON,DMCB,(6,STASTADT),(5,SFMNSDT)                             
         OI    SFMNSDTH+6,X'80'                                                 
         SPACE 1                                                                
         EJECT                                                                  
*==========================================================                     
* DISPLAY HOME MARKET CITY AND RECEIVING ID                                     
*==========================================================                     
DR30     L     R3,AIO                                                           
         USING STAHOMD,R3                                                       
         MVI   ELCODE,STAHOMCQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         MVC   SFMCITY,STAHOMCT                                                 
         OI    SFMCITYH+6,X'80'                                                 
         MVC   SFMRID,STAHOMIC                                                  
         OI    SFMRIDH+6,X'80'                                                  
*                                                                               
DRX      DS    0H                                                               
         L     R3,AIO              POINT TO EDICT RECORD                        
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   DRXX                NONE PRESENT                                 
*                                                                               
         USING ACTVD,R3                                                         
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(11,LUPDATE)                            
         XC    DMCB,DMCB                                                        
         GOTO1 GETTXT,DMCB,28,0,(C'I',0),(11,LUPDATE)                           
         OI    GENSTAT2,USMYOK                                                  
         DROP  R3                                                               
*                                                                               
DRXX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   LIST RECORDS                                                                
*                                                                               
LR       NTR1                                                                   
         LA    R4,KEY                                                           
         USING STAKEYD,R4                                                       
*                                                                               
         OC    KEY(STAKSTIN-STAKEY),KEY                                         
         BNZ   LR10                                                             
*                                                                               
         MVI   STAKSYS,STAKSYSQ    BUILD SEARCH KEY USE INPUT                   
         MVI   STAKTYP,STAKTYPQ                                                 
         CLI   SFLMEDAH+5,1        IS THE MEDIA 1 CHAR?                         
         BNE   VK10                                                             
         CLI   SFLMEDA,C'T'        IS THE MEDIA T?                              
         BE    *+12                                                             
         CLI   SFLMEDA,C'R'           OR  MEDIA R?                              
         BNE   VK10                                                             
         MVC   STAKMEDA,SFLMEDA    BUILD SEARCH KEY                             
         OC    SFLSTAT,SFLSTAT                                                  
         BZ    *+10                                                             
         MVC   STAKSTIN,SFLSTAT                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                                                             
*                                                                               
LR20     CLC   KEY(STAKSTIN-STAKEY),SAVEKEY                                     
         BNE   LR70                                                             
*                                                                               
         XC    LISTAR,LISTAR                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         LA    R3,STAFSTEL                                                      
         USING STAREPD,R3                                                       
*                                                                               
         MVI   ELCODE,STAREPCQ                                                  
         BAS   RE,FIRSTEL          MUST BE AN INFO ELEMENT                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR30     OC    REP,REP           CHECK TO MATCH FILTERS                         
         BZ    *+14                                                             
         CLC   REP,STAREPCR                                                     
         BNE   LR50                                                             
         OC    PREP,PREP                                                        
         BZ    *+14                                                             
         CLC   PREP,STAREPPR                                                    
         BNE   LR50                                                             
*                                                                               
         MVC   LSTMEDIA,STAKMEDA                                                
         MVC   LSTSTATN,STAKSTIN                                                
         MVC   LSTREP,STAREPCR                                                  
         MVC   LSTPREP,STAREPPR                                                 
         OC    STAREPED,STAREPED                                                
         BZ    LR32                                                             
         GOTO1 DATCON,DMCB,(6,STAREPED),(5,LSTEFDT)                             
*                                                                               
LR32     LA    R3,STAFSTEL                                                      
         MVI   ELCODE,STASTACQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   LR34                                                             
*                                                                               
         USING STASTAD,R3          REP ELEMENT DSECT                            
         MVC   LSTNSTA,STASTA                                                   
         GOTO1 DATCON,DMCB,(6,STASTADT),(5,LSTNSDT)                             
*                                                                               
LR34     LA    R3,STAFSTEL                                                      
         MVI   ELCODE,STAHOMCQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   LR40                                                             
*                                                                               
         USING STAHOMD,R3          MARKET ELEMENT DSECT                         
         MVC   LSTMKT,STAHOMCT                                                  
         MVC   LSTRID,STAHOMIC                                                  
*                                                                               
LR40     GOTO1 LISTMON             DISPLAY RECORD                               
*                                                                               
LR50     GOTO1 SEQ                 GET NEXT RECORD AND CHECK FOR MATCH          
         LA    R4,KEY                                                           
         B     LR20                                                             
*                                                                               
LR70     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* DISPLAY KEY / IN SELECT                                                       
*                                                                               
DK       NTR1                                                                   
         L     R4,AIO                                                           
         USING STAKEYD,R4                                                       
*                                                                               
         MVC   SFMMEDA,STAKMEDA    PUT KEY TO SCREEN                            
         OI    SFMMEDAH+6,X'80'                                                 
*                                                                               
         MVC   SFMSTAT(4),STAKSTIN                                              
         CLI   SFMSTAT+3,C' '                                                   
         BNE   *+18                                                             
         MVI   SFMSTAT+3,C'-'                                                   
         MVC   SFMSTAT+4(1),STAKSTIN+4                                          
         B     DK10                                                             
         MVI   SFMSTAT+4,C'-'                                                   
         MVC   SFMSTAT+5(1),STAKSTIN+4                                          
*                                                                               
DK10     OI    SFMSTATH+6,X'80'                                                 
         MVC   TEMPEXT,STAKSTIN+4  CALL LETTER EXTENSION                        
         B     EXIT                                                             
         EJECT                                                                  
*================================================================               
* CHECK REP                                                                     
*================================================================               
CHECK    NTR1                                                                   
         MVC   GERROR,=AL2(BADREP)                                              
         MVI   STATUS,0                                                         
         CLI   5(R2),0                                                          
         BE    SFMERROR                                                         
*                                                                               
         LA    R5,REPIDS                                                        
CHECK1   CLC   8(3,R2),0(R5)                                                    
         BE    CHECK2                                                           
         LA    R5,L'REPIDS(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   CHECK1                                                           
         B     SFMERROR                                                         
*                                                                               
CHECK2   TM    13(R5),X'20'        TEST NOT A DARE REP                          
         BZ    CHECKX                                                           
         MVI   STATUS,STAREPST_ND  SET STATUS TO NOT DARE                       
*                                                                               
CHECKX   XIT1                                                                   
         EJECT                                                                  
*                                                                               
* PRINT REPORTS                                                                 
*                                                                               
PR       NTR1                                                                   
         LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
*                                                                               
         CLI   SFRSORTH+5,0        ANY SORT?                                    
         BE    PR2                 NO                                           
         CLI   WHEN,X'40'          SORT ON NOW?                                 
         BNE   PRSORT              NO                                           
         MVC   GERROR,=AL2(INVALID)                                             
         LA    R2,SFRSORTH                                                      
         B     SFMERROR                                                         
*                                                                               
PRSORT   OI    MYFLAG,CALLSORT     CALLED SORTER                                
         GOTO1 SORTER,DMCB,SCARDR,RECCARDR                                      
*                                                                               
PR2      LA    R4,KEY                                                           
         USING STAKEYD,R4                                                       
*                                                                               
         MVI   STAKSYS,STAKSYSQ    BUILD SEARCH KEY USE INPUT                   
         MVI   STAKTYP,STAKTYPQ                                                 
*                                                                               
         CLI   SFRMEDAH+5,1        IS THE MEDIA 1 CHAR?                         
         BNE   VK10                                                             
         CLI   SFRMEDA,C'T'        IS THE MEDIA T?                              
         BNE   *+14                                                             
         MVC   MEDNAME,=C'TELEVISION'                                           
         B     PR5                                                              
         CLI   SFRMEDA,C'R'           OR  MEDIA R?                              
         BNE   VK10                                                             
         MVC   MEDNAME,=C'RADIO'                                                
                                                                                
PR5      MVC   STAKMEDA,SFRMEDA    BUILD SEARCH KEY                             
         OC    SFRSTAT,SFRSTAT                                                  
         BZ    *+10                                                             
         MVC   STAKSTIN,SFRSTAT                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
PR10     GOTO1 HIGH                                                             
*                                                                               
PR20     CLC   KEY(STAKSTIN-STAKEY),SAVEKEY                                     
         BE    PR22                                                             
         CLI   SFRSORTH+5,0        ANY SORT?                                    
         BNE   PR31                YES                                          
         B     PR70                                                             
*                                                                               
PR22     XC    P,P                                                              
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         OC    LUPDATE,LUPDATE     DATE FILTER PRESENT?                         
         BZ    PR30                NO                                           
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   PR50                NONE PRESENT SKIP IT                         
*                                                                               
         USING ACTVD,R3                                                         
         CLC   ACTVCHDT,LUPDATE    UPDATED BEFORE FILTER START DATE?            
         BL    PR50                YES - SKIP IT                                
         OC    LUPDATE+3(3),LUPDATE+3    FILTER ON END DATE                     
         BZ    PR30                                                             
         CLC   ACTVCHDT,LUPDATE+3  UPDATED AFTER FILTER END DATE?               
         BH    PR50                YES - SKIP IT                                
         DROP  R3                                                               
*                                                                               
PR30     L     R4,AIO                                                           
         LA    R3,STAFSTEL         FIRST ELEMENT                                
         USING STAREPD,R3          REP ELEMENT                                  
*                                                                               
         MVI   ELCODE,STAREPCQ     REP ELEMENT                                  
         BAS   RE,FIRSTEL          MUST BE AN INFO ELEMENT                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    REP,REP           CHECK TO MATCH FILTERS                         
         BZ    *+14                                                             
         CLC   REP,STAREPCR                                                     
         BNE   PR50                                                             
*                                                                               
         OC    PREP,PREP                                                        
         BZ    *+14                                                             
         CLC   PREP,STAREPPR                                                    
         BNE   PR50                                                             
*                                                                               
         CLI   SFRSORTH+5,0        ANY SORT?                                    
         BE    PR32                NO                                           
*                                                                               
         L     R4,AIO                                                           
         MVC   RECLEN,STARECLN     RECORD LENGTH                                
         USING SORTKEYD,R4                                                      
         MVC   SORTLEN,RECLEN      RECORD LENGTH                                
         MVC   SORTDARE,STAREPST   DARE OR NOT DARE STATUS                      
         MVC   SORTCREP,STAREPCR   CURRENT REP                                  
         DROP  R4                                                               
         USING STAKEYD,R4                                                       
         GOTO1 SORTER,DMCB,=C'PUT',AIO                                          
         B     PR50                                                             
                                                                                
PR31     GOTO1 SORTER,DMCB,=C'GET'                                              
         OC    DMCB+4(4),DMCB+4    ANY MORE SORTED RECORDS?                     
         BZ    PR70                NO                                           
         L     R3,DMCB+4                                                        
         L     R4,DMCB+4                                                        
         USING STAKEYD,R4                                                       
         LA    R3,STAFSTEL         FIRST ELEMENT                                
         USING STAREPD,R3          REP ELEMENT                                  
*                                                                               
         LA    R3,STAFSTEL         FIRST ELEMENT                                
         MVI   ELCODE,STAREPCQ     REP ELEMENT                                  
         BAS   RE,FIRSTEL          MUST BE AN INFO ELEMENT                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SFRSORT,C'S'        SORT ON STATUS?                              
         BNE   PR31A                                                            
         CLC   PREVSTAT,STAREPST   PREVIOUS STATUS = CURRENT STATUS?            
         BE    PR32                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PREVSTAT,STAREPST                                                
         B     PR32                                                             
*                                                                               
PR31A    CLC   PREVREP,STAREPCR    PREVIOUS REP = CURRENT REP?                  
         BE    PR32                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PREVREP,STAREPCR    PREVIOUS REP                                 
*                                                                               
PR32     MVC   RPTSTATN,STAKSTIN                                                
         MVC   RPTREP,STAREPCR                                                  
         MVC   RPTPREP,STAREPPR                                                 
*                                                                               
         MVC   RPTSTAT,=C'* DARE *'                                             
         TM    STAREPST,STAREPST_ND TEST NOT DARE REP                           
         BZ    *+10                                                             
         MVC   RPTSTAT,=C'NOT DARE'                                             
*                                                                               
         OC    STAREPED,STAREPED                                                
         BZ    PR35                                                             
         GOTO1 DATCON,DMCB,(6,STAREPED),(5,RPTEFDT)                             
*                                                                               
PR35     LA    R3,STAFSTEL         FIRST ELEMENT                                
         MVI   ELCODE,STASTACQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   PR47                                                             
*                                                                               
         USING STASTAD,R3          REP ELEMENT DSECT                            
         MVC   RPTNSTA,STASTA                                                   
         GOTO1 DATCON,DMCB,(6,STASTADT),(5,RPTNSDT)                             
*                                                                               
PR47     LA    R3,STAFSTEL         FIRST ELEMENT                                
         MVI   ELCODE,STAHOMCQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   PR40                                                             
*                                                                               
         USING STAHOMD,R3          HOME MARKET ELEMENT DSECT                    
         MVC   RPTMKT,STAHOMCT                                                  
         MVC   RPTRID,STAHOMIC                                                  
*                                                                               
PR40     GOTO1 SPOOL,DMCB,(R8)     DISPLAY RECORD                               
         CLI   SFRSORTH+5,0        ANY SORT?                                    
         BNE   PR31                YES                                          
*                                                                               
PR50     GOTO1 SEQ                 GET NEXT RECORD AND CHECK FOR MATCH          
         LA    R4,KEY                                                           
         B     PR20                                                             
*                                                                               
PR70     TM    MYFLAG,CALLSORT     CALLED SORTER?                               
         BZ    EXIT                                                             
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* HEAD HOOK                                                                     
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H1+55(22),=C'DSTATION RECORD REPORT'                             
         MVC   H4+8(L'MEDNAME),MEDNAME MEDIA                                    
*                                                                               
         CLI   SFRFILTH+5,0        ANY FILTERS?                                 
         BE    HD10                NO                                           
         MVC   H5(8),=C'FILTERS:'                                               
         MVC   H5+10(L'SFRFILT),SFRFILT FILTERS                                 
*                                                                               
HD10     CLI   SFRSORTH+5,0        ANY SORTS?                                   
         BE    HDHOOKX             NO                                           
         MVC   H6(8),=C'SORT BY:'                                               
         MVC   H6+10(L'SAVESORT),SAVESORT                                       
*                                                                               
HDHOOKX  B     EXIT                                                             
*                                                                               
HEDSPECS SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H4,1,C'MEDIA:'                                                   
         SSPEC H8,2,C'STATION'                                                  
         SSPEC H8,12,C'REP'                                                     
         SSPEC H8,19,C'STATUS'                                                  
         SSPEC H8,29,C'EFF DATE'                                                
         SSPEC H8,41,C'PREV'                                                    
         SSPEC H8,47,C'NEW STA'                                                 
         SSPEC H8,55,C'EFF DATE'                                                
         SSPEC H8,66,C'MKT'                                                     
         SSPEC H8,71,C'USER ID'                                                 
         DC    X'00'                                                            
*                                                                               
ERR      OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
*                                                                               
SFMERROR GOTO1 SFMERR                                                           
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
SAVESORT DS    CL6                 SAVE SORT FIELD FROM SCREEN                  
PREVREP  DS    CL3                 PREVIOUS REP                                 
PREVSTAT DS    XL1                 PREVIOUS STATUS                              
SCARDR   DC    CL80'SORT FIELDS=(5,4,A),FORMAT=BI,WORK=1'                       
RECCARDR DC    CL80'RECORD TYPE=V,LENGTH=2000'                                  
*                                                                               
         GETEL  R3,DATADISP,ELCODE                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENDTBD                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMC1D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMC2D                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   SYSSPARE                                                         
MYIO     DS    1000X                                                            
*                                                                               
         EJECT                                                                  
* ONLINE LIST (1 LINE)                                                          
SPOOLD   DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL2                                                              
LSTREP   DS    CL3                 REP                                          
         DS    CL2                                                              
LSTREPN  DS    CL10                REP NAME                                     
         DS    CL2                                                              
LSTREPX  DS    CL10                REP PREFIX                                   
         DS    CL2                                                              
LSTFLAGS DS    CL15                FLAGS                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061CTSFM16   05/01/02'                                      
         END                                                                    
