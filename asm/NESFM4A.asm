*          DATA SET NESFM4A    AT LEVEL 051 AS OF 10/31/05                      
*PHASE T31C4AA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T31C4A  -- OFFICE RECORD MAINTENANCE                 *         
*                                                                     *         
*  COMMENTS:     MAINTAINS OFFICE RECORDS ON SPFILE                   *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T31C00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS NESFMG1, AND NESFMG2                         *         
*                                                                     *         
*  OUTPUTS:      UPDATED OFFICE RECORDS                               *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T31C4A - OFFICE RECORD MAINTENANCE'                             
T31C4A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1C4A**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
         CLI   MODE,RECDEL                                                      
         BE    RCDL                LIST RECORDS                                 
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       DS    0H                                                               
         MVC   OFFICE,SPACES                                                    
         MVC   CLIENT,SPACES                                                    
         MVC   SUPV,SPACES                                                      
*                                                                               
         NI    MYFLAG,X'FF'-FROMVR                                              
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK50                                                             
*                                                                               
         LA    R2,OFLOFLH                                                       
         CLI   5(R2),0             DATA IN OFFICE FIELD?                        
         BE    VK10                NO SKIP                                      
         MVC   OFFICE,OFLOFL                                                    
         OC    OFFICE,SPACES                                                    
*                                                                               
VK10     LA    R2,OFLSUPH                                                       
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         MVC   SUPV,OFLSUP                                                      
         OC    SUPV,SPACES                                                      
*                                                                               
VK20     LA    R2,OFLCLIH                                                       
         CLI   5(R2),0                  IS CLIENT FIELD BLANK?                  
         BNE   VK25                     NO, STORE CLIENT INPUT                  
         CLC   SUPV,SPACES              CLIENT BLANK, IS SUPV BLANK?            
         BE    VK100                    YES, GOTO LISTRECS                      
         BAS   RE,OFFSUPV               SUPV NOT BLANK, CLIENT BLANK            
         B     VK100                                                            
*                                                                               
VK25     MVC   CLIENT,OFLCLI                                                    
         OC    CLIENT,SPACES                                                    
*                                                                               
         LA    R2,OFLMEDH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         BAS   RE,OFFCLI                GET OFFICE BY CLIENT                    
         B     VK100                                                            
*                                                                               
VK50     DS    0H             VALIDATE FOR DISPLAY/ADD/CHANGE                   
         LA    R2,OFDOFDH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         CLI   8(R2),C' '               FIRST CHAR CANNOT BE BLANK              
         BNH   ERRINV                                                           
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK59                                                             
         LA    R1,OFDOFD                VALID ALPHA NUMERIC                     
         LA    R0,2                                                             
VK54     CLI   0(R1),C' '                                                       
         BL    VK59                                                             
         CLI   0(R1),C'A'                                                       
         BL    ERRINV                                                           
         CLI   0(R1),C'9'                                                       
         BH    ERRINV                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VK54                                                          
*                                                                               
VK59     MVC   OFFICE,OFDOFD                                                    
         OC    OFFICE,SPACES                                                    
*                                                                               
VK100    DS    0H                                                               
         LA    R2,OFDMEDH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,OFLMEDH                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VK105                                                            
         MVI   8(R2),C'N'                                                       
         MVI   5(R2),X'01'                                                      
         OI    6(R2),X'80'                                                      
         B     VK110                                                            
*                                                                               
VK105    CLI   8(R2),C'N'                                                       
         BNE   ERRINV                                                           
*                                                                               
VK110    DS    0H                                                               
         GOTO1 VALIMED                                                          
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD OFFICE KEY                             
         USING OFCRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   OFCKTYP,=X'0D60'                                                 
         MVC   OFCKAGY,AGYX                                                     
         MVC   OFCKOFC,OFFICE      OFFICE CODE                                  
*                                                                               
         CLC   SAVEKEY,KEY              HAS CHANGED SINCE LAST TIME?            
         BE    *+8                      NO                                      
         OI    STATUS,KEYCHG            YES                                     
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         DROP  R4                                                               
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VKX                                                              
*                                                                               
VKX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         OI    MYFLAG,FROMVR                                                    
*                                                                               
         TM    OFDMNDH+4,X'20'     VALIDATED BEFORE?                            
         BO    VRX                                                              
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
         MVI   ELCODE,X'01'        OFFICE NAME ELEMENT                          
         GOTO1 REMELEM                                                          
*        GOTO1 HELLO,DMCB,(C'D',SPTFILE),(X'01',AIO),0                          
*                                                                               
VR10     LA    R5,ELEM                                                          
         USING OFCNAMEL,R5                                                      
         XC    ELEM,ELEM                                                        
         MVI   OFCNAMEL,X'01'      ELEMENT CODE                                 
         MVI   OFCNAMLN,26         ELEMENT LENGTH                               
*                                                                               
         LA    R2,OFDMNDH          CHECK MANAGER FIELD                          
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         MVC   OFCNAM,OFDMND                                                    
         OC    OFCNAM,SPACES                                                    
*                                                                               
         GOTO1 ADDELEM                                                          
*        GOTO1 HELLO,DMCB,(C'P',SPTFILE),AIO,(R5),0                             
*        CLI   12(R1),0            TEST FOR ERROR                               
*        BE    *+6                                                              
*        DC    H'00'                                                            
*                                                                               
         OI    OFDMNDH+4,X'20'                                                  
*                                                                               
VRX      DS    0H                                                               
         B     DR                  REDISPLAY RECORD                             
SPTFILE  DC    CL8'SPTFILE'                                                     
         DROP  R5                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         OI    GENSTAT2,RETEQSEL                                                
*                                                                               
         L     R6,AIO                                                           
         USING OFCRECD,R6                                                       
         MVC   OFDMND,SPACES                                                    
*                                                                               
         MVC   OFDMND,OFCNAM       DISPLAY MANAGER                              
         OI    OFDMNDH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP THE REST ON ADD                         
         BE    DRXX                                                             
*                                                                               
         LA    R2,OFDLISH                                                       
         CLI   5(R2),0                                                          
         BNE   DR10                                                             
         MVI   8(R2),C'S'                                                       
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
DR10     CLI   8(R2),C'S'                                                       
         BNE   DR50                                                             
         TM    4(R2),X'80'              INPUT THIS TIME?                        
         BNO   DR11                                                             
         XC    STARTSPV,STARTSPV                                                
         XC    FRSTSPV,FRSTSPV                                                  
         XC    LASTSPV,LASTSPV                                                  
*                                                                               
DR11     LA    R2,OFDMEDH                                                       
*                                                                               
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'                                                      
*&&DO                                                                           
         CLI   5(R2),0                                                          
         BE    DR15                                                             
*                                                                               
         LA    R4,MEDTAB                                                        
*                                                                               
DR12     CLC   OFDMED,0(R4)                                                     
         BE    DR13                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   DR12                                                             
         B     ERRINV                                                           
*                                                                               
DR13     MVC   MEDELTX,0(R4)            SAVE MEDIA FOR SUPV DISPLAY             
*&&                                                                             
*                                                                               
         MVI   MEDELTX,C'N'                                                     
         USING SPVRECD,R4                                                       
DR15     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,AGYX                                                     
         MVC   SPVKOFC,OFFICE                                                   
*                                                                               
         BAS   RE,CLRSCR                                                        
*                                                                               
         MVC   OFDSTRT,SUHEAD                                                   
         OI    OFDSTRTH+6,X'80'                                                 
*                                                                               
         CLI   PFAID,8                  DOWN?                                   
         BNE   DR20                     NO CHECK FOR PF6= TOP                   
         CLI   OFDENDH+7,0              LAST FIELD BLANK?                       
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTSPV,FRSTSPV          YES,DON'T PAGE DOWN                     
         MVC   STARTSPV,LASTSPV                                                 
         B     DR25                                                             
*                                                                               
DR20     CLI   PFAID,6                  TOP?                                    
         BNE   *+10                                                             
         XC    FRSTSPV,FRSTSPV          START AT FIRST CLIENT KEY               
*                                                                               
         MVC   STARTSPV,FRSTSPV         IF ANY OTHER PFAID,START                
*                                                                               
DR25     XC    FRSTSPV,FRSTSPV                                                  
         XC    LASTSPV,LASTSPV                                                  
*                                                                               
         TM    STATUS,KEYCHG            DID KEY CHANGE?                         
         BNO   *+10                     NO, DISPLAY FROM STARTCLI               
         XC    STARTSPV,STARTSPV        YES, DISPLAY FROM FIRST CLIENT          
*                                                                               
         MVC   SPVKSPV,STARTSPV                                                 
         DROP  R4                                                               
*                                                                               
         USING LISD,R3                                                          
         LA    R3,OFDSELH                                                       
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         B     DR35                                                             
DR30     GOTO1 SEQ                                                              
DR35     GOTO1 GETREC                                                           
*                                                                               
         USING SPVRECD,R4                                                       
         L     R4,AIO                                                           
         CLC   =X'0D61',SPVKTYP                                                 
         BNE   DR100                    WE ARE DONE WITH SUPV REC'S             
         CLC   OFFICE,SPVKOFC                                                   
         BNE   DR100                                                            
         MVC   SPVCD,SPVKSPV                                                    
         DROP  R4                                                               
*                                                                               
         LA    R1,OFDSELH               ARE WE AT FIRST SELECT FIELD?           
         CR    R3,R1                                                            
         BNE   *+10                     NO                                      
         MVC   FRSTSPV,SPVCD            YES, STORE FIRST SUPV                   
*                                                                               
         MVC   LASTSPV,SPVCD            STORE LAST SUPV EVERY TIME              
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
         XC    BLOCK(100),BLOCK                                                 
         USING SPVNAMED,R6                                                      
         MVC   BLOCK(L'SPVLNAME),SPVLNAME                                       
         MVC   BLOCK+16(L'SPVFNAME),SPVFNAME                                    
         GOTO1 SQUASHER,DMCB,BLOCK,(C',',L'SPVNM)                               
         MVC   SPVNM,BLOCK                                                      
         LA    R1,LSDSLQ(R3)                                                    
         OI    6(R1),X'80'                                                      
         DROP  R3                                                               
*                                                                               
         LA    R3,LSDLQ(R3)                                                     
         LA    R1,OFDENDH                                                       
         CR    R3,R1                                                            
         BNH   DR30                                                             
         B     DR100                                                            
*                                                                               
DR50     CLI   8(R2),C'C'                                                       
         BNE   ERRINV                                                           
         TM    4(R2),X'80'                                                      
         BNO   DR55                                                             
         XC    STARTCLI,STARTCLI                                                
         XC    FRSTCLI,FRSTCLI                                                  
         XC    LASTCLI,LASTCLI                                                  
*                                                                               
DR55     BAS   RE,NOSELECT                                                      
         BAS   RE,CLRSCR                                                        
*                                                                               
         MVC   OFDSTRT,CLHEAD                                                   
         OI    OFDSTRTH+6,X'80'                                                 
*                                                                               
         LA    R2,OFDMEDH                                                       
         TM    4(R2),X'80'                                                      
         BNO   DR56                                                             
         XC    STARTCLI,STARTCLI                                                
         XC    FRSTCLI,FRSTCLI                                                  
         XC    LASTCLI,LASTCLI                                                  
*                                                                               
DR56     CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
**** MAKE VALIMED CALL SO CAN MAKE VALICLT CALL ****                            
         MVI   BYTE,C'A'                                                        
         MVC   SAVEKEY2,KEY                                                     
         MVC   AIO,AIO2                                                         
*        MVI   USEIONUM,2               DON'T USE IO AREA OF RECORD             
         GOTO1 VALIMED                  MAKES A DM CALL                         
*                                                                               
         LA    R4,MEDTAB                                                        
*                                                                               
DR60     CLC   OFDMED,0(R4)                                                     
         BE    DR65                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   DR60                                                             
         B     ERRINV                                                           
*                                                                               
DR65     MVC   MEDELCD,1(R4)                                                    
*                                                                               
         LA    R4,KEY                                                           
         USING SPVPASS,R4                                                       
         XC    KEY,KEY                                                          
         MVC   SPVPTYP,=X'0DE1'                                                 
         MVC   SPVPAM,BAGYMD                                                    
         MVC   SPVPOFC,OFFICE                                                   
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
*PFAID STUFF                                                                    
         CLI   PFAID,8                  DOWN?                                   
         BNE   DR67                     NO CHECK FOR PF6= TOP                   
         CLI   OFDENDH+7,0              LAST FIELD BLANK?                       
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTCLI,FRSTCLI          YES,DON'T PAGE DOWN                     
         MVC   STARTCLI,LASTCLI                                                 
         B     DR69                                                             
*                                                                               
DR67     CLI   PFAID,6                  TOP?                                    
         BNE   *+10                                                             
         XC    FRSTCLI,FRSTCLI          START AT FIRST CLIENT KEY               
*                                                                               
         MVC   STARTCLI,FRSTCLI         IF ANY OTHER PFAID,START                
*                                                                               
DR69     XC    FRSTCLI,FRSTCLI                                                  
         XC    LASTCLI,LASTCLI                                                  
*                                                                               
         TM    STATUS,KEYCHG            DID KEY CHANGE?                         
         BNO   *+10                     NO, DISPLAY FROM STARTCLI               
         XC    STARTCLI,STARTCLI        YES, DISPLAY FROM FIRST CLIENT          
*                                                                               
         MVC   SPVPCLT,STARTCLI         COMPLETE THE KEY                        
         DROP  R4                                                               
*                                                                               
         USING LISD,R3                                                          
         LA    R3,OFDSELH                                                       
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         B     DR75                                                             
*                                                                               
DR70     MVC   KEY(L'SAVEKEY2),SAVEKEY2                                         
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
*                                                                               
         USING SPVPASS,R4                                                       
DR75     LA    R4,KEY                                                           
         CLC   SAVEKEY2(5),KEY                                                  
         BNE   DR100                                                            
*                                                                               
         MVC   CLC,SPVPCLT                                                      
         MVC   CLISUPV,SPVPSPV                                                  
*                                                                               
         LA    R1,OFDSELH               ARE WE AT FIRST SELECT FIELD?           
         CR    R3,R1                                                            
         BNE   *+10                     NO                                      
         MVC   FRSTCLI,CLC              YES, STORE FIRST CLIENT                 
*                                                                               
         MVC   LASTCLI,CLC              STORE LAST CLIENT EVERY TIME            
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD(8),=X'0B00000000030000'                                  
         MVC   TEMPFLD+8(3),SPVPCLT                                             
         MVC   SAVEKEY2,KEY                                                     
         LA    R2,TEMPFLD                                                       
         MVC   AIO,AIO3                                                         
         MVI   BYTE,C'A'                                                        
*        MVI   USEIONUM,3               NEED AIO2 FOR GETEL CALLS               
         GOTO1 VALICLT                                                          
         MVC   AIO,AIO2                                                         
         DROP  R4                                                               
*                                                                               
         MVC   CLNM,CLTNM                                                       
         LA    R1,LSDSLQ(R3)                                                    
         OI    6(R1),X'80'                                                      
         DROP  R3                                                               
*                                                                               
         LA    R3,LSDLQ(R3)                                                     
         LA    R1,OFDENDH                                                       
         CR    R3,R1                                                            
         BNH   DR70                                                             
*                                                                               
DR100    DS    0H                                                               
         TM    MYFLAG,FROMVR       FROM VALREC                                  
         BZ    DR110                                                            
*                                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BE    *+12                                                             
DR110    CLI   ACTNUM,ACTCHA                                                    
         BNE   DRX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         L     RF,AIO1                                                          
         MVC   KEY(13),0(RF)                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
DRX      MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
*                                                                               
         BAS   RE,CHKSEL                ROUTINE TO CHECK SELECT FIELDS          
         NI    STATUS,X'FF'-KEYCHG      ALWAYS TURN OFF KEYCHG BIT              
DRXX     B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING OFCRECD,R6                                                       
*                                                                               
         MVC   OFFICE,OFCKOFC                                                   
         MVC   OFDOFD,OFCKOFC                                                   
         OI    OFDOFDH+6,X'80'                                                  
         MVC   SAVEKEY,0(R6)                                                    
*                                                                               
DKX      CLI   ACTNUM,ACTSEL                                                    
         BNE   *+10                                                             
         MVC   SAVESEL,THISLSEL                                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*ONLINE LIST                                                                    
*                                                                               
LR       DS    0H                                                               
         XC    OCTABL(200),OCTABL                                               
*                                                                               
         CLC   SUPV,SPACES                                                      
         BNE   LR01                                                             
         CLC   CLIENT,SPACES                                                    
         BE    LR02               NO FILTERS?                                   
*                                                                               
LR01     BAS   RE,CHKFLT          CHECK SUPERVISOR FILTER                       
*                                                                               
LR02     LA    R6,KEY                                                           
         USING OFCRECD,R6                                                       
         OC    KEY,KEY        IS THIS FIRST TIME AT LIST SCREEN?                
         BNZ   LR10           NO, DO NOT BUILD KEY                              
*                                                                               
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*                                                                               
         MVC   OFCKTYP,=X'0D60'                                                 
         MVC   OFCKAGY,AGYX                                                     
         MVC   OFCKOFC,OFFICE                                                   
*                                                                               
LR10     GOTO1 HIGH               FIND FIRST DIRECTORY REC                      
         CLC   SUPV,SPACES                                                      
         B     LR30                                                             
*                                                                               
LR20     GOTO1 SEQ                FIND SUBSEQUENT DIRECTORY RECS                
         LA    R6,KEY                                                           
*                                                                               
LR30     DS    0H                                                               
         CLC   KEY(3),KEYSAVE     IF PAST OFFICE RECORDS...                     
         BNE   LRX                STOP READING RECORDS                          
LR40     LA    R3,OCTABL                                                        
         CLI   0(R3),X'00'         EMPTY TABLE MEANS NO FILTER                  
         BNE   LR50                                                             
         CLC   SUPV,SPACES         OR THE FILTER DOESN'T EXIST                  
         BNE   LRX                                                              
         CLC   CLIENT,SPACES                                                    
         BNE   LRX                                                              
         B     LR60                                                             
LR50     CLI   0(R3),X'FF'         END OF TABLE? -> {CODE != FILTER}            
         BE    LR20                THEN SKIP THIS RECORD FROM LIST              
         CLC   OFCKOFC,0(R3)                                                    
         BE    LR60                                                             
         LA    R3,2(R3)                                                         
         B     LR50                                                             
***                                                                             
LR60     GOTO1 GETREC             GET OFFICE DATA                               
*                                                                               
         MVC   LISTAR,SPACES      CLEAR PRINT LINE OF LIST ENTRIES              
         L     R6,AIO                                                           
         USING OFCRECD,R6                                                       
         MVC   LSOFFICE,OFCKOFC                                                 
         MVC   LSMGR,OFCNAM       PRINT OFFICE NAME                             
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*RECORD DELETE ACTION                                                           
*                                                                               
RCDL     DS    0H                       WIIL NOT ALLOW OFFICE DELETION          
         USING SPVRECD,R4               IF SUPVERVISOR RECORDS WITHIN           
         XC    KEY,KEY                  SAME OFFICE ARE PRESENT                 
         LA    R4,KEY                                                           
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,AGYX                                                     
         MVC   SPVKOFC,OFFICE                                                   
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
         GOTO1 HIGH                                                             
         B     RCDL20                                                           
*                                                                               
RCDL10   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
RCDL20   CLC   SAVEKEY2(5),KEY                                                  
         BNE   RCDL50                                                           
*                                                                               
         TM    KEY+13,X'80'        MARKED DELETED?                              
         BZ    ERRSPV                                                           
         B     RCDL10                                                           
*                                                                               
RCDL50   DS    0H                                                               
         USING BYRRECD,R4               WILL NOT DELETE OFFICE IF               
         XC    KEY,KEY                  BUYER RECORDS WITHIN SAME               
         LA    R4,KEY                   OFFICE ARE PRESENT                      
         MVC   BYRKTYP,=X'0D62'                                                 
         MVC   BYRKAGY,AGYX                                                     
         MVC   BYRKOFC,OFFICE                                                   
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
         GOTO1 HIGH                                                             
         B     RCDL70                                                           
*                                                                               
RCDL60   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
RCDL70   CLC   SAVEKEY2(5),KEY                                                  
         BNE   RCDLX                                                            
*                                                                               
         TM    KEY+13,X'80'        MARKED DELETED                               
         BZ    ERRBYR                                                           
         B     RCDL60                                                           
*                                                                               
RCDLX    MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'Y'                                                    
         B     XIT                                                              
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
ERRELEM  MVC   ERRNUM,=AL2(MISSEL)                                              
         B     SPERREX                                                          
ERRSPV   MVC   ERRNUM,=AL2(DELSPV)                                              
         B     SPERREX                                                          
ERRBYR   MVC   ERRNUM,=AL2(DELBYR)                                              
         B     SPERREX                                                          
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
SELDISPL MVI   ERRNUM,64           SELECTION DISPLAYED (W/O HIT ENTER)          
         B     INFEXIT             DISPLAY INFOMATION MESSAGE                   
*                                                                               
INFEXIT  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO1,ERRNUM                                                  
         MVI   GTMTYP,GTMINF                                                    
         MVI   GTMSYS,23                                                        
         B     VSFMERR                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
*                                                                               
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
DELSPV   EQU   437                                                              
DELBYR   EQU   438                                                              
MISSEL   EQU   471                 RECORD MISSING REQUIRED ELEM                 
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
**** ROUTINE TO BUMP TO NEXT SCREEN FIELD ****                                  
NXTSCRF  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* MATCH FILTER AGAINST SUPV PASSIVE KEY, THEN STORE OFFICE CODE      *          
**********************************************************************          
*                                                                               
CHKFLT   NTR1                                                                   
         MVC   SVKEY1,KEY                                                       
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         LA    R3,OCTABL                                                        
*                                                                               
         CLC   CLIENT,SPACES       NO CLIENT FILTER?                            
         BNE   CHKFLT35                                                         
*                                                                               
         USING SPVKEY,R2                                                        
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,AGYX                                                     
*                                                                               
         GOTO1 HIGH                                                             
         B     CHKFLT20                                                         
CHKFLT10 GOTO1 SEQ                                                              
         DS    0H                                                               
CHKFLT20 CLC   KEYSAVE(3),KEY      STILL ON SAME REC WITH SAME AGY?             
         BNE   CHKFLTX                                                          
         CLC   SPVKSPV,SUPV        MATCH FILTER?                                
         BNE   CHKFLT10                                                         
         MVC   0(2,R3),SPVKOFC     STORE OFFICE INTO TABLE                      
         LA    R3,2(R3)                                                         
         CLC   =X'FFFF',0(R3)      END OF TABLE?                                
         BNE   *+8                                                              
         B     ERRINV                                                           
         MVI   0(R3),X'FF'                                                      
         B     CHKFLT10                                                         
*                                                                               
         USING SPVPASS,R2          USE CLIENT PASSIVE KEY                       
CHKFLT35 MVC   KEY,=X'0DE1'        X'0DE1'                                      
         MVC   SPVPAM,BAGYMD       AGY/MED                                      
         GOTO1 HIGH                                                             
         B     CHKFLT50                                                         
CHKFLT40 GOTO1 SEQ                                                              
         DS    0H                                                               
CHKFLT50 CLC   KEYSAVE(3),KEY      STILL ON SAME REC WITH SAME AGY/MED?         
         BNE   CHKFLTX                                                          
         CLC   SUPV,SPACES         COMPARE SUPERVISOR CODE IF ANY               
         BE    CHKFLT60                                                         
         CLC   SPVPSPV(4),SUPV                                                  
         BNE   CHKFLT40            READ NEXT REC IF DOESN'T MATCH               
*                                                                               
CHKFLT60 CLC   CLIENT,SPACES       COMPARE CLIENT CODE IF ANY                   
         BNE   CHKFLT70                                                         
         CLC   SUPV,SPACES         WAS THERE A FILTER ON SUPV?                  
         BE    CHKFLT40            NO, NEXT RECORD                              
         B     CHKFLT80            YES,MUST HAVE QUALIFIED TO GET HERE          
CHKFLT70 CLC   SPVPCLT,CLIENT                                                   
         BNE   CHKFLT40            READ NEXT REC IF DOESN'T MATCH               
*                                                                               
CHKFLT80 MVC   0(2,R3),SPVPOFC     STORE OFFICE CODE INTO TABLE                 
         LA    R3,2(R3)                                                         
         CLC   =X'FFFF',0(R3)      END OF TABLE?                                
         BNE   *+8                                                              
         B     ERRINV                                                           
         MVI   0(R3),X'FF'                                                      
         B     CHKFLT40                                                         
*                                                                               
CHKFLTX  MVC   KEY,SVKEY1          RESTORE KEY TO WHATEVER IT WAS               
         XIT                                                                    
         DROP  R2                                                               
*                                                                               
* ROUTINE TO GET OFFICE CODE FROM SUPERVISOR RECORDS                            
*                                                                               
OFFSUPV  NTR1                                                                   
         XC    OFLMED,OFLMED            CLEAR MEDIA ENTRY IF PRESENT            
         MVI   OFLMEDH+5,0                                                      
         OI    OFLMEDH+6,X'80'                                                  
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         XC    KEY,KEY                                                          
         USING SPVRECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,AGYX                                                     
         MVC   SPVKOFC,OFFICE                                                   
         MVC   SPVKSPV,SUPV                                                     
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
         B     OFSPV15                                                          
OFSPV10  GOTO1 SEQ                                                              
OFSPV15  CLC   KEY(3),SAVEKEY           PAST CLIENT PASSIVES ON MEDIA?          
         BNE   OFSPVX                   YES, DON'T PICK UP OFFICE CODE          
*                                                                               
         CLC   KEY+5(4),SAVEKEY+5       COMPARE SUPV                            
         BL    OFSPV10                  IF LESS THAN, KEY VALUES, NEXT          
         MVC   OFFICE,SPVKOFC           GET OFFICE CODE FROM SUPV KEY           
OFSPVX   XIT                                                                    
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO GET OFFICE CODE FROM PASSIVE KEY TO SUPERVISOR VIA CLIENT          
*                                                                               
OFFCLI   NTR1                                                                   
*                                                                               
         LA    R2,OFLMEDH                                                       
         LA    R4,MEDTAB                                                        
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
OFCLI10  CLC   OFLMED,0(R4)                                                     
         BE    OFCLI15                                                          
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   OFCLI10                                                          
         B     ERRINV                                                           
*                                                                               
OFCLI15  MVC   MEDELCD,1(R4)                                                    
*                                                                               
         XC    KEY,KEY                                                          
         USING SPVRECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   SPVPTYP,=X'0DE1'                                                 
         MVC   SPVPAM,BAGYMD                                                    
         MVC   SPVPOFC,OFFICE                                                   
         MVC   SPVPCLT,CLIENT                                                   
         MVC   SPVPSPV,SUPV                                                     
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
         B     OFCLI25                                                          
OFCLI20  GOTO1 SEQ                                                              
OFCLI25  CLC   KEY(3),SAVEKEY           PAST CLIENT PASSIVES ON MEDIA?          
         BNE   OFSPVX                   YES, DON'T PICK UP OFFICE CODE          
*                                                                               
         CLC   KEY+5(7),SAVEKEY+5       COMPARE CLI AND SUPV                    
         BL    OFCLI20                  IF LESS THAN, KEY VALUES, NEXT          
         MVC   OFFICE,SPVPOFC           GET OFF CODE FROM CLI KEY               
OFCLIX   XIT                                                                    
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
**** SUBROUTINE TO CHANGE SELECT FIELDS TO PROTECTED FIELDS ****                
*                                                                               
NOSELECT NTR1                                                                   
         LA    R2,OFDSELH                                                       
*                                                                               
NS10     OI    6(R2),X'20'              CHANGE TO PROTECTED                     
         OI    6(R2),X'80'              TRANSMIT                                
*                                                                               
         BAS   RE,NXTSCRF               R2 POINTS TO DATA FIELD                 
         LA    R0,OFDENDH                                                       
         CR    R2,R0                    END OF SCREEN?                          
         BE    NSX                      YES,EXIT                                
*                                                                               
         BAS   RE,NXTSCRF               R2 POINTS TO NEXT SELECT FIELD          
         B     NS10                                                             
*                                                                               
NSX      B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        CLEARS BOTTOM OF SCREEN                                                
***********************************************************************         
*                                                                               
CLRSCR   NTR1  CLEARS             CLEARS SCREEN FROM SUPV/CLI HEADER            
         LA    R2,OFDSTRTH                                                      
         LA    R3,OFDPFKYH                                                      
*                                                                               
CLRSCR10 TM    1(R2),X'20'                                                      
         BO    CLRSCR15                                                         
         TM    6(R2),X'20'                                                      
         BNO   CLRSCR20                                                         
CLRSCR15 ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
CLRSCR20 OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CLRSCR10            NO                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
**** SUBROUTINE TO CHECK SELECT FIELDS ****                                     
*                                                                               
CHKSEL   NTR1                                                                   
         LA    R2,OFDSELH                                                       
         TM    6(R2),X'20'              CHANGED TO PROTECTED FIELD?             
         BO    CSX                      YES, NO SELECTING, EXIT                 
*                                                                               
CS10     CLI   5(R2),0                                                          
         BE    CS20                                                             
         CLI   8(R2),C'S'                                                       
         BNE   ERRINV                                                           
*                                                                               
         LR    R3,R2                                                            
         ZIC   R0,0(R2)                                                         
         AR    R3,R0                                                            
*                                                                               
         CLI   7(R3),0                  IS THERE SUPERVISOR OUTPUT?             
         BE    ERRINV                                                           
*                                                                               
         MVC   SPVSW,8(R3)                                                      
         OC    SPVSW,SPACES                                                     
         MVI   8(R2),C' '               CLEAR SELECT ENTRY                      
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
         MVI   PFAID,3                  FORCE SUPV PF KEY                       
         BAS   RE,SELSPV                                                        
*                                                                               
CS20     BAS   RE,NXTSCRF               R2 POINTS TO DATA FIELD                 
         LA    R0,OFDENDH                                                       
         CR    R2,R0                    END OF SCREEN?                          
         BE    CSX                      YES,EXIT                                
*                                                                               
CS30     BAS   RE,NXTSCRF               R2 POINTS TO NEXT SELECT FIELD          
         B     CS10                                                             
*                                                                               
CSX      B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO SELECT SUPERVISOR RECORD                                           
*                                                                               
SELSPV   NTR1                                                                   
         MVC   MPF03ACT,=C'       '                                             
         CLC   =C'SELECT',CONACT                                                
         BNE   *+10                                                             
         MVC   MPF03ACT,=CL8'DISPLAY'                                           
*                                                                               
         GOTO1 VINITPF,DMCB,MPFTABLE                                            
SLSPX    B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         XC    SPVSW,SPVSW                                                      
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         CLI   PFAID,11                                                         
         BNE   SETUP05                                                          
         OI    GENSTAT2,NEXTSEL                                                 
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         MVI   PFAID,0                                                          
         B     SETUPX                                                           
*                                                                               
SETUP05  CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    SETUPX                                                           
*                                                                               
         MVC   MPF03ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF03ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF04ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF04ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF06ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF06ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF08ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF08ACT,=CL8'DISPLAY'                                           
*                                                                               
         LR    RE,RA                ANYTHING WAITING TO RETURN TO?              
         AH    RE,=Y(TWAENDLQ-2)                                                
         CLI   1(RE),0                                                          
         BE    *+8                  NO                                          
         NI    OFDREH+1,X'FF'-X'04' LIGHT UP PF12=RETURN FIELD                  
         OI    OFDREH+6,X'80'       TRANSMIT THE RESULT                         
*                                                                               
*                                                                               
SETUP99  GOTO1 VINITPF,DMCB,MPFTABLE                                            
SETUPX   B     XIT                                                              
*==============================================================*                
         EJECT                                                                  
MPFTABLE DS    0X                                                               
* PF03 = SUPERVISOR                                                             
         DC    AL1(MPF03X-*,03,PFTCPROG,(MPF03X-MPF03)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'SUPV   '        RECORD: SUPERVISOR                           
MPF03ACT DC    CL8'       '        ACTION:                                      
MPF03    DC    AL1(KEYTYTWA,L'OFDOFD-1),AL2(OFDOFD-T31CFFD)                     
         DC    AL1(KEYTYWS,L'SPVSW-1),AL2(SPVSW-SYSSPARE)                       
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYWS,L'MEDELTX-1),AL2(MEDELTX-SYSSPARE)                   
MPF03X   EQU   *                                                                
*                                                                               
* PF04 = BUYER                                                                  
         DC    AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'BUYER  '        RECORD: BUYER                                
MPF04ACT DC    CL8'       '        ACTION:                                      
MPF04    DC    AL1(KEYTYTWA,L'OFDOFD-1),AL2(OFDOFD-T31CFFD)                     
MPF04X   EQU   *                                                                
*                                                                               
* PF06 = TOP                                                                    
         DC    AL1(MPF06X-*,06,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8' '                                                    
MPF06ACT DC    CL8' '                                                           
MPF06X   EQU   *                                                                
*                                                                               
* PF08 = DOWN                                                                   
         DC    AL1(MPF08X-*,08,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8' '                                                    
MPF08ACT DC    CL8' '                                                           
MPF08X   EQU   *                                                                
*                                                                               
* PF12 = RETURN TO CALLER                                                       
         DC    AL1(RETCALL-*,12,PFTRPROG,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
RETCALL  EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**** CONSTANTS AND TABLES                                                       
*                                                                               
CLHEAD   DC    CL79'     CODE CLIENT NAME           SUPV          CODE +        
               CLIENT NAME           SUPV'                                      
SUHEAD   DC    CL79'SEL  CODE SUPERVISOR NAME                SEL  CODE +        
               SUPERVISOR NAME'                                                 
*                                                                               
**** TABLE FOR MEDIA AND ELEMENT CODES ****                                     
MEDTAB   DS    0H                                                               
         DC    CL1'T',XL1'02'                                                   
MTABLQ   EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'N',XL1'05'                                                   
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYFILWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMA0D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMA1D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENOFC                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSPV                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBYR                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
OFFICE   DS    CL2                                                              
AGYX     DS    XL1                      AGENCY HEX, MEDIA=0                     
STATUS   DS    XL1                      STATUS BYTE FLAG                        
KEYCHG   EQU   X'80'                    KEY HAS CHANGED                         
SUPV     DS    CL4                                                              
SPVSW    DS    CL4                                                              
CLIENT   DS    CL3                                                              
STARTCLI DS    CL3                                                              
FRSTCLI  DS    CL3                                                              
LASTCLI  DS    CL3                                                              
STARTSPV DS    CL4                                                              
FRSTSPV  DS    CL4                                                              
LASTSPV  DS    CL4                                                              
TEMPFLD  DS    XL11                                                             
SAVEKEY  DS    CL13                                                             
SAVEKEY2 DS    CL13                                                             
SVKEY1   DS    CL48                A BACKUP KEY ALL TO MYSELF                   
MEDELCD  DS    XL1                                                              
MEDELTX  DS    CL1                                                              
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
*                                                                               
MYFLAG   DS    XL1                                                              
FROMVR   EQU   X'01'               CAME FROM VREC                               
*                                                                               
OCTABL   DS    100XL2              TABLE TO STORE FILTERED OFFICE CODE          
OCTABLX  DC    XL2'FFFF'                                                        
MYDMWRK  DS    12D                                                              
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
*                                                                               
**** ONLINE LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSOFFICE DS    CL2                 STAFF MEMBER NAME (IN E-MAIL FORM)           
         DS    CL6                                                              
LSMGR    DS    CL24                                                             
         EJECT                                                                  
*                                                                               
LISD     DSECT                           DSECT TO LIST OFFICE DATA              
SPVD     DS    0CL50                    LIST BY SUPERVISOR                      
LSSELH   DS    CL8                      SELECT FIELD HEADER                     
         DS    CL1                      SELECT FIELD                            
LSDSLQ   EQU   *-LSSELH                 LENGTH EQUATE FOR SELECT FIELD          
         DS    CL8                      OUTPUT FIELD HEADER                     
SPVCD    DS    CL4                      SUPERVISOR CODE                         
         DS    CL1                                                              
SPVNM    DS    CL25                     SUPERVISOR NAME                         
         DS    CL3                                                              
LSDLQ    EQU   *-LSSELH                 LENGTH OF BOTH FIELDS                   
         EJECT                                                                  
*                                                                               
         ORG   LISD                                                             
CLID     DS    0CL50                                                            
         DS    CL8                      SELECT FIELD HEADER                     
         DS    CL1                      SELECT FIELD                            
         DS    CL8                      HEADER FOR OUTPUT                       
CLC      DS    CL3                      CLIENT CODE                             
         DS    CL2                                                              
CLNM     DS    CL20                     CLIENT NAME                             
         DS    CL2                                                              
CLISUPV  DS    CL4                      SUPERVISOR CODE                         
         DS    CL2                                                              
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051NESFM4A   10/31/05'                                      
         END                                                                    
