*          DATA SET TAGEN79    AT LEVEL 004 AS OF 11/21/12                      
*PHASE T70279A,*                                                                
         TITLE 'T70279 - GCON, GCONTRK LIST/MAINTENANCE'                        
T70279   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70279,R8,R7,R5                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
         CLI   RECNUM,GCONTRK      GCONTRK?                                     
         JNE   GCON10                                                           
         GOTO1 INITIAL,DMCB,KPFTAB  INIT GCONTRK PF KEYS                        
         J     GCON30                                                           
*                                                                               
GCON10   CLI   TWASCR,SCR5D        GCON MAINT SCREEN?                           
         JE    GCON15                                                           
         GOTO1 INITIAL,DMCB,PF2TAB  INIT GCON/LIST PF KEYS                      
         J     GCON30                                                           
*                                                                               
GCON15   CLI   PFAID,0                                                          
         JE    GCON20                                                           
         LA    RE,GCOCY1H          SET CONTRACT YEARS AS FIRST VALID            
         ST    RE,AFRSTREC         PF LINES FOR THE SCREEN                      
GCON20   GOTO1 INITIAL,DMCB,PFTAB  INIT GCON PF KEYS                            
*                                                                               
GCON30   CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+12                                                             
         BAS   RE,DKEY                                                          
         J     XIT                                                              
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+12                                                             
         BAS   RE,DREC                                                          
         J     XIT                                                              
*                                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         JNE   *+12                                                             
         BAS   RE,CHKDEL           CHECK IF GCON ELIGIBLE FOR DELETE            
         J     XIT                                                              
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+12                                                             
         BAS   RE,VKEY                                                          
         J     XIT                                                              
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         JNE   *+12                                                             
         BRAS  RE,BLDREC                                                        
         J     XIT                                                              
*                                                                               
         CLI   MODE,XRECREST       RECORD RESTORED                              
         JE    GCON40                                                           
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         JE    GCON40                                                           
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         JNE   GCON50                                                           
GCON40   BAS   RE,DREC                                                          
         J     XIT                                                              
*                                                                               
GCON50   CLI   MODE,LISTRECS       LIST RECORD                                  
         JNE   GCONX                                                            
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         BAS   RE,LREC             GO LIST THE RECORDS                          
*                                                                               
GCONX    J     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROUTINE TO VALIDATE THE KEY                                                   
*                                                                               
VKEY     NTR1                                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JNE   *+12                                                             
         BRAS  RE,LVKEY                                                         
         J     XIT                                                              
*                                                                               
         XC    SVVALS(SVVALLNQ),SVVALS                                          
*                                                                               
         LA    R2,GCOSSNH          VALIDATE PID                                 
         BRAS  RE,VALPID                                                        
         GOTO1 CHAROUT,DMCB,(X'80',TANAELQ),GCONAMEH                            
         OI    GCONAMEH+6,X'80'                                                 
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         JNE   VK10                                                             
*                                                                               
         MVC   GCOCODE,=C'GC0001'  DEFAULT TO FIRST CODE                        
         MVI   GCOCODEH+5,L'GCOCODE                                             
         OI    GCOCODEH+6,X'80'                                                 
*                                                                               
         LA    R3,KEY              GET NEXT AVAILABLE GRT CONT CODE #           
         USING TLGCD,R3                                                         
         XC    TLGCKEY,TLGCKEY                                                  
         MVI   TLGCCD,TLGCCDQ      GCON RECORD                                  
         MVC   TLGCSSN,TGSSN       SOCIAL SECURITY #                            
         OI    DMINBTS,X'08'       SET READ DELETED RECORDS                     
         GOTO1 HIGH                                                             
         CLC   KEY(TLGCGCNT-TLGCKEY),KEYSAVE                                    
         JNE   VK10                                                             
         XC    TLGCGCNT,HEXFFS     UNCOMPLEMENT GRT CONTRACT CODE               
         ICM   R1,15,TLGCGCNT                                                   
         AHI   R1,1                                                             
         AHI   R1,1                BUMP TO NEXT AVAILABLE CODE #                
*                                                                               
         EDIT  (R1),GCOCODE,0,ALIGN=RIGHT                                       
         OC    GCOCODE,=C'000000'                                               
         MVC   GCOCODE(2),=C'GC'                                                
         J     VK20                                                             
*                                                                               
VK10     TM    GCOCODEH+4,X'80'                                                 
         JO    VK20                                                             
         OC    TGGCNT,TGGCNT       COMING BACK TO THIS SCREEN?                  
         JZ    VK20                                                             
         EDIT  TGGCNT,GCOCODE,0,ALIGN=RIGHT                                     
         OC    GCOCODE,=C'000000'                                               
         MVC   GCOCODE(2),=C'GC'                                                
         MVI   GCOCODEH+5,L'GCOCODE                                             
*                                                                               
VK20     LA    R2,GCOCODEH                                                      
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         CLI   5(R2),L'GCOCODE                                                  
         JNE   ERRINV                                                           
         CLC   GCOCODE(2),=C'GC'                                                
         JNE   ERRINV                                                           
*                                                                               
         LA    RF,4                L'#### OF GCON CODE                          
         LA    RE,10(R2)                                                        
VK30     CLI   0(RE),C'0'          ENSURE IT'S NUMERIC AFTER GC                 
         JL    ERRINV                                                           
         CLI   0(RE),C'9'                                                       
         JH    ERRINV                                                           
         AHI   RE,1                                                             
         BCT   RF,VK30                                                          
*                                                                               
         ZIC   R1,5(R2)                                                         
         SHI   R1,3                2 FOR 'GC' AND 1 FOR EXPACK                  
         EX    R1,*+8                                                           
         J     *+10                                                             
         PACK  DUB,10(0,R2)                                                     
         CVB   R1,DUB                                                           
         LNR   R1,R1               COMPLEMENT GRT CONTRACT CODE                 
*                                                                               
         ST    R1,SVGCNT           GRT CONTRACT CODE                            
*                                                                               
         LA    R3,KEY                                                           
         USING TLGCD,R3                                                         
         XC    TLGCKEY,TLGCKEY                                                  
         MVI   TLGCCD,TLGCCDQ      GCON RECORD                                  
         MVC   TLGCSSN,TGSSN       SOCIAL SECURITY #                            
         MVC   TLGCGCNT,SVGCNT     GRT CONTRACT CODE (COMPLEMENTED)             
         MVC   TGGCNT,TLGCGCNT                                                  
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         JE    XIT                                                              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLGCKEY),KEYSAVE                                           
         JNE   ERRNFND                                                          
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO CHECK IF GCON ELIGIBLE FOR DELETE                                  
*                                                                               
CHKDEL   NTR1                                                                   
         L     R4,AIO                                                           
         USING TLGCD,R4                                                         
         MVC   SVGCNT,TLGCGCNT                                                  
*                                                                               
         LA    R3,KEY                                                           
         USING TLOTD,R3                                                         
         XC    TLOTKEY,TLOTKEY                                                  
         MVI   TLOTCD,TLOTCDQ      CHECK IF GCONTRK RECORD EXISTS               
         MVC   TLOTSSN,TLGCSSN     SOCIAL SECURITY NUMBER                       
         MVC   TLOTGCNT,TLGCGCNT   GRT CONTRACT CODE                            
         GOTO1 HIGH                                                             
         CLC   KEY(TLOTSTRT-TLOTCD),KEYSAVE                                     
         JE    CANTDEL                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLGCKEY),0(R4)                                             
         GOTO1 HIGH                                                             
*                                                                               
         LA    R3,KEY                                                           
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY                                                  
         MVI   TLGUCD,TLGUCDQ      CHECK IF ANY GRT HAS THIS GCON CODE          
         MVC   TLGUSSN,TLGCSSN     SOCIAL SECURITY NUMBER                       
         GOTO1 HIGH                                                             
*                                                                               
CHKD10   GOTO1 SEQ                                                              
CHKD20   CLC   KEY(TLGUGUA-TLGUCD),KEYSAVE                                      
         JNE   CHKDELX                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         USING TAGUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGUELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   CHKD10                                                           
         CLC   TAGUGCNT,SVGCNT     SAME GUARANTEE CONTRACT                      
         JE    CANTDEL                                                          
         J     CHKD10                                                           
*                                                                               
CHKDELX  L     RF,AIO1             RESTORE GETREC/PUTREC FOR GCON               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLGCKEY),0(RF)                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROUTINE CONTROLS RECORD DISPLAY                                               
*                                                                               
DREC     NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(3,GCODATEH),(X'80',999)                             
         GOTO1 (RF),(R1),(1,GCOCY1H),GCOBAL5                                    
         GOTO1 (RF),(R1),(2,GCODATEH),999                                       
*                                                                               
         L     R4,AIO                                                           
         USING TAGDD,R4                                                         
         MVI   ELCODE,TAGDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(X'11',TAGDSTRT),(8,GCODATE) TERM                    
*                                                                               
         MVC   GCOAGY,TAGDAGY      AGENCY                                       
         MVC   GCOCLI,TAGDCLI      CLIENT                                       
*                                                                               
         MVC   GCOPAGY,SPACES                                                   
         MVC   GCOPAGL,SPACES                                                   
         MVC   GCOPCLI,SPACES                                                   
         MVC   GCOPCLL,SPACES                                                   
         OC    TAGDPAGY,TAGDPAGY                                                
         JZ    DR05                                                             
         MVC   GCOPAGY,TAGDPAGY    PREVIOUS AGENCY                              
         MVC   GCOPAGL,=C'Previous Agency:'                                     
*                                                                               
DR05     OC    TAGDPCLI,TAGDPCLI                                                
         JZ    DR10                                                             
         MVC   GCOPCLI,TAGDPCLI    PREVIOUS CLIENT                              
         MVC   GCOPCLL,=C'Previous Client:'                                     
*                                                                               
DR10     MVC   AIO,AIO2                                                         
         LA    R2,GCOAGYH                                                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A8',GCOAGY),GCOAGYNH                      
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'88',GCOCLI),GCOCLINH                      
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,GCOCY1H          DISPLAY CONTRACT YEARS                       
*                                                                               
         L     R4,AIO                                                           
         USING TAGCD,R4                                                         
         MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DR20     BRAS  RE,NEXTEL                                                        
         JNE   DR70                                                             
         ST    R4,CURTAGC                                                       
*                                                                               
         MVC   SVSTRT,TAGCSTRT                                                  
         MVC   SVEND,TAGCEND                                                    
         MVC   SVPNH,TAGCAMT                                                    
         MVC   SVBAL,TAGCBAL                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(X'11',TAGCSTRT),(8,8(R2))  YEAR RANGE               
         MVI   16(R2),C'-'                                                      
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         EDIT  TAGCAMT,(L'GCOLIM1,8(R2)),2,ALIGN=RIGHT,ZERO=NOBLANK,   *        
               FLOAT=-                                                          
         MVC   FULL,TAGCAMT                                                     
*                                                                               
         XC    DOLSAG,DOLSAG                                                    
         XC    DOLAFT,DOLAFT                                                    
*                                                                               
         LA    R3,KEY                                                           
         USING TLOTD,R3                                                         
         XC    TLOTKEY,TLOTKEY                                                  
         MVI   TLOTCD,TLOTCDQ                                                   
         MVC   TLOTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLOTGCNT,TGGCNT     GUARANTEE CONTRACT CODE                      
         MVC   TLOTSTRT,SVSTRT     SET START DATE                               
         GOTO1 HIGH                                                             
         J     DR40                                                             
*                                                                               
DR30     GOTO1 SEQ                                                              
DR40     CLC   KEY(TLOTSEQ-TLOTCD),KEYSAVE                                      
         JNE   DR60                                                             
         TM    TLOTSTA,TLOTSBA0                                                 
         JO    DR30                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING TAGCD,R4                                                         
         MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    TAGCUNI,TAGCUNI     P&H LIMIT ADJUSTMENT?                        
         JZ    DR30                                                             
*                                                                               
         CLC   TAGCUNI,=C'SAG'                                                  
         JNE   DR50                                                             
         ICM   RE,15,DOLSAG                                                     
         A     RE,TAGCAMT                                                       
         STCM  RE,15,DOLSAG                                                     
         J     DR30                                                             
*                                                                               
DR50     ICM   RE,15,DOLAFT        NOT SAG, MUST BE AFT                         
         A     RE,TAGCAMT                                                       
         STCM  RE,15,DOLAFT                                                     
         J     DR30                                                             
*                                                                               
DR60     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         EDIT  DOLSAG,(L'GCOSAG1,8(R2)),2,ALIGN=RIGHT,ZERO=NOBLANK,    *        
               FLOAT=-                 ACTUAL SAG                               
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         EDIT  DOLAFT,(L'GCOAFT1,8(R2)),2,ALIGN=RIGHT,ZERO=NOBLANK,    *        
               FLOAT=-                 ACTUAL AFT                               
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R4,CURTAGC                                                       
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         EDIT  TAGCBAL,(L'GCOLIM1,8(R2)),2,ALIGN=RIGHT,ZERO=NOBLANK,   *        
               FLOAT=-                                                          
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     DR20                                                             
*                                                                               
DR70     GOTO1 ACTVOUT,DMCB,GCOLCHGH   ACTIVITY                                 
*                                                                               
         CLI   MODE,XRECADD                                                     
         JNE   XIT                                                              
         MVC   CONACT,=CL8'CHANGE'                                              
         OI    CONACTH+6,X'80'                                                  
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROUTINE CONTROLS KEY DISPLAY                                                  
*                                                                               
DKEY     NTR1                                                                   
         LA    R3,KEY                                                           
         USING TLGCD,R3                                                         
*                                                                               
         LA    R2,GCOSSNH                                                       
         MVC   GCOSSN,TGPID        PID                                          
         MVI   GCOSSNH+5,L'TGPID                                                
         OI    GCOSSNH+6,X'80'                                                  
*                                                                               
         MVC   AIO,AIO2                                                         
         BRAS  RE,VALPID                                                        
         GOTO1 CHAROUT,DMCB,(X'80',TANAELQ),GCONAMEH                            
         OI    GCONAMEH+6,X'80'                                                 
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R3,AIO                                                           
         MVC   TGGCNT,TLGCGCNT                                                  
         MVC   SVGCNT,TLGCGCNT                                                  
*                                                                               
         XC    TLGCGCNT,HEXFFS                                                  
         ICM   R1,15,TLGCGCNT      UNCOMPLEMENT GRT CONTRACT CODE               
         AHI   R1,1                                                             
         EDIT  (R1),GCOCODE,0,ALIGN=RIGHT                                       
         OC    GCOCODE,=C'000000'                                               
         MVC   GCOCODE(2),=C'GC'                                                
         MVI   GCOCODEH+5,L'GCOCODE                                             
         OI    GCOCODEH+6,X'80'                                                 
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE CONTROLS RECORD LISTING                                               
*                                                                               
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
*                                                                               
         CLI   RECNUM,GCONTRK      GCONTRK                                      
         JNE   LR10                                                             
         LA    R0,LRKHOOK                                                       
         LA    R2,GKLLSTH                                                       
         ST    R2,ATHISLST                                                      
*                                                                               
LR10     ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
*                                                                               
         CLI   RECNUM,GCONTRK      GCONTRK                                      
         JE    LR20                                                             
*                                                                               
         MVI   NLISTS,17           IN ORDER TO GET CONTROL                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,16           BACK AFTER 1 FULL PAGE                       
         J     XIT                                                              
*                                                                               
LR20     MVI   NLISTS,16                                                        
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,16                                                        
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* PROCESS SYSIO RECORDS (LIST GCON RECORDS)                                     
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         JNE   XIT                                                              
*                                                                               
         USING LISTD,R2            R2=A(OUTPUT AREA)                            
         MVC   LISTD(LISTLNQ),SPACES                                            
*                                                                               
         USING TLGCD,R4                                                         
         L     R4,TIAREC                                                        
*                                                                               
         CLC   FLTSSN,TLGCSSN      FILTER BY PID                                
         JNE   XIT                                                              
*                                                                               
         OC    FLTGCNT,FLTGCNT     ANY GRT CONTRACT CODE FILTER?                
         JZ    *+14                                                             
         CLC   FLTGCNT,TLGCGCNT                                                 
         JH    XIT                                                              
*                                                                               
         XC    TLGCGCNT,HEXFFS                                                  
         ICM   R1,15,TLGCGCNT      UNCOMPLEMENT GRT CONTRACT CODE               
         AHI   R1,1                                                             
         EDIT  (R1),LSTCODE,0,ALIGN=RIGHT                                       
         OC    LSTCODE,=C'000000'                                               
         MVC   LSTCODE(2),=C'GC'                                                
*                                                                               
         USING TAGDD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAGDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(X'11',TAGDSTRT),(8,LSTCY)   TERM RANGE              
         MVI   LSTCY+8,C'-'                                                     
*                                                                               
         OC    FLTAGY,FLTAGY       ANY AGENCY FILTER?                           
         JZ    *+14                                                             
         CLC   FLTAGY,TAGDAGY                                                   
         JNE   XIT                                                              
         MVC   LSTAGY,TAGDAGY      AGENCY                                       
*                                                                               
         OC    FLTCLI,FLTCLI       ANY CLIENT FILTER?                           
         JZ    *+14                                                             
         CLC   FLTCLI,TAGDCLI                                                   
         JNE   XIT                                                              
         MVC   LSTCLI,TAGDCLI      CLIENT                                       
*                                                                               
         CLI   LISTNUM,15          END OF 1 PAGE                                
         JNE   LRH10                                                            
         MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,GCLSELH                                                       
         GOTO1 EXIT,DMCB,0         AND DON'T COME BACK NO MORE, NO MORE         
*                                                                               
LRH10    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         J     XIT                                                              
*                                                                               
* PROCESS SYSIO RECORDS (LIST GCONTRK RECORDS)                                  
*                                                                               
LRKHOOK  NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         JNE   XIT                                                              
*                                                                               
         LA    R2,LISTAR                                                        
         USING GKLISTD,R2            R2=A(OUTPUT AREA)                          
         MVC   GKLISTD(GKLSTLNQ),SPACES                                         
*                                                                               
         USING TLOTD,R4                                                         
         L     R4,TIAREC                                                        
*                                                                               
         CLC   FLTSSN,TLOTSSN      FILTER BY PID                                
         JNE   XIT                                                              
         CLC   FLTGCNT,TLOTGCNT                                                 
         JNE   XIT                                                              
*                                                                               
         OC    FLTSTRT,FLTSTRT     ANY YEAR FILTER?                             
         JZ    *+14                                                             
         CLC   FLTSTRT,TLOTSTRT    YEAR FILTER = GCONTRK START?                 
         JNE   XIT                                                              
*                                                                               
         OC    TLOTINV,TLOTINV     INVOICE                                      
         JZ    LRKH10                                                           
         GOTO1 TINVCON,DMCB,TLOTINV,GKLINV,DATCON                               
         TM    TLOTSTA,TLOTSPHA                                                 
         JZ    LRKH10                                                           
         MVI   GKLIND,C'*'                                                      
         DROP  R4                                                               
*                                                                               
         USING TAGCD,R4                                                         
LRKH10   MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(X'11',TAGCSTRT),(8,GKLCYCLE)  TERM RANGE            
*                                                                               
         MVC   GKLUNI,TAGCUNI      UNION                                        
*                                                                               
         EDIT  TAGCAMT,GKLPNH,2,ALIGN=RIGHT,ZERO=NOBLANK,FLOAT=-                
         EDIT  TAGCBAL,GKLBAL,2,ALIGN=RIGHT,ZERO=NOBLANK,FLOAT=-                
*                                                                               
         OC    TAGCUSE,TAGCUSE                                                  
         JZ    LRKH20                                                           
         MVC   GKLUSE,TAGCUSE      USE                                          
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',TAGCCOM)                             
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   GKLCID,TACOCID                                                   
         DROP  R4                                                               
*                                                                               
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
         J     LRKH25                                                           
*                                                                               
         USING TAACD,R4                                                         
LRKH20   L     R4,TIAREC                                                        
         MVI   ELCODE,TAACELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   GKLINV(13),=C'ADJUSTMENT by'                                     
         MVC   GKLINV+14(8),TAACSTAF                                            
         DROP  R4                                                               
*                                                                               
LRKH25   MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON                                                          
         MVC   GKLISTD(GKLSTLNQ),SPACES                                         
*                                                                               
         USING TAGCD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    TAGCGRT,TAGCGRT     GUARANTEE CODE?                              
         JZ    *+16                                                             
         MVC   GKLGUAH,=C'GRT:'                                                 
         XC    TAGCGRT,=4X'FF'                                                  
         MVC   GKLGUA,TAGCGRT                                                   
*                                                                               
         CLI   TAGCLEN,TAGCLNQ                                                  
         JNE   LRKH30                                                           
         OC    TAGCUSE,TAGCUSE     USE                                          
         JNZ   LRKH30                                                           
         DROP  R4                                                               
*                                                                               
         USING TLOTD,R4                                                         
LRKH30   L     R4,TIAREC                                                        
         OC    TLOTAGY,TLOTAGY     AGENCY                                       
         JZ    LRKH40                                                           
         MVC   GKLAGY,TLOTAGY                                                   
         MVC   AIO,TIAREC                                                       
         MVC   BLOCK(L'GKLRECA),SPACES                                          
         GOTO1 CHAROUT,DMCB,TACMELQ,(1,BLOCK),TACMTYPG   COMMENTS               
         MVC   GKLRECA,BLOCK+8                                                  
         J     LRKH50                                                           
         DROP  R4                                                               
*                                                                               
LRKH40   MVC   AIO,TIAREC                                                       
         MVC   BLOCK(L'GKLCMNT),SPACES                                          
         GOTO1 CHAROUT,DMCB,TACMELQ,(1,BLOCK),TACMTYPG   COMMENTS               
         MVC   GKLCMNT,BLOCK+8                                                  
*                                                                               
LRKH50   CLI   LISTNUM,16          END OF 1 PAGE                                
         JNE   LRKH60                                                           
         MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,GKLLSTH                                                       
         GOTO1 EXIT,DMCB,0         AND DON'T COME BACK NO MORE, NO MORE         
*                                                                               
LRKH60   MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         J     XIT                                                              
*                                                                               
HEXFFS   DC    (L'TLGCGCNT)X'FF'                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PF KEYS                                                                       
*                                                                               
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,PFTNOSEL)                   
         DC    CL3'   ',CL8'GCONTRK',CL8'LIST'                                  
PF13     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYTWA,L'GCOCODE-1),AL2(GCOCODE-T702FFD)                   
         DC    AL1(KEYTYCUR,L'CYLCY-1),AL2(CYLCY-CYLISTD)                       
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'GCON',CL8'LIST'                                     
PF14     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'GRT',CL8'LIST'                                      
PF15     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYTWA,L'GCOCODE-1),AL2(GCOCODE-T702FFD)                   
PF15X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PF2TAB   DS    0C                                                               
         DC    AL1(PF213X-*,13,0,(PF213X-PF213)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'GRT',CL8'LIST'                                      
PF213    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LSTCODE-1),AL2(LSTCODE-LISTD)                     
PF213X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
KPFTAB   DS    0C                                                               
         DC    AL1(KPF13X-*,13,0,(KPF13X-KPF13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'GCON',CL8'DISPLAY'                                  
KPF13    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYTWA,L'GKLCODE-1),AL2(GKLCODE-T702FFD)                   
KPF13X   EQU   *                                                                
         DC    AL1(KPF14X-*,14,0,(KPF14X-KPF14)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'GCON',CL8'LIST'                                     
KPF14    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
KPF14X   EQU   *                                                                
         DC    AL1(KPF15X-*,15,0,(KPF15X-KPF15)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'GRT',CL8'LIST'                                      
KPF15    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYTWA,L'GKLCODE-1),AL2(GKLCODE-T702FFD)                   
KPF15X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
* ROUTINE TO VALIDATE PID INPUT                                                 
*   ON ENTRY ... R2=A(PID/SSN FIELD)                                            
*                                                                               
VALPID   NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         JNE   VPID10                                                           
         OC    TGPID,TGPID         GLOBAL PID MUST BE SET                       
         JNZ   VPID30              GO TRANSLATE IT TO SS#                       
         J     ERRMISS                                                          
*                                                                               
VPID10   CLI   5(R2),9             IF SS# INPUT, SKIP RIGHT TO RECVAL           
         JNE   VPID20                                                           
         MVC   TGSSN,8(R2)                                                      
         J     VPID40                                                           
*                                                                               
VPID20   CLI   5(R2),6             IF PID INPUT                                 
         JNE   ERRINV                                                           
         MVC   TGPID,8(R2)         MOVE TO GLOBAL PID FIELD                     
VPID30   GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
*                                                                               
VPID40   GOTO1 RECVAL,DMCB,TLW4CDQ,(X'24',0)                                    
         JNE   ERREXIT                                                          
*                                                                               
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   8(9,R2),SPACES                                                   
         MVC   8(L'TGPID,R2),TGPID                                              
         MVI   5(R2),L'TGPID                                                    
         OI    6(R2),X'80'                                                      
                                                                                
         MVC   SVSSN,TGSSN                                                      
         MVC   SVPID,TGPID                                                      
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ROUTINE TO VALIDATE THE KEY (LIST)                                            
*                                                                               
LVKEY    NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,GCONTRK      GCONTRK?                                     
         JE    LVKKEY                                                           
*                                                                               
         XC    FLTVALS(FLTVALNQ),FLTVALS                                        
*                                                                               
         LA    R2,GCLSSNH          PID                                          
         BRAS  RE,VALPID                                                        
         MVC   FLTSSN,TGSSN                                                     
*                                                                               
         LA    R2,GCLCODEH         GRT CONTRACT CODE                            
         CLI   5(R2),0                                                          
         JE    LVK10                                                            
         CLI   5(R2),L'GCOCODE                                                  
         JNE   ERRINV                                                           
         CLC   GCLCODE(2),=C'GC'                                                
         JNE   ERRINV                                                           
*                                                                               
         LA    RF,4                L'#### OF GCON CODE                          
         LA    RE,10(R2)                                                        
LVK08    CLI   0(RE),C'0'          ENSURE IT'S NUMERIC AFTER GC                 
         JL    ERRINV                                                           
         CLI   0(RE),C'9'                                                       
         JH    ERRINV                                                           
         AHI   RE,1                                                             
         BCT   RF,LVK08                                                         
*                                                                               
         ZIC   R1,5(R2)                                                         
         SHI   R1,3                2 FOR 'GC' AND 1 FOR EXPACK                  
         EX    R1,*+8                                                           
         J     *+10                                                             
         PACK  DUB,10(0,R2)                                                     
         CVB   R1,DUB                                                           
         LNR   R1,R1               COMPLEMENT GRT CONTRACT CODE                 
         STCM  R1,15,FLTGCNT       GRT CONTRACT CODE                            
*                                                                               
LVK10    LA    R2,GCLCYH           CONTRACT YEAR                                
         CLI   5(R2),0                                                          
         JE    LVK20                                                            
*                                                                               
         USING PERVALD,R3                                                       
         LA    R3,BLOCK                                                         
         XC    WORK,WORK                                                        
*                                                                               
         MVC   BYTE,5(R2)                                                       
         GOTO1 PERVAL,DMCB,(BYTE,8(R2)),BLOCK                                   
         CLI   4(R1),0                                                          
         JNE   ERRINV                                                           
*                                                                               
         MVC   FLTSTRT,PVALPSTA    START DATE                                   
         MVC   FLTEND,PVALPEND     END DATE                                     
         DROP  R3                                                               
*                                                                               
LVK20    LA    R2,GCLAGYH          AGENCY                                       
         CLI   5(R2),0                                                          
         JE    LVK30                                                            
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',GCLAGYH),BLOCK                        
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         MVC   FLTAGY,TGAGY                                                     
*                                                                               
LVK30    LA    R2,GCLCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         JE    LVK40                                                            
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'04',GCLCLIH)                              
         JNE   ERRINV              CLIENT REC NOT FOUND                         
         MVC   FLTCLI,TGCLI                                                     
*                                                                               
LVK40    LA    R2,GCLOPTH          OPTIONS                                      
         CLI   5(R2),0                                                          
         JE    LVK50                                                            
*                                                                               
LVK50    MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF   STAFF                                        
         MVI   TIREAD,TLGCCDQ      READ GCON RECORDS                            
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROUTINE TO VALIDATE THE KEY (GCONTRK/LIST)                                    
*                                                                               
LVKKEY   XC    FLTVALS(FLTVALNQ),FLTVALS                                        
*                                                                               
         LA    R2,GKLSSNH          PID                                          
         BRAS  RE,VALPID                                                        
         MVC   FLTSSN,TGSSN                                                     
*                                                                               
         LA    R2,GKLCODEH         GRT CONTRACT CODE                            
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         CLI   5(R2),L'GKLCODE                                                  
         JNE   ERRINV                                                           
         CLC   GKLCODE(2),=C'GC'                                                
         JNE   ERRINV                                                           
*                                                                               
         LA    RF,4                L'#### OF GCON CODE                          
         LA    RE,10(R2)                                                        
LVKK08   CLI   0(RE),C'0'          ENSURE IT'S NUMERIC AFTER GC                 
         JL    ERRINV                                                           
         CLI   0(RE),C'9'                                                       
         JH    ERRINV                                                           
         AHI   RE,1                                                             
         BCT   RF,LVKK08                                                        
*                                                                               
         ZIC   R1,5(R2)                                                         
         SHI   R1,3                2 FOR 'GC' AND 1 FOR EXPACK                  
         EX    R1,*+8                                                           
         J     *+10                                                             
         PACK  DUB,10(0,R2)                                                     
         CVB   R1,DUB                                                           
         LNR   R1,R1               COMPLEMENT GRT CONTRACT CODE                 
         STCM  R1,15,FLTGCNT       GRT CONTRACT CODE                            
*                                                                               
         LA    R2,GKLYEARH         CONTRACT YEAR                                
         CLI   5(R2),0                                                          
         JE    LVKK10                                                           
*                                                                               
         USING PERVALD,R3                                                       
         LA    R3,BLOCK                                                         
         XC    WORK,WORK                                                        
*                                                                               
         MVC   BYTE,5(R2)                                                       
         GOTO1 PERVAL,DMCB,(BYTE,8(R2)),BLOCK                                   
         CLI   4(R1),0                                                          
         JNE   ERRINV                                                           
*                                                                               
         MVC   FLTSTRT,PVALPSTA    START DATE                                   
         MVC   FLTEND,PVALPEND     END DATE                                     
         DROP  R3                                                               
*                                                                               
LVKK10   OI    GLSTSTAT,NOSELFLD                                                
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF   STAFF                                        
         MVI   TIREAD,TLOTCDQ      READ GCONTRK RECORDS                         
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ROUTINE CONTROLS RECORD VALIDATION & BUILDING                                 
*                                                                               
BLDREC   NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTADD       IF ADDING RECORD                             
         JNE   BLD10                                                            
         USING TAGDD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAGDEL,TAGDELQ      INITIALIZE GRT CONTRACT INFO                 
         MVI   TAGDLEN,TAGDLNQ     ELEMENT AND ADD IT TO RECORD                 
         GOTO1 ADDELEM                                                          
         J     BLD20                                                            
         DROP  R4                                                               
*                                                                               
         USING TAGCD,R4                                                         
BLD10    XC    ORIGCYPH,ORIGCYPH                                                
         LA    R3,ORIGCYPH                                                      
         L     R4,AIO                                                           
         MVI   ELCODE,TAGCELQ      BUILD TABLE OF ORIGINAL CONTRACT             
         BRAS  RE,GETEL            YEARS AND LIMITS                             
         J     *+8                                                              
BLD12    BRAS  RE,NEXTEL                                                        
         JNE   BLD14                                                            
*                                                                               
         MVC   0(L'TAGCPD,R3),TAGCPD                                            
         MVC   L'TAGCPD(L'TAGCAMT,R3),TAGCAMT                                   
         AHI   R3,L'TAGCPD+L'TAGCAMT                                            
         J     BLD12                                                            
*                                                                               
BLD14    MVI   0(R3),X'FF'                                                      
         MVI   ELCODE,TAGCELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         USING TAGDD,R4                                                         
BLD20    L     R4,AIO                                                           
         MVC   GCONKEY,0(R4)       SAVE GCON KEY                                
         MVI   ELCODE,TAGDELQ      GET GRT CONTRACT INFO ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   ORIGDATE,TAGDPD     SAVE ORIGINAL TERM                           
*                                                                               
         USING PERVALD,R3                                                       
         MVI   TERMCHG,C'N'                                                     
         LA    R2,GCODATEH       TERM                                           
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)                                                  
         TM    4(R1),PVRCONE     MUST INCLUDE START AND END DATES               
         BO    ERENDMIS                                                         
         MVC   TAGDSTRT,PVALPSTA                                                
         MVC   TAGDEND,PVALPEND                                                 
         CLC   TAGDSTRT,TAGDEND    START > END?                                 
         JNL   ERRINV              NO - ERROR                                   
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,TAGDSTRT),(5,WORK)                                
         MVI   WORK+8,C'-'                                                      
         MVC   WORK+9(5),=C'(60M)' 5 YEAR RANGE                                 
         GOTO1 PERVAL,DMCB,(14,WORK),('PVINSGLS+PVIN1DYL',BLOCK)                
         CLI   4(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TAGDEND,PVALPEND    IS TERM END > 5 YEAR END?                    
         JH    ERRINV              NO - ERROR                                   
         DROP  R3                                                               
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         JE    BLD50                                                            
*                                                                               
         LA    R2,GCODATEH                                                      
         TM    4(R2),X'80'         FIELD INPUT AT THIS TIME?                    
         JZ    BLD40                                                            
         CLC   TAGDSTRT,ORIGDATE   USER CHANGED START DATE?                     
         JE    BLD30                                                            
         MVI   TERMCHG,C'Y'                                                     
*                                                                               
         LA    R3,KEY                                                           
         USING TLOTD,R3            IF GCONTRK EXISTS, THIS IS NOT               
         XC    TLOTKEY,TLOTKEY     ALLOWABLE                                    
         MVI   TLOTCD,TLOTCDQ                                                   
         MVC   TLOTSSN,TGSSN                                                    
         MVC   TLOTGCNT,TGGCNT                                                  
         MVC   TLOTSTRT,TAGDSTRT                                                
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLOTSTRT-TLOTKEY),KEYSAVE   GCONTRK EXIST?                   
         JNE   BLD30                           - OK TO CHANGE                   
         GOTO1 DATCON,DMCB,(X'11',ORIGDATE),(8,GCODATE) TERM                    
         OI    GCODATEH+6,X'80'                                                 
         J     ERRTRK                                AND ERROR                  
*                                                                               
BLD30    CLC   TAGDEND,ORIGDATE+L'TAGDSTRT  USER CHANGED END DATE?              
         JE    BLD40                                                            
         MVI   TERMCHG,C'Y'                                                     
*                                                                               
         LA    R3,KEY              IF TERM END DATE CHANGED,                    
         USING TLOTD,R3            THEN CHECK IF THERE ARE ANY GCONTRK          
         XC    TLOTKEY,TLOTKEY     RECORDS LATER THAN THIS DATE                 
         MVI   TLOTCD,TLOTCDQ                                                   
         MVC   TLOTSSN,TGSSN                                                    
         MVC   TLOTGCNT,TGGCNT                                                  
         MVC   TLOTSTRT,TAGDEND                                                 
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLOTSTRT-TLOTKEY),KEYSAVE                                    
         JNE   BLD32                                                            
         CLC   TLOTSTRT,TAGDEND    GCONTRK AFTER TERM END?                      
         JH    BLD38                                                            
         DROP  R3                                                               
*                                                                               
BLD32    LA    R3,ORIGCYPH                                                      
BLD34    CLI   0(R3),X'FF'         CHECK IF GCONTRK EXISTS FOR LAST             
         JE    BLD40               CONTRACT YEAR                                
         CLC   TAGDEND,L'TAGCSTRT(R3) FIND CONTRACT YEAR START FOR              
         JNH   *+12                   TERM END                                  
         AHI   R3,L'TAGCPD+L'TAGCAMT                                            
         J     BLD34                                                            
*                                                                               
         LA    RF,KEY              TERM END DATE CHANGED,                       
         USING TLOTD,RF            THEN CHECK IF THERE ARE ANY GCONTRK          
         XC    TLOTKEY,TLOTKEY     RECORDS FOR THE LAST CONTRACT YEAR           
         MVI   TLOTCD,TLOTCDQ                                                   
         MVC   TLOTSSN,TGSSN                                                    
         MVC   TLOTGCNT,TGGCNT                                                  
         MVC   TLOTSTRT,0(R3)                                                   
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLOTSEQ-TLOTKEY),KEYSAVE                                     
         JNE   BLD40                                                            
         DROP  RF                                                               
*                                                                               
BLD38    GOTO1 DATCON,DMCB,(X'11',ORIGDATE),(8,GCODATE) TERM                    
         OI    GCODATEH+6,X'80'                                                 
         J     ERRITRK             YES - ERROR                                  
*                                                                               
BLD40    CLI   TERMCHG,C'Y'        DID TERM CHANGE?                             
         JNE   *+14                                                             
         MVC   PARAS(L'TAGDPD),TAGDPD                                           
         BRAS  RE,CHKGRT           CHECK AGAINST ATTACHED GRT'S                 
*                                                                               
         XC    KEY,KEY             RESTORE GCON KEY                             
         MVC   KEY(L'TLGCKEY),GCONKEY                                           
         GOTO1 HIGH                                                             
*                                                                               
BLD50    MVC   FULL,TAGDSTRT       SET TERM START DATE                          
         LA    R2,GCOCY1H          FILL IN SCREEN WITH CONTRACT YEARS           
         MVI   CYLINE#,0                                                        
BLD60    LA    RF,GCOBAL5H                                                      
         CR    R2,RF                                                            
         JH    BLD80                                                            
*                                                                               
         USING PERVALD,R3                                                       
         LA    R3,BLOCK                                                         
         XC    WORK,WORK                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(1,FULL),(5,WORK)                                    
         MVI   WORK+8,C'-'                                                      
         MVC   WORK+9(5),=C'(12M)' 1 YEAR RANGE                                 
         GOTO1 PERVAL,DMCB,(14,WORK),('PVINSGLS+PVIN1DYL',BLOCK)                
         CLI   4(R1),0                                                          
         JE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(X'11',PVALPSTA),(8,8(R2))                           
         OI    6(R2),X'80'                                                      
*                                                                               
         CLC   PVALPEND,TAGDEND    YEAR END > TERM END?                         
         JNH   BLD70                                                            
         GOTO1 DATCON,DMCB,(1,TAGDEND),(8,17(R2))                               
*                                                                               
BLD70    GOTO1 DATCON,DMCB,(1,PVALPEND),(0,WORK) SET NEXT CONTRACT YEAR         
         GOTO1 ADDAY,DMCB,WORK,DUB,F'+1'         TO BEGIN 1 DAY LATER           
         GOTO1 DATCON,DMCB,(0,DUB),(1,FULL)                                     
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         JE    BLD72                                                            
         LA    R3,ORIGCYPH         IT'S A CHANGE, CHECK IF THIS IS A            
         CLI   CYLINE#,0           NEW CONTRACT YEAR.                           
         JE    BLD74                                                            
         ZIC   RF,CYLINE#                                                       
         MHI   RF,L'TAGCPD+L'TAGCAMT                                            
         AR    R3,RF                                                            
         CLI   0(R3),0             IF IT IS, DEFAULT P&H LIMIT TO $1MIL         
         JE    *+12                                                             
         CLI   0(R3),X'FF'                                                      
         JNE   BLD74                                                            
*                                                                               
BLD72    MVC   8(L'GCOLIM1,R2),=C'1000000.00' DEFAULT P&H LIMIT                 
         MVI   5(R2),10                                                         
         OI    6(R2),X'80'                                                      
*                                                                               
BLD74    CLC   FULL(3),TAGDEND     PAST TERM END?                               
         JH    BLD80                                                            
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         ZIC   RF,CYLINE#          KEEP TRACK OF WHICH CONTRACT YEAR            
         AHI   RF,1                LINE WE'RE WORKING ON                        
         STC   RF,CYLINE#                                                       
         J     BLD60                                                            
         DROP  R3                                                               
*                                                                               
BLD80    MVC   SVSTRT,TAGDSTRT     SAVED TERM START                             
         MVC   SVEND,TAGDEND       SAVED TERM END                               
                                                                                
         LA    R2,GCOAGYH          AGENCY                                       
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',GCOAGYH),GCOAGYNH                     
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
         LA    R2,GCOCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',GCOCLIH),GCOCLINH                     
         JNE   ERRINV              CLIENT REC NOT FOUND                         
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         JE    BLD100                                                           
         CLC   TAGDAGY,TGAGY       CURRENT AGENCY IS CHANGING?                  
         JNE   BLD90                                                            
         CLC   TAGDCLI,TGCLI       CURRENT CLIENT IS CHANGING?                  
         JE    BLD100                                                           
BLD90    MVC   TAGDPAGY,TAGDAGY    SAVE PREVIOUS AGENCY                         
         MVC   TAGDPCLI,TAGDCLI    SAVE PREVIOUS CLIENT                         
*                                                                               
         MVC   GCOPAGY,TAGDPAGY    PREVIOUS AGENCY                              
         OI    GCOPAGYH+6,X'80'                                                 
*                                                                               
         MVC   GCOPCLI,TAGDPCLI    PREVIOUS CLIENT                              
         OI    GCOPCLIH+6,X'80'                                                 
*                                                                               
BLD100   MVC   TAGDAGY,TGAGY       ALWAYS, SAVE CURRENT AGENCY                  
         MVC   TAGDCLI,TGCLI       ALWAYS, SAVE CURRENT CLIENT                  
*                                                                               
         LA    R2,GCOCY1H          VALIDATE CONTRACT YEAR LINES                 
         MVI   CYLINE#,0                                                        
         XC    FULL,FULL                                                        
         MVC   FULL(L'SVSTRT),SVSTRT                                            
*                                                                               
BLD110   LA    RF,GCOBAL5H         FINISHED 5 YEARS?                            
         CR    R2,RF                                                            
         JH    BLD170                                                           
*                                                                               
         USING TAGCD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAGCEL,TAGCELQ      GRT CONTRACT YEAR ELEM                       
         MVI   TAGCLEN,TAGCLNQ                                                  
*                                                                               
         USING PERVALD,R3                                                       
         LA    R3,BLOCK                                                         
         XC    WORK,WORK                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(1,FULL),(5,WORK)                                    
         MVI   WORK+8,C'-'                                                      
         MVC   WORK+9(5),=C'(12M)' 1 YEAR RANGE                                 
         GOTO1 PERVAL,DMCB,(14,WORK),('PVINSGLS+PVIN1DYL',BLOCK)                
         CLI   4(R1),0                                                          
         JE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   TAGCSTRT,PVALPSTA   GRT CONTRACT YEAR START DATE                 
         MVC   TAGCEND,PVALPEND    GRT CONTRACT YEAR END DATE                   
*                                                                               
         CLC   TAGCEND,SVEND       YEAR END > TERM END?                         
         JNH   *+10                                                             
         MVC   TAGCEND,SVEND       YES - SET YEAR END TO TERM END               
         DROP  R3                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(1,TAGCEND),(0,WORK)  SET NEXT CONTRACT YEAR         
         GOTO1 ADDAY,DMCB,WORK,DUB,F'+1'         TO BEGIN 1 DAY LATER           
         GOTO1 DATCON,DMCB,(0,DUB),(1,FULL)                                     
*                                                                               
BLD120   ZIC   RF,0(R2)            BUMP TO P&H LIMIT                            
         AR    R2,RF                                                            
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         JE    BLD130                                                           
         TM    4(R2),X'80'         USER ENTERED P&H LIMIT?                      
         JZ    BLD130                                                           
*                                                                               
         LA    R3,KEY              LOOK UP LATEST GCONTRK RECORD                
         USING TLOTD,R3            TO GET TRUE LATEST BALANCE                   
         XC    TLOTKEY,TLOTKEY                                                  
         MVI   TLOTCD,TLOTCDQ                                                   
         MVC   TLOTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLOTGCNT,TGGCNT     GUARANTEE CONTRACT CODE                      
         MVC   TLOTSTRT,TAGCSTRT   SET START DATE                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(TLOTSEQ-TLOTCD),KEYSAVE                                      
         JNE   BLD130                                                           
*                                                                               
         LA    R3,ORIGCYPH         GCONTRK EXISTS FOR THIS CONTRACT             
         MVC   FULL,L'TAGCPD(R3)                                                
         CLI   CYLINE#,0           YEAR, PUT BACK ORIGINAL VALUE AND            
         JE    BLD125              RETURN ERROR                                 
         ZIC   RF,CYLINE#                                                       
         MHI   RF,L'TAGCPD+L'TAGCAMT                                            
         AR    R3,RF                                                            
         MVC   FULL,L'TAGCPD(R3)                                                
BLD125   EDIT  FULL,(L'GCOLIM1,8(R2)),2,ALIGN=RIGHT,ZERO=NOBLANK,      *        
               FLOAT=-                                                          
         OI    6(R2),X'80'                                                      
         J     ERRTRK                                                           
*                                                                               
BLD130   CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   DMCB,0                                                           
         JNE   ERRINV                                                           
         MVC   TAGCAMT,DMCB+4                                                   
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         JNE   BLD140                                                           
         MVC   TAGCBAL,TAGCAMT     ON ADD, BALANCE = P&H LIMIT                  
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     BLD160                                                           
*                                                                               
BLD140   ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         MVC   SVBAL,TAGCAMT       DEFAULT BALANCE = P&H LIMIT                  
*                                                                               
         LA    R3,KEY              LOOK UP LATEST GCONTRK RECORD                
         USING TLOTD,R3            TO GET TRUE LATEST BALANCE                   
         XC    TLOTKEY,TLOTKEY                                                  
         MVI   TLOTCD,TLOTCDQ                                                   
         MVC   TLOTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLOTGCNT,TGGCNT     GUARANTEE CONTRACT CODE                      
         MVC   TLOTSTRT,TAGCSTRT   SET START DATE                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         J     BLD144                                                           
BLD142   GOTO1 SEQ                                                              
BLD144   CLC   KEY(TLOTSEQ-TLOTCD),KEYSAVE                                      
         JNE   BLD150                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING TAGCD,R4                                                         
         MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVBAL,TAGCBAL       GET LATEST BALANCE                           
*                                                                               
BLD150   XC    KEY,KEY             RESTORE GETREC/PUTREC                        
         MVC   KEY(L'TLGCKEY),GCONKEY                                           
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R4,ELEMENT                                                       
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVC   TAGCBAL,SVBAL       UPDATE GCON WITH LATEST BALANCE              
         EDIT  TAGCBAL,(L'GCOLIM1,8(R2)),2,ALIGN=RIGHT,ZERO=NOBLANK,   *        
               FLOAT=-                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
BLD160   GOTO1 ADDELEM                                                          
*                                                                               
         CLC   FULL(L'SVEND),SVEND PAST TERM END?                               
         JH    BLD170              YES - DONE VALIDATING CONTRACT YEARS         
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         ZIC   RF,CYLINE#          KEEP TRACK OF WHICH CONTRACT YEAR            
         AHI   RF,1                LINE WE'RE WORKING ON                        
         STC   RF,CYLINE#                                                       
         J     BLD110                                                           
         DROP  R4                                                               
*                                                                               
BLD170   CLI   ACTNUM,ACTADD                                                    
         JNE   BLD180                                                           
         LA    R2,GCOACYH          IF ADD, THEN ADJUSTMENT IS NOT               
         CLI   5(R2),0             ALLOWED                                      
         JNE   ERRINV                                                           
         LA    R2,GCOALIMH                                                      
         CLI   5(R2),0                                                          
         JNE   ERRINV                                                           
         LA    R2,GCOAUNIH                                                      
         CLI   5(R2),0                                                          
         JNE   ERRINV                                                           
         LA    R2,GCOABALH                                                      
         CLI   5(R2),0                                                          
         JNE   ERRINV                                                           
         LA    R2,GCOCMNTH                                                      
         CLI   5(R2),0                                                          
         JNE   ERRINV                                                           
         J     BLD270              OK TO GO ADD                                 
*                                                                               
BLD180   LA    R2,GCOACYH          ADJUSTMENT CONTRACT YEAR                     
         CLI   5(R2),0                                                          
         JE    BLD260                                                           
*                                                                               
         GOTO1 DTVAL,DMCB,TGDATE                                                
         L     R4,AIO              FIND MATCHING CONTRACT YEAR                  
         USING TAGCD,R4            ELEMENT FOR ADJUSTMENT DATE ENTERED          
         MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
BLD190   BRAS  RE,NEXTEL                                                        
         JNE   ERRINV                                                           
         CLC   TGDATE,TAGCSTRT                                                  
         JL    BLD190                                                           
         CLC   TGDATE,TAGCEND      FOUND CONTRACT YEAR MATCH?                   
         JH    BLD190                                                           
         MVC   SVCYS,TAGCSTRT                                                   
         MVC   SVCYE,TAGCEND                                                    
         MVC   SVCYLIM,TAGCAMT                                                  
         MVC   SVCYBAL,TAGCBAL                                                  
         DROP  R3                                                               
*                                                                               
         XC    SVALIM,SVALIM                                                    
         XC    SVABAL,SVABAL                                                    
         XC    SVAUNI,SVAUNI                                                    
*                                                                               
         LA    R2,GCOALIMH         P&H LIMIT ADJ                                
         LA    R3,SVALIM                                                        
         CLI   5(R2),0                                                          
         JE    BLD200                                                           
         LA    R2,GCOAUNIH                                                      
         CLI   5(R2),0             IF P&H LIMIT ADJ ENTERED, THEN               
         JNE   ERRINV              UNION AND...                                 
         LA    R2,GCOABALH                                                      
         CLI   5(R2),0                                                          
         JNE   ERRINV              ...ACTUAL AMOUNT MUST BE BLANK               
         LA    R2,GCOALIMH                                                      
         J     BLD210                                                           
*                                                                               
BLD200   LA    R2,GCOABALH                                                      
         CLI   GCOABALH+5,0        NO P&H LIMIT - MUST BE SAG/AFT ADJ           
         JE    ERRMISS                                                          
         LA    R2,GCOABALH         ACTUAL AMOUNT                                
         LA    R3,SVABAL                                                        
BLD210   ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   DMCB,0                                                           
         JNE   ERRINV                                                           
         L     RE,DMCB+4                                                        
         LTR   RE,RE                                                            
         JM    BLD215                                                           
         CLI   GCOALIMH+5,0        P&H LIMIT ENTERED?                           
         JNE   *+14                                                             
         CLC   TAGCAMT,DMCB+4      P&H LIMIT > ADJUSTMENT AMOUNT?               
         JL    ERRINV                                                           
BLD215   MVC   0(L'SVALIM,R3),DMCB+4                                            
*                                                                               
         LA    R2,GCOAUNIH         UNION                                        
         CLI   GCOALIMH+5,0        P&H LIMIT ADJ ENTERED?                       
         JNE   BLD220                                                           
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         CLC   8(3,R2),=C'SAG'     ONLY SAG + AFT ARE VALID UNIONS              
         JE    *+14                                                             
         CLC   8(3,R2),=C'AFT'                                                  
         JNE   ERRINV                                                           
         MVC   SVAUNI,8(R2)                                                     
*                                                                               
BLD220   LA    R2,GCOALIMH                                                      
         CLI   5(R2),0                                                          
         JE    BLD230                                                           
*                                                                               
         LA    R3,KEY                                                           
         USING TLOTD,R3            IF GCONTRK DOES NOT EXIST FOR THIS           
         XC    TLOTKEY,TLOTKEY     CONTRACT YEAR, THEN CAN'T MAKE A             
         MVI   TLOTCD,TLOTCDQ      P&H LIMIT ADJUSTMENT                         
         MVC   TLOTSSN,TGSSN                                                    
         MVC   TLOTGCNT,TGGCNT                                                  
         MVC   TLOTSTRT,TAGCSTRT                                                
         GOTO1 HIGH                                                             
         CLC   KEY(TLOTSEQ-TLOTKEY),KEYSAVE                                     
         JNE   ERRINV                                                           
*                                                                               
         XC    KEY,KEY                 RESTORE GETREC/PUTREC SEQUENCE           
         MVC   KEY(L'TLGCKEY),GCONKEY  FOR GCON RECORD                          
         GOTO1 HIGH                                                             
*                                                                               
         LA    R2,GCOALIMH                                                      
         ICM   RE,15,TAGCAMT       P&H LIMIT                                    
         ICM   RF,15,SVALIM        P&H LIMIT ADJUSTMENT                         
         AR    RE,RF                                                            
         STCM  RE,15,TAGCAMT       UPDATE GCON P&H + P&H ADJUSTMENT             
         CLC   TAGCAMT,=F'999999999'                                            
         JH    ERRINV                                                           
         ICM   RE,15,TAGCBAL       ALSO INCREASE THE BALANCE                    
         ICM   RF,15,SVALIM        BY THE SAME ADJUSTMENT                       
         AR    RE,RF                                                            
         LTR   RE,RE                                                            
         JM    ERRADJ1                                                          
         STCM  RE,15,TAGCBAL                                                    
         MVC   SVBAL,TAGCBAL       = GCON BAL + P&H LIMIT                       
         J     BLD240                                                           
*                                                                               
BLD230   LA    R2,GCOABALH                                                      
         L     RE,SVABAL           ALWAYS ALLOW NEGATIVE AMOUNTS                
         LTR   RE,RE                                                            
         JM    *+14                                                             
         CLC   SVABAL,TAGCBAL      SAG/AFT ADJUSTMENT > EXISTING BAL?           
         JH    ERRADJ1                                                          
         ICM   RE,15,TAGCBAL       ONLY SAG/AFT ADJUSTMENT                      
         ICM   RF,15,SVABAL        CALCULATE NEW BALANCE                        
         SR    RE,RF                                                            
         STCM  RE,15,TAGCBAL       UPDATE GCON BAL W/ ADJUSTMENT                
         MVC   SVBAL,TAGCBAL       = GCON BAL - ADJ AMOUNT                      
         CLC   TAGCBAL,SVCYLIM     IS BALANCE > LIMIT?                          
         JH    ERRADJ1                                                          
*                                                                               
BLD240   XC    HALF,HALF                                                        
         LA    R3,KEY                                                           
         USING TLOTD,R3                                                         
         XC    TLOTKEY,TLOTKEY                                                  
         MVI   TLOTCD,TLOTCDQ      CHECK IF GCONTRK RECORD EXISTS               
         MVC   TLOTSSN,TGSSN       FOR THIS CONTRACT YEAR                       
         MVC   TLOTGCNT,SVGCNT     IF IT DOES, GET NEXT AVAILABLE               
         MVC   TLOTSTRT,SVCYS      SEQUENCE #                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLOTSEQ-TLOTCD),KEYSAVE                                      
         JNE   BLD250                                                           
*                                                                               
         XC    TLOTSEQ,BHEXFFS     UNCOMPLEMENT SEQUENCE #                      
         SR    R1,R1                                                            
         ICM   R1,3,TLOTSEQ                                                     
         AHI   R1,1                                                             
         STCM  R1,3,HALF           NEW SEQUENCE #                               
         DROP  R3                                                               
*                                                                               
BLD250   L     RE,AIO2                                                          
         LA    RF,4000                                                          
         XCEF                                                                   
         L     R4,AIO2                                                          
         USING TLOTD,R4                                                         
         MVI   TLOTCD,TLOTCDQ      BUILD NEW GCONTRK RECORD FOR THIS            
         MVC   TLOTSSN,TGSSN       ADJUSTMENT                                   
         MVC   TLOTGCNT,SVGCNT                                                  
         MVC   TLOTSTRT,SVCYS                                                   
         MVC   TLOTSEQ,HALF        SEQUENCE # (COMLEMENTED)                     
         XC    TLOTSEQ,BHEXFFS                                                  
         MVI   TLOTLEN+1,40                                                     
*                                                                               
         USING TAGCD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAGCEL,TAGCELQ      GRT CONTRACT YEAR ELEM                       
         MVI   TAGCLEN,TAGCLNQ                                                  
         MVC   TAGCSTRT,SVCYS      CONTRACT YEAR START DATE                     
         MVC   TAGCEND,SVCYE       CONTRACT YEAR END DATE                       
         MVC   TAGCAMT,SVALIM      P&H LIMIT                                    
         OC    SVABAL,SVABAL       ADJUSTMENT AMOUNT ENTERED?                   
         JZ    *+10                                                             
         MVC   TAGCAMT,SVABAL                                                   
         MVC   TAGCBAL,SVBAL       BALANCE = GCON BAL - ADJ AMOUNT              
         MVC   TAGCUNI,SVAUNI      UNION                                        
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GCOCMNTH         COMMENT                                      
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         GOTO1 NAMIN,DMCB,(1,TACMELQ),(X'80',GCOCMNTH),TACMTYPG                 
*                                                                               
         GOTO1 ACTVIN,DMCB,0       UPDATE ACTIVITY ELEMENT IN GCONTRK           
         GOTO1 ADDREC              ADD GCONTRK RECORD                           
*                                                                               
BLD260   XC    KEY,KEY                                                          
         MVC   KEY(L'TLGCKEY),GCONKEY                                           
         GOTO1 HIGH                RESTORE GETREC/PUTREC SEQUENCE               
*                                  FOR GCON RECORD                              
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AIO1                                                         
BLD270   GOTO1 ACTVIN,DMCB,0       UPDATE ACTIVITY ELEMENT IN GCON              
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADD, GET NEXT AVAILABLE CODE #            
         JNE   XIT                                                              
*                                                                               
         LA    R3,KEY              GET NEXT AVAILABLE GRT CONT CODE #           
         USING TLGCD,R3                                                         
         XC    TLGCKEY,TLGCKEY                                                  
         MVI   TLGCCD,TLGCCDQ                                                   
         MVC   TLGCSSN,TGSSN                                                    
         OI    DMINBTS,X'08'       SET READ DELETED RECORDS                     
         GOTO1 HIGH                                                             
         CLC   KEY(TLGCGCNT-TLGCKEY),KEYSAVE                                    
         JNE   XIT                                                              
         ICM   R1,15,TLGCGCNT                                                   
         SHI   R1,1                                                             
         STCM  R1,15,TLGCGCNT                                                   
*                                                                               
         L     R3,AIO                                                           
         STCM  R1,15,TLGCGCNT                                                   
*                                                                               
         EDIT  (R1),GCOCODE,0,ALIGN=RIGHT                                       
         OC    GCOCODE,=C'000000'                                               
         MVC   GCOCODE(2),=C'GC'                                                
         J     XIT                                                              
         DROP  R4                                                               
*                                                                               
BHEXFFS  DC    (L'TLGCGCNT)X'FF'                                                
*                                                                               
* ROUTINE CHECKS GUARANTEES WITH GCON & VALIDATES PERIOD                        
* ON ENTRY, PARAS(6) = TAGDPD                                                   
*                                                                               
CHKGRT   NTR1                                                                   
         USING TLGUD,R3                                                         
         LA    R3,KEY                                                           
         XC    TLGUKEY,TLGUKEY                                                  
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,TGSSN                                                    
         GOTO1 HIGH                                                             
         J     CGRT20                                                           
CGRT10   GOTO1 SEQ                                                              
CGRT20   CLC   KEY(TLGUGUA-TLGUCD),KEYSAVE                                      
         JNE   CHKGRTX                                                          
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         USING TAGUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   CGRT10                                                           
         CLC   TAGUGCNT,TGGCNT     SAME GCON?                                   
         JNE   CGRT10                                                           
         CLC   TAGUSTRT,PARAS      CHECK IF GRT PERIOD IS                       
         JL    ERRIGRT             WITHIN NEW GCON TERM                         
         CLC   TAGUEND,PARAS+L'TAGDSTRT                                         
         JH    ERRIGRT                                                          
         J     CGRT10                                                           
*                                                                               
CHKGRTX  MVC   AIO,AIO1                                                         
         J     XIT                                                              
         DROP  R3,R4                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ERROR/INFO MESSAGES                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         J     ERREXIT                                                          
*                                                                               
ERRNFND  LA    R2,GCOCODEH                                                      
         MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         J     ERREXIT                                                          
*                                                                               
ERENDMIS MVI   GCODATEH+5,9                                                     
         MVC   GCODATE+9(8),SPACES                                              
         MVI   ERROR,ERMISEDT        MISSING END DATE                           
         MVI   ERRDISP,9                                                        
         J     ERREXIT                                                          
*                                                                               
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         J     ERREXIT                                                          
*                                                                               
CANTDEL  MVI   ERROR,ERINVDEL      RECORD NOT AVAILABLE FOR DELETION            
         J     ERREXIT                                                          
*                                                                               
ERRTRK   MVC   MYMSGNO,=Y(ERMUS512)  INVALID INPUT - TRACKING EXISTS            
         J     ERREXIT2              FOR THIS CONTRACT                          
*                                                                               
ERRITRK  MVC   MYMSGNO,=Y(ERMUS513)  INVALID INPUT - NOT INCLUSIVE OF           
         J     ERREXIT2              TRACKING CYCLES                            
*                                                                               
ERRIGRT  GOTO1 DATCON,DMCB,(X'11',ORIGDATE),(8,GCODATE) TERM                    
         OI    GCODATEH+6,X'80'                                                 
         MVC   MYMSGNO,=Y(ERMUS514)  INVALID INPUT - NOT INCLUSIVE OF           
         J     ERREXIT2              GUARANTEE CYCLES                           
*                                                                               
ERRADJ1  MVC   MYMSGNO,=Y(ERMUS521)  CAN'T ADJUST - CHECK BALANCE FOR           
         J     ERREXIT2              CONTRACT YEAR                              
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     ERREXIT2                                                         
                                                                                
ERREXIT2 MVI   BLOCK,0                                                          
         MVI   MYMTYP,C'E'                                                      
         OI    GENSTAT2,USGETTXT                                                
*                                                                               
ERREXIT  GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
*                                                                               
* EXITS                                                                         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DSECT TO COVER LIST LINE                                                      
*                                                                               
LISTD    DSECT                                                                  
LSTCODE  DS    CL6                 GRT CONTRACT CODE                            
         DS    CL2                                                              
LSTCY    DS    CL17                CONTRACT YEAR                                
         DS    CL2                                                              
LSTAGY   DS    CL6                 AGENCY                                       
         DS    CL2                                                              
LSTCLI   DS    CL6                 CLIENT                                       
LISTLNQ  EQU   *-LISTD                                                          
*                                                                               
* DSECT TO COVER GCONTRK LIST LINE                                              
*                                                                               
GKLISTD  DSECT                                                                  
GKLIND   DS    CL1                                                              
GKLINV   DS    CL6                                                              
         DS    CL3                                                              
GKLCID   DS    CL12                                                             
         DS    CL1                                                              
GKLUSE   DS    CL3                                                              
         DS    CL2                                                              
GKLCYCLE DS    CL17                                                             
         DS    CL2                                                              
GKLUNI   DS    CL3                                                              
         DS    CL3                                                              
GKLPNH   DS    CL11                                                             
         DS    CL2                                                              
GKLBAL   DS    CL11                                                             
GKLSTLNQ EQU   *-GKLISTD                                                        
         ORG   GKLINV                                                           
GKLCMNT  DS    CL50                                                             
         ORG   GKLINV                                                           
GKLAGY   DS    CL6                                                              
         DS    CL3                                                              
GKLGUAH  DS    CL4                                                              
         DS    CL1                                                              
GKLGUA   DS    CL4                                                              
         DS    CL4                                                              
GKLRECA  DS    CL21                                                             
*                                                                               
* DSECT TO COVER GCON CONTRACT YEARS LIST LINE (FOR PFKEY TAB)                  
*                                                                               
CYLISTD  DSECT                                                                  
CYLCY    DS    CL17                                                             
CYLSTLNQ EQU   *-CYLISTD                                                        
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR5DD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR5FD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR5ED                                                       
*                                                                               
COUNTER  DS    PL4                 RECORD COUNTER                               
DOLSAG   DS    F                   TOTAL ACTUAL SAG DOLLARS                     
DOLAFT   DS    F                   TOTAL ACTUAL AFT DOLLARS                     
*                                                                               
SVVALS   DS    0X                                                               
SVPNH    DS    F                   P&H LIMIT                                    
SVSTRT   DS    XL3                 CONTRACT YEAR START                          
SVEND    DS    XL3                 CONTRACT YEAR END                            
SVSSN    DS    CL9                 SSN                                          
SVPID    DS    CL6                 PID                                          
SVGCNT   DS    F                   GRT CONTRACT CODE                            
SVALIM   DS    F                   ADJ P&H LIMIT ADJ                            
SVCYS    DS    XL3                 CONTRACT YEAR START                          
SVCYE    DS    XL3                 CONTRACT YEAR END                            
SVAUNI   DS    CL3                 ADJ UNION                                    
SVABAL   DS    F                   ADJ ACTUAL AMOUNT                            
SVBAL    DS    F                   = GCON BAL - ADJ ACTUAL AMOUNT               
SVCYLIM  DS    F                   CONTRACT YEAR P&H LIMIT                      
SVCYBAL  DS    F                   CONTRACT YEAR BALANCE                        
SVVALLNQ EQU   *-SVVALS                                                         
*                                                                               
CURTYPE  DS    CL1                                                              
CURNUM   DS    XL1                                                              
CURTAGC  DS    F                                                                
*                                                                               
THREEFLD DS    XL3                                                              
*                                                                               
ORIGDATE DS    XL6                 ORIGINAL TERM                                
ORIGCYPH DS    XL51                ORIGINAL CONTRACT YEARS/P&H LIMITS           
TERMCHG  DS    CL1                 TERM CHANGE Y/N                              
*                                                                               
CYLINE#  DS    XL1                 LINE OF CONT YEAR BEING PROCESSED            
*                                                                               
GCONKEY  DS    XL32                                                             
*                                                                               
GCONTRK  EQU   191                 GCONTRK RECORD EQUATE                        
SCR5D    EQU   X'5D'               GCON MAINTENANCE SCREEN                      
*                                                                               
FLTVALS  DS    0C                                                               
FLTSSN   DS    XL(L'TLGCSSN)       SSN                                          
FLTGCNT  DS    CL(L'TLGCGCNT)      GRT CONTRACT CODE (COMPLEMENTED)             
FLTPD    DS    0XL6                                                             
FLTSTRT  DS    XL3                 TERM START DATE (PWOS)                       
FLTEND   DS    XL3                 TERM END DATE (PWOS)                         
FLTAGY   DS    CL(L'TAGDAGY)       AGENCY                                       
FLTCLI   DS    CL(L'TAGDCLI)       CLIENT                                       
FLTVALNQ EQU   *-FLTVALS                                                        
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004TAGEN79   11/21/12'                                      
         END                                                                    
