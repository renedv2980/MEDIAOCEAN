*          DATA SET TAGEN2A    AT LEVEL 062 AS OF 05/09/15                      
*PHASE T7022AC,*                                                                
         TITLE 'T7022A - YTD MAINTENANCE'                                       
T7022A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7022A                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=A(TWAHOLE)                                
         USING WORKD,R7                                                         
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTABLE                                             
*                                                                               
         CLI   RECNUM,CD                                                        
         BNE   YTD1                                                             
         BRAS  RE,DISPCYTD                                                      
         CLI   SPHCHKH+5,0         IF CHECK NUMBER INPUT                        
         BNE   YTD2                                                             
         B     YTD4                                                             
*                                                                               
YTD1     TM    TGSYSTAT,TASYSPID                                                
         BZ    *+14                                                             
         MVC   SPHPHED(7),=C'Pid Num'                                           
         OI    SPHPHEDH+6,X'80'                                                 
         MVC   SPHHED2(23),=C' Fica/SUI  Reimb/SDI  '                           
         OI    SPHHED2H+6,X'80'                                                 
         MVC   SPHHED3(18),=C'  FLI    SDI QTD  '                               
         OI    SPHHED3H+6,X'80'                                                 
         SPACE 2                                                                
         MVC   SPHCUH,=C'Curr.'                                                 
         OI    SPHCUHH+6,X'80'                                                  
         SPACE 2                                                                
         MVC   SPHTBI2,=C'Billing   '                                           
         NI    SPHTBI2H+1,X'FB'                                                 
         MVC   SPHTCHK,=C'Checks     '                                          
         NI    SPHTCHKH+1,X'FB'                                                 
         CLI   SPHCHKH+5,0         IF CHECK NUMBER INPUT                        
         BNE   YTD2                                                             
         MVC   SPHTBIL,=C'Agy Comm  '  MAKE SURE HEADING IS CORRECT             
         NI    SPHTBILH+1,X'FB'        TURN HEADING TO HIGH INTENSITY           
         B     YTD4                                                             
         SPACE 2                                                                
YTD2     CLI   ACTNUM,ACTCHA       AND ACTION IS CHANGE                         
         BNE   YTD4                                                             
         LA    R2,CONACTH                                                       
         B     ERCHACT             GIVE ERROR                                   
         SPACE 3                                                                
YTD4     CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,DISPREC        IF MODE IS DISPLAY REC, GO DO IT             
         BE    YTD6                                                             
         CLI   MODE,XRECPUT        IF MODE IS RECORD CHANGED                    
         BNE   YTD8                                                             
         CLI   SPHCHKH+5,0         DON'T BOTHER IF CHECK NUMBER INPUT           
         BNE   XIT                                                              
YTD6     BAS   RE,DISPLAY          (RE)DISPLAY THE RECORD                       
         B     XIT                                                              
         SPACE 3                                                                
YTD8     CLI   MODE,VALREC         IF MODE VALREC                               
         BNE   XIT                                                              
         CLI   SPHCHKH+5,0         DON'T BOTHER IF CHECK NUMBER INPUT           
         BNE   XIT                                                              
         BAS   RE,BLDSSPNH         VALIDATE SOAP SUBJ. TO P&H FIELD             
         B     XIT                 THEN DONE - ONLY CHANGEABLE FLD              
         EJECT                                                                  
*              VALIDATE THE KEY                                                 
         SPACE 1                                                                
VKEY     NTR1                                                                   
         SPACE                                                                  
         OI    SPHTBILH+1,X'0C'    TURN HEADINGS TO LOW INTENSITY               
         OI    SPHTBILH+6,X'80'                                                 
         OI    SPHTBI2H+1,X'0C'                                                 
         OI    SPHTBI2H+6,X'80'                                                 
         OI    SPHTCHKH+1,X'0C'                                                 
         OI    SPHTCHKH+6,X'80'                                                 
         GOTO1 FLDVAL,DMCB,(X'40',SPHSSNH),(X'80',SPHCHKH)                      
         BE    *+10                                                             
         XC    SVUNIT,SVUNIT       IF ANYTHING CHANGED CLR SAVED UNIT           
         SPACE 1                                                                
         TM    SPHCHKH+4,X'20'     IF CHECK NUM CHANGED - USE IT                
         BNO   VK3                                                              
         GOTO1 FLDVAL,DMCB,(X'40',SPHSSNH),(X'80',SPHCURH)                      
         BE    VK3                 IF SOMETHING ELSE CHANGED                    
         CLI   RECNUM,CD                                                        
         BE    VK3                                                              
         MVC   SPHCHK,SPACES       CLEAR CHECK # FIELD                          
         MVI   SPHCHKH+5,0                                                      
         OI    SPHCHKH+6,X'80'                                                  
         SPACE 1                                                                
VK3      CLI   SPHCHKH+5,0         IF CHECK NUMBER INPUT                        
         BE    *+8                                                              
         BAS   RE,VALCHK           VALIDATE IT AND EXTRACT KEY FIELDS           
         SPACE 1                                                                
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK3A                                                             
         CLI   SPHSSNH+5,6                                                      
         BH    VK3A                                                             
         MVC   TGPID,SPHSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK3A                                                             
         MVC   SPHSSN,TGSSN                                                     
         MVI   SPHSSNH+5,9                                                      
VK3A     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SPHSSNH),SPHSSNNH  S/S NUMB.          
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK3B                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SPHSSN,SPACES                                                    
         MVC   SPHSSN(L'TGPID),TGPID                                            
         MVI   SPHSSNH+5,6                                                      
         OI    SPHSSNH+6,X'80'                                                  
         SPACE 1                                                                
VK3B     LA    R2,SPHEMPH          VALIDATE EMPLOYER                            
         CLI   RECNUM,CD                                                        
         BNE   VK3C                                                             
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   *+10                                                             
         MVC   8(2,R2),=C'P+'                                                   
         CLC   =C'P+',8(R2)                                                     
         BNE   FLDINV                                                           
VK3C     TM    4(R2),X'20'         IF NOT PREV VALIDATED                        
         BO    VK6                                                              
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK4                                                              
         OC    TGEMP,TGEMP         AND NO GLOBAL DEFINED                        
         BNZ   VK4                                                              
         MVC   8(L'TGTPEMP,R2),TGTPEMP  USE DEFAULT EMPLOYER                    
         OI    6(R2),X'80'                                                      
         MVI   5(R2),L'TGTPEMP                                                  
         SPACE                                                                  
VK4      GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',(R2)),SPHEMPNH                        
         SPACE 1                                                                
VK6      LA    R2,SPHDTEH          AS/OF DATE                                   
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   SPHCHKH+5,0         IF CHECK DATE INPUT DON'T DEFAULT            
         BNE   VK20                                                             
         MVC   SVPEND,TGTODAY1     DEFAULT TO TODAY'S DATE                      
         MVC   8(8,R2),TGTODAY8    DISPLAY IT AS WELL                           
         OI    6(R2),X'80'                                                      
         B     VK20                                                             
         SPACE 1                                                                
VK10     GOTO1 DTVAL,DMCB,SVPEND                                                
         SPACE 1                                                                
VK20     LA    R2,SPHCURH          VALIDATE CURRENCY                            
         MVC   SPHHED2+L'SPHHED2-L'SPHSDIT(L'SPHSDIT),SPHSDIT                   
         OI    SPHHED2H+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         SPACE 1                                                                
         CLI   8(R2),C'U'                                                       
         BE    VK30                                                             
         CLI   8(R2),C'E'                                                       
         BE    VK30                                                             
         CLI   8(R2),C'C'                                                       
         BNE   FLDINV                                                           
VK29     MVC   SPHHED2+L'SPHHED2-L'SPHGSTT(L'SPHGSTT),SPHGSTT                   
         SPACE 1                                                                
VK30     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'40',0)  EXIT WITH KEY OF W4 REC.          
         GOTO1 FLDVAL,DMCB,(X'20',SPHSSNH),(X'80',SPHCHKH)                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CHECK NUMBER AND EXTRACT KEY FIELDS          
         SPACE 1                                                                
VALCHK   NTR1                                                                   
         LA    R2,SPHCHKH          R2=A(CHECK NUMBER)                           
         SPACE 1                                                                
         CLI   5(R2),8                                                          
         BNE   FLDINV                                                           
         MVC   TGCHK,8(R2)         SAVE IN GLOBAL STORAGE                       
         XC    TGCHK,=8X'FF'       (COMPLEMENTED)                               
         MVC   SYSDIR,=C'CHKDIR'                                                
         MVC   SYSFIL,=C'CHKFIL'                                                
         MVC   AIO,AIO2            SAVE RECORD IN I/O 2                         
         GOTO1 RECVAL,DMCB,TLCKCCDQ,(X'20',0)                                   
         BNE   THEEND                                                           
         MVC   SVCHKDA,DMDSKADD                                                 
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET TAPD ELEMENT TO CHECK IF VOID            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAPDADJS,TAPDADVD   IF CHECK IS A VOID CHECK                     
         BZ    VCHK10                                                           
         GOTO1 SEQ                 THEN BUMP TO NORMAL CHECK                    
         LA    R3,KEY                                                           
         USING TLCKPD,R3                                                        
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BE    VCHK8                                                            
         MVC   KEY,KEYSAVE         IF CAN'T FIND (I.E. ORIGINAL PURGED)         
         GOTO1 HIGH                SHOW VOID                                    
VCHK8    GOTO1 GETREC              & GET THE RECORCD INTO AIO2                  
         DROP  R3,R4                                                            
*                                                                               
VCHK10   MVC   AIO,AIO1                                                         
         MVC   SYSDIR,=C'TALDIR'                                                
         MVC   SYSFIL,=C'TALFIL'                                                
         XC    TGCHK,=8X'FF'       (UN-COMPLEMENT GLOBAL)                       
         SPACE 1                                                                
         L     R3,AIO2             EXTRACT KEY FIELD VALUES FOR SCREEN          
         USING TLCKD,R3                                                         
         MVC   SPHSSN,TLCKSSN      S/S NUMBER                                   
         MVI   SPHSSNH+5,L'SPHSSN                                               
         OI    SPHSSNH+6,X'80'                                                  
         SPACE 1                                                                
         LR    R4,R3                                                            
         MVI   ELCODE,TATIELQ      LOOK FOR CORP ID ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TATID,R4                                                         
         MVC   SPHSSN,TATIID       THEN THAT GOES IN S/S FIELD                  
         SPACE 1                                                                
         TM    TGSYSTAT,TASYSPID                                                
         BZ    VCHK15                                                           
         GOTO1 SSNPACK,DMCB,SPHSSN,TGPID                                        
         MVC   SPHSSN,SPACES                                                    
         MVC   SPHSSN(L'TGPID),TGPID                                            
         MVI   SPHSSNH+5,6                                                      
         OI    SPHSSNH+6,X'80'                                                  
         SPACE 1                                                                
VCHK15   XC    SPHDTE,SPHDTE       DISPLAY CHECK DATE IF AROUND                 
         MVI   SPHDTEH+5,0                                                      
         OI    SPHDTEH+6,X'80'                                                  
         LR    R4,R3                                                            
         MVI   ELCODE,TACDELQ      LOOK FOR CHECK DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   VCHK20                                                           
         USING TACDD,R4                                                         
         OC    TACDDTE,TACDDTE     IF WE HAVE CHECK DATE                        
         BZ    VCHK20                                                           
         GOTO1 DATCON,DMCB,(1,TACDDTE),(8,SPHDTE)  DISPLAY IT                   
         MVI   SPHDTEH+5,8                                                      
         SPACE 1                                                                
VCHK20   LR    R4,R3                                                            
         MVI   ELCODE,TAPDELQ      LOOK FOR PAYMENT DETAILS ELEMENT             
         BAS   RE,GETEL                                                         
         BNE   VCHKX                                                            
         USING TAPDD,R4                                                         
         MVC   SPHEMP,TAPDEMP      DISPLAY EMPLOYER                             
         MVI   SPHEMPH+5,L'SPHEMP                                               
         OI    SPHEMPH+6,X'80'                                                  
         SPACE 1                                                                
         MVI   SPHCURH+5,1                                                      
         OI    SPHCURH+6,X'80'                                                  
         SPACE 1                                                                
*                                                                               
         CLI   RECNUM,CD                                                        
         JNE   *+12                                                             
         TM    SPHCHKH+4,X'20'                                                  
         JO    VCHKX                                                            
*                                                                               
         MVI   SPHCUR,C'C'         DISPLAY CAN$                                 
         TM    TAPDSTAT,TAPDSCAN                                                
         BO    VCHKX                                                            
         MVI   SPHCUR,C'E'         OR EUROS                                     
         TM    TAPDPST2,TAPDPEUR                                                
         BO    VCHKX                                                            
         MVI   SPHCUR,C'U'         OR US$                                       
         SPACE 1                                                                
VCHKX    B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD - AIO HAS W4 RECORD                           
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SPHDETH,SPHTOTSH,PROT=Y                                          
         MVC   SPHTACO,SPACES                                                   
         OI    SPHTACOH+6,X'80'                                                 
         MVC   SPHTAX1,SPACES      CLEAR TAXABLE YTD LINES                      
         OI    SPHTAX1H+6,X'80'                                                 
         MVC   SPHTAX2,SPACES                                                   
         OI    SPHTAX2H+6,X'80'                                                 
         MVC   SPHTAX3,SPACES                                                   
         OI    SPHTAX3H+6,X'80'                                                 
         MVC   SPHTAX4,SPACES                                                   
         OI    SPHTAX4H+6,X'80'                                                 
         MVC   SPHTX1B,SPACES                                                   
         OI    SPHTX1BH+6,X'80'                                                 
         MVC   SPHTX2B,SPACES                                                   
         OI    SPHTX2BH+6,X'80'                                                 
         MVC   SPHTX3B,SPACES                                                   
         OI    SPHTX3BH+6,X'80'                                                 
         MVC   SPHTX4B,SPACES                                                   
         OI    SPHTX4BH+6,X'80'                                                 
         SPACE 1                                                                
         BAS   RE,YTDINIT          INITIALIZE TAYTD PARAMETER BLOCK             
         XC    TGFULL,TGFULL                                                    
         SPACE 1                                                                
         USING TYD,R3              RETURNS R3=A(PARAMETER BLOCK)                
         USING YTDD,R4                     R4=A(YTD TABLE)                      
         SPACE 1                                                                
         CLI   SPHCHKH+5,0         IF CHECK NUMBER INPUT                        
         BE    DISP10                                                           
         OI    TYSTAT,TYSTCHK      SET PASSING CHECK RECORD                     
         MVC   TYAREC,AIO2         PASS A(RECORD)                               
         MVC   DMDSKADD,SVCHKDA    SET CHECK RECORD'S D/A                       
         SPACE 1                                                                
DISP10   GOTO1 TGTAYTD,DMCB,(RC),SYSCOMM,(R3)  BUILD YTD TABLE                  
         SPACE 1                                                                
         OC    SVUNIT,SVUNIT       IF THERE IS A SAVED UNIT                     
         BZ    DISP30                                                           
         SPACE 1                                                                
DISP20   CLC   SVUNIT,YTDUNIT      BUMP UNTIL WE REACH IT                       
         BE    DISP30                                                           
         LA    R4,YTDNEXT                                                       
         B     DISP20                                                           
         SPACE 1                                                                
DISP30   LA    R5,SPHTOTS                                                       
         LA    R2,SPHDET           R2=A(CURRENT SCREEN LINE TO DISPLAY)         
         USING LINED,R2                                                         
         SPACE 1                                                                
         USING L2NED,R2                                                         
DISP40   CR    R2,R5               IF END-OF-SCREEN                             
         BNH   DISP50                                                           
         MVC   SVUNIT,YTDUNIT      SAVE UNIT                                    
         CLI   RECNUM,CD                                                        
         BNE   DISP70                                                           
         LA    R2,SPHDET           DISP TOTAL PR/QU TAXES ON CN LINE            
DISP45   CR    R2,R5                                                            
         BH    DISP70                                                           
         CLC   L2NUNIT,=C'CN '                                                  
         JE    *+12                                                             
         LA    R2,L2NNEXT                                                       
         B     DISP45                                                           
         L     R1,TGFULL                                                        
         LA    RF,L2NPR                                                         
         BAS   RE,EDIT11                                                        
         B     DISP70                                                           
         SPACE 1                                                                
DISP50   CLI   0(R4),0             END-OF-TABLE                                 
         BNE   DISP60                                                           
         XC    SVUNIT,SVUNIT       THEN CLR SAVED UNIT                          
         CLI   RECNUM,CD                                                        
         BNE   DISP70                                                           
         LA    R2,SPHDET           DISP TOTAL PR/QU TAXES ON CN LINE            
DISP55   CR    R2,R5                                                            
         BH    DISP70                                                           
         CLC   L2NUNIT,=C'CN '                                                  
         JE    *+12                                                             
         LA    R2,L2NNEXT                                                       
         B     DISP55                                                           
         L     R1,TGFULL                                                        
         LA    RF,L2NPR                                                         
         BAS   RE,EDIT11                                                        
         B     DISP70                                                           
         SPACE 1                                                                
*                                                                               
         USING LINED,R2                                                         
DISP60   CLI   RECNUM,CD                                                        
         BNE   DISP62                                                           
         CLC   YTDUNIT,=C'CN '                                                  
         BE    DISP61                                                           
         CLC   YTDUNIT,=C'FD '                                                  
         BE    DISP68                                                           
         MVC   TGCTRY,=C'CA'       CANADIAN                                     
         GOTO1 TAXVAL,DMCB,(X'FF',YTDUNIT) UNIT                                 
         BNE   DISP68                                                           
DISP61   BRAS  RE,CANYTD                                                        
         JE    DISP67                                                           
         J     DISP68                                                           
*                                                                               
DISP62   CLC   =C'P+',SPHEMP                                                    
         BNE   DISP62A                                                          
         MVC   TGCTRY,=C'CA'       CANADIAN                                     
         GOTO1 TAXVAL,DMCB,(X'FF',YTDUNIT) UNIT                                 
         BE    DISP68                                                           
*                                                                               
DISP62A  CLI   YTDUNIT+2,C' '      CITY?                                        
         BE    DISP62X                                                          
         CLC   YTDUNIT,=C'PHL'     ALWAYS SHOW FOR PHILLY                       
         BE    DISP62X                                                          
         CLC   YTDUNIT,=C'NYC'     DON'T SHOW FOR NYC NON-RESIDENTS             
         BNE   DISP62B                                                          
         BRAS  RE,NYCRES           CHECK IF NYC RESIDENT FOR TAX YEAR           
         BE    DISP62X             YES, SHOW IT                                 
*                                                                               
DISP62B  OC    YTDTAX(8),YTDTAX    IF NO TAXES, DON'T DISPLAY                   
         BZ    DISP68                                                           
DISP62X  OC    YTDEARN,YTDEARN     ANY EARNINGS?                                
         JNZ   DISP63                                                           
         OC    YTDTAX,YTDTAX                                                    
         JNZ   DISP63                                                           
         OC    YTDSUI,YTDSUI                                                    
         JNZ   DISP63                                                           
         CLC   YTDUNIT,=C'FD '                                                  
         JE    *+14                                                             
         OC    YTDSDI,YTDSDI                                                    
         JNZ   DISP63                                                           
         OC    YTDSFLI,YTDSFLI                                                  
         JZ    DISP68                                                           
*                                                                               
DISP63   MVC   LINUNIT,YTDUNIT     DISPLAY UNIT                                 
*                                                                               
         L     R1,YTDEARN                                                       
         LA    RF,LINEARN                                                       
         BAS   RE,EDIT11N                                                       
*                                                                               
         L     R1,YTDTAX                                                        
         LA    RF,LINTAX                                                        
         BAS   RE,EDIT11                                                        
*                                                                               
         L     R1,YTDSUI                                                        
         LA    RF,LINSUI                                                        
         BAS   RE,EDIT11                                                        
*                                                                               
         L     R1,YTDSDI                                                        
         LA    RF,LINSDI                                                        
         BAS   RE,EDIT11                                                        
*                                                                               
         L     R1,YTDSFLI                                                       
         LA    RF,LINSFLI                                                       
         BAS   RE,EDIT11                                                        
*                                                                               
         CLC   =C'P+',SPHEMP                                                    
         BNE   DISP65                                                           
*                                                                               
         L     R1,YTDEARN          EARNINGS                                     
         L     RF,YTDTXRE          + TAXABLE REIMBURSMENTS                      
         AR    R1,RF               = TOTAL EARNINGS                             
         LA    RF,LINEARN                                                       
         BAS   RE,EDIT11N                                                       
*                                                                               
         CLC   =C'FD',YTDUNIT                                                   
         BNE   DISP64                                                           
         L     R1,YTDREXP          TOTAL REIMBURSEMENTS                         
         L     RF,YTDTXRE          - TAXABLE REIMBURSMENTS                      
         SR    R1,RF               = NON TAXABLE REIMBURSMENTS                  
         LA    RF,LINSDI                                                        
         BAS   RE,EDIT11                                                        
         B     DISP65                                                           
*                                                                               
DISP64   CLC   =C'CN',YTDUNIT                                                   
         BNE   DISP65                                                           
         L     R1,YTDNTRE          TOTAL REIMBURSEMENTS                         
         LA    RF,LINSDI                                                        
         BAS   RE,EDIT11                                                        
*                                                                               
DISP65   BRAS  RE,DISPQTD          DISPLAY QTD FOR THIS UNIT                    
*                                                                               
DISP67   LA    R2,LINNEXT          BUMP TO NEXT LINE                            
DISP68   LA    R4,YTDNEXT          BUMP TO NEXT TABLE ENTRY                     
         B     DISP40                                                           
         SPACE 1                                                                
DISP70   LA    R2,SPHTAX2          R2=A(CURRENT SCREEN LINE TO DISPLAY)         
         CLI   RECNUM,CD                                                        
         BE    DISP75                                                           
         LA    R5,TYCYTD                                                        
         OC    0(TYCYLNQ,R5),0(R5)                                              
         BZ    DISP80                                                           
         LA    R6,5                N'FIELDS TO OUTPUT                           
         BAS   RE,OUTSCR                                                        
         B     DISP80                                                           
*                                                                               
DISP75   LA    R2,SPHTAX2          R2=A(CURRENT SCREEN LINE TO DISPLAY)         
         LA    R5,TYCUTAX                                                       
         CLI   SPHCUR,0                                                         
         BE    *+12                                                             
         CLI   SPHCUR,C'C'                                                      
         BNE   *+8                                                              
         LA    R5,TYCCTAX                                                       
*        OC    0(TYCYLNQ,R5),0(R5)                                              
*        BZ    DISP80                                                           
         LA    R6,3                N'FIELDS TO OUTPUT                           
         BAS   RE,OUTSCRC                                                       
         SPACE 1                                                                
DISP80   CLI   TGCTSTTY,TASTTYPP   IF THIS IS A PROGRAMMER                      
         BNE   DISP90                                                           
         MVI   SPHCLBL+1,C' '      DISPLAY CHECK D/A                            
         GOTO1 HEXOUT,DMCB,TYCDA,SPHCLBL+2,4,0                                  
         OI    SPHCLBLH+6,X'80'    TRANSMIT                                     
         SPACE 1                                                                
DISP90   BAS   RE,BILLYTD          DISPLAY BILLED YTD                           
         CLI   SPHCHKH+5,0         IF NO CHECK NUMBER INPUT                     
         BNE   *+8                                                              
         BAS   RE,DISSSPNH         DISPLAY SOAP YTD SUBJ. TO P&H                
         LA    R2,SPHSSNH                                                       
         CLI   TGCTSTTY,TASTTYPP   IF THIS IS A PROGRAMMER                      
         BNE   DISP100                                                          
         CLI   RECNUM,CD                                                        
         BE    DISP100                                                          
         MVI   SPHBLBL+1,C' '      DISPLAY BILLING D/A                          
         GOTO1 HEXOUT,DMCB,TYBDA,SPHBLBL+2,4,0                                  
         OI    SPHBLBLH+6,X'80'    TRANSMIT                                     
         SPACE 1                                                                
DISP100  CLI   SPHCHKH+5,0         CHECK NUMBER ENTERED?                        
         BNE   DISPX                                                            
         L     R4,AIO2             CHECK RECORD IN AIO2                         
         MVI   ELCODE,TACXELQ                                                   
         BAS   RE,GETEL            GET AGENCY COMMISSION YTD                    
         BNE   DISPX                                                            
         SPACE 1                                                                
         USING TACXD,R4                                                         
         CLI   TACXLEN,TACXLNQ                                                  
         BL    DISPX                                                            
         L     R1,TACXPPAC         P+ AGY COMM YTD                              
         LA    RF,SPHTACO                                                       
         BAS   RE,EDIT11                                                        
         SPACE 1                                                                
DISPX    OC    SVUNIT,SVUNIT       IF ONLY PARTIAL REC DISPLAYED                
         BNZ   MOREXIT             GIVE MESSAGE                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS BILLED YTD EARNINGS                             
         SPACE 1                                                                
BILLYTD  NTR1                                                                   
         CLI   RECNUM,CD                                                        
         BE    BYTDX                                                            
         SPACE 1                                                                
         USING TYD,R3              R3=A(TAYTD PARAMETER BLOCK)                  
         USING YTDD,R4             R4=A(YTD TABLE)                              
         CLI   SPHCHKH+5,0         IF CHECK NUMBER INPUT                        
         BE    BILL10                                                           
         LA    R3,YTDBLK           JUST SET R3 & R4 BECAUSE                     
         LA    R4,YTDTAB                                                        
         B     BILL20              ALREADY WENT TO TAYTD                        
         SPACE 1                                                                
BILL10   BAS   RE,YTDINIT          INITIALIZE TAYTD PARAMETER BLOCK             
         SPACE 1                                                                
         OI    TYSTAT,TYSTBILL     SET WANT BILLED YTD                          
         SPACE 1                                                                
         GOTO1 TGTAYTD,DMCB,(RC),SYSCOMM,(R3)  BUILD YTD ENTRY                  
         SPACE 1                                                                
         TM    TYRETURN,TYFNDPTR   TEST IT FOUND DATA                           
         BZ    BILL60                                                           
         SPACE 1                                                                
         USING LINE2D,R2                                                        
BILL20   LA    R2,SPHTAX1          R2=A(CURRENT SCREEN LINE TO DISPLAY)         
         LA    R5,TYBEARN                                                       
         LA    R6,5                N'FIELDS TO OUTPUT                           
         BAS   RE,OUTSCR                                                        
         SPACE 1                                                                
         CLI   SPHCHKH+5,0         IF CHECK NUMBER INPUT                        
         BE    BYTDX                                                            
         NI    SPHTBILH+1,X'F3'    TURN HEADINGS TO NORMAL INTENSITY            
         OI    SPHTBILH+6,X'80'                                                 
         NI    SPHTBI2H+1,X'F3'                                                 
         OI    SPHTBI2H+6,X'80'                                                 
         NI    SPHTCHKH+1,X'F3'                                                 
         OI    SPHTCHKH+6,X'80'                                                 
         MVC   SPHTBIL,=C'This Bill'  MAKE SURE HEADING IS CORRECT              
         NI    SPHTX3BH+1,X'DF'       MAKE DATA FIELDS UNPROTECTED              
         NI    SPHTX4BH+1,X'DF'                                                 
         SPACE 1                                                                
         USING LINE2D,R2                                                        
         LA    R2,SPHTAX3          R2=A(CURRENT SCREEN LINE TO DISPLAY)         
         MVC   AIO,AIO2            CHECK RECORD IN AIO2                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL            GET NON TAXABLE EARNINGS FROM                
         BNE   BILL30              PAYC + REXP                                  
         SPACE 1                                                                
         USING TAPDD,R4                                                         
         L     R1,TAPDREXP                                                      
         CLI   TAPDW4TY,TAW4TYCO   IF CORPORATION                               
         BE    BILL25                                                           
         CLI   TAPDW4TY,TAW4TYCA   OR CANADIAN                                  
         BE    BILL25                                                           
         CLI   TAPDW4TY,TAW4TYTR   OR TRUSTEE                                   
         BE    BILL25                                                           
         CLI   TAPDW4TY,TAW4TYFO   OR FOREIGNER                                 
         BNE   *+8                                                              
*                                                                               
BILL25   A     R1,TAPDPAYC         ADD IN PAYC                                  
         ST    R1,TGFULL                                                        
*                                                                               
         USING TACYD,R4                                                         
         CLC   =C'P+',SPHEMP       PAYROLL PLUS?                                
         JNE   BILL27                                                           
         MVI   ELCODE,TACYELQ                                                   
         GOTO1 GETL,DMCB,(3,=C'FD ')                                            
         BE    BILL26                                                           
         L     R1,TGFULL                                                        
         B     BILL27                                                           
BILL26   L     R4,TGELEM                                                        
         ICM   RF,15,TACYTXRE                                                   
         L     R1,TGFULL           SUBTRACT OFF TAXABLE REIMBURSEMENTS          
         SR    R1,RF                                                            
         DROP  R4                                                               
*                                                                               
BILL27   LA    RF,LTXNTAX                                                       
         BAS   RE,EDIT11                                                        
         SPACE 1                                                                
BILL30   MVI   ELCODE,TAYEELQ                                                   
         MVI   BYTE,TAYETBIL       GET BILLING YE ELEMENT                       
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BNE   BILL40                                                           
         SPACE 1                                                                
         USING TAYED,R4                                                         
         L     R4,TGELEM                                                        
         LA    R5,TAYETXBL         TAXABLE AMOUNT THIS CHECK                    
         LA    R6,4                N'FIELDS TO OUTPUT                           
         BAS   RE,OUTSCR                                                        
         SPACE 1                                                                
BILL40   MVI   BYTE,TAYETCHK       GET CHECK YE ELEMENT                         
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BNE   BILL50                                                           
         SPACE 1                                                                
         USING TAYED,R4                                                         
         L     R4,TGELEM                                                        
         LA    R2,SPHTAX4          R2=A(CURRENT SCREEN LINE TO DISPLAY)         
         LA    R5,TAYETXBL         TAXABLE AMOUNT THIS CHECK                    
         LA    R6,4                N'FIELDS TO OUTPUT                           
         BAS   RE,OUTSCR                                                        
         SPACE 1                                                                
         USING TACDD,R4                                                         
BILL50   L     R4,AIO2                                                          
         MVI   ELCODE,TACDELQ      GET NON TAXABLE EARNINGS FROM                
         BAS   RE,GETEL            TACD ELEMENT                                 
         BNE   BYTDX                                                            
         L     R1,TACDNTAX                                                      
         LA    RF,LTXNTAX                                                       
         BAS   RE,EDIT11                                                        
         B     BYTDX                                                            
         SPACE 1                                                                
BILL60   MVI   ELCODE,TAPOELQ      ELSE GET POOLED EARNINGS FROM W4 REC         
         MVC   WORK(1),SVPEND      BUILD SEARCH ARG. FOR GETL                   
         MVC   WORK+1(1),TYCUR                                                  
         MVC   WORK+2(3),TYEMP                                                  
         GOTO1 GETL,DMCB,(5,WORK)                                               
         BNE   BYTDX                                                            
         USING LINE2D,R2                                                        
         LA    R2,SPHTAX1                                                       
         L     R4,TGELEM           R4=A(POOLED EARNINGS ELEMENT)                
         USING TAPOD,R4                                                         
         ICM   R1,15,TAPOEARN                                                   
         LA    RF,LTXEARN                                                       
         BAS   RE,EDIT11                                                        
         SPACE 1                                                                
         ICM   R1,15,TAPOSUI                                                    
         LA    RF,LTXSUI                                                        
         BAS   RE,EDIT11                                                        
         SPACE 1                                                                
BYTDX    MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS SOAP YTD SUBJ. TO P&H FROM W4 RECORD            
         SPACE                                                                  
DISSSPNH NTR1                                                                   
*        MVC   SPHTBIL,=C'Soap SP&&H'  MAKE SURE HEADING IS CORRECT             
*        NI    SPHTBILH+1,X'FB'        TURN HEADING TO HIGH INTENSITY           
         CLI   SPHCHKH+5,0         CHECK NUMBER ENTERED?                        
         BNE   DISSP10                                                          
         CLI   RECNUM,CD                                                        
         BE    DISSP10                                                          
         MVC   SPHTBI2,=C'AFT SP&&H '  MAKE SURE HEADING IS CORRECT             
         MVC   SPHTCHK,=C'WGA SP&&H  '                                          
DISSP10  NI    SPHTBI2H+1,X'FB'        TURN HEADING TO HIGH INTENSITY           
         NI    SPHTCHKH+1,X'FB'                                                 
         OI    SPHTAX3H+4,X'20'        SET INPUT FLD VALIDATED                  
         OI    SPHTX3BH+1,X'20'        MAKE OTHER DATA FIELDS PROTECTED         
         OI    SPHTAX4H+4,X'20'                                                 
         OI    SPHTX4BH+1,X'20'                                                 
         SPACE                                                                  
         USING LINE2D,R2                                                        
         USING TAYSD,R4                                                         
         BRAS  RE,GETTAYS          GET CORRECT TAYS EL                          
         BNE   DISSSPX                                                          
         L     R4,TGELEM           DISPLAY AMOUNT IF FOUND                      
         LA    R2,SPHTAX3                                                       
         EDIT  TAYSSSPH,(11,LTXEARN),2,ZERO=NOBLANK,FLOAT=-                     
         SPACE                                                                  
         MVC   LTXFUI+8(13),=C'Last Changed:' DISPLAY LAST CHANGED INFO         
         GOTO1 ACTVOUT,DMCB,(X'C0',LTXFICA)                                     
         LA    R2,SPHTAX4                                                       
         EDIT  TAYSWSPH,(11,LTXEARN),2,ZERO=NOBLANK,FLOAT=-                     
DISSSPX  B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE VALIDATES SOAP YTD SUBJ. TO P&H ON W4 RECORD             
         SPACE                                                                  
         USING TAYSD,R4                                                         
BLDSSPNH NTR1                                                                   
         TM    SPHTAX3H+4,X'20'    DON'T BOTHER IF PREV. VALIDATED              
         BZ    *+12                                                             
         TM    SPHTAX4H+4,X'20'    DON'T BOTHER IF PREV. VALIDATED              
         BO    XIT                                                              
         BRAS  RE,GETTAYS          GET CORRECT TAYS EL                          
         BNE   BLDSSP2                                                          
         L     R4,TGELEM                                                        
         BRAS  RE,VALAMT           VALIDATE AMT IN FLD AND SAVE IN EL.          
         B     BLDSSP9                                                          
         SPACE                                                                  
BLDSSP2  XC    ELEMENT,ELEMENT     INITIALIZE ELEMENT TO ADD                    
         LA    R4,ELEMENT                                                       
         MVI   TAYSEL,TAYSELQ                                                   
         MVI   TAYSLEN,TAYSLNQ                                                  
         MVC   TAYSYEAR,FULL       YEAR SAVED IN FULL BY GETTAYS                
         BRAS  RE,VALAMT           VALIDATE AMT IN FLD AND SAVE IN EL.          
         GOTO1 ADDELEM                                                          
BLDSSP9  GOTO1 ACTVIN,DMCB,(X'80',0)                                            
BLDSSPX  B     XIT                                                              
         SPACE 3                                                                
         SPACE 3                                                                
*        OUTPUT AMOUNTS TO SCREEN                                               
*              R2 - SCREEN LINE                                                 
*              R5 - AMOUNTS TO OUTPUT                                           
*              R6 - N'FIELDS TO OUTPUT                                          
         SPACE 1                                                                
         USING LINE2D,R2                                                        
OUTSCR   NTR1                                                                   
         L     R1,0(R5)                                                         
         LA    RF,LTXEARN                                                       
         BAS   RE,EDIT11N                                                       
         SPACE 1                                                                
         LA    R5,4(R5)                                                         
         LA    RF,LTXFUI                                                        
         SPACE 1                                                                
OUTS10   L     R1,0(R5)                                                         
         BAS   RE,EDIT11                                                        
         LA    R5,4(R5)            BUMP TO NEXT AMOUNT                          
         LA    RF,11(RF)                                                        
         BCT   R6,OUTS10                                                        
         SPACE 1                                                                
         B     XIT                                                              
*                                                                               
* P+ CANADIAN                                                                   
*                                                                               
OUTSCRC  NTR1                                                                   
         L     R1,0(R5)                                                         
         LA    RF,LTXEARN                                                       
         BAS   RE,EDIT11N                                                       
         MVC   LTXFUI,LTXEARN                                                   
*                                                                               
         LA    R5,4(R5)                                                         
         LA    RF,LTXSUI                                                        
OUTSC10  L     R1,0(R5)                                                         
         BAS   RE,EDIT11                                                        
         LA    R5,4(R5)            BUMP TO NEXT AMOUNT                          
         LA    RF,11(RF)                                                        
         BCT   R6,OUTSC10                                                       
         SPACE 1                                                                
         B     XIT                                                              
*              ROUTINE INITIALIZES TAYTD PARAMTER BLOCK                         
         SPACE 1                                                                
YTDINIT  NTR1                                                                   
                                                                                
         XC    YTDBLK,YTDBLK       CLEAR TAYTD PARAMETER BLOCK                  
         SPACE 1                                                                
         LA    R3,YTDBLK           R3=A(PARAMETER BLOCK)                        
         USING TYD,R3                                                           
         SPACE 1                                                                
         MVC   TYASYSIO,TASYSIO    A(SYSIO)                                     
         SPACE 1                                                                
         LA    R4,YTDTAB                                                        
         ST    R4,TYATAB           R4=A(YTD TABLE)                              
         SPACE 1                                                                
         MVC   TYPEND,SVPEND       END DATE                                     
         SPACE 1                                                                
         MVI   TYCUR,C'C'          SET CURRENCY=CAN$                            
         CLI   SPHCUR,C'C'                                                      
         BNE   YTDI10                                                           
         CLI   RECNUM,CD                                                        
         BNE   YTDI10                                                           
         CLC   =C'P+',SPHEMP                                                    
         BNE   YTDI10                                                           
         MVI   TYCUR,C'U'                                                       
         B     YTDI20                                                           
*                                                                               
YTDI10   MVI   TYCUR,C'E'          OR EUROS                                     
         CLI   SPHCUR,C'E'                                                      
         BE    YTDI20                                                           
         MVI   TYCUR,C'U'          OR US$                                       
         B     YTDI20                                                           
         SPACE 1                                                                
YTDI20   MVC   TYEMP,TGEMP         EMPLOYER                                     
         MVC   TYSSN,TGSSN         SOCIAL SECURITY NUMBER                       
         SPACE 1                                                                
         XIT1  REGS=(R3,R4)        RETURN A(PARAMETER BLOCK), A(TABLE)          
         EJECT                                                                  
         SPACE 1                                                                
*        EDIT A LINE - INPUT (R1)                                               
*                      OUTPUT LENGTH = 11                                       
*                      OUTPUT ADDR   = RF                                       
*                                                                               
         SPACE 1                                                                
EDIT11N  EDIT  (R1),(11,(RF)),2,ZERO=NOBLANK,FLOAT=-                            
         BR    RE                                                               
         SPACE 3                                                                
*        EDIT A LINE - INPUT (R1)                                               
*                      OUTPUT LENGTH = 11                                       
*                      OUTPUT ADDR   = RF                                       
*                                                                               
         SPACE 1                                                                
EDIT11   LTR   R1,R1                                                            
         BZR   RE                                                               
         EDIT  (R1),(11,(RF)),2,FLOAT=-                                         
         BR    RE                                                               
         EJECT                                                                  
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
AMTINV   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     THEEND                                                           
         SPACE 1                                                                
ERCHACT  MVC   MYMSGNO,=Y(ERCHKACT) INVALID ACTION IF CHK NUM INPUT             
         OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     THEEND                                                           
         SPACE                                                                  
DATEINV  MVI   ERROR,INVDATE       INVALID DATE                                 
         B     THEEND                                                           
         SPACE                                                                  
MOREXIT  MVI   MYMSGNO1,36                                                      
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
PFTABLE  DS    0C                  PF KEYS TABLE                                
*                                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'PAYEE   ',CL8'DISPLAY '                               
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CCOM    ',CL8'LIST    '                               
PF14     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF14X    EQU   *                                                                
*                                                                               
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3' ',CL8'LIEN    ',CL8'LIST    '                               
PF16     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF16X    EQU   *                                                                
*                                                                               
         DC    AL1(PF17X-*,17,0,(PF17X-PF17)/KEYLNQ,0)                          
         DC    CL3' ',CL8'DUECOMP ',CL8'LIST    '                               
PF17     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF17X    EQU   *                                                                
*                                                                               
         DC    AL1(PF18X-*,18,0,0,0)                                            
         DC    CL3' ',CL8'GRT     ',CL8'LIST    '                               
PF18X    EQU   *                                                                
*                                                                               
         DC    AL1(PF19X-*,19,0,(PF19X-PF19)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CHECK   ',CL8'LIST    '                               
PF19     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF19X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*---------------------------------------------------------------------          
*              CHECK IF NYC RESIDENT FOR TAX YEAR                               
*---------------------------------------------------------------------          
NYCRES   NTR1  BASE=*,LABEL=*                                                   
         MVC   SYSDIR,=C'CHKDIR'                                                
         MVC   SYSFIL,=C'CHKFIL'                                                
         LHI   R7,1                DEFAULT, NYC NON-RESIDENT                    
                                                                                
         GOTO1 DATCON,DMCB,(1,SVPEND),(20,WORK)                                 
                                                                                
         MVC   AIO,AIO2            FIND LATEST NYC CHECK FOR TAX YEAR           
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING TLCKPD,R4                                                        
         MVI   TLCKPCD,TLCKYCDQ                                                 
         MVC   TLCKYEAR,WORK                                                    
         MVC   TLCKYCUR,TGCUR                                                   
         CLI   TLCKYCUR,C' '                                                    
         JH    *+8                                                              
         MVI   TLCKYCUR,C'U'                                                    
         MVC   TLCKYEMP,TGEMP                                                   
         MVC   TLCKYSSN,TGSSN                                                   
         MVC   TLCKYTXU,=C'NYC'                                                 
         MVC   TLCKYDTE,SVPEND                                                  
         XC    TLCKYDTE,=X'FFFFFF'                                              
         GOTO1 HIGH                                                             
         J     NYCRES5                                                          
*                                                                               
NYCRES3  GOTO1 SEQ                                                              
NYCRES5  CLC   KEY(TLCKYDTE-TLCKPD),KEYSAVE                                     
         JNE   NYCRESX                                                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,TACWELQ                                                   
         GOTO1 GETL,DMCB,(3,=C'NYC')                                            
         JNE   NYCRESX                                                          
*                                                                               
         USING TACWD,R4                                                         
         L     R4,TGELEM                                                        
         TM    TACWSTAT,TACWSRES   RESIDENT?                                    
         JZ    NYCRES3             NO, KEEP LOOKING THRU TAX YEAR               
*                                                                               
         XR    R7,R7                                                            
*                                                                               
NYCRESX  MVC   SYSDIR,=C'TALDIR'                                                
         MVC   SYSFIL,=C'TALFIL'                                                
         LTR   R7,R7                                                            
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE VALIDATES AMT AT SPHTAX3                                 
*              IF INVALID, GIVES ERROR, ELSE SAVES IN ELEMENT AT R4             
         USING TAYSD,R4                                                         
VALAMT   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SPHTAX3H                                                      
         ZIC   R3,5(R2)            R3=L'INPUT                                   
         LTR   R3,R3               IF NOTHING INPUT                             
         JNZ   VALAM5                                                           
         XC    TAYSSSPH,TAYSSSPH   CLEAR FLD IN ELEM                            
         J     VALAM10                                                          
VALAM5   GOTO1 CASHVAL,DMCB,8(R2),(R3)  ELSE VALIDATE INPUT                     
         CLI   0(R1),X'FF'                                                      
         JE    AMTINV              IF INVALID, GIVE ERROR                       
         MVC   TAYSSSPH,4(R1)      ELSE SAVE IN ELEM                            
*                                                                               
VALAM10  LA    R2,SPHTAX4H                                                      
         ZIC   R3,5(R2)            R3=L'INPUT                                   
         LTR   R3,R3               IF NOTHING INPUT                             
         JNZ   VALAM15                                                          
         XC    TAYSWSPH,TAYSWSPH   CLEAR FLD IN ELEM                            
         J     XIT                                                              
VALAM15  GOTO1 CASHVAL,DMCB,8(R2),(R3)  ELSE VALIDATE INPUT                     
         CLI   0(R1),X'FF'                                                      
         JE    AMTINV              IF INVALID, GIVE ERROR                       
         MVC   TAYSWSPH,4(R1)      ELSE SAVE IN ELEM                            
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         DROP  R4                                                               
         SPACE 1                                                                
*              FIND QTD FOR UNIT AND DISPLAY IT                                 
         USING QTDD,RE                                                          
         USING YTDD,R4                     R4=A(YTD TABLE)                      
         USING LINED,R2                                                         
DISPQTD  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,QTDLOOK          LOOK UP QTD FOR THIS CHECK/DATE              
         LA    RE,QTDTAB                                                        
DISPQTD3 CLI   0(RE),0                                                          
         JE    XIT                                                              
         CLC   YTDUNIT,QTDUNIT     MATCH ON CURRENT UNIT                        
         JE    DISPQTD9                                                         
         AHI   RE,QTDLNQ           NO, BUMP TO NEXT                             
         J     DISPQTD3                                                         
                                                                                
DISPQTD9 ICM   R1,15,QTDSDI        QTD SDI FOR UNIT                             
         LA    RF,LINSDQTD                                                      
         EDIT  (R1),(11,(RF)),2,ZERO=NOBLANK,FLOAT=-                            
         J     XIT                                                              
         LTORG                                                                  
         DROP  RE,R4,R2                                                         
         EJECT                                                                  
*              FIND QTD'S FOR THIS CHECK OR DATE                                
QTDLOOK  NTR1  BASE=*,LABEL=*                                                   
         XC    QTDTAB(255),QTDTAB       CLEAR QTDTAB                            
         MVI   QTDTABX,X'00'                                                    
         LA    R4,KEY                                                           
         XC    KEY,KEY             YES, QTD BY CHECK NUMBER                     
                                                                                
         MVC   SYSDIR,=C'CHKDIR'                                                
         MVC   SYSFIL,=C'CHKFIL'                                                
         MVC   AIO,AIO2                                                         
                                                                                
         CLI   SPHCHKH+5,0         IF CHECK NUMBER INPUT                        
         BE    QTDLK500            NO, QTD BY DATE                              
                                                                                
         USING TLCKPD,R4                                                        
         MVI   TLCKPCD,TLCKCCDQ    LOOKUP BY CHECK NUMBER                       
         MVC   TLCKCCHK,SPHCHK                                                  
         XC    TLCKCCHK,=X'FFFFFFFFFFFFFFFF'    COMPLEMENT                      
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE     MAKE SURE SAME CHK #            
         BE    QTDLK800                                                         
         DC    H'0'                             ELSE ABEND                      
                                                                                
QTDLK500 MVI   TLCKPCD,TLCKECDQ    LOOKUP BY EMPLOYEE'S CHECKS                  
         MVC   TLCKESSN,TGSSN                                                   
         MVI   TLCKECUR,C'U'       DEFAULT US CURRENCY                          
         CLI   SPHCURH+5,0                                                      
         BE    *+10                                                             
         MVC   TLCKECUR,SPHCUR                                                  
         LA    R2,TGEMP                                                         
         OC    TGEMP,TGEMP         EMPLOYER                                     
         BNZ   *+8                                                              
         LA    R2,TGTPEMP                                                       
         MVC   TLCKEEMP,0(R2)                                                   
         MVC   TLCKEDTE,SVPEND                                                  
         XC    TLCKEDTE,=X'FFFFFF'                                              
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKEDTE+1-TLCKPD),KEYSAVE   SAME UP TO YEAR                 
         JNE   XIT                                                              
         CLI   SVPEND+1,4          1ST QUARTER                                  
         BL    QTDLK800            FIND UNIT                                    
                                                                                
         CLI   SVPEND+1,7          IN 2ND QUARTER?                              
         BNL   QTDLK600            NO                                           
         CLI   TLCKEDTE+1,X'FB'    BEFORE APRIL?                                
         JH    XIT                 YES, NOTHING FOR THIS QUARTER                
         B     QTDLK800                                                         
                                                                                
QTDLK600 CLI   SVPEND+1,10         IN 3RD QUARTER?                              
         BNL   QTDLK650            NO, HAS TO BE 4TH QUARTER                    
         CLI   TLCKEDTE+1,X'F8'    BEFORE JULY?                                 
         JH    XIT                 YES, NOTHING FOR THIS QUARTER                
         B     QTDLK800                                                         
                                                                                
QTDLK650 CLI   TLCKEDTE+1,X'F5'    BEFORE OCTOBER?                              
         JH    XIT                                                              
                                                                                
         USING QTDD,R6                                                          
         USING TACQD,R4                                                         
QTDLK800 GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LA    R6,QTDTAB                                                        
         MVI   ELCODE,TACQELQ                                                   
         BRAS  RE,GETEL                                                         
         B     QTDLK820                                                         
                                                                                
QTDLK810 BRAS  RE,NEXTEL                                                        
QTDLK820 BNE   QTDLK900                                                         
         MVC   QTDUNIT,TACQUNIT                                                 
         MVC   QTDSDI,TACQSDI                                                   
         AHI   R6,QTDLNQ                                                        
         B     QTDLK810                                                         
         DROP  R6                                                               
                                                                                
QTDLK900 MVC   SYSDIR,=C'TALDIR'                                                
         MVC   SYSFIL,=C'TALFIL'                                                
         MVC   AIO,AIO1                                                         
                                                                                
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE GETS TAYS ELEMENT FOR CORRECT YEAR                       
*              RETURNS CC EQUAL IF FINDS ONE & RETURNS CCYY IN FULL             
         SPACE                                                                  
GETTAYS  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TAYSELQ                  GET TAYS ELEMENT                 
         GOTO1 DATCON,DMCB,(1,SVPEND),(20,DUB) FOR YEAR OF AS/OF DATE           
         MVC   FULL(4),DUB         SET CCYY                                     
         GOTO1 GETL,DMCB,(4,FULL)                                               
         J     XIT                 GETL SETS CC                                 
         LTORG                                                                  
*                                                                               
* CANADIAN YTD                                                                  
*                                                                               
DISPCYTD NTR1  BASE=*,LABEL=*                                                   
         TM    TGSYSTAT,TASYSPID                                                
         BZ    *+14                                                             
         MVC   SPHPHED(7),=C'Pid    '                                           
         OI    SPHPHEDH+6,X'80'                                                 
         MVC   SPHHD1(36),=C'YTD Taxable   Earnings       PR/QU  '              
         OI    SPHHD1H+6,X'80'                                                  
         MVC   SPHHD2(36),=C'CPP/QPP     EI/QEI      QPIP        '              
         OI    SPHHD2H+6,X'80'                                                  
         MVC   SPHHED2(23),=C'FD      PR/QU  CPP/QPP '                          
         OI    SPHHED2H+6,X'80'                                                 
         MVC   SPHHED3(18),=C'  EI/QEI     QPIP '                               
         OI    SPHHED3H+6,X'80'                                                 
         MVC   SPHTBI2,SPACES                                                   
         MVC   SPHTCHK,SPACES                                                   
         MVC   SPHBLBL,SPACES      CLEAR TAXABLE YTD LINES                      
         OI    SPHBLBLH+1,X'20'                                                 
         OI    SPHTAX1H+1,X'20'                                                 
         OI    SPHTX1BH+1,X'20'                                                 
         NI    SPHTBI2H+1,X'FB'        TURN HEADING TO HIGH INTENSITY           
         NI    SPHTCHKH+1,X'FB'                                                 
         OI    SPHTACOH+1,X'20'                                                 
         OI    SPHTAX3H+1,X'20'                                                 
         OI    SPHTAX4H+1,X'20'                                                 
         OI    SPHTX3BH+1,X'20'                                                 
         OI    SPHTX4BH+1,X'20'                                                 
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
* CANADIAN YTD                                                                  
*                                                                               
         USING L2NED,R2                                                         
         USING TYD,R3              RETURNS R3=A(PARAMETER BLOCK)                
         USING YTDD,R4                     R4=A(YTD TABLE)                      
CANYTD   NTR1  BASE=*,LABEL=*                                                   
         MVC   SPHTAX1,SPACES      CLEAR TAXABLE YTD LINES                      
         MVC   SPHTX1B,SPACES      CLEAR TAXABLE YTD LINES                      
*                                                                               
         OC    YTDEARN,YTDEARN     SEE IF THERE'S ANYTHING TO DISPLAY           
         JNZ   CYTD05                                                           
         OC    YTDTXRE,YTDTXRE                                                  
         JNZ   CYTD05                                                           
         OC    YTDNTRE,YTDNTRE                                                  
         JNZ   CYTD05                                                           
         OC    YTDUTAX,YTDUTAX                                                  
         JNZ   CYTD05                                                           
         OC    YTDUPP,YTDUPP                                                    
         JNZ   CYTD05                                                           
         OC    YTDUEI,YTDUEI                                                    
         JNZ   CYTD05                                                           
         OC    YTDUPIP,YTDUPIP                                                  
         JZ    NO                                                               
*                                                                               
CYTD05   MVC   L2NUNIT,YTDUNIT     DISPLAY UNIT                                 
*                                                                               
         L     R1,YTDEARN          EARNINGS                                     
         L     RF,YTDTXRE          + TAXABLE REIMBURSMENTS                      
         AR    R1,RF               = TOTAL EARNINGS                             
         CLI   SPHCUR,0                                                         
         BE    CYTD06                                                           
         CLI   SPHCUR,C'C'                                                      
         BNE   CYTD07                                                           
CYTD06   L     R1,YTDCEARN                                                      
         L     RF,YTDCTXRE          + TAXABLE REIMBURSMENTS                     
         AR    R1,RF                                                            
CYTD07   LA    RF,L2NEARN                                                       
         BAS   RE,EDITC11N                                                      
*                                                                               
         L     R1,YTDUTAX                                                       
         CLI   SPHCUR,0                                                         
         BE    *+12                                                             
         CLI   SPHCUR,C'C'                                                      
         BNE   *+8                                                              
         L     R1,YTDCTAX                                                       
*                                                                               
         CLC   YTDUNIT,=C'CN '                                                  
         JNE   CYTD10                                                           
         LA    RF,L2NFD            FEDERAL TAX                                  
         BAS   RE,EDITC11                                                       
         J     CYTD20                                                           
*                                                                               
CYTD10   L     RE,TGFULL           ACCUMULATE PR TAXES                          
         AR    RE,R1                                                            
         ST    RE,TGFULL                                                        
*                                                                               
         LA    RF,L2NPR            PROVINCIAL TAX?                              
         BAS   RE,EDITC11                                                       
*                                                                               
CYTD20   LA    RF,L2NNTRE          NON TAX REIMB                                
         L     R1,YTDNTRE                                                       
         CLI   SPHCUR,0                                                         
         BE    *+12                                                             
         CLI   SPHCUR,C'C'                                                      
         BNE   *+8                                                              
         L     R1,YTDCNTRE                                                      
         BAS   RE,EDITC11                                                       
*                                                                               
         L     R1,YTDUPP           CPP                                          
         CLI   SPHCUR,0                                                         
         BE    *+12                                                             
         CLI   SPHCUR,C'C'                                                      
         BNE   *+8                                                              
         L     R1,YTDCPP                                                        
         LA    RF,L2NCPP                                                        
         BAS   RE,EDITC8                                                        
*                                                                               
         L     R1,YTDUEI           EI                                           
         CLI   SPHCUR,0                                                         
         BE    *+12                                                             
         CLI   SPHCUR,C'C'                                                      
         BNE   *+8                                                              
         L     R1,YTDCEI                                                        
         LA    RF,L2NEI                                                         
         BAS   RE,EDITC8                                                        
*                                                                               
         L     R1,YTDUPIP          QPIP                                         
         CLI   SPHCUR,0                                                         
         BE    *+12                                                             
         CLI   SPHCUR,C'C'                                                      
         BNE   *+8                                                              
         L     R1,YTDCPIP                                                       
         LA    RF,L2NQPIP                                                       
         BAS   RE,EDITC8                                                        
*                                                                               
CANYTDX  J     YES                                                              
*                                                                               
*        EDIT A LINE - INPUT (R1)                                               
*                      OUTPUT LENGTH = 11                                       
*                      OUTPUT ADDR   = RF                                       
*                                                                               
         SPACE 1                                                                
EDITC11N EDIT  (R1),(11,(RF)),2,ZERO=NOBLANK,FLOAT=-                            
         BR    RE                                                               
*                                                                               
EDITC11  LTR   R1,R1                                                            
         BZR   RE                                                               
         EDIT  (R1),(11,(RF)),2,FLOAT=-                                         
         BR    RE                                                               
*                                                                               
EDITC8   LTR   R1,R1                                                            
         BZR   RE                                                               
         EDIT  (R1),(8,(RF)),2,FLOAT=-                                          
         BR    RE                                                               
         LTORG                                                                  
*              DSECTS TO COVER LINES ON SCREEN                                  
         SPACE                                                                  
LINE2D   DSECT                                                                  
LTXEARN  DS    CL11                                                             
         DS    CL8                 EXTENDED FLD HEADER                          
         DS    CL8                 FLD HEADER                                   
LTXFUI   DS    CL11                                                             
LTXSUI   DS    CL11                                                             
LTXFICA  DS    CL11                                                             
LTXMED   DS    CL11                                                             
LTXNTAX  DS    CL11                                                             
         SPACE 5                                                                
LINED    DSECT                                                                  
LINDET   DS    CL(L'SPHDET)                                                     
         ORG   LINDET                                                           
LINUNIT  DS    CL3                                                              
         DS    CL9                                                              
LINAMTS  DS    0C                                                               
LINEARN  DS    CL11                                                             
LINTAX   DS    CL11                                                             
LINSUI   DS    CL11                                                             
LINSDI   DS    CL11                                                             
LINSFLI  DS    CL11                                                             
LINSDQTD DS    CL11                                                             
         ORG                                                                    
         DS    CL8                 HEADER FOR NEXT LINE                         
LINNEXT  EQU   *                                                                
*                                                                               
L2NED    DSECT                                                                  
L2NDET   DS    CL(L'SPHDET)                                                     
         ORG   L2NDET                                                           
L2NUNIT  DS    CL3                                                              
         DS    CL2                                                              
L2NAMTS  DS    0C                                                               
L2NEARN  DS    CL11                                                             
L2NNTRE  DS    CL11                                                             
L2NFD    DS    CL11                                                             
L2NPR    DS    CL11                                                             
         DS    CL1                                                              
L2NCPP   DS    CL8                                                              
         DS    CL1                                                              
L2NEI    DS    CL8                                                              
         DS    CL1                                                              
L2NQPIP  DS    CL8                                                              
         ORG                                                                    
         DS    CL8                 HEADER FOR NEXT LINE                         
L2NNEXT  EQU   *                                                                
         EJECT                                                                  
*              DSECT TO COVER QTD                                               
QTDD     DSECT                                                                  
QTDUNIT  DS    CL3                                                              
QTDSDI   DS    XL4                                                              
QTDLNQ   EQU   *-QTDD                                                           
       ++INCLUDE TAYTDD                                                         
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR2AD                                                       
         SPACE 2                                                                
SVUNIT   DS    CL3                 NEXT UNIT TO DISPLAY                         
SVPEND   DS    PL3                 END DATE FOR YTD                             
SVCHKDA  DS    XL4                 CHECK RECORD'S D/A                           
YTDBLK   DS    CL(TYLNQ)           PARAMETER BLOCK FOR TAYTD                    
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
WORKD    DSECT                                                                  
         DS    0F                                                               
QTDTAB   DS    XL(NYTD*QTDLNQ)                                                  
QTDTABX  DS    X                                                                
         DS    0F                                                               
YTDTAB   DS    CL(NYTD*YTDLNQ+1)   YTD TABLE                                    
HOLSPARE DS    CL(L'TWAHOLE+800-(*-WORKD))   DEFINE SPARE SO ERROR IF           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062TAGEN2A   05/09/15'                                      
         END                                                                    
