*          DATA SET REDAR00S   AT LEVEL 017 AS OF 09/26/03                      
*PHASE T80F00A,*                                                                
*INCLUDE RECUP                                                                  
*INCLUDE OUTDAY                                                                 
*INCLUDE REGENDAB                                                               
*INCLUDE REGENDHT                                                               
*INCLUDE REDARTKO                                                               
*INCLUDE KHDUMMY                                                                
         TITLE 'T80F00 - REDAR00 - REP DARE'                                    
*                                                                               
*******************************************************************             
*                                                                 *             
*    REDAR00 --- REP DOCUMENT AUTOMATIC RETRIEVAL EXCHANGE        *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* SEP26/03 HQ  BUG FIX: VALIDATE MAKEGOOD SCREEN BEFORE MOVE INFO *             
* JAN20/03 HQ  BUG FIX: CLEAR PRODUCT NAME FOR MULTI-P PRINT BUG  *             
* AUG22/02 HQ  CHANGE READ TO CTFILE TO BE NON-UPDATIVE PER ALAN A*             
* JUL02/02 SKU DIRECT PROCESSING BYPASS OFFICE LIMIT ACCESS       *             
* JAN04/02 SKU CHANGE APPROVE TO OPEN                             *             
* OCT31/01 SKU ADD ORDER HISTORY                                  *             
* APR09/01 SKU NEW REVISION APPROVAL MATCHING                     *             
* JUL12/00 SKU RETREIVE USER SIGNON FOR LOCAL ORDER PROCESSING    *             
* JUN27/00 SKU SET CURRENT SCREEN NUMBER IN TIOBCNT FOR STEREO    *             
* APR19/00 SKU FIX OFFICE RESTRICTION                             *             
* JAN27/00 SKU MAP DV OFFICE TO DN (DENVER)                       *             
* JUN29/99 SKU SUPPORT NEW C= OFFICE FILTER                       *             
* NOV06/98 SKU CHECK HOTKEY FROM CONTRACT                         *             
* MAY20/98 SKU DDS SIGN-ON RESTRICTED TO 'REP' ONLY               *             
* MAR14/98 SKU 4K VERSION. ALLOCATE EXTRA IO AREA                 *             
* MAR09/98 SKU DISABLE STEREO COLOR TEMPORARILY                   *             
* DEC19/97 SKU SUPPORT STEREO COLOR SCHEME                        *             
* OCT23/97 SKU DARE REVISION SUPPORT                              *             
* MAR03/97 SKU ADD PRODUCT LIST FOR VARIOUS ORDER                 *             
* FEB27/97 SKU FIX MAKEGOOD SELECT BUG                            *             
* OCT07/96 SKU SUPPORT LOW POWER STATION                          *             
* JUN21/96 SKU STORE CONTRACT STATUS                              *             
* MAR07/95 SKU MAKEGOOD SUPPORT                                   *             
* JAN23/95 SKU INSTALL LIMIT ACCESS OFFICE                        *             
* JAN05/95 SKU ADD ROUTINE TO RECORD 1ST ACTIVITY DATE/TIME STAMP *             
* OCT11/94 SKU ADD VALIDATION ROUTINES FOR GROUP/SUBGROUP/TEAM    *             
* MAY18/94 SKU INITIAL RELEASE                                    *             
*                                                                 *             
*                    **  END TOMBSTONE  **                        *             
*HERE**************************************************************             
T80F00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T80F00,R7,R5,RR=R2,CLEAR=YES                           
         USING GEND,RC                                                          
                                                                                
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
                                                                                
         ST    R2,RELO                                                          
         ST    RD,SAVERD                                                        
         LR    R8,RC               R8=A(SPOOLD)                                 
         LA    RC,SPOOLEND         RC=A(GENCON STORAGE)                         
         LA    R9,IO               START OF IO 1 + TOTAL IO AREA LEN            
         AH    R9,=Y(LENIOAS)      R9=A(DAR SYSTEM WORKING STORAGE)             
*                                  GRABBING 4 4000 BYTE I/O AREAS               
         LA    RF,IO                                                            
         AH    RF,=Y(NIOS*(LIOS+8))                                             
         ST    RF,AIOAREA          STORE ADDRESS TO EXTRA IO AREA               
                                                                                
         ST    R1,SYSPARMS                                                      
         L     RF,0(R1)                                                         
         ST    RF,ATIOB                                                         
                                                                                
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,PFKEY            PFKEY ADJUSTED TO 1..12 (0 = ENTER)          
         MVC   CURDISP,TIOBCURD    SAVE CURSOR DISPLACEMENT                     
         DROP  RF                                                               
                                                                                
         L     RA,4(R1)                                                         
         ST    RA,ATWA             RA=A(TWA)                                    
         USING CONHEADH-64,RA                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
                                                                                
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    R7,SYSR7            SECOND BASE REGISTER                         
         ST    R5,SYSR5            THIRD BASE REGISTER                          
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
                                                                                
         OI    CONSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD     CLEAR SAVED MESSAGES                         
         XC    CONHED2,CONHED2                                                  
         OI    CONHED2H+6,X'80'                                                 
                                                                                
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
DAR20    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0         GO TO CALLOV                                 
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,DAR20                                                         
*                                                                               
         BAS   RE,CKGLOB                                                        
*                                                                               
         BAS   RE,SETPROF          GET REVISION AUTO PROCESSING                 
*                                  PROFILE FROM CONTRACT PROFILES               
*                                                                               
         BAS   RE,STEREOUP         CHECK IF STEREO IN USE                       
*                                                                               
* ALL RECORD/ACTIONS OTHER THAN ORDER/LIST MUST BE INVOKED BY PRESSING          
*  THE CORRESPONDING PFKEY. THEREFORE, AT THE FIRST PASS, ONLY                  
*  RECORD/ACTION ORDER/LIST IS PERMITTED.                                       
*                                                                               
         TM    CONRECH+4,X'20'     IF RECORD FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRNSTAT,RCHANG      THEN SET RCHANG FLAG                         
                                                                                
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRNSTAT,ACHANG      THEN SET ACHANG FLAG                         
                                                                                
         TM    TRNSTAT,RACHANG                                                  
         BZ    DAR180                                                           
* EXCEPT FOR MAKEGOODS,                                                         
* IF USER ENTERS CHANGES REC/ACT, IT MUST BE ORDER/LIST,                        
* MGGROUP/LIST, MGGROUP/DIS, OR REVISION/REPORT                                 
* OTHERWISE, THE LAST RECORD/ACTION WILL BE PUT BACK                            
                                                                                
DAR30    DS    0H                  EXPECTING REC/ACT ORDER/LIST                 
         ZIC   R1,CONRECH+5        EXCEPT FOR MAKEGOOD                          
         LTR   R1,R1               NO INPUT, FORCE ORDER/LIST                   
         BZ    DAR140                                                           
         BCTR  R1,0                                                             
         EX    R1,DAR40                                                         
         BE    DAR110                                                           
         B     DAR50                                                            
DAR40    CLC   CONREC(0),=C'MGGROUP'                                            
                                                                                
DAR50    DS    0H                  SOFT MATCH ON ORDER?                         
         EX    R1,DAR60                                                         
         BNE   DAR70                                                            
         B     DAR110                                                           
DAR60    CLC   CONREC(0),=C'ORDER'                                              
                                                                                
DAR70    DS    0H                  SOFT MATCH ON ORDER?                         
         EX    R1,DAR80                                                         
         BNE   DAR140                                                           
         B     DAR90                                                            
DAR80    CLC   CONREC(0),=C'REVISION'                                           
                                                                                
DAR90    DS    0H                  SOFT MATCH ON REPORT?                        
         ZIC   R1,CONACTH+5                                                     
         LTR   R1,R1               NO INPUT, FORCE ORDER/LIST                   
         BZ    DAR140                                                           
         BCTR  R1,0                                                             
         EX    R1,DAR100                                                        
         BE    DAR130                                                           
         B     DAR140                                                           
DAR100   CLC   CONACT(0),=C'REPORT'                                             
                                                                                
DAR110   DS    0H                  SOFT MATCH ON LIST?                          
         ZIC   R1,CONACTH+5                                                     
         LTR   R1,R1               NO INPUT, FORCE ORDER/LIST                   
         BZ    DAR140                                                           
         BCTR  R1,0                                                             
         EX    R1,DAR120                                                        
         BE    DAR130                                                           
         EX    R1,DAR125                                                        
         BE    DAR130                                                           
         B     DAR140                                                           
DAR120   CLC   CONACT(0),=C'LIST'                                               
DAR125   CLC   CONACT(0),=C'DISPLAY'                                            
*                                                                               
DAR130   DS    0H                                                               
         MVI   LISTNUM,0           IN CASE WE CAME FROM SELECT SCREEN           
         B     DAR180                                                           
                                                                                
DAR140   DS    0H                  IF NOT, DEFAULT ORDER/LIST                   
         XC    CONREC,CONREC                                                    
         MVC   CONREC,=C'ORDER   '                                              
         MVI   CONRECH+5,8                                                      
         OI    CONRECH+6,X'80'     XMIT                                         
         XC    CONACT,CONACT                                                    
         MVC   CONACT,=C'LIST    '                                              
         MVI   CONACTH+5,8                                                      
         OI    CONACTH+6,X'80'     XMIT                                         
                                                                                
         CLI   TWALREC,0                                                        
         BE    DAR180                                                           
         CLI   TWALACT,0                                                        
         BE    DAR180                                                           
                                                                                
         LA    R1,RECACT           GET LAST RECORD/ACTION                       
                                                                                
DAR150   DS    0H                                                               
         CLI   0(R1),X'01'         FIND RECORD IN THE RECACT TABLE              
         BNE   DAR160                                                           
         CLC   TWALREC,9(R1)                                                    
         BNE   DAR170                                                           
         MVC   CONREC,1(R1)        PUT TO SCREEN                                
         B     DAR170                                                           
                                                                                
DAR160   DS    0H                                                               
         CLI   0(R1),X'02'         FIND ACTION IN THE RECACT TABLE              
         BNE   DAR170                                                           
         CLC   TWALACT,9(R1)                                                    
         BNE   DAR170                                                           
         MVC   CONACT,1(R1)        PUT TO SCREEN                                
         OI    CONRECH+6,X'40'     FORCE CURSOR TO RECORD FIELD                 
*        B     DONE                AND DON'T GO TO GENCON                       
         B     DAR180              AND INVOKE GENCON                            
                                                                                
DAR170   DS    0H                                                               
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BNE   DAR150                                                           
                                                                                
DAR180   DS    0H                                                               
* CALL A ROUTINE THAT CALLS GENCON SO THAT GENCON WILL RETURN TO                
* NEXT INSTRUCTION AFTER THIS                                                   
* DON'T GO AGAIN UNLESS OVERLAY WANTS TO CALL ANOTHER OVERLAY                   
                                                                                
AGAIN    MVI   GOAGAIN,C'N'                                                     
         BAS   RE,CALLGENC                                                      
         CLC   =X'800F',TWAAUTH    IF AUTH NE 800F                              
         BE    DAR200                                                           
*                                                                               
* HOME MARKET SIGNON DO NOT NEED TO SET OFFICE RESTRICTION FILTER               
* AND NEITHER DOES DIRECT ORDER PROCESSING (CONTRACT PROFILE 51)                
* WHEN SIGNONID IS 4 CHARS OR LESS                                              
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
         TM    SVPGPBIT+CNTDIRCB,CNTDIRCA                                       
         BZ    DAR183                                                           
         CLI   SIGNONID+4,C' '                                                  
         BE    DAR205                                                           
*                                                                               
DAR183   DS    0H                  SAME FOR MAKEGOOD                            
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    DAR185                                                           
         DROP  R4                                                               
*                                                                               
         CLI   SIGNONID+4,C'L'                                                  
         BE    DAR205                                                           
*                                                                               
DAR185   DS    0H                  SAME FOR MAKEGOOD                            
         CLC   =C'ORD',CONREC                                                   
         BNE   DAR190                                                           
         CLC   =C'LIS',CONACT                                                   
         BNE   DAR250                                                           
         CLI   DRFOFFH+5,0         AND FIRST TIME                               
         BNE   DAR200                                                           
*        TM    DRFOFFH+4,X'20'     FORCE OFFICE LIMIT ACCESS                    
*        BO    DAR200                                                           
         CLC   =C'O=',TWAACCS                                                   
         BNE   DAR200                                                           
         MVC   DRFOFF(2),TWAACCS+2    IN THE OFFICE SCRN FIELD                  
*                                                                               
* REMAP FROM DV TO DN (DENVER) OFFICES                                          
*                                                                               
         CLC   =C'BL',AGENCY       EXCEPT FOR BL AND PV                         
         BE    DAR200                                                           
         CLC   =C'PV',AGENCY                                                    
         BE    DAR200                                                           
DAR187   DS    0H                  SAME FOR MAKEGOOD                            
         CLC   =C'DV',DRFOFF                                                    
         BNE   *+10                                                             
         MVC   DRFOFF(2),=C'DN'                                                 
         MVI   DRFOFFH+5,2                                                      
         B     DAR200                                                           
                                                                                
DAR190   DS    0H                  SAME FOR MAKEGOOD                            
         CLC   =C'MGGROUP',CONREC                                               
         BNE   DAR200                                                           
         CLC   =C'LIS',CONACT                                                   
         BE    DAR195                                                           
         CLC   =C'DIS',CONACT                                                   
         BNE   DAR200                                                           
DAR195   CLI   MKGOFFH+5,0         AND FIRST TIME                               
         BNE   DAR200                                                           
*        TM    MKGOFFH+4,X'20'     FORCE OFFICE LIMIT ACCESS                    
*        BO    DAR200                                                           
         CLC   =C'O=',TWAACCS                                                   
         BNE   DAR200                                                           
         CLI   MYSCRNUM,X'F2'      IS THE MAKEGOOD SCREEN LOADED?               
         BNE   DAR200              NO                                           
         MVC   MKGOFF,TWAACCS+2    IN THE OFFICE SCRN FIELD                     
         MVI   MKGOFFH+5,2                                                      
*                                                                               
DAR200   DS    0H                                                               
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    DAR250                                                           
         DROP  R4                                                               
*                                                                               
         CLI   SIGNONID+4,C'L'                                                  
         BNE   DAR250                                                           
*                                                                               
DAR205   DS    0H                                                               
         CLC   =C'LIS',CONACT                                                   
         BNE   DAR250                                                           
         CLC   =C'ORD',CONREC                                                   
         BNE   DAR210                                                           
         CLI   DRFSTATH+5,0        AND FIRST TIME                               
         BNE   DAR250                                                           
         MVC   DRFSTAT(4),SIGNONID                                              
         MVI   DRFSTATH+5,4                                                     
         B     DAR250                                                           
*                                                                               
DAR210   DS    0H                                                               
         CLC   =C'MGGROUP',CONREC                                               
         BNE   DAR250                                                           
*                                                                               
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
         TM    SVPGPBIT+CNTDIRCB,CNTDIRCA                                       
         BZ    DAR220                                                           
         CLI   SIGNONID+4,C' '                                                  
         BNE   DAR250                                                           
         DROP  R4                                                               
*                                                                               
DAR220   DS    0H                                                               
         CLI   MKGSTAH+5,0         AND FIRST TIME                               
         BNE   DAR250                                                           
         CLI   MYSCRNUM,X'F2'      IS THE MAKEGOOD SCREEN LOADED?               
         BNE   DAR250              NO                                           
         MVC   MKGSTA(4),SIGNONID                                               
         MVI   MKGSTAH+5,4                                                      
         B     DAR250                                                           
*                                                                               
* IF OVERLAY WISHED TO CALL GENCON WITH A NEW RECORD AND ACTION,                
* THEN THIS FLAG WILL BE SET                                                    
*                                                                               
DAR250   DS    0H                                                               
         CLI   GOAGAIN,C'Y'                                                     
         BNE   DONE                                                             
         NI    CONRECH+6,X'BF'     RESET INSERT CURSOR BIT HERE                 
         B     AGAIN                                                            
                                                                                
DONE     DS    0H                                                               
         OI    CONRECH+4,X'20'                                                  
         OI    CONACTH+4,X'20'                                                  
*****                                                                           
         CLC   =C'SJ',TWAAGY                                                    
         BNE   DAR300                                                           
         CLC   =AL2(3512),TWAORIG  MUST BE 'REP' SIGN-ON                        
         BE    DAR300                                                           
         CLC   =AL2(8769),TWAORIG  WDDSL SIGN-ON                                
         BE    DAR300                                                           
         MVC   RERROR,=AL2(802)                                                 
         XC    CONREC,CONREC                                                    
         B     MYERR                                                            
*                                                                               
DAR300   DS    0H                                                               
*****                                                                           
         B     XIT                 ELSE EXIT BACK TO USER                       
         EJECT                                                                  
*******************************************************************             
* THIS ROUTINE CALL GENCON BUT DOES NOT RETAIN CONTROL WHEN GENCON              
* EXITS.  INSTEAD, THE ROUTINE THAT CALLS THIS WILL GET CONTROL.                
* THIS ALLOWS THE CONTROLLER TO CALL GENCON AGAIN WITH A NEW                    
* RECORD AND ACTION IF, FOR INSTANCE, AN OVERLAY HAD A LIST ACTION              
* AND A SELECTION WAS MADE.                                                     
*******************************************************************             
CALLGENC NTR1                                                                   
         ST    RD,SYSRD            GENCON USES THIS TO EXIT ITSELF              
                                                                                
         GOTO1 GENCON,DMCB,(R8)    CALL GENCON-PASS A(WORKING STORAGE)          
         B     XIT                 THEN WE'RE THROUGH                           
         EJECT                                                                  
*******************************************************************             
* INITIALIZE SYSTEM DEPENDENT VALUES                                            
*******************************************************************             
SYSINIT  NTR1                                                                   
*              GET TERMINAL VALUES                                              
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAORIG                                                   
         MVC   AGENCY,TWAAGY                                                    
                                                                                
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
                                                                                
SYS20    L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS20                                                         
                                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R1,VCOUNT                                                        
                                                                                
SYS40    ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R1,SYS40                                                         
*                                  SET SYSTEM DEPENDENT VALUES                  
         LA    R1,STARTSAV                                                      
         ST    R1,ASTARTSV                                                      
         MVI   NTWA,X'81'          SAVE 1 LARGE TWA                             
         MVI   SYSTEM,C'R'         REP                                          
*        MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVI   MAXIOS,NIOS+1       USES 4 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 4000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETREP      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'27'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         MVI   ACTELOPT,C'N'       DON'T ADD ACTIVITY ELEMENT                   
         MVI   GETMSYS,8           USES GETMSG FOR SYSTEM 8                     
         MVC   LWORK,=Y(LENWORK)   SPACE TAKEN IN NMOD                          
         MVC   RCPROG(2),=C'RE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9080F00'    PRESET FOR SYSTEM CALLOVS               
                                                                                
         LA    R1,RECACT                                                        
         ST    R1,ARECACT                                                       
                                                                                
* SET UP CERTAIN ROUTINE ADDRESSES - CAN'T WAIT FOR GENCON                      
                                                                                
         L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(SYSLIST) A(TIA) A(COMFACS)                 
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   DEMAND,CDEMAND                                                   
         MVC   DEMOVAL,CDEMOVAL                                                 
         MVC   DEMOUT,CDEMOUT                                                   
         MVC   GETFACT,CGETFACT                                                 
         DROP  R4                                                               
                                                                                
         LA    R1,CONRECH          SET EFH TAGS                                 
         ST    R1,EFHREC                                                        
         LA    R1,CONACTH                                                       
         ST    R1,EFHACT                                                        
         LA    R1,CONKEYH                                                       
         ST    R1,EFHKEY                                                        
         LA    R1,CONWHENH                                                      
         ST    R1,EFHWHEN                                                       
         LA    R1,CONOUTH                                                       
         ST    R1,EFHOUT                                                        
         LA    R1,CONDESTH                                                      
         ST    R1,EFHDEST                                                       
         LA    R1,CONOTHH                                                       
         ST    R1,EFHOTH                                                        
         LA    R1,CONTAGH                                                       
         ST    R1,EFHTAG                                                        
                                                                                
         OI    GENSTAT3,USEKEYSV   USE KEYSAVE,KEY (INTEREP)                    
         OI    GENSTAT1,NOSETEFH+APPLIC                                         
         OI    GENSTAT3,OKVALSEL+RESTXE00                                       
         MVC   LSVTWA0,=AL2(MAXLTWA0)  L'STORAGE TO SAVE IN TWA0                
         MVI   NTWA,0              DON'T SAVE ANY EXTRA PAGES                   
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9080F22',0                                      
         MVC   VREDAR22,0(R1)      ESTABLISH REDAR22 OVERLAY                    
*&&DO                                                                           
         GOTO1 CALLOV,DMCB,0,X'D9080F30',0                                      
         MVC   VREDAR30,0(R1)      ESTABLISH REDAR30 OVERLAY                    
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9080F31',0                                      
         MVC   VREDAR31,0(R1)      ESTABLISH REDAR31 OVERLAY                    
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9080F32',0                                      
         MVC   VREDAR32,0(R1)      ESTABLISH REDAR32 OVERLAY                    
*&&                                                                             
SYSXIT   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK FOR GLOBBER LOADER VARIABLES FROM THE CONTRACT PROGRAM                  
***********************************************************************         
CKGLOB   NTR1                                                                   
         CLI   TWAMODE,1               1=OFFLINE                                
         BE    CKGLZ                                                            
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETF',CONKEYH,,GLRCONNO                         
         TM    DMCB+8,X'10'                                                     
         BNE   CKGLZ                                                            
         GOTO1 CGLOBBER,DMCB,=C'DELE',,,GLRCONNO                                
*                                                                               
* INSERT COMMA BEFORE THE CONTRACT NUMBER TO TAB TO CORRECT KEY FIELD           
*                                                                               
                                                                                
         ZIC   R1,CONKEYH+5                                                     
         AHI   R1,1                                                             
         STC   R1,CONKEYH+5                                                     
*                                                                               
         AHI   R1,-2                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),CONKEY                                                 
         MVI   WORK,C','                                                        
*                                                                               
         AHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONKEY(0),WORK                                                   
*                                                                               
CKGLOB10 DS    0H                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETD',FULL,L'FULL,GLRSTAT                       
         TM    DMCB+8,X'10'                                                     
         BNE   CKGLOB20                                                         
         GOTO1 CGLOBBER,DMCB,=C'DELE',,,GLRSTAT                                 
*                                                                               
         ZIC   R1,CONKEYH+5                                                     
         LA    R2,CONKEYH                                                       
         LA    R2,8(R1,R2)                                                      
         MVC   0(2,R2),=C',#'                                                   
         MVC   2(2,R2),FULL                                                     
         AHI   R1,4                                                             
         STC   R1,CONKEYH+5                                                     
*                                                                               
CKGLOB20 DS    0H                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETD',WORK,24,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BNE   CKGLZ                                                            
         GOTO1 CGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
*                                                                               
         XC    CONREC,CONREC                                                    
         MVC   CONREC(7),=C'MGGROUP'                                            
         OI    CONRECH+6,X'80'     XMIT                                         
         MVI   CONRECH+4,X'80'     TURN ON FIELD INPUT THIS TIME                
         MVI   CONRECH+5,7         SET INPUT LENGTH                             
         XC    CONACT,CONACT                                                    
         MVC   CONACT(4),=C'LIST'                                               
         OI    CONACTH+6,X'80'                                                  
         MVI   CONACTH+4,X'80'                                                  
         MVI   CONACTH+5,4                                                      
*                                                                               
*        CKGLOB EXIT                                                            
*                                                                               
CKGLZ    SR    R0,R0                                                            
         B     *+6                                                              
CKGLNZ   LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SETPROF  --- GET AND SET DAR/REP PROFILES FROM THE REP REC                    
* WE ARE ACTUALLY READING THE CONTRACT PROFILE FOR NOW                          
* THERE ARE PROFILES SET IN THE CONTRACT PROFILES FOR THE AUTOMATIC             
* PROCESSING/REVISION                                                           
***********************************************************************         
SETPROF  NTR1                                                                   
         LR    R2,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
*                                                                               
*- IF NOT ALREADY DONE, READ IN REP RECORD FOR PGM PROFILE                      
         CLC   SVPGREP(2),AGENCY                                                
         BNE   SETP005                                                          
         CLI   SVPGP#,RREPQCNT                                                  
         BE    SETP100             IN TWA FROM PRIOR HIT                        
*                                                                               
SETP005  EQU   *                                                                
         XC    SVPGENTY,SVPGENTY                                                
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   SETP010                                                          
         MVC   SVPGREP(2),AGENCY                                                
         MVI   SVPGP#,RREPQCNT                                                  
****>>   MVC   SVPGPBIT,=8X'FF'                                                 
*                                                                               
*   DON'T SET ALL BITS FOR DDS TERMINAL!!                                       
*                                                                               
SETP010  EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),AGENCY                                                 
         MVC   KEYSAVE,KEY                                                      
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEYSAVE,KEY,         X        
               0,0                                                              
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                REP RECORD NOT ON FILE?  HOW?                
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IO,DMWORK,0           
*                                                                               
         LA    R6,IO                                                            
         USING RREPREC,R6                                                       
*                                                                               
         MVC   SVPGREP(2),AGENCY                                                
         MVI   SVPGP#,RREPQCNT                                                  
*                                                                               
         LA    RE,RREPREC          RECORD IS HERE                               
         ZICM  RF,RREPLEN,2                                                     
         DROP  R6                                                               
         AR    RF,RE                                                            
         MVI   0(RF),0             FORCE 0 AT END OF RECORD                     
*                                                                               
         LA    RE,34(RE)           A(1ST ELEMENT)                               
SETP020  EQU   *                                                                
         CLI   0(RE),0             END OF RECORD W/O MATCH?                     
         BE    SETP100                                                          
*                                                                               
         CLI   0(RE),X'04'         PROGRAM PROFILE ELEMENT?                     
         BE    SETP040                                                          
*                                                                               
         ZIC   RF,1(RE)            GET NEXT ELEMENT                             
         AR    RE,RF                                                            
         B     SETP020                                                          
*                                                                               
*- FIND CNT PROGRAM UNIT WITHIN PROGRAM PROFILE ELEMENT                         
SETP040  EQU   *                                                                
         USING RREPPGMP,RE                                                      
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
         DROP  RE                                                               
*                                                                               
SETP050  CLI   0(RE),RREPQCNT      LOOKING FOR CONTRACT                         
         BE    SETP060                                                          
         LA    RE,RREPPGML(RE)     NEXT UNIT                                    
         BCT   R0,SETP050                                                       
         B     SETP100             NO MATCH. USE DEFAULTS                       
*                                                                               
SETP060  MVC   SVPGPBIT(8),2(RE)   SAVE UNIT IN TWA.                            
*                                                                               
SETP100  EQU   *                                                                
         XC    SIGNONID,SIGNONID                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),AGENCY                                                  
         GOTOX (RFGETID,REPFACS),DMCB,(RA),SIGNONID,0,DUB                       
         OC    SIGNONID,SPACES                                                  
*                                                                               
SETPGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*******************************************************************             
* CHECK IF STEREO IN USE                                                        
*******************************************************************             
STEREOUP NTR1                                                                   
         GOTO1 GETFACT,DMCB,(X'80',0),F#UTLD                                    
*                                                                               
         MVI   STEREOFG,0          INIT STEREO FLAG                             
*                                                                               
         L     R1,0(R1)                                                         
         USING F@UTLD,R1           TEST FOR STEREO                              
         TM    F@TSTAT6,TST6STRO+TST6STFU                                       
         BZ    STEREOX                                                          
         DROP  R1                                                               
*                                                                               
         OI    STEREOFG,STFINUSE   STEREO IS IN USE                             
*                                                                               
STEREOX  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                                
*******************************************************************             
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         L     RA,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD            RESTORE POINTER TO SYSTEM STORAGE            
         L     R7,SYSR7            2ND BASE REG - R7 MUST BE SET FIRST          
         L     R5,SYSR5            3RD BASE REG - R5 MUST BE SET FIRST          
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
                                                                                
                                                                                
VBRANCH  B     VUSER                                                            
         B     PACK                PACK                                         
         B     VSTA                                                             
         B     VAGY                                                             
         B     VOFF                                                             
         B     VSAL                                                             
         B     VCON                                                             
         B     LOAD                                                             
         B     VGRP                                                             
         B     VTEAM                                                            
         B     TOUCHED                                                          
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         B     VINIT               INITIALIZE SYSSPARE                          
         B     MYERR               CALL GETTXT FOR MESSAGE                      
         B     VREFTOPK                                                         
         B     VPKTOREF                                                         
         B     VGETTWA                                                          
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* VUSER - GET REP DATA FROM CONTROL FILE USER ID RECORD                         
*******************************************************************             
VUSER    MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAORIG                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'                                                     
         BRAS  RE,FIRSTEL                                                       
         BNE   VU10                                                             
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
                                                                                
VU10     XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         DROP  R4                                                               
                                                                                
         TM    TRNSTAT,RACHANG     IF RECORD/ACTION FIELD WAS CHANGED           
         BZ    VUSERX                                                           
                                                                                
         XC    CONHED2,CONHED2     CLEAR DISPLAY OF SCREENS TO POP TO           
                                                                                
VUSERX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PACK - R2 HAS ADDRESS OF HEADER                                               
***********************************************************************         
PACK     DS    0H                                                               
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    PACKX               LENGTH ERROR                                 
         TM    4(R2),X'08'                                                      
         BZ    PACKX               NON-NUMERIC                                  
         BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
PACKX    XIT1  REGS=(R0)                                                        
                                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
*******************************************************************             
* VALIDATE STATION CALL LETTERS                                                 
* R2 POINTS AT SCREEN FIELD ON ENTRY                                            
* WORK    - STATION CALL LETTERS                                                
* WORK+4  - A=AM F=FM C=CM T=BLANK                                              
* WORK+10 - MARKET NAME                                                         
* WORK+30 - 1 OR 2 IF SATELLITE STATION                                         
* WORK+31 - GROUP/SUBGROUP CODE                                                 
* WORK+33 - STATION JOIN DATE                                                   
* USES IO3                                                                      
*******************************************************************             
VSTA     DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         MVC   WORK(50),SPACES                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         LA    R4,BLOCK                                                         
                                                                                
         CLI   0(R4),3                                                          
         BL    NO                                                               
         CLI   0(R4),4                                                          
         BH    NO                                                               
         TM    2(R4),X'40'         TEST ALPHA                                   
         BZ    NO                                                               
         MVC   WORK(4),12(R4)      SAVE CALL LETTERS                            
                                                                                
         SR    RE,RE                                                            
         ICM   RE,1,1(R4)          DEFAULT = TV                                 
         BZ    VS100               YES                                          
         BCTR  RE,0                                                             
         EX    RE,STATV            TV LEAVE BLANK                               
         BE    VS100                                                            
         MVI   WORK+4,C'L'         LOW POWER                                    
         EX    RE,STAL                                                          
         BE    VS100                                                            
         MVI   WORK+4,C'A'         AM = A                                       
         EX    RE,STAAM                                                         
         BE    VS100                                                            
         MVI   WORK+4,C'F'         FM = F                                       
         EX    RE,STAFM                                                         
         BE    VS100                                                            
         MVI   WORK+4,C'C'         CM = C                                       
         EX    RE,STACM                                                         
         BE    VS100                                                            
         MVI   WORK+4,C' '         MAY BE SATELLITE STATION                     
         EX    RE,STA1                                                          
         BNE   VS50                                                             
         MVI   WORK+30,C'1'                                                     
         B     VS100                                                            
                                                                                
VS50     EX    RE,STA2                                                          
         BNE   NO                                                               
         MVI   WORK+30,C'2'                                                     
                                                                                
VS100    LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING RSTAKEY,R4                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY     REP                                          
         MVC   RSTAKSTA,WORK       STATION                                      
         DROP  R4                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NO                                                               
                                                                                
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,1                                                         
         BRAS  RE,GETEL                                                         
         USING RSTAELEM,R6                                                      
         MVC   RTKODATE,RSTASTRT   TAKEOVER DATE                                
         MVC   WORK+10(L'RSTAMKT),RSTAMKT                                       
         MVC   WORK+31(L'RSTAGRUP),RSTAGRUP                                     
         MVC   WORK+33(L'RSTASTRT),RSTASTRT  JOIN DATE                          
         DROP  R6                                                               
*                                                                               
* CHECK IF STATION AUTO REVISION METHOD OVERRIDE EXIST                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'                                                     
         BRAS  RE,GETEL                                                         
         USING RSTAXXEL,R6                                                      
         MVI   STAMETH,0                                                        
         CLI   RSTAOPT5,C'1'                                                    
         BL    VSEXT                                                            
         CLI   RSTAOPT5,C'3'                                                    
         BH    VSEXT                                                            
         MVN   STAMETH,RSTAOPT5                                                 
*                                                                               
VSEXT    DS    0H                                                               
         MVC   AIO,SVAIO                                                        
         B     YES                                                              
*                                                                               
STATV    CLC   22(0,R4),=C'TV'                                                  
STAL     CLC   22(0,R4),=C'L '                                                  
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
STACM    CLC   22(0,R4),=C'CM'                                                  
STA1     CLC   22(0,R4),=C'1'                                                   
STA2     CLC   22(0,R4),=C'2'                                                   
         EJECT                                                                  
*********************************************************************           
* VALIDATE AGENCY CODE                                                          
* R2 IS FIELD HEADER                                                            
* WORK WILL HAVE AGENCY EXPANDED NAME                                           
* WORK+20 WILL HAVE AGENCY CODE + OFF                                           
* USES IO3                                                                      
*********************************************************************           
VAGY     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGYKEY,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKREP,AGENCY     REP                                          
*                                                                               
         MVC   RAGYKAGY(6),SPACES                                               
         XC    WORK,WORK                                                        
         MVC   WORK(7),8(R2)                                                    
         OC    WORK(10),SPACES     USE SPACE AS SENTINEL                        
         LA    RE,WORK                                                          
                                                                                
VAGY10   DS    0H                  CHECK FOR AGENCY OFFICE                      
         CLI   0(RE),C'-'                                                       
         BE    VAGY20                                                           
         CLI   0(RE),C' '                                                       
         BE    VAGY30                                                           
         LA    RE,1(RE)                                                         
         B     VAGY10                                                           
                                                                                
VAGY20   DS    0H                  AGENCY OFFICE                                
         MVC   RAGYKAOF,1(RE)                                                   
         LA    RF,WORK+1                                                        
         SR    RE,RF                                                            
         EX    RE,MOVEAGY                                                       
         B     VAGY40                                                           
*                                                                               
MOVEAGY  MVC   RAGYKAGY(0),WORK                                                 
*                                                                               
VAGY30   MVC   RAGYKAGY(4),WORK                                                 
*                                                                               
VAGY40   DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NO                                                               
         DROP  R6                                                               
                                                                                
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RAGYREC,R6                                                       
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   WORK(20),RAGYNAM1                                                
         MVC   WORK+20(6),RAGYKAGY                                              
         MVC   AIO,SVAIO                                                        
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
*  VALIDATE OFFICE                                                              
*  - R2 POINTS AT SCREEN FIELD ON ENTRY                                         
*  - WORK HAS OFFICE EXPANSION ON EXIT                                          
*  - USES IO3                                                                   
*******************************************************************             
VOFF     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ROFFKEY,R6                                                       
         MVI   ROFFKTYP,4                                                       
         MVC   ROFFKREP,AGENCY     REP                                          
*                                                                               
         MVC   ROFFKOFF,8(R2)                                                   
         CLC   =C'C=',8(R2)                                                     
         BNE   VOFF10                                                           
         MVC   ROFFKOFF,10(R2)                                                  
*                                                                               
VOFF10   DS    0H                                                               
         OC    ROFFKOFF,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NO                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   WORK(20),ROFFNAME                                                
         DROP  R6                                                               
                                                                                
         MVC   AIO,SVAIO                                                        
         B     YES                                                              
         EJECT                                                                  
*********************************************************************           
* VALIDATE SALESPERSON                                                          
* R2 IS FIELD HEADER                                                            
* USES IO3 FOR TEMP IOAREA                                                      
* ON EXIT:                                                                      
* WORK(20)   SALESPERSON NAME                                                   
* WORK+20(2) OFFICE CODE                                                        
*********************************************************************           
VSAL     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSALKEY,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY     REP                                          
         MVC   RSALKSAL,8(R2)      SALESMAN INITIALS                            
         OC    RSALKSAL,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NO                                                               
         DROP  R6                                                               
                                                                                
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   WORK(20),RSALNAME                                                
         MVC   WORK+20(2),RSALOFF                                               
         MVC   AIO,SVAIO                                                        
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* VALIDATE GROUP/SUBGROUP                                                       
* R2 IS FIELD HEADER                                                            
*********************************************************************           
VGRP     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RGRPKEY,R6                                                       
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,AGENCY     REP                                          
         MVC   RGRPKGRP,8(R2)      GROUP/SUBGROUP CODE                          
         OC    RGRPKGRP,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NO                                                               
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* VALIDATE TEAM                                                                 
* R2 IS FIELD HEADER                                                            
*********************************************************************           
VTEAM    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RTEMKEY,R6                                                       
         MVI   RTEMKTYP,X'05'                                                   
         MVC   RTEMKREP,AGENCY     REP                                          
         MVC   RTEMKTEM,8(R2)      DIV/TEAM                                     
         OC    RTEMKTEM,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NO                                                               
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* IF THE AGENCY RECORD IS BEING PROCESSED FOR THE FIRST TIME                    
* THEN SAVE OFF THE CURRENT DATE AND TIME TO THE X'30' ELEMENT                  
***********************************************************************         
TOUCHED  DS    0H                                                               
         OC    SELECTKY,SELECTKY                                                
         BZ    NO                                                               
                                                                                
* SET FLAG IN GENCON: APPLICATION CONTROLS READ FOR UPDATE                      
* THIS ENABLES US TO UPDATE THE RECORD WHEN THE USER PRINTS FROM                
* THE LIST SCREEN                                                               
                                                                                
         OI    GENSTAT1,RDUPAPPL                                                
                                                                                
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVI   ELCODE,X'30'        HAS RECORD EVER BEEN VIEWED/PRINTED          
         BRAS  RE,GETEL                                                         
         BE    TOUCHX                                                           
                                                                                
         MVI   RDUPDATE,C'Y'       SET READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         XC    ELEM,ELEM           NO? NOW IT HAS!                              
         LA    R6,ELEM                                                          
         USING RDARVPEM,R6                                                      
         MVI   RDARVPCD,X'30'                                                   
         MVI   RDARVPLN,RDARVPLQ                                                
                                                                                
* FETCH TODAY'S DATE                                                            
         GOTO1 DATCON,DMCB,(5,0),(2,RDARVPDT)                                   
*&&DO                                                                           
         TIME  DEC                                                              
*                                  RETRIEVE TIME:  RETURNED IN RO               
*                                     AS HH:MM:SS:TT                            
         STCM  R0,4,RDARVPTM+1     STORE MINUTES IN SAVE AREA                   
         SRL   R0,24               SHIFT HOURS TO LOW-ORDER                     
         LR    R2,R0               MOVE TO ANOTHER REGISTER                     
         LA    R2,8(R2)            ADJUST FROM DDS CLOCK TIME                   
         EDIT  (R2),(2,WORK+17),FILL=0,ZERO=NOBLANK                             
         GOTO1 HEXIN,DMCB,WORK+17,RDARVPTM,2,0                                  
*&&                                                                             
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,RDARVPTM                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 ADDELEM                                                          
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
TOUCHX   DS    0H                                                               
         NI    GENSTAT1,X'FF'-RDUPAPPL                                          
*                                  RESET STATUS                                 
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE CONTRACT - 1ST PARAMETER POINTS TO CONTRACT FIELD HEADER             
***********************************************************************         
VCON     L     R2,0(R1)                                                         
         GOTO1 ANY                                                              
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   5(R2),8                                                          
         BH    MYERR                                                            
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    MYERR                                                            
         SPACE 1                                                                
         LA    RF,CCONLEN                                                       
         XCEF  CCONNUM,(RF)        CLEAR ALL CONTRACT GLOBAL VALUES             
         SPACE 1                                                                
         ZAP   WORK+10(5),=P'0'                                                 
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)       CONTRACT NUM IN PWOS                    
         MVC   CCONKNUM,WORK                                                    
                                                                                
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)       CONTRACT NUM IN 9'S COMPLEMENT          
         SPACE 1                                                                
         MVC   CCONNUM,WORK                                                     
         SPACE 1                                                                
         MVC   RERROR,=AL2(NOTFOUND)                                            
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONPTYP,R6                                                      
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,WORK       CONTRACT NUMBER                              
         DROP  R6                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NO                                                               
         SPACE 1                                                                
         MVC   AIO,AIO2            PUT CONTRACT IN IO2                          
         MVC   CCONDKAD,KEY+28     SAVE OFF DISK ADDRESS                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RCONKEY,R6                                                       
*                                                                               
         TM    RCONCNTL,X'80'      CHECK IF CONTRACT DELETED                    
         BO    MYERR                                                            
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    CON10                                                            
         CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    CON10                                                            
         CLC   TWAACCS(2),=C'O='   TEST FOR OFFICE RESTRICTION                  
         BNE   CON10                                                            
         TM    TWAAUTH,X'80'       TEST IF TERMINAL ALLOWED ACCESS              
         BO    CON10               TO ALL OFFICES                               
         CLC   RCONKOFF,TWAACCS+2  ELSE,COMPARE OFFICES                         
         BE    CON10                                                            
         MVC   RERROR,=AL2(SECLOCK) SECURITY LOCKOUT                            
         B     MYERR                                                            
         SPACE 1                                                                
CON10    MVC   CCONKSTA,RCONKSTA   STATION CALL LETTERS                         
         MVC   ESTATION,SPACES                                                  
         MVC   ESTATION(4),RCONKSTA ALSO SAVE IN PRINTABLE FORMAT               
         CLI   RCONKSTA+4,C' '     TV                                           
         BE    CON15                                                            
         LA    RE,ESTATION+3       FOR RADIO, SHOW -A,-F,-C                     
         CLI   0(RE),C' '          AND -L FOR LOW POWER                         
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),RCONKSTA+4                                               
         SPACE 1                                                                
CON15    MVC   CCONKAGY(6),RCONKAGY  AGENCY CODE                                
         MVC   CCONKADV,RCONKADV   ADVERTISER CODE                              
         MVC   CCONKOFF,RCONKOFF                                                
         DROP  R6                                                               
         SPACE 1                                                                
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         USING RCONELEM,R6                                                      
         MVC   CCONSTAT,RCONMODR   STATUS FLAGS                                 
         MVC   CCONSAL,RCONSAL     SALESPERSON CODE                             
         MVC   CCONDAT,RCONDATE    START/END DATES                              
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,ECONDATE)     CONTRACT               
         MVI   ECONDATE+8,C'-'                           DATES                  
         GOTO1 (RF),(R1),(3,RCONDATE+3),(5,ECONDATE+9)                          
         MVC   CSOURCE,RCONRTGS    RATING SERVICE                               
         MVC   ECONBUYR,RCONBUYR   BUYER NAME                                   
         MVC   CCONWKS,RCONWKS     NUMBER OF WEEKS IN CONTRACT                  
         MVC   CCONCTGY,RCONCTGY   CATEGORY                                     
         XC    EPRDNAME,EPRDNAME                                                
         CLC   RCONPRD,SPACES                                                   
         BE    *+14                                                             
         MVC   CCONPRD,RCONPRD                                                  
         B     CON16                                                            
         DROP  R6                                                               
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        GET PRODUCT NAME                             
         BRAS  RE,GETEL                                                         
         USING RCONEXEL,R6                                                      
         MVC   EPRDNAME,RCONEXPR                                                
         DROP  R6                                                               
         SPACE 1                                                                
CON16    L     R6,AIO                                                           
         MVI   ELCODE,X'12'        GET SAR ELEMENT (IF ANY)                     
         BRAS  RE,GETEL                 -SAVE BOOKS, DEMOS, LENGTHS             
         BNE   CON18A                                                           
         USING RSAREL,R6                                                        
CON18    CLC   RSARBKS(2),=C'DR'   FOR DIRECT RESPONSE - SKIP BOOKS             
         BE    *+16                AND DEMOS                                    
         MVC   CSARBKS,RSARBKS                                                  
         MVC   CSARDEM,RSARDEM                                                  
         MVC   CSARLEN,RSARRFRM                                                 
         MVC   CSARDPT,RSARDPT                                                  
         DROP  R6                                                               
         SPACE 1                                                                
CON18A   DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'0B'        GET SAR BOOK LABEL ELEMENT (IF ANY)          
         BRAS  RE,GETEL                                                         
         BNE   CON19                                                            
         ZIC   RE,1(R6)            LENGTH OF ELEMENT                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CSAR0B(0),0(R6)        SAVE ELEMENT                              
         SPACE 1                                                                
CON19    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'A2'        GET EASI CODE ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   CON19A                                                           
         USING RCONIEL,R6                                                       
         MVC   CCONIEST,RCONIEST   ESTIMATE                                     
         OC    CCONIEST,SPACES                                                  
         MVC   CCONXEST,RCONXEST   EXPANDED ESTIMATE                            
         DROP  R6                                                               
                                                                                
CON19A   DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'1D'        GET DATE/TIME STAMPS                         
         BRAS  RE,GETEL                                                         
         BNE   CON19B                                                           
         USING RCONDREL,R6                                                      
         MVC   CDARNUM,RCONDRLK    AGY ORDER NUMBER                             
         MVC   CDARDLDT,RCONDRDD   DATE DELIVERED                               
         MVC   CDARDLTM,RCONDRTD   TIME DELIVERED                               
         MVC   CDARAPDT,RCONDRDA   DATE APPROVED                                
         MVC   CDARAPTM,RCONDRTA   TIME APPROVED                                
         MVC   CDARRJDT,RCONDRDR   DATE REJECTED                                
         MVC   CDARRJTM,RCONDRTR   TIME REJECTED                                
*                                                                               
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    CON19B                                                           
         TM    RCONDRF2,X'04'      MANUAL REVISION?                             
         BZ    CON19B                                                           
         OI    CCONFLAG,CCONMANR   SET MANUAL REVISION                          
         DROP  R6                                                               
                                                                                
CON19B   DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        GET WIP STATUS                               
         BRAS  RE,GETEL                                                         
         BNE   CON20                                                            
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'10'                                                   
         BO    CON20                                                            
         OI    CCONFLAG,CCONFSWP   SET STATION IN WIP                           
         DROP  R6                                                               
                                                                                
CON20    XC    KEY,KEY             GET MARKET NAME                              
         LA    R6,KEY                                                           
         USING RSTAREC,R6                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,CCONKSTA                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EMKTNAME,RSTAMKT    MARKET NAME                                  
         EDIT  (2,RSTACHAN),(4,ESTACHAN),ALIGN=LEFT                             
         MVC   ESTAAFFL,RSTAAFFL         AFFILIATE                              
         DROP  R6                                                               
         SPACE 1                                                                
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   STNX                                                             
*  IF STATION IS USER, CHECK THAT THE STATION IS AUTHORIZED                     
*   TO SEE THIS CONTRACT                                                        
         MVC   RERROR,=AL2(SECLOCK) SECURITY LOCKOUT                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
STN10    BRAS  RE,NEXTEL                                                        
         BNE   MYERR                                                            
         USING RSTASOEL,R6         RSTASID IS VALID SIGN-ON FOR THIS            
         CLC   TWAORIG,RSTASID     STATION'S CONTRACTS                          
         BNE   STN10                                                            
         DROP  R6                                                               
STNX     DS    0H                                                               
         SPACE 1                                                                
         XC    KEY,KEY             GET SALESPERSON NAME                         
         LA    R6,KEY                                                           
         USING RSALREC,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,CCONSAL                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   ESALNAME,RSALNAME                                                
         MVC   ESALTEL,RSALTEL     TELEPHONE NUMBER                             
         DROP  R6                                                               
         SPACE 1                                                                
         XC    KEY,KEY             GET AGENCY NAME                              
         LA    R6,KEY                                                           
         USING RAGYREC,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY(6),CCONKAGY                                             
         MVC   RAGYKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    AGY75                                                            
         CLC   KEY+25(2),KEYSAVE+25                                             
         BE    AGY75                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
AGY75    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EAGYNAM1,RAGYNAM1                                                
         MVC   EAGYNAM2,RAGYNAM2                                                
         DROP  R6                                                               
         SPACE 1                                                                
         MVC   CDARAGY(20),SPACES                                               
         XC    KEY,KEY             GET DARE AGENCY EQUIVALENCY CODE             
         LA    R6,KEY                                                           
         USING RAGY2REC,R6                                                      
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY(6),CCONKAGY                                             
         MVC   RAGK2REP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ADV10                                                            
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    AGY78                                                            
         CLC   KEY+25(2),KEYSAVE+25                                             
         BE    AGY78                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ADV10                                                            
AGY78    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   CDARAGY(20),RAGY2DAR                                             
         OC    CDARAGY(20),SPACES                                               
         DROP  R6                                                               
         SPACE 1                                                                
ADV10    DS    0H                                                               
         MVC   EADVNAME,=C'***** MISSING ******'                                
         XC    KEY,KEY             GET ADVERTISER NAME                          
         LA    R6,KEY                                                           
         USING RADVREC,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,CCONKADV                                                
         MVC   RADVKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   PRD10                                                            
         CLC   KEY+25(2),KEYSAVE+25                                             
         BE    ADV75                                                            
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    ADV75                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PRD10                                                            
ADV75    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EADVNAME,RADVNAME                                                
         DROP  R6                                                               
         SPACE 1                                                                
PRD10    EQU   *                                                                
         OC    EPRDNAME,EPRDNAME                                                
         BNE   PRDX                                                             
         XC    KEY,KEY             GET PRODUCT NAME                             
         LA    R6,KEY                                                           
         USING RPRDREC,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,CCONKADV                                                
         MVC   RPRDKPRD,CCONPRD                                                 
         MVC   RPRDKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+25(2),AGENCY                                                 
         BE    PRD75                                                            
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    PRD75                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
PRD75    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EPRDNAME,RPRDNAME                                                
         DROP  R6                                                               
PRDX     DS    0H                                                               
         SPACE 1                                                                
         XC    ECONNUM,ECONNUM                                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ECONNUM(0),8(R2)    SAVE EBCDIC CONTRACT NUMBER                  
         SPACE 1                                                                
         XC    KEY,KEY             NOW LOOK UP REP RECORD                       
         LA    R6,KEY                                                           
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   CPARREP,RREPPAR     SAVE PARENT REP                              
         DROP  R6                                                               
         SPACE 1                                                                
         MVC   AIO,AIO1            RESTORE IO AREA                              
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* LOAD OVERLAY AND GO                                                           
*******************************************************************             
LOAD     DS    0H                                                               
         ZIC   R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         GOTO1 CALLOV,DMCB,((R2),0),0                                           
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(R3)                                              
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* ROUTINE TO INITIALIZE THE SYSSPARE AREA                                       
*******************************************************************             
VINIT    DS    0H                                                               
         ICM   R3,7,1(R1)          IF PFKEY VALIDATION TABLE PASSED             
         BZ    INIT10                                                           
         BAS   RE,TESTSEL          TEST FOR SPECIAL SELECT CODE                 
         GOTO1 PFVAL,DMCB,(R3)     HANDLE LOCAL PFKEY PRESENCE                  
         BE    DUMMYERR            TAKE DUMMY ERROR EXIT FOR GOAGAIN            
*                                                                               
INIT10   MVI   SCRSTAT,0           CLEAR SCREEN STATUS BYTE                     
*                                                                               
         CLC   TWASCR,SVSCR        TEST SCREEN CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,SCRCHG                                                   
*                                                                               
         CLC   RECNUM,SVREC        TEST RECORD CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,RECCHG                                                   
*                                                                               
         MVC   BYTE,ACTNUM         MOVE CURRENT ACTION TO TEMP. W/S             
         CLI   BYTE,ACTCHA         IF CURRENT ACTION IS CHANGE                  
         BNE   *+16                                                             
         CLI   SVACT,ACTSEL        AND SAVED ACTION WAS SELECT                  
         BNE   *+8                                                              
         MVI   BYTE,ACTSEL         PRETEND CURRENT ACTION IS SELECT             
*                                                                               
         CLC   BYTE,SVACT          TEST ACTION CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,ACTCHG                                                   
*                                                                               
         TM    SCRSTAT,RECCHG      ALWAYS CLEAR IF RECORD TYPE CHANGED          
         BO    INIT20                                                           
         TM    SCRSTAT,SCRCHG      NEVER CLEAR IF SCREEN DIDN'T CHANGE          
         BZ    INIT30                                                           
         CLI   BYTE,ACTREP         ALWAYS CLEAR IF ACTION IS NOW REPORT         
         BE    INIT20                                                           
         CLI   SVACT,MYACTSEL      IF LAST ACTION NOT MY OWN SELECT             
         BE    INIT30                                                           
         CLI   BYTE,MYACTSEL       AND THIS ACTIN NOT MY OWN SELECT             
         BE    INIT30                                                           
         CLI   SVACT,ACTSEL        IF LAST ACTION NOT SELECT                    
         BE    INIT30                                                           
         CLI   BYTE,ACTSEL         AND THIS ACTION NOT SELECT                   
         BE    INIT30                                                           
*                                                                               
INIT20   DS    0H                                                               
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         MVC   WORK(L'DARPROFS),0(R2) SAVE OFF PROFILE INFO                     
*                                                                               
         LA    RE,SYSSPARE         CLEAR APPLICATION STORAGE                    
         LH    RF,=AL2(L'SYSSPARE)                                              
         XCEFL                                                                  
         LA    RE,CONHEADH         FIND END OF SCREEN                           
         SR    RF,RF                                                            
         ICM   RF,1,0(RE)                                                       
         BZ    *+10                                                             
         AR    RE,RF                                                            
         B     *-10                                                             
         LA    RE,3(RE)            BUMP PAST CONTROL BYTES                      
         LR    RF,RE                                                            
         SR    RF,RA                                                            
         SH    RF,=AL2(3520+64)    L'AVAIL TWA0 AS DEFINED IN DDGENTWA          
         LCR   RF,RF                                                            
         XCEFL ,                   CLEAR AREA AFTER SCREEN END                  
*                                                                               
         LR    R2,RA               RESTORE PROFILE INFO                         
         AHI   R2,DARPROFS-CONHEADH                                             
         MVC   0(L'DARPROFS,R2),WORK                                            
*                                                                               
INIT30   MVC   SVSCR,TWASCR        SAVE CURRENT SCREEN                          
         MVC   SVREC,RECNUM                     RECORD                          
         MVC   SVACT,BYTE                       ACTION                          
*                                                                               
         B     INITX                                                            
         CLI   TWAOFFC,C'*'        IF DDS TERMINAL                              
         BNE   INITX                                                            
         MVC   CONHED2(5),=C'(D/A=' DISPLAY D/A OF RECORD                       
         GOTO1 HEXOUT,DMCB,DMDSKADD,CONHED2+5,4,0                               
         MVI   CONHED2+13,C')'                                                  
         B     *+10                                                             
INIT40   XC    CONHED2(14),CONHED2 PRE-CLEAR D/A DISPLAY AREA                   
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* LOCAL ROUTINE TO HANDLE PFKEY PRESENCE                                        
* P1  BYTES 1-3 = A(PFKEY VAL. TABLE)                                           
*******************************************************************             
PFVAL    NTR1                                                                   
         CLI   PFKEY,0             USER HIT ENTER?                              
         BE    NO                  YES                                          
*                                                                               
         L     RF,0(R1)            RF=A(PFKEY TABLE)                            
         USING PFTABD,RF           LOOK UP PFKEY NUMBER IN TABLE                
PFV2     CLI   0(RF),X'FF'                                                      
         BE    PFERR                                                            
         CLC   PFKEY,PFTAID        MATCH ON NUMBER                              
         BE    PFV3                                                             
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     PFV2                                                             
*                                                                               
PFV3     DS    0H                                                               
         TM    PFTSTAT2,PFTRETRN   TEST RETURN TO APPLICATION                   
         BO    NO                                                               
*                                                                               
         BAS   RE,PFINVOKE         OK TO INVOKE PFKEY                           
         B     YES                 IF RETURNS, RETURN CC EQUAL                  
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO PROCESS PFKEY REQUEST                                 
***********************************************************************         
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
PFINVOKE NTR1                                                                   
         MVI   PFKEY,0             CLEAR PFKEY FOR NEXT SCREEN                  
         L     RE,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         MVI   TIOBAID-TIOBD(RE),0 CLEAR PF KEY HERE AS WELL                    
*                                                                               
         CLI   CALLSP,0            DON'T NEED TO CHECK PREVIOUS SCREEN?         
         BE    PFI40               DON'T NEED TO                                
         ZIC   R3,CALLSP                                                        
         BCTR  R3,0                                                             
         LA    R2,CALLSTCK(R3)     PREVIOUS SCREEN IS THE SAME ONE WE           
         CLC   PFTSCRN,0(R2)           WANT TO GO TO?                           
         BNE   PFI40               NO                                           
         NI    PFTSTAT,X'FF'-PFTCPROG  YES, DON'T PUSH IF WE CAN POP            
         OI    PFTSTAT,PFTRPROG                                                 
*                                                                               
PFI40    TM    PFTSTAT,PFTCPROG    TEST PFKEY GENERATES CALLPROG CALL           
         BZ    *+8                                                              
         BAS   RE,CPROG                                                         
*                                                                               
         CLI   PFTNKEYS,0          TEST KEY FIELDS PRESENT                      
         BE    *+8                                                              
         BAS   RE,EXPNDKEY         EXPAND THEM INTO 'KEY' FIELD                 
*                                                                               
         TM    PFTSTAT,PFTRPROG    POP NESTED CALL SEQUENCE                     
         BZ    PFI50                                                            
         BAS   RE,RPROG            ROUTINE TO RESTORE PREV. SCREEN              
         B     DUMMYERR            GO AGAIN TO GENCON                           
*                                                                               
PFI50    CLI   PFTREC,C' '         IF NEW RECORD TYPE DEFINED                   
         BE    PFI80                                                            
         MVC   CONREC,PFTREC       MOVE IT OUT                                  
         OI    CONRECH+6,X'80'     TRANSMIT                                     
         MVI   CONRECH+5,8         SET L'I/P                                    
*                                                                               
         L     RE,EFHKEY           RE=A(KEY FIELD)                              
         CLI   5(RE),0             IF THERE'S NO INPUT IN KEY FIELD             
         BNE   *+12                                                             
         MVI   8(RE),C','          MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   5(RE),1             APPLICATION GETS CONTROL                     
*                                                                               
PFI80    CLI   PFTACT,C' '         TEST FOR ACTION CHANGE                       
         BE    PFIX                                                             
         MVC   CONACT,PFTACT       MOVE IT OUT                                  
         OI    CONACTH+6,X'80'     TRANSMIT                                     
         MVI   CONACTH+5,5         SET L'I/P - NOTE ONLY 5                      
*                                                                               
         TM    CTLRFLG1,CF1ISREV   REVISION APPROVE/REJECT OVERRIDE             
         BZ    PFIX                                                             
         CLC   =C'OPEN',CONACT                                                  
         BNE   PFI90                                                            
         MVC   CONREC,=CL8'REVISION'                                            
         MVC   CONACT,=CL8'DIFFEREN'                                            
         B     PFIX                                                             
*                                                                               
PFI90    DS    0H                                                               
         CLC   =C'REJECT',CONACT                                                
         BNE   PFIX                                                             
         MVC   CONREC,=CL8'REVISION'                                            
*                                                                               
PFIX     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS                              
* R3=A(PFKEY TABLE)                                                             
*******************************************************************             
TESTSEL  NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TSELX                                                            
*                                                                               
         TM    CTLRFLG1,CF1TSELQ         DON'T TEST SEL FIELDS?                 
         BZ    *+12                                                             
         NI    CTLRFLG1,X'FF'-CF1TSELQ   YES, RESET FOR NEXT TIME               
         B     TSELX                                                            
*                                                                               
         OC    LISTDIR(6),LISTDIR                                               
         BZ    TSELX                                                            
*                                                                               
         LA    R4,LISTDIR          LIST OF DISK ADDRESSES                       
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         B     TSEL3                                                            
*                                                                               
TSEL2    DS    0H                                                               
         LA    R4,6(R4)            NEXT DISK ADDRESS                            
*                                                                               
TSEL3    DS    0H                                                               
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    TSEL6                                                            
         LR    RF,R3               RF=A(START OF TABLE)                         
         USING PFTABD,RF                                                        
TSEL4    CLI   0(RF),X'FF'                                                      
         BE    TSEL6                                                            
         CLC   PFTSEL(1),8(R2)     MATCH ON EXACT SELECT CODE                   
         BE    TSEL8                                                            
         CLI   PFTLEN,0                                                         
         BE    TSELX               NO MORE, EXIT                                
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     TSEL4                                                            
*                                                                               
TSEL6    ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    TSELX               (E-O-S)                                      
         TM    1(R2),X'20'         IGNORE PROTECTED FIELDS                      
         BO    TSEL6                                                            
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   TSEL2               SELECT FIELD                                 
         B     TSEL6                                                            
*                                                                               
TSEL8    DS    0H                                                               
         MVI   8(R2),C' '          FOUND A MATCH - CLEAR SELECT FIELD           
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
TSEL10   DS    0H                                                               
         LR    R3,R2               SAVE A(FIELD)                                
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   7(R2),0             TEST THERE'S SOMETHING TO SELECT             
         BE    TSEL6               (NO, SO IGNORE)                              
*                                                                               
         MVC   PFKEY,PFTAID        SET CORRESPONDING PFKEY NUMBER               
         SR    R3,RA                                                            
         STH   R3,CURDISP          SAVE DISP. TO FIELD                          
*                                                                               
         TM    CTLRFLG1,CF1SVDAQ   DON'T SAVE D/A OF SELECTED RECORD?           
         BZ    *+12                                                             
         NI    CTLRFLG1,X'FF'-CF1SVDAQ   YES, RESET FOR NEXT TIME               
         B     TSELX                                                            
*                                                                               
         MVC   KEY+28(4),2(R4)                                                  
         XC    SELECTKY,SELECTKY                                                
         OC    KEY+28(4),KEY+28                                                 
         BZ    TSELX                                                            
         MVI   RDUPDATE,C'N'                                                    
         OI    DMINBTS,X'08'       PASS BACK DELETES                            
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   SELECTKY,0(R6)                                                   
         CLC   =C'BRAND',CONREC                                                 
         BE    TSELX                                                            
         MVC   SVSELKEY,SELECTKY                                                
*                                                                               
TSELX    B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* THIS ROUTINE SAVES THE CURRENT TWA IN THE FIRST HALF OF TEMPSTR               
* RECORD NUMBERS 2.  IT THEN SAVES THE SCREEN NUMBER FOUND IN                   
* TWASCR ONTO A STACK.  THE USE OF THIS ROUTINE IN CONJUNCTION WITH             
* THE CHANGING OF THE RECORD, ACTION, AND KEY FIELDS ALLOWS USERS TO            
* CALL UP A NEW SCREEN AND THEN LATER RETURN TO THE SCREEN THEY WERE            
* WORKING ON.  WHEN THE USER WANTS TO RETURN TO A SCREEN, RETPROG WILL          
* BE CALLED TO RESTORE THE SCREEN.                                              
*******************************************************************             
CPROG    NTR1                                                                   
         CLI   CALLSP,L'CALLSTCK   IF ALREADY HAVE MAX NEST LEVELS              
         BNL   CANTPUSH                                                         
*                                                                               
         ZIC   R3,CALLSP           SAVE SCREEN NUMBER ON STACK                  
         LA    RF,CALLSTCK(R3)                                                  
         MVC   0(1,RF),TWASCR                                                   
*                                                                               
* SPECIAL FOR STEREO SCREEN INDICATOR                                           
*                                                                               
*                                                                               
         TM    STEREOFG,STFINUSE   STEREO IS IN USE                             
         BZ    CPROG20                                                          
         CLI   MYSCRNUM,X'FB'                                                   
         BE    CPROG10                                                          
         CLI   MYSCRNUM,X'FD'                                                   
         BE    CPROG10                                                          
         CLI   MYSCRNUM,X'FE'                                                   
         BNE   CPROG20                                                          
*                                                                               
CPROG10  DS    0H                                                               
         MVC   0(1,RF),MYSCRNUM                                                 
*                                                                               
CPROG20  DS    0H                                                               
         LA    R3,1(R3)            INCREMENT STACK POINTER                      
         STC   R3,CALLSP                                                        
*                                                                               
         L     RE,ATIA             SAVE SCREEN IN FIRST HALF OF TWA             
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,X'82'            WRITE TWA RECORD #2                          
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         LA    R2,X'83'            WRITE TWA RECORD #3                          
         GOTO1 GETTWA,DMCB,((R2),STARTSAV)                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* THIS ROUTINE RESTORES THE USER TO THE SCREEN THEY WERE WORKING ON             
* BEFORE CALLING ANOTHER SCREEN WHICH WAS SAVED IN TEMPSTR BY CALLPROG.         
*******************************************************************             
RPROG    NTR1                                                                   
         CLI   CALLSP,0                                                         
         BE    PFERR               ERROR IF STACK IS EMPTY                      
*                                                                               
         LA    R2,2                READ TWA RECORD #2                           
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         L     RE,ATIA             RESTORE SCREEN FROM 1ST HALF OF TWA          
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   ACTNUM,TWALACT      SPECIAL CODE TO KEEP SELECT GOING            
         MVC   RECNUM,TWALREC                                                   
         MVC   CONHEAD(30),=CL30'Came back from another screen.'                
         MVC   CONHEAD+30(30),=CL30'  Please continue ...'                      
*                                                                               
         LA    R2,3                READ TWA RECORD #3                           
         GOTO1 GETTWA,DMCB,((R2),STARTSAV)                                      
*                                                                               
         ZIC   R3,CALLSP           DECREMENT STACK POINTER                      
         BCTR  R3,0                                                             
         STC   R3,CALLSP                                                        
*                                                                               
         LA    RF,CALLSTCK(R3)     EXTRACT TWASCR                               
         MVC   TWASCR,0(RF)                                                     
*                                                                               
         L     R2,ATWA             MUST SET INDICTOR TO XMIT ALL FIELDS         
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         OI    TRNSTAT,RETURNED    SET THAT RETPROG HAS BEEN CALLED             
*                                                                               
* SPECIAL FOR STEREO. WE NEED TO SET IN THE OUT TRANSLATOR BLOCK THE            
* SCREEN NUMBER THAT WE HAVE RESTORED SO STEREO IS PASSED THE CORRECT           
* SCREEN IN USE.                                                                
*                                                                               
         TM    STEREOFG,STFINUSE   STEREO IS IN USE                             
         BZ    XIT                                                              
*                                                                               
         L     R4,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,R4                                                         
         OI    TIOBINDS,TIOBSCRN   SET SCREEN NUM IN TIOBCNT(1)                 
         MVC   TIOBCNT(1),TWASCR                                                
         DROP  R4                                                               
*                                                                               
         CLI   TWASCR,X'FD'                                                     
         BE    RPROG10                                                          
         CLI   TWASCR,X'FE'                                                     
         BNE   RPROG20                                                          
*                                                                               
RPROG10  DS    0H                                                               
         MVI   TWASCR,X'F0'                                                     
*                                                                               
RPROG20  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* ROUTINE TO EXPAND KEY FIELDS INTO TMPKEY FIELD                                
*******************************************************************             
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
EXPNDKEY NTR1                                                                   
         MVC   WORK,SPACES         BUILD KEY FIELD IN WORK FIRST                
         LA    R2,WORK             R2=A(WORK)                                   
         ZIC   R3,PFTNKEYS         R3=N'KEY FIELDS                              
         LA    R4,PFTKEYS          SET R4=A(1ST KEY FIELD)                      
         USING KEYD,R4                                                          
*                                                                               
EXP10    CLI   KEYTYPE,KEYTYCOM    TEST SIMPLY PLACE IMBEDDED COMMA             
         BE    EXP20                                                            
         LR    RF,RA               SET WHERE DATA IS                            
         CLI   KEYTYPE,KEYTYTWA    TWA                                          
         BE    EXP15                                                            
         L     RF,ASTARTSV                                                      
         LA    RF,SYSSPARE                                                      
         CLI   KEYTYPE,KEYTYWS     W/S (SYSSPARE)                               
         BE    EXP15                                                            
         CLI   KEYTYPE,KEYTYCUR    CURSOR LOCATION                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    RF,CURDISP          ASSUME THIS IS A SELECT FIELD                
         AR    RF,RA               RF=A(FLD WHERE CURSOR IS)                    
         BAS   RE,BMPTOROW         BUMP TO FIRST FIELD FOR THIS ROW             
         BNE   PFERR                                                            
         L     RF,FULL             RETURNS ADDRESS IN FULL                      
*                                                                               
EXP15    AH    RF,KEYDISP          RF=A(DATA)                                   
         ZIC   RE,KEYLEN           RE=L'DATA-1                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)       MOVE TO WORK                                 
         AR    R2,RE               BUMP TO LAST CHARACTER OF FIELD              
*                                                                               
         CLI   0(R2),C' '          SHUFFLE BACK TO 1ST NON-SPACE                
         BH    *+10                                                             
         BCTR  R2,0                                                             
         BCT   RE,*-10                                                          
         LA    R2,1(R2)            BUMP TO 1ST POSITION PAST                    
*                                                                               
         CH    R3,=H'1'            TEST THIS IS LAST KEY FIELD                  
         BE    EXPX                SO FINISH UP                                 
*                                                                               
EXP20    MVI   0(R2),C','          INSERT COMMA BEFORE NEXT FIELD               
         LA    R2,1(R2)            BUMP PAST COMMA TO NEXT POSITION             
         LA    R4,KEYNEXT          BUMP TO NEXT KEY FIELD                       
         BCT   R3,EXP10            AND PROCESS                                  
*                                                                               
EXPX     LA    R3,WORK                                                          
         SR    R2,R3               R2=L'TMPKEY FIELD                            
         CLM   R2,1,=AL1(L'TMPKEY)                                              
         BNH   *+6                                                              
         DC    H'0'                MAKE TMPKEY BIGGER                           
*                                                                               
         STC   R2,TMPKEYH+5        STORE LENGTH IN FIELD HEADER                 
         MVI   TMPKEYH,L'TMPKEY+L'TMPKEYH SET LENGTH OF FIELD                   
         LA    RE,TMPKEYH                                                       
         ST    RE,EFHKEY           TELL GENCON TO USE TMPKEY FIELD              
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     XIT                                                              
         MVC   TMPKEY(0),WORK      MOVE DATA TO FAKE KEY FIELD                  
         SPACE 5                                                                
BMPTOROW NTR1                      BUMP TO FIRST FIELD IN ROW                   
         LR    R2,RF               R2=A(CURRENT FIELD)                          
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         L     R2,AFRSTREC                                                      
BMPT2    LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE                                                        
         BE    BMPT4                                                            
         ZIC   R1,0(R2)            TRY NEXT FIELD                               
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BNE   BMPT2                                                            
         B     NO                  RETURN CC NE IF REACHED E-O-S                
*                                                                               
BMPT4    ZIC   R1,0(R2)                                                         
         AR    R2,R1               ASSUMING SELECT FIELD -- BUMP PAST           
         LA    R2,8(R2)            AND PAST HEADER OF (FIRST) DATA FLD          
         ST    R2,FULL             MATCH-RETURN A(FLD HEADER) IN FULL           
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* REFTOPAK                                                                      
*        THIS ROUTINE CONVERTS THE REFERENCE NUMBER TO ITS PACKED WITH          
* SIGN AND UNCOMPLEMENTED EQUIVALENT.                                           
*                                                                               
* ON ENTRY:    P1                  A(REFERENCE # PWOS AND 9'S COMP.)            
*                                                                               
* ON EXIT:     P1                  PACKED EQUIVALENT                            
***********************************************************************         
VREFTOPK DS    0H                                                               
         L     R4,0(R1)                                                         
         ZICM  R1,0(R4),3                                                       
         SLL   R1,4                                                             
         ST    R1,DMCB                                                          
         OI    DMCB+3,X'0F'                                                     
         SP    DMCB(4),=P'999999'  UNCOMPLEMENT REFERENCE NUMBER                
         OI    DMCB+3,X'0F'                                                     
         B     XIT                                                              
***********************************************************************         
* PAKTOREF                                                                      
*        THIS ROUTINE CONVERTS THE REFERENCE NUMBER TO ITS PACKED               
* WITHOUT SIGN AND 9'S COMPLEMENTED EQUIVALENT.                                 
*                                                                               
* ON ENTRY:    P1                  A(REFERENCE NUMBER)                          
*                                                                               
* ON EXIT:     P1                  PWOS AND 9'S COMPLEMENT                      
***********************************************************************         
VPKTOREF DS    0H                                                               
         L     R4,0(R1)                                                         
         L     R1,0(R4)                                                         
         ST    R1,DMCB                                                          
         SP    DMCB(4),=P'999999'  COMPLEMENT IT                                
         OI    DMCB+3,X'0F'                                                     
         L     R1,DMCB                                                          
         SRL   R1,4                                                             
         ST    R1,DMCB                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ/WRITE TEMPSTR PAGES                                           
* P1, BYTE  0=BIT SETTINGS/PAGE NUMBER                                          
* P1, BYTES 1-3=READ/WRITE ADDRESS                                              
***********************************************************************         
VGETTWA  MVC   BYTE,0(R1)          BIT SETTINGS/PAGE NUMBER                     
         L     R2,0(R1)            READ/WRITE ADDRESS                           
*                                                                               
         MVC   COMMAND(6),=C'DMWRT '                                            
         TM    BYTE,X'80'          X'80'=1 IS WRITE, ELSE READ                  
         BO    GTWA10                                                           
         MVC   COMMAND(6),=C'DMRDIR'                                            
         TM    BYTE,X'40'          X'40'=1 IS 2304 BYTE TWAS ELSE 6144          
         BNZ   GTWA10                                                           
         MVC   COMMAND(6),=C'DMREAD'                                            
*                                                                               
GTWA10   NI    BYTE,X'0F'          TURN OFF HIGH ORDER BITS                     
*                                                                               
         MVC   DMCB+8(1),BYTE      PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
*                                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TEMPSTR',,(R2),0                         
*                                                                               
         CLI   8(R1),0             IF COULDN'T DO IT, DIE                       
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*              LOCAL EXIT/ERROR ROUTINES                                        
***********************************************************************         
CANTPUSH MVI   GERROR1,ERNOPUSH    PUSH ERROR - TOO MANY NEST LEVELS            
         B     RETCURS                                                          
PFERR    MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
RETCURS  LR    R2,RA                                                            
         AH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         GOTO1 MYERR                                                            
*                                                                               
PLSENTER MVI   GERROR1,2           PLEASE ENTER FIELDS AS REQUIRED              
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         L     R2,AFRSTKEY         R2 TO 1ST KEY FIELD                          
         L     RE,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         NI    TIOBINDS-TIOBD(RE),X'FF'-TIOBALRM   TURN OFF BEEP                
         GOTO1 MYERR                                                            
*                                                                               
DUMMYERR MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
DUMYERR1 LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         GOTO1 ERREX2                                                           
*                                                                               
SELFIRST MVI   GERROR1,10          SELECT OR HIT ENTER FOR FIRST                
         B     *+8                                                              
SELNEXT  MVI   GERROR1,9           SELECT OR HIT ENTER FOR NEXT                 
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         MVI   OKNO,0              CLEAR OKNO SO WON'T LOOP ENDLESSLY           
         L     R2,AFRSTREC         R2 TO 1ST SEL FIELD                          
         GOTO1 MYERR                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
* THIS ROUTINE INTERFACES TO GENCON'S ERREX ROUTINE AND SETS SPECIAL            
* PARAMETERS TO MAKE IT CALL GETTXT INSTEAD OF GETMSG.  SINCE REDAR             
* USES MESSAGE NUMBERS GREATER THAN 255, GETTXT MUST BE USED BECAUSE            
* GETMSG IS NOT DESIGNED TO HANDLE THESE NUMBERS.                               
*******************************************************************             
MYERR    OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,RINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,RERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,RMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,RTXTLEN      LENGTH OF OPTIONAL TEXT                      
         MVC   GTATXT,RTXTADR      A(OPTIONAL TEXT)                             
         CLC   RERROR,=H'60'       IF MESSAGE NUMBER <= 60                      
         BH    *+8                                                              
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
         DROP  RF                                                               
         GOTO1 ERREX                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL (R6),DATADISP,ELCODE                                             
*              CONSTANTS TABLES ETC                                             
RELO     DS    A                                                                
SAVERD   DS    A                                                                
         SPACE 1                                                                
SYSVCON  DS    0F                                                               
         DC    V(RECUP)                                                         
         DC    V(DUMMY)                                                         
         DC    V(OUTDAY)                                                        
         DC    V(REGENDAB)                                                      
         DC    V(REGENDHT)                                                      
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    V(REDARTKO)                                                      
         SPACE 1                                                                
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 3                                                                
*  TABLE OF CORE RESIDENT MODULE ADDRESSES                                      
CORETAB  DS    0X                                                               
         DC    X'30'               GENCON                                       
         DC    X'E0'               DEMOCON                                      
         DC    X'47'               RANSID                                       
         DC    X'26'               DEFINE                                       
         DC    X'21'               SPDEMLK (SPGETDEMO)                          
         DC    X'13'               UPVAL                                        
         DC    X'22'               SPDEMUP                                      
         DC    AL1(QQSORT)         QSORT                                        
         DC    X'08'               DEMUP                                        
         DC    X'09'               INVEDIT                                      
         DC    X'1D'               GETBROAD                                     
         DC    X'AC'               REPFACS                                      
*                                                                               
CORES    EQU   (*-CORETAB)                                                      
                                                                                
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 3                                                                
RECACT   DS    0D                                                               
         SPACE 1                                                                
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
         SPACE 1                                                                
         DC    X'01',C'ORDER   ',AL1(01),X'0000'                                
         DC    X'01',C'BUY     ',AL1(02),X'0000'                                
         DC    X'01',C'CONTRACT',AL1(03),X'0000'                                
         DC    X'01',C'MGGROUP ',AL1(04),X'0000'                                
         DC    X'01',C'BRAND   ',AL1(05),X'0000'                                
         DC    X'01',C'REVISION',AL1(06),X'0000'                                
         DC    X'01',C'HISTORY ',AL1(07),X'0000'                                
                                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
                                                                                
* DARE ACTIONS ARE LIMITED                                                      
*                                                                               
*        DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'SELECT  ',AL1(13,13,00)                                  
         DC    X'02',C'OPEN    ',AL1(15,15,00)                                  
         DC    X'02',C'REJECT  ',AL1(16,15,00)                                  
         DC    X'02',C'DIFFEREN',AL1(17,17,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
                                                                                
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                      01=USER MAINTENANCE                      
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
         SPACE 1                                                                
         DC    X'03',AL1(01,10),X'F001000080',C'    '  ORDER     LIST           
         DC    X'03',AL1(01,13),X'F001000081',C'    '  ORDER     SEL            
*        DC    X'03',AL1(01,12),X'00300030C0',C'    '  ORDER     REPORT         
         DC    X'03',AL1(01,15),X'F920000081',C'    '  ORDER     OP/RJ          
         DC    X'03',AL1(01,18),X'E050000081',C'    '  ORDER     OP/RJ          
         DC    X'03',AL1(02,10),X'FA10000081',C'    '  BUY       LIST           
         DC    X'03',AL1(03,10),X'F115000080',C'    '  CONTRACT  LIST           
*        DC    X'03',AL1(04,01),X'F319000081',C'    '  MGGROUP   DIS            
         DC    X'03',AL1(04,10),X'F218000080',C'    '  MGGROUP   LIST           
         DC    X'03',AL1(04,13),X'F319000081',C'    '  MGGROUP   SEL            
         DC    X'03',AL1(06,10),X'F540000081',C'    '  REVISION  SEL            
         DC    X'03',AL1(06,12),X'F612003120',C'DADA'  REVISION  REPORT         
         DC    X'03',AL1(06,15),X'F822000081',C'    '  REVISION  REJECT         
         DC    X'03',AL1(06,17),X'E822000081',C'    '  REVISION  OP/RJ          
         DC    X'03',AL1(07,10),X'E050000081',C'    '  HISTORY   LIST           
*                                                                               
* MULTIPLE BRAND ORDERS CAN BE ATTACHED TO AN ORDER                             
*                                                                               
         DC    X'03',AL1(05,10),X'F001000080',C'    '  BRAND     LIST           
         DC    X'03',AL1(05,13),X'F001000081',C'    '  BRAND     SEL            
         DC    X'03',AL1(05,12),X'00300030C0',C'    '  BRAND     REPORT         
         DC    X'03',AL1(05,15),X'F920000081',C'    '  BRAND     OP/RJ          
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*************                                                                   
* DDSPOOLD  *                                                                   
*************                                                                   
         SPACE 1                                                                
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
**************                                                                  
* DDSPLWORKD *                                                                  
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
************                                                                    
* REDARFFD *                                                                    
************                                                                    
         SPACE 1                                                                
       ++INCLUDE REDARFFD                                                       
         SPACE 1                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE REDARF0D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REDARF2D          MAKEGOOD LIST SCREEN                         
**********************************************                                  
* DDGENTWA - DSECT TO COVER GENCON TWA AREAS *                                  
**********************************************                                  
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
**************                                                                  
* REDARWTWA  *                                                                  
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE REDARTWA                                                       
         EJECT                                                                  
**************                                                                  
* REDARWORKD *                                                                  
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE REDARWORKD                                                     
         EJECT                                                                  
****************************                                                    
* REGENALLA                *                                                    
* DEDEMFILE                *                                                    
* CTGENFILE                *                                                    
* DDCOMFACS                *                                                    
* FAFACTS                  *                                                    
* FATIOB                   *                                                    
* DDREPMASTD               *                                                    
* DDCOREQUS                *                                                    
* DEDBLOCK (DBLOCKD DSECT) *                                                    
****************************                                                    
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE REGENALLA                                                      
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REGENDAR                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE REPFACSQ                                                       
       ++INCLUDE RECNTPROF                                                      
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017REDAR00S  09/26/03'                                      
         END                                                                    
