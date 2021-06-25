*          DATA SET CTMAD26    AT LEVEL 109 AS OF 12/21/09                      
*PHASE TA0C26B                                                                  
*INCLUDE RECUP                                                                  
*==============================================================                 
* SCHLNNUM IS THE UNIQUE IDENTIFIER FOR UPLOADED BUYS                           
*==============================================================                 
         TITLE 'TA0C26 - $MAD UPLOAD BUYLINES'                                  
TA0C26   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENBLNTB,TA0C26,RA,RR=R2,CLEAR=YES                               
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
*                                                                               
         LR    RF,RC               RF = A(OVERLAY'S SPARE MEMORY)               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
*                                                                               
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
         DROP  R7                                                               
*                                                                               
         ST    RD,SAVERD           SAVE STACK POINTER FOR RETURNING             
         ST    RC,ACONTROL         SAVE A(CONTROLLER STORAGE)                   
         ST    RF,ABLNTBL          SAVE A(BUYLINE TABLE)                        
         ST    R2,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         L     RE,=V(RECUP)                                                     
         A     RE,APRELO                                                        
         ST    RE,RECUP                                                         
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   PERVERT,CPERVERT                                                 
         DROP  RE                                                               
*                                                                               
         LHI   R0,QDAYUNPK                                                      
         ICM   R0,14,=XL3'D9000A'                                               
         GOTO1 CALLOV,DMCB,0,(R0)                                               
         MVC   DAYUNPK,0(R1)                                                    
*                                                                               
         LHI   R0,QSPAUTH                                                       
         ICM   R0,14,=XL3'D9000A'                                               
         GOTO1 CALLOV,DMCB,0,(R0)                                               
         MVC   SPAUTH,0(R1)                                                     
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         EJECT                                                                  
*=====================================================================          
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*=====================================================================          
         SPACE 1                                                                
MAIN     MVI   SETSENT,C'N'        INITIALIZE SYSTEM                            
         GOTO1 SETSYS,DMCB,=C'SPOT',=CL8'SPTDIR',=CL8'SPTFIL'                   
         BNE   EXIT2                                                            
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      B     EXIT2               ELSE EXIT (SKIP WORKER FILE CLOSE)           
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
*=====================================================================          
* PROCESS START MODE                                                            
* GET HEADER OBJECT THEN SCHEDULE AND SPILL OBJECTS                             
*=====================================================================          
         SPACE 1                                                                
PROCSTRT NTR1  ,                                                                
         GOTO1 WRKCRE,DMCB,0       OPEN WORKER FILE FOR CREATION                
         BRAS  RE,GETHDR           GET THE HEADER OBJECT                        
****                                                                            
         CLI   CABLEFLG,C'Y'       ARE WE CABLE?                                
         BE    PS20                YES, PROCESS AS USUAL                        
         LA    R1,IDLIST                                                        
PS10     CLI   0(R1),X'FF'         ID IS ON EXCEPTION LIST?                     
         BE    INVLVFS             NO, VFS IS DEACTIVATED                       
         CLC   SIGNON8,0(R1)                                                    
         BE    PS20                YES, PROCESS AS USUAL                        
         LA    R1,L'IDLIST(R1)                                                  
         B     PS10                                                             
****                                                                            
PS20     BRAS  RE,PROCALL          DO THIS EVERY TRANSACTION                    
         J     XIT                                                              
IDLIST   DS    0CL10                                                            
*&&DO                                                                           
         DC    C'CARATL    '       THIS WAS THE LIST OF USERIDS THAT            
         DC    C'CARLA     '         STILL WANTED VFS TIL DEC18/09              
         DC    C'CARNY     '                                                    
         DC    C'CLDE      '                                                    
         DC    C'CLNYNC    '                                                    
         DC    C'LCIDA     '                                                    
         DC    C'LCIDAA    '                                                    
         DC    C'LCIDE     '                                                    
         DC    C'LCIDEA    '                                                    
         DC    C'LCINO     '                                                    
         DC    C'LCINY     '                                                    
         DC    C'MCLAS     '                                                    
         DC    C'MUWEN     '                                                    
         DC    C'MUWS      '                                                    
         DC    C'UMSF      '                                                    
         DC    C'WIAT      '                                                    
         DC    C'WICH      '                                                    
         DC    C'WILA      '                                                    
         DC    C'WINY      '                                                    
         DC    C'WIPL      '                                                    
         DC    C'ZEAT      '                                                    
         DC    C'ZEATNC    '                                                    
         DC    C'ZECH      '                                                    
         DC    C'ZECHNC    '                                                    
         DC    C'ZEDA      '                                                    
         DC    C'ZEDANC    '                                                    
         DC    C'ZEDN      '                                                    
         DC    C'ZEDNNC    '                                                    
         DC    C'ZEKC      '                                                    
         DC    C'ZEKCNC    '                                                    
         DC    C'ZELA      '                                                    
         DC    C'ZELANC    '                                                    
         DC    C'ZENY      '                                                    
         DC    C'ZENYA     '                                                    
         DC    C'ZENYNC    '                                                    
         DC    C'ZEOCNC    '                                                    
         DC    C'ZEPO      '                                                    
         DC    C'ZEPONC    '                                                    
         DC    C'ZESF      '                                                    
         DC    C'ZOGAT     '                                                    
         DC    C'ZOGCH     '                                                    
         DC    C'ZOGDA     '                                                    
         DC    C'ZOGDN     '                                                    
         DC    C'ZOGKC     '                                                    
         DC    C'ZOGLA     '                                                    
         DC    C'ZOGMI     '                                                    
         DC    C'ZOGNY     '                                                    
         DC    C'ZOGNYM    '                                                    
         DC    C'ZOGNYP    '                                                    
         DC    C'ZOGNYT    '                                                    
         DC    C'ZOGON     '                                                    
         DC    C'ZOGPO     '                                                    
         DC    C'ZOGSF     '                                                    
*&&                                                                             
IDLISTX  DC    X'FF'                                                            
         EJECT                                                                  
*=====================================================================          
* THIS ROUTINE PROCESSES THE CONTINUATION MODE.  IT CONTINUES TO GET            
* ALL THE SCHEDULES UNTIL AN END-OF-DATA OBJECT OCCURS.                         
*=====================================================================          
         SPACE 1                                                                
PROCMID  NTR1  ,                                                                
         CLI   ENDSCHED,C'Y'       IF END OF SCHEDULES                          
         BNE   PM10                                                             
         GOTO1 WRKREGT             REOPEN WORKER FILE FOR GETS                  
         B     PM20                                                             
*                                                                               
PM10     GOTO1 WRKREPT             ELSE REOPEN WORKER FILE FOR PUTS             
*                                                                               
PM20     BAS   RE,PROCALL          DO THIS EVERY TRANSACTION                    
*                                                                               
PMX      J     XIT                                                              
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO LOOP ROUND, GET SCHEDULES THEN ADD BUYS AND T/A REQUEST            
*=====================================================================          
         SPACE 1                                                                
PROCALL  NTR1  ,                                                                
         CLI   ENDSCHED,C'Y'       IF NOT END OF SCHEDULES                      
         BE    PA10                                                             
*                                                                               
         BRAS  RE,GETSCH           THEN GET THE SCHEDULE OBJECTS                
         CLI   ENDSCHED,C'Y'       IF END OF SCHEDULES                          
         JNE   XIT                                                              
*                                                                               
         GOTO1 WRKCLOS             THEN CLOSE WORKER FILE                       
         GOTO1 WRKLOC              REOPEN WORKER FILE FOR GETS                  
         MVI   OBJFLAG,0           INITIALIZE LAST OBJ FLAG                     
         GOTO1 WRKGET,DMCB,AFREE   GET AND THROW AWAY MAD HEADER OBJ            
*                                                                               
         LA    R0,SCHOBJCT                                                      
         LHI   R1,SCHLEN                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SPLOBJCT(SPLLEN),SPLOBJCT                                        
         XC    SVPURP,SVPURP                                                    
*                                                                               
PA10     BRAS  RE,CONFIRM          SEND OUT CONFIRMATIONS                       
*                                                                               
         CLI   MDLAST,C'Y'         IF LAST TRANSACTION                          
         JNE   XIT                                                              
         CLI   HDRGENTA,C'Y'       TEST TO GENERATE T/A REQUESTS                
         JNE   XIT                                                              
         BRAS  RE,ADDREQ                                                        
         J     XIT                                                              
         EJECT                                                                  
*=====================================================================          
* THIS ROUTINE CONFIRMS THE BUILDING OF THE BUY RECORDS USING THE               
* SCHEDULE OBJECTS BY SENDING OUT CONFIRMATION OBJECTS.                         
* NOTE SEQUENCE OF OBJECTS ---                                                  
* FOR RADIO SPILL OBJECT (WITH OR WITHOUT PURPOSE CODES) ALWAYS                 
*   COMES BEFORE SCHED OBJECT                                                   
* FOR CABLE, SCHEDOBJ THEN SPILLOBJ ARE ALWAYS PAIRED                           
*=====================================================================          
         SPACE 1                                                                
CONFIRM  NTR1  ,                                                                
         CLI   CONFLAG,C'Y'        IF PENDING CONFIRM OBJECT PUTITEM            
         BE    CNF20               THEN PUT OUT CONFIRMATION OBJECT             
*                                                                               
CNF02    BRAS  RE,GETTMPL          GET A SCHEDULE OBJECT FROM NWK FILE          
*                                                                               
         CLI   OBJFLAG,C'Z'        DONE IF NO MORE SCHEDULES                    
         BNE   CNF04                                                            
         MVI   MDLAST,C'Y'         LAST OUTPUT DATA FRAME                       
         MVI   SETSENT,C'Y'        SET WORKER FILE TO SENT AFTER CLOSE          
*                                                                               
         GOTO1 PUTITEM,DMCB,ITEOD,0  PUT END-OF-DATA OBJECT OUT                 
         BNE   EXIT                                                             
         B     CNFX                EXIT ROUTINE                                 
*                                                                               
CNF04    XC    CNFOBJCT(CNFLENQ),CNFOBJCT                                       
         MVC   CNFERROR,=CL2'00'                                                
         MVI   CNFLEN,CNFLNQ1                                                   
*                                                                               
         CLI   CABLEFLG,C'Y'                                                    
         BE    CNF06                                                            
         CLI   OBJFLAG,C'P'        TEST READ SPILL OBJECT                       
         BNE   CNF06               NO                                           
*                                                                               
         MVC   CNFLNNUM,=CL8'*MARKET*' CONFIRM SPILL OBJ FOR STRATA             
         MVI   CNFLEN,L'CNFLNNUM+L'CNFERROR                                     
         B     CNF20               LOOP BACK FOR FOLLOWING SCHEDULES            
*                                                                               
CNF06    GOTO1 HEXOUT,DMCB,SCHLNNUM,CNFLNNUM,L'SCHLNNUM,0,0,0                   
*                                                                               
         CLI   NUMERRS,0           TEST IF ANY SLINE ERRORS                     
         BE    *+8                 NO                                           
         BRAS  RE,CHKERR           CHECK IF ANY ERRORS FOR THIS SLINE           
*                                                                               
         OC    SCHLNNUM,SCHLNNUM   MAKE SURE THIS IS RIGHT                      
         BNZ   CNF07                                                            
*                                                                               
         MVC   CNFERROR,=C'FF'                                                  
         MVI   CNFLEN,L'CNFLNNUM+L'CNFERROR                                     
         B     CNF20               LOOP BACK FOR FOLLOWING SCHEDULES            
*                                                                               
CNF07    OC    SCHERROR,SCHERROR   IF THERE WAS AN ERROR                        
         BZ    CNF08               YES                                          
*                                                                               
         MVI   CNFLEN,L'CNFLNNUM+L'CNFERROR                                     
         GOTO1 HEXOUT,DMCB,SCHERROR,CNFERROR,L'SCHERROR,0,0,0                   
         B     CNF20               PUT OUT THE ERROR IN CONFIRMATION            
*                                                                               
CNF08    MVC   BSPOTLEN,SCHTOTLN   SPOT LENGTH IS TOTAL SPOT LENGTH             
         GOTO1 HEXOUT,DMCB,SCHLNNUM,CNFLNNUM,L'SCHLNNUM,0,0,0                   
*                                                                               
         BRAS  RE,BLDBRECS         BUILD THE BUY RECORDS                        
*                                                                               
         XC    SVSPLMKT,SVSPLMKT                                                
*                                                                               
CNF20    LHI   RF,ITUPLCNF                                                      
         XR    R0,R0                                                            
         IC    R0,CNFLEN                                                        
         GOTO1 PUTITEM,DMCB,(RF),(R0),CNFOBJCT                                  
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME FLAG SET                     
         BE    CNFX                THEN WE NEED MORE                            
         B     CNF02               LOOP BACK UNTIL NO MORE SCHEDULES            
*                                                                               
CNFX     MVI   CONFLAG,C'Y'        CONFIRM OBJECT PUTITEM PENDING               
         B     XIT                 RETURN TO THE CALLER                         
         EJECT                                                                  
*=====================================================================          
* BUILD BUY RECORDS BASED ON INFORMATION FROM THE SCHEDULE OBJECTS              
*=====================================================================          
         SPACE 1                                                                
BLDBRECS NTR1  ,                                                                
         XC    LNEWTOT,LNEWTOT     CLEAR NEW SPOTS/WEEK ARRAY                   
         MVI   LFLAG,0                                                          
*RE-READ THE STATION (PAN=CTMADUS)                                              
         GOTO1 VALISTA,DMCB,(X'80',SCHSTA),SPLNETWK                             
         L     RE,AIO                                                           
         USING STAREC,RE                                                        
         MVC   SVSTABTY,SBKTYPE    SAVE STATION BOOKTYPE IF ANY                 
         CLI   SVSTABTY,C'A'                                                    
         BL    *+12                                                             
         CLI   SVSTABTY,C'Z'                                                    
         BNH   *+8                                                              
         MVI   SVSTABTY,0                                                       
         DROP  RE                                                               
*                                                                               
         CLI   CABLEFLG,C'Y'                                                    
         BE    BLDB01                                                           
         CLC   SPLMKT,=8C'0'       SPILL PROCESSING?                            
         BH    BLDB08              YES                                          
*                                                                               
BLDB01   BRAS  RE,DARBATCH         ADD DARE BATCH REC IF NEEDED                 
         BRAS  RE,BLDBLTB          BUILD THE BUYLINE TABLE                      
*                                                                               
         LHI   R0,MAXBLNS                                                       
         L     R2,ABLNTBL                                                       
         USING BLND,R2                                                          
*                                                                               
BLDB02   CLC   BLNSEQ,SCHLNNUM     MATCH LINE NUMBER?                           
         BE    BLDB06              YES - RETRANSFER                             
         AHI   R2,BLNLNQ                                                        
         BCT   R0,BLDB02                                                        
*                              *** PROCESS ORIGINAL BUYLINE TRANSFER            
BLDB04   MVI   LBADDCHA,LBADD      SET ADDING                                   
         BRAS  RE,BLDFACT          BUILD TRACE ELEMENT                          
         BRAS  RE,BLDWKS           BUILD WEEKS TABLE                            
         BRAS  RE,ADDBUY           BUILD BUYLINES                               
         BE    BRCYES                                                           
         B     BRCNO                                                            
*                              *** PROCESS BUYLINE RETRANSFER                   
BLDB06   ST    R2,AENTRY           SAVE MASTER ENTRY                            
         OI    SCHINDS,SCHIRET     SET RE-TRANSFER FLAG FOR BLDFACT             
         BRAS  RE,BLDFACT          BUILD TRACE ELEMENT                          
         BRAS  RE,RESFR            BUILD BUYLINES                               
         BE    BRCYES                                                           
         B     BRCNO                                                            
*                              *** PROCESS RETRANSFER FOR SPILL MARKET          
BLDB08   BRAS  RE,BLDFACT          BUILD TRACE ELEMENT                          
         BRAS  RE,BLDBLTB          BUILD THE BUYLINE TABLE                      
         BRAS  RE,RESPL            BUILD SPILL RETRANSFER                       
         BE    BRCYES                                                           
         B     BRCNO                                                            
*                                                                               
BRCYES   TM    LFLAG,LDELETE                                                    
         BO    BRCYES1                                                          
         BRAS  RE,ADDSTA           UPDATE STATION LIST                          
*                                                                               
BRCYES1  MVI   BYTE,0              INDICATORS                                   
         TM    SCHINDS,SCHIRET     TEST RETRANSFER                              
         BZ    BRCYES2                                                          
         TM    LFLAG,LDELETE                                                    
         BZ    *+8                                                              
         OI    BYTE,CNFIDEL        SET BUY LINES DELETED FLAG                   
         TM    LFLAG,LFREEZE                                                    
         BZ    *+8                                                              
         OI    BYTE,CNFIFRZ        SET FREEZE INDICATOR                         
*                                                                               
BRCYES2  GOTO1 HEXOUT,DMCB,BYTE,CNFINDS,L'BYTE,0,0,0                            
*                                                                               
         BRAS  RE,SETLNEW          SET UP SPOTS/WEEK ARRAY                      
*                                                                               
         XR    R0,R0                                                            
         IC    R0,BNUMWKS          SEND BACK NEW SPOTS/WK ARRAY                 
         GOTO1 HEXOUT,DMCB,LNEWTOT,CNFSPOTS,(R0),0,0,0                          
         XR    R1,R1                                                            
         IC    R1,CNFLEN                                                        
         SLL   R0,1                ADJUST CONFIRM OBJECT LENGTH                 
         AR    R1,R0                                                            
         STC   R1,CNFLEN                                                        
         J     YES                 RETURN 'YES' TO CALLER                       
*                                                                               
BRCNO    GOTO1 HEXOUT,DMCB,BYTE,CNFERROR,L'BYTE,0,0,0                           
         MVI   CNFLEN,L'CNFLNNUM+L'CNFERROR                                     
         XC    APPLERR,APPLERR                                                  
         J     NO                  RETURN 'NO' TO CALLER                        
         EJECT                                                                  
*=====================================================================          
* BUILD BTRCELEM (X'98')                                                        
* ALLOWS US TO TRACE WHERE BUY CAME FROM                                        
* ELEMENT HOLDS: SCHEDULE #, DATE, TIME, AND LUID                               
*                                                                               
* EXIT: SET DATE + TIME IN CONFIRMATION OBJECT                                  
*=====================================================================          
         SPACE 1                                                                
BLDFACT  NTR1  ,                                                                
         LA    R6,TRCEELEM         SET UP THE TRACE ELEMENT                     
         USING BTRCELEM,R6                                                      
         XC    TRCEELEM,TRCEELEM                                                
         MVI   BTRCCODE,BTRCCODQ                                                
         MVI   BTRCLEN,BTRCLENQ                                                 
         MVC   BTRCSEQN,SCHLNNUM   SAVE SCHEDULE LINE SEQUENCE NUMBER           
*                                                                               
         GOTO1 GETFACT,DMCB,(X'02',0)  GET INFO (DATE, TIME, LUID)              
*                                                                               
         L     R1,0(R1)            R1 = A(GETFACT BLOCK)                        
         USING FACTSD,R1                                                        
*                                                                               
         MVC   BTRCDATE,FADATEB    SAVE THE DATE                                
         MVC   BTRCLUID,FASYM      SAVE THE TERMINAL SYSMOLIC ID                
*                                                                               
         TIME  DEC                 R0 = HHMMSSTH                                
         LR    R2,R0                                                            
*                                                                               
         XC    DUB,DUB             CONVERT HOURS TO BINARY AND SAVE             
         SRL   R0,24                                                            
         SLL   R0,4                                                             
         ST    R0,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         STC   R1,BTRCTIME                                                      
*                                                                               
         XC    DUB,DUB             CONVERT MINUTES AND SAVE                     
         LR    R0,R2                                                            
         SLL   R0,8                                                             
         SRL   R0,24                                                            
         SLL   R0,4                                                             
         ST    R0,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         STC   R1,BTRCTIME+1                                                    
*                                                                               
         XC    DUB,DUB             CONVERT SECONDS AND SAVE                     
         LR    R0,R2                                                            
         SLL   R0,16                                                            
         SRL   R0,24                                                            
         SLL   R0,4                                                             
         ST    R0,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         STC   R1,BTRCTIME+2                                                    
*                                  FILL IN SOME CONFIRMATION INFO               
         TM    SCHINDS,SCHIRET     TEST FOR RE-TRANSFER                         
         BO    BFCX                YES-DON'T NEED TO PASS IT BACK               
         GOTO1 HEXOUT,DMCB,BTRCTIME,CNFTTIME,L'BTRCTIME,0,0,0                   
         GOTO1 (RF),(R1),BTRCDATE,CNFTDATE,L'BTRCDATE,0,0,0                     
*                                                                               
BFCX     J     XIT                 RETURN TO CALLER                             
         DROP  R1,R6                                                            
         EJECT                                                                  
*=====================================================================          
* BUILD BUYLINE TABLE                                                           
* READ ALL BUYLINES FOR A GIVEN MASTER KEY                                      
*=====================================================================          
         SPACE 1                                                                
BLDBLTB  NTR1  ,                                                                
         L     R0,ABLNTBL          CLEAR THE BUYLINE TABLE                      
         LHI   R1,LENBLNTB                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ABLNTBL                                                       
         LHI   R1,1                                                             
         LHI   R0,MAXBLNS                                                       
         USING BLND,R2                                                          
BBL02    STC   R1,BLNLINE                                                       
         AHI   R2,BLNLNQ                                                        
         AHI   R1,1                                                             
         CHI   R1,MAXBLNS                                                       
         BNH   BBL02                                                            
*                                                                               
         LA    R4,KEY              BUILD KEY FOR THE MASTER KEY                 
         USING BUYRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   BUYKAM,BAGYMED                                                   
         MVC   BUYKCLT,BCLT                                                     
         MVC   BUYKPRD,KEYPRD                                                   
         MVC   BUYMSTA,BMKTSTA                                                  
         MVC   BUYKEST,BEST                                                     
         MVC   PARTKEY,KEY         MAKE A COPY OF KEY DETAILS                   
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 HIGH                                                             
         B     BBL06                                                            
*                                                                               
BBL04    GOTO1 SEQ                                                              
*                                                                               
BBL06    CLC   PARTKEY,KEY         IF SAME MASTER KEY                           
         BNE   BBLX                                                             
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         XR    R2,R2                                                            
         IC    R2,BUYKEST+2        R2 = A(BUYLINE ENTRY)                        
         AHI   R2,-1                                                            
         MHI   R2,BLNLNQ                                                        
         A     R2,ABLNTBL          ZERO BASE AND INDEX INTO TABLE               
*                                                                               
         TM    KEY+13,X'80'        RECORD DELETED?                              
         BZ    *+12                NO                                           
         MVI   BLNSTAT,BLNSDEL                                                  
         B     BBL04                                                            
*                                                                               
         CLI   KEYPRD,FF           IF PRODUCT IN KEY IS NOT POL                 
         BE    BBL08                                                            
         MVC   BLNMAS,KEYPRD       THEN COPY THE PRODUCT CODE                   
         B     BBL10                                                            
*                                                                               
BBL08    GOTO1 GETELEM,DMCB,1      ELSE GET IT FROM DESCRIPTION ELEM            
         BE    *+6                                                              
         DC    H'0'                DIE IF NO DESCRIPTION ELEMENT                
         USING BDELEM,R6                                                        
         MVC   BLNMAS,BDMASPRD                                                  
         CLI   BLNMAS,0            TEST FOR UNALLOCATED MASTER PRODUCT          
         BNE   *+8                 NO                                           
         MVI   BLNMAS,FF           YES-SET TO POL (TRUE POL)                    
*                                                                               
BBL10    GOTO1 GETELEM,DMCB,4      GET PIGGYBACK CODE IF ANY                    
         BNE   BBL12                                                            
         USING PBELEM,R6                                                        
         MVC   BLNPIG,PBPROD                                                    
*                                                                               
BBL12    GOTO1 (RF),(R1),BTRCCODQ  GET TRACE ELEMENT (X'98')                    
         BNE   BBL14                                                            
         USING BTRCELEM,R6                                                      
         MVC   BLNSEQ,BTRCSEQN     EXTRACT SCHEDULE SEQUENCE #                  
*                                                                               
BBL14    GOTO1 (RF),(R1),5         GET TRACE ELEMENT (X'98')                    
         BNE   BBL16                                                            
         USING PKGELEM,R6                                                       
         MVI   BLNPACK,BLNPMSTR                                                 
         CLI   PKGIND,2                                                         
         BNE   *+8                                                              
         MVI   BLNPACK,BLNPSLVE                                                 
*                                                                               
BBL16    CLI   SVB0PROF+9,C'Y'     TEST PURPOSE CODES REQD                      
         BNE   BBL20                                                            
         GOTO1 (RF),(R1),X'70'                                                  
         USING IDELEM,R6                                                        
         CLC   3(6,R6),SVPURP      MATCH 6 CHARS OF PURPOSE CODE                
         BE    BBL20                                                            
         MVI   BLNSTAT,BLNSPURP    SET DIFFERENT PURPOSE CODE                   
                                                                                
BBL20    B     BBL04               NEXT ESTIMATE                                
*                                                                               
BBLX     NI    DMINBTS,255-X'08'   TURN OFF READ DELETED RECORDS                
         J     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO RE-TRANSFER TO BUY PROGRAM                                         
*=====================================================================          
         SPACE 1                                                                
RESFR    NTR1  ,                                                                
         L     R2,AENTRY           SAVE A(BUYLINE ENTRY)                        
         USING BLND,R2                                                          
         MVC   NEWLINE,BLNLINE     SAVE BUYLINE TO USE                          
*                                                                               
         MVC   AIO,AIO2                                                         
         BRAS  RE,BLDBKEY                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUYKEY),KEYSAVE                                            
         BE    RESF010                                                          
* INSTEAD OF BLOWING UP, JUST DO IT AS AN ADD                                   
         MVI   LBADDCHA,LBADD      SET ADDING                                   
         BRAS  RE,BLDFACT          BUILD TRACE ELEMENT                          
         BRAS  RE,BLDWKS           BUILD WEEKS TABLE                            
         BRAS  RE,ADDBUY           BUILD BUYLINES                               
         JE    YES                                                              
         J     NO                                                               
*                                                                               
RESF010  GOTO1 GETREC                                                           
*                                                                               
         MVI   LBADDCHA,LBCHA      BUILD WEEKS TABLE                            
         BRAS  RE,BLDWKS             AND SET CERTAIN BITS IN LFLAG              
*                                                                               
         L     R2,AIO           *** PAID SPOTS - APPLY RULES                    
         USING BUYRECD,R2                                                       
         MVC   SVREP,BDREP         SAVE SPECIAL REP FROM BUY                    
*                                                                               
         TM    LFLAG,LPAIDSPT      WE HAVE A PAID SPOT?                         
         BZ    RESF030             NO                                           
********                                                                        
* WE HAVE A PAID SPOT IN THIS BUYLINE, CHECK IF TAX RATE CHANGING               
********                                                                        
         CLC   BDNTAX,BTAXRATE     TAX RATE CHANGE?                             
         BE    *+12                                                             
         MVI   BYTE,SC$ER50        PAID SPOTS, CANNOT CHANGE TAX RATE           
         J     NO                                                               
********                                                                        
* WE HAVE A PAID SPOT IN THIS BUYLINE, CHECK IF COST INDS CHANGING              
********                                                                        
         MVI   HALF,X'20'                                                       
         MVI   HALF+1,0                                                         
         MVC   BYTE,HDRRATE                                                     
         CLI   BYTE,C'0'           HEADER RATE TYPE OVERRIDE?                   
         BH    RESF015                                                          
         CLI   ESTRATE,C'*'        ESTIMATE SAYS NO RATE TYPE?                  
         BE    RESF025                                                          
         MVC   BYTE,ESTRATE                                                     
         CLI   ESTRATE,0                                                        
         BNE   *+10                                                             
         MVC   BYTE,CPROFILE+14                                                 
*                                                                               
RESF015  LR    RE,RB                                                            
         AHI   RE,(RATETYPS-TA0C26)                                             
RESF020  CLI   0(RE),0             TEST FOR EOT                                 
         BE    RESF025                                                          
         CLC   BYTE,0(RE)          MATCH ON RATE TYPE                           
         BE    RESF022                                                          
         LA    RE,3(RE)                                                         
         B     RESF020                                                          
*                                                                               
RESF022  MVC   HALF,1(RE)          COST IND FROM TABLE                          
*                                                                               
RESF025  CLC   BDCIND,HALF         COST IND CHANGE?                             
         BE    RESF029                                                          
RESF027  MVI   BYTE,SC$ER51        PAID SPOTS, CANNOT CHANGE COST INDS          
         J     NO                                                               
RESF029  CLC   BDCIND2,HALF+1      COST IND2 CHANGE?                            
         BNE   RESF027                                                          
*                                                                               
RESF030  TM    LFLAG,LAFFDVT       MATCHED?                                     
         BZ    RESF040             NO                                           
         CLC   BDSEC,SCHTOTLN      SPOT LENGTH CHANGE?                          
         BE    RESF040                                                          
         MVI   BYTE,SC$SECS        NOT ALLOWED                                  
         J     NO                                                               
*                                                                               
*ESF03   TM    SCHINDS,SCHIORB     TEST ORBIT                                   
*        BO    RESF070             YES - DAYS=0 ANYHOW                          
RESF040  MVC   BYTE,SCHDAY                                                      
         CLC   BYTE,BDDAY                                                       
         BE    RESF070                                                          
         OI    LCHGIND,LDAYS       DAYS CHANGE                                  
         GOTO1 DAYUNPK,DMCB,BYTE,(7,WORK)                                       
         GOTO1 DAYUNPK,(R1),BDDAY,(7,WORK+7)                                    
         LA    RE,WORK                                                          
         LA    R0,7                                                             
*                                                                               
RESF050  TM    LFLAG,LFREEZE       ANY FROZEN WEEKS?                            
         BZ    RESF060             NO                                           
         CLC   0(1,RE),7(RE)       START DAY CHANGE                             
         BE    RESF060                                                          
         MVI   BYTE,SC$ODTE                                                     
         J     NO                                                               
*                                                                               
RESF060  CLI   0(RE),C'.'                                                       
         BNE   RESF070                                                          
         LA    RE,1(RE)                                                         
         BCT   R0,RESF050                                                       
*                                                                               
RESF070  TM    LFLAG,LAFFDVT       MATCHED?                                     
         BZ    RESF080             NO                                           
         CLC   BDTIMST(4),SCHSTIME                                              
         BE    RESF080                                                          
         OI    LCHGIND,LTIMES      TIMES CHANGE                                 
         CLC   SCHSTIME,BDTIMST                                                 
         BH    *+14                                                             
         CLC   SCHNTIME,BDTIMEND                                                
         BNL   RESF080                                                          
         MVI   BYTE,SC$MTCH        NOT ALLOWED UNLESS EXPANSION                 
         J     NO                                                               
*                                                                               
RESF080  TM    LFLAG,LFREEZE       FROZEN WEEK?                                 
         BZ    RESF090             NO                                           
         CLC   BDCOST,SCHCOST      TEST CHANGE AGAINST SCHED OBJECT             
         BE    *+12                                                             
         MVI   BYTE,SC$COST        COST CHANGE NOT ALLOWED                      
         J     NO                                                               
*                                                                               
RESF090  TM    LFLAG,LFREEZE+LAFFDVT ANY FROZEN WEEKS OR AFFIDS ?               
         BNZ   RESF100             YES                                          
         GOTO1 GETELEM,DMCB,5      PART OF A PACKAGE?                           
         BE    RESF100             YES - UPDATE PACKAGE AS A WHOLE              
         GOTO1 GETELEM,DMCB,102    COMMENTS?                                    
         BE    RESF100             YES                                          
*                                                                               
         MVI   LBADDCHA,LBADD      BUILD WEEKS TABLE AS IF FOR ADD              
         BRAS  RE,BLDWKS                                                        
         MVI   LBADDCHA,LBCHA                                                   
         BRAS  RE,ADDBUY           SAFE TO REBUILD RECORD AS IF NEW             
         JE    YES                                                              
         J     NO                                                               
*                                                                               
RESF100  BRAS  RE,UPDBUY           UPDATE BUY RECORD(S)                         
         JNE   NO                                                               
         J     YES                                                              
         EJECT                                                                  
*=====================================================================          
* ADD BUY TURNAROUND REQUEST                                                    
* NTRY: AIO2 = A(BUY RECORD)                                                    
*=====================================================================          
         SPACE 1                                                                
ADDREQ   NTR1  ,                                                                
         L     R2,AIO2                                                          
         USING BUYRECD,R2                                                       
         MVI   BYTE,0                                                           
         LA    R6,LSTALIST         DO FOR EVERY STATION                         
         LA    R3,MAXSTA                                                        
*                                                                               
ADDR1    OC    0(L'LSTALIST,R6),0(R6)  TEST NO MORE STATIONS                    
         BZ    ADDRX                                                            
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+14,106                                                   
         LA    R4,ELEMENT+26                                                    
         MVC   0(80,R4),SPACES                                                  
         MVC   0(2,R4),=C'61'      NON-POL GET 61'S                             
*                                                                               
         LA    R5,BLOCK                                                         
         XC    BLOCK(255),BLOCK                                                 
         USING STAPACKD,R5                                                      
*                                                                               
         MVI   STAPACT,C'U'        UNPACK THE MARKET/STATION                    
         MVC   STAPAGY,SIGNON2C                                                 
         MVI   STAPMED,C'R'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,0(R6)                                                   
         GOTO1 STAPACK,STAPACKD                                                 
*                                                                               
         CLI   KEYPRD,FF           TEST POL                                     
         BNE   *+12                                                             
         CLI   CPROFILE,C'2'       YES - TEST BRD POL 61'S                      
         BNE   ADDR2                                                            
*                                                                               
         CLI   CPROFILE+2,C'1'     TEST BRD POL 61'S BY STA                     
         BNE   ADDR4                                                            
         MVC   18(5,R4),STAPQSTA                                                
         MVI   BYTE,1                                                           
         B     ADDR4                                                            
*                                                                               
ADDR2    MVC   0(2,R4),=C'81'      PROF+0 = 0 OR 1 SO GET 81'S                  
         CLI   CPROFILE+2,C'2'     TEST U3 BY MKT                               
         BNE   *+14                                                             
         MVC   0(2,R4),=C'U3'                                                   
         B     ADDR4                                                            
*                                                                               
         CLI   CPROFILE+2,C'3'     TEST U3 BY STATION                           
         BNE   *+10                                                             
         MVC   0(2,R4),=C'U3'                                                   
         MVC   18(5,R4),STAPQSTA    AND ALL 81'S ARE BY STATION                 
         MVI   BYTE,1                                                           
*                                                                               
ADDR4    CLC   0(2,R4),=C'U3'                                                   
         BE    ADDR6                                                            
         PACK  DUB,0(2,R4)                                                      
         CVB   R0,DUB                                                           
         STC   R0,ELEMENT+10                                                    
*                                                                               
ADDR6    MVC   2(2,R4),SIGNON2C                                                 
         MVI   4(R4),C'R'                                                       
         MVC   5(3,R4),QCLT                                                     
         MVC   8(2,R4),=C'NN'      NO MKT/PRD GRPS                              
         MVC   11(3,R4),HDRPRD                                                  
         CLI   KEYPRD,FF           TEST POL                                     
         BNE   ADDR8               NO                                           
         CLI   CPROFILE,C'0'       TEST BRD POL                                 
         BE    ADDR8               NO                                           
         MVC   11(3,R4),HDRPR1     YES-SET MASTER PRODUCT ALLOCATION            
*                                                                               
ADDR8    MVC   14(4,R4),STAPQMKT   MARKET                                       
*                                                                               
         ZIC   R0,BEST             EST                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R4),DUB                                                     
*                                                                               
         MVC   37(12,R4),ESTSTRT   EST START/END DATES                          
*                                                                               
         MVI   59(R4),C'B'         T/A IND                                      
         CLI   ELEMENT+10,81                                                    
         BNE   *+8                                                              
         MVI   59(R4),C'A'         SET TO GEN U4                                
         CLI   ELEMENT+10,61                                                    
         BNE   ADDR10                                                           
         MVC   61(5,R4),=C'30000'                                               
         MVI   ELEMENT+10,0                                                     
         MVC   ELEMENT+26(2),=C'U3'                                             
         MVC   ELEMENT+26+49(19),SPACES CLEAR ALL OPTIONS                       
*                                                                               
ADDR10   MVC   68(12,R4),SPACES                                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',ELEMENT,ELEMENT               
         CLI   8(R1),0                                                          
         BNE   ADDR90              DATAMGR ERROR                                
*                                                                               
         CLC   SVRFPGRP,SPACES                                                  
         BNH   ADDR12                                                           
         CLC   =C'G7',SIGNON2C                                                  
         BNE   *+12                                                             
         TM    SVEFLAG1,EF1REQ                                                  
         BZ    ADDR12                                                           
* ADD RFP GROUP REQUEST                                                         
         MVC   ELEMENT+128(80),ELEMENT      SAVE ORIGINAL REQUEST               
         MVC   0(2,R4),=C'RF'                                                   
         MVC   49(8,R4),SVRFPGRP                                                
         GOTO1 (RF),(R1)                                                        
         MVC   ELEMENT+26(80),ELEMENT+128   RESTORE ORIGINAL REQUEST            
*                                                                               
ADDR12   CLI   BDTIME,0            TEST PIGGYBACK                               
         BE    ADDR14              NO                                           
         MVC   11(3,R4),HDRPPB     PASSIVE PRD                                  
         ZIC   R0,BEST             PASSIVE EST                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R4),DUB                                                     
         GOTO1 DATAMGR,DMCB                                                     
         CLI   8(R1),0                                                          
         BNE   ADDR90              DATAMGR ERROR                                
         B     ADDR20                                                           
*                                                                               
ADDR14   CLI   BINPBPRD,0          CHECK FOR P/B MASPRD                         
         BE    ADDR20              NO                                           
*                                                                               
ADDR16   MVC   11(3,R4),HDRPPB     ADD REQUEST FOR SECOND BRAND                 
         GOTO1 DATAMGR,DMCB                                                     
         CLI   8(R1),0                                                          
         BNE   ADDR90                                                           
*                                                                               
ADDR20   CLI   BYTE,1              TEST BY STATION                              
         BNE   ADDRX                                                            
         LA    R6,L'LSTALIST(R6)   YES-NEXT STATION                             
         BCT   R3,ADDR1                                                         
         B     ADDRX                                                            
*                                                                               
ADDR90   DS    0H                  DATAMGR ERROR                                
*                                                                               
ADDRX    J     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
*=====================================================================          
* LITERALS AND EQUATED VALUES                                                   
*=====================================================================          
         SPACE 1                                                                
MAXWEEKS EQU   54                  MAXIMUM NUMBER OF WEEKS                      
MAXBKEYS EQU   52                  MAXIMUM BUY KEYS PER HEADER OBJECT           
MAXBLINE EQU   10                  MAXIMUM NUMBER OF BUYLINES/SCHEDULE          
MAXDEMOS EQU   14                                                               
MAXSTA   EQU   40                  MAXIMUM STATION TABLE ENTRIES                
MAXERRS  EQU   100                 MAXIMUM SLINE ERRORS PER HEADER              
MAXCOM   EQU   74                  MAXIMUM BUY PROGRAM COMMENT LEN              
MAXBLNS  EQU   255                 MAXIMUM NUMBRT OF BUYLINES ON BUY            
LENBKEY  EQU   L'BUYKAM+L'BUYKCLT+L'BUYKPRD+L'BUYMSTA+L'BUYKEST                 
LENBLNTB EQU   MAXBLNS*BLNLNQ      SPARE MEMORY FOR BUYLINE TABLE               
LIOS     EQU   2048                LENGTH OF IO AREA AS IN CONTROLLER           
FF       EQU   X'FF'                                                            
SPLITTER EQU   C'|'                                                             
         EJECT                                                                  
*                                                                               
SC$ODTE  EQU   1                   INVALID ORIGINAL TRANSFER DATE               
SC$OTIM  EQU   2                   INVALID ORIGINAL TRANSFER TIME               
SC$TIME  EQU   3                   INVALID START/END TIMES                      
SC$SPLN  EQU   4                   INVALID SPOT LENGTH                          
SC$COST  EQU   5                   INVALID COST                                 
SC$SPOT  EQU   6                   INVALID SPOTS                                
SC$DEMS  EQU   7                   INVALID DEMOS                                
SC$DATM  EQU   8                   INVALID DAY/TIME BLOCK                       
SC$DPT   EQU   9                   INVALID DAYPART                              
SC$STA   EQU   10                  INVALID STATION                              
SC$TRAN  EQU   11                  INVALID TRANSFERRED SPOTS ARRAY              
SC$BIG   EQU   12                  RECORD TOO BIG                               
SC$OVER  EQU   13                  INVALID DEMO OVERRIDE BIT FIELD              
SC$NUMS  EQU   14                  MORE THAN 31 SPOTS IN A WEEK                 
SC$STBK  EQU   15                  ONE OF THE BOOKS DOESN'T EXIST FOR           
SC$OTIME EQU   16                  INVALID OVERRIDE FOR SPILL                   
SC$SECS  EQU   17                  TRYING TO CHANGE NUMBER OF SECONDS           
SC$MTCH  EQU   18                  WEEK HAS MATCHED SPOTS                       
SC$EDAT  EQU   19                  BUY END DATE PAST ESTIMATE END DATE          
SC$BSPT  EQU   30                  BAD SPOT DATE                                
***********************************                                             
* RESERVED FOR FUTURE ERRORS SO STRATA DOES NOT HAVE TO REBUILD VFS             
* VFS WILL RETURN WITH THE ERROR # AND HAVE USERS CALL DDS TO REPORT            
***********************************                                             
SC$ER50  EQU   50                  PAID SPOTS, CANNOT CHANGE TAX RATE           
SC$ER51  EQU   51                  PAID SPOTS, CANNOT CHANGE COST INDS          
SC$ER52  EQU   52                                                               
SC$ER53  EQU   53                                                               
SC$ER54  EQU   54                                                               
SC$ER55  EQU   55                                                               
SC$ER56  EQU   56                                                               
SC$ER57  EQU   57                                                               
SC$ER58  EQU   58                                                               
SC$ER59  EQU   59                                                               
SC$ER60  EQU   60                                                               
SC$ER61  EQU   61                                                               
SC$ER62  EQU   62                                                               
SC$ER63  EQU   63                                                               
SC$ER64  EQU   64                                                               
SC$ER65  EQU   65                                                               
SC$ER66  EQU   66                                                               
SC$ER67  EQU   67                                                               
SC$ER68  EQU   68                                                               
SC$ER69  EQU   69                                                               
SC$ER70  EQU   70                                                               
         EJECT                                                                  
*=====================================================================          
* LITERALS                                                                      
*=====================================================================          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* ERRORS AND EXITS USED BY OVERLAY                                              
*=====================================================================          
         SPACE 1                                                                
INVLHDR  MVI   BYTE,1              NEED A HEADER BEFORE ANYTHING                
         J     ABORT                                                            
INVLWKS  MVI   BYTE,2              INVALID NUMBER OF WEEKS                      
         J     ABORT                                                            
INVLSSEQ MVI   BYTE,3              INVALID SCHEDULE SEQUENCE NUMBER             
         J     ABORT                                                            
INVLSTDT MVI   BYTE,4              START DATE NOT WITHIN ESTIMATE RANGE         
         J     ABORT                                                            
INVLOBJ  MVI   BYTE,5              INVALID OBJECT                               
         J     ABORT                                                            
INVLNSCH MVI   BYTE,6              CAN'T BUILD WITHOUT SCHEDULES                
         J     ABORT                                                            
INVLDTYP MVI   BYTE,7              NEED AT LEAST ONE VALID DEMO TYPE            
         J     ABORT                                                            
INVLFULL MVI   BYTE,8              NOT ENOUGH AVAILABLE BUYLINES                
         MVI   UNWIND,C'Y'         SET FLAG TO UNWIND TRANSACTIONS              
         J     ABORT                                                            
INVLBOOK MVI   BYTE,9              INVALID BOOK                                 
         J     ABORT                                                            
INVLRSRV MVI   BYTE,10             INVALID RATING SERVICE                       
         J     ABORT                                                            
INVLENDT MVI   BYTE,11             SCHED END PAST ESTIMATE END                  
         J     ABORT                                                            
INVLMED  MVI   BYTE,12             INVALID MEDIA CODE                           
         J     ABORT                                                            
INVLERRT MVI   BYTE,13             TOO MANY SLINE ERRORS                        
         J     ABORT                                                            
INVLCLT  MVI   BYTE,14             INVALID CLIENT CODE                          
         J     ABORT                                                            
INVLPRD  MVI   BYTE,15             INVALID PRODUCT CODE                         
         J     ABORT                                                            
INVLEST  MVI   BYTE,16             INVALID ESTIMATE CODE                        
         J     ABORT                                                            
INVLPOL  MVI   BYTE,17             PRODUCT POL ONLY FOR TRUE POOL               
         J     ABORT                                                            
INVLPIG  MVI   BYTE,18             POL IS NOT A VALID PIGGYBACK PRODUCT         
         J     ABORT                                                            
INVLRATE MVI   BYTE,19             INVALID RATE TYPE                            
         J     ABORT                                                            
INVLREP  MVI   BYTE,20             INVALID SPECIAL REP                          
         J     ABORT                                                            
INVLMKT  MVI   BYTE,21             INVALID MARKET                               
         J     ABORT                                                            
INVLPW   MVI   BYTE,22             PW RECORD LOCKED                             
         J     ABORT                                                            
INVLDAYS MVI   BYTE,23             ANOTHER STELLAR PERFORMANCE BY               
         J     ABORT               THOSE WONDERFUL FOLKS AT STRATA              
INVLELK  MVI   BYTE,24             ESTIMATE LOCKED,CAN'T UPLOAD BUY             
         J     ABORT                                                            
INVLCLK  MVI   BYTE,25             CLIENT LOCKED,CAN'T UPLOAD BUY               
         J     ABORT                                                            
INVLPURP MVI   BYTE,26             INVALID PURPOSE CODE                         
         J     ABORT                                                            
NOCLTACC MVI   BYTE,27             NO ACCESS TO CLIENT                          
         J     ABORT                                                            
NOMKTACC MVI   BYTE,28             NO ACCESS TO MKT                             
         J     ABORT                                                            
INVLBYR  MVI   BYTE,29             INVALID BUYER                                
         J     ABORT                                                            
INVLVFS  MVI   BYTE,30             VIEW FOR SPOTPAK DEACTIVATED                 
         J     ABORT                                                            
*                                                                               
ABORT    GOTO1 HEXOUT,DMCB,BYTE,HALF,1,0,0,0                                    
         GOTO1 PUTITEM,(R1),ITUPLABT,2,HALF                                     
         JNE   EXIT                                                             
         GOTO1 (RF),(R1),ITEOD,0                                                
         JNE   EXIT                                                             
         MVI   MDLAST,C'Y'                                                      
         J     EXIT                                                             
*                                                                               
EXIT     GOTO1 WRKCLOS             CLOSE WORKER FILE                            
         CLI   SETSENT,C'Y'        IF SET SENT FLAG IS TRUE                     
         JNE   EXIT2                                                            
         GOTO1 WRKSENT             SET WORKER FILE TO SENT                      
*                                                                               
EXIT2    L     RD,SAVERD           RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1  ,                                                                
*                                                                               
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO SET LNEWTOT FOR CONFIRMATION OBJECT                                
*=====================================================================          
         SPACE 1                                                                
SETLNEW  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,SWEEKS                                                        
         USING SWEEKSD,R4                                                       
         LA    R3,LNEWTOT                                                       
         LHI   R0,MAXWEEKS                                                      
*                                                                               
SETL02   CLI   SWBSTDT,FF          END OF LIST?                                 
         BE    SETLNEWX            YES                                          
         MVC   0(1,R3),SWNSPT      SET NEW SPOTS FOR THIS WEEK                  
         TM    SWIND1,SWI1FRZ      UNLESS FROZEN                                
         BZ    *+10                                                             
         MVC   0(1,R3),SWOSPT      THEN WEEK IS UNCHANGED                       
         AHI   R4,SWEEKSLQ         NEXT WEEK                                    
         LA    R3,1(R3)                                                         
         BCT   R0,SETL02           DO FOR ALL WEEKS                             
*                                                                               
SETLNEWX J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*=====================================================================          
* GET HEADER OBJECT FROM INPUT FRAME                                            
*=====================================================================          
         SPACE 1                                                                
GETHDR   NTR1  BASE=*,LABEL=*                                                   
         XC    HDROBJCT(HDRLEN),HDROBJCT                                        
         XC    NUMBKEYS,NUMBKEYS                                                
         MVI   NUMSCHED,0                                                       
         MVI   NUMERRS,0           ZERO SLINE ERROR COUNT                       
         MVI   MDLAST,C'N'                                                      
         MVI   BITFLAG,0                                                        
         MVI   ENDSCHED,C'N'       NOT END OF SCHEDULES                         
         MVI   CONFLAG,C'N'                                                     
         XC    SVSPLMKT,SVSPLMKT                                                
         XC    SVPURP,SVPURP                                                    
*                                                                               
         LA    R0,ERRTAB           CLEAR SLINE ERROR TABLE                      
         LA    R1,MAXERRS*L'ERRTAB                                              
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,GETITEML         GET AN OBJECT                                
         CLC   TYPENUM,=A(ITUPLBUY)                                             
         JNE   INVLHDR             ERROR IF NOT A HEADER OBJECT                 
*                                                                               
         L     R2,ADATA                                                         
UPL      USING HDROBJCT,R2         POPULATE HEADER OBJECT                       
         MVC   HDRMED,UPL.HDRMED                                                
*                                                                               
         MVI   CABLEFLG,C'N'                                                    
         CLI   HDRMED,C'C'         TEST CABLE UPLOAD                            
         BNE   *+12                                                             
         MVI   HDRMED,C'T'                                                      
         MVI   CABLEFLG,C'Y'                                                    
*                                                                               
         MVC   HDRCLT,UPL.HDRCLT                                                
         MVC   HDRPR1,UPL.HDRPR1                                                
         MVC   HDRPPB,UPL.HDRPPB                                                
         MVC   HDREST,UPL.HDREST                                                
         MVC   HDRSRV,UPL.HDRSRV                                                
         MVC   HDRBOOKS,UPL.HDRBOOKS                                            
         MVC   HDRSDATE,UPL.HDRSDATE                                            
         MVC   HDREDATE,UPL.HDREDATE                                            
         MVC   HDRWKS,UPL.HDRWKS                                                
         MVC   HDRGENTA,UPL.HDRGENTA                                            
         MVC   HDRRETR,UPL.HDRRETR                                              
         MVC   HDRPREND,UPL.HDRPREND                                            
         MVC   HDRREPRV,UPL.HDRREPRV                                            
         MVC   HDRCOST,UPL.HDRCOST                                              
         MVC   HDRDEMO,UPL.HDRDEMO                                              
*                                                                               
         LA    RE,UPL.HDRDTYPS     LOOP UNTIL END OF DEMOS                      
         LR    R1,RE                                                            
*                                                                               
GHEAD02  CLC   =C'FF',0(RE)                                                     
         BE    GHEAD04                                                          
         AHI   RE,6                                                             
         B     GHEAD02                                                          
*                                                                               
GHEAD04  LA    RE,2(RE)                                                         
         SR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   HDRDTYPS(0),UPL.HDRDTYPS                                         
*                                                                               
         LA    R2,1(RE,R1)                                                      
UPL      USING HDRDAILY,R2                                                      
         MVC   HDRDAILY,UPL.HDRDAILY                                            
         MVC   HDRRATE,UPL.HDRRATE                                              
         MVC   HDRREP,UPL.HDRREP                                                
         MVC   HDRBUYER,UPL.HDRBUYER                                            
         DROP  UPL                                                              
*                                                                               
         BRAS  RE,VALHDR           VALIDATE THE HEADER OBJECT                   
         BRAS  RE,GETSDTE          GET SCHEDULE DATES                           
         J     XIT                                                              
         EJECT                                                                  
*=====================================================================          
* MAKE SURE THE HEADER OBJECT IS VALID                                          
*=====================================================================          
         SPACE 1                                                                
VALHDR   NTR1  ,                                                                
         GOTO1 VALIMED,DMCB,HDRMED VALIDATE THE MEDIA (PAN=CTMADUS)             
         JNE   INVLMED                                                          
*                                                                               
         L     RF,AIO              EXTRACT AGYFLAG1                             
         MVC   AFLAG1,AGYFLAG1-AGYHDRD(RF)                                      
         MVC   SVAPROF,AGYPROF-AGYHDRD(RF)                                      
*                                                                               
         GOTO1 VALICLT,DMCB,HDRCLT VALIDATE THE CLIENT                          
         JNE   INVLCLT                                                          
*                                                                               
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         MVC   SVOFFC,COFFICE        SAVE OFFICE CODE                           
         MVC   SVCACCS,CACCESS                                                  
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0BW'                                                 
         MVC   WORK+4(2),SIGNON2C                                               
         MVC   WORK+6(1),HDRMED                                                 
         MVC   WORK+7(3),HDRCLT                                                 
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         MVC   WORK+16(16),WORK       SAVE PROFILE CALL ARGS                    
*                                                                               
         XC    SVBWPROF,SVBWPROF                                                
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SVBWPROF,DATAMGR                                  
*                                                                               
         XC    SV1WPROF,SV1WPROF                                                
         MVC   WORK(16),WORK+16    RESTORE                                      
         MVC   WORK(4),=C'S01W'                                                 
         GOTO1 (RF),DMCB,WORK,SV1WPROF                                          
*                                                                               
         XC    SVB0PROF,SVB0PROF                                                
         MVC   WORK(16),WORK+16    RESTORE                                      
         MVC   WORK(4),=C'S0B0'                                                 
         GOTO1 (RF),DMCB,WORK,SVB0PROF                                          
*                                                                               
         XC    SVDARPRF,SVDARPRF                                                
         MVC   WORK(16),WORK+16    RESTORE                                      
         MVC   WORK(4),=C'SDAR'                                                 
         NI    WORK,X'BF'          MAKE S LOWERCASE                             
         GOTO1 (RF),DMCB,WORK,SVDARPRF                                          
*                                                                               
         XC    SVMKPROF,SVMKPROF                                                
         MVC   WORK(16),WORK+16    RESTORE                                      
         MVC   WORK(4),=C'S0MK'                                                 
         GOTO1 (RF),DMCB,WORK,SVMKPROF                                          
*                                                                               
         OC    CLOCKYM,CLOCKYM     IS THERE A CLIENT LOCK?                      
         BZ    VHD02                                                            
         MVC   SVCLOCK,CLOCKYM     CLIENT LOCK YEAR MONTH.                      
         LA    RE,SVCLOCK                                                       
         ST    RE,SVLKADD                                                       
         BRAS  RE,SETLKDT          GET THE DATE RANGE                           
*                                                                               
VHD02    MVC   SVRFPGRP,CRFPGRP                                                 
         DROP  R6                                                               
*                                                                               
         MVI   BINPBPRD,0          CLEAR PIGGYBACK CODE FIRST                   
         OC    HDRPR1,SPACES                                                    
         OC    HDRPPB,SPACES                                                    
*                                                                               
         CLC   HDRPPB,SPACES       IF NO PRODUCT SPECIFIED                      
         BE    VHD04                                                            
*                                                                               
         GOTO1 VALIPRD,DMCB,HDRPPB VALIDATE THE PIGGYBACK PRODUCT               
         JNE   INVLPRD                                                          
*                                                                               
         MVC   BINPBPRD,BPRD       SAVE PIGGY BACK BINARY CODE                  
         CLC   =C'POL',HDRPPB      POL CANNOT BE USED IN PIGGBACK               
         JE    INVLPIG                                                          
         CLC   =C'POL',HDRPR1                                                   
         JE    INVLPIG                                                          
         CLC   HDRPR1,HDRPPB       PIGGYBACK PRODUCTS MUST BE DIFFERENT         
         JE    INVLPRD                                                          
*                                                                               
VHD04    GOTO1 VALIPRD,DMCB,HDRPR1 VALIDATE THE PRODUCT                         
         JNE   INVLPRD                                                          
         MVC   KEYPRD,BPRD         PRD IN KEY WILL BE SAME IF NOT POOL          
*                                                                               
VHD06    GOTO1 VALIEST,DMCB,HDREST VALIDATE EST FOR PROD IN HEADER              
         JNE   INVLEST                                                          
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         TM    ECNTRL,X'0C'        OLD LOCKING MECHANISM?                       
         JNZ   INVLELK                                                          
         MVC   SVESTBTY,EBKTYPE                                                 
         MVC   SVELOCK,ELOCKYM     ELSE SAVE LOCK DATE (IF ANY)                 
         MVC   SVEFLAG1,EFLAG1                                                  
         MVC   SVECOST2,ESTCOST2   SAVE VALUE FROM BRAND EST                    
         MVC   SVBRDREP,EREP       SAVE THE BRD REP                             
         MVC   SVBRDRTY,ERATE      SAVE THE BRD RATE TYPE                       
*                                                                               
         MVC   QPRD,=C'POL'        IF POL ESTIMATE DOESN'T EXIST                
         GOTO1 VALIEST,DMCB,HDREST                   (PAN=CTMADUS)              
         BE    VHD08                                                            
         CLC   APPLERR,=Y(ERA1EST) THEN IF THE RECORD ISN'T THERE               
         BE    VHD14                    THEN CHECK FOR BRAND ESTIMATE           
         J     INVLEST                  ELSE SOME OTHER ERROR, EXIT             
*                                                                               
VHD08    OI    BITFLAG,BITFPOL     ELSE FLAG THAT POL ESTIMATE EXISTS           
*                                                                               
         OC    SVBRDREP,SVBRDREP   HAVE BRD REP?                                
         BZ    *+10                NO                                           
         MVC   ESTREPCD,SVBRDREP   USE BRD REP FOR BRD POL BR BRD               
*                                                                               
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         OC    ELOCKYM,ELOCKYM     TEST LOCK DATE IN POL ESTHDR                 
         BZ    *+10                                                             
         MVC   SVELOCK,ELOCKYM                                                  
*                                                                               
         OC    SVELOCK,SVELOCK     TEST BRD OR POL LOCKED                       
         BZ    VHD10               NO                                           
         LA    RE,SVELOCK                                                       
         ST    RE,SVLKADD                                                       
         BRAS  RE,SETLKDT             GET THE DATE RANGE                        
*                                                                               
VHD10    CLI   SVBRDRTY,C'0'       RATE TYPE ALREADY SET FROM BRD EST?          
         BH    *+10                                                             
         MVC   SVBRDRTY,ERATE      NO, GET IT FROM THE POL EST THEN             
*                                                                               
         OC    SVEFLAG1,EFLAG1     'OR' POL VALUE TO BRAND                      
         DROP  R6                                                               
*                                                                               
         MVI   KEYPRD,FF           POL PRODUCT CODE TO BE IN THE KEY            
         CLI   CPROFILE,C'0'       TEST TRUE POOL                               
         BE    VHD12               YES                                          
         CLI   APROFILE+11,C'Y'    NO-TEST BRAND POOL NPW                       
         BNE   VHD12                                                            
         OI    BITFLAG,BITFNPW     YES-SET INDICATOR                            
*                                                                               
VHD12    CLI   BPRD,FF             TEST FOR PRODUCT=POL                         
         BNE   VHD16               NO                                           
         CLI   CPROFILE,C'0'       TEST TRUE POOL                               
         BE    VHD16               YES-OK TO USE POL(UNALLOCATED)               
         J     INVLPOL             NO-CANNOT USE IT FOR BRAND POOL              
*                                                                               
VHD14    MVC   QPRD,HDRPRD         IF BRAND ESTIMATE DOESN'T EXISTS             
         GOTO1 VALIEST,DMCB,HDREST                                              
         JNE   INVLEST             THEN EXIT                                    
         XC    APPLERR,APPLERR     RESET IT--NOTE BRAND BUYING                  
*                                                                               
VHD16    L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         TM    ECNTRL,X'0C'        OLD LOCKING MECHANISM?                       
         JNZ   INVLELK                                                          
         OC    ELOCKYM,ELOCKYM                                                  
         BZ    VHD18                                                            
         MVC   SVELOCK,ELOCKYM                                                  
         LA    RE,SVELOCK                                                       
         ST    RE,SVLKADD                                                       
         BRAS  RE,SETLKDT          GET THE DATE RANGE                           
*                                                                               
VHD18    MVC   ESTRATE,SVBRDRTY                                                 
         MVI   PWFLAG,0                                                         
         OC    EPWPCT,EPWPCT                                                    
         BZ    *+8                                                              
         OI    PWFLAG,X'01'        SET PW EST FLAG                              
         DROP  R6                                                               
*                                                                               
VHD20    CLI   HDRSRV,C'A'        TEST FOR VALID RATING SERVICES                
         BE    *+12                                                             
         CLI   HDRSRV,C'N'                                                      
         JNE   INVLRSRV                                                         
*                                                                               
         GOTO1 HEXIN,DMCB,HDRBOOKS,BOOKS,L'HDRBOOKS                             
         OC    12(4,R1),12(R1)                                                  
         JZ    INVLBOOK                                                         
*                                                                               
         LA    R4,BOOKS            CONVERT ANY AUG BOOKS TO JUL BOOKS           
         LA    R0,L'BOOKS/2                                                     
VHD22    OC    0(2,R4),0(R4)                                                    
         BZ    VHD24                                                            
         CLI   1(R4),8             DID THEY SEND US AN AUG BOOK?                
         BNE   *+8                                                              
         MVI   1(R4),7             YES, HARD CODE THAT TO A JUL BOOK            
* CONVERT BOOK TO Y2K FORMAT                                                    
         MVC   FULL(2),0(R4)                                                    
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),DUB    CONV TO YYMMDD                       
         GOTO1 (RF),(R1),DUB,(3,FULL)      AND THEN BACK TO 3 BYTE              
         MVC   0(2,R4),FULL                                                     
         LA    R4,2(R4)                                                         
         BCT   R0,VHD22                                                         
*                                                                               
* CONVERT ALL HEADER DATES TO Y2K FORMAT                                        
*                                                                               
VHD24    GOTO1 DATCON,DMCB,HDRSDATE,(3,DUB)                                     
         GOTO1 (RF),(R1),(3,DUB),HDRSDATE                                       
         GOTO1 (RF),(R1),HDREDATE,(3,DUB)                                       
         GOTO1 (RF),(R1),(3,DUB),HDREDATE                                       
         GOTO1 (RF),(R1),HDRPREND,(3,DUB)                                       
         GOTO1 (RF),(R1),(3,DUB),HDRPREND                                       
*                                                                               
         CLC   HDRSDATE,ESTSTRT    BUY START DATE < ESTIMATE START              
         JL    INVLSTDT                                                         
         CLC   HDRSDATE,ESTEND     BUY START DATE > ESTIMATE END                
         JH    INVLSTDT                                                         
         CLC   HDREDATE,ESTEND     BUY END DATE > ESTIMATE END                  
         JH    INVLENDT                                                         
         GOTO1 GETDAY,DMCB,HDRSDATE,WORK                                        
         MVC   BDAY1,0(R1)         SAVE BUY START DAY NUMBER                    
*                                                                               
         GOTO1 HEXIN,DMCB,HDRWKS,BNUMWKS,L'HDRWKS                               
         OC    12(4,R1),12(R1)                                                  
         JZ    INVLWKS             # OF WEEKS NOT VALID HEX                     
         CLI   BNUMWKS,0                                                        
         JE    INVLWKS             NO WEEKS                                     
         CLI   BNUMWKS,53                                                       
         JH    INVLWKS             TOO MANY WEEKS                               
*                                                                               
         XR    R2,R2               R2 = COUNT OF VALID DEMO TYPES               
         LA    R3,HDRDTYPS         R3 = A(DEMO TYPE LIST)                       
         LA    R4,DTYPLIST         R4 = A(BINARY LIST)                          
*                                                                               
VHD26    CLC   =C'FF',0(R3)        END OF LIST?                                 
         BE    VHD28               YES                                          
         GOTO1 HEXIN,DMCB,0(R3),0(R4),L'HDRDTYPS                                
         OC    12(4,R1),12(R1)                                                  
         JZ    INVLDTYP            ERROR - INVALID HEX                          
*                                                                               
         AHI   R3,L'HDRDTYPS                                                    
         AHI   R4,L'DTYPLIST                                                    
         AHI   R2,1                                                             
         B     VHD26                                                            
*                                                                               
VHD28    STC   R2,NUMDCATS         STORE # VALID DEMO CATEGORIES                
*                                                                               
         CLI   SVAPROF+14,C'Y'     TEST BUYER/BILLER SAVED                      
         BNE   VHD30                                                            
*                                                                               
         CLI   HDRBUYER,C' '       TEST BUYER NAME PRESENT                      
         BNH   VHD30                                                            
*                                                                               
         CLI   SVMKPROF+4,C'Y'     TEST TO VALIDATE BUYER NAME                  
         BNE   VHD30                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DE4'                                                  
         MVC   KEY+2(1),BAGYMED                                                 
         NI    KEY+2,X'F0'         DROP MEDIA                                   
*                                                                               
         LA    RE,HDRBUYER                                                      
         LHI   RF,L'HDRBUYER-1     SET FOR EX MOVE                              
         CLI   0(RE),C'='                                                       
         BNE   *+10                                                             
         LA    RE,1(RE)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),0(RE)                                                   
*                                                                               
         OC    KEY+3(10),SPACES                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   INVLBYR                                                          
         MVC   HDRBUYER,KEY+3      SAVE BUYER NAME OVER INPUT                   
*                                                                               
VHD30    CLI   HDRRATE,0           ADDITIONAL DATA?                             
         JE    XIT                 NO                                           
*                                                                               
         CLI   HDRRATE,C'0'                                                     
         JL    INVLRATE                                                         
         CLI   HDRRATE,C'8'                                                     
         JH    INVLRATE                                                         
*                                                                               
         GOTO1 HEXIN,DMCB,HDRREP,REP,L'HDRREP                                   
         OC    12(4,R1),12(R1)                                                  
         JZ    INVLREP                                                          
         OC    REP,REP             SPECIAL REP?                                 
         JZ    XIT                 NO                                           
         CLC   REP,=H'999'                                                      
         JH    INVLREP                                                          
*                                                                               
         MVI   KEY,C'0'            VALIDATE SPECIAL REP                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+5(2),SIGNON2C                                                
         XR    R0,R0                                                            
         ICM   R0,3,REP                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(3),DUB                                                     
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
         L     RE,AIO                                                           
         CLC   KEY(15),0(RE)       TEST IF REP FOUND                            
         JE    XIT                                                              
         J     INVLREP             BAD REP                                      
         EJECT                                                                  
*=====================================================================          
* BUILD A SCHEDULE BROADCAST TABLE (ALSO SET SOME OTHER DATE FIELDS)            
*=====================================================================          
         SPACE 1                                                                
GETSDTE  NTR1  BASE=*,LABEL=*                                                   
         MVC   DUB,HDREDATE        SAVE END DATE                                
         CLI   HDRRETR,C'Y'                                                     
         BNE   GDT02                                                            
         CLC   HDREDATE,HDRPREND   TEST IF END DATE > LAST TRANSFER END         
         BH    GDT02               YES                                          
         MVC   HDREDATE,HDRPREND   FUDGE THE LAST TRANSFER                      
*                                                                               
GDT02    CLI   HDRDAILY,C'Y'       TEST HDRDAILY SCHEDULING                     
         BE    GDT08                                                            
*                                                                               
         XC    WORK,WORK           ZERO DUMMY SYSPROF AREA                      
         MVC   WORK+8(1),ESTOWKSD  OUT OF WEEK START                            
         MVC   BLOCK(4),GETBROAD                                                
         MVC   BLOCK+4(4),ADDAY                                                 
         MVC   BLOCK+8(4),GETDAY                                                
         MVC   BLOCK+12(4),DATCON                                               
*                                                                               
         GOTO1 MOBILE,DMCB,('MAXWEEKS',HDRSDATE),(5,SCHDATS),BLOCK,WORK         
         MVC   HDREDATE,DUB        RESTORE END DATE IN CASE                     
*                                                                               
         GOTO1 DATCON,DMCB,(2,SCHDATS),(0,SCHSTMON)                             
         GOTO1 (RF),(R1),(0,SCHSTMON),(3,SCHSTMNB)                              
         GOTO1 (RF),(R1),(0,HDREDATE),(3,SCHENDB)                               
         MVC   SCHSTMNP,SCHDATS                                                 
         CLI   HDRRETR,C'Y'        TEST FOR RETRANSFER                          
         JNE   XIT                 NO                                           
*                                                                               
         CLI   ESTOWKSD,0          TEST FOR OUT-OF-WEEK ROTATOR EST             
         BNE   GDT06               YES                                          
*                                                                               
* COMPUTE NUMBER OF WEEKS IN SCHEDULE AS OF LAST TRANSFER                       
*                                                                               
         GOTO1 GETDAY,DMCB,HDRPREND,FULL                                        
         MVC   BYTE,0(R1)          GET DAY NUMBER OF FORMER LAST DAY            
*                                                                               
         MVC   WORK(6),SCHSTMON    WORK(6)=START WEEK'S MONDAY                  
         MVC   WORK+6(6),HDRPREND  WORK+6(6)=FORMER END WEEK'S SUNDAY           
         CLI   BYTE,7              TEST IF IT WAS SUNDAY                        
         BE    GDT04               YES                                          
*                                                                               
* FIND SUNDAY OF FORMER END DATE'S WEEK                                         
*                                                                               
         LA    R2,7                                                             
         XR    RE,RE                                                            
         IC    RE,BYTE                                                          
         SR    R2,RE               COMPUTE # OF DAYS TO ADD                     
         GOTO1 ADDAY,DMCB,HDRPREND,WORK+6,(R2)                                  
*                                                                               
GDT04    GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   BPRVWKS,DMCB+13                                                  
         J     XIT                                                              
*                                                                               
* FOR OUT-OF-WEEK ROTATOR, FIND NUMBER OF WEEKS BETWEEN START                   
* AND END DATE INCLUSIVE.                                                       
*                                                                               
GDT06    MVC   WORK(12),HDRSDATE   ACTUAL START/END DATE                        
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         LH    RE,12(R1)           GET N'DAYS/7                                 
         OC    10(2,R1),10(R1)     TEST FOR REMAINDER                           
         BZ    *+8                 NO                                           
         LA    RE,1(RE)            YES-INCREMENT N'WEEKS                        
         STC   RE,BPRVWKS                                                       
         J     XIT                                                              
*                                                                               
* FOR HDRDAILY SCHEDULING, ADD ONE DATE ENTRY FOR EACH DAY                      
*                                                                               
GDT08    MVC   WORK(12),HDRSDATE   START DATE/END DATE                          
         LA    R4,SCHDATS                                                       
*                                                                               
GDT10    CLC   WORK(6),WORK+6      TEST PAST SCHEDULE END                       
         BH    GDT12               YES                                          
         GOTO1 DATCON,DMCB,WORK,(2,(R4))                                        
         MVC   2(2,R4),0(R4)       START DATE=END DATE                          
         GOTO1 ADDAY,DMCB,WORK,WORK+12,1                                        
         MVC   WORK(6),WORK+12     NEW DATE                                     
         LA    R4,4(R4)            NEXT DATE PAIR                               
         B     GDT10                                                            
*                                                                               
GDT12    MVI   0(R4),FF                                                         
         MVC   HDREDATE,DUB        RESTORE REAL END DATE                        
         GOTO1 DATCON,DMCB,HDREDATE,(3,SCHENDB)                                 
         CLI   HDRRETR,C'Y'        TEST FOR RETRANSFER                          
         JNE   XIT                                                              
*                                                                               
* FIND PREVIOUS NUMBER OF DAYS IN SCHEDULE                                      
*                                                                               
         GOTO1 PERVERT,DMCB,HDRSDATE,HDRPREND                                   
         MVC   BPRVWKS,DMCB+9      NUMBER OF DAYS INCLUSIVE                     
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*=====================================================================          
* GET OBJECTS AND PROCESS THEM ACCORDINGLY.                                     
*=====================================================================          
         SPACE 1                                                                
GETSCH   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
GSCH02   BRAS  RE,GETITEML         GET NEXT OBJECT                              
         CLI   EIFFLAG,C'Y'        IF END OF FRAME                              
         BNE   GSCH04                                                           
         TM    BITFLAG,BITFEOD     IF PREVIOUS OBJ WAS END-OF-DATA              
         BO    GSCH12              THEN WE'RE DONE                              
         B     GSCHX               ELSE WE NEED MORE OBJECTS                    
*                                                                               
GSCH04   TM    BITFLAG,BITFEOD     ERROR IF END-OF-DATA WAS PREV OBJECT         
         JO    INVLOBJ                                                          
*                                                                               
         ICM   RF,15,TYPENUM                                                    
         CHI   RF,ITEOD            END-OF-DATA?                                 
         BE    GSCH06                                                           
         CHI   RF,ITUPLBYS         SCHEDULE?                                    
         BE    GSCH08                                                           
         CHI   RF,ITSPLMKT         SPILL?                                       
         BE    GSCH10                                                           
         J     INVLOBJ                                                          
*                                                                               
GSCH06   OI    BITFLAG,BITFEOD     THEN SET BIT IN OBJECT FLAG                  
         TM    BITFLAG,BITF1SL     ERROR IF NO SCHEDULES READ                   
         JZ    INVLNSCH                                                         
         B     GSCH02              LOOP BACK, CAN'T HAVE ANY MORE DATA          
*                                                                               
GSCH08   OI    BITFLAG,BITF1SL     SET AT LEAST 1 SLINE PROCESSED               
         LA    RE,SCHOBJCT         CLEAR SCHEDULE OBJECT                        
         LA    RF,SCHLEN                                                        
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BRAS  RE,VALISCHD         VALIDATE & ADD SCHEDULE TO TEMPSTR           
         B     GSCH02                                                           
*                                                                               
GSCH10   LA    RE,SPLOBJCT         CLEAR SPILL OBJECT                           
         LA    RF,SPLLEN                                                        
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BRAS  RE,VALISPL          VALIDATE & ADD SPILL TO WORKER FILE          
         B     GSCH02              LOOP BACK TO GET NEXT OBJECT                 
*                                                                               
GSCH12   MVI   ENDSCHED,C'Y'       NO MORE SCHEDULES TO READ                    
         NI    BITFLAG,FF-(BITF1SL+BITFEOD)                                     
*                                                                               
GSCHX    J     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
*=====================================================================          
* VALIDATE SCHEDULE OBJECT AND ADD IT TO TEMPORARY WORKER FILE                  
*=====================================================================          
         SPACE 1                                                                
VALISCHD NTR1  ,                                                                
         L     R2,ADATA            R2 = A(SCHEDULE OBJECT)                      
         USING SLD,R2                                                           
         CLI   HDRRETR,C'Y'        TEST OVERALL RETRANSFER                      
         BNE   *+8                 NO                                           
         OI    SCHINDS,SCHIRET     YES-SET SLINE RETRANSFER FLAG                
*                                                                               
* STORE SCHEDULE LINE SEQUENCE NUMBER                                           
*                                                                               
         GOTO1 HEXIN,DMCB,SLSEQ,SCHLNNUM,L'SLSEQ                                
         OC    12(4,R1),12(R1)                                                  
         JZ    INVLSSEQ                                                         
*                                                                               
* VALIDATE ORIGINAL TRANSFER DATE/TIME - IF BOTH ARE ZERO, ITS                  
* AN ORIGINAL TRANSFER EVEN IF HEADER RETRANSFER FLAG IS YES                    
*                                                                               
         CLI   SLODATE,C'*'        TIME FIELD HAS *NNNNN WHERE NNNNN            
         BNE   VSCH01              IS OVERRIDE LINE NUMBER                      
         MVI   SCHODATE,C'*'                                                    
         GOTO1 HEXIN,DMCB,SLODATE+1,SCHODATE+1,8                                
         OC    12(4,R1),12(R1)                                                  
         JNZ   *+12                                                             
         MVI   SCHERROR,SC$ODTE    INVALID ORIGINAL TRANSFER DATE               
         B     VSCHSV                                                           
*                                                                               
         NI    SCHINDS,FF-SCHIRET  IF ZERO-TURN OFF RETRANSFER FLAG             
         B     VSCH06                                                           
*                                                                               
VSCH01   GOTO1 DECIN,DMCB,SLODATE,L'SCHODATE                                    
         JE    *+12                                                             
         MVI   SCHERROR,SC$ODTE    INVALID ORIGINAL TRANSFER DATE               
         B     VSCHSV                                                           
*                                                                               
         MVC   SCHODATE,SLODATE    ORIGINAL TRANSFER DATE                       
         OC    0(4,R1),0(R1)       TEST FOR ZEROES                              
         BNZ   *+10                                                             
         XC    SCHODATE,SCHODATE   YES-FORCE ORIG XFR ON SLINE                  
*                                                                               
VSCH02   GOTO1 HEXIN,DMCB,SLOTIME,SCHOTIME,L'SLOTIME                            
         OC    12(4,R1),12(R1)                                                  
         JNZ   *+12                                                             
         MVI   SCHERROR,SC$OTIM    INVALID ORIGINAL TRANSFER TIME               
         B     VSCHSV                                                           
*                                                                               
VSCH04   OC    SCHODATE(L'SCHODATE+L'SCHOTIME),SCHODATE                         
         BNZ   VSCH06                                                           
         NI    SCHINDS,FF-SCHIRET  IF ZERO-TURN OFF RETRANSFER FLAG             
*                                                                               
VSCH06   GOTO1 HEXIN,DMCB,SLDAYTIM,SCHDT,L'SLDAYTIM                             
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLDATM                                                         
*                                                                               
         BRAS  RE,GETDT            SET THE DAY AND START/END TIMES              
         TM    SCHDAY,X'7F'        THIS MUST NOT HAPPEN                         
         JZ    INVLDAYS            MUST HAVE AT LEAST ONE BIT ON                
*                                                                               
         MVC   HALF+1(1),SCHDAYNM                                               
         NI    HALF+1,X'0F'        FIND END DAY NUMBER                          
         XR    R0,R0                                                            
         IC    R0,SCHDAYNM                                                      
         SRL   R0,4                                                             
         STC   R0,HALF             FIND START DAY NUMBER                        
         MVC   BYTE,ESTOWKSD       GET ESTIMATE OOW BYTE                        
         CLI   BYTE,0                                                           
         BNE   *+8                                                              
         MVI   BYTE,1                                                           
*                                                                               
         CLC   HALF(1),HALF+1      LOOK AT START/END DAY NUMBERS                
         BE    VSCH06M             SAME DAY, NO PROBLEM                         
         BH    VSCH06C             START DAY NUMBER > END DAY NUMBER?           
*                                                                               
         CLC   HALF(1),BYTE        LOOK AT START DAY # AND EOOW                 
         BNL   VSCH06A             SAME DAY                                     
         CLC   HALF+1(1),BYTE      BDSEDAY CAN'T CROSS EOOW                     
         JH    INVLDATM                                                         
         B     VSCH06M             SAME DAY, NO PROBLEM                         
*                                                                               
VSCH06A  CLC   HALF+1(1),BYTE      END DAY CAN'T END WITH EOOW                  
         JE    INVLDATM                                                         
         B     VSCH06M             SAME DAY, NO PROBLEM                         
*                                                                               
VSCH06C  CLC   HALF+1(1),BYTE      END DAY NUMBER >= ROTATOR DAY NUM?           
         JNL   INVLDATM            YES, OOWR PROBLEM                            
*                                                                               
VSCH06M  MVC   SCHSTA,SLSTAT                                                    
         CLI   SCHSTA+4,C' '                                                    
         BNE   *+8                                                              
         MVI   SCHSTA+4,C'T'                                                    
         GOTO1 VALISTA,DMCB,SCHSTA VALIDATE THE STATION                         
         JNE   INVLSTA                                                          
*                                                                               
         L     RE,AIO                                                           
         USING STAREC,RE                                                        
         MVC   PWMKT,SMKT                   CURRENT MARKET                      
         MVC   SVOLDMKT(2),STOLDMK1         PREVIOUS MARKETS                    
         MVC   SVOLDMKT+2(2),STOLDMK2                                           
         MVC   SVSTABTY,SBKTYPE    SAVE STATION BOOKTYPE IF ANY                 
         MVC   SVSTAMKT,SMKTALPH                                                
         CLI   SVSTABTY,C'A'                                                    
         BL    *+12                                                             
         CLI   SVSTABTY,C'Z'                                                    
         BNH   *+8                                                              
         MVI   SVSTABTY,0                                                       
         DROP  RE                                                               
*                                                                               
         BRAS  RE,GOGETBU          SAVE BUYER NAME IF NEEDED                    
*                                                                               
         BRAS  RE,GETMKT           READ MARKET RECORD                           
         L     RE,AIO                                                           
         USING MKTRECD,RE                                                       
         MVC   SVMACCS,MKTLTACC                                                 
         MVC   SVMKTMKT,MKTALST                                                 
         DROP  RE                                                               
*                                                                               
VSCH06X  BRAS  RE,CALLOFCR         CHECK LIMIT ACCESS                           
*                                                                               
         OC    SVECOST2,SVECOST2   TEST COS2 ESTIMATE                           
         BZ    VSCH07                                                           
         BRAS  RE,GETPW            NEED PW/C2 LOCK FLAG                         
         JNE   INVLPW                                                           
         B     VSCH08                                                           
*                                                                               
VSCH07   TM    PWFLAG,X'01'        TEST WI PW ESTIMATE                          
         BNO   VSCH08                                                           
         BRAS  RE,CHKMKT           GET CORRECT MKT FROM PW RECS                 
         BRAS  RE,GETMKT           MAKE SURE IT'S VALID                         
         JNE   INVLMKT                                                          
*                                                                               
         L     RE,AIO                                                           
         USING MKTRECD,RE                                                       
         MVC   SVMKTMKT,MKTALST                                                 
         DROP  RE                                                               
*                                                                               
         BRAS  RE,GETPW            MAKE SURE IT'S NOT LOCKED                    
         JNE   INVLPW                                                           
         PACK  DUB,PWMKT                                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,BMKTSTA        FORCE CORRECT MARKET IN BUY KEY              
*****                                                                           
VSCH08   MVC   SCHAMKT,SVMKTMKT    SAVE ALPHA MKT IN SCHEDULE OBJECT            
         CLI   SVSTAMKT,C' '                                                    
         BNH   *+10                                                             
         MVC   SCHAMKT,SVSTAMKT                                                 
*****                                                                           
         GOTO1 VALIDPT,DMCB,SLDAYPRT                                            
         JNE   INVLDPT                                                          
         MVC   SCHDYPRT,SLDAYPRT   STORE THE DAYPART CODE                       
*                                                                               
         GOTO1 VALISLN,DMCB,SLLENGTH                                            
         JNE   INVLSPLN                                                         
         MVC   SCHTOTLN,BSPOTLEN                                                
*                                                                               
         GOTO1 (RF),(R1),SLMASLEN                                               
         JNE   INVLSPLN                                                         
         MVC   SCHMASLN,BSPOTLEN   MASTER PRODUCT SPOT LENGTH                   
*                                                                               
         CLI   BINPBPRD,0          TEST FOR PIGGYBACK                           
         BE    *+14                NO                                           
         CLC   SCHTOTLN,SCHMASLN   YES-PREVENT ALLOCATING ALL TIME              
         BE    INVLSPLN            TO MASTER                                    
*                                                                               
         GOTO1 HEXIN,DMCB,SLCOST,SCHCOST,L'SLCOST                               
         OC    12(4,R1),12(R1)                                                  
         JZ    INVLCOST                                                         
*                                                                               
         MVC   SCHPROG,SLPROG                                                   
         OC    SCHPROG,SPACES                                                   
*                                                                               
         LA    RE,SCHPROG+L'SCHPROG-2    POINT TO 2ND TO LAST CHAR              
         LA    RF,L'SCHPROG-1            SET FOR BCT LOOP                       
*                                                                               
VSCH08A  CLC   =C'-S',0(RE)              SPECIAL BUY?                           
         BNE   *+12                      NO                                     
         MVI   SCHPROG+L'SCHPROG-1,0     YES - INDICATE SPECIAL BUY!            
         B     VSCH08B                   AND EXIT LOOP!                         
         BCTR  RE,0                      DECREMENT PROGRAM NAME POINTER         
         BCT   RF,VSCH08A                LOOP BACK & KEEP SEARCHING             
*                                                                               
VSCH08B  GOTO1 HEXIN,DMCB,SLOVERS,SCHOVERS,L'SLOVERS                            
         OC    12(4,R1),12(R1)                                                  
         JZ    INVLOVER                                                         
*                                                                               
         LA    R5,SLDATA           STORE SPOTS PER WEEK                         
         LA    R3,SCHDATA          R3 = A(1ST ENTRY FOR SPOTS PER WEEK)         
         ZIC   R4,BNUMWKS                                                       
         SLL   R4,1                                                             
         GOTO1 HEXIN,DMCB,0(R5),0(R3),(R4)                                      
         OC    12(4,R1),12(R1)                                                  
         JZ    INVLSPOT                                                         
*                                                                               
         MVI   BYTE,99             MAX NORMAL SPOTS/WEEK                        
         TM    BITFLAG,BITFNPW     TEST BRAND POOL NPW                          
         BZ    *+8                 NO                                           
         MVI   BYTE,63             MAX POOL NPW                                 
*                                                                               
         XR    R0,R0                                                            
         IC    R0,BNUMWKS                                                       
         LA    RE,SCHSPOTS                                                      
         CLC   0(1,RE),BYTE        CHECK EXCEEDING MAXSPOTS/WEEK                
         JH    INVLNUMS                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         CLI   HDRDAILY,C'Y'       WE HAVE PROBLEMS W/ DAILY END DATES          
         BNE   VSCH30                                                           
         MVI   LBADDCHA,LBADD      LET'S SEE IF OUR END DATE FITS               
         BRAS  RE,BLDWKS                                                        
         OC    LAWKENN,LAWKENN     NO SPOTS?                                    
         BZ    VSCH30                                                           
         L     RF,LAWKENN                                                       
         GOTO1 DATCON,DMCB,(2,SWBSTDT-SWEEKSD(RF)),(0,DUB)                      
         GOTO1 GETDAY,DMCB,DUB,WORK+6                                           
         CLC   WORK+6(3),SPACES                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R1,7                R1=END DAY NUMBER                            
         SR    RF,RF               CLEAR RF FOR BIT TESTING                     
         XR    RE,RE                                                            
         IC    RE,SCHDAY                                                        
*                                                                               
VSCH10   SRDL  RE,1                                                             
         LTR   RF,RF               TEST FOR HIGHEST DAY ('ON')                  
         BNZ   *+8                                                              
         BCT   R1,VSCH10                                                        
*                                                                               
         XR    RE,RE                                                            
         IC    RE,DMCB             GET POTENTIAL END DATE DAY                   
         SR    R1,RE               FIND BDDAY END DAY - END DATE DAY            
         BP    *+14                BDDAY END DAY FOLLOWS END DATE DAY           
         MVC   WORK+6(6),DUB       SET END DATE                                 
         B     VSCH12                                                           
*                                                                               
         ST    R1,DMCB+8           BRING END DATE FORWARD                       
         GOTO1 ADDAY,DMCB,DUB,WORK+6                                            
VSCH12   GOTO1 DATCON,DMCB,WORK+6,(3,DUB)                                       
         CLC   DUB(3),SCHENDB      TEST END AFTER SCHEDULE END                  
         BH    INVLEDAT                                                         
*                                                                               
VSCH30   AR    R5,R4               STORE DEMO VALUES                            
         XR    R4,R4                                                            
         IC    R4,BNUMWKS          R3 = A(1ST ENTRY FOR DEMO VALUES)            
         AR    R3,R4                                                            
         XR    R4,R4                                                            
         CLI   NUMDCATS,0          IF THERE ARE DEMO CATEGORIES                 
         BE    VSCH40                                                           
         IC    R4,NUMDCATS         R4 = NUMBER OF BYTES TO CONVERT              
         SLL   R4,2                X 4 FOR LENGTH OF DEMO VALUE                 
         GOTO1 HEXIN,DMCB,0(R5),0(R3),(R4)                                      
         OC    12(4,R1),12(R1)     ERROR IF INVALID HEX                         
         JZ    INVLDEMS                                                         
*                                                                               
VSCH40   AR    R5,R4               BUMP PAST DEMO VALUES                        
         SRL   R4,1                R3 = A(AFTER ALL DATA)                       
         AR    R3,R4                                                            
*                                                                               
         TM    SCHINDS,SCHIRET     TEST FOR RETRANSFER                          
         BZ    VSCH60              NO                                           
*                                                                               
* VALIDATE PREVIOUSLY TRANSFERRRED SPOTS ARRAY                                  
*                                                                               
         ZIC   R4,BPRVWKS                                                       
         SLL   R4,1                X 2 FOR LENGTH OF WEEK                       
         GOTO1 HEXIN,DMCB,0(R5),0(R3),0(R4)                                     
         OC    12(4,R1),12(R1)     TEST FOR VALID HEX                           
         JZ    INVLTRAN                                                         
*                                                                               
         AR    R5,R4               POINT TO NEXT DATA AREA                      
         SRL   R4,1                                                             
         AR    R3,R4               BUMP PAST TRANSFERRED SPOTS ARRAY            
*                                                                               
* LOOK FOR OPTIONAL COMMENT AT END OF OBJECT                                    
*                                                                               
VSCH60   LR    R4,R5               R4=A(END OF OBJECT)                          
         S     R4,ADATA            COMPUTE LENGTH PROCESSED SO FAR              
         C     R4,DATALEN          TEST FOR MORE DATA TO PROCESS                
         BNL   VSCHSV              NO                                           
*                                                                               
         LNR   R4,R4               MAKE LENGTH PROCESSED NEGATIVE               
         A     R4,DATALEN          COMPUTE LENGTH REMAINING                     
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   SCHCOM(0),0(R5)     MOVE COMMENT TO OBJECT                       
*                                                                               
         LA    R4,1(R4)            RESTORE COMMENT LENGTH                       
         STC   R4,SCHCOMLN         AND SAVE IT                                  
         LA    R3,SCHCOM(R4)                                                    
*                                                                               
VSCHSV   LA    RE,SCHOBJCT         SAVE SCHEDULE FOR LATER PROCESSING           
         CLI   SCHERROR,0                                                       
         BE    *+8                                                              
         LA    R3,SCHODATE         USE THIS LENGTH IF ERROR                     
         SR    R3,RE                                                            
         XC    APPLERR,APPLERR                                                  
         CLI   SCHERROR,0          IF ERROR OCCURED                             
         BE    *+8                                                              
         BRAS  RE,ADDERR           ADD ERROR TO TABLE                           
         MVI   PUTFLAG,PUTFSCH                                                  
         STCM  R3,3,PUTLEN                                                      
         BRAS  RE,PUTTMPL                                                       
         JNE   EXIT                                                             
*                                                                               
         XR    R1,R1                                                            
         IC    R1,NUMSCHED         INCREMENT NUMBER OF SCHEDULES READ           
         LA    R1,1(R1)                                                         
         STC   R1,NUMSCHED                                                      
*                                                                               
VSX      J     XIT                 DONE VALIDATING THE SCHEDULE OBJECT          
         DROP  R2                                                               
*                                                                               
INVLTIME MVI   SCHERROR,SC$TIME    INVALID START/END TIMES                      
         B     VSCHSV                                                           
INVLSPLN MVI   SCHERROR,SC$SPLN    INVALID SPOT LENGTH                          
         B     VSCHSV                                                           
INVLCOST MVI   SCHERROR,SC$COST    INVALID COST                                 
         B     VSCHSV                                                           
INVLSPOT MVI   SCHERROR,SC$SPOT    INVALID SPOTS                                
         B     VSCHSV                                                           
INVLDEMS MVI   SCHERROR,SC$DEMS    INVALID DEMOS                                
         B     VSCHSV                                                           
INVLDATM MVI   SCHERROR,SC$DATM    INVALID DAY/TIME BLOCK                       
         B     VSCHSV                                                           
INVLDPT  MVI   SCHERROR,SC$DPT     INVALID DAYPART                              
         B     VSCHSV                                                           
INVLSTA  MVI   SCHERROR,SC$STA     INVALID STATION                              
         B     VSCHSV                                                           
INVLTRAN MVI   SCHERROR,SC$TRAN    INVALID TRANSFERRED SPOTS ARRAY              
         B     VSCHSV                                                           
INVLOVER MVI   SCHERROR,SC$OVER    INVALID DEMO OVERRIDE BIT FIELD              
         B     VSCHSV                                                           
INVLNUMS MVI   SCHERROR,SC$NUMS    MORE THAN 63 SPOTS IN A WEEK                 
         B     VSCHSV                                                           
*&&DO                                                                           
INVLSTBK MVI   SCHERROR,SC$STBK    ONE OF THE BOOKS DOESN'T EXIST FOR           
         B     VSCHSV                                                           
*&&                                                                             
INVOTIME MVI   SCHERROR,SC$OTIME   INVALID OVERRIDE FOR SPILL                   
         B     VSCHSV                                                           
INVLSECS MVI   SCHERROR,SC$SECS                                                 
         B     VSCHSV              TRYING TO CHANGE NUMBER OF SECONDS           
INVLEDAT MVI   SCHERROR,SC$EDAT                                                 
         B     VSCHSV              INVALID END DATE                             
         EJECT                                                                  
*=====================================================================          
* SUB-ROUTINE TO ADD SLINE ERROR TO TABLE                                       
*=====================================================================          
         SPACE 1                                                                
ADDERR   ST    RE,FULL                                                          
         LA    RE,ERRTAB                                                        
         SR    R1,R1                                                            
         ICM   R1,1,NUMERRS        GET N'ERRORS                                 
         BZ    ADDERR4                                                          
         LR    R0,R1               COPY ERROR COUNT TO R0                       
*                                                                               
ADDERR2  CLC   SCHLNNUM,0(RE)      TEST IF SLINE NUMBER IS IN TABLE             
         BE    ADDERRX             YES-EXIT                                     
         LA    RE,L'ERRTAB(RE)                                                  
         BCT   R0,ADDERR2                                                       
*                                                                               
ADDERR4  LA    R1,1(R1)                                                         
         CHI   R1,MAXERRS          TEST FOR TABLE OVERFLOW                      
         JH    INVLERRT                                                         
         STC   R1,NUMERRS                                                       
*                                                                               
         MVC   0(4,RE),SCHLNNUM    SET SLINE NUMBER                             
         MVC   4(1,RE),SCHERROR    COPY ERROR NUMBER                            
*                                                                               
ADDERRX  L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*=====================================================================          
* DERIVE THE SLINE DAY AND START/END TIMES FROM THE DAYTIME BLOCK               
* NTRY:                                                                         
* EXIT: SCHDAY, SCHSTIME, SCHNTIME, SCHINDS FILLED IN                           
*=====================================================================          
         SPACE 1                                                                
GETDT    NTR1  ,                                                                
         MVI   SCHDAY,0                                                         
         MVI   SCHDAYNM,0                                                       
         XC    SCHSTIME(4),SCHSTIME                                             
         LA    R2,SCHDT            R2=A(DAYTIME BLOCK)                          
         USING DTD,R2                                                           
         XR    R3,R3               R3=N'DAYTIME BLOCK ENTRIES                   
*                                                                               
GETDT2   CLI   DTDAY,0             TEST FOR EOB                                 
         BE    GETDT4              YES                                          
*                                                                               
         OC    SCHDAY,DTDAY        FORM CUMULATIVE DAY MASK                     
*                                                                               
         CLI   SCHDAYNM,0          TEST IF DAY NUMBERS HAVE BEEN SET            
         BNE   *+10                NO                                           
         MVC   SCHDAYNM,DTDAYNUM                                                
*                                                                               
         OC    SCHSTIME,SCHSTIME   TEST IF ANY START TIME SET                   
         BZ    *+14                NO SO SET IT                                 
         CLC   DTSTART,SCHSTIME    TEST FOR LOWEST START TIME                   
         BNL   *+10                NO                                           
         MVC   SCHSTIME,DTSTART                                                 
*                                                                               
         OC    SCHNTIME,SCHNTIME   TEST IF ANY END TIME SET                     
         BZ    *+14                NO                                           
         CLC   DTEND,SCHNTIME      TEST FOR HIGHEST END TIME                    
         BNH   *+10                NO                                           
         MVC   SCHNTIME,DTEND                                                   
*                                                                               
         LA    R3,1(R3)            INCREMENT ENTRY COUNT                        
         LA    R2,DTLNQ(R2)        NEXT DAYTIME BLOCK ENTRY                     
         B     GETDT2                                                           
*                                                                               
GETDT4   CHI   R3,1                TEST TO SET ORBIT FLAG                       
         BNH   *+8                                                              
         OI    SCHINDS,SCHIORB     YES-MORE THAN ONE ENTRY                      
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*=====================================================================          
* READ MARKET RECORD                                                            
* NTRY: PWMKT(4) = MARKET                                                       
* EXIT: AIO      = A(MARKET RECORD)                                             
*=====================================================================          
         SPACE 1                                                                
GETMKT   NTR1  ,                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   KEY+2(4),PWMKT      MARKET                                       
         MVC   KEY+6(2),SIGNON2C   AGY ALPHA                                    
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
         L     RF,AIO                                                           
         CLC   KEY(8),0(RF)                                                     
         JE    YES                                                              
         J     NO                                                               
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO FIGURE OUT WHICH MARKET TO USE                                     
*     INPUT : PWMKT(4) = CURRENT MARKET                                         
*             SVOLDMKT = OLD MARKETS                                            
*     OUTPUT: PWMKT(4) = VALID PW MARKET                                        
*=====================================================================          
         SPACE 1                                                                
CHKMKT   NTR1  ,                                                                
         OC    SVOLDMKT,SVOLDMKT   TEST ANY OLD MARKET NUMBER                   
         BZ    CHKMKTX             NO - EXIT                                    
*                                                                               
         XC    KEY,KEY             CHECK OLD MARKET FIRST                       
         LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,BAGYMED     A-M                                          
         MVC   PWKCLT,BCLT                                                      
         MVC   PWKPRD,BPRD         USE BRAND NOT POL                            
         MVC   PWKEST,BEST                                                      
         MVC   PWKMKT,SVOLDMKT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND                                   
         BNE   CHKMKT20            NO - TRY SECOND OLD MARKET                   
*                                                                               
         GOTO1 GETREC                                                           
         BRAS  RE,CHKDOLS                                                       
         BZ    CHKMKT20            NO LOCKED DOLLARS, TRY AGAIN                 
         MVC   PWKSTA,BMKTSTA+2    MOVE STATION TO KEY                          
*                                                                               
         GOTO1 HIGH                TEST STATION IN THIS MARKET TOO              
         CLC   KEY(12),KEYSAVE                                                  
         BE    CHKMKT30            YES- USE THIS MARKET                         
*                                                                               
CHKMKT20 OC    SVOLDMKT+2(2),SVOLDMKT+2   TEST SECOND OLD MKT                   
         BZ    CHKMKTX                    NO - DONE                             
         XC    KEY,KEY                    ELSE TRY AGAIN                        
         MVC   KEY(7),KEYSAVE             TYPE/A-M/CLT/PRD/EST                  
         MVC   PWKMKT,SVOLDMKT+2                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND                                   
         BNE   CHKMKTX             IF NOT, USE ORIGINAL MARKET                  
         GOTO1 GETREC                                                           
         BRAS  RE,CHKDOLS                                                       
         BZ    CHKMKTX             NO LOCKED DOLLARS, USE CURRENT MKT           
*                                                                               
         MVC   PWKSTA,BMKTSTA+2    MOVE STATION TO KEY                          
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     TEST THIS STATION WAS LOCKED                 
         BNE   CHKMKTX             NO - USE CURRENT MKT                         
*                                                                               
CHKMKT30 EDIT  PWKMKT,(4,PWMKT),FILL=0                                          
*                                                                               
CHKMKTX  J     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*=====================================================================          
* CHECK FOR LOCKED PW DOLLARS AND RETURN WITH CC NEQ IF FOUND                   
*=====================================================================          
         SPACE 1                                                                
CHKDOLS  NTR1  ,                                                                
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
         USING PWDOLEL,R6                                                       
         XR    RF,RF                                                            
*                                                                               
CHKDOL2  CLI   PWDOLCD,0                                                        
         JE    YES                                                              
         CLI   PWDOLCD,PWDOLCDQ                                                 
         BNE   CHKDOL4                                                          
*                                                                               
         OC    PWDOLWG,PWDOLWG                                                  
         JNZ   NO                                                               
         OC    PWDOLWN,PWDOLWN                                                  
         JNZ   NO                                                               
         OC    PWDOLCG,PWDOLCG                                                  
         JNZ   NO                                                               
*                                                                               
CHKDOL4  IC    RF,PWDOLLEN                                                      
         BXH   R6,RF,CHKDOL2                                                    
         DC    H'0'                                                             
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO READ PW STATUS RECORD                                              
*=====================================================================          
         SPACE 1                                                                
GETPW    NTR1  ,                                                                
*                                                                               
         MVC   CURCOS2,SVECOST2    SET CURRENT = ESTIMATE FACTOR                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,BAGYMED     A-M                                          
         MVC   PWKCLT,BCLT                                                      
         MVC   PWKPRD,BPRD         USE BRAND NOT POL                            
         MVC   PWKEST,BEST                                                      
         PACK  DUB,PWMKT                                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,PWKMKT                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   PWFKEY,KEYSAVE      TEST FOUND                                   
         JNE   YES                 IF NOT, CAN'T BE LOCKED                      
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
*                                                                               
         MVC   BYTE,PWGNFLG        MOVE STATUS FLAG                             
         NI    BYTE,(PWGNBUYQ+PWGNBILQ)                                         
         OC    PWFLAG,BYTE                                                      
*                                                                               
         OC    SVECOST2,SVECOST2   TEST COS2 ESTIMATE                           
         BNZ   GETPW10                                                          
*                                                                               
* NOW SEE IF DOLLARS ARE LOCKED IN YET                                          
*                                                                               
         LA    R6,PWEL             POINT TO FIRST ELEMENT                       
         XR    RF,RF                                                            
*                                                                               
GETPW02  CLI   0(R6),0                                                          
         BE    GETPWX                                                           
*                                                                               
         CLI   0(R6),PWDOLCDQ                                                   
         BNE   *+12                                                             
         OI    PWFLAG,X'20'        SET DOLLARS LOCKED FLAG                      
         B     GETPWX                                                           
*                                                                               
         IC    RF,1(R6)                                                         
         BXH   R6,RF,GETPW02                                                    
         B     GETPWX                                                           
         DROP  R6                                                               
         EJECT                                                                  
*================================================================               
* FIND CURRENT COS2 FACTOR IF ANY                                               
*================================================================               
         SPACE 1                                                                
         USING PWRECD,R6                                                        
GETPW10  LA    R6,PWEL             POINT TO FIRST ELEMENT                       
         XR    RF,RF                                                            
*                                                                               
GETPW12  CLI   0(R6),C2STCODQ       FIND COS2 ELEMENT                           
         BE    GETPW14                                                          
         CLI   0(R6),0                                                          
         BE    GETPWX                                                           
         IC    RF,1(R6)                                                         
         BXH   R6,RF,GETPW12                                                    
         B     GETPWX                                                           
         DROP  R6                                                               
*                                                                               
         USING C2STEL,R6                                                        
GETPW14  ICM   R0,15,C2STFCTR      GET CURRENT FACTOR                           
         BZ    *+8                                                              
         STCM  R0,15,CURCOS2                                                    
         B     GETPWX                                                           
*                                                                               
GETPWX   TM    PWFLAG,(PWGNBUYQ+PWGNBILQ)                                       
         JNZ   NO                                                               
         J     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
*=====================================================================          
* VALIDATE SPILL OBJECT AND ADD IT TO WORKER FILE                               
* SPLMKT IS A MARKET NUMBER                                                     
*=====================================================================          
         SPACE 1                                                                
VALISPL  NTR1  ,                                                                
         L     R2,ADATA            R2 = A(SPILL OBJECT)                         
*                                                                               
         MVC   SPLMKT,0(R2)                                                     
         MVC   SPLNETWK,8(R2)      CABLE NETWORK                                
*                                                                               
VALISP4  CLI   SVB0PROF+9,C'Y'     TEST PURPOSE CODES REQD                      
         BNE   VALISP10                                                         
         MVC   SPLPURP,11(R2)      PURPOSE CODE                                 
         OC    SPLPURP,SPACES                                                   
         CLC   SPLPURP,=C'000000'                                               
         BNH   VALISP10                                                         
*                                                                               
         XC    KEY,KEY                                                          
K        USING PRPRECD,KEY                                                      
         MVI   K.PRPKTYP,X'0D'                                                  
         MVI   K.PRPKSUB,X'19'                                                  
         MVC   K.PRPKAGY,SIGNON2C                                               
         MVC   K.PRPKMED,HDRMED                                                 
         MVC   K.PRPCODE,SPLPURP                                                
         DROP  K                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   INVLPURP                                                         
*                                                                               
VALISP10 MVI   PUTFLAG,PUTFSPL                                                  
         MVC   PUTLEN,=AL2(SPLLEN)                                              
         BRAS  RE,PUTTMPL                                                       
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* BUILD AND ADD BUY RECORD(S)                                                   
* EXIT: CC=EQ IF OK, CC=NEQ FOR RECORD OVERFLOW                                 
*=====================================================================          
         SPACE 1                                                                
ADDBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,AIO2             CLEAR IO2                                    
         LHI   R1,LIOS                                                          
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OC    LNSPWKSN,LNSPWKSN                                                
*        BNZ   *+12                                                             
         B     *+12                                                             
         MVI   BYTE,SC$SPOT        STUPID FUCKING STRATA DATA                   
         J     NO                                                               
*                                                                               
         CLI   LBADDCHA,LBCHA      RE-ADDING OVER EXISTING RECORD?              
         BE    *+8                 YES                                          
         BRAS  RE,GETBLINE         GET NEW LINE NUMBER                          
         BRAS  RE,BLDBKEY          BUILD BUYLINE KEY                            
*                                                                               
         L     R2,AIO2             MOVE KEY TO IO2                              
         ST    R2,AIO                                                           
         USING BUYRECD,R2                                                       
         MVC   BUYKEY(10),KEY                                                   
         MVC   BUYKEY+10(2),KEY+11 SHIFT OVER BUYLINE                           
         MVI   BUYKEY+12,0                                                      
         LA    R0,BDELEM-BUYREC    STORE DISPLACEMENT TO 1ST ELEMENT            
         STCM  R0,3,BUYRLEN                                                     
         MVC   BUYALPHA,SIGNON2C   INSERT AGENCY CODE                           
*                                                                               
         BRAS  RE,ADDNSPT          ADD NON-SPOT ELEMENTS                        
         JNE   NO                                                               
*                                                                               
ADBY02   XR    R1,R1               GET MAX N'SPOTS THAT'LL FIT                  
         ICM   R1,3,BUYRLEN                                                     
         BRAS  RE,GETMAXSP                                                      
*                                                                               
         LA    R4,SWEEKS                                                        
         USING SWEEKSD,R4                                                       
         LHI   R0,MAXWEEKS                                                      
         XR    RF,RF                                                            
ADBY04   CLI   SWBSTDT,FF          END OF LIST?                                 
         BE    ADBY06              YES                                          
         CLC   SWNSPT,SWOSPT       ANY SPOTS TO ADD THIS WEEK?                  
         BNH   *+8                 NO                                           
         AHI   RF,1                                                             
         AHI   R4,SWEEKSLQ                                                      
         BCT   R0,ADBY04                                                        
*                                                                               
ADBY06   XR    R0,R0                                                            
         L     R1,LMAXSPTS                                                      
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LHI   RF,1                STOP DIVIDE BY ZERO ERROR                    
         DR    R0,RF                                                            
         STC   R1,LMAXSPW                                                       
         CLI   LMAXSPW,0           AVOID DISASTER LATER                         
         BNE   *+8                                                              
         MVI   LMAXSPW,1                                                        
*                                                                               
ADBUY10  XC    BUYELEM,BUYELEM                                                  
         LA    R6,BUYELEM                                                       
         USING DLUELEM,R6                                                       
         MVI   DLUCODE,X'24'                                                    
         MVI   DLULEN,6                                                         
         LA    R1,SVESTBTY                                                      
         CLI   SVESTBTY,0          ANY OVERRIDE BOOKTYPE FROM ESTIMATE          
         BNE   ADBY11              YES, WE'LL USE THIS OVER STATION'S           
         LA    R1,SVSTABTY                                                      
         CLI   SVSTABTY,0                                                       
         BE    ADBY11B             NEITHER ARE SET, DON'T BOTHER                
*                                                                               
ADBY11   MVC   DLUBKTYP,0(R1)                                                   
*                                                                               
ADBY11B  CLI   SV1WPROF+6,C'Y'     TEST RADIO BUY LOOKUPS                       
         BNE   ADBY11X                                                          
****     MVC   DLUBAMKT,SVMKTMKT                                                
****     CLI   SVSTAMKT,C' '                                                    
****     BNH   *+10                                                             
****     MVC   DLUBAMKT,SVSTAMKT                                                
         MVC   DLUBAMKT,SCHAMKT    USE ALPHA MKT THAT WAS SAVE IN OBJ           
*                                                                               
ADBY11X  OC    DLUBKTYP(L'DLUBKTYP+L'DLUBAMKT),DLUBKTYP                         
         BZ    ADBY12                                                           
         GOTO1 ADDELEM,DMCB,BUYELEM                                             
         BNE   ADDBN                                                            
         DROP  R6                                                               
*                                                                               
* BUILD SPOT ELEMENTS                                                           
*                                                                               
ADBY12   XC    BUYELEM,BUYELEM     BUILD SPOT ELEMENTS                          
         LA    R6,BUYELEM                                                       
         USING REGELEM,R6                                                       
         TM    BITFLAG,BITFPOL     TEST FOR POOL ESTIMATE OPEN                  
         BZ    ADBY26                                                           
         TM    BITFLAG,BITFNPW     TEST FOR BRAND POL NPW                       
         BO    ADBY30              YES                                          
         EJECT                                                                  
*=====================================================================          
* BUILD ONE ELEMENT FOR EACH SPOT FOR BRAND OR TRUE POOL                        
*=====================================================================          
         SPACE 1                                                                
         MVI   RCODE,11            YES - POOL ORIGINAL ELEMENT                  
         CLI   BINPBPRD,0          TEST FOR PIGGYBACKS                          
         BNE   ADBY14              YES                                          
*                                                                               
         MVI   RLEN,10                                                          
         CLI   BPRD,FF             TEST FOR PRODUCT=POL (TRUE POL)              
         BE    ADBY16              YES-ADD SPOTS AS UNALLOCATED                 
*                                                                               
         MVI   RLEN,14                                                          
         MVC   RPPRD,BPRD          NO - ALLOCATED PRD IS CAMPAIGN PRD           
         MVC   RPTIME,BDSEC             TIME SHARE IS SPOT LENGTH               
         B     ADBY16                                                           
*                                                                               
ADBY14   MVI   RLEN,18             PIGGYBACKS- SET ACTIVE AND PASSIVE           
         MVC   RPPRD,BPRD                      PRODUCTS AND TIME SHARES         
         MVC   RPTIME,SCHMASLN                                                  
         MVC   RPPRD+L'RPALLOC(1),BINPBPRD                                      
         XR    R1,R1                                                            
         IC    R1,SCHTOTLN                                                      
         XR    R1,R1                                                            
         IC    R0,SCHMASLN                                                      
         SR    R1,R0                                                            
         STC   R1,RPTIME+L'RPALLOC                                              
         B     ADBY16                                                           
*                                                                               
ADBY16   LA    R4,SWEEKS                                                        
         USING SWEEKSD,R4                                                       
         LA    R3,LNEWTOT                                                       
         LHI   R0,MAXWEEKS                                                      
         MVC   LNSPTS,LMAXSPW      RESTRICT ADDS TO MAX SPOTS/WEEK              
         CLI   MASTER,0            PACKAGED RECORDS?                            
         BNE   ADBY18              NO                                           
         L     RF,LSPTOTN          RF=NUMBER OF SPOTS WE ARE ADDING             
         C     RF,LMAXSPTS         ADDING THAN MAX SPOTS THAT WILL FIT          
         BH    ADBY18              YES                                          
         CHI   RF,255              NEVER MORE THAN THIS ON RECORD               
         BH    ADBY18                                                           
         STC   RF,LNSPTS           ALLOW ADD OF ALL RECORDS                     
*                                                                               
ADBY18   CLI   SWBSTDT,FF          END OF LIST?                                 
         BE    ADBY24              YES                                          
*                                                                               
         CLC   SWOSPT,SWNSPT       ANY SPOTS TO ADD THIS WEEK?                  
         BE    ADBY22              NO                                           
         BL    ADBY19              YES                                          
         TM    SWIND1,SWI1FRZ      TEST FROZEN                                  
         BO    ADBY22              YES - SKIP WEEK                              
         DC    H'0'                                                             
*                                                                               
ADBY19   MVC   SVRDATE,SWDATE                                                   
         BRAS  RE,CHKLK            CHECK FOR ANY CLT OR EST LOCK                
         CLI   SVLOCK,C'C'         CLIENT LOCK?                                 
         JE    INVLCLK                                                          
         CLI   SVLOCK,C'E'         ESTIMATE LOCK?                               
         JE    INVLELK                                                          
*                                                                               
         MVC   RDATE,SWDATE        BUY DATE                                     
         MVC   ELEMENT(L'BUYELEM),BUYELEM                                       
*                                                                               
         XR    R5,R5                                                            
         IC    R5,SWNSPT                                                        
         XR    RF,RF                                                            
         IC    RF,SWOSPT                                                        
         SR    R5,RF               R5=NUMBER OF SPOTS LEFT TO PROCESS           
         CLM   R5,1,LNSPTS                                                      
         BL    *+8                                                              
         IC    R5,LNSPTS           DO ONLY NUMBER THAT WILL FIT FOR WK          
*                                                                               
         XR    RF,RF                                                            
         IC    RF,0(R3)                                                         
         AR    RF,R5               UPDATE CUMULATIVE SPOTS/WEEK RETURN          
         STC   RF,0(R3)                                                         
*                                                                               
         IC    RF,SWOSPT           UPDATE CUMULATIVE SPOTS ON FILE              
         AR    RF,R5                                                            
         STC   RF,SWOSPT                                                        
*                                                                               
ADBY20   BRAS  RE,ADDELS                                                        
         BNE   ADDBN                                                            
         BCT   R5,ADBY20                                                        
*                                                                               
ADBY22   AHI   R4,SWEEKSLQ         NEXT WEEK                                    
         LA    R3,1(R3)                                                         
         BCT   R0,ADBY18           DO FOR ALL WEEKS                             
*                                                                               
ADBY24   B     ADBY36              NOW WRITE THE BUY RECORD                     
         DROP  R4                                                               
         EJECT                                                                  
*=====================================================================          
* ADD SPOT ELEMENTS FOR BRAND BUYING (NON-POOL) - ONE FOR EACH WEEK             
*=====================================================================          
         SPACE 1                                                                
ADBY26   MVI   RCODE,6             NON-POOL SPOTS                               
         MVI   RLEN,10                                                          
         CLI   BINPBPRD,0          TEST FOR PIGGYBACKS                          
         BE    *+8                                                              
         MVI   RLEN,12             YES - EXPAND ELEMENT                         
*                                                                               
         LA    R4,SWEEKS                                                        
         USING SWEEKSD,R4                                                       
         LA    R3,LNEWTOT          R3=A(NEW SPOTS/WEEK ARRAY)                   
         LA    R0,MAXWEEKS                                                      
*                                                                               
ADBY28   CLI   SWBSTDT,FF          END OF LIST?                                 
         BE    ADDB23              YES                                          
         OC    SWNSPT,SWNSPT       TEST ANY SPOTS THIS WEEK                     
         BZ    ADDB22                                                           
         MVC   RDATE,SWDATE        BUY DATE                                     
         MVC   RNUM,SWNSPT         NUMBER OF SPOTS                              
         MVC   SWOSPT,SWNSPT                                                    
*                                                                               
         MVC   SVRDATE,RDATE                                                    
         BRAS  RE,CHKLK            CHECK FOR ANY CLT OR EST LOCK                
         CLI   SVLOCK,C'C'         CLIENT LOCK?                                 
         JE    INVLCLK                                                          
         CLI   SVLOCK,C'E'         ESTIMATE LOCK?  *GOCHA*                      
         JE    INVLELK                                                          
*                                                                               
         MVC   ELEMENT(L'BUYELEM),BUYELEM                                       
         BAS   RE,ADDELS           ADD BUY ELEMENT                              
         BNE   ADDBN                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,0(R3)            GET N'SPOTS FOR WEEK                         
         XR    RE,RE                                                            
         IC    RE,RNUM             GET N'SPOTS ADDED TO WEEK                    
         AR    RF,RE                                                            
         STC   RF,0(R3)            REPLACE CUMULATIVE SPOTS/WEEK                
*                                                                               
ADDB22   AHI   R4,SWEEKSLQ         NEXT WEEK                                    
         LA    R3,1(R3)                                                         
         BCT   R0,ADBY28           DO FOR ALL WEEKS                             
*                                                                               
ADDB23   B     ADBY36                                                           
         EJECT                                                                  
*=====================================================================          
* ADD SPOT ELEMENTS FOR BRAND POL NPW - ONE SPOT ELEMENT PER WEEK               
* WHERE HIGH ORDER 6 BITS OF COST CONTAIN THE NUMBER OF SPOTS                   
*=====================================================================          
         SPACE 1                                                                
ADBY30   MVI   RCODE,11            BRAND POL NPW                                
         MVI   RLEN,14                                                          
         MVC   RPPRD,BPRD          SET PRODUCT ALLOCATION                       
         MVC   RPTIME,BDSEC                                                     
         LA    R4,SWEEKS           R4=A(WEEK TABLE)                             
         USING SWEEKSD,R4                                                       
         LA    R3,LNEWTOT          R3=A(NEW SPOTS/WEEK ARRAY)                   
         LHI   R0,MAXWEEKS         R0=LOOP COUNTER                              
*                                                                               
ADBY32   CLI   SWBSTDT,FF          END OF LIST?                                 
         BE    ADDB23              YES                                          
*                                                                               
         OC    SWNSPT,SWNSPT       TEST ANY SPOTS THIS WEEK                     
         BZ    ADBY34              NO                                           
         MVC   RDATE,SWDATE                                                     
         MVC   SVRDATE,RDATE                                                    
*                                                                               
         BRAS  RE,CHKLK            CHECK FOR ANY CLT OR EST LOCK                
         CLI   SVLOCK,C'C'         CLIENT LOCK?                                 
         JE    INVLCLK                                                          
         CLI   SVLOCK,C'E'         ESTIMATE LOCK?                               
         JE    INVLELK                                                          
*                                                                               
         XR    RE,RE               GET SPOTS THIS WEEK                          
         IC    RE,SWNSPT           GET SPOTS THIS WEEK                          
         SLL   RE,2                USE HIGH ORDER 6 BITS                        
         STC   RE,RPCOST                                                        
         MVC   SWOSPT,SWNSPT                                                    
*                                                                               
         XR    RF,RF                                                            
         IC    RF,0(R3)            UPDATE NEW SPOTS/WEEK                        
         SRL   RE,2                RESTORE NUMBER OF SPOTS                      
         AR    RF,RE                                                            
         STC   RF,0(R3)                                                         
*                                                                               
         MVC   ELEMENT(L'BUYELEM),BUYELEM                                       
         BAS   RE,ADDELS                                                        
         BNE   ADDBN                                                            
*                                                                               
ADBY34   AHI   R4,SWEEKSLQ         NEXT WEEK                                    
         LA    R3,1(R3)                                                         
         BCT   R0,ADBY32                                                        
         DROP  R4                                                               
         EJECT                                                                  
*=====================================================================          
* FINISHED ADDING ALL SPOT ELEMENTS FOR THIS RECORD - ADD RECORD                
*=====================================================================          
         SPACE 1                                                                
ADBY36   LA    R4,SWEEKS           R4=A(WEEK TABLE)                             
         USING SWEEKSD,R4                                                       
         LHI   R0,MAXWEEKS         R0=LOOP COUNTER                              
ADBY36A  CLI   SWBSTDT,FF          END OF LIST?                                 
         BE    ADBY36Z                                                          
         CLC   SWNSPT,SWOSPT       ALL SPOTS ACCOUNTED FOR?                     
         BNE   ADBY38              YES                                          
         AHI   R4,SWEEKSLQ         NEXT WEEK                                    
         BCT   R0,ADBY36A                                                       
         DROP  R4                                                               
*                                                                               
ADBY36Z  CLI   MASTER,0            PACKAGE MASTER ON PREVIOUS BUY?              
         BNE   ADBY40              YES                                          
*                                                                               
         L     R1,AIO2             MAKE SURE THERE ARE SPOT ELEMENTS            
         LA    R1,BDELEM-BUYRECD(R1)                                            
         XR    RF,RF                                                            
*                                                                               
ADBY37   CLI   0(R1),0                                                          
         BE    ADBY37X                                                          
*                                                                               
         CLI   0(R1),X'06'         TEST FOR ANY ELEM 06-0C                      
         BL    ADBY37A                                                          
         CLI   0(R1),X'0D'                                                      
         BL    ADBY37Z                                                          
*                                                                               
ADBY37A  IC    RF,1(R1)                                                         
         BXH   R1,RF,ADBY37                                                     
*                                                                               
ADBY37X  BRAS  RE,DELBREC                                                       
         B     ADDBX                                                            
*                                                                               
ADBY37Z  BRAS  RE,WRTBREC                                                       
         BRAS  RE,CHGBTBL          UPDATE BUY LINE TABLE ENTRY                  
         B     ADDBX                                                            
*                                                                               
ADBY38   CLI   MASTER,0            ALREADY HAVE PACKAGE MASTER?                 
         BNE   ADBY40              YES                                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ADD MASTER PACKAGE ELEMENT                   
         USING PKGELEM,R6                                                       
         MVI   PKGCODE,5                                                        
         MVI   PKGLEN,3                                                         
         MVI   PKGIND,1            1=MASTER                                     
         MVC   MASTER,NEWLINE      SET MASTER ELEMENT ON THIS RECORD            
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         BRAS  RE,WRTBREC                                                       
         BRAS  RE,CHGBTBL          UPDATE BUY LINE TABLE ENTRY                  
         GOTO1 DELELEM,DMCB,5      REMOVE PACKAGE ELEMENT                       
         B     ADBY42                                                           
         DROP  R6                                                               
*                                                                               
ADBY40   XC    ELEMENT,ELEMENT     ADD SLAVE PACKAGE ELEMENT                    
         LA    R6,ELEMENT                                                       
         USING PKGELEM,R6                                                       
         MVI   PKGCODE,5                                                        
         MVI   PKGLEN,4                                                         
         MVI   PKGIND,2            2=SLAVE                                      
         MVC   PKGLINES,MASTER     POINT SLAVE TO MASTER                        
         MVC   SLAVE,NEWLINE                                                    
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         BRAS  RE,WRTBREC                                                       
         BRAS  RE,CHGBTBL          UPDATE BUY LINE TABLE ENTRY                  
         GOTO1 DELELEM,DMCB,5      REMOVE PACKAGE ELEMENT                       
*                                                                               
         MVC   NEWLINE,MASTER      READ MASTER FOR UPDATE INTO AADDIO           
         BRAS  RE,BLDBKEY                                                       
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUYKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                WHERE MASTER RECORD?                         
*                                                                               
         MVC   FULL,AIO                                                         
         MVC   AIO,AADDIO                                                       
         GOTO1 GETREC              GET MASTER RECORD INTO AADDIO                
         GOTO1 GETELEM,DMCB,5                                                   
         USING PKGELEM,R6                                                       
         XR    RF,RF               COPY PACKAGE ELEMENT LOCALLY                 
         IC    RF,PKGLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R6)                                                 
         GOTO1 DELELEM,DMCB,5      THEN REMOVE IT PRIOR TO UPDATING             
*                                                                               
         LA    R6,ELEMENT          UPDATE BY ADDING SLAVE TO LIST               
         XR    RF,RF                                                            
         IC    RF,PKGLEN                                                        
         LA    RE,PKGCODE(RF)                                                   
         MVC   0(1,RE),SLAVE       ADD SLAVE TO LIST IN MASTER                  
         AHI   RF,1                                                             
         STC   RF,PKGLEN           UPDATE LENGTH AND ADD BACK TO RECORD         
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         GOTO1 PUTREC              WRITE RECORD BACK                            
         MVC   AIO,FULL                                                         
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
*                                                                               
ADBY41   LA    R4,SWEEKS           R4=A(WEEK TABLE)                             
         USING SWEEKSD,R4                                                       
         LHI   R0,MAXWEEKS         R0=LOOP COUNTER                              
ADBY41A  CLI   SWBSTDT,FF          END OF LIST?                                 
         JE    YES                                                              
         CLC   SWNSPT,SWOSPT       ALL SPOTS ACCOUNTED FOR?                     
         BNE   ADBY42              YES                                          
         AHI   R4,SWEEKSLQ         NEXT WEEK                                    
         BCT   R0,ADBY41A                                                       
         DROP  R4                                                               
*                                                                               
ADBY42   BRAS  RE,GETBLINE         GET NEXT AVAILABLE LINE NUMBER               
         BRAS  RE,BLDBKEY          CONSTRUCT BUY LINE KEY IN KEY                
         MVC   BUYKEY+10(2),KEY+11 SET BUY LINE NUMBER BYTES IN RECORD          
         MVI   BUYKEY+12,0                                                      
         GOTO1 DELELEM,DMCB,11     DELETE ALL BUY ELEMENTS                      
*                                                                               
         BRAS  RE,RESWKS                                                        
         B     ADBY02              PROCESS REMAINING SPOTS                      
*                                                                               
ADDBX    MVC   AIO,AIO1            RESET IO POINTER                             
         J     YES                                                              
*                                                                               
ADDBN    MVI   BYTE,SC$BIG         SET ERROR CODE FOR RECORD OVERFLOW           
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
*=====================================================================          
* THIS ROUTINE GETS THE BUY LINE FROM THE BUYLINE TABLE.                        
*                                                                               
* EXIT: AENTRY  = A(ENTRY IN BUYLINE TABLE)                                     
*       NEWLINE = NEXT BUY LINE AVAILABLE                                       
*=====================================================================          
         SPACE 1                                                                
GETBLINE NTR1  ,                                                                
         L     R2,ABLNTBL          R2 = A(BUYLINE TABLE)                        
         USING BLND,R2                                                          
         LHI   R0,MAXBLNS          R0 = MAXIMUM NUMBER OF BUYLINES              
*                                                                               
GTBLP    CLI   BLNMAS,0            IF THERE IS NO PRODUCT IN BUYLINE            
         BE    GTB20                                                            
         CLI   BLNSTAT,BLNSDEL     IF BUYLINE IS DELETED                        
         BE    GTB20                                                            
         AHI   R2,BLNLNQ           NEXT BUYLINE ENTRY                           
         BCT   R0,GTBLP                                                         
         J     INVLFULL                                                         
*                                                                               
GTB20    MVC   NEWLINE,BLNLINE     SAVE BUYLINE TO USE                          
         ST    R2,AENTRY           SAVE A(BUYLINE ENTRY)                        
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO ADD AN ELEMENT                                                     
* AT ENTRY, AIO=RECORD AND ELEMENT CONTAINS ELEMENT TO BE ADDED                 
* ON EXIT, CC=EQ IF OK, CC=NEQ FOR RECORD OVERFLOW                              
*=====================================================================          
         SPACE 1                                                                
ADDELS   NTR1  ,                                                                
         GOTO1 HELLO,DMCB,(C'P',=CL8'SPTFIL'),AIO,ELEMENT,0,0                   
         CLI   12(R1),0            TEST OK                                      
         JE    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* BUILD AND UPDATE BUY RECORD(S)                                                
* ON EXIT, CC=EQ IF OK, CC=NEQ FOR RECORD OVERFLOW                              
*=====================================================================          
         SPACE 1                                                                
UPDBUY   NTR1  BASE=*,LABEL=*                                                   
         XC    SLAVE,SLAVE                                                      
*                                                                               
UPBY02   BRAS  RE,BUYDESC          UPDATE BDELEM                                
         JNE   NO                                                               
         BRAS  RE,UPDBNSP          UPDATE NON-SPOT ELEMENTS                     
*                                                                               
         LA    R4,SWEEKS                                                        
         USING SWEEKSD,R4                                                       
         LHI   R0,MAXWEEKS                                                      
         OI    LFLAG,LFSTWK                                                     
*                                                                               
UPBY04   CLI   SWBSTDT,FF          END OF LIST?                                 
         BE    UPBY14              YES                                          
*                                                                               
         TM    SWIND1,SWI1FRZ      FROZEN WEEK?                                 
         BO    UPBY10              YES - NEXT WEEK                              
*********                                                                       
* NEXT 2 LINES ARE COMMENTED OUT BECAUSE IF THE ROTATION CHANGES FROM           
*  W-F  TO  TH-F, THE SPOTS REMAIN WITH THE WRONG DATE IF WE DON'T              
*********                                                                       
*****    CLC   SWOSPT,SWNSPT       SAME NUMBER OF OLD AND NEW SPOTS?            
*****    BE    UPBY10              YES NEXT WEEK                                
*                                                                               
         TM    BITFLAG,BITFNPW     TEST BRAND POOL NPW                          
         BO    UPBY06                                                           
         TM    BITFLAG,BITFPOL                                                  
         BO    UPBY08                                                           
*                                                                               
         GOTOR NPOOLSPT,DMCB,SWEEKSD     ADD/DELETE NON POOL SPOTS              
         B     UPBY10                                                           
*                                                                               
UPBY06   GOTOR NPWSPT,DMCB,SWEEKSD       ADD/DELETE BRAND NPW SPOTS             
         B     UPBY10                                                           
*                                                                               
UPBY08   GOTOR POOLSPT,DMCB,SWEEKSD      ADD/DELETE POOL SPOTS                  
*                                                                               
UPBY10   NI    LFLAG,255-LFSTWK                                                 
         AHI   R4,SWEEKSLQ                                                      
         BCT   R0,UPBY04                                                        
*                                                                               
UPBY14   L     R2,AIO              SEE IF ANY SPOTS ON THIS RECORD              
         AHI   R2,BDELEM-BUYREC                                                 
         USING REGELEM,R2                                                       
         XR    RF,RF                                                            
UPBY16   CLI   RCODE,0                                                          
         BE    UPBY20                                                           
         CLI   RCODE,X'06'                                                      
         BL    UPBY18                                                           
         CLI   RCODE,X'0C'                                                      
         BH    UPBY18                                                           
         BRAS  RE,CHKSLN           MAKE SURE SLN'S AGREE                        
         B     UPBY32                                                           
UPBY18   IC    RF,RLEN                                                          
         BXH   R2,RF,UPBY16                                                     
         DROP  R2                                                               
*                                                                               
UPBY20   CLC   MASTER,NEWLINE      NO SPOTS - LEAVE ALONE IF PKG MSTR           
         BE    UPBY32                                                           
         GOTO1 GETELEM,DMCB,5      PACKAGE SLAVE?                               
         BNE   UPBY30              NO - JUST DELETE RECORD                      
         USING PKGELEM,R6                                                       
*                                                                               
* NEED TO REMOVE POINTER TO THIS SLAVE FROM PACKAGE ELEMENT IN MASTER           
* THEN REMOVE PACKAGE ELEMENT IN SLAVE BEFORE DELETING SLAVE                    
*                                                                               
         CLI   PKGIND,2            BETTER BE A SLAVE                            
         BE    *+6                                                              
         DC    H'0'                SUPPOSED TO KEEP MASTERS                     
*                                                                               
         MVC   SNEWLINE,NEWLINE                                                 
         MVC   NEWLINE,PKGLINES    GET POINTER TO MASTER                        
*                                                                               
         BRAS  RE,BLDBKEY                                                       
         MVC   NEWLINE,SNEWLINE                                                 
         GOTO1 HIGH                GET MASTER                                   
         CLC   KEY(L'BUYKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FULL,AIO            SAVE IO AREA FOR SLAVE                       
         MVC   AIO,AADDIO                                                       
         GOTO1 GETREC                                                           
*                                                                               
* WE ARE NOW PROCESSING MASTER (IN AADDIO)                                      
*                                                                               
         GOTO1 GETELEM,DMCB,5      PACKAGE ELEMENT?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PKGELEM,R6                                                       
         CLI   PKGIND,1            BETTER BE A MASTER                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEMENT,ELEMENT     COPY AND DELETE MASTER PACKAGE               
         XR    RF,RF                                                            
         IC    RF,PKGLEN                                                        
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),PKGELEM                                               
         GOTO1 DELELEM,DMCB,5                                                   
*                                                                               
         LA    R6,ELEMENT          NOW REMOVE THIS SLAVE FROM LIST              
         XR    RF,RF                                                            
         IC    RF,PKGLEN                                                        
         AHI   RF,-3                                                            
         LA    R1,PKGLINES                                                      
*                                                                               
         CLC   NEWLINE,0(R1)                                                    
         BE    UPBY22                                                           
         LA    R1,1(R1)                                                         
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
*                                                                               
UPBY22   LA    R0,PKGLINES                                                      
         CR    R1,R0               FIRST SLAVE IN LIST?                         
         BNE   UPBY24              NO                                           
         MVI   SLAVE,0             RESET SLAVE FOR UPBY36 CODE                  
         B     UPBY26                                                           
*                                                                               
UPBY24   AHI   R1,-1                                                            
         MVC   SLAVE,0(R1)         RESET SLAVE FOR UPBY36 CODE                  
         AHI   R1,1                                                             
*                                                                               
UPBY26   EX    RF,*+8              NOT *+4 - DESTRUCTIVE MOVE                   
         B     *+10                                                             
         MVC   0(0,R1),1(R1)       CLOSE GAP                                    
         XR    RF,RF                                                            
         IC    RF,PKGLEN           ADJUST LENGTH AND PUT BACK                   
         AHI   RF,-1                                                            
         STC   RF,PKGLEN                                                        
*                                                                               
         XC    SLAVELST,SLAVELST   RESET LIST OF SLAVES                         
         XR    RF,RF               AND ADD NEW PACKAGE ELEMENT                  
         IC    RF,PKGLEN                                                        
         AHI   RF,-4                                                            
         BM    UPBY28                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SLAVELST(0),PKGLINES                                             
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         DROP  R6                                                               
*                                                                               
UPBY28   GOTO1 PUTREC              WRITE BACK MASTER RECORD                     
         MVC   AIO,FULL            RESTORE AIO FOR SLAVE                        
*                                                                               
* HAVING WRITTEN MASTER (IF APPLICABLE) WE ARE NOW BACK TO SLAVE                
*                                                                               
UPBY30   GOTO1 DELELEM,DMCB,5      REMOVE ANY PACKAGE ELEMENT IF SLAVE          
         LHI   RE,X'98'                                                         
         ST    RE,DMCB                                                          
         GOTO1 DELELEM,DMCB        DO NOT WRITE X'98' IN GOTO1 MACRO            
         BRAS  RE,BLDBKEY                                                       
         BRAS  RE,DELBREC          DELETE RECORD                                
         B     UPBY34                                                           
*                                                                               
UPBY32   BRAS  RE,WRTBREC          RE-READ FOR UPDATE/WRITE IT BACK             
*                                                                               
UPBY34   CLI   MASTER,0            ANY SLAVES?                                  
         BE    UPBY42              NO, THAT'S ALL                               
*                                                                               
         LA    RF,SLAVELST         FIND NEXT SLAVE                              
         CLI   SLAVE,0             FIRST SLAVE?                                 
         BE    UPBY40              YES                                          
*                                                                               
UPBY36   CLI   0(RF),0             FIND OUR SLAVE                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SLAVE,0(RF)                                                      
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     UPBY36                                                           
*                                                                               
UPBY38   LA    RF,1(RF)            NEXT SLAVE                                   
*                                                                               
UPBY40   CLI   0(RF),0             WAS THIS THE LAST SLAVE?                     
         BE    UPBY42              YES                                          
         MVC   NEWLINE,0(RF)                                                    
         MVC   SLAVE,0(RF)                                                      
         BRAS  RE,BLDBKEY                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUYKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC              EDIT THIS RECORD NOW.                        
         B     UPBY02                                                           
*                                                                               
UPBY42   LA    R4,SWEEKS           DONE ALL RECORDS - SEE WHATS LEFT            
         LHI   R0,MAXWEEKS                                                      
*                                                                               
UPBY44   CLI   SWBSTDT,FF          END OF LIST?                                 
         BE    UPBY48              YES                                          
         TM    SWIND1,SWI1FRZ      FROZEN WEEK?                                 
         BO    UPBY46                                                           
         CLC   SWOSPT,SWNSPT       ALL SPOTS MATCH?                             
         BE    UPBY46                                                           
         BL    *+6                                                              
         DC    H'0'                TOO MANY OLD SPOTS                           
*                                                                               
         BRAS  RE,RESWKS                                                        
         MVI   LBADDCHA,LBADD      FUDGE FOR NEW RECORD ADD                     
         BRAS  RE,ADDBUY           NO                                           
         MVI   LBADDCHA,LBCHA      STILL HAVE OLD SPOTS?                        
         B     UPBY48                                                           
*                                                                               
UPBY46   AHI   R4,SWEEKSLQ                                                      
         BCT   R0,UPBY44                                                        
*                                                                               
UPBY48   CLI   MASTER,0            MASTER/SLAVE SETUP?                          
         JE    YES                                                              
*                                                                               
         MVC   NEWLINE,MASTER      RESTORE MASTER RECORD                        
         BRAS  RE,BLDBKEY                                                       
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         J     YES                                                              
         EJECT                                                                  
*=====================================================================          
* UPDATE NON BUY ELEMENTS IN RECORD FROM UPLOAD                                 
*=====================================================================          
         SPACE 1                                                                
UPDBNSP  NTR1  ,                                                                
*??      CLI   HDRDEMO,C'N'        TEST DEMO CHANGES ALLOWED                    
*??      BE    UBNS01              NO                                           
         BRAS  RE,BLDDEMO          REPLACE NEW DEMO ELEMENT                     
         BRAS  RE,GETSPL                                                        
*                                                                               
UBNS01   GOTO1 DELELEM,DMCB,99     REPLACE RADIO DESCRIPTION ELEMENT            
         OC    BOOKS,BOOKS                                                      
         BZ    UBNS02                                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING RDELEM,R6                                                        
         MVI   RDCODE,X'63'                                                     
         MVI   RDLEN,10                                                         
         MVC   RDBOOKS,BOOKS                                                    
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         DROP  R6                                                               
*                                                                               
UBNS02   GOTO1 DELELEM,DMCB,107    DELETE CANADIAN PST ELEMENT                  
*        OC    STAPST,STAPST                                                    
*        BZ    UBNS04                                                           
*        MVI   ELEMENT,X'6B'                                                    
*        MVI   ELEMENT+1,12                                                     
*        MVC   ELEMENT+2(10),STAPST                                             
*        GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
UBNS04   GOTO1 DELELEM,DMCB,97     REPLACE TRAFFIC MASTER ELEMENT               
         CLI   MCLUNQ,0                                                         
         BE    UBNS06                                                           
         MVI   ELEMENT,X'61'                                                    
         MVI   ELEMENT+1,6                                                      
         MVC   ELEMENT+2(2),MCLCOD                                              
         MVC   ELEMENT+4(1),MCLUNQ                                              
         MVC   ELEMENT+5(1),MCLPRD                                              
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
UBNS06   BRAS  RE,BLDCOM           REPLACE 1-3 COMMENT ELEMENTS                 
*                                                                               
         B     UBNS10              IGNORE ORBIT ELEMENT                         
*        GOTO1 DELELEM,DMCB,103                                                 
*        TM    SCHINDS,SCHIORB     TEST ORBIT                                   
*        BZ    UBNS10                                                           
*        BRAS  RE,BLDORB                                                        
*        GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
UBNS10   XC    ELEMENT,ELEMENT                                                  
         LHI   RE,X'99'                                                         
         ST    RE,DMCB                                                          
         GOTO1 GETELEM,DMCB        DO NOT WRITE X'99' IN GOTO1                  
         BNE   UBNS12                                                           
         XR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R6)                                                 
         LHI   RE,X'99'                                                         
         ST    RE,DMCB                                                          
         GOTO1 DELELEM,DMCB                                                     
*                                                                               
UBNS12   GOTO1 GETFACT,DMCB,(X'02',0),0                                         
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         OC    FAPASSWD,FAPASSWD                                                
         BZ    UBNS14                                                           
         MVI   ELEMENT,X'99'                                                    
         MVI   ELEMENT+1,12                                                     
         MVC   ELEMENT+7(2),FAPASSWD  SET DATE OF LAST CHANGE                   
         MVC   ELEMENT+9(3),FADATEB                                             
*                                                                               
UBNS14   CLI   ELEMENT,X'99'       IF WE HAD ONE - PUT IT BACK                  
         BNE   UBNS16                                                           
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
UBNS16   J     XIT                                                              
         EJECT                                                                  
*=====================================================================          
* ADD/DELETE NON-POOL SPOT ELEMENTS                                             
* NTRY:   P1 = A(WEEK TABLE ENTRY)                                              
*         LFLAG = LFSTWK FOR THE FIRST WEEK                                     
*=====================================================================          
         SPACE 1                                                                
NPOOLSPT NTR1  ,                                                                
         L     R4,0(R1)                                                         
         USING SWEEKSD,R4                                                       
         L     R2,AIO                                                           
         USING BUYRECD,R2                                                       
         CLI   SWOSPT,0            CURRENTLY ANY SPOTS?                         
         BNE   NPS02               YES                                          
         CLI   SWNSPT,0            ANY NEW SPOTS TO ADD?                        
         JE    YES                 NO                                           
*                                                                               
         XC    BUYELEM,BUYELEM     BUILD NEW SPOT ELEMENT                       
         LA    R6,BUYELEM                                                       
         USING REGELEM,R6                                                       
         MVI   RCODE,6                                                          
         MVI   RLEN,10                                                          
         MVC   RNUM,SWNSPT         NUMBER OF SPOTS                              
         MVC   RDATE,SWDATE        BUY DATE                                     
         MVC   SVRDATE,RDATE                                                    
         BRAS  RE,CHKLK            CHECK FOR ANY CLT OR EST LOCK                
         JE    XIT                                                              
*                                                                               
         XC    SWNSPT,SWNSPT                                                    
         XC    SWOSPT,SWOSPT       CLEAR NEW/OLD COUNT                          
         BRAS  RE,ADDREG           ADD SPOT ELEMENT                             
         J     XIT                                                              
*                                                                               
NPS02    XR    RF,RF               FIND THE SPOT ELEMENT FOR THIS WEEK          
         LA    R6,BDELEM                                                        
*                                                                               
NPS04    CLI   RCODE,0             NOT IN THIS RECORD                           
         JE    YES                                                              
         CLI   RCODE,6             NON-POOL BUY ELEMENT                         
         BNE   NPS06               NO                                           
*                                                                               
         TM    LFLAG,LFSTWK+LWEEKLY  TEST WEEKLY SKED AND FIRST WEEK            
         BNO   NPS06                                                            
         CLC   RDATE,SCHSTMNP      YES-COMP DATE TO SDEF START MON              
         BL    NPS06                                                            
         CLC   RDATE,SWBENDT                                                    
         BNH   NPS10                                                            
         B     NPS06                                                            
*                                                                               
NPS06    CLC   RDATE,SWBSTDT       TEST BUY DATE IS IN THIS WEEK                
         BL    NPS08                                                            
         CLC   RDATE,SWBENDT                                                    
         BNH   NPS10                                                            
*                                                                               
NPS08    IC    RF,RLEN             NEXT BUY ELEMENT                             
         BXH   R6,RF,NPS04                                                      
*                                                                               
NPS10    CLI   SWNSPT,0            BUY ELEMENT FOUND -                          
         BE    NPS12                                                            
         MVC   RNUM,SWNSPT         NON-ZERO SPOTS - CHANGE NUMBER               
         XC    SWNSPT,SWNSPT                                                    
         XC    SWOSPT,SWOSPT       CLEAR NEW/OLD COUNT                          
         J     YES                                                              
*                                                                               
NPS12    MVC   SVRDATE,RDATE                                                    
         BRAS  RE,CHKLK            CHECK FOR ANY CLT OR EST LOCK                
         JE    XIT                                                              
*                                                                               
         MVI   RCODE,FF            NO SPOTS NOW - DELETE THE ELEMENT            
         GOTO1 DELELEM,DMCB,FF                                                  
         XC    SWOSPT,SWOSPT                                                    
         XC    SWNSPT,SWNSPT       CLEAR OLD AND NEW SPOT COUNTS                
         J     YES                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*=====================================================================          
* ADD/DELETE BRAND POOL NPW SPOT ELEMENTS                                       
* NTRY:   P1 = A(WEEK TABLE ENTRY)                                              
*         LFLAG = LFSTWK FOR THE FIRST WEEK                                     
*=====================================================================          
         SPACE 1                                                                
NPWSPT   NTR1  ,                                                                
         L     R4,0(R1)                                                         
         USING SWEEKSD,R4                                                       
         L     R2,AIO                                                           
         USING BUYRECD,R2                                                       
         CLI   SWOSPT,0            CURRENTLY ANY SPOTS?                         
         BNE   NPW02               YES                                          
         CLI   SWNSPT,0            ANY NEW SPOTS TO ADD?                        
         JE    YES                 NO                                           
*                                                                               
         XC    BUYELEM,BUYELEM     BUILD NEW SPOT ELEMENT                       
         LA    R6,BUYELEM                                                       
         USING REGELEM,R6                                                       
         MVI   RCODE,11                                                         
         MVI   RLEN,14                                                          
         MVC   RDATE,2(R3)         BUY DATE                                     
         MVC   RPPRD,BPRD          SET PRODUCT ALLOCATION                       
         MVC   RPTIME,BDSEC                                                     
         XR    RE,RE                                                            
         IC    RE,SWNSPT           NUMBER OF SPOTS                              
         SLL   RE,2                USE HIGH ORDER 6 BITS OF COST                
         STC   RE,RPCOST                                                        
         MVC   RDATE,SWDATE        BUY DATE                                     
*                                                                               
         MVC   SVRDATE,RDATE                                                    
         BRAS  RE,CHKLK            CHECK FOR ANY CLT OR EST LOCK                
         JE    NO                                                               
*                                                                               
         BRAS  RE,ADDREG           ADD SPOT ELEMENT                             
         JNE   NO                                                               
         XC    SWNSPT,SWNSPT                                                    
         XC    SWOSPT,SWOSPT                                                    
         J     YES                                                              
*                                                                               
NPW02    XR    RF,RF               FIND THE SPOT ELEMENT FOR THIS WEEK          
         LA    R6,BDELEM                                                        
*                                                                               
NPW04    CLI   RCODE,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RCODE,11            NON-POOL BUY ELEMENT                         
         BNE   NPW06               NO                                           
*                                                                               
         TM    LFLAG,LFSTWK+LWEEKLY  TEST WEEKLY SKED AND FIRST WEEK            
         BNO   NPW06                                                            
         CLC   RDATE,SCHSTMNP      YES-COMP DATE TO SDEF START MON              
         BL    NPW06                                                            
         CLC   RDATE,SWBENDT                                                    
         BNH   NPW10                                                            
         B     NPW06                                                            
*                                                                               
NPW06    CLC   RDATE,SWBSTDT       TEST BUY DATE IS IN THIS WEEK                
         BL    NPW08                                                            
         CLC   RDATE,SWBENDT                                                    
         BNH   NPW10                                                            
*                                                                               
NPW08    IC    RF,RLEN             NEXT BUY ELEMENT                             
         BXH   R6,RF,NPW04                                                      
*                                                                               
NPW10    CLI   SWNSPT,0            BUY ELEMENT FOUND -                          
         BE    NPW12                                                            
*                                                                               
         NI    RPCOST,X'03'        TURN OFF HIGH ORDER 6 BITS OF COST           
         ZIC   RE,SWNSPT           NON-ZERO SPOTS - CHANGE NUMBER               
         SLL   RE,2                                                             
         STC   RE,BYTE                                                          
         OC    RPCOST(1),BYTE                                                   
         XC    SWNSPT,SWNSPT                                                    
         XC    SWOSPT,SWOSPT                                                    
         J     YES                                                              
*                                                                               
NPW12    MVC   SVRDATE,RDATE                                                    
         BRAS  RE,CHKLK            CHECK FOR ANY CLT OR EST LOCK                
         JE    XIT                                                              
*                                                                               
NPW14    MVI   RCODE,FF            NO SPOTS NOW - DELETE THE ELEMENT            
         GOTO1 DELELEM,DMCB,FF                                                  
*                                                                               
         CLI   RCODE,X'10'         DELETE TRAILING ELEMENTS ALSO                
         BL    NPW16                                                            
         CLI   RCODE,X'1F'         DELETE TRAILING ELEMENTS ALSO                
         BH    NPW16                                                            
         B     NPW14                                                            
*                                                                               
NPW16    XC    SWNSPT,SWNSPT                                                    
         XC    SWOSPT,SWOSPT                                                    
         J     YES                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*=====================================================================          
* ADD/DELETE POOL SPOT ELEMENTS                                                 
* INPUT : P1    = A(WEEK TABLE ENTRY)                                           
*         LFLAG = LFSTWK FOR THE FIRST WEEK                                     
*=====================================================================          
         SPACE 1                                                                
POOLSPT  NTR1  ,                                                                
         L     R4,0(R1)                                                         
         USING SWEEKSD,R4                                                       
*********                                                                       
* NEXT 2 LINES ARE COMMENTED OUT BECAUSE IF THE ROTATION CHANGES FROM           
*  W-F  TO  TH-F, THE SPOTS REMAIN WITH THE WRONG DATE IF WE DON'T              
*********                                                                       
*****    CLC   SWOSPT,SWNSPT       OLD SPOTS=NEW SPOTS                          
*****    JE    YES                 NEITHER ADD NOR DELETE REQUIRED              
*                                                                               
         L     R2,AIO                                                           
         AHI   R2,BDELEM-BUYRECD                                                
         USING REGELEM,R2                                                       
         XR    RF,RF                                                            
         XR    R0,R0               R0=COUNT OF POOL BUY ELEMENTS FOUND          
*                                                                               
PSPT02   CLI   RCODE,0             FIND BUY ELEMENTS FOR THIS WEEK              
         BE    PSPT08                                                           
*                                                                               
         CLI   RCODE,X'0B'         POOL BUY ELEMENT                             
         BNE   PSPT06                                                           
         CLC   RDATE,SWBSTDT       BUY ELEMENT IN THIS WEEK                     
         BL    PSPT06                                                           
         CLC   RDATE,SWBENDT                                                    
         BH    PSPT06                                                           
         MVI   RCODE,FF                                                         
         AHI   R0,1                                                             
*                                                                               
PSPT04   IC    RF,RLEN             NOW DELETE ASSOCIATED ELEMENTS               
         AR    R2,RF                                                            
         CLI   RCODE,X'0C'         DELETE ALL ASSOCIATED ELEMENTS               
         BL    PSPT02                                                           
         BH    PSPT05                                                           
         TM    RSTATUS,X'80'       TEST MINUS                                   
         BZ    PSPT02              NO - LEAVE PLUS OTO'S ALONE !                
PSPT05   CLI   RCODE,X'1F'                                                      
         BH    PSPT02                                                           
         MVI   RCODE,FF                                                         
         B     PSPT04                                                           
*                                                                               
PSPT06   IC    RF,RLEN             NEXT BUY ELEMENT                             
         BXH   R2,RF,PSPT02                                                     
         DROP  R2                                                               
*                                                                               
PSPT08   GOTO1 DELELEM,DMCB,FF     ALL OLD SPOTS NOW DELETED                    
         XR    RF,RF                                                            
         IC    RF,SWOSPT                                                        
         SR    RF,R0                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STC   RF,SWOSPT           NEW COUNT OF SPOTS ON RECORD(S)              
         CLC   SWOSPT,SWNSPT       OLD SPOTS>=NEW SPOTS                         
         JNL   YES                 FINISHED                                     
*                                                                               
         LA    R2,BUYELEM                                                       
         USING REGELEM,R2                                                       
         XC    BUYELEM,BUYELEM     BUILD BUY ELEMENT                            
         MVI   RCODE,11                                                         
         MVI   RLEN,10                                                          
         MVC   RDATE,SWDATE        BUY DATE                                     
         CLI   BPRD,FF             TEST UNALLOCATED SPOTS(PRODUCT=POL)          
         BE    PSPT10              YES                                          
*                                                                               
         MVI   RLEN,14                                                          
         MVC   RPPRD,BPRD                                                       
         MVC   RPTIME,BSPOTLEN                                                  
         CLI   BINPBPRD,0          TEST FOR PIGGYBACKS                          
         BE    PSPT10              NO                                           
*                                                                               
         MVI   RLEN,18             RESET LENGTH FOR PIGGYBACK                   
         MVC   RPTIME,SCHMASLN     FIRST PRODUCT IS MASTER LENGTH               
         MVC   RPPRD+L'RPALLOC(L'RPPRD),BINPBPRD                                
         XR    RE,RE                                                            
         IC    RE,SCHTOTLN                                                      
         XR    RF,RF                                                            
         IC    RF,SCHMASLN                                                      
         SR    RE,RF               COMPUTE PIGGYBACK LENGTH                     
         STC   RE,RPTIME+L'RPALLOC                                              
         DROP  R2                                                               
*                                                                               
PSPT10   L     R2,AIO              SET HOW MANY SPOTS WILL FIT NOW              
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,13(R2)         GET RECORD LENGTH                            
         BRAS  RE,GETMAXSP         SET MAX SPOTS IN LMAXSPTS                    
         L     R0,LMAXSPTS                                                      
         LTR   R0,R0               CAN WE ADD EVEN 1 RIGHT OFF?                 
         JZ    NO                  NO, RECORD IS FULL                           
*                                                                               
         AHI   R2,BDELEM-BUYREC                                                 
         USING REGELEM,R2                                                       
*                                                                               
PSPT12   BRAS  RE,ADDREG           ADD NEW ELEMENT                              
         JNE   NO                  RECORD FULL                                  
*                                                                               
         IC    RF,SWOSPT           INCREMENT NUMBER OF SPOTS ON RECORD          
         LA    RF,1(RF)                                                         
         STC   RF,SWOSPT                                                        
*                                                                               
         CLC   SWOSPT,SWNSPT       OLD SPOTS=NEW SPOTS                          
         JE    YES                 YES - FINISHED                               
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BCT   R0,PSPT12           NO  - LOOP UNTIL NO MORE SPOTS FIT           
         J     YES                 AND EXIT                                     
         DROP  R2,R4                                                            
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO ADD A REGULAR BUY ELEMENT                                          
* AIO=A(BUY RECORD)                                                             
* BUYELEM CONTAINS BUY ELEMENT                                                  
* ON EXIT, CC=EQ IF OK, CC=NEQ FOR RECORD OVERFLOW                              
*=====================================================================          
         SPACE 1                                                                
ADDREG   NTR1  ,                                                                
         L     R2,AIO              R2=A(BUY RECORD)                             
         USING BUYRECD,R2                                                       
         LA    R3,BDELEM                                                        
         SR    R0,R0                                                            
         SR    R5,R5                                                            
         MVI   HALF,6              SET RANGE OF BRAND SPOT EL CODES             
         MVI   HALF+1,8                                                         
         TM    BITFLAG,BITFPOL     TEST POOL ESTIMATE                           
         BZ    AREG2                                                            
         MVI   HALF,11             YES-CHANGE RANGE OF EL CODES                 
         MVI   HALF+1,13                                                        
*                                                                               
AREG2    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    AREG4                                                            
         CLC   0(1,R3),HALF                                                     
         BL    AREG2                                                            
         CLC   0(1,R3),HALF+1                                                   
         BH    AREG2                                                            
         LR    R5,R3                                                            
         USING REGELEM,R3                                                       
         CLC   RDATE,BUYELEM+RDATE-REGELEM                                      
         BL    AREG2                                                            
         B     AREG8                                                            
*                                                                               
AREG4    LTR   R3,R5                                                            
         BZ    AREG10                                                           
*                                                                               
AREG6    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    AREG8                                                            
         CLI   0(R3),X'10'                                                      
         BL    AREG8                                                            
         CLI   0(R3),X'1F'                                                      
         BNH   AREG6                                                            
*                                                                               
AREG8    GOTO1 RECUP,DMCB,(0,BUYREC),BUYELEM,(C'R',(R3))                        
         CLI   8(R1),0             RECUP RETURNS ZERO ON ERROR                  
         JNE   YES                                                              
         J     NO                                                               
*                                                                               
AREG10   GOTO1 ADDELEM,DMCB,BUYELEM                                             
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R3                                                            
         EJECT                                                                  
*=====================================================================          
* ADD NON-SPOT ELEMENTS TO NEW RECORD                                           
*=====================================================================          
         SPACE 1                                                                
ADDNSPT  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,BUYDESC          ADD BUY DESCRIPTION ELEMENT                  
         JNE   NO                                                               
*                                                                               
         BRAS  RE,BLDDEMO          ADD DEMO ELEMENT                             
         BRAS  RE,GETSPL           MUST DO THIS RIGHT AFTER BLDDEMO             
*                                                                               
         CLI   SVB0PROF+9,C'Y'     TEST PURPOSE CODES REQD                      
         BNE   *+8                                                              
         BRAS  RE,BLDPURP                                                       
*                                                                               
         BRAS  RE,BLDCOM           BUILD 1-3 COMMENT ELEMENTS                   
*                                                                               
         TM    BITFLAG,BITFPOL     TEST FOR NON-POOL                            
         BO    ADNS02                                                           
         CLI   BINPBPRD,0          AND PIGGYBACKS                               
         BE    ADNS02                                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          BUILD AND ADD PIGGBACK ELEMENT               
         USING PBELEM,R6                                                        
         MVI   PBCODE,X'04'                                                     
         MVI   PBLEN,(PBPRD+L'PBPRD-PBELEM)                                     
         MVC   PBPROD,BINPBPRD                                                  
         MVC   PBEST,BEST                                                       
         XR    R0,R0                                                            
         IC    R0,SCHTOTLN                                                      
         XR    R1,R1                                                            
         IC    R1,SCHMASLN                                                      
         SR    R0,R1                                                            
         STC   R0,PBTIME                                                        
         STC   R0,PBCOST                                                        
         MVC   PBPRD,HDRPPB                                                     
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         DROP  R6                                                               
*                                                                               
ADNS02   TM    SCHINDS,SCHIORB     TEST ORBIT                                   
         BZ    ADNS04                                                           
         BRAS  RE,BLDORB                                                        
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
ADNS04   GOTO1 ADDELEM,DMCB,TRCEELEM                                            
         OC    BOOKS,BOOKS                                                      
         BZ    ADNS05                                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    RE,ELEMENT          ADD RADIO DEMO DESC ELEM FOR BOOKS           
         USING RDELEM,RE                                                        
         MVI   RDCODE,X'63'                                                     
         MVI   RDLEN,10                                                         
         MVC   RDBOOKS,BOOKS                                                    
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         DROP  RE                                                               
*                                                                               
ADNS05   OC    STAPST,STAPST       ADD CANADIAN PST ELEMENT                     
         BZ    ADNS06                                                           
         MVI   ELEMENT,X'6B'                                                    
         MVI   ELEMENT+1,12                                                     
         MVC   ELEMENT+2(10),STAPST                                             
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
ADNS06   CLI   MCLUNQ,0            ADD TRAFFIC MASTER ELEMENT                   
         BE    ADNS08                                                           
         MVI   ELEMENT,X'61'                                                    
         MVI   ELEMENT+1,6                                                      
         MVC   ELEMENT+2(2),MCLCOD                                              
         MVC   ELEMENT+4(1),MCLUNQ                                              
         MVC   ELEMENT+5(1),MCLPRD                                              
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
ADNS08   OC    SVECOST2,SVECOST2   TEST COS2 ESTIMATE                           
         BZ    ADNS10                                                           
         MVI   ELEMENT,X'73'                                                    
         MVI   ELEMENT+1,COS2LENQ                                               
         MVC   ELEMENT+2(4),CURCOS2                                             
         GOTO1 ADDELEM,DMCB,ELEMENT  USE CURRENT FACTOR                         
*                                                                               
ADNS10   LHI   RE,X'99'                                                         
         ST    RE,DMCB                                                          
         GOTO1 GETELEM,DMCB        ACTIVITY THERE ALREADY?                      
         BE    ADNS12              YES                                          
*                                                                               
         GOTO1 GETFACT,DMCB,(X'02',0),0                                         
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         OC    FAPASSWD,FAPASSWD                                                
         BZ    ADNS14                                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,X'99'                                                    
         MVI   ELEMENT+1,12                                                     
         MVC   ELEMENT+2(2),FAPASSWD                                            
         MVC   ELEMENT+4(3),FADATEB                                             
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         B     ADNS14                                                           
*                                                                               
ADNS12   XC    ELEMENT,ELEMENT     CHANGE EXISTING ELEMENT                      
         XR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R6)                                                 
*                                                                               
         GOTO1 GETFACT,DMCB,(X'02',0),0                                         
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         OC    FAPASSWD,FAPASSWD                                                
         BZ    ADNS14                                                           
         MVI   ELEMENT,X'99'                                                    
         MVI   ELEMENT+1,12                                                     
         MVC   ELEMENT+7(2),FAPASSWD  SET DATE OF LAST CHANGE                   
         MVC   ELEMENT+9(3),FADATEB                                             
         LHI   RE,X'99'                                                         
         ST    RE,DMCB                                                          
         GOTO1 DELELEM,DMCB           DO NOT WRITE X'99' IN GOTO1               
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
ADNS14   J     YES                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO BUILD AND ADD BUY DESCRIPTION ELEMENT                              
* NTRY: AIO      = A(RECORD)                                                    
*       LBADDCHA = ADD/CHANGE INDICATION                                        
* EXIT: CC EQ    = RECORD UPDATED                                               
*       CC NE    = PROBLEM BUILDING ELEMENT                                     
* BUYDATE IS CALLED TO COMPLETE BUY PERIOD AND WEEK RELATED FIELDS              
* COSTCHA IS CALLED TO CHANGE COST AMOUNTS (IF NECESSARY)                       
*=====================================================================          
         SPACE 1                                                                
BUYDESC  NTR1  BASE=*,LABEL=*                                                   
         XC    DESCELEM,DESCELEM                                                
*                                                                               
         CLI   LBADDCHA,LBADD      TEST ADDING RECORD                           
         BE    BDSC02              YES                                          
*                                                                               
         L     R2,AIO              COPY EXISTING BDELEM                         
         AHI   R2,BDELEM-BUYREC                                                 
         USING BDELEM,R2                                                        
         CLI   BDCODE,X'01'        MAKE SURE ALREADY HAVE ONE                   
         BNE   BDSC02                                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,BDLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DESCELEM(0),BDELEM                                               
*                                                                               
         GOTO1 DELELEM,DMCB,1      THEN DELETE IT                               
*                                                                               
BDSC02   LA    R2,DESCELEM         BUILD THE DESCRIPTION ELEMENT USING          
         USING BDELEM,R2               ONE WEEK & # OF WKS METHOD               
         MVI   BDCODE,X'01'                                                     
         MVI   BDLEN,NDELEM-BDELEM                                              
         MVI   BDINPUT,2                                                        
*                                                                               
         MVC   BDDAY,SCHDAY        STORE DATA INTO DESCRIPTION ELEMENT          
         CLI   HDRDAILY,C'Y'       TEST HDRDAILY SCHEDULING                     
         BE    *+10                YES-DON'T PRESET DAY NUMBERS                 
         MVC   BDSEDAY,SCHDAYNM    SET DAY NUMBERS                              
*                                                                               
         MVC   BDWKS,BNUMWKS                                                    
         MVC   BDSEC,BSPOTLEN                                                   
         MVC   BDPROGRM,SCHPROG    NO PROGRAM=SPACES                            
         MVC   BDNTAX,BTAXRATE                                                  
*                                                                               
         MVC   BDREP,SVREP         MOVE PREVIOUS REP                            
         OC    BDREP,BDREP                                                      
         BNZ   *+10                                                             
         MVC   BDREP,ESTREPCD                                                   
*                                                                               
         OC    REP,REP             TEST FOR UPLOAD SPECIAL REP                  
         BZ    *+10                                                             
         MVC   BDREP,REP           YES                                          
*                                                                               
         MVC   BDTIMST(L'BDTIMST+L'BDTIMEND),SCHSTIME                           
*                                                                               
* ADJUST 5AM BASED PC TIMES TO 0-2400 MAINFRAME SCHEME                          
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,BDTIMST        GET START TIME                               
         CHI   R1,2400                                                          
         BNH   *+8                                                              
         AHI   R1,-2400            ADJUST 1201A-5A                              
         STCM  R1,3,BDTIMST                                                     
*                                                                               
         ICM   R1,3,BDTIMEND                                                    
         CHI   R1,2400                                                          
         BNH   *+8                                                              
         AHI   R1,-2400                                                         
         STCM  R1,3,BDTIMEND                                                    
* NOW SEE IF THE TIMES ARE VALID                                                
         LHI   R0,500                                                           
         SR    RE,RE                                                            
         ICM   RE,3,BDTIMST        START TIME                                   
         CR    RE,R0               TEST PRIOR TO START OF DAY                   
         BNL   *+8                                                              
         AHI   RE,2400                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,BDTIMEND       END TIME                                     
         BZ    BDSC03                                                           
         CR    RF,R0               TEST PRIOR TO START OF DAY                   
         BNL   *+8                                                              
         AHI   RF,2400                                                          
*                                                                               
         CR    RE,RF               START OF DAY TO START TIME                   
         JH    NO                  THIS SHOULD NOT BE !                         
*                                                                               
BDSC03   BRAS  RE,BUYDATE                                                       
         JNE   NO                                                               
*                                                                               
         TM    LCHGIND,LCST        TEST FOR COST CHANGE                         
         BO    *+14                YES                                          
         MVC   BDCOST,SCHCOST                                                   
         B     BDSC04                                                           
*                                                                               
         BRAS  RE,COSTCHA          DO COST CHANGE                               
*                                                                               
BDSC04   TM    BITFLAG,BITFPOL     TEST IF POOL ESTIMATE                        
         BO    BDSC06                                                           
         CLI   BINPBPRD,0          TEST IF PIGGYBACK                            
         BE    BDSC06              NO                                           
         MVC   BDTIME,SCHMASLN                                                  
         MVC   BDCOSTP,SCHMASLN                                                 
*                                                                               
BDSC06   MVI   BDCIND,X'20'        PLAIN SPOTS = CASH SPOTS = DEFAULT           
         MVI   BDCIND2,0                                                        
*                                                                               
         MVC   BYTE,HDRRATE                                                     
         CLI   BYTE,C'0'           TEST FOR HEADER RATE TYPE OVERRIDE           
         BH    BDSC08              YES                                          
         CLI   ESTRATE,C'*'        TEST ESTIMATE SAYS NO RATE TYPE              
         BE    BDSC14                                                           
         MVC   BYTE,ESTRATE        SET RATE TYPE                                
         CLI   ESTRATE,0                                                        
         BNE   *+10                                                             
         MVC   BYTE,CPROFILE+14                                                 
*                                                                               
BDSC08   LA    RE,RATETYPS         RE=A(RATE TYPE TABLE)                        
*                                                                               
BDSC10   CLI   0(RE),0             TEST FOR EOT                                 
         BE    BDSC14                                                           
         CLC   BYTE,0(RE)          MATCH ON RATE TYPE                           
         BE    BDSC12                                                           
         LA    RE,3(RE)            NEXT TABLE ENTRY                             
         B     BDSC10                                                           
*                                                                               
BDSC12   MVC   BDCIND,1(RE)        SET COST INDICATORS FROM TABLE               
         MVC   BDCIND2,2(RE)                                                    
*                                                                               
BDSC14   CLI   LBADDCHA,LBADD                                                   
         BNE   *+8                                                              
         MVI   BDWHY,X'80'         NEW BUY CREATED TODAY                        
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,BDCHG)                                      
         OI    BDSTAT,X'08'        SO MEL KNOWS IT'S $MAD RADIO BUY             
*                                                                               
         CLI   KEYPRD,FF           IF POOL                                      
         BNE   BDSC16                                                           
         CLI   BPRD,FF             TEST PRODUCT=POL(TRUE POOL)                  
         BE    BDSC16              YES-SKIP MASTER ALLOCATION                   
*                                  NO-THEN SET POOL MASTER PRODUCT CODE         
         MVC   BDMASPRD(1),PRDCODE+1                                            
         MVC   BDMASPRD+1(1),BINPBPRD                                           
*                                                                               
BDSC16   MVC   BDDAYPT,SCHDYPRT                                                 
         TM    BITFLAG,BITFNPW     TEST BRAND POL NPW                           
         BZ    *+8                 NO                                           
         OI    BDSTAT,X'80'        THEN SET FLAGS TO USE HIGH ORDER 5           
         CLI   HDRDAILY,C'Y'       TEST HDRDAILY SCHEDULING                     
         BNE   *+8                                                              
         OI    BDSTAT2,X'80'       HDRDAILY SKED BIT                            
         TM    AFLAG1,X'02'        TEST TRADE AGENCY (NOT CTA)                  
         BZ    *+8                                                              
         OI    BDSTAT2,X'20'       SET TRADE AGENCY BUY                         
*                                                                               
         GOTO1 ADDELEM,DMCB,DESCELEM                                            
         J     YES                 RETURN OK                                    
         DROP  R2                                                               
*                                                                               
RATETYPS DC    C'1',X'0400'        S                                            
         DC    C'2',X'8000'        F                                            
         DC    C'3',X'1000'        N                                            
         DC    C'4',X'4000'        Q                                            
         DC    C'5',X'0800'        V                                            
         DC    C'6',X'0200'        X                                            
         DC    C'7',X'0000'        P                                            
         DC    C'8',X'2080'        C                                            
         DC    X'00'                                                            
         EJECT                                                                  
*=====================================================================          
* BUILD DATE RELATED BUY RECORD FIELDS                                          
* INPUT  : LAWKSTN  = A(FIRST ENTRY IN WEEKS TABLE)                             
*          LAWKENN  = A(LAST ENTRY IN WEEKS TABLE)                              
*          LNSPWKSN = N'WEEKS WITH SPOTS                                        
*          AIO      = A(BUY RECORD), WITH UPDATED BDELEM                        
* EXIT   : CC=EQ IF OK, CC=NEQ IF NOT OK                                        
*=====================================================================          
         SPACE 1                                                                
BUYDATE  NTR1  ,                                                                
         LA    R2,DESCELEM                                                      
         USING BDELEM,R2                                                        
         ICM   R6,15,LAWKSTN                                                    
         JZ    YES                                                              
         USING SWEEKSD,R6                                                       
*                                                                               
* SET BDSTART (AND PUT YYMMDD IN WORK)                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,SWDATE),(3,BDSTART)                               
         GOTO1 (RF),(R1),(3,BDSTART),WORK                                       
         DROP  R6                                                               
*                                                                               
* FOR DAILY SCHEDULING ADJUST START DATE TO START DAY (BDDAY) IF REQ            
*                                                                               
         CLI   HDRDAILY,C'Y'       HDRDAILY SKED?                               
         BNE   BDTE06              NO                                           
         GOTO1 GETDAY,(R1),WORK,WORK+6                                          
         CLC   WORK+6(3),SPACES                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    R0,R0               R0=DAY NUMBER OF START DAY                   
         IC    R0,DMCB                                                          
*                                                                               
         SR    R1,R1               GET DAY-OF-WEEK OF FIRST DAY OF DAYS         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,8,BDDAY                                                       
         BNZ   BDTE02                                                           
         DC    H'0'                                                             
*                                                                               
*        TM    SCHINDS,SCHIORB     TEST ORBIT                                   
*        BO    *+6                                                              
*        DC    H'0'                                                             
*        LA    R1,1                YES-FIRST DAY ALWAYS = MONDAY                
*        B     BDTE04                                                           
*                                                                               
BDTE02   SLDL  RE,1                FIND NUMBER OF LOWEST DAY(ON BIT)            
         LTR   RE,RE                                                            
         BNZ   BDTE04                                                           
         LA    R1,1(R1)            RESULT IS IN R1                              
         B     BDTE02                                                           
*                                                                               
BDTE04   SR    R1,R0               TEST IT'S THE SAME DAY                       
         BNM   BDTE06                                                           
*                                                                               
         ST    R1,DMCB+8           NO-BRING START DATE BACK                     
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         GOTO1 DATCON,(R1),WORK+6,(3,BDSTART)                                   
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(6),ESTSTRT     TEST BEFORE ESTIMATE START                   
         BL    BDTE99              YES-ABORT                                    
*                                                                               
* SET BDEND - END OF BUY PERIOD                                                 
* FIRST FIND MONDAY OF LAST WEEK IN SCHEDULE WITH SPOTS                         
*                                                                               
BDTE06   MVC   DUB(6),SCHSTMON     FIND BUY END DATE                            
         CLI   HDRDAILY,C'Y'       TEST HDRDAILY SCHEDULING                     
         BNE   *+10                                                             
         MVC   DUB(6),HDRSDATE     YES-USE SCHED START DATE                     
*                                                                               
         L     R6,LAWKENN          R3=A(LAST WEEK TABLE ENTRY)                  
         USING SWEEKSD,R6                                                       
         GOTO1 DATCON,DMCB,(2,SWBSTDT),(0,DUB)                                  
         DROP  R6                                                               
*                                                                               
BDTE08   CLI   HDRDAILY,C'Y'       TEST HDRDAILY SCHEDULING                     
         BNE   BDTE12              NO                                           
*                                                                               
* FOR HDRDAILY SCHEDULING, ADJUST END DATE TO END DAY(BDDAY) IF NEEDED          
*                                                                               
         GOTO1 GETDAY,DMCB,DUB,WORK+6                                           
         CLC   WORK+6(3),SPACES                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,7                R1=END DAY NUMBER                            
         SR    RF,RF               CLEAR RF FOR BIT TESTING                     
         XR    RE,RE                                                            
         IC    RE,BDDAY                                                         
*                                                                               
BDTE10   SRDL  RE,1                                                             
         LTR   RF,RF               TEST FOR HIGHEST DAY ('ON')                  
         BNZ   *+8                                                              
         BCT   R1,BDTE10                                                        
*                                                                               
         XR    RE,RE                                                            
         IC    RE,DMCB             GET POTENTIAL END DATE DAY                   
         SR    R1,RE               FIND BDDAY END DAY - END DATE DAY            
         BP    *+14                BDDAY END DAY FOLLOWS END DATE DAY           
         MVC   WORK+6(6),DUB       SET END DATE                                 
         B     BDTE18                                                           
*                                                                               
         ST    R1,DMCB+8           BRING END DATE FORWARD                       
         GOTO1 ADDAY,DMCB,DUB,WORK+6                                            
         B     BDTE18              TO AGREE WITH END DAY                        
*                                                                               
* FIND DAY NUMBER-1 OF LAST DAY IN BDDAY                                        
*                                                                               
BDTE12   LA    R1,6                SET LOOP COUNTER TO SUNDAY-1                 
*        TM    SCHINDS,SCHIORB                                                  
*        BO    BDTE14                                                           
         SR    RE,RE                                                            
         SR    RF,RF               CLEAR RF FOR BIT TESTING                     
         ICM   RE,1,BDDAY                                                       
         SR    R0,R0                                                            
         ICM   R0,1,ESTOWKSD                                                    
         BZ    BDTE14                                                           
*                                                                               
* FOR AN OUT-OF-WEEK ROTATOR, RE-ARRANGE DAY BITS AS THOUGH BIT MASK            
* STARTED WITH OUT-OF-WEEK DAY                                                  
*                                                                               
         AHI   R0,-8                                                            
         LPR   R0,R0                                                            
         SRDL  RE,1                                                             
         BCT   R0,*-4                                                           
         LR    R0,RF                                                            
         SR    RF,RF                                                            
         SRDL  RE,7                                                             
         OR    RF,R0                                                            
         SLDL  RE,7                                                             
         SR    RF,RF                                                            
*                                                                               
BDTE14   SRDL  RE,1                CALCULATE END DATE                           
         LTR   RF,RF               SHIFT OVER RIGHTMOST BIT                     
         BNZ   BDTE16              AND TEST IF ON                               
         BCT   R1,*-10                                                          
         MVC   WORK+6(6),DUB       MONDAY IS LAST DAY                           
         B     BDTE18                                                           
*                                                                               
BDTE16   ST    R1,DMCB+8           SET NUMBER OF DAYS TO ADD TO MONDAY          
         GOTO1 ADDAY,DMCB,DUB,WORK+6                                            
*                                                                               
BDTE18   GOTO1 DATCON,DMCB,WORK+6,(3,BDEND)   END DATE                          
         CLI   HDRDAILY,C'Y'       TEST FOR HDRDAILY SCHEDULING                 
         BE    BDTE34                                                           
*                                                                               
* TEST IF CALCULATED BDEND IS PAST SCHEDULE END                                 
*                                                                               
BDTE19   CLC   BDEND,SCHENDB       NO-TEST END AFTER SCHEDULE END               
         BNH   *+10                                                             
         MVC   BDEND,SCHENDB       YES-SET END TO SCHEDULE END                  
*                                                                               
         L     R6,LAWKSTN          R6 INITIALIZED TO 1ST WEEK W SPOTS           
         USING SWEEKSD,R6                                                       
         XC    HALF,HALF           INITIALIZE SPOTS/WEEK                        
         MVI   FLAG,0              INITIALIZE INTERVAL BETWEEN WEEKS            
*                                                                               
* WALK WEEK TABLE TO SEE IF SAME NUMBER OF SPOTS PER WEEK APPEARS               
* WITH SAME INTERVAL BETWEEN WEEKS WITH SPOTS                                   
*                                                                               
BDTE20   OC    SWNSPT,SWNSPT       TEST IF ANY SPOTS IN WEEK                    
         BE    BDTE24              NO                                           
         OC    BYTE,BYTE           BYTE = SPOTS/WEEK                            
         BNE   *+14                                                             
         MVC   BYTE,SWNSPT         FIRST WEEK WITH SPOTS                        
         B     BDTE22                                                           
*                                                                               
         CLC   BYTE,SWNSPT         TEST FOR CHANGE IN SPOTS/WEEK                
         BNE   BDTE26              YES                                          
*                                                                               
         LR    R0,R6               NO-COMPUTE INTERVAL BETWEEN WEEKS            
         S     R0,FULL             DISTANCE BETWEEN WEEK TABLE ENTRIES          
         SRDL  R0,32               / LENGTH OF WEEK TABLE ENTRY                 
         LHI   RF,SWEEKSLQ                                                      
         DR    R0,RF                                                            
*                                                                               
         CLI   FLAG,0              FLAG = FREQUENCY                             
         BNE   *+12                                                             
         STC   R1,FLAG             SET FIRST FREQUENCY                          
         B     BDTE22                                                           
*                                                                               
         CLM   R1,1,FLAG           TEST FOR CHANGE IN FREQUENCY                 
         BNE   BDTE26              YES                                          
*                                                                               
BDTE22   ST    R6,FULL             SAVE A(LAST WEEK WITH SPOTS)                 
*                                                                               
BDTE24   AHI   R6,SWEEKSLQ         NEXT WEEK                                    
         C     R6,LAWKENN          TEST IF PAST LAST WEEK W SPOTS               
         BNH   BDTE20              NO                                           
*                                                                               
* ONLY FALL THROUGH IF SAME NUMBER OF SPOTS APPEARS WITH SAME                   
* FREQUENCY (INTERVAL BETWEEN WEEKS)                                            
*                                                                               
         CLI   FLAG,2              TEST FOR FREQUENCY = 2-4                     
         BL    BDTE26                                                           
         CLI   FLAG,4                                                           
         BH    BDTE26                                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FLAG                                                          
         AHI   RF,-2               SUBTRACT 2 FROM FREQUENCY                    
         LA    RE,=C'ATF'                                                       
         AR    RE,RF               INDEX INTO FREQUENCY INDICATORS              
         MVC   BDWKIND,0(RE)       WEEK INDICATOR - A/T/F                       
         MVC   BDWKS,LNSPWKSN+1    ACTUAL NUMBER OF WEEKS                       
         B     BDTE28                                                           
*                                                                               
BDTE26   MVI   BDWKIND,C'O'        NO A/T/F PATTERN -                           
         ICM   RE,15,LAWKENN       SET BDWKS TO OVERALL NUMBER OF WEEKS         
         BZ    BDTE28                                                           
*                                                                               
         S     RE,LAWKSTN          DISP BETWEEN LAST/FIRST WK W SPTS            
         SRDL  RE,32                                                            
         LHI   R0,SWEEKSLQ                                                      
         DR    RE,R0                                                            
         LA    RF,1(RF)                                                         
         STC   RF,BDWKS                                                         
         DROP  R6                                                               
*                                                                               
* FIND MOST FREQUENT NUMBER OF SPOTS/WEEK                                       
*                                                                               
BDTE28   XC    ELEMENT,ELEMENT     TABLE OF SPOTS/WEEK                          
         L     R6,LAWKSTN          R3=A(FIRST WEEK W SPOTS)                     
         USING SWEEKSD,R6                                                       
         SR    RF,RF               RF=NUMBER OF SPOTS                           
         SR    RE,RE               RE=CUMULATIVE NUMBER OF SPOTS                
*                                                                               
BDTE30   ICM   RF,1,SWNSPT         GET SPOTS/WEEK                               
         IC    RE,ELEMENT(RF)      INDEX INTO FREQUENCY TABLE                   
         LA    RE,1(RE)            INCREMENT NUMBER OF OCCURRENCES              
         STC   RE,ELEMENT(RF)                                                   
         AHI   R6,SWEEKSLQ         NEXT WEEK ENTRY                              
         C     R6,LAWKENN                                                       
         BNH   BDTE30                                                           
         DROP  R6                                                               
*                                                                               
         LA    R1,ELEMENT          R1=A(HIGHEST FREQUENCY WEEK)                 
         LA    RE,ELEMENT+1        RE=TABLE INDEX REGISTER                      
         LHI   R0,MAXWEEKS         RF=LOOP COUNTER                              
         MVI   BYTE,0              MAXIMUM OCCURRENCES                          
*                                                                               
BDTE32   CLC   BYTE,0(RE)          TEST FOR NEW MAXIMUM                         
         BNL   *+12                                                             
         MVC   BYTE,0(RE)          YES-SAVE NUMBER                              
         LR    R1,RE               AND SAVE POINTER TO ENTRY                    
         LA    RE,1(RE)            NEXT WEEK                                    
         BCT   R0,BDTE32                                                        
*                                                                               
         LA    RE,ELEMENT          DISP OF MAXIMUM OCCURRENCE WEEK              
         SR    R1,RE               FROM START OF TABLE =                        
         STC   R1,BDNOWK           NUMBER OF SPOTS PER WEEK                     
         B     BDATX                                                            
*                                                                               
* HDRDAILY SCHEDULING - FIRST SET START/END DAY NUMBER BYTE                     
*                                                                               
BDTE34   MVI   BDWKIND,C'O'        HDRDAILY SKED --------------                 
         CLI   BDSEDAY,0           TEST S/E DAYS ALREADY SET                    
         BNE   BDTE38                                                           
*                                                                               
         SR    RE,RE               NO--                                         
         SR    RF,RF                                                            
         ICM   RF,1,BDDAY          TEST DAYS SET YET                            
*        BNZ   BDTE36                                                           
*        TM    SCHINDS,SCHIORB     NO-IF IT'S AN ORBIT,                         
*        BZ    BDTE38                                                           
*        LA    RF,X'7F'            ASSUME M-SU                                  
*                                                                               
BDTE36   LR    R4,RF               SAVE DAY BITS IN R4                          
         SLL   RF,24                                                            
         SR    R1,R1                                                            
         SLDL  RE,1                                                             
         LTR   RE,RE               TEST FOR FIRST 'ON' BIT=START DAY            
         BNZ   *+12                                                             
         LA    R1,16(R1)           HOB/HIGH NIBBLE OF R1 HAS DAY NUMBER         
         B     *-14                                                             
*                                                                               
         LR    R0,R1               SAVE START DAY NUMBER IN R0                  
         LR    RE,R4               RESTORE DAY BITS TO RE                       
         SR    RF,RF               RF=BIT TEST REGISTER                         
         LA    R1,7                R1=END DAY NUMBER                            
         SRDL  RE,1                FIND HIGHEST 'ON' BIT=END DAY                
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         BCTR  R1,0                                                             
         B     *-12                                                             
         OR    R1,R0               COMBINE START/END DAY NUMBER                 
         STC   R1,BDSEDAY                                                       
*                                                                               
* SET NUMBER OF WEEKS IN BUY LINE                                               
*                                                                               
BDTE38   GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         LH    RE,12(R1)           NUMBER OF DAYS/7                             
         OC    10(2,R1),10(R1)     TEST FOR ANY REMAINDER                       
         BZ    *+8                                                              
         LA    RE,1(RE)            YES-ADD ANOTHER WEEK                         
         STC   RE,BDWKS                                                         
*                                                                               
* BUILD A TABLE OF BROADCAST WEEK DATES FROM SCHEDULE START DATE                
*                                                                               
         L     R6,LAWKSTN                                                       
         USING SWEEKSD,R6                                                       
         GOTO1 DATCON,DMCB,(2,SWDATE),WORK                                      
         L     R6,LAWKENN                                                       
         GOTO1 (RF),(R1),(2,SWDATE),WORK+6                                      
         DROP  R6                                                               
*                                                                               
         XC    BLOCK,BLOCK                                                      
         MVC   BLOCK(4),GETBROAD                                                
         MVC   BLOCK+4(4),ADDAY                                                 
         MVC   BLOCK+8(4),GETDAY                                                
         MVC   BLOCK+12(4),DATCON                                               
         CLI   ESTOWKSD,0                                                       
         BE    *+10                                                             
         MVC   BLOCK+24(1),ESTOWKSD                                             
         GOTO1 MOBILE,(R1),(10,WORK),(4,WORK+12),BLOCK,BLOCK+16                 
*                                                                               
* WORK OUT FROM THE SPOTS/DAY, THE NUMBER OF SPOTS IN EACH WEEK                 
*                                                                               
BDTE40   LA    R4,WORK+12                                                       
         L     R6,LAWKSTN                                                       
         USING SWEEKSD,R6                                                       
         LA    R0,10               R0=MAXIMUM NUMBER OF WEEKS                   
         LA    R5,BLOCK            R5=SPOTS/WEEK ARRAY                          
         XC    BLOCK(16),BLOCK                                                  
         SR    RF,RF               RF=ACCUMULATOR FOR SPOTS IN WEEK             
*                                                                               
BDTE42   CLC   SWDATE,2(R4)        TEST IF DAY IS <= WEEK END DATE              
         BNH   BDTE44              YES-ITS IN THE WEEK                          
         LA    R4,4(R4)            NO-ADVANCE TO NEXT WEEK START/END            
         STC   RF,0(R5)            SAVE SPOTS IN PREVIOUS WEEK                  
         LA    R5,1(R5)            POINT TO NEXT WEEK                           
         SR    RF,RF               ZERO SPOTS ACCUMULATOR                       
         BCT   R0,*+6                                                           
         DC    H'0'                                                             
         CLI   0(R4),FF                                                         
         BNE   BDTE42                                                           
         DC    H'0'                                                             
*                                                                               
BDTE44   XR    RE,RE               GET SPOTS FOR TODAY                          
         ICM   RE,1,SWNSPT                                                      
         AR    RF,RE               UPDATE WEEK ACCUMULATOR                      
*                                                                               
         AHI   R6,SWEEKSLQ         NEXT DAY                                     
         C     R6,LAWKENN          TEST IF PAST LAST DAY                        
         BNH   BDTE42              NO                                           
         STC   RF,0(R5)                                                         
         DROP  R6                                                               
*                                                                               
* FIND MOST FREQUENT NUMBER OF SPOTS/WEEK                                       
*                                                                               
BDTE46   LA    R4,BLOCK            R4=A(SPOTS/WEEK ARRAY)                       
         LA    R0,10               R0=LOOP COUNTER                              
         SR    R3,R3               R3=NUMBER OF SPOTS                           
         SR    R5,R5               R5=CUMULATIVE NUMBER OF SPOTS                
         XC    ELEMENT,ELEMENT     TABLE OF SPOTS/WK                            
*                                                                               
BDTE48   IC    R3,0(R4)            GET SPOTS/WEEK                               
         IC    R5,ELEMENT(R3)      INDEX INTO FREQUENCY TABLE                   
         LA    R5,1(R5)            INCREMENT NUMBER OF OCCURRENCES              
         STC   R5,ELEMENT(R3)                                                   
         LA    R4,1(R4)            NEXT WEEK ENTRY                              
         BCT   R0,BDTE48                                                        
*                                                                               
         LA    R1,ELEMENT          R1=A(HIGHEST FREQUENCY WEEK)                 
         LA    RE,ELEMENT+1        RE=TABLE INDEX REGISTER                      
         LA    RF,99               RF=LOOP COUNTER                              
         MVI   BYTE,0              MAXMIMUM OCCURRENCES                         
*                                                                               
BDTE50   CLC   BYTE,0(RE)          TEST FOR NEW MAXIMUM                         
         BNL   *+12                                                             
         MVC   BYTE,0(RE)          YES-SAVE NUMBER                              
         LR    R1,RE               AND SAVE POINTER TO ENTRY                    
         LA    RE,1(RE)            NEXT WEEK                                    
         BCT   RF,BDTE50                                                        
         LA    RE,ELEMENT          DISP OF MAXIMUM OCCURRENCE WEEK              
         SR    R1,RE               FROM START OF TABLE =                        
         STC   R1,BDNOWK           NUMBER OF SPOTS PER WEEK                     
         B     BDATX                                                            
*                                                                               
BDATX    J     YES                 SET CC=EQ                                    
*                                                                               
BDTE99   MVI   BYTE,15             SET ERROR MESSAGE                            
         J     NO                                                               
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO CHANGE THE COST OF EXISTING BUY RECORD                             
* NTRY: SCHCOST = NEW COST                                                      
*=====================================================================          
         SPACE 1                                                                
COSTCHA  NTR1  ,                                                                
         MVI   BYTE,0                                                           
         L     R2,AIO                                                           
         USING BUYRECD,R2                                                       
*                                                                               
COST1    LA    R4,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
COST2    CLI   0(R4),0                                                          
         BE    COST10                                                           
         CLI   KEYPRD,FF           TEST POL BUY                                 
         BE    COST4                                                            
         CLI   0(R4),6             NO-TEST BUY ELEMENT                          
         BL    COST8                                                            
         CLI   0(R4),8                                                          
         BH    COST8                                                            
         USING REGELEM,R4                                                       
         OC    RPAY,RPAY           YES-TEST PAID                                
         BNZ   COSTX               YES-EXIT WITHOUT COST CHANGE                 
         B     COST8                                                            
*                                                                               
COST4    CLI   0(R4),11            TEST POL BUY ELEMENT                         
         BL    COST8                                                            
         CLI   0(R4),13                                                         
         BH    COST8                                                            
         TM    RSTATUS,X'20'       YES-TEST ALREADY HAS COST OVERRIDE           
         BO    COST8                                                            
         CLI   BYTE,0              NO-TEST LOOKING FOR PAID SPOTS               
         BNE   COST6                                                            
         OC    RPAY,RPAY           YES-TEST PAID                                
         BZ    COST8                                                            
         MVI   BYTE,1              YES-NOW PUT COST OVERRIDES ON UNPAID         
         B     COST1                   SPOTS                                    
*                                                                               
COST6    OC    RPAY,RPAY           TEST PAID                                    
         BNZ   COST8                                                            
         OI    RSTATUS,X'20'       SET COST OVERRIDE FLAG                       
         TM    BITFLAG,BITFNPW     TEST BRAND POL NPW                           
         BO    COST7               YES-HANDLE COST FIELD DIFFERENTLY            
         MVC   RPCOST,SCHCOST      NO-PUT COST OVERRIDE                         
         B     COST8                                                            
*                                                                               
COST7    MVC   DUB(1),RPCOST       GET HIGH ORDER BYTE OF PRESENT COST          
         NI    DUB,FF-X'03'        ISOLATE HIGH ORDER 6 BITS                    
         MVC   RPCOST,SCHCOST                                                   
         NI    RPCOST,X'03'        MAKE SURE HIGH ORDER 6 BITS ARE OFF          
         OC    RPCOST(1),DUB       COMBINE NPW BITS WITH COST                   
*                                                                               
COST8    IC    R0,1(R4)            NEXT ELEMENT                                 
         AR    R4,R0                                                            
         B     COST2                                                            
*                                                                               
COST10   CLI   KEYPRD,FF           TEST POL BUY                                 
         BNE   *+12                                                             
         CLI   BYTE,0              YES-TEST FOUND PAID SPOTS                    
         BNE   COSTX                                                            
         LA    R6,DESCELEM                                                      
         USING BDELEM,R6                                                        
         MVC   BDCOST,SCHCOST      NO-CHANGE THE COST                           
*                                                                               
COSTX    J     XIT                                                              
         DROP  R2,R4,R6                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO CHECK CLOCK/ELOCK                                                  
*=====================================================================          
         SPACE 1                                                                
CHKLK    NTR1  BASE=*,LABEL=*      CHECK CLOCK/ELOCK                            
         XC    SVLOCK,SVLOCK                                                    
*                                                                               
         OC    SVCLOCK,SVCLOCK     ANY CLIENT LOCK?                             
         BZ    CHK10                                                            
         CLC   SVRDATE,SVCLKSDT                                                 
         BL    CHK10                                                            
         CLC   SVRDATE,SVCLKNDT                                                 
         BH    CHK10                                                            
         MVI   SVLOCK,C'C'         CLIENT LOCK                                  
         J     YES                                                              
*                                                                               
CHK10    OC    SVELOCK,SVELOCK     ANY ESTIMATE LOCK?                           
         BZ    CHK20                                                            
         CLC   SVRDATE,SVELKSDT                                                 
         BL    CHK20                                                            
         CLC   SVRDATE,SVELKNDT                                                 
         BH    CHK20                                                            
         MVI   SVLOCK,C'E'         ESTIMATE LOCK                                
         J     YES                 CC = E                                       
*                                                                               
CHK20    J     NO                  NO LOCK                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* R4 POINTS TO LOCK DATE Y/M  (2)                                               
* OUTPUT LOCK START DATE AT 2(2,R4)                                             
*    AND LOCK   END DATE AT 4(2,R4)                                             
*=====================================================================          
         SPACE 1                                                                
SETLKDT  NTR1  BASE=*,LABEL=*                                                   
         L     R4,SVLKADD          WHICH LOCK ARE WE CALCULATING                
         MVC   DUB(2),0(R4)                                                     
         NI    DUB+1,X'3F'         DROP PRIOR/SUBSEQ FLAGS                      
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(3,DUB),WORK                                         
         GOTO1 GETBROAD,(R1),(1,WORK),WORK+6,GETDAY,ADDAY                       
         GOTO1 DATCON,(R1),WORK+6,(2,2(R4))                                     
         GOTO1 (RF),(R1),WORK+12,(2,4(R4))                                      
*                                                                               
         TM    1(R4),X'80'         TEST MONTH AND PRIOR                         
         BZ    *+10                                                             
         XC    2(2,R4),2(R4)       CLEAR START DATE                             
*                                                                               
         TM    1(R4),X'40'         TEST MONTH AND SUBSEQUENT                    
         BZ    *+12                                                             
         LHI   R0,-1                                                            
         STCM  R0,3,4(R4)          SET HIGH END DATE                            
         J     XIT                                                              
         EJECT                                                                  
*=====================================================================          
* SUB-ROUTINE TO CHECK FOR ANY ERRORS FOR IDENTICAL SLINE NUMBER                
* THIS DEALS WITH POSSIBILITY OF ERRORS WITH COMBOS                             
*                                                                               
* EXIT: SCHERROR SET ACCORDINGLY                                                
*=====================================================================          
         SPACE 1                                                                
CHKERR   NTR1  BASE=*,LABEL=*                                                   
         XR    R1,R1                                                            
         IC    R1,NUMERRS                                                       
         LA    RE,ERRTAB                                                        
*                                                                               
CHKERR2  CLC   SCHLNNUM,0(RE)                                                   
         BE    CHKERR4                                                          
         LA    RE,L'ERRTAB(RE)                                                  
         BCT   R1,CHKERR2                                                       
         B     CHKERRX                                                          
*                                                                               
CHKERR4  MVC   SCHERROR,4(RE)      SET ERROR NUMBER                             
*                                                                               
CHKERRX  J     XIT                                                              
         EJECT                                                                  
*=====================================================================          
* CALL GETITEM AND TRACE THE OBJECT TO THE TEMPORARY WORKER FILE                
*=====================================================================          
         SPACE 1                                                                
GETITEML NTR1  BASE=*,LABEL=*                                                   
         GOTO1 GETITEM             GET ITEM FROM INPUT FRAME                    
*                                                                               
         L     R2,ADATA            R2 = A(OBJECT DATA)                          
         L     R3,AFREE            R3 = A(MVS TYPE RECORD)                      
         XC    0(4,R3),0(R3)       CLEAR MVS LENGTH AREA                        
         AHI   R3,4                                                             
         MVC   0(L'TYPENUM,R3),TYPENUM                                          
         MVI   L'TYPENUM(R3),SPLITTER                                           
         AHI   R3,L'TYPENUM+1      MOVE IN TYPE NUMBER + SEPERATOR              
*                                                                               
         ICM   RF,15,TYPENUM       GET OBJECT NUMBER                            
         CHI   RF,ITEOD            END OF DATA?                                 
         JE    YES                 YES - EXIT                                   
         CLI   EIFFLAG,C'Y'        END OF FRAME?                                
         JE    YES                 YES - EXIT                                   
*                                                                               
         CHI   RF,ITUPLBUY         HEADER OBJECT?                               
         BE    GITMHDR                                                          
         CHI   RF,ITUPLBYS         SLINE OBJECT?                                
         BE    GITMSLN                                                          
         CHI   RF,ITSPLMKT         SPILL OBJECT?                                
         BE    GITMSPL                                                          
         B     GITMUNK             ALL UNKNOWN OBJECTS                          
*                                                                               
         USING HDROBJCT,R2         POPULATE DEBUG HEADER RECORD                 
GITMHDR  MVC   0(L'HDRMED,R3),HDRMED                                            
         MVI   L'HDRMED(R3),SPLITTER                                            
         AHI   R3,L'HDRMED+1                                                    
         MVC   0(L'HDRCLT,R3),HDRCLT                                            
         MVI   L'HDRCLT(R3),SPLITTER                                            
         AHI   R3,L'HDRCLT+1                                                    
         MVC   0(L'HDRPR1,R3),HDRPR1                                            
         MVI   L'HDRPR1(R3),SPLITTER                                            
         AHI   R3,L'HDRPR1+1                                                    
         MVC   0(L'HDRPPB,R3),HDRPPB                                            
         MVI   L'HDRPPB(R3),SPLITTER                                            
         AHI   R3,L'HDRPPB+1                                                    
         MVC   0(L'HDREST,R3),HDREST                                            
         MVI   L'HDREST(R3),SPLITTER                                            
         AHI   R3,L'HDREST+1                                                    
         MVC   0(L'HDRSRV,R3),HDRSRV                                            
         MVI   L'HDRSRV(R3),SPLITTER                                            
         AHI   R3,L'HDRSRV+1                                                    
         MVC   0(L'HDRBOOKS,R3),HDRBOOKS                                        
         MVI   L'HDRBOOKS(R3),SPLITTER                                          
         AHI   R3,L'HDRBOOKS+1                                                  
         MVC   0(L'HDRSDATE,R3),HDRSDATE                                        
         MVI   L'HDRSDATE(R3),SPLITTER                                          
         AHI   R3,L'HDRSDATE+1                                                  
         MVC   0(L'HDREDATE,R3),HDREDATE                                        
         MVI   L'HDREDATE(R3),SPLITTER                                          
         AHI   R3,L'HDREDATE+1                                                  
         MVC   0(L'HDRWKS,R3),HDRWKS                                            
         MVI   L'HDRWKS(R3),SPLITTER                                            
         AHI   R3,L'HDRWKS+1                                                    
         MVC   0(L'HDRGENTA,R3),HDRGENTA                                        
         MVI   L'HDRGENTA(R3),SPLITTER                                          
         AHI   R3,L'HDRGENTA+1                                                  
         MVC   0(L'HDRRETR,R3),HDRRETR                                          
         MVI   L'HDRRETR(R3),SPLITTER                                           
         AHI   R3,L'HDRRETR+1                                                   
         MVC   0(L'HDRPREND,R3),HDRPREND                                        
         MVI   L'HDRPREND(R3),SPLITTER                                          
         AHI   R3,L'HDRPREND+1                                                  
         MVC   0(L'HDRREPRV,R3),HDRREPRV                                        
         MVI   L'HDRREPRV(R3),SPLITTER                                          
         AHI   R3,L'HDRREPRV+1                                                  
         MVC   0(L'HDRCOST,R3),HDRCOST                                          
         MVI   L'HDRCOST(R3),SPLITTER                                           
         AHI   R3,L'HDRCOST+1                                                   
         MVC   0(L'HDRDEMO,R3),HDRDEMO                                          
         MVI   L'HDRDEMO(R3),SPLITTER                                           
         AHI   R3,L'HDRDEMO+1                                                   
*                                                                               
         LA    RE,HDRDTYPS         LOOP UNTIL END OF DEMOS                      
         LR    R1,RE                                                            
*                                                                               
GITMH02  CLC   =C'FF',0(RE)                                                     
         BE    GITMH04                                                          
         LA    RE,6(RE)                                                         
         B     GITMH02                                                          
*                                                                               
GITMH04  LA    RE,2(RE)                                                         
         SR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),HDRDTYPS    MOVE DEMOS INTO RECORD                       
*                                                                               
         LA    R3,1(RE,R3)                                                      
         LA    RE,1(RE,R1)                                                      
         MVI   0(R3),SPLITTER                                                   
         AHI   R3,1                                                             
*                                                                               
         MVC   0(L'HDRDAILY,R3),0(RE)                                           
         MVI   L'HDRDAILY(R3),SPLITTER                                          
         AHI   R3,L'HDRDAILY+1                                                  
         AHI   RE,L'HDRDAILY                                                    
         MVC   0(L'HDRRATE,R3),0(RE)                                            
         MVI   L'HDRRATE(R3),SPLITTER                                           
         AHI   R3,L'HDRRATE+1                                                   
         AHI   RE,L'HDRRATE                                                     
         MVC   0(L'HDRREP,R3),0(RE)                                             
         MVI   L'HDRREP(R3),SPLITTER                                            
         AHI   R3,L'HDRREP+1                                                    
*                                                                               
         AHI   R3,L'TYPENUM+1+4                                                 
         L     RE,AFREE                                                         
         SR    R3,RE                                                            
         STH   R3,0(RE)                                                         
         B     GITM06              PUT DEBUG RECORD TO FILE                     
*                                                                               
         USING SLD,R2              POPULATE DEBUG SLINE RECORD                  
GITMSLN  MVC   0(L'SLSEQ,R3),SLSEQ                                              
         MVI   L'SLSEQ(R3),SPLITTER                                             
         AHI   R3,L'SLSEQ+1                                                     
*                                                                               
         CLI   SLODATE,C'*'                                                     
         BNE   GITM02                                                           
         MVC   0(9,R3),SLODATE                                                  
         MVI   9(R3),SPLITTER                                                   
         MVC   10(3,R3),SLODATE+9                                               
         MVI   13(R3),SPLITTER                                                  
         AHI   R3,14                                                            
         B     GITM04                                                           
*                                                                               
GITM02   MVC   0(L'SLODATE,R3),SLODATE                                          
         MVI   L'SLODATE(R3),SPLITTER                                           
         AHI   R3,L'SLODATE+1                                                   
         MVC   0(L'SLOTIME,R3),SLOTIME                                          
         MVI   L'SLOTIME(R3),SPLITTER                                           
         AHI   R3,L'SLOTIME+1                                                   
*                                                                               
GITM04   MVC   0(L'SLSTAT,R3),SLSTAT                                            
         MVI   L'SLSTAT(R3),SPLITTER                                            
         AHI   R3,L'SLSTAT+1                                                    
         MVC   0(L'SLDAYTIM,R3),SLDAYTIM                                        
         MVI   L'SLDAYTIM(R3),SPLITTER                                          
         AHI   R3,L'SLDAYTIM+1                                                  
         MVC   0(L'SLDAYPRT,R3),SLDAYPRT                                        
         MVI   L'SLDAYPRT(R3),SPLITTER                                          
         AHI   R3,L'SLDAYPRT+1                                                  
         MVC   0(L'SLMASLEN,R3),SLMASLEN                                        
         MVI   L'SLMASLEN(R3),SPLITTER                                          
         AHI   R3,L'SLMASLEN+1                                                  
         MVC   0(L'SLLENGTH,R3),SLLENGTH                                        
         MVI   L'SLLENGTH(R3),SPLITTER                                          
         AHI   R3,L'SLLENGTH+1                                                  
         MVC   0(L'SLCOST,R3),SLCOST                                            
         MVI   L'SLCOST(R3),SPLITTER                                            
         AHI   R3,L'SLCOST+1                                                    
         MVC   0(L'SLPROG,R3),SLPROG                                            
         MVI   L'SLPROG(R3),SPLITTER                                            
         AHI   R3,L'SLPROG+1                                                    
         MVC   0(L'SLOVERS,R3),SLOVERS                                          
         MVI   L'SLOVERS(R3),SPLITTER                                           
         AHI   R3,L'SLOVERS+1                                                   
*                                                                               
         LR    RE,R3               TO                                           
         LA    R0,SLDATA           FROM                                         
         L     R1,DATALEN                                                       
         AHI   R1,-(SLDATA-SLD)                                                 
         LR    RF,R1                                                            
         AR    R3,R1               GO PAST VARIABLE STUFF                       
         MVCL  RE,R0               MOVE IN VARIABLE LENGTH DATA                 
         MVI   0(R3),SPLITTER                                                   
         AHI   R3,1                                                             
         L     RE,AFREE            PUT LENGTH IN HEADER                         
         SR    R3,RE                                                            
         STH   R3,0(RE)                                                         
         B     GITM06                                                           
*                                  POPULATE DEBUG SPILL OBJECT                  
GITMSPL  MVC   0(L'SPLMKT,R3),0(R2)                                             
         MVI   L'SPLMKT(R3),SPLITTER                                            
         AHI   R3,L'SPLMKT+1                                                    
         MVC   0(L'SPLNETWK,R3),8(R2)                                           
         MVI   L'SPLNETWK(R3),SPLITTER                                          
         AHI   R3,L'SPLNETWK+1                                                  
         MVC   0(L'SPLPURP,R3),11(R2)                                           
         MVI   L'SPLPURP(R3),SPLITTER                                           
         AHI   R3,L'SPLPURP+1                                                   
         L     RE,AFREE            PUT LENGTH IN HEADER                         
         SR    R3,RE                                                            
         STH   R3,0(RE)                                                         
         B     GITM06                                                           
*                                                                               
GITMUNK  L     R0,ADATA            PUT DATA AFTER TYPE NUMBER                   
         L     R1,DATALEN                                                       
         LA    RE,L'TYPENUM+1+4(R3)                                             
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         A     R3,DATALEN                                                       
         MVI   0(R3),SPLITTER                                                   
         AHI   R3,1                                                             
         L     RE,AFREE            PUT LENGTH IN HEADER                         
         SR    R3,RE                                                            
         STH   R3,0(RE)                                                         
         B     GITM06                                                           
*                                                                               
GITM06   GOTO1 WRKPUT,DMCB,AFREE   PUT RECORD TO WORKER FILE                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* ROUTINE PUTS OUT A TEMPORARY RECORD TO A WORKER FILE                          
*=====================================================================          
         SPACE 1                                                                
PUTTMPL  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SCHOBJCT                                                      
         CLI   PUTFLAG,PUTFSCH                                                  
         BE    PUTT02                                                           
         LA    R2,SPLOBJCT                                                      
         CLI   PUTFLAG,PUTFSPL                                                  
         BE    PUTT02                                                           
         DC    H'0'                                                             
*                                                                               
PUTT02   XR    RE,RE                                                            
         ICM   RE,3,PUTLEN                                                      
         AHI   RE,8                ADD LENGTH OF HEADER                         
         SLL   RE,16                                                            
*                                                                               
         L     R3,AFREE            R3 = A(MVS TYPE RECORD)                      
         ST    RE,0(R3)                                                         
*                                                                               
         MVC   4(4,R3),=F'9999'    PUT FAKE TYPE IN FIRST 4 BYTES               
         CLI   PUTFLAG,PUTFSCH                                                  
         BE    PUTT04                                                           
         MVC   4(4,R3),=F'9998'    PUT FAKE TYPE IN FIRST 4 BYTES               
         CLI   PUTFLAG,PUTFSPL                                                  
         BE    PUTT04                                                           
         DC    H'0'                                                             
*                                                                               
PUTT04   LA    R0,8(R3)            PUT DATA AFTER TYPE NUMBER                   
         LR    RE,R2                                                            
         XR    RF,RF                                                            
         ICM   RF,3,PUTLEN                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 WRKPUT,DMCB,AFREE   PUT RECORD TO WORKER FILE                    
*                                                                               
         XC    PUTFLAG,PUTFLAG                                                  
         XC    PUTLEN,PUTLEN                                                    
         J     XIT                                                              
         EJECT                                                                  
*=====================================================================          
* GET NEXT TEMP OBJECT FROM WORKER FILE (NWK)                                   
* IF CABLE SPILL OBJECT MUST FOLLOW SCHED OBJ                                   
* REMEMBER STUPIDO - CALLS TO WRKGET IGNORE EMBEDDED TRACE OBJECTS              
*=====================================================================          
         SPACE 1                                                                
GETTMPL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
GTL01    GOTO1 WRKGET,DMCB,AFREE   GET NEXT OBJECT                              
*                                                                               
         L     RF,AFREE            IF END OF WORKER FILE                        
         CLC   0(8,RF),=C'*EOFEOF*'                                             
         BNE   GTL02                                                            
         MVI   OBJFLAG,C'Z'        SET EOF FLAG                                 
         J     XIT                                                              
*                                                                               
GTL02    L     RF,AFREE            RF = A(MVS TYPE RECORD)                      
         LH    R1,0(RF)                                                         
         AHI   R1,-8               R1=LENGTH OF OBJECT                          
*                                                                               
         MVI   OBJFLAG,C'S'        SET SCHEDULE RECEIVED                        
         LA    R0,SCHOBJCT                                                      
         CLC   4(4,RF),=F'9999'                                                 
         BNE   GTL04                                                            
* SAVE SCHEDULE OBJECT LOCALLY                                                  
         LA    RE,8(RF)            BUMP PAST HEADER                             
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   CABLEFLG,C'Y'       IF CABLE, READ SPILL OBJ FOR NTWK            
         BE    GTL01                                                            
         J     XIT                                                              
*                                                                               
GTL04    MVI   OBJFLAG,C'P'        SET SPILL RECEIVED                           
         LA    R0,SPLOBJCT                                                      
         CLC   4(4,RF),=F'9998'                                                 
         BNE   GTL10                                                            
* SAVE SPILL OBJECT LOCALLY                                                     
         LA    RE,8(RF)                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLC   SPLPURP,SPACES                                                   
         BNH   *+10                                                             
         MVC   SVPURP,SPLPURP      SAVE PURPOSE CODE                            
         J     XIT                                                              
*                                                                               
GTL10    B     GTL01               IGNORE NON-SCHED/NON-SPILL OBJ               
         EJECT                                                                  
*=====================================================================          
* ALTER BUYLINE TABLE ENTRY POINTED TO BY AENTRY.                               
*=====================================================================          
         SPACE 1                                                                
CHGBTBL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,TRCEELEM                                                      
         USING BTRCELEM,R3                                                      
         L     R2,AENTRY           SAVE BUYLINE TABLE INFORMATION               
         USING BLND,R2                                                          
         MVC   BLNSEQ,BTRCSEQN                                                  
         MVC   BLNMAS,BPRD                                                      
         MVC   BLNPIG,BINPBPRD                                                  
         MVI   BLNSTAT,0                                                        
*                                                                               
         GOTO1 GETELEM,DMCB,5      GET PACKAGE ELEMENT                          
         BNE   CHGBL02                                                          
         USING PKGELEM,R6                                                       
         MVI   BLNPACK,BLNPMSTR                                                 
         CLI   PKGIND,2                                                         
         BNE   *+8                                                              
         MVI   BLNPACK,BLNPSLVE                                                 
*                                                                               
CHGBL02  DS    0H                                                               
*                                                                               
CHGBX    J     XIT                 RETURN TO CALLER                             
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
*=====================================================================          
* ADD STATION TO LIST OF STATIONS ADDED TO OR CHANGED IN BUY FILE               
*=====================================================================          
         SPACE 1                                                                
ADDSTA   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,LSTALIST                                                      
         LA    RF,MAXSTA                                                        
*                                                                               
ASTA02   OC    0(L'LSTALIST,R1),0(R1)                                           
         BNZ   *+14                                                             
         MVC   0(L'BMKTSTA,R1),BMKTSTA                                          
         J     XIT                                                              
*                                                                               
         CLC   BMKTSTA,0(R1)                                                    
         JE    XIT                                                              
         LA    R1,L'LSTALIST(R1)                                                
         BCT   RF,ASTA02                                                        
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* CHANGE SPILL VALUE FOR MARKET                                                 
*=====================================================================          
         SPACE 1                                                                
RESPL    NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,0              CLEAR SFEC                                   
         CLI   CABLEFLG,C'Y'                                                    
         JE    YES                                                              
         CLI   SCHODATE,C'*'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DECIN,DMCB,SPLMKT,L'SPLMKT                                       
         ICM   RF,15,0(R1)                                                      
         STCM  RF,3,SVSPLMKT                                                    
*                                                                               
         L     R2,ABLNTBL                                                       
         USING BLND,R2                                                          
         LA    R0,MAXBLNS                                                       
*                                                                               
RESP02   CLC   BLNSEQ,SCHODATE+1   COMPARE UNIQUE LINE NUMBER                   
         BE    RESP04                                                           
         AHI   R2,BLNLNQ                                                        
         BCT   R0,RESP02                                                        
         MVI   BYTE,X'55'          ERROR NUMBER???                              
         J     NO                                                               
*                                                                               
RESP04   MVC   AIO,AIO2                                                         
         MVC   NEWLINE,BLNLINE                                                  
         BRAS  RE,BLDBKEY                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUYKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         AHI   R6,BDELEM-BUYREC                                                 
         USING NDELEM,R6                                                        
         XR    RF,RF                                                            
*                                                                               
RESP06   CLI   NDCODE,0            RECORD END - ADD ELEMENT???                  
         BNE   *+12                                                             
         MVI   BYTE,X'66'                                                       
         J     NO                                                               
*                                                                               
*&&DO*&& BE    RESP14              <== IF EVER HAVE TO ADD SPILL                
*                                                                               
         CLI   NDCODE,X'03'        SPILL ELEMENT?                               
         BNE   RESP08              NO                                           
         CLC   SVSPLMKT,NDPROG     MATCH OVERRIDE MARKET                        
         BE    RESP10              TFFT                                         
*                                                                               
RESP08   IC    RF,NDLEN                                                         
         BXH   R6,RF,RESP06                                                     
*                                                                               
RESP10   MVI   DEMAD,C'N'                                                       
         BRAS  RE,BLDDEMO          BUILD 02 ELEMENT IN ELEMENT                  
*                                                                               
EL03     USING NDELEM,ELEMENT                                                   
         MVI   EL03.NDCODE,X'03'   OVERWRITE WITH SPILL SPECIFIC STUFF          
         MVC   EL03.NDPROG,NDPROG                                               
*                                                                               
         CLC   EL03.NDLEN,NDLEN    SAME LENGTH?                                 
         BNE   RESP12              NO                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,NDLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8              JUST MOVE IT OVER THE TOP                    
         B     *+10                                                             
         MVC   NDCODE(0),EL03.NDCODE                                            
         B     RESP20                                                           
*                                                                               
RESP12   LHI   RE,L'NDBOOK+L'NDPROG-1                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),NDBOOK      SAVE MATCH CHARACTERS                        
*                                                                               
* IF YOU DONT SAVE THE MATCH CHARACTERS AND PASS IT THE ADDRESS OF THE          
* STRING IN THE RECORD IT KEEPS ON MATCHING FOR ALL SUBSEQUENT ELEMENTS         
* AND DELETES THEM. THIS IS A REAL PAIN AND I FOUND IT THE HARD WAY...          
*                                                                               
         LA    R0,1(RE)                                                         
         GOTO1 DELELEM,DMCB,3,((R0),WORK)                                       
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         B     RESP20                                                           
         DROP  EL03,R6                                                          
*&&DO                                                                           
* MATCHING SPILL ELEMENT NOT FOUND - GET ORIGINAL AND ADD IT                    
*                                                                               
RESP14   L     R6,AIO                                                           
         AHI   R6,BDELEM-BUYREC                                                 
         USING NDELEM,R6                                                        
         XR    RF,RF                                                            
*                                                                               
RESP16   CLI   NDCODE,0            RECORD END - ADD ELEMENT???                  
         BNE   *+12                                                             
         MVI   BYTE,X'66'          ERROR NUMBER???                              
         J     NO                                                               
*                                                                               
         CLI   NDCODE,X'02'        DEMO ELEMENT?                                
         BE    RESP18                                                           
         IC    RF,NDLEN                                                         
         BXH   R6,RF,RESP06                                                     
*                                                                               
RESP18   IC    RF,NDLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R6)    COPY ORIGINAL DEMO ELEMENT                   
*                                                                               
         BRAS  RE,GETSPLS          ADD SPILL FOR THIS DEMO/MARKET               
         JE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
*                                                                               
RESP20   BRAS  RE,WRTBREC          RE-READ FOR UPDATE/WRITE IT BACK             
         MVI   BYTE,0              RESET ERROR FLAG                             
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*=====================================================================          
* ROUTINE TO CHECK IF THE DEMO BOOKS ARE AVAILABLE FOR THE STATION              
*=====================================================================          
         SPACE 1                                                                
CHKDMBKS NTR1  BASE=*,LABEL=*                                                   
         LA    R6,BOOKS                                                         
CKDBK10  XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         L     R0,AIO                                                           
         ST    R0,DBAREC                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELMED,HDRMED                                                  
         MVC   DBSELSRC,HDRSRV                                                  
         MVC   DBSELSTA,SCHSTA                                                  
         MVC   DBSELALF,ALPHMRKT    ALPHA MARKET FROM MARKET RECORD             
         OC    DBSELALF,SPACES                                                  
         MVC   DBSELAGY,SIGNON2C                                                
         MVC   DBSELBK,0(R6)                                                    
***      MVC   DBBTYPE,             NOT BLACK, HISPANIC, OR OLYMPIC             
         MVI   DBFUNCT,DBGETTLB                                                 
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCK,0,0                                           
         CLI   DBERROR,0           TEST FOR DEMAND ERRORS                       
         BNE   CKDBKNO                                                          
*                                                                               
         LA    R6,2(R6)            NEXT BOOK                                    
         LA    R0,BOOKS+L'BOOKS                                                 
         CR    R6,R0               PAST ALL 4 BOOKS?                            
         BNL   CKDBKYES            YES                                          
         OC    0(2,R6),0(R6)       NO, ANY BOOK HERE?                           
         BNZ   CKDBK10                 YES, CHECK THIS ONE TOO                  
*                                                                               
CKDBKYES J     YES                                                              
CKDBKNO  J     NO                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*=====================================================================          
* BUILD BUYLINE KEY FOR THIS LINE NUMBER                                        
* NTRY: NEWLINE = BUY LINE NUMBER                                               
* EXIT: KEY     = BUY KEY                                                       
*=====================================================================          
         SPACE 1                                                                
BLDBKEY  NTR1  BASE=*,LABEL=*      BUILD BUY KEY                                
         LA    R2,KEY              BUILD BUY KEY                                
         USING BUYKEY,R2                                                        
         XC    KEY,KEY                                                          
         MVC   BUYKAM,BAGYMED                                                   
         MVC   BUYKCLT,BCLT                                                     
         MVC   BUYKPRD,KEYPRD                                                   
         MVC   BUYMSTA,BMKTSTA                                                  
         MVC   BUYKEST,BEST                                                     
         MVC   BUYKBUY+1(L'NEWLINE),NEWLINE                                     
         MVI   BUYKBUY+2,X'01'                                                  
         J     XIT                 RETURN TO CALLER                             
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*=====================================================================          
* THIS ROUTINE BUILDS THE DEMOGRAPHICS ELEMENT FOR THE BUY RECORD               
*=====================================================================          
         SPACE 2                                                                
BLDDEMO  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,ELEMENT          BUILD 'ORIGINAL' DEMO ELEMENT                
         USING NDELEM,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   NDCODE,X'02'                                                     
*                                                                               
         LA    RF,ESTDEMOS         COMPUTE NUMBER OF DEMOS IN ESTIMATE          
         CLI   1(RF),0                                                          
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     *-12                                                             
         XR    RE,RE                                                            
         D     RE,=F'3'                                                         
         CHI   RF,20                                                            
         BL    *+8                                                              
         LHI   RF,20               RF HAS NUMBER OF DEMOS NOW                   
*                                                                               
         LR    R1,RF               STORE LENGTH OF OUR DEMO ELEMENT             
         SLL   R1,3                                                             
         LA    R1,NDEMNO-NDELEM(R1)                                             
         STC   R1,NDLEN                                                         
*                                                                               
         OC    NDBOOK,BOOKS        USE FIRST BOOK IN PC HEADER IF THERE         
         BNZ   *+10                                                             
         MVC   NDBOOK,ESTBOOK      DEFAULT TO EST HDR RATING BOOK               
*                                                                               
         LA    R1,NDEMNO                                                        
         BRAS  RE,BLDNDEM          BUILD NDEMNO FROM SCHED DEMOS                
*                                                                               
         XR    RE,RE               LOOK FOR FIRST NON-ZERO ENTRY                
         IC    RE,NDLEN                                                         
         AHI   RE,-8                                                            
         LA    RF,ELEMENT(RE)                                                   
BDEMO04  OC    0(8,RF),0(RF)                                                    
         BNZ   BDEMO06                                                          
         AHI   RE,-8                                                            
         CHI   RE,NDEMNO-NDELEM                                                 
         BNH   BDEMO06                                                          
         AHI   RF,-8                                                            
         B     BDEMO04                                                          
*                                                                               
BDEMO06  AHI   RE,8                                                             
         STC   RE,NDLEN                                                         
*                                                                               
         CLI   DEMAD,C'N'          DO NOT CHANGE DEMO ELEMENT                   
         BE    BDEMO08                                                          
         GOTO1 DELELEM,DMCB,2      DELETE OLD AND ADD NEW DEMO ELEMENT          
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
BDEMO08  MVI   DEMAD,C'Y'          DEFAULT IS TO CHANGE ELEMENT                 
         J     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*=====================================================================          
* BUILD THE NDEMNO FIELD OF THE BUY DEMO ELEMENT FROM THE SCHED DEMO            
* CATEGORY AND VALUE LIST                                                       
* NTRY: P1 = A(NDEMNO)                                                          
*=====================================================================          
         SPACE 1                                                                
BLDNDEM  NTR1  ,                                                                
         LR    R6,R1               R6 = A(NDEMNO)                               
         LA    R5,ESTDEMOS         R5 = A(BRD OR BRD/POL EST DEMOS)             
*                                                                               
BND10    CLI   1(R5),0             WHILE NOT END OF ESTIMATE DEMOS              
         BE    BNDX                                                             
*                                                                               
         XC    FULL,FULL           INIT DEMO VALUE TO ZERO                      
*                                                                               
         CLI   NUMDCATS,0          IF NO DEMOS IN SCHED THEN USE ZEROS          
         BE    BND90                                                            
*                                                                               
         LA    R4,DTYPLIST         R4 = A(SCHED ESTIMATE DEMOS)                 
         ZIC   R3,BNUMWKS          R3 = A(SCHED DEMO VALUES)                    
         LA    R3,SCHDATA(R3)                                                   
         ZIC   R2,NUMDCATS         R2 = # SCHED DEMO CATEGORIES                 
         SR    RF,RF                                                            
         ICM   RF,12,SCHOVERS      RF=DEMO OVERRIDE BIT MASK                    
*                                                                               
BND50    SR    RE,RE               GET BIT POSITION FOR DEMO IN RE              
         SLDL  RE,1                                                             
         CLC   0(3,R5),0(R4)       IF DEMO MATCHES ESTIMATE'S                   
         BE    BND70               THEN USE THE CORRESPONDING VALUE             
*                                                                               
         LA    R4,3(R4)            ELSE BUMP TO NEXT SCHED DEMO                 
         LA    R3,2(R3)                                                         
         BCT   R2,BND50            LOOP BACK                                    
         B     BND90               USE ZEROS FOR DEMO VALUE                     
*                                                                               
BND70    MVC   FULL+2(2),0(R3)     USE DEMO VALUE FROM SCHED                    
         CLI   SVBWPROF+8,C'Y'     TEST ALL DEMOS ARE OVERRIDES                 
         BNE   *+8                                                              
         OI    FULL,X'80'          YES-TURN ON SPOT OVERRIDE BIT                
*                                                                               
*                                  FILL NDEMNO ENTRY FOR THIS DEMO              
BND90    MVC   0(3,R6),0(R5)           DEMO CATEGORY                            
         MVI   3(R6),100               HUT ADJ (ALWAYS 100)                     
         MVC   4(4,R6),FULL            DEMO VALUE                               
*                                                                               
         LA    R5,3(R5)            BUMP TO NEXT EST DEMO                        
         LA    R6,8(R6)            BUMP TO NEXT NDEMNO ENTRY                    
         B     BND10               LOOP BACK                                    
*                                                                               
BNDX     J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO BUILD AN ORBIT ELEMENT FROM THE DAYTIME BLOCK                      
*=====================================================================          
         SPACE 1                                                                
BLDORB   NTR1  BASE=*,LABEL=*                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING ORBELEM,R6                                                       
         MVI   ORBCODE,X'67'                                                    
         MVI   ORBLEN,4                                                         
         LA    R2,SCHDT            R2=A(DAYTIME BLOCK)                          
         USING DTD,R2                                                           
         LA    R3,ORBDAY           R3=A(OUTPUT POINTER)                         
         ZIC   R4,ORBLEN           R4=CUMULATIVE ELEMENT LENGTH                 
*                                                                               
BLDORB2  CLI   DTDAY,0             TEST FOR END OF BLOCK                        
         BE    BLDORBX             YES                                          
*                                                                               
         MVC   0(1,R3),DTDAY                                                    
         MVC   1(2,R3),DTSTART                                                  
         MVC   3(2,R3),DTEND                                                    
*                                                                               
         SR    R1,R1                                                            
         LHI   R0,2400                                                          
         ICM   R1,3,1(R3)          ADJUST THE TIME FROM 5AM BASED TO 0          
         CR    R1,R0                                                            
         BNH   *+6                                                              
         SR    R1,R0                                                            
         STCM  R1,3,1(R3)                                                       
*                                                                               
         ICM   R1,3,3(R3)                                                       
         CR    R1,R0                                                            
         BNH   *+6                                                              
         SR    R1,R0                                                            
         STCM  R1,3,3(R3)                                                       
*                                                                               
         MVC   5(L'ORBDESC,R3),SPACES                                           
         LA    R3,16(R3)                                                        
         LA    R4,16(R4)                                                        
         LA    R2,DTLNQ(R2)                                                     
         ZIC   R1,ORBLEN                                                        
         LA    R1,16(R1)                                                        
         STC   R1,ORBLEN                                                        
         B     BLDORB2                                                          
*                                                                               
BLDORBX  J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R6                                                            
         EJECT                                                                  
*=====================================================================          
* READ SPILL RECORD AND ADD SPILL DEMO ELEMENTS FOR EACH MARKET                 
* NTRY:  ASSUMES ELEMENT HOLDS DEMO ELEMENT (X'02')                             
*=====================================================================          
         SPACE 1                                                                
GETSPL   NTR1  BASE=*,LABEL=*                                                   
         CLI   CABLEFLG,C'Y'                                                    
         JE    YES                                                              
*                                                                               
DEF      USING SDEFRECD,KEY        BUILD KEY FOR SPILL DEFINITION REC           
         XC    KEY,KEY                                                          
         MVC   DEF.SDEFKEY(2),=X'0D13'                                          
         MVC   DEF.SDEFKAGY,SIGNON2C   AGENCY ALPHA                             
         MVI   DEF.SDEFKRSV,C'0'       AGENCY RATING SERVICE                    
         CLI   HDRSRV,C'A'                                                      
         BNE   *+8                                                              
         MVI   DEF.SDEFKRSV,C'1'       AGENCY RATING SERVICE                    
         MVC   DEF.SDEFKSTA,SCHSTA     STATION CALL LETTERS                     
         CLI   KEY+9,C' '                                                       
         BH    *+8                                                              
         MVI   KEY+9,0                                                          
         CLI   KEY+9,C'T'          NO MEDIA IF TV                               
         BNE   *+8                                                              
         MVI   KEY+9,0                                                          
*****                                                                           
         CLI   CPROFILX+5,C'Y'      TEST US SPILL                               
         BE    *+12                                                             
         CLI   CPROFILX+5,C'D'      TEST US DPT SUMMARY SPILL                   
         BNE   GETSPLX                                                          
*****                                                                           
         CLI   CPROFILE+7,C'C'     TEST CANADIAN AGY                            
         BE    GSPL06                                                           
         CLI   HDRMED,C'R'         SKIP IF RADIO                                
         BE    GSPL06                                                           
         CLI   DEF.SDEFKRSV,C'0'   TEST RTGSVC = NSI                            
         BE    GSPL06                                                           
*                                                                               
* IF ANY SPOTS AFTER 26DEC93, FORCE RTGSVC TO NSI                               
*                                                                               
         L     R6,AIO                                                           
         AHI   R6,BDELEM-BUYRECD                                                
         USING REGELEM,R6                                                       
         XR    RF,RF                                                            
*                                                                               
GSPL02   CLI   RCODE,0                                                          
         BE    GSPL06                                                           
         CLI   RCODE,6                                                          
         BL    GSPL04                                                           
         CLI   RCODE,13                                                         
         BH    GSPL04                                                           
         CLC   RDATE,=X'BB9A'      TEST AFTER 26DEC93                           
         BNH   GSPL04                                                           
         MVI   DEF.SDEFKRSV,C'0'   AGENCY RATING SERVICE                        
         B     GSPL06                                                           
         DROP  DEF                                                              
*                                                                               
GSPL04   IC    RF,RLEN                                                          
         BXH   R6,RF,GSPL04                                                     
         DROP  R6                                                               
*                                                                               
GSPL06   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     CHECK FOR SPILL DEFINITION RECORD            
         BNE   GETSPLX             EXIT WITH CC NOT =                           
*                                                                               
         MVC   WORK(20),KEY        SAVE SPILL DEF KEY                           
         MVC   KEY+10(2),BCLT      CHECK IF CLIENT HAS EXCEPTION REC            
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     USE CLIENT RECORD IF FOUND                   
         BE    *+10                                                             
         MVC   KEY,WORK                                                         
*                                                                               
         MVC   FULL,AIO                                                         
         MVC   AIO,AADDIO          SAVE A(IO AREA)                              
         GOTO1 GETREC                                                           
         MVC   AIO,FULL            RESTORE A(IO AREA)                           
*                                                                               
         L     RF,AADDIO                                                        
         USING SDEFRECD,RF                                                      
         CLC   SDEFLEN,=AL2(256)                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R7,SDEFEL                                                        
         USING SDEFEL05,R7                                                      
         DROP  RF                                                               
*                                                                               
         MVC   BLOCK,ELEMENT       SAVE 02 DEMO ELEMENT IN BLOCK                
EL02     USING NDCODE,BLOCK        CREATE SPILL DEMO ELEMENT IN ELEMENT         
EL03     USING NDCODE,ELEMENT                                                   
*                                                                               
         MVI   EL03.NDCODE,3       SET SPILL DEMO ELEMENT CODE                  
         MVI   EL03.NDLEN,24       RESET LENGTH                                 
*                                                                               
         XR    R0,R0           *** MOVE ALL RATINGS TO SPILL ELEM               
         IC    R0,EL02.NDLEN                                                    
         AHI   R0,-24                                                           
         BNP   GSPL14                                                           
*                                                                               
         SRL   R0,3                SET FOR BCT (EACH ONE IS 8 LONG)             
         LA    R1,EL03.NDEMNO                                                   
XL03     USING NDEMNO,R1                                                        
         LA    RF,EL02.NDEMNO      POINT TO FIRST DEMO IN 02                    
XL02     USING NDEMNO,RF                                                        
*                                                                               
GSPL08   CLI   HDRMED,C'R'         RADIO SAVES SPILL FOR ALL DEMOS              
         BE    GSPL10                                                           
         CLI   XL02.NDEMNO+1,C'R'                                               
         BE    GSPL10                                                           
         CLI   XL02.NDEMNO+1,C'E'                                               
         BE    GSPL10                                                           
         B     GSPL12                                                           
*                                                                               
GSPL10   MVC   XL03.NDEMNO,XL02.NDEMNO        MOVE DEMO DESC                    
         XC    XL03.NDSVI,XL03.NDSVI          AND CLEAR REST                    
         XC    XL03.NDEMRAW,XL03.NDEMRAW                                        
*                                                                               
         AHI   R1,8                NEXT 03 ELEMENT DEMO VALUE                   
         XR    RE,RE                                                            
         IC    RE,EL03.NDLEN       INCREMENT ELEMENT LENGTH                     
         AHI   RE,8                                                             
         STC   RE,EL03.NDLEN                                                    
*                                                                               
GSPL12   AHI   RF,8                                                             
         BCT   R0,GSPL08                                                        
         DROP  XL02,XL03                                                        
*                                                                               
* ADD SPILL DEMEL FOR EACH SPILL MKT *                                          
*                                                                               
GSPL14   CLI   0(R7),0              END OF SPILL RECORD?                        
         BE    GETSPLX                                                          
         CLI   0(R7),5              SKIP IF NOT A SPILL MARKET ELEMENT          
         BNE   GSPL24                                                           
*                                                                               
         CLC   SDEFAMKT,BMKTSTA     TEST SPILL MKT = ACTUAL MKT                 
         BE    GSPL24               YES - SKIP                                  
         TM    SDEFCEX,X'80'        TEST '*' FEATURE                            
         BO    GSPL24               YES SKIP                                    
*                                                                               
         MVC   EL03.NDPROG+00(4),SDEFAMKT                                       
         MVC   EL03.NDPROG+04(1),SDEFBKTY                                       
         MVC   EL03.NDPROG+06(2),SDEFOSET                                       
         MVC   EL03.NDPROG+13(3),SDEFALPH                                       
*                                                                               
GSPL16   L     R6,AIO                                                           
         AHI   R6,BDELEM-BUYREC                                                 
         USING NDELEM,R6                                                        
         XR    RF,RF                                                            
*                                                                               
GSPL18   CLI   NDCODE,0            SEE IF MARKET IS IN RECORD ALREADY           
         BE    GSPL22                                                           
         CLI   NDCODE,3                                                         
         BNE   GSPL20                                                           
         CLC   NDPROG(2),EL03.NDPROG                                            
         BE    GSPL24                                                           
*                                                                               
GSPL20   IC    RF,NDLEN                                                         
         BXH   R6,RF,GSPL18                                                     
*                                                                               
GSPL22   GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
GSPL24   XR    RF,RF               BUMP TO NEXT SPILL MARKET                    
         IC    RF,1(R7)                                                         
         BXH   R7,RF,GSPL14                                                     
         DC    H'0'                                                             
*                                                                               
GETSPLX  BRAS  RE,BLDBKEY          RESTORE BUY KEY                              
         J     YES                                                              
         DROP  EL02,EL03,R6,R7                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* CALCULATE MAX NUMBER OF SPOTS THAT WILL FIT IN REMAINDER OF BUYREC            
* INPUT:  R1=LENGTH OF BUY RECORD                                               
* OUTPUT: R1=LMAXSPTS=MAX NUMBER OF SPOTS                                       
*=====================================================================          
         SPACE 1                                                                
GETMAXSP NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,5900                                                          
         BL    GTMAXSP1                                                         
         XR    R1,R1               NO MORE SPOTS WILL FIT                       
         B     GTMAXSPX                                                         
*                                                                               
GTMAXSP1 LNR   R1,R1                                                            
         AHI   R1,5900             <== ALLOW SPARE FOR PACKAGE                  
         SR    R0,R0                                                            
         D     R0,=F'20'           ** ALLOW 20 BYTES PER SPOT **                
GTMAXSPX ST    R1,LMAXSPTS                                                      
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*=====================================================================          
* READ SPILL RECORD AND ADD SPILL DEMO ELEMENTS FOR SPECIFIC MARKET             
* NTRY:  ASSUMES ELEMENT HOLDS DEMO ELEMENT (X'02')                             
*=====================================================================          
         SPACE 1                                                                
GETSPLS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
DEF      USING SDEFRECD,KEY        BUILD KEY FOR SPILL DEFINITION REC           
         XC    KEY,KEY                                                          
         MVC   DEF.SDEFKEY(2),=X'0D13'                                          
         MVC   DEF.SDEFKAGY,SIGNON2C   AGENCY ALPHA                             
         MVC   DEF.SDEFKRSV,HDRSRV     AGENCY RATING SERVICE                    
         MVC   DEF.SDEFKSTA,SCHSTA     STATION CALL LETTERS                     
         CLI   KEY+9,C' '                                                       
         BH    *+8                                                              
         MVI   KEY+9,0                                                          
         CLI   KEY+9,C'T'          NO MEDIA IF TV                               
         BNE   *+8                                                              
         MVI   KEY+9,0                                                          
*                                                                               
         CLI   CPROFILE+7,C'C'     TEST CANADIAN AGY                            
         BE    GSPLS06                                                          
         CLI   HDRMED,C'R'         SKIP IF RADIO                                
         BE    GSPLS06                                                          
         CLI   DEF.SDEFKRSV,C'0'   TEST RTGSVC = NSI                            
         BE    GSPLS06                                                          
         DROP  DEF                                                              
*                                                                               
* IF ANY SPOTS AFTER 26DEC93, FORCE RTGSVC TO NSI                               
*                                                                               
         L     R6,AIO                                                           
         AHI   R6,BDELEM-BUYRECD                                                
         USING REGELEM,R6                                                       
         XR    RF,RF                                                            
*                                                                               
GSPLS02  CLI   RCODE,0                                                          
         BE    GSPLS06                                                          
         CLI   RCODE,6                                                          
         BL    GSPLS04                                                          
         CLI   RCODE,13                                                         
         BH    GSPLS04                                                          
         CLC   RDATE,=X'BB9A'      TEST AFTER 26DEC93                           
         BNH   GSPLS04                                                          
         MVI   KEY+4,C'0'          FORCE RTGSVC TO NSI                          
         B     GSPLS06                                                          
*                                                                               
GSPLS04  IC    RF,RLEN                                                          
         BXH   R6,RF,GSPLS04                                                    
         DROP  R6                                                               
*                                                                               
GSPLS06  MVC   FULL,AIO            SAVE A(IO AREA)                              
         MVC   AIO,AADDIO                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     CHECK FOR SPILL DEFINITION RECORD            
         JNE   GETSPLSX            EXIT WITH CC NOT =                           
*                                                                               
         MVC   WORK(20),KEY        SAVE SPILL DEF KEY                           
         MVC   KEY+10(2),BCLT      CHECK IF CLIENT HAS EXCEPTION REC            
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     USE CLIENT RECORD IF FOUND                   
         BE    *+10                                                             
         MVC   KEY,WORK                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,FULL            RESTORE A(IO AREA)                           
*                                                                               
         L     RF,AADDIO                                                        
         USING SDEFRECD,RF                                                      
         CLC   SDEFLEN,=AL2(256)                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R7,SDEFEL                                                        
         USING SDEFEL05,R7                                                      
         DROP  RF                                                               
*                                                                               
         MVC   BLOCK,ELEMENT       SAVE 02 DEMO ELEMENT IN BLOCK                
EL02     USING NDCODE,BLOCK        CREATE SPILL DEMO ELEMENT IN ELEMENT         
EL03     USING NDCODE,ELEMENT                                                   
*                                                                               
         MVC   EL03.NDELEM(24),EL02.NDELEM                                      
         MVI   EL03.NDCODE,3       SET SPILL DEMO ELEMENT CODE                  
         MVI   EL03.NDLEN,24       RESET LENGTH                                 
*                                                                               
* MOVE ALL RATINGS TO SPILL ELEM                                                
*                                                                               
         XR    R0,R0                                                            
         IC    R0,EL02.NDLEN                                                    
         AHI   R0,-24                                                           
         BZ    GSPLS14                                                          
         SRL   R0,3                SET FOR BCT (EACH ONE IS 8 LONG)             
         LA    R1,EL03.NDEMNO                                                   
XL03     USING NDEMNO,R1                                                        
         LA    RF,EL02.NDEMNO      POINT TO FIRST DEMO IN 02                    
XL02     USING NDEMNO,RF                                                        
*                                                                               
GSPLS08  CLI   HDRMED,C'R'         RADIO SAVES SPILL FOR ALL DEMOS              
         BE    GSPLS10                                                          
         CLI   XL02.NDEMNO+1,C'R'                                               
         BE    GSPLS10                                                          
         CLI   XL02.NDEMNO+1,C'E'                                               
         BE    GSPLS10                                                          
         B     GSPLS12                                                          
*                                                                               
GSPLS10  MVC   XL03.NDEMNO,XL02.NDEMNO        MOVE DEMO DESC                    
         XC    XL03.NDSVI,XL03.NDSVI          AND CLEAR REST                    
         XC    XL03.NDEMRAW,XL03.NDEMRAW                                        
         DROP  XL02,XL03                                                        
*                                                                               
         AHI   R1,8                NEXT 03 ELEMENT DEMO VALUE                   
         XR    R0,R0                                                            
         IC    R0,EL03.NDLEN       INCREMENT ELEMENT LENGTH                     
         AHI   R0,8                                                             
         STC   R0,EL03.NDLEN                                                    
*                                                                               
GSPLS12  AHI   RF,8                                                             
         BCT   R0,GSPLS08                                                       
*                                                                               
* ADD SPILL DEMEL FOR EACH SPILL MKT *                                          
*                                                                               
GSPLS14  CLI   0(R7),0              END OF SPILL RECORD?                        
         BE    GETSPLSX                                                         
*                                                                               
         CLI   0(R7),5              SKIP IF NOT A SPILL MARKET ELEMENT          
         BNE   GSPLS24                                                          
         CLC   SVSPLMKT,SDEFAMKT    SKIP IF NOT REQUIRED SPILL MKT              
         BNE   GSPLS24                                                          
*                                                                               
         MVC   EL03.NDPROG+00(4),SDEFAMKT                                       
         MVC   EL03.NDPROG+04(1),SDEFBKTY                                       
         MVC   EL03.NDPROG+06(2),SDEFOSET                                       
         MVC   EL03.NDPROG+13(3),SDEFALPH                                       
*                                                                               
GSPLS16  L     R6,AIO                                                           
         AHI   R6,BDELEM-BUYRECD                                                
         USING NDELEM,R6                                                        
         XR    RF,RF                                                            
*                                                                               
GSPLS18  CLI   NDCODE,0            SEE IF MARKET IS IN RECORD ALREADY           
         BE    GSPLS22                                                          
         CLI   NDCODE,3                                                         
         BNE   GSPLS20                                                          
         CLC   NDPROG(2),EL03.NDPROG                                            
         BE    GSPLS24                                                          
*                                                                               
GSPLS20  IC    RF,NDLEN                                                         
         BXH   R6,RF,GSPLS18                                                    
*                                                                               
GSPLS22  GOTO1 ADDELEM,DMCB,ELEMENT                                             
         B     GETSPLSX                                                         
*                                                                               
GSPLS24  XR    RF,RF               BUMP TO NEXT SPILL MARKET                    
         IC    RF,1(R7)                                                         
         BXH   R7,RF,GSPLS14                                                    
         DC    H'0'                                                             
*                                                                               
GETSPLSX BRAS  RE,BLDBKEY          RESTORE BUY KEY                              
         J     YES                                                              
         DROP  EL02,EL03,R6,R7                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*=====================================================================          
* BUILD 1-3 COMMENT ELEMENTS IN ELEMENT AND ADD TO RECORD                       
* BUY PROGRAM HAS A MAXIMUM OF 76 BYTES PER ELEMENT                             
*=====================================================================          
         SPACE 1                                                                
BLDCOM   NTR1  BASE=*,LABEL=*                                                   
         CLI   SCHCOMLN,0          TEST IF ANY COMMENT                          
         JE    YES                 NO                                           
*                                                                               
         MVI   BYTE,1              REMOVE ANY OLD LINE 1-3 COMMENTS             
         GOTO1 SRCHDEL,DMCB,102,(1,BYTE)                                        
         MVI   BYTE,2                                                           
         GOTO1 (RF),(R1),102,(1,BYTE)                                           
         MVI   BYTE,3                                                           
         GOTO1 (RF),(R1),102,(1,BYTE)                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         CLI   SCHCOMLN,3*MAXCOM                                                
         BNH   *+8                                                              
         MVI   SCHCOMLN,3*MAXCOM                                                
         ZIC   R4,SCHCOMLN         R4=COMMENT LENGTH                            
         LA    R3,MAXCOM           R3=MAX LENGTH OF 1 BUY PROG COMMENT          
         LA    R2,1                R2=COMMENT NUMBER                            
         LA    R5,SCHCOM           R5=A(COMMENT)                                
         LA    R6,ELEMENT          R6=COMMENT ELEMENT POINTER                   
         USING COMELEM,R6                                                       
*                                                                               
BLDCOM2  MVI   CMCODE,X'66'                                                     
         MVI   CMLEN,CMDATA-CMCODE                                              
         STC   R2,CMNUM            SET COMMENT NUMBER                           
         LR    R1,R4               GET LENGTH REMAINING                         
         CR    R1,R3               TEST IF MORE THAN 1 LINE                     
         BNH   *+6                 NO                                           
         LR    R1,R3               YES-TRUNCATE LENGTH                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CMDATA(0),0(R5)                                                  
         EX    R1,*+8              FORCE COMMENT TO BE UPPER CASE               
         B     BLDCOM4                                                          
         OC    CMDATA(0),SPACES                                                 
*                                                                               
BLDCOM4  LA    R1,1(R1)            RESTORE COMMENT LENGTH                       
         ZIC   RF,CMLEN                                                         
         AR    RF,R1               COMPUTE ELEMENT LENGTH                       
         STC   RF,CMLEN                                                         
         LA    R6,CMCODE(RF)       POINT TO NEXT ELEMENT POSITION               
         LA    R5,0(R1,R5)         NEXT COMMENT POSITION                        
         LA    R2,1(R2)            INCREMENT COMMENT NUMBER                     
         SR    R4,R1               REDUCE LENGTH LEFT TO PROCESS                
         BP    BLDCOM2             MORE LEFT TO GO                              
*                                                                               
         LA    R6,ELEMENT                                                       
BLDCOM6  CLI   0(R6),0             ADD THE COMMENT ELEMENTS                     
         BE    BLDCOMX                                                          
         GOTO1 ADDELEM,DMCB,(R6)                                                
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     BLDCOM6                                                          
*                                                                               
BLDCOMX  J     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* BUILD ID ELEMENT FOR PURPOSE CODE IF REQUIRED                                 
*=====================================================================          
         SPACE 1                                                                
BLDPURP  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,X'70'                                                    
         MVI   ELEMENT+1,15                                                     
         MVI   ELEMENT+2,0                                                      
         MVC   ELEMENT+3(6),SVPURP                                              
         OC    ELEMENT+3(12),SPACES                                             
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO BUILD WEEK/SPOTS TABLE                                             
* NTRY: LBADDCHA = LBADD WHEN BUILDING NEW BUY RECORD                           
*                  LBCHA WHEN CHANGING EXISTING BUY RECORD (IN AIO)             
*       SCHSPOTS = SCHEDULE SPOTS/WEEK ARRAY                                    
*                                                                               
* EXIT: LSPTOTN  = TOTAL NUMBER OF SPOTS IN NEW UPLOAD                          
*       LSPTOTO  = TOTAL NUMBER OF SPOTS IN OLD RECORD                          
*       LAWKSTO  = A(FIRST WEEK WITH OLD SPOTS)                                 
*       LAWKSTN  = A(FIRST WEEK WITH NEW SPOTS)                                 
*       LAWKENO  = A(LAST WEEK WITH OLD SPOTS)                                  
*       LAWKENN  = A(LAST WEEK WITH NEW SPOTS)                                  
*       LNSPWKSO = NUMBER OF WEEKS WITH OLD SPOTS                               
*       LNSPWKSN = NUMBER OF WEEKS WITH NEW SPOTS                               
*       SWEEKS   = WEEKS/SPOTS TABLE                                            
*                                                                               
* CODE FOR OUT OF WEEK ROTATORS WAS REMOVED                                     
*=====================================================================          
         SPACE 1                                                                
BLDWKS   NTR1  BASE=*,LABEL=*                                                   
         MVI   LDAYDSPL,0                                                       
         XC    MASTER,MASTER                                                    
         XC    SLAVELST,SLAVELST                                                
*                                                                               
         XC    LNSPWKSO(L'LNSPWKSO*2),LNSPWKSO                                  
         LA    R0,SWEEKS           CLEAR WEEKS TABLE                            
         LHI   R1,SWEEKSL                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XR    RE,RE               FIND FIRST DAY OF SCHEDULE WEEK              
         XR    RF,RF                                                            
         ICM   RF,8,SCHDAY                                                      
         SLDL  RE,1                                                             
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,1,ESTOWKSD                                                    
         BZ    BWKS02              ITS NOT AN OUT-OF-WEEK ROTATOR               
*                                                                               
* FOR AN OUT-OF-WEEK ROTATOR-REARRANGE THE DAY BITS AS THOUGH BIT               
* PATTERN STARTED ON THE OUT-OF-WEEK DAY IN THE ESTIMATE HEADER                 
*                                                                               
         AHI   R1,-8                                                            
         LPR   R1,R1               ISOLATE BITS STARTING WITH                   
         SR    RF,RF               OUT-OF-WEEK DAY                              
         IC    RE,SCHDAY           GET SLINE DAY BIT MASK                       
         SRDL  RE,1                                                             
         BCT   R1,*-4                                                           
         LR    R1,RF               SAVE BIT PATTERN STARTING AT OWK DAY         
         SR    RF,RF               SHIFT BITS PRECEDING OWK DAY TO              
         SRDL  RE,7                RIGHTMOST POSITIONS                          
         OR    RF,R1               ATTACH THESE BITS TO RIGHT OF                
         XR    R1,R1               BITS STARTING AT OWK DAY                     
*                                                                               
BWKS02   SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BNZ   *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-14                                                             
         STC   R1,LDAYDSPL         SAVE DAY DISPLACMENT (0=MO,1=TU,ETC)         
*                                                                               
         LA    R2,SWEEKS       *** PUT DATES IN SWEEKS TABLE                    
         USING SWEEKSD,R2                                                       
*                                                                               
         LA    R5,SCHDATS                                                       
         LHI   R0,MAXWEEKS         MAXIMUM NUMBER OF WEEKS                      
*                                                                               
BWKS04   MVC   SWBSTDT(L'SWBSTDT*2),0(R5)   BRDCST WEEK ST/END DATES            
         MVC   SWDATE,0(R5)                                                     
         CLI   SWDATE,FF           END OF TABLE                                 
         BE    BWKS08                                                           
*                                                                               
         CLI   HDRDAILY,C'Y'       HDRDAILY SCHEDULING?                         
         BE    BWKS06              YES-USE ACTUAL DATE                          
*                                                                               
         XR    R1,R1               DAYS START ON MONDAY?                        
         ICM   R1,1,LDAYDSPL                                                    
         BZ    BWKS06              YES-USE MONDAY DATE                          
*                                                                               
         ST    R1,DMCB+8           NO-CALCULATE START DATE                      
         GOTO1 DATCON,DMCB,(2,(R5)),(0,WORK)                                    
         GOTO1 ADDAY,(R1),WORK,WORK                                             
         GOTO1 DATCON,(R1),(0,WORK),(2,SWDATE)                                  
*                                                                               
BWKS06   CLC   SWDATE,SWBENDT      SPOT DATE PRIOR TO ACTUAL WEEK END           
         BNH   BWKS07              YES, WE'RE GOOD                              
         MVC   SWDATE,0(R5)        NO, SAVE THE DATE FROM SCHDATS               
         AHI   R2,SWEEKSLQ                                                      
         MVC   0(6,R2),=6X'FF'     AND SET EOT AFTER THIS ENTRY                 
         B     BWKS08                                                           
*                                                                               
BWKS07   AHI   R2,SWEEKSLQ                                                      
         AHI   R5,4                                                             
         BCT   R0,BWKS04                                                        
*                                                                               
BWKS08   LA    R2,SWEEKS       *** NOW ADD NEW/RETRANS SPOTS TO TABLE           
         LA    R3,SCHSPOTS                                                      
         LA    R4,SCHTRANS                                                      
         XR    RF,RF                                                            
         XR    R0,R0                                                            
         IC    R0,BNUMWKS                                                       
*                                                                               
BWKS10   CLI   0(R2),X'FF'         TEST EOT?                                    
         BE    BWKS12X                                                          
*                                                                               
         OC    SWNSPT,0(R3)        ANY SPOTS THIS WEEK?                         
         BZ    BWKS12                                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,SWNSPT                                                        
         L     RE,LSPTOTN          UPDATE NEW SPOT TOTALS                       
         AR    RE,RF                                                            
         ST    RE,LSPTOTN                                                       
*                                                                               
BWKS12   CLI   HDRRETR,C'Y'        RETRANSFER?                                  
         BNE   *+10                NO                                           
         OC    SWRSPT,0(R4)                                                     
*                                                                               
         AHI   R2,SWEEKSLQ                                                      
         AHI   R3,1                                                             
         AHI   R4,1                                                             
         BCT   R0,BWKS10                                                        
*                                                                               
BWKS12X  MVC   FULL,AIO            SAVE AIO                                     
         MVC   SNEWLINE,NEWLINE    SAVE OUR LINE NUMBER                         
         CLI   LBADDCHA,LBCHA      CHANGE?                                      
         BNE   BWKS24              NO - NO OLD SPOTS                            
*                                                                               
BWKS13   L     R6,AIO          *** NOW ADD OLD SPOTS TO TABLE                   
         AHI   R6,BDELEM-BUYREC                                                 
         USING REGELEM,R6                                                       
*                                                                               
BWKS14   CLI   RCODE,0             END OF RECORD?                               
         BE    BWKS23              YES                                          
         CLI   RCODE,X'06'                                                      
         BE    BWKS18                                                           
         CLI   RCODE,X'07'                                                      
         BE    BWKS18                                                           
         CLI   RCODE,X'08'                                                      
         BE    BWKS18                                                           
         CLI   RCODE,X'0B'                                                      
         BE    BWKS18                                                           
         CLI   RCODE,X'0C'                                                      
         BE    BWKS18                                                           
         CLI   RCODE,X'0D'                                                      
         BE    BWKS18                                                           
         CLI   RCODE,X'10'         AFFIDAVID ELEMENT                            
         BNE   *+8                 NO                                           
         OI    LFLAG,LAFFDVT                                                    
*                                                                               
BWKS16   XR    RF,RF                                                            
         IC    RF,RLEN                                                          
         BXH   R6,RF,BWKS14                                                     
*                                  ADD ELEMENT TO CORE TABLE                    
BWKS18   LA    R2,SWEEKS           FIND WEEK FOR THIS SPOT                      
         LHI   R0,MAXWEEKS                                                      
*                                                                               
         LA    RF,SCHSTMNP         FIRST MONDAY                                 
         CLI   HDRDAILY,C'Y'       DAILY SCHEDULING?                            
         BE    *+8                 YES                                          
         LA    RF,SWDATE           FIRST DAY IN SCHEDULE                        
         CLC   RDATE,2(RF)         COMPARE TO START OF FIRST WEEK               
         BL    BWKS16              THIS SPOT BEFORE START OF SCHEDULE           
*                                                                               
BWKS20   CLI   SWDATE,FF           SWDATE <= RDATE < NEXT SWDATE                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   RDATE,SWBSTDT       SWBSTDT <= RDATE < SWBENDT                   
         BL    BWKS22                                                           
         CLC   RDATE,SWBENDT                                                    
         BH    BWKS22                                                           
*                                                                               
         XR    RE,RE               INCREMENT NUMBER OF OLD SPOTS                
         ICM   RE,1,SWOSPT                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,1,SWOSPT                                                      
*                                                                               
         L     RE,LSPTOTO          UPDATE OLD SPOT TOTALS                       
         LA    RE,1(RE)                                                         
         ST    RE,LSPTOTO                                                       
*                                                                               
         OC    RPAY,RPAY           PAID SPOT?                                   
         BZ    *+12                NO                                           
         OI    SWIND1,SWI1FRZ      FREEZE WEEK                                  
         OI    LFLAG,LPAIDSPT      WE HAVE A PAID SPOT                          
*                                                                               
         CLI   RCODE,X'07'         OTO                                          
         BNE   *+8                 NO                                           
         OI    SWIND1,SWI1FRZ      FREEZE WEEK                                  
*                                                                               
         CLI   RCODE,X'0C'         POOL OTO                                     
         BNE   *+8                 NO                                           
         OI    SWIND1,SWI1FRZ      FREEZE WEEK                                  
*                                                                               
         TM    SWIND1,SWI1FRZ                                                   
         BZ    BWKS16                                                           
         OI    LFLAG,LFREEZE       SET FROZEN WEEK                              
* WE HAVE A PROBLEM WHEN THE WEEK IS FROZEN AND ALSO IS THE START DATE          
*  OF THE BUYING PERIOD.  IF THE # OF NEW SPOTS ARE SET TO ZERO, THE            
*  FROZEN WEEK WAS NOT CONSIDERED PART OF THE BUYING PERIOD, RESULTING          
*  IN SPOTS OUTSIDE THE BUYING PERIOD.                                          
         MVC   SWNSPT,SWOSPT       DONE BECAUSE IF THE NEW SPOTS WERE           
         B     BWKS16                                                           
*                                                                               
BWKS22   AHI   R2,SWEEKSLQ                                                      
         BCT   R0,BWKS20                                                        
         DC    H'0'                                                             
*                                                                               
BWKS23   GOTO1 GETELEM,DMCB,5      IS THIS PART OF A PACKAGE?                   
         BNE   BWKS24              NO                                           
         USING PKGCODE,R6                                                       
         CLI   PKGIND,1            IS THIS THE MASTER?                          
         BNE   BWKS23A             NO                                           
*                                                                               
         MVC   MASTER,NEWLINE      SET MASTER RECORD                            
         XC    SLAVE,SLAVE         CLEAR CURRENT SLAVE                          
         XR    RF,RF                                                            
         IC    RF,PKGLEN                                                        
         AHI   RF,-4               3 FOR OVERHEAD, 1 FOR EX BELOW               
         BM    BWKS24                                                           
         EX    RF,*+8              SAVE LIST OF SLAVES FOR FURTHER USE          
         B     *+10                                                             
         MVC   SLAVELST(0),PKGLINES                                             
         DROP  R6                                                               
*                                                                               
BWKS23A  MVC   AIO,AADDIO          READ SLAVES INTO AADDIO                      
         LA    RF,SLAVELST         FIND NEXT SLAVE                              
         CLI   SLAVE,0             FIRST SLAVE?                                 
         BE    BWKS23Z             YES - DON'T SEARCH                           
*                                                                               
BWKS23B  CLI   0(RF),0             FIND CURRENT SLAVE                           
         BNE   *+6                                                              
         DC    H'0'                NOT THERE - ERROR                            
*                                                                               
         CLC   SLAVE,0(RF)         CURRENT SLAVE                                
         BE    BWKS23C                                                          
         LA    RF,1(RF)                                                         
         B     BWKS23B                                                          
*                                                                               
BWKS23C  LA    RF,1(RF)            NEXT SLAVE                                   
*                                                                               
BWKS23Z  CLI   0(RF),0             WAS THIS THE LAST SLAVE?                     
         BE    BWKS24              YES                                          
         MVC   NEWLINE,0(RF)                                                    
         MVC   SLAVE,0(RF)                                                      
         BRAS  RE,BLDBKEY                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUYKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              GET RECORD                                   
         B     BWKS13              ADD THESE SPOTS                              
*                                                                               
BWKS24   MVC   AIO,FULL            RESTORE AIO                                  
         MVC   NEWLINE,SNEWLINE    RESTORE REAL NEWLINE                         
         BRAS  RE,BLDBKEY          AND MAKE SURE KEY IS CORRECT                 
         GOTO1 HIGH                                                             
*                                                                               
         XC    LAWKSTO,LAWKSTO     CLEAR FIRST/LAST ENTRY SAVE AREAS            
         XC    LAWKSTN,LAWKSTN                                                  
         XC    LAWKENO,LAWKENO                                                  
         XC    LAWKENN,LAWKENN                                                  
         XC    LNSPWKSO(L'LNSPWKSO*2),LNSPWKSO                                  
*                                                                               
         LA    R2,SWEEKS           SET SAVE AREAS                               
         LHI   R0,MAXWEEKS                                                      
*                                                                               
BWKS26   OC    SWOSPT,SWOSPT       OLD SPOTS FOR THIS WEEK?                     
         BZ    BWKS28              NO                                           
         OC    LAWKSTO,LAWKSTO                                                  
         BNZ   *+8                                                              
         ST    R2,LAWKSTO          SET FIRST                                    
         ST    R2,LAWKENO          SET LAST                                     
         LH    RF,LNSPWKSO                                                      
         LA    RF,1(RF)                                                         
         STH   RF,LNSPWKSO         UPDATE NUMBER OF WEEKS WITH SPOTS            
*                                                                               
BWKS28   OC    SWNSPT,SWNSPT       NEW SPOTS FOR THIS WEEK?                     
         BZ    BWKS30              NO                                           
         OC    LAWKSTN,LAWKSTN                                                  
         BNZ   *+8                                                              
         ST    R2,LAWKSTN          SET FIRST                                    
         ST    R2,LAWKENN          SET LAST                                     
         LH    RF,LNSPWKSN                                                      
         LA    RF,1(RF)                                                         
         STH   RF,LNSPWKSN         UPDATE NUMBER OF WEEKS WITH SPOTS            
*                                                                               
BWKS30   AHI   R2,SWEEKSLQ                                                      
         BCT   R0,BWKS26                                                        
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO RESET WEEK/SPOTS TABLE TOTALS                                      
* NTRY: LBADDCHA = LBADD WHEN BUILDING NEW BUY RECORD                           
*                  LBCHA WHEN CHANGING EXISTING BUY RECORD (IN AIO)             
*                                                                               
* EXIT: LSPTOTN  = TOTAL NUMBER OF SPOTS IN NEW UPLOAD                          
*       LSPTOTO  = TOTAL NUMBER OF SPOTS IN OLD RECORD                          
*       LAWKSTO  = A(FIRST WEEK WITH OLD SPOTS)                                 
*       LAWKSTN  = A(FIRST WEEK WITH NEW SPOTS)                                 
*       LAWKENO  = A(LAST WEEK WITH OLD SPOTS)                                  
*       LAWKENN  = A(LAST WEEK WITH NEW SPOTS)                                  
*       LNSPWKSO = NUMBER OF WEEKS WITH OLD SPOTS                               
*       LNSPWKSN = NUMBER OF WEEKS WITH OLD SPOTS                               
*=====================================================================          
         SPACE 1                                                                
RESWKS   NTR1  BASE=*,LABEL=*                                                   
         XC    LSPTOTN,LSPTOTN                                                  
         XC    LSPTOTO,LSPTOTO                                                  
         XC    LNSPWKSN,LNSPWKSN                                                
         XC    LNSPWKSO,LNSPWKSO                                                
*                                                                               
         LHI   R0,MAXWEEKS                                                      
         LA    R2,SWEEKS                                                        
         USING SWEEKSD,R2                                                       
RWKS02   CLI   SWBSTDT,FF          END OF LIST?                                 
         BE    RWKS08              YES                                          
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,SWNSPT                                                      
         BZ    RWKS04                                                           
         A     RF,LSPTOTN                                                       
         ST    RF,LSPTOTN                                                       
*                                                                               
         LH    RF,LNSPWKSN                                                      
         AHI   RF,1                                                             
         STH   RF,LNSPWKSN                                                      
*                                                                               
RWKS04   XR    RF,RF                                                            
         ICM   RF,1,SWOSPT                                                      
         BZ    RWKS06                                                           
         A     RF,LSPTOTO                                                       
         ST    RF,LSPTOTO                                                       
*                                                                               
         LH    RF,LNSPWKSO                                                      
         AHI   RF,1                                                             
         STH   RF,LNSPWKSO                                                      
*                                                                               
RWKS06   AHI   R2,SWEEKSLQ                                                      
         BCT   R0,RWKS02                                                        
*                                                                               
RWKS08   J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*=====================================================================          
* THIS ROUTINE WRITES THE BUY RECORD TO THE FILE.                               
* NTRY: AIO2   = A(BUY RECORD)                                                  
*       KEY    = BUY DIRECTORY RECORD                                           
*=====================================================================          
         SPACE 1                                                                
KY       USING BUYKEY,KEY                                                       
*                                                                               
WRTBREC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    SVEFLAG1,EF1SDE     TEST SUPERDESK AUTH                          
         BZ    *+8                                                              
         BRAS  RE,TSTSDE           CHECK FOR SUPERB-DESK UPDATES                
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
         CLC   KY.BUYKEY,KEYSAVE   RECORD ALREADY THERE?                        
         BE    WBR02               YES                                          
*                                                                               
         MVC   AIO,AIO2            ADD IT                                       
         GOTO1 ADDREC                                                           
         B     WBRX                                                             
*                                                                               
WBR02    TM    KY.BUYRLEN,X'80'    DIRECTORY DELETED?                           
         BZ    WBR04               NO                                           
         NI    KY.BUYRLEN,255-X'80'                                             
         GOTO1 WRITE               UNDELETE AND WRITE BACK                      
         DROP  KY                                                               
*                                                                               
WBR04    MVC   AIO,AADDIO          READ OLD RECORD INTO TEMP IO AREA            
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2            RESTORE OUR RECORD                           
*                                                                               
         BRAS  RE,BLDSPTL          BUILD ADDED PRODUCT LIST                     
         GOTO1 PUTREC                                                           
         BRAS  RE,DOSPILL          MAKE SURE HAVE ALL POL SPILL PTRS            
*                                                                               
WBRX     NI    DMINBTS,FF-X'08'    RESET READ DELETED RECORDS                   
         MVI   RDUPDATE,C'N'       RESET READ FOR UPDATE                        
         J     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
*=====================================================================          
* BUILD OF LIST OF PRODUCTS IN SPOT BUYREC BEFORE PUTREC                        
*=====================================================================          
         SPACE 2                                                                
BLDSPTL  NTR1  ,                                                                
         XC    ELEMENT,ELEMENT     CLEAR LIST AREA                              
         L     R6,AIO              POINT TO BUY RECORD                          
         CLI   3(R6),FF            TEST POL BUY                                 
         JNE   XIT                 NO                                           
*                                                                               
         AHI   R6,BDELEM-BUYREC    FIRST ELEMENT                                
         USING REGELEM,R6                                                       
         XR    RF,RF                                                            
*                                                                               
BSPTL02  CLI   RCODE,0             END OF RECORD?                               
         BE    BSPTL08             YES                                          
         CLI   RCODE,X'0B'                                                      
         BL    BSPTL06                                                          
         CLI   RCODE,X'0D'                                                      
         BH    BSPTL06                                                          
*                                                                               
         IC    RF,RLEN                                                          
         AHI   RF,-10                                                           
         BNP   BSPTL06             SKIP IF NOT ALLOCATED                        
         SRL   RF,2                GIVES NUMBER OF ALLOCATIONS                  
*                                                                               
BSPTL04  LA    R2,RPALLOC          POINT TO FIRST ALLOCATION                    
         BRAS  RE,BLDSINS                                                       
         LA    R2,4(R2)            NEXT PRODUCT                                 
         BCT   RF,BSPTL04                                                       
*                                                                               
BSPTL06  XR    RF,RF                                                            
         IC    RF,RLEN                                                          
         BXH   R6,RF,BSPTL02                                                    
         DROP  R6                                                               
*                                                                               
BSPTL08  CLI   ELEMENT,0           NO ALLOCATED ELEMENTS                        
         JE    XIT                                                              
         LA    RE,ELEMENT                                                       
         ST    RE,APRDLIST         SET DM6 FOR PUTREC                           
         J     XIT                                                              
*                                                                               
BLDSINS  LA    R1,ELEMENT          TEST PRODUCT IN LIST ALREADY                 
*                                                                               
BLDSINS2 CLC   0(1,R1),0(R2)       R1 POINTS TO NEW PRODUCT                     
         BER   RE                                                               
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     BLDSINS2                                                         
*                                                                               
         MVC   0(1,R1),0(R2)       MOVE NEW PRD TO LIST                         
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* THIS ROUTINE DELETES BUY RECORD FROM FILE UNLESS IT IS A MASTER               
* NTRY: AIO2   = A(BUY RECORD)                                                  
*       KEY    = BUY DIRECTORY RECORD                                           
*=====================================================================          
         SPACE 1                                                                
DELBREC  NTR1  BASE=*,LABEL=*                                                   
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     RECORD  THERE?                               
         BNE   DELBRECX            NO - JUST EXIT                               
*                                                                               
         OI    KEY+13,X'80'        SET DIRECTORY DELETED                        
         GOTO1 WRITE               WRITE BACK                                   
*                                                                               
         MVC   AIO,AADDIO          READ OLD RECORD INTO TEMP IO AREA            
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2            RESTORE OUR RECORD                           
         L     RF,AIO                                                           
         OI    15(RF),X'80'        SET IT DELETED                               
         GOTO1 PUTREC                                                           
*                                                                               
DELBRECX NI    DMINBTS,FF-X'08'    RESET READ DELETED RECORDS                   
         MVI   RDUPDATE,C'N'       RESET READ FOR UPDATE                        
         J     XIT                 RETURN TO CALLER                             
*                                                                               
         LTORG                                                                  
*=====================================================================          
* CALL SPAUTH TO GENERATE SUPERDESK AUTHORIZATION TABLES IF NOT YET             
* BUILT, AND MAKE SURE ALL DATES IN RECORD BEING WRITTEN ARE COVERED            
* BY AN AUTHORIZATION.                                                          
* TABLE CONTAINS DATES OF AUTHORIZATIONS WITH NO STATION LEVEL RECORD           
* THAT MEANS IF THERE IS NO AUTH FOR A TIME PERIOD, WE IGNORE IT                
*=====================================================================          
         SPACE 1                                                                
TSTSDE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,SPAUTHWK         BUILD SPAUTH PARAM BLOCK                     
         XC    SPAUTHWK,SPAUTHWK                                                
         USING SPAUTHD,R1                                                       
*                                                                               
         MVC   SPAIO,AADDIO                                                     
*                                                                               
         MVC   SPACOM,ACOMFACS                                                  
         L     RE,AIO2             POINT TO BUYREC                              
         MVC   SPAKAM(1),0(RE)     A-M                                          
         MVC   SPAKCLT,1(RE)       CLT                                          
*                                                                               
         MVC   SPAKPRD,3(RE)       USE HEADLINE PRD                             
         CLI   SPAKPRD,X'FF'       TEST PRD=POL                                 
         BNE   TSTSDE2             NO - USE IT                                  
         CLI   CPROFILE+0,C'0'     TEST TRUE POL CLIENT                         
         BE    TSTSDE2                                                          
         MVC   SPAKPRD(2),BDMASPRD-BUYREC(RE)  USE MASPRDS                      
*                                                                               
TSTSDE2  CLI   SPAKPRD,0           MAKE SURE HAVE GOOD PRD                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SPAKEST,9(RE)                                                    
         MVC   SPAKMKT(5),4(RE)   MKT/STA                                       
*                                                                               
         LA    R4,SDEDATES                                                      
         ST    R4,SPAFATBL               SET DATE TABLE ADDRESS                 
*                                                                               
         CLC   SPAKEY,SDEKEY             TEST KEYS AGREE                        
         BE    TSTSDE10                                                         
         MVC   SDEKEY,SPAKEY             SAVE NEW KEY                           
*                                                                               
         OI    SPAFLAG,SPAFTBL     BUILD STA AUTH TABLE                         
         GOTO1 SPAUTH,(R1)         BUILD DATE TABLE                             
*                                                                               
* COMPARE DATES IN BIGGLESWORTH TABLE TO DATES IN BUY RECORD                    
*                                                                               
TSTSDE10 L     RE,AIO2             POINT TO BUY RECORD                          
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM-BUYREC(RE)                                             
*                                                                               
TSTSDE12 BRAS  RE,NEXTEL                                                        
         BNE   TSTSDEX                                                          
*                                                                               
         LR    RE,R4               POINT TO START OF TABLE                      
         CLC   0(2,RE),=X'FFFF'    TEST EOT                                     
         BE    TSTSDEX                                                          
*                                                                               
TSTSDE20 CLC   2(2,R6),0(RE)       SPOT TO AUTH START                           
         BL    TSTSDE22                                                         
         CLC   2(2,R6),2(RE)       SPOT TO AUTH END                             
         BNH   TSTSDE24            IN PERIOD, NEXT SPOT                         
*                                                                               
TSTSDE22 AHI   RE,4                                                             
         CLC   0(2,RE),=X'FFFF'    TEST EOT                                     
         BE    TSTSDE12                                                         
         B     TSTSDE20                                                         
*                                                                               
* NEED TO ADD STA LEVEL AUTH                                                    
*                                                                               
TSTSDE24 MVI   SPAFLAG,SPAFBUY+SPAFUPT                                          
         MVC   SPASDTE,2(R6)       SET ELEMENT DATE AS START                    
         MVC   SPAEDTE,2(R6)       AND END                                      
         GOTO1 SPAUTH,(R1)                                                      
         CLI   SPAERR,0                                                         
         BE    TSTSDE12                                                         
         DC    H'0'                                                             
*                                                                               
TSTSDEX  J     YES                                                              
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* SUBROUTINE CHECKS THAT SPOTLEN IN POL ELEMENT AGREES WITH                     
* SPOTLEN IN BUY DESC ELEMENT. IF NOT, ADJUST TO BDSEC                          
*=================================================================              
         SPACE 1                                                                
CHKSLN   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO              SEE IF ANY SPOTS ON THIS RECORD              
         USING BUYRECD,R2                                                       
         CLI   BUYKPRD,X'FF'       TEST POL BUY                                 
         BNE   CHKSLNX                                                          
*                                                                               
         LA    R6,BDELEM                                                        
         USING REGELEM,R6                                                       
*                                                                               
CHKSLN2  CLI   RCODE,0                                                          
         BE    CHKSLNX                                                          
         CLI   RCODE,X'0B'                                                      
         BL    CHKSLN20                                                         
         CLI   RCODE,X'0C'                                                      
         BH    CHKSLN20                                                         
*                                                                               
         CLI   RLEN,10             CHECK UNALLOCATED                            
         BNH   CHKSLN20                                                         
         CLI   RLEN,14             TEST SOLO                                    
         BH    CHKSLN10            HIGH IS PIGGYBACK                            
         MVC   RPTIME,BDSEC        JUST SET SLN TO BDSEC                        
         B     CHKSLN20                                                         
*                                                                               
CHKSLN10 SR    R0,R0                                                            
         IC    R0,RPTIME           GET FIRST SLN                                
         SR    RE,RE                                                            
         IC    RE,RPTIME+4         GET SECOND SLN                               
         AR    R0,RE                                                            
         CLM   R0,1,BDSEC          TEST EQUAL TO BDSEC                          
         BE    CHKSLN20                                                         
* NEED TO PROPORTION                                                            
         XR    RE,RE                                                            
         IC    RE,RPTIME           GET FIRST SLN                                
         XR    RF,RF                                                            
         IC    RF,BDSEC            GET NEW SLN                                  
         MR    RE,RE                                                            
         DR    RE,R0               DIVIDE BY OLD TOTAL SLN                      
         STC   RF,RPTIME                                                        
*                                                                               
         XR    RE,RE                                                            
         IC    RE,RPTIME+4                                                      
         XR    RF,RF                                                            
         IC    RF,BDSEC                                                         
         MR    RE,RE                                                            
         DR    RE,R0                                                            
         STC   RF,RPTIME+4                                                      
*                                                                               
CHKSLN20 XR    RF,RF                                                            
         IC    RF,RLEN                                                          
         BXH   R6,RF,CHKSLN2                                                    
*                                                                               
CHKSLNX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
         SPACE 1                                                                
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    TWAACCS,TWAACCS          OR HAVE LIMIT ACCESS                    
         JZ    YES                                                              
         DROP  RE                                                               
*                                                                               
         L     R0,AIO                                                           
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,ACOMFACS         INITIALIZE SECRET                            
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',AIO),0                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R1,BLOCK                                                         
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         MVC   OFCAUTH,TWAACCS     SET AUTH CODE PASSED TO US                   
         MVC   OFCLMT(4),TWAACCS   4 CHAR VERSION TOO                           
         DROP  RE                                                               
         MVC   OFCAGY,SIGNON2C                                                  
         MVC   OFCOFC,SVOFFC                                                    
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,BAGYMED                                                 
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKTREC                   
         MVC   OFCSECD,AIO                                                      
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',BLOCK),ACOMFACS                                  
         CLI   0(R1),0                                                          
         JE    YES                                                              
*                                                                               
         MVI   BYTE,27             SET NO CLT ACCESS                            
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         CLI   TWAACCS,C'+'        TEST LIMIT ACCESS BY MARKET                  
         BNE   *+8                                                              
         MVI   BYTE,28             SET NO MKT ACCESS                            
         J     ABORT                                                            
         DROP  RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* MAKE SURE ALL POL SPILL POINTERS EXIST                                        
*=====================================================================          
         SPACE 2                                                                
DOSPILL  NTR1  BASE=*,LABEL=*                                                   
         XC    BUYELEM,BUYELEM     CLEAR KEY SAVE AREA                          
         L     R6,AIO              POINT TO BUY RECORD                          
*                                                                               
         AHI   R6,BDELEM-BUYREC    FIRST ELEMENT                                
         USING NDELEM,R6                                                        
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
*                                                                               
DOSPIL2  BRAS  RE,NEXTEL                                                        
         BNE   DOSPILX                                                          
         OC    BUYELEM,BUYELEM                                                  
         BNZ   *+10                                                             
         MVC   BUYELEM,KEY         SAVE CURRENT BUY KEY                         
*                                                                               
         L     RE,AIO                                                           
         MVC   KEY(10),0(RE)       A-M/CLT/PRD/MKT/STA/EST                      
         MVC   KEY+4(2),4(R6)      AGY SPILL MKT NUMBER                         
         MVI   KEY+10,X'80'        SPILL FLAG                                   
         MVC   KEY+11(1),10(RE)    LINE NUMBER                                  
         MVI   KEY+12,1                                                         
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR',KEY,KEY                        
         B     DOSPIL2                                                          
*                                                                               
DOSPILX  OC    BUYELEM,BUYELEM     TEST CHANGED KEY                             
         BZ    DOSPILX2                                                         
         MVC   KEY(13),BUYELEM                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
DOSPILX2 XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=====================================================*                         
* ADD DARE BATCH POINTERS IF THE AGENCY USES THEM     *                         
*=====================================================*                         
         SPACE 1                                                                
DARBATCH NTR1  BASE=*,LABEL=*                                                   
         CLI   SVDARPRF+4,C'Y'     USING DARE BATCH ORDERING?                   
         BNE   DBTCHX              NO, NOTHING TO DO HERE                       
*                                                                               
         XC    KEY,KEY             SEE IF WE HAVE A BATCH ORDER ALREADY         
         LA    R6,KEY                                                           
         USING DBTKEY,R6                                                        
*                                                                               
         MVI   DBTKTYP,DBTKTYPQ                                                 
         MVI   DBTKSTYP,DBTKSTYQ                                                
         MVC   DBTKAGMD,BAGYMED                                                 
         MVC   DBTKMKT(L'BUYMSTA),BMKTSTA                                       
         MVC   DBTKCLT,BCLT                                                     
         MVC   DBTKEST,BEST                                                     
         MVC   DBTKPRD,PRDCODE+1   YES                                          
         MVC   DBTKPRD2,BINPBPRD                                                
         CLI   DBTKPRD,X'00'       IF NO PRODUCT ALLOCATED                      
         BNE   *+8                                                              
         MVI   DBTKPRD,X'FF'       THEN POL                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DBTCHX                                                           
*                                                                               
DBTCH10  LA    R6,ELEMENT          BUILD RECORD IN ELEMENT                      
         XC    0(256,R6),0(R6)                                                  
         MVC   0(13,R6),KEYSAVE                                                 
         MVC   13(2,R6),=AL2(DBTRFRST-DBTKEY+DBINFLNQ)                          
*                                                                               
         LA    R6,24(R6)               CREATE AN INFO ELEMENT                   
         USING DBINFELD,R6                                                      
*                                                                               
         MVI   DBINFEL,DBINFELQ                                                 
         MVI   DBINFLEN,DBINFLNQ                                                
         GOTO1 DATCON,DMCB,(5,0),(19,DBINFDTC),0  FOR TODAY                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'SPTFILE',KEY+14,ELEMENT,     X        
               DMWORK                                                           
*                                                                               
DBTCHX   XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*============================================================                   
* INTERFACE TO SPGETBUBL TO SAVE BUYER NAME                                     
*============================================================                   
         SPACE 1                                                                
GOGETBU  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVAPROF+14,C'Y'     TEST BUYER/BILLER SAVED                      
         BNE   GETBUX                                                           
*                                                                               
         CLI   HDRBUYER,C' '       TEST BUYER NAME PRESENT                      
         BNH   GETBUX                                                           
*                                                                               
GETBU2   XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING GETBUBLD,R4                                                      
*                                                                               
         MVC   GBCOMFAC,ACOMFACS                                                
         MVC   GBIOA,AIO                                                        
* BUILD A DUMMY FLDHDR                                                          
         XC    WORK,WORK                                                        
         MVI   WORK,20                                                          
         MVC   WORK+8(12),HDRBUYER                                              
         OC    WORK+8(12),SPACES                                                
         LA    RE,WORK+19          POINT TO LAST CHAR                           
         LHI   RF,12                                                            
GETBU4   CLI   0(RE),C' '                                                       
         BH    GETBU6                                                           
         BCTR  RE,0                                                             
         BCT   RF,GETBU4                                                        
         DC    H'0'                                                             
*                                                                               
GETBU6   STC   RF,WORK+5                                                        
         LA    RE,WORK                                                          
         ST    RE,GBNAMFLD                                                      
*                                                                               
         MVC   GBAGY,SIGNON2C                                                   
         MVC   GBMEDEBC,QMED                                                    
         MVC   GBCLTEBC,QCLT                                                    
         MVC   GBOFFICE,SVOFFC                                                  
         MVC   GBAGYMD,BAGYMED                                                  
         MVC   GBCLT,BCLT                                                       
         MVC   GBPRD,KEYPRD                                                     
         MVC   GBEST,BEST                                                       
         MVC   GBMKT(5),BMKTSTA                                                 
         MVI   GBTYPE,C'B'         SET FOR BUYER NAME                           
* GET ADDRESS OF CORE RESIDENT GETBUBL *                                        
         GOTO1 CALLOV,DMCB,0,X'D9000A77'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R4)                                                   
         CLI   GBERR,0                                                          
         JNE   INVLBYR                                                          
*                                                                               
GETBUX   CR    RB,RB               EXIT WITH CC EQ                              
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* OVERLAY WORKING STORAGE                                                       
*=====================================================================          
         SPACE 1                                                                
OVERD    DSECT                                                                  
APRELO   DS    A                                                                
APBASE1  DS    A                                                                
APBASE2  DS    A                                                                
SAVERD   DS    A                   RD UPON ENTRY TO OVERLAY                     
ACONTROL DS    A                   A(CONTD)                                     
AENTRY   DS    A                   A(ENTRY) FOR BUYLINE                         
RECUP    DS    V                   V(RECUP)                                     
DAYUNPK  DS    V                   V(DAYUNPK)                                   
SPAUTH   DS    V                                                                
PERVERT  DS    V                                                                
NUMBKEYS DS    A                   NUMBER OF DIFFERENT BUY KEYS                 
ABLNTBL  DS    A                   A(BUYLINE TABLE)                             
CURCOS2  DS    F                                                                
*                                                                               
SETSENT  DS    C                   SET WORKER FILE TO SENT (Y/N)                
SPILMODE DS    C                   PROCESSING SPILL RECORDS                     
*                                                                               
*                   BINARY EQUIVALENTS                                          
*                                                                               
KEYPRD   DS    XL1                 PRODUCT CODE TO BE IN THE KEY                
BNUMWKS  DS    XL1                 # OF WEEKS WITHIN ESTIMATE PERIOD            
BPRVWKS  DS    XL1                 # OF WEEKS AS OF LAST TRANSFER               
BDAY1    DS    XL1                 BUY START DAY                                
DEMAD    DS    XL1                                                              
*                                                                               
*                   ELEMENTS                                                    
*                                                                               
DESCELEM DS    XL256               DESCRIPTION ELEMENT                          
BUYELEM  DS    XL18                BUY ELEMENT                                  
TRCEELEM DS    XL(BTRCLENQ)        TRACE ELEMENT                                
*                                                                               
*                   MISCELLANEOUS                                               
*                                                                               
SPACES   DS    CL80                BLANKS FILLED FIELD                          
NUMSCHED DS    XL1                 NUMBER OF SCHEDULE OBJECTS                   
NUMWEEKS DS    XL1                 NUMBER OF WEEKS USED FOR LOOP                
FLAG     DS    XL1                                                              
BITFLAG  DS    XL1                 OBJECT FLAG                                  
BITFPOL  EQU   X'80'               POOL PRODUCT                                 
BITFNPW  EQU   X'40'               BRAND POOL NPW                               
BITF1SL  EQU   X'02'               AT LEAST 1 SLINE PROCESSED                   
BITFEOD  EQU   X'01'               END-OF-DATA OBJECT READ                      
*                                                                               
NUMERRS  DS    XL1                 SLINE ERROR COUNT                            
PARTKEY  DS    CL(L'BUYKEY-L'BUYKBUY)   PARTIAL MASTER KEY                      
NEWLINE  DS    XL1                 BUYLINE FOR RECORD                           
SNEWLINE DS    XL1                 BUYLINE FOR RECORD                           
NUMDCATS DS    XL1                 NUMBER OF VALID DATA CATEGORIES              
BINODATE DS    XL3                 ORIGINAL TRANSFER DATE IN BINARY             
ENDSCHED DS    CL1                 NO MORE SCHEDULES (Y/N)                      
BINPBPRD DS    XL1                 PIGGYBACK PRODUCT CODE                       
BOOKS    DS    XL8                 BOOKS4| - YEAR/MONTH                        
MASTER   DS    XL1                                                              
SLAVE    DS    XL1                                                              
ESTRATE  DS    XL2                 ESTIMATE RATE TYPE                           
CONFLAG  DS    CL1                 Y/N=CONFIRM OBJECT PUTITEM PENDING           
SVRFPGRP DS    CL8                 CLIENT RFP REQUEST GROUP                     
SVBWPROF DS    CL16                                                             
SV1WPROF DS    CL16                                                             
SVB0PROF DS    CL16                                                             
SVDARPRF DS    CL16                                                             
SVMKPROF DS    CL16                                                             
SVAPROF  DS    CL20                FROM SPOT AGENCY HEADER                      
*                                                                               
SVESTBTY DS    CL1                 BOOKTYPE IN ESTIMATE RECORD                  
SVSTABTY DS    CL1                 BOOKTYPE IN STATION RECORD                   
SVMKTMKT DS    CL3                 ALPHA MARKET FROM MARKET RECORD              
SVSTAMKT DS    CL3                 ALPHA MARKET FROM STATION RECORD             
SVELOCK  DS    XL2                 ESTIMATE LOCK                                
SVELKSDT DS    XL2                                                              
SVELKNDT DS    XL2                                                              
SVCLOCK  DS    XL2                 CLIENT LOCK                                  
SVCLKSDT DS    XL2                                                              
SVCLKNDT DS    XL2                                                              
SVLKADD  DS    F                                                                
SVECOST2 DS    F                                                                
SVLOCK   DS    CL1                                                              
SVRDATE  DS    XL2                                                              
SVPURP   DS    CL6                                                              
*                                                                               
SVEFLAG1 DS    XL1                 EFLAG1                                       
*                                                                               
*                                                                               
PUTFLAG  DS    X                   DEFINE OBJECT BEING ADDED AT PUTTMPL         
PUTFSCH  EQU   C'C'                                                             
PUTFSPL  EQU   C'P'                                                             
PUTLEN   DS    XL2                 LENGTH OF OBJECT BEING ADDED                 
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
PWFLAG   DS    XL1                                                              
PWMKT    DS    CL4                                                              
SVOFFC   DS    CL1                                                              
SVCACCS  DS    CL3                                                              
SVMACCS  DS    CL3                                                              
SVSPLMKT DS    XL2                                                              
SVOLDMKT DS    XL4                 SAVED OLD MARKETS                            
SPAUTHWK DS    XL64                                                             
*                                                                               
REP      DS    XL2                 SPECIAL REP                                  
SVREP    DS    XL2                 SPECIAL REP FROM GETREC                      
SVBRDREP DS    XL2                 SPECIAL REP FROM THE BRAND ESTIMATE          
SVBRDRTY DS    CL1                 RATE TYPE FROM ESTIMATE                      
AFLAG1   DS    XL1                 FLAG1 FROM AGYHDR                            
CABLEFLG DS    C                   C'Y' FOR CABLE/HDRMED SET TO C'T'            
OBJFLAG  DS    C                                                                
DTYPLIST DS    (MAXDEMOS+1)XL3     DEMO TYPE LIST AND ROOM FOR X'FF'            
*                                                                               
**     ++INCLUDE DEDBLOCK                                                       
*                                                                               
*                   OBJECTS                                                     
*                                                                               
HDROBJCT DS    0C                  HEADER OBJECT STRUCTURE                      
HDRMED   DS    CL1            C    MEDIA                                        
HDRCLT   DS    CL3            C    CLIENT NAME                                  
HDRPRD   DS    0CL6           C    PRODUCT NAME WITH PIGGYBACK                  
HDRPR1   DS    CL3                                                              
HDRPPB   DS    CL3                                                              
HDREST   DS    CL2            X    ESTIMATE #                                   
HDRSRV   DS    CL1            C    RATING SERVICE                               
HDRBOOKS DS    XL16           X    BOOKS (4 ZERO PADDED YEAR/MONTH)             
HDRSDATE DS    CL6            N    BUY OR RETRANSFER START DATE (YMD)           
HDREDATE DS    CL6            N    END DATE (YMD)                               
HDRWKS   DS    CL2            N    # OF WEEKS                                   
HDRGENTA DS    CL1            C    GENERATE T/A REQUEST(Y/N)                    
HDRRETR  DS    CL1            C    RETRANSFER (Y/N)                             
HDRPREND DS    CL6            N    PREVIOUS END DATE (YMD)                      
HDRREPRV DS    CL1            C    RETRANSFER PREVIOUSLY UPLOADED WEEKS         
HDRCOST  DS    CL1            C    COST CHANGES ALLOWED (Y/N)                   
HDRDEMO  DS    CL1            C    DEMO CHANGES ALLOWED (Y/N)                   
HDRDTYPS DS    (MAXDEMOS)CL6  X    VARIABLE LENGTH LIST OF DEMO TYPES           
         DS    CL2            X    IN CASE WE HAVE MAX # OF DEMO TYPES          
HDRDAILY DS    CL1                 Y=DAILY SCHEDULE                             
HDRRATE  DS    CL1                 RATE TYPE                                    
HDRREP   DS    XL4                 SPECIAL REP                                  
HDRBUYER DS    CL12                BUYER NAME                                   
         DS    XL1                 NULL TERMINATOR                              
HDRLEN   EQU   *-HDROBJCT          LENGTH OF HEADER OBJECT                      
*                                                                               
SCHOBJCT DS    0C                  STRUCTURE OF SCHEDULE OBJECT (BIN)           
SCHERROR DS    XL1                 ERRORS IF ANY                                
SCHLNNUM DS    XL4                 SCHEDULE LINE UNIQUE ID                      
SCHINDS  DS    XL1                 INDICATORS                                   
SCHIRET  EQU   X'80'               SLINE IS A RE-TRANSFER                       
SCHIORB  EQU   X'40'               SLINE IS AN ORBIT                            
SCHODATE DS    CL6                 ORIGINAL TRANSFER DATE                       
SCHOTIME DS    XL3                 ORIGINAL TRANSFER TIME                       
SCHSTA   DS    CL5                 STATION CALL LETTERS                         
SCHAMKT  DS    CL3                                                              
SCHDAY   DS    XL1                 DAY CODE (BITWISE)                           
SCHSTIME DS    XL2                 START TIME (MILITARY)                        
SCHNTIME DS    XL2                 END TIME                                     
SCHDYPRT DS    CL1                 DAYPART CODE                                 
SCHMASLN DS    XL1                 MASTER PRD SPOT LENGTH IN SECONDS            
SCHTOTLN DS    XL1                 TOTAL SPOT LENGTH IN SECONDS                 
SCHCOST  DS    XL3                 SPOT COST                                    
SCHPROG  DS    CL18                PROGRAM NAME                                 
SCHDT    DS    XL31                DAYTIME BLOCK FOR ORBIT                      
SCHOVERS DS    XL2                 DEMO OVERRIDE BIT LIST                       
SCHCOMLN DS    XL1                 COMMENT LENGTH                               
SCHDAYNM DS    XL1                 SPOTPAK DAY NUMBERS                          
SCHDATA  DS    0C                  ALLOCATE THE MAXIMUM SPACE NEEDED            
SCHSPOTS DS    (MAXWEEKS)XL1       SPOTS PER WEEK                               
SCHDVALS DS    (MAXDEMOS)XL2       DEMO VALUES PER DEMO TYPE                    
SCHTRANS DS    (MAXWEEKS)XL1       SPOTS PER WEEK IN LAST TRANSFER              
SCHCOM   DS    XL256               COMMENT                                      
SCHLEN   EQU   *-SCHOBJCT          LENGTH OF SCHEDULE OBJECT                    
*                                                                               
SPLOBJCT DS    0C                                                               
SPLMKT   DS    CL8                 SPILL MARKET IDENTIFIER                      
SPLNETWK DS    CL3                 IF MEDIA C, CABLE NETWORK                    
SPLPURP  DS    CL6                 PURPOSE CODE                                 
SPLLEN   EQU   *-SPLOBJCT                                                       
*                                                                               
CNFOBJCT DS    0C                  CONFIRMATION OBJECT STRUCTURE                
CNFLNNUM DS    CL8                 SCHEDULE LINE SEQUENCE NUMBER                
CNFERROR DS    CL2                 ERROR IF ANY                                 
CNFINDS  DS    CL2                 INDICATORS                                   
CNFIFRZ  EQU   X'80'               SOMETHING WAS FROZEN ON BUY SIDE             
CNFIDEL  EQU   X'40'               BUY LINE(S) WERE DELETED                     
CNFTDATE DS    CL6                 TRANSFER DATE                                
CNFTTIME DS    CL6                 TRANSFER TIME (HOURS/MINS./SECONDS)          
CNFLNQ1  EQU   *-CNFOBJCT          FIXED LENGTH OF CONFIRMATION OBJECT          
CNFSPOTS DS    XL(MAXWEEKS*2)      TRANSFERRED SPOTS/WEEK                       
CNFLENQ  EQU   *-CNFOBJCT          LENGTH OF CONFIRMATION OBJECT                
CNFLEN   DS    CL1                                                              
*                                                                               
SCHDATS  DS    XL((MAXWEEKS*4)+1)                                               
SCHSTMON DS    CL6                 SCHEDULE START MONDAY (YYMMDD)               
SCHSTMNP DS    XL2                 SCHEDULE START MONDAY (COMPRESSED)           
SCHSTMNB DS    XL3                 SCHEDULE START MONDAY (BINARY)               
SCHENDB  DS    XL3                 SCHEDULE END DATE(BINARY)                    
*                                                                               
LAWKSTO  DS    A                   FIRST OLD SWEEKS ENTRY WITH SPOTS            
LAWKSTN  DS    A                   FIRST NEW SWEEKS ENTRY WITH SPOTS            
LAWKENO  DS    A                   LAST OLD SWEEKS ENTRY WITH SPOTS             
LAWKENN  DS    A                   LAST NEW SWEEKS ENTRY WITH SPOTS             
LSPTOTN  DS    F                   NEW TOTAL NUMBER OF SPOTS                    
LSPTOTO  DS    F                   OLD TOTAL NUMBER OF SPOTS                    
LMAXSPTS DS    F                   MAX # SPOTS THAT FIT ON THIS BUYREC          
LNSPWKSO DS    H                   NUMBER OF WEEKS WITH OLD SPOTS               
LNSPWKSN DS    H                   NUMBER OF WEEKS WITH NEW SPOTS               
*                                                                               
SWEEKS   DS    (MAXWEEKS)XL(SWEEKSLQ)                                           
SWEEKSL  EQU   *-SWEEKS                                                         
LNEWTOT  DS    XL(MAXWEEKS)        NEW SPOTS/WEEK TOTALS FOR RETURN             
*                                                                               
LNSPTS   DS    X                                                                
LMAXSPW  DS    X                                                                
LDAYDSPL DS    X                                                                
*                                                                               
LBADDCHA DS    X                                                                
LBADD    EQU   1                                                                
LBCHA    EQU   2                                                                
*                                                                               
LFLAG    DS    X                                                                
LFREEZE  EQU   X'80'                                                            
LAFFDVT  EQU   X'40'                                                            
LFSTWK   EQU   X'20'                                                            
LNEWEEK  EQU   X'10'                                                            
LDELETE  EQU   X'08'                                                            
LPAIDSPT EQU   X'04'                                                            
LORBIT   EQU   X'02'                                                            
LWEEKLY  EQU   X'01'                                                            
*                                                                               
LCHGIND  DS    X                   RECORD CHANGE INDICATOR                      
LSLN     EQU   X'01'                                                            
LDAYS    EQU   X'02'                                                            
LSTDATE  EQU   X'04'                                                            
LTIMES   EQU   X'08'                                                            
LTIMNOTX EQU   X'10'                                                            
LCST     EQU   X'20'                                                            
LNEWPRD  EQU   X'80'                                                            
*                                                                               
LORBDAY  DS    XL1                                                              
LORBTIM1 DS    XL2                                                              
LORBTIM2 DS    XL2                                                              
*                                                                               
SLAVELST DS    XL255                                                            
LSTALIST DS    (MAXSTA)XL(L'BMKTSTA)  STATION LIST                              
*                                                                               
ERRTAB   DS    (MAXERRS)XL5         SLINE ERROR TABLE                           
SDEKEY   DS    XL11                                                             
SDEDATES DS    XL128                                                            
* FOLLOWING ORG SHOULD NOT EXCEED X'1000'                                       
         ORG                                                                    
         EJECT                                                                  
*=====================================================================          
* DSECTS                                                                        
*=====================================================================          
         SPACE 1                                                                
SWEEKSD  DSECT                 *** WEEKS TABLE DSECT                            
SWDATE   DS    XL2                 SPOT DATE                                    
SWBSTDT  DS    XL2                 BROADCAST WEEK START DATE                    
SWBENDT  DS    XL2                 BROADCAST WEEK END   DATE                    
SWOSPT   DS    X                   COUNT OF OLD SPOTS                           
SWNSPT   DS    X                   COUNT OF NEW SPOTS                           
SWRSPT   DS    X                   COUNT OF PREVIOUS SPOTS                      
SWIND1   DS    X                   INDICATORS 1                                 
SWI1FRZ  EQU   X'80'                                                            
SWIND2   DS    X                   INDICATORS 2                                 
         DS    XL5                                                              
SWEEKSLQ EQU   *-SWEEKSD                                                        
*                                                                               
SLD      DSECT                 *** SLINE OBJECT DSECT ***                       
SLSEQ    DS    CL8                 SEQUENCE NUMBER                              
SLODATE  DS    CL6                 ORIGINAL TRANSFER DATE                       
SLOTIME  DS    XL6                 ORIGINAL TRANSFER TIME                       
SLSTAT   DS    CL5                 STATION                                      
SLDAYTIM DS    XL62                ORBIT DAYTIME BLOCK                          
SLDAYPRT DS    CL1                 DAYPART CODE                                 
SLMASLEN DS    XL2                 2 BINARY SPOT LENGTHS (MASTER)               
SLLENGTH DS    CL2                 2 BINARY SPOT LENGTHS (TOTAL)                
SLCOST   DS    XL6                 BINARY COST                                  
SLPROG   DS    CL18                PROGRAM NAME                                 
SLOVERS  DS    XL4                 DEMO OVERRIDE BIT MASK                       
SLDATA   DS    CL524               VARIABLE DATA                                
SLLENQ   EQU   *-SLD               OBJECT LENGTH                                
*                                                                               
BLND     DSECT                 *** BUY LINE TABLE ENTRY DSECT                   
BLNLINE  DS    XL1                 LINE NUMBER                                  
BLNSTAT  DS    XL1                 STATUS                                       
BLNSDEL  EQU   C'D'                DELETED RECORD                               
BLNSPURP EQU   C'P'                DIFFERENT PURPOSE CODE                       
BLNPACK  DS    XL1                 MASTER/SLAVE IN PACKAGE?                     
BLNPSLVE EQU   C'M'                                                             
BLNPMSTR EQU   C'S'                                                             
         DS    XL1                 N/D                                          
BLNSEQ   DS    XL4                 SEQUENCE NUMBER                              
BLNMAS   DS    XL1                 MASTER PRODUCT CODE                          
BLNPIG   DS    XL1                 PIGGYBACK PRODUCT CODE                       
         DS    XL6                 N/D                                          
BLNLNQ   EQU   *-BLND              ENTRY LENGTH                                 
*                                                                               
DTD      DSECT                 *** DAYTIME BLOCK DSECT                          
DTDAY    DS    XL1                 SPOTPAK DAY BITS                             
DTDAYNUM DS    XL1                 DAY NUMBERS                                  
DTSTART  DS    XL2                 START TIME                                   
DTEND    DS    XL2                 END TIME                                     
DTLNQ    EQU   *-DTD               BLOCK ENTRY LENGTH                           
         EJECT                                                                  
*=====================================================================          
* OTHER INCLUDED DSECTS                                                         
*=====================================================================          
         SPACE 1                                                                
* SPSTAPACKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPSTAPACKD                                                     
         PRINT ON                                                               
* SPGENSTA                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
* SPGENMKT                                                                      
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
* SPGENSDEF                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENSDEF                                                      
         PRINT ON                                                               
* SPGENWIPW                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENWIPW                                                      
         PRINT ON                                                               
AGYHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
CLTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
ESTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
BUYRECD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENPURP                                                      
         PRINT ON                                                               
* CTMADWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTMADWORKD                                                     
         PRINT ON                                                               
* CTMADEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTMADEQUS                                                      
         PRINT ON                                                               
* CTMADDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTMADDSECT                                                     
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
* SPAUTHD                                                                       
       ++INCLUDE SPAUTHD                                                        
* FASECRETD                                                                     
       ++INCLUDE FASECRETD                                                      
* FATWAD                                                                        
       ++INCLUDE FATWA                                                          
* DDOFFICED                                                                     
       ++INCLUDE DDOFFICED                                                      
* SPGENDRBTC                                                                    
       ++INCLUDE SPGENDRBTC                                                     
* SPGETBUBLD                                                                    
       ++INCLUDE SPGETBUBLD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109CTMAD26   12/21/09'                                      
         END                                                                    
