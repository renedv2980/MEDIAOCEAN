*          DATA SET SPSFM2B    AT LEVEL 035 AS OF 08/11/11                      
*PHASE T2172BA                                                                  
                                                                                
***********************************************************************         
*                                                                     *         
*  TITLE: T2172B - DEMO OVERIDE VALUE MAINTENANCE (DEMOVER)           *         
*                                                                     *         
* MHER 031 11FEB05 READ 00A PROFILE FOR 0/NOVAL DEFAULT               *         
*                  ALLOW + AND ++ ENTRIES                             *         
* PWES 030 26OCT04 ENSURE PCPAK GIVES PFKEY BUTTONS + DONT MRKDEMO IF *         
*                  DEMO VALUE INPUT=0 + CORRECT LIST END-OF-RECS CHECK*         
* PWES 029 30SEP04 SKIP AUTO UPDATE OF REC ON DISP IF NON-UPDATIVE!   *         
* PWES 028 07JUL04 ALTER STA/SPILL MSG                                *         
* PWES 027 23JUN04 HILITE STATIONS / STA/SPILL MSG / PROG DETS 1 FIELD*         
* PWES 026 14MAY04 PF4 TEXT CHANGE & FIX <PF4> ON ADD / GENNDOV EQUS  *         
*                  + FIX DIR KEY FILTERS / BBM DEFAULT RTG SVC        *         
* AKAT 025 03SEP03 FIX MAXELS LOOP BUG                                *         
* PWES 024 28MAY03 MSG NOT DUMP IF STATIONS/SPILL EXCEED MAX# 05 ELS  *         
* PWES 023 08MAY03 ALWAYS USE AGENCY LEVEL SPILL RECORD               *         
* PWES 022 06FEB03 GENERAL NICETIES INCLUDING DEMO SCROLLING          *         
* PWES 021 19NOV01 CLEAR ACURFORC SO ERROR IN CORRECT FIELD           *         
* PWES 020 26OCT01 NEW FORMAT CANADIAN CABLE                          *         
* ABEA ?19 04SEP01 USER DEMOS                                         *         
* TZIH ?18 21AUG01 OPTIMISING                                         *         
* ABEA ?17 25JUL01 INCREASE MAXELS                                    *         
* ???? 011 ??????? FORCE CURSOR TO RECORD FIELD ON VKEY (NWK UPLOAD?) *         
* ???? 7&8 ??????? PFKEYS                                             *         
* ???? 006 ??????? PREVENT DELETE FROM LIST                           *         
*                                                                     *         
*  CALLED FROM: SPOT CONTROLLER (T21700), WHICH CALLS                 *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  ***  'NETWORK UPLOAD' SCSPFILDEM SCRIPTS THE MAINT SCREEN!  ***    *         
*                                                                     *         
*  INPUTS: SCREENS SPSFMA1  (T217A1) -- MAINTENANCE                   *         
*          SCREENS SPSFMA4  (T217A4) -- LIST                          *         
*                                                                     *         
*  OUTPUTS: UPDATED DEMOVER RECORDS                                   *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - SECOND BASE REG                                       *         
*          R8 - OVERRIDE REG                                          *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T2172B DEMO OVERIDE VALUE'                                      
T2172B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2172B*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         MVI   FILTFLAG,0                                                       
         OI    GENSTAT4,NODELLST                                                
*                                                                               
         BAS   RE,SETUP                                                         
         BAS   RE,RDPROF                                                        
*                                                                               
* NOTE: ADD AND CHANGE ACTIONS ARE TREATED AS 'OTHER' ACTIONS BY GENCON         
*       SO ONLY GET VALKEY/VALREC MODES (SEE SFM00 RECACT TABLE)                
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*=============================================================*                 
*   BMPFLD      BUMP TO THE NEXT FIELD                        *                 
*=============================================================*                 
BMPFLD   ZIC  R0,0(R2)                                                          
         AR   R2,R0                                                             
         BR   RE                                                                
         EJECT                                                                  
*=============================================================*                 
*   VK                  VALIDATE KEY                          *                 
*=============================================================*                 
VK       XC    SAVEKEY,SAVEKEY                                                  
         LA    R3,SAVEKEY                                                       
         USING DOVRECD,R3                                                       
         MVC   DOVKTYP,=X'0D17'    DEMO OVERIDE DEF RECORD                      
*      ------------------------                                                 
         XC    TMPFLD,TMPFLD       AGENCY/MEDIA                                 
         MVC   TMPFLD,=XL9'0900000000010000D5' X'D5'= C'N' FOR NETWORK          
         LA    R2,TMPFLD                                                        
         GOTO1 VALIMED     VALIMED - READ AGYHDR & FILL IN SVAPROFF             
         MVC   DOVKAGMD,BAGYMD                                                  
*      ------------------------                                                 
         CLI   ACTEQU,ACTLIST                                                   
         BE    VKL                                                              
*      ------------------------                                                 
         LA    R2,DMVNTWKH         NETWORK                                      
         CLI   DMVNTWKH+5,0                                                     
         BE    ERRMISS                                                          
         BAS   RE,VALINET                                                       
         MVC   DOVKNET,BSTA                                                     
*      ------------------------                                                 
         LA    R2,DMVSHOWH        PROGRAM (SHOW)                                
         CLI   DMVSHOWH+5,0                                                     
         BE    ERRMISS                                                          
*                                                                               
         XC    DMVPGMD,DMVPGMD                                                  
         OI    DMVPGMDH+FHOID,FHOITR                                            
*                                                                               
         BAS   RE,VALISHOW                                                      
         MVC   DMVPGMD(L'PGMDNAME),SVFULLSH                                     
         MVC   DMVPGMD+PGMDDAY-PGMDETSD(L'PGMDDAY),SVDAY                        
         MVC   DMVPGMD+PGMDTIME-PGMDETSD(L'PGMDTIME),SVTIME                     
*                                                                               
         MVC   DOVKPGM,DMVSHOW                                                  
         OC    DOVKPGM,=C'    '                                                 
*      ------------------------                                                 
         LA    R2,DMVRATSH         RATING SERVICE                               
         CLI   DMVRATSH+5,0                                                     
*        BE    ERRMISS                                                          
         BNE   VK5                                                              
         MVC   FHDAD(3,R2),=C'BBM' DEFAULT                                      
         MVI   FHILD(R2),3                                                      
         OI    FHIID(R2),FHIITH                                                 
         OI    FHOID(R2),FHOITR                                                 
VK5      BAS   RE,VALIRATS                                                      
         MVC   DOVKRTS,SVRATS                                                   
*      ------------------------                                                 
         LA    R2,DMVDEMOH         DEMO, VALIDATE AND PUT 3 BYTE DEMO           
         CLI   DMVDEMOH+5,0        CODE INTO SVDEMOCD                           
         BE    ERRMISS                                                          
         LA    R4,DMVDEMOH                                                      
         BAS   RE,VALIDEMO                                                      
*                                                                               
         BAS   RE,DEMSCROL                                                      
*      ------------------------                                                 
         LA    R2,DMVCLTH          CLIENT (OPTIONAL)                            
         XC    BCLT,BCLT                                                        
         CLI   DMVCLTH+5,0                                                      
         BE    VK10                                                             
         GOTO1 VALICLT                                                          
         MVC   DOVKCLT,BCLT                                                     
*      ------------------------                                                 
VK10     LA    R2,DMVSEQH          SEQUENCE NUMBER (OPTIONAL)                   
         CLI   DMVSEQH+5,0                                                      
         BE    VK100                                                            
         TM    DMVSEQH+4,X'08'     TEST FIELD IS VALID NUMERIC!                 
         BZ    ERRINV                                                           
         PACK  DUB,DMVSEQ(1)                                                    
         CVB   R6,DUB                                                           
         CH    R6,=H'1'            MUST BE 0 OR 1                               
         BH    ERRINV                                                           
         STC   R6,DOVKSEQ                                                       
*      ------------------------                                                 
VK100    MVC   KEY,SAVEKEY         SET UP KEY FOR GENCON                        
*      ------------------------                                                 
         NI    KEYFLAG,X'FF'-KEYDIFF                                            
         CLC   SAVEKEY,SAVEKEY2    HAS KEY BEEN CHANGED                         
         BE    *+8                                                              
         OI    KEYFLAG,KEYDIFF                                                  
         MVC   SAVEKEY2,SAVEKEY                                                 
* - - - - - - - - - - - - - - - - -                                             
         CLI   ACTEQU,ACTCHA                                                    
         BE    VK110                                                            
         CLI   ACTEQU,ACTADD                                                    
         BNE   VK120                                                            
VK110    CLI   DMVCOPY,C'Y'                                                     
         BE    VK125                                                            
VK120    CLC   SVDEMOCD,SVDEMOC2   HAS DEMO BEEN CHANGED                        
         BE    *+8                                                              
         OI    KEYFLAG,KEYDIFF                                                  
VK125    MVC   SVDEMOC2,SVDEMOCD                                                
         XC    DMVCOPY,DMVCOPY                                                  
         OI    DMVCOPYH+6,X'80'                                                 
*                                                                               
VKX      DS    0H                                                               
         LA    R2,CONRECH                                                       
         STCM  R2,15,ACURFORC      FORCE CURSOR TO RECORD FIELD                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* VALIDATE KEY FOR A LIST                                            *          
**********************************************************************          
         SPACE                                                                  
VKL      CLI   LSTNTWKH+5,0        NETWORK                                      
         BE    VKL10                                                            
         LA    R2,LSTNTWKH                                                      
         BAS   RE,VALINET                                                       
         MVC   DOVKNET,BSTA        START POINT                                  
*                                                                               
VKL10    CLI   LSTCLNTH+FHILD,0    CLIENT                                       
         BE    VKL20                                                            
         LA    R2,LSTCLNTH                                                      
         CLI   FHDAD(R2),C'>'      START POINT?                                 
         BNE   VKL12               NO, FILTER                                   
         CLI   LSTCLNTH+FHILD,1                                                 
         BNH   ERRINV              ONLY >                                       
*                                                                               
         MVC   QCLT,=C'   '        SOME PHONEY CLIENT                           
         MVC   QCLT+1(2),=C'AA'    JUST TO GET CLOSER                           
         SR    RF,RF                                                            
         IC    RF,FHILD(R2)                                                     
         SHI   RF,2                1 FOR EX + 1 FOR >                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QCLT(0),FHDAD+1(R2)                                              
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   ERRINV                                                           
         CLI   LSTNTWKH+5,0        CLIENT START REQUIRES NETWORK                
         BNE   VKL15                                                            
         LA    R2,LSTNTWKH                                                      
         B     ERRMISS                                                          
*                                                                               
VKL12    GOTO1 VALICLT                                                          
VKL15    CLI   LSTNTWKH+5,0        CANNOT SET CLIENT IN KEY IF NO NWK!          
         BE    *+10                                                             
         MVC   DOVKCLT,BCLT                                                     
*                                                                               
VKL20    EQU   *                   PROGRAM (SHOW) NO VALIDATION                 
*                                                                               
         CLI   LSTRATSH+5,0        RATING SERVICE                               
         BE    VKL30                                                            
         LA    R2,LSTRATSH                                                      
         BAS   RE,VALIRATS                                                      
*                                  *** FILE RECORD FILTERS ***                  
VKL30    LA    R2,LSTDEMOH         DEMO, VALIDATE AND PUT 3 BYTE DEMO           
         CLI   LSTDEMOH+5,0        CODE INTO SVDEMOCD                           
         BE    VKLX                                                             
         LA    R4,LSTDEMOH                                                      
         BAS   RE,VALIDEMO                                                      
*                                                                               
VKLX     B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*                 
*   DK                 DISPLAY THE KEY                        *                 
*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*                 
DK       L     R3,AIO                                                           
         USING DOVRECD,R3                                                       
*      ------------------------------                                           
         XC    BMKTSTA,BMKTSTA             MARKET IS NETWORK = NULL             
         MVC   BSTA(2),DOVKNET             DITTO FOR STATION SEQ                
         GOTO1 MSUNPK,DMCB,BMKTSTA,MYQMKT,MYQSTA  NETWORK (EBCDIC)              
         MVC   DMVNTWK(4),MYQSTA                                                
         OI    DMVNTWKH+6,X'80'                                                 
*                                                                               
         LA    R2,DMVNTWKH         NEED SVNETSEQ FOR BLDSTAB ROUTINE            
         MVC   SAVEKEY3,KEY                                                     
         MVC   AIO,AIO2            DEMOVER RECORD IS IN I/O1                    
         BAS   RE,VALINET                                                       
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY3                                                     
*      ------------------------------                                           
         TM    DEMFLG,DEMFLT                                                    
         BO    DK10                                                             
*        BO    *+10                                                             
*        MVC   SVDEMOCD,DOVDLSTC                                                
         SR    RF,RF               DISPLAY FIRST DEFINED DEMO                   
         IC    RF,1(R3)            ELEM LENGTH                                  
         SH    RF,=H'12'           SUBTRACT DATE FLDS + ELEM ID + LEN           
         BNP   DK10                NO DEMOS IN ELEM                             
         SR    RE,RE                                                            
         D     RE,=F'3'            SET NUM DEMOS SLOTS IN RF                    
         LA    R1,DOVDLSTC                                                      
DK05     MVC   SVDEMOCD,0(R1)                                                   
         OC    SVDEMOCD,SVDEMOCD                                                
         BNZ   DK10                FOUND FIRST DEFINED DEMO                     
         LA    R1,3(R1)                                                         
         BCT   RF,DK05                                                          
DK10     BAS   RE,GETDEMNM                                                      
         MVC   DMVDEMO,SVDEMO                                                   
         NI    SVDEMOCD,X'FF'-X'80' SET OFF HIGH ORDER BIT                      
         OI    DMVDEMOH+6,X'80'                                                 
*      ------------------------------                                           
         MVC   DMVSHOW,DOVKPGM    PROGRAM (SHOW)                                
         OI    DMVSHOWH+6,X'80'                                                 
         MVC   SAVEKEY3,KEY                                                     
*                                                                               
         LA    R2,DMVSHOWH                                                      
         BAS   RE,VALISHOW                                                      
         MVC   KEY,SAVEKEY3                                                     
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   DMVPGMD(L'PGMDNAME),SVFULLSH                                     
         MVC   DMVPGMD+PGMDDAY-PGMDETSD(L'PGMDDAY),SVDAY                        
         MVC   DMVPGMD+PGMDTIME-PGMDETSD(L'PGMDTIME),SVTIME                     
         OI    DMVPGMDH+FHOID,FHOITR                                            
*      ------------------------------                                           
         MVC   DMVRATS,=C'BBM'     RATING SERVICE BBM                           
         CLI   DOVKRTS,C'1'        RATING SERVICE OF C'1'?                      
         BE    *+10                                                             
         MVC   DMVRATS,=C'NSI'     RATING SERVICE NSI                           
         OI    DMVRATSH+6,X'80'                                                 
*      ------------------------------                                           
         EDIT  DOVKSEQ,DMVSEQ,ZERO=BLANK    SEQUENCE NUMBER                     
         OI    DMVSEQH+6,X'80'                                                  
*      ------------------------------                                           
         GOTO1 CLUNPK,DMCB,DOVKCLT,DMVCLT                                       
         OI    DMVCLTH+6,X'80'                                                  
         MVC   BCLT,DOVKCLT                                                     
*      ------------------------------                                           
         MVC   SAVEKEY,KEY                                                      
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*                 
*   DR                 DISPLAY THE RECORD                     *                 
*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*                 
DR       BAS   RE,DEMOPOSN         CALCULATE DEMO POSITION AND                  
*                                                                               
DR10     BRAS  RE,DEMCOUNT         COUNT NUMBER OF DEMOS                        
*                                  GET CORRECT SEQUENCE RECORD                  
         TWAXC DMVMSGH,DMVDVLLH,PROT=Y                                          
*                                                                               
         L     R1,AIO                                                           
         TM    DOVCNTL-DOVRECD(R1),DOVCACDQ                                     
         BNZ   DR12                                                             
*                                                                               
         BAS   RE,BLDSTAB                                                       
         BH    ERR05S              CANNOT ADD ALL REQUIRED 05 ELEMS             
*                                                                               
         MVC   AIO,AIO3                                                         
         BAS   RE,ADD05            BUILD RECORD WITH CORRECT 05 ELEMS           
*                                        IN AIO1                                
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 GETREC              RESET POINTER FOR PUTREC                     
         MVC   AIO,AIO1            POINT TO NEWLY BUILT RECORD                  
*                                                                               
         L     RF,ACOMFACS         DON'T UPDATE RECORD IN READONLY MODE         
         ICM   RF,15,CXTRAINF-COMFACSD(RF)                                      
         BZ    *+12                                                             
         TM    XIFLAG1-XTRAINFD(RF),XIROSYS+XIROMODE+XIWRONGF                   
         BNZ   DR12                LIST/SEL SCRNS WILL CORRUPT IF R/O           
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
DR12    MVI   NEWDEMO,C'Y'         SET FLAG IF DEMO PREV INPUT                  
        L     RE,AIO                                                            
        LA    RE,DOVDLSTC-DOVRECD(RE)                                           
        LH    RF,SVDEMNUM                                                       
        MHI   RF,3                LENGTH OF EACH DEMO CODE                      
        AR    RE,RF                                                             
        TM    0(RE),X'80'     X'80' SIGNIFIES PRESENCE OF A DEM VAL             
        BZ    *+8                                                               
        MVI   NEWDEMO,C'N'                                                      
*                                                                               
         LA    R2,DMVDVL1H         FIRST DEMO VALUE HEADER                      
         LA    R5,DMVHD1H          FIRST STATION FIELD HEADER                   
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         USING DOVEL05,R3                                                       
*                                                                               
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR15     BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
*                                                                               
         NI    FHOID(R5),X'FF'-FHOIHI                                           
         MVC   BSTA,DOVSTA                                                      
         MVC   BMKT,DOVMKT                                                      
         GOTO1 MSUNPK,DMCB,(X'80',BMKTSTA),MYQMKT,MYQSTA                        
         CLI   DOVSTA,0            IS THIS A STATION?                           
         BNE   DR20                NO                                           
         SR    R1,R1                                                            
         ICM   R1,3,DOVMKT                                                      
         CHI   R1,9999             MARKET IF <= 9999                            
         BNH   DR22                                                             
*                                                                               
DR20     OI    FHOID(R5),FHOIHI                                                 
         CLI   SVNETSEQ,NDEFCABQ                                                
         BE    *+14                                                             
         MVC   FHDAD(4,R5),MYQSTA                                               
         B     DR30                                                             
         MVC   FHDAD(3,R5),MYQSTA+4    JUST SHOW MARKET SUFFIX                  
         B     DR30                                                             
*                                                                               
DR22     CLI   DMVDEMO,C'R'        SEE IF DOING A RATING                        
         BE    DR25                YES - THEN SPILL                             
         CLI   DMVDEMO,C'E'        SEE IF DOING AN EXTENDED DEMO                
         BE    DR25                YES - SPILL                                  
         CLI   DMVDEMO,C'U'        SEE IF DOING A USER DEMO                     
         BNE   DR35                NO - THEN NO SPILL                           
DR25     MVI   FHDAD(R5),C'*'      SPILL MARKET                                 
         MVC   FHDAD+1(4,R5),MYQMKT                                             
         MVC   DMVMSG+L'LISTMSG1(L'LISTMSG2),LISTMSG2                           
*                                                                               
DR30     BAS   RE,GETDVL           RETURNS DEMO VALUE IN SVDEMVAL               
         CLI   NEWDEMO,C'Y'        TEST CREATING NEW DEMO                       
         BNE   DR31                NO                                           
*                                                                               
         CLI   SVDFLTOV,C'L'       TEST DEFAULT TO LLOKUP                       
         BE    DR31                                                             
         MVC   SVDEMVAL,=X'8000'                                                
*                                                                               
DR31     XC    8(L'DMVDVL1,R2),8(R2)                                            
         MVI   8(R2),C'L'          ELSE NEED AN L                               
         OC    SVDEMVAL,SVDEMVAL   TEST LOOKUP                                  
         BZ    DR33                                                             
*                                                                               
         MVI   8(R2),C'0'                                                       
         CLC   SVDEMVAL,=X'8000'   TEST ZERO                                    
         BE    DR33                                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,SVDEMVAL                                                    
         CHI   R1,999              IF GREATER THAN 99.9 NO DECIMAL              
         BH    DR32                                                             
         EDIT  SVDEMVAL,(L'DMVDVL1,8(R2)),1,ZERO=BLANK,ALIGN=LEFT               
         B     DR33                                                             
*                                                                               
DR32     SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(L'DMVDVL1,8(R2)),ZERO=BLANK,ALIGN=LEFT                     
*                                                                               
DR33     OI    6(R2),X'80'         TRANSMIT DEMO VALUE                          
         OI    6(R5),X'80'         TRASMIT STATION HEADER FIELD                 
*                                                                               
DR35     LA    RF,DMVDVLLH                                                      
         BAS   RE,BMPFLD                                                        
         CR    R2,RF                                                            
         BH    DRX                 EOS                                          
         TM    FHATD(R2),FHATPR                                                 
         BNZ   DR35                                                             
*                                                                               
         LA    RF,DMVHDLLH                                                      
DR37     SR    R0,R0                                                            
         IC    R0,FHLND(R5)                                                     
         AR    R5,R0                                                            
         CR    R5,RF                                                            
         BH    DRX                 EOS                                          
         TM    FHATD(R5),FHATPR                                                 
         BZ    DR37                                                             
         B     DR15                                                             
*                                                                               
DRX      MVC   DMVMSG(L'LISTMSG1),LISTMSG1                                      
         CLI   SVNETSEQ,NDEFCABQ                                                
         BNE   *+10                                                             
         MVC   DMVMSG(L'LISTMSG3),LISTMSG3                                      
*                                                                               
         OI    GENSTAT2,DISTHSPG   REDISPLAY SAME LIST PAGE                     
*                                                                               
         NI    KEYFLAG,X'FF'-KEYDIFF                                            
*                                                                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*                
*   VR             VALIDATE AND BUILD RECORD                   *                
*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*                
VR       DS    0H                                                               
         XC    ACURFORC,ACURFORC   CLEAR CURSOR O/R - WHY IS IT SET?            
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                NEED TO DO OWN I/O FOR CHANGE                
         CLC   KEY(13),KEYSAVE     AND ADD                                      
         BNE   ERRINV                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,DEMOPOSN         CALCULATE DEMO POSITION AND                  
*                                   GET CORRECT SEQUENCE RECORD                 
*                                                                               
         TM    KEYFLAG,KEYDIFF     KEY HAS CHANGED?                             
         BO    DR10                YES,JUST DISPLAY                             
*                                                                               
         LA    R2,DMVDVL1H         FIRST DEMO VALUE HEADER                      
         LA    R5,DMVHD1H          FIRST STATION FIELD HEADER                   
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         USING DOVEL05,R3                                                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VR15     BAS   RE,NEXTEL                                                        
         BNE   VR200                                                            
*                                                                               
         OI    8(R5),C' '          COULD BE NULL OR SPACE                       
         OI    8(R2),C' '          FORCE SPACE                                  
         CLI   8(R5),C' '          IS THERE A STATION OR SPILL MKT?             
         BH    VR30                YES                                          
         CLI   8(R2),C' '          MUST BE NO VALUE IF NO STA OR SPILL          
         BH    ERRINV                                                           
         B     VR40                GO TO NEXT FIELD                             
*                                                                               
VR30     CLI   8(R2),C' '          IS THERE A BLANK FOR VALUE?                  
         BH    VR32                NO                                           
*                                                                               
VR31     L     R0,=X'00008000'     ASSUME IT SHOULD BE 0                        
         CLI   SVDFLTOV,C'L'       TEST IT SHOULD BE LOOKUP                     
         BNE   *+6                 NO                                           
         SR    R0,R0                                                            
         B     VR36                                                             
*                                                                               
VR32     BAS   RE,GETDVL           INDEX TO DEM VAL IN SVDVALIX                 
*                                                                               
         SR    R6,R6                                                            
         IC    R6,5(R2)            PICK UP INPUT LENGTH AND                     
         ST    R6,DMCB+4           SET IT FOR CASHVAL                           
*                                                                               
         CHI   R6,1                TEST MORE THAN 1 CHAR INPUT                  
         BNH   VR34                NO - SO THERE CAN'T BE ANY ++'S              
         BCTR  R6,0                                                             
         EX    R6,TEST0            TEST FOR 0++                                 
         BE    VR33                                                             
         EX    R6,TESTL            TEST FOR L++                                 
         BNE   VR34                                                             
*                                                                               
VR33     XC    SVDFLTOV,SVDFLTOV                                                
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   SVDFLTOV(0),8(R2)   SAVE 3 CHARS                                 
         B     VR31                                                             
*                                                                               
VR34     SR    R0,R0               SET FOR LOOKUP                               
         CLI   8(R2),C'L'                                                       
         BE    VR36                                                             
         GOTO1 CASHVAL,DMCB,(1,8(R2))                                           
         CLI   DMCB,0              VALID?                                       
         BNE   ERRINV                                                           
*                                                                               
         ICM   R0,15,DMCB+4                                                     
         BNZ   *+12                                                             
         L     R0,=X'00008000'     THIS IS ZERO !                               
         B     VR36                                                             
*                                                                               
         C     R0,=F'32767'         MAX IS X'7FFF'                              
         BH    ERRINV                                                           
         B     VR36                                                             
*                                                                               
TEST0    CLC  8(0,R2),=C'0++'                                                   
TESTL    CLC  8(0,R2),=C'L++'                                                   
*                                                                               
VR36     LTR   R0,R0               DON'T FLAG DEMOS SET TO ZERO                 
         BZ    *+8                                                              
         OI    FILTFLAG,VALYES     SOME DEMO VALUE IS PRESENT                   
*                                                                               
         LA    RE,DOVDEMV                                                       
         AH    RE,SVDVALIX                                                      
         STCM  R0,3,0(RE)                                                       
*                                                                               
         CLI   SVDFLTOV+2,C'+'     TEST IN ++ MODE                              
         BNE   VR40                NO                                           
         CLI   SVDFLTOV,C'L'       TEST OVERRIDE 0 TO L                         
         BNE   VR38                                                             
         CLC   0(2,RE),=X'8000'    IS IT 0                                      
         BNE   VR40                                                             
         XC    0(2,RE),0(RE)       THEN MAKE IT L                               
         B     VR40                                                             
*                                                                               
VR38     OC    0(2,RE),0(RE)       IS IT LOOKUP                                 
         BNE   VR40                NO                                           
         MVC   0(2,RE),=X'8000'    THEN MAKE IT 0                               
*                                                                               
VR40     LA    RF,DMVDVLLH                                                      
*                                                                               
VR42     BAS   RE,BMPFLD                                                        
         CR    R2,RF                                                            
         BNH   *+6                                                              
         DC    H'0'                EOS                                          
         TM    FHATD(R2),FHATPR                                                 
         BNZ   VR42                                                             
*                                                                               
         LA    RF,DMVHDLLH                                                      
VR44     SR    R0,R0                                                            
         IC    R0,FHLND(R5)                                                     
         AR    R5,R0                                                            
         CR    R5,RF                                                            
         BNH   *+6                                                              
         DC    H'0'                EOS                                          
         TM    FHATD(R5),FHATPR                                                 
         BZ    VR44                                                             
         B     VR15                                                             
*      ------------------------------                                           
VR200    BAS   RE,MRKDEMO          MARK DEMO FOR DEM VALUE                      
*                                                                               
VRX      GOTO1 PUTREC                                                           
         OI    GENSTAT2,RETEQSEL   REDISPLAY SAME SELECTION                     
         B     DR10                                                             
         DROP  R3                                                               
         EJECT                                                                  
*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*                
*   LR                     LIST                                *                
*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*                
LR       L     R3,AIO                                                           
         USING DOVRECD,R3                                                       
*                                                                               
         OC    KEY,KEY             TEST FOR 1ST PASS (KEY NULL)                 
         BNZ   LR10                                                             
         MVC   KEY,SAVEKEY                                                      
*                                                                               
LR10     GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR15     GOTO1 SEQ                                                              
*R20     CLC   KEY(3),KEYSAVE      SAME REC TYPE/AGENCY?                        
LR20     CLC   KEY(3),SAVEKEY      SAME REC TYPE/AGENCY?                        
         BNE   LRX                 NO                                           
         CLI   LSTNTWKH+5,0        NETWORK INPUT                                
         BE    *+14                                                             
         CLC   KEY(DOVKNET-DOVKEY+L'DOVKNET),KEYSAVE  STILL REQD NWK?           
         BNE   LRX                 NO - DONE                                    
         BAS   RE,FILTDIR          APPLY DIR FILTERS                            
         BNE   LR30                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,FILTDEMO                                                      
         BNE   LR30                                                             
*                                                                               
         XC    LISTAR,LISTAR                                                    
*      ------------------------------                                           
         XC    BMKTSTA,BMKTSTA             MARKET IS NETWORK = NULL             
         MVC   BSTA(2),DOVKNET             DITTO FOR STATION SEQ                
         GOTO1 MSUNPK,DMCB,BMKTSTA,MYQMKT,MYQSTA  NETWORK (EBCDIC)              
         MVC   LLNET,MYQSTA                                                     
*      ------------------------------                                           
         BAS   RE,GETDEMNM                                                      
         MVC   LLDEMO,SVDEMO                                                    
*      ------------------------------                                           
         MVC   LLSHOW,DOVKPGM      PROGRAM (SHOW)                               
*      ------------------------------                                           
         MVC   LLRATS,=C'BBM'      RATING SERVICE ARB                           
         CLI   DOVKRTS,C'1'        RATING SERVICE OF C'1'?                      
         BE    *+10                                                             
         MVC   LLRATS,=C'NSI'      RATING SERVICE BBM                           
*      ------------------------------                                           
         EDIT  DOVKSEQ,LLSEQ,ZERO=BLANK    SEQUENCE NUMBER                      
*      ------------------------------                                           
         GOTO1 CLUNPK,DMCB,DOVKCLT,LLCLNT                                       
*      ------------------------------                                           
         TM    FILTFLAG,DEMOFILT                                                
         BO    *+10                                                             
         XC    SVDEMOCD,SVDEMOCD                                                
*      ------------------------------                                           
         GOTO1 LISTMON                                                          
         MVC   KEYSAVE,KEY                                                      
*                                                                               
LR30     B     LR15                                                             
*      ------------------------------                                           
LRX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*==========================================================                     
*  READ 00 PROFILE FOR USER DEMO SEQNUMS OPTION                                 
*  AND 00A PROFILE FOR DEFAULT EMPTY FIELD VALUE                                
*==========================================================                     
                                                                                
RDPROF   NTR1                                                                   
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),AGENCY    READ AGENCY LEVEL                            
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,ELEM,DATAMGR                                      
         MVC   SV00UDEM,ELEM+9      SAVE USER DEMO SEQNUM OPT                   
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S00A'                                                 
         NI    WORK,X'BF'          MAKE S LOWERCASE                             
         MVC   WORK+4(2),AGENCY    READ AGENCY LEVEL                            
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,ELEM,DATAMGR                                      
         CLI   ELEM+1,C'L'         IS IT A CHAR 0                               
         BE    *+8                                                              
         MVI   ELEM+1,C'0'         THEN SET TO LOOKUP DEFAULT                   
         MVC   SVDFLTOV(1),ELEM+1   AND SET OVERRIDE VALUE                      
         XC    SVDFLTOV+1(2),SVDFLTOV  AND CLEAR ++                             
*                                                                               
RPX      XIT1                                                                   
         EJECT                                                                  
*==============================================================*                
*  BLDSTAB        BUILD THE STATION AND SPILL MARKET TABLE     *                
*  EXIT - CC SET WRT NUMBER OF TABLE ENTRIES VS MAX ALLOWED                     
*==============================================================*                
BLDSTAB  NTR1                                                                   
         MVC   AIO,AIO2                                                         
*                                                                               
         XC    KEY,KEY                                                          
         USING NDEFRECD,R3                                                      
         USING DOVRECD,R2                                                       
         LA    R3,KEY                                                           
         LA    R2,SAVEKEY                                                       
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,AGENCY                                                  
         MVC   NDEFKNET,DMVNTWK                                                 
         OC    NDEFKNET,=C'    '                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(NDEFKCLT-NDEFKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         DROP  R3                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         LA    R4,STATAB                                                        
         SR    R6,R6               R6=STATAB ENTRY COUNT                        
         USING NDEFEL01,R3                                                      
         MVI   ELCODE,X'01'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         B     *+8                 PICK UP STATIONS UNDER THIS NETWORK          
BS20     BAS   RE,NEXTEL                                                        
         BNE   BSX                                                              
*                                                                               
         CLC   NDEFSTA,=C'ZZZZ'    USED AS A DUMMY STATION                      
         BE    BS20                SKIP THEM                                    
*                                                                               
         XC    MYQSTA,MYQSTA                                                    
         MVC   MYQMKT,=C'3619'                                                  
         CLI   SVNETSEQ,NDEFCABQ                                                
         BE    BS22                                                             
         MVC   MYQSTA(L'NDEFSTA),NDEFSTA                                        
         OC    MYQSTA,=C'    '                                                  
         MVI   MYQSTA+4,C' '                                                    
         B     BS25                                                             
BS22     MVC   MYQSTA(4),DMVNTWK                                                
         OC    MYQSTA,=C'    '                                                  
         MVI   MYQSTA+4,C'/'                                                    
         MVC   MYQSTA+5(2),NDEFMSUF                                             
BS25     GOTO1 MSPACK,DMCB,MYQMKT,MYQSTA,BMKTSTA                                
         AHI   R6,1                                                             
         CHI   R6,MAXELS                                                        
         BH    BSX                                                              
         MVC   0(3,R4),BSTA                                                     
         LA    R4,3(R4)                                                         
         BAS   RE,ADDSPILL         GET SPILL MKTS FOR STATION                   
         BH    BSX                                                              
         B     BS20                                                             
*                                                                               
BSX      MVC   0(2,R4),=X'FFFF'         DENOTES END OF TABLE                    
         MVC   AIO,AIO1                                                         
         CHI   R6,MAXELS           SET CC                                       
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*================================================================*              
*  ADDSPILL  ADD THE SPILL MARKETS TO STATION/SPILL MARKET TABLE *              
* --- R4 POINTS TO NEXT AVAILABLE SPACE IN THE TABLE                            
* --- R6 IS COUNT OF TABLE ENTRIES                                              
*================================================================*              
ADDSPILL NTR1                                                                   
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         USING DOVRECD,R2                                                       
         USING SDEFRECD,R3                                                      
         LA    R2,SAVEKEY                                                       
         LA    R3,KEY                                                           
         MVC   SDEFKTYP,=X'0D13'     CHECK FOR SPILL RECS                       
         MVC   SDEFKAGY,AGENCY                                                  
         MVC   SDEFKRSV,DOVKRTS       RATING SERVICE                            
         MVC   SDEFKSTA(4),MYQSTA                                               
*ALWAYS  MVC   SDEFKCLT,BCLT       CHECK FOR CLIENT LEVEL                       
*USE     GOTO1 HIGH                WORK HAS STATION                             
*AGENCY  CLC   KEY(13),KEYSAVE                                                  
*LEVEL   BE    ASP04               FOUND CLIENT LEVEL                           
*SPILL   XC    KEY,KEY                                                          
*11MAR03 MVC   KEY(10),KEYSAVE     LEAVE OUT CLIENT                             
         GOTO1 HIGH                WORK HAS STATION                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   ASP100              NO SPILLS                                    
         DROP  R2,R3                                                            
ASP04    GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         USING SDEFEL05,R3                                                      
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
AS10     BAS   RE,NEXTEL           ANY MORE SPILLS?                             
         BNE   ASP100              NO                                           
*                                                                               
         AHI   R6,1                                                             
         CHI   R6,MAXELS           WILL THIS 'BREAK THE BANK'?                  
         BH    ASP100                                                           
         XC    0(3,R4),0(R4)                                                    
         MVC   1(2,R4),SDEFAMKT    STORE SPILL MKT                              
         LA    R4,3(R4)                                                         
         B     AS10                                                             
*                                                                               
ASP100   MVI   ELCODE,X'01'        RESET ELCODE                                 
         MVC   AIO,AIO2                                                         
         CHI   R6,MAXELS                                                        
         XIT1  REGS=(R4,R6)        LEAVE CHANGES TO R4 & R6                     
         DROP  R3                                                               
         EJECT                                                                  
*==============================================================*                
*  ADD05        BUILD THE STATION/SPILL MKT ELEMENTS           *                
*=============================================================                  
                                                                                
ADD05    NTR1                                                                   
         L     R6,AIO1                                                          
         AHI   R6,24                                                            
         SR    R0,R0                                                            
         IC    R0,1(R6)            POINT TO 01 ELEM                             
         AHI   R0,-12                                                           
         BNP   ADD0504                                                          
*                                                                               
         SRDL  R0,32                                                            
         D     R0,=F'3'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1               NUMBER OF DEMOS IN LIST                      
*                                                                               
         LA    R1,DOVDLSTC-DOVEL01(R6) POINT TO DEMO LIST                       
*                                                                               
         MVC   WORK(10),=10C'N'    PRESET TABLE TO VALUE NOT INPUT              
         LA    R4,WORK                                                          
*                                                                               
ADD0502  TM    0(R1),X'80'                                                      
         BZ    *+8                                                              
         MVI   0(R4),C'Y'                                                       
*                                                                               
         AHI   R1,3                                                             
         AHI   R4,1                                                             
         BCT   R0,ADD0502                                                       
                                                                                
ADD0504  L     R0,AIO3             MOVE ORIGINAL RECORD TO IO3                  
         LHI   R1,2000                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         MVCL  R0,RE                                                            
*  --------------------------------------------------------------               
         L     R3,AIO1             GET POSITION WHERE TO PUT 05'S               
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         LR    R4,R3                                                            
*      ------------------------------                                           
         MVC   AIO,AIO1            REMOVE ALL X'05' ELEMENTS                    
         GOTO1 REMELEM                                                          
         MVC   AIO,AIO3                                                         
*      ------------------------------                                           
         L     R3,AIO3             AIO3 HAS ORIGINAL RECORD                     
         LA    R3,24(R3)                                                        
         LA    R5,STATAB           TABLE OF STATIONS                            
         LA    R6,MAXELS+1         MAX STATION/SPILL MKTS ON SCREEN             
*                                  (+1 FOR EOT ENTRY)                           
         MVI   ELCODE,X'05'                                                     
*                                                                               
ADD0505  CLC   0(2,R5),=X'FFFF'    END OF STATION/SP MKT TABLE?                 
         BE    ADD05X              YES                                          
*                                                                               
         CLI   0(R5),0             STATION OR SP MKT?                           
         BNE   *+16                SPILL                                        
*                                                                               
         CLI   0(R3),0             STATION NOT FOUND IN ORIG REC                
         BE    ADD0520             JUST ADD ITS SPILL MARKETS                   
         B     ADD0510                                                          
*                                                                               
         L     R3,AIO3                                                          
         USING DOVEL05,R3                                                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
ADD0510  BAS   RE,NEXTEL           IS STA/SP MKTS IN ORIG REC?                  
         BNE   ADD0520             NO, ADD NEW ELEM                             
*                                                                               
         CLI   0(R5),0             STATION OR SP MKT?                           
         BNE   *+12                SPILL                                        
         CLI   DOVSTA,0                                                         
         BNE   ADD0520                                                          
*                                                                               
         CLC   DOVSTA,0(R5)        DOES ELEM ALREADY EXIST?                     
         BNE   ADD0510             DON'T KNOW TRY NEXT                          
*      ------------------------------                                           
         XC    ELEM,ELEM           COPY THE ELEMENT                             
         ZIC   RE,1(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R3)                                                 
* CALCULATE ELEMENT LENGTH FROM NUMBER OF DEMOS AND PUT IT IN                   
         ZIC   R0,DEMONUM                                                       
         MHI   R0,2                                                             
         AHI   R0,5                                                             
         STC   R0,ELEMENT+1                                                     
*                                                                               
         GOTO1 RECUP,DMCB,(0,AIO1),ELEM,(C'R',(R4))                             
         CLI   8(R1),0                                                          
         BE    ERRBIG                                                           
*                                                                               
         ZIC   R0,1(R4)            LENGTH OF ELEMENT                            
         AR    R4,R0               POINT TO NEXT PLACE FOR EL INSERT            
*                                                                               
         LA    R5,3(R5)            NEXT ENTRY IN STA/SP MKT TABLE               
         BCT   R6,ADD0505                                                       
*      ------------------------------                                           
         DC    H'0'                TOO MANY ELEMS                               
*      ------------------------------                                           
ADD0520  XC    ELEMENT,ELEMENT     BUILD ELEM AND ADD TO NEW REC                
         MVI   ELEMENT,X'05'                                                    
* CALCULATE LENGTH OF ELEMENT FROM NUMBER OF DEMOS SAVED PREVIOUSLY             
         ZIC   R0,DEMONUM                                                       
         AR    R0,R0               X2                                           
         AHI   R0,5                                                             
         STC   R0,ELEMENT+1                                                     
* DETERMINE TO FILL WITH X'8000' OR X'0000'                                     
         CLI   SVDFLTOV,C'L'       TEST DEFAULT TO LOOKUP                       
         BE    ADD0530                                                          
*                                                                               
         LA    RE,ELEMENT+5                                                     
         SR    R0,R0                                                            
         IC    R0,DEMONUM                                                       
         LA    R1,WORK             POINT TO LIST OF INPUT FLAGS                 
*                                                                               
ADD0525  CLI   0(R1),C'Y'                                                       
         BNE   *+8                                                              
         OI    0(RE),X'80'                                                      
*                                                                               
         AHI   R1,1                                                             
         AHI   RE,2                                                             
         BCT   R0,ADD0525                                                       
*                                                                               
ADD0530  MVC   ELEMENT+2(L'DOVSTA),0(R5)                                        
         GOTO1 RECUP,DMCB,(0,AIO1),ELEM,(C'R',(R4))                             
         CLI   8(R1),0                                                          
         BE    ERRBIG                                                           
         ZIC   R0,1(R4)            LENGTH OF ELEMENT                            
         AR    R4,R0               POINT TO NEXT PLACE FOR EL INSERT            
*                                                                               
         LA    R5,3(R5)            BUMP TO NEXT TABLE ENTRY                     
         BCT   R6,ADD0505                                                       
*      ------------------------------                                           
         DC    H'0'                TOO MANY ELEMS                               
         DROP  R3                                                               
*  --------------------------------------------------------------               
ADD05X   B     EXIT                                                             
MAXELS   EQU   DOV5MAXQ                                                         
         EJECT                                                                  
*==============================================================*                
*  GETDEMNM   GET DEMO NAME FROM DEMO CODE                     *                
*==============================================================*                
GETDEMNM NTR1                                                                   
*                                                                               
         CLI   SV00UDEM,C'Y'       SPECIAL USER DEMO OPTION                     
         BNE   GD10                                                             
         CLI   SVDEMOCD+1,X'21'                                                 
         BNE   GD10                                                             
         XC    SVDEMO,SVDEMO                                                    
         MVI   SVDEMO,C'U'         U THEN CHAR                                  
         MVC   SVDEMO(1),SVDEMOCD+2                                             
         B     GDX                                                              
*                                                                               
GD10     XC    ELEM,ELEM                                                        
         XC    WORK,WORK                                                        
         LA    R5,ELEM                                                          
         USING DBLOCK,R5                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'C'          CANADIAN                                  
         GOTO1 DEMOCON,DMCB,(1,SVDEMOCD),(2,WORK),(C'S',DBLOCK),0               
         DROP  R5                                                               
         MVC   SVDEMO,WORK                                                      
*                                                                               
         CLC   =C'USER',WORK           USER DEMO?                               
         BNE   GDX                                                              
         MVC   SVDEMO(3),=C'U /'       FORMAT - U#/                             
         MVC   SVDEMO+1(1),WORK+4      NUMBER AFTER 'USER' IN AIO2              
         MVC   SVDEMO+3(4),WORK        MOVE 'USER' ONTO SCREEN                  
*                                                                               
GDX      B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* APPLY DIRECTORY KEY FILTERS                                        *          
* EXIT  - CC NEQ IF FAIL FILTERS                                     *          
**********************************************************************          
         SPACE                                                                  
FILTDIR  NTR1  ,                                                                
         LA    R4,KEY                                                           
         USING DOVRECD,R4                                                       
*                                                                               
         SR    RF,RF               FILTER BY SHOW                               
         ICM   RF,1,LSTSHOWH+FHILD                                              
         BZ    FILT25              NO                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   LSTSHOW(0),DOVKPGM                                               
         BNE   FLTX                                                             
*                                                                               
FILT25   CLI   LSTRATSH+FHILD,0    RATING SERVICE                               
         BE    *+14                                                             
         CLC   SVRATS,DOVKRTS                                                   
         BNE   FLTX                                                             
*                                                                               
         CLI   LSTCLNTH+FHILD,0    CLIENT?                                      
         BE    FLTX                                                             
         CLI   LSTCLNT,C'>'        IS CLIENT A START AT FILTER?                 
         BE    FLTX                                                             
         CLC   BCLT,DOVKCLT                                                     
FLTX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*==============================================================*                
*  FILTDEMO    FILTER THE RECORD BY DEMO                       *                
*  ENTRY - R3=AIO                                              *                
*  EXIT  - NEQ IF FAIL FILTER                                  *                
*==============================================================*                
*                                                                               
FILTDEMO NTR1  ,                                                                
         LA    R3,24(R3)                                                        
         USING DOVEL01,R3                                                       
*                                                                               
         NI    DEMFLG,X'FF'-DEMFLT                                              
         CLI   LSTDEMOH+5,0        NO DEMO FILT SHOW FIRST                      
         BNE   FD02                                                             
         SR    RF,RF                                                            
         IC    RF,1(R3)            ELEM LENGTH                                  
         SH    RF,=H'12'           SUBTRACT DATE FLDS + ELEM ID + LEN           
         BNP   FD10                NO DEMOS IN ELEM                             
         SR    RE,RE                                                            
         D     RE,=F'3'            SET NUM DEMOS SLOTS IN RF                    
         LA    R1,DOVDLSTC                                                      
FD01     MVC   SVDEMOCD,0(R1)                                                   
         OC    SVDEMOCD,SVDEMOCD                                                
         BNZ   FDX                 FOUND FIRST DEFINED DEMO                     
***      LA    R1,3(R1)            *** ONLY SHOW IF HAS 1ST DEMO SINCE          
***      BCT   RF,FD01             *** =BUY ONLY PULLS RTGS IF 1ST DEMO         
         B     FD10                ALL DEMOS ARE NULL (DELETED)                 
*                                                                               
FD02     OI    FILTFLAG,DEMOFILT                                                
         OI    DEMFLG,DEMFLT                                                    
         ZIC   RF,1(R3)            ELEM LENGTH                                  
         SH    RF,=H'12'           SUBTRACT DATE FLDS + ELEM ID + LEN           
         BNP   FD10                MEANS DEMD ELEM HAD NO DEMOS                 
         SR    RE,RE                                                            
         D     RE,=F'3'                                                         
         LR    R6,RF               SET R6 FOR BCT                               
*      --------------------------                                               
         LA    R5,DOVDLSTC                                                      
         SR    RE,RE               CLEAR DEMO COUNTER                           
FD05     MVC   WORK(3),0(R5)                                                    
         NI    WORK,X'FF'-X'80'    SET OFF HIGH ORDER BIT                       
         CLC   SVDEMOCD,WORK       NOW MATCH DEMOS                              
         BE    FDX                 FOUND - OK                                   
         LA    RE,1(RE)                                                         
         LA    R5,3(R5)                                                         
         BCT   R6,FD05                                                          
FD10     LTR   RB,RB                                                            
         B     *+6                                                              
FDX      CR    RB,RB                                                            
         B     EXIT                                                             
***********************************************************************         
*  CALCULATE THE POSITION OF THE DEMO                                 *         
*  ENTRY - SVDEMOCD IS DEMO LOOKING FOR                               *         
*  EXIT  - SVDEMNUM = POSITION# OF DEMO                               *         
*        - SEQUENCE RECORD READ IF DEMO ON IT                         *         
*        - PFKEY SCROLLING DETAILS SET ALONG WITH LINE24 PFK TEXT     *         
***********************************************************************         
DEMOPOSN NTR1  ,                                                                
DP01     L     R3,AIO                                                           
         MVI   ELCODE,X'01'        01 ELEMENT USED TO FIND DEMO                 
         USING DOVEL01,R3          POSITION                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,1(R3)            ELEM LENGTH                                  
         SH    RF,=H'12'           SUBTRACT DATE FLDS + ELEM ID + LEN           
         BNP   DEMERR              MEANS DEMD ELEM HAD NO DEMOS                 
         SR    RE,RE                                                            
         D     RE,=F'3'                                                         
         LR    R6,RF               R6=#DEMOS PRESENT (FOR BCT)                  
*                                                                               
         LA    R5,DOVDLSTC                                                      
         SR    RE,RE               CLEAR DEMO COUNTER                           
         MVC   SV1DEMCD,0(R5)                                                   
         NI    SV1DEMCD,255-X'80'                                               
         MVC   SVPDEMCD,SV1DEMCD                                                
         XC    SVNDEMCD,SVNDEMCD                                                
DP05     MVC   WORK(3),0(R5)                                                    
         NI    WORK,X'FF'-X'80'    SET OFF HIGH ORDER BIT                       
         CLC   SVDEMOCD,WORK       NOW MATCH DEMOS                              
         BE    DP10                FOUND - OK                                   
         OC    WORK(3),WORK        DEMO NULL (DELETED)?                         
         BZ    DP07                IGNORE THIS POSITON                          
         OC    SV1DEMCD,SV1DEMCD   TEST PREVIOUS DEMOS WERE DELETED             
         BNZ   *+10                NO                                           
         MVC   SV1DEMCD,WORK       YES - TAKE THIS AS FIRST DEMO                
         MVC   SVPDEMCD,WORK                                                    
DP07     LA    RE,1(RE)                                                         
         LA    R5,3(R5)                                                         
         BCT   R6,DP05                                                          
         DROP  R3                                                               
*      ---------------------------                                              
         LA    R3,KEY                                                           
         USING DOVRECD,R3                                                       
         CLI   DOVKSEQ,0           TEST ON FIRST DEMO RECORD                    
         BNE   DEMERR              - NOT FOUND                                  
         MVI   DOVKSEQ,1           TRY FOR SEQ RECORD                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DEMERR                 SEQ RECORD NOT FOUND                      
         MVI   SAVEKEY+DOVKSEQ-DOVKEY,1                                         
         GOTO1 GETREC                                                           
         B     DP01                                                             
*      -----------------------------                                            
DEMERR   LA    R2,DMVDEMOH                                                      
         B     ERRINVD                                                          
*      -----------------------------                                            
DP10     STH   RE,SVDEMNUM         SAVE POSITION OF DEMO                        
         CLI   ACTNUM,ACTDIS                                                    
         BNE   DP20                                                             
DP12     LA    RE,1(RE)            SET NEXT DEMO FOR SCROLLING                  
         LA    R5,3(R5)                                                         
         BCT   R6,DP15                                                          
         B     DP20                NO MORE DEMOS                                
DP15     MVC   SVNDEMCD,0(R5)                                                   
         NI    SVNDEMCD,255-X'80'  SET OFF HIGH ORDER BIT                       
         OC    SVNDEMCD,SVNDEMCD                                                
         BZ    DP12                THIS ONE NULL (DELETED)                      
*                                                                               
DP20     CLI   ACTNUM,ACTDIS       ACTION DISPLAY                               
         BNE   DP30                                                             
         OC    SV1DEMCD,SV1DEMCD                                                
         BZ    DP25                FIRST ITEM NULL                              
         CLC   SV1DEMCD,SVDEMOCD                                                
         BE    DP25                AT FIRST ITEM                                
         MVC   DMVLN24+LN24SCRF-LN24D(L'PF06FRST),PF06FRST                      
         MVC   DMVLN24+LN24SCRP-LN24D(L'PF07PREV),PF07PREV                      
DP25     OC    SVNDEMCD,SVNDEMCD                                                
         BZ    *+10                NO NEXT                                      
         MVC   DMVLN24+LN24SCRN-LN24D(L'PF08NEXT),PF08NEXT                      
         OI    DMVLN24H+6,X'80'    TRANSMIT THE RESULT                          
*                                  ENSURE PCPAK GIVES PFKEY BUTTONS             
DP30     L     RF,ASPOOLD                                                       
         CLC   DMVLN24+LN24PFKT-LN24D(L'LN24PFKT),SPACES-SPOOLD(RF)             
         BNE   DPX                 IF 1ST SLOT EMPTY BUTTONS NOT APPEAR         
         LA    R1,DMVLN24+LN24SCRF-LN24D  FIND FIRST DEFINED SLOT               
         CLC   0(L'LN24PFKT,R1),SPACES-SPOOLD(RF)                               
         BNE   DP35                                                             
         LA    R1,DMVLN24+LN24SCRP-LN24D                                        
         CLC   0(L'LN24PFKT,R1),SPACES-SPOOLD(RF)                               
         BNE   DP35                                                             
         LA    R1,DMVLN24+LN24SCRN-LN24D                                        
         CLC   0(L'LN24PFKT,R1),SPACES-SPOOLD(RF)                               
         BNE   DP35                                                             
         LA    R1,DMVLN24+LN24RETN-LN24D                                        
         CLC   0(L'LN24PFKT,R1),SPACES-SPOOLD(RF)                               
         BE    DPX                                                              
DP35     MVC   DMVLN24+LN24PFKT-LN24D(L'LN24PFKT),0(R1) MOVE TXT TO             
         MVC   0(L'LN24PFKT,R1),SPACES-SPOOLD(RF)       1ST SLOT                
DPX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*==============================================================*                
*  MRKDEMO      DEMO IS MARKED WHEN IT HAS DEMO VALUES UNDER IT*                
*==============================================================*                
MRKDEMO  NTR1                                                                   
         L     R3,AIO                                                           
         USING DOVRECD,R3                                                       
         LA    R6,DOVDLSTC                                                      
         LA    R5,3                LENGTH OF EACH DEMO CODE                     
         MH    R5,SVDEMNUM                                                      
         LA    R6,0(R5,R6)                                                      
         OI    0(R6),X'80'     X'80' SIGNIFIES PRESENCE OF A DEM VAL            
         TM    FILTFLAG,VALYES                                                  
         BO    *+8                                                              
         NI    0(R6),X'FF'-X'80'   NO DEM VAL PRESENT                           
*                                                                               
         NI    FILTFLAG,X'FF'-VALYES                                            
         B     EXIT                                                             
*==============================================================*                
*  GETDVL          GET THE DEMO VALUE FROM LIST                *                
*==============================================================*                
                                                                                
GETDVL   NTR1                                                                   
         USING DOVEL05,R3                                                       
         LA    R6,DOVDEMV                                                       
         LA    R5,L'DOVDEMV                                                     
         MH    R5,SVDEMNUM                                                      
         AR    R6,R5                                                            
         MVC   SVDEMVAL,0(R6)                                                   
         STH   R5,SVDVALIX                                                      
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*==============================================================*                
*  VALINET         VALIDATE THE NETWORK                        *                
* ENTRY - R2=A(NETWORK FIELD HDR)                              *                
* EXIT  - SVNETSEQ=NETWORK SEQUENCE NUMBER                     *                
*       - BSTA SET (NOT IF DISPKEY - ALREADY GOT)              *                
*==============================================================*                
                                                                                
VALINET  NTR1                                                                   
         OC    8(4,R2),=C'    '                                                 
         LA    R8,KEY                                                           
         USING NDEFRECD,R8                                                      
         XC    KEY,KEY                                                          
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,AGENCY                                                  
         MVC   NDEFKNET,8(R2)                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(NDEFKCLT-NDEFKEY),KEYSAVE                                    
         BNE   ERRINV                                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'       SEQNUM ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVNETSEQ,NDEFNET-NDEFEL02(R3)                                    
*                                                                               
         CLI   MODE,DISPKEY        DKEY ALREADY GOT BSTA - SAVE I/O             
         BE    VN10        (NOTE: SET USEIONUM>1 IF EVER CALL FOR DKEY)         
         GOTO1 VALISTA             VALIDATE STATION (NEED BSTA)                 
VN10     XC    KEY,KEY                                                          
*                                                                               
VNX      B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
*==============================================================*                
*  VALISHOW       VALIDATE THE PROGRAM (SHOW)                  *                
*==============================================================*                
VALISHOW NTR1                                                                   
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING NPGMRECD,R3                                                      
         MVC   NPGMKTYP,=X'0D12'   PROGRAM RECORDS                              
         MVC   NPGMKAGY,AGENCY     AGENCY ID                                    
         MVC   NPGMKNET,DMVNTWK    NETWORK ON SCREEN                            
         OC    NPGMKNET,=C'    '                                                
         MVC   NPGMKID,DMVSHOW     NETWORK ON SCREEN                            
         OC    NPGMKID,=C'    '                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(NPGMLEN-NPGMKEY),KEYSAVE                                     
         BNE   ERRINV                                                           
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   SVFULLSH,NPGMPGM                                                 
         GOTO1 UNDAY,DMCB,NPGMDAY,SVDAY                                         
         XC    SVTIME,SVTIME                                                    
         GOTO1 UNTIME,DMCB,NPGMSTR,SVTIME                                       
*                                                                               
VSX      MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*==============================================================*                
*  VALIRATS       VALIDATE THE RATING SERVICE                  *                
*  ENTRY - R2=A(FLDHDR)                                                         
*==============================================================*                
VALIRATS NTR1                                                                   
         MVI   SVRATS,C'1'         RATING SERVICE OF C'1'                       
VRATS10  CLC   =C'BBM',FHDAD(R2)   RATING SERVICE BBM?                          
         BE    VRATSX              YES                                          
         MVI   SVRATS,C'0'                                                      
         CLC   =C'NSI',FHDAD(R2)   RATING SERVICE NSI?                          
         BNE   ERRINV              YES                                          
VRATSX   B     EXIT                                                             
         EJECT                                                                  
*=========================================================*                     
*  VALIDEMO       VALIDATE DEMO                           *                     
*=========================================================*                     
* VALIDATES DEMO AND RETURNS CONVERTED DEMO CODE IN SVDEMOCD                    
VALIDEMO NTR1                                                                   
*                                                                               
         CLI   SV00UDEM,C'Y'                                                    
         BNE   VDEMO10                                                          
         CLI   8(R4),C'U'                                                       
         BNE   VDEMO10                                                          
         MVI   SVDEMOCD,0                                                       
         MVI   SVDEMOCD+1,X'21'    USER DEMOS                                   
         MVC   SVDEMOCD+2(1),9(R4)                                              
         B     VALDEMOX                                                         
*                                                                               
VDEMO10  L     R5,AIO1                                                          
         XC    ELEMENT,ELEMENT                                                  
         XC    0(250,R5),0(R5)                                                  
         LA    R6,ELEMENT                                                       
         USING DBLOCK,R6                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'C'       CANADIAN                                     
         MVC   DBFILE,=C'TPT'                                                   
         MVC   DMCB+4(4),=X'D9000AD9' DEMVAL                                    
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,(R4)),(1,DUB),(C'S',DBLOCK),0(R5)                   
         CLI   DMCB+4,0                                                         
         BE    ERRINVD                                                          
         MVC   SVDEMOCD,DUB                                                     
*                                                                               
VALDEMOX B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*==============================================================*                
*  SCROLL DEMOS                                                                 
*==============================================================*                
DEMSCROL NTR1  ,                                                                
         CLC   SVDEMOCD,SVDEMOC2   HAS DEMO BEEN CHANGED                        
         BNE   DSCRX                                                            
         LA    RF,SV1DEMCD                                                      
         CLI   PFKEY,6                                                          
         BL    DSCRX                                                            
         BE    DSCR10                                                           
         LA    RF,SVPDEMCD                                                      
         CLI   PFKEY,8                                                          
         BL    DSCR10                                                           
         BH    DSCRX                                                            
         LA    RF,SVNDEMCD                                                      
DSCR10   OC    0(L'SV1DEMCD,RF),0(RF)                                           
         BZ    *+10                                                             
         MVC   SVDEMOCD,0(RF)                                                   
         BAS   RE,GETDEMNM                                                      
         MVC   DMVDEMO(L'SVDEMO),SVDEMO                                         
         OI    DMVDEMOH+6,X'80'    TRANSMIT                                     
DSCRX    B     EXIT                                                             
*==============================================================*                
ERRMISS  MVI   ERROR,MISSING       ERRORS                                       
         B     ERRX                                                             
ERRINV   MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
ERRINVD  MVC   ERRNUM,=AL2(INVDEM)    ERROR: INVALID DEMO                       
         B     SPERREX                                                          
ERRBIG   MVC   ERRNUM,=AL2(ERRRECOV)  RECORD TOO BIG                            
         B     SPERREX                                                          
ERR05S   MVC   ERRNUM,=AL2(9)      TOO MANY STATIONS/SPILL MKTS                 
         LA    R2,DMVNTWKH                                                      
         XC    ACURFORC,ACURFORC   CLEAR CURSOR O/R                             
         B     SPERREX                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
ERRX     MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
*==============================================================*                
         DROP  RF                                                               
INVDEM   EQU   388                 INVALID DEMO                                 
ERRRECOV EQU   58                                                               
*==============================================================*                
         GETEL R3,DATADISP,ELCODE                                               
*==============================================================*                
         EJECT                                                                  
*==============================================================*                
*              SETUP FOR RETURN PF12                                            
*==============================================================*                
SETUP    NTR1  ,                                                                
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    SETUPX              NO PFKEYS ON LIST                            
*                                                                               
         CLI   PFKEY,12                                                         
         BE    SETUP99                                                          
*                                                                               
* SET LINE24 PFKEY HELP TEXT                                                    
         L     RF,ASPOOLD                                                       
         MVC   DMVLN24,SPACES-SPOOLD(RF)    CLEAR PFK TEXT                      
         OI    DMVLN24H+6,X'80'    TRANSMIT THE RESULT                          
         CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    SETUP00             NO                                           
         CLI   CALLSTCK,X'A7'      IF FROM DEMODEF THEN DON'T SHOW PF4          
         BE    *+10                                                             
SETUP00  MVC   DMVLN24(L'PF04DDEF),PF04DDEF                                     
*                                                                               
* HANDLE PFKEY SPECIFICS                                                        
SETUP04  CLI   PFKEY,4             PF4 TO DEMODEF                               
         BNE   SETUP12                                                          
*                                                                               
         MVC   MPF04ACT,=C'        '                                            
         CLI   ACTNUM,ACTSEL       ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF04ACT,=CL8'DISPLAY'                                           
         CLI   CALLSP,0            DID WE ORIGINALLY COME FROM SHOWDEF          
         BE    SETUP12                                                          
         CLI   CALLSTCK,X'F0'      (SHOWDEF SCREEN NUMBER)                      
         BNE   SETUP12                                                          
         NI    MPF04ST1,255-PFTCPROG  DON'T PUSH SO RETURN TO SHOWDEF           
*                                                                               
SETUP12  CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    *+10                NO                                           
         MVC   DMVLN24+LN24RETN-LN24D(L'PF12RET),PF12RET                        
         OC    PFKEY,PFKEY                                                      
         BZ    SETUPX                                                           
SETUP99  LA    RF,MPFTABLE                                                      
*        CLI   ACTNUM,ACTDIS                                                    
*        BNE   *+8                                                              
         LA    RF,DPFTABLE                                                      
         GOTO1 INITPFKY,DMCB,(RF)                                               
SETUPX   B     EXIT                                                             
*                                                                               
*==============================================================*                
* DEMCOUNT, THE SUBROUTINE TO COUNT NUMBER OF DEMOS                             
* DEMONUM WILL HAVE NUMBER OF DEMOS AT RETURN                                   
*==============================================================*                
DEMCOUNT NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         USING DOVEL01,R3                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,DOVDLSTC                                                      
         LR    RF,R4               POINTER TO LAST DEMO                         
         LR    RE,R4               SAVE ADDRESS OF DEMO LIST                    
         LHI   R0,DOVDMAXQ         NO MORE THAN 10 DEMOS                        
*                                                                               
         ZIC   R1,DOVEL01+1        LENGTH OF ELEMENT                            
         LA    R1,0(R1,R3)         A(BYTE FOLLOWING '01' ELEMENT)               
*                                                                               
DEMC10   DS    0H                                                               
         CR    R4,R1                                                            
         BNL   DEMC20                                                           
         CLC   0(3,R4),=XL3'0'                                                  
         BE    *+6                                                              
         LR    RF,R4               DEMO AT THIS POSITION                        
         AHI   R4,3                ADVANCE TO NEXT DEMO                         
         BCT   R0,DEMC10                                                        
*                                  ALL 10 POSITIONS SCANNED                     
DEMC20   DS    0H                                                               
         SR    RF,RE               NO. OF BYTES TO LAST DEMO                    
         SR    RE,RE                                                            
         D     RE,=F'3'            NUMBER OF DEMOS                              
         AHI   RF,1                                                             
         STC   RF,DEMONUM                                                       
*                                                                               
         J     EQXIT                                                            
         DROP  R3                                                               
*                                                                               
*                                                                               
*==============================================================*                
DPFTABLE DS    0X                  DISPLAY ONLY PFKEYS                          
* PF06 = FIRST DEMO                                                             
         DC    AL1(MPF06X-*,06)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0,PFTRETRN)                                                  
         DC    CL3' '              NOT SELECTABLE                               
         DC    CL8'       '        RECORD:                                      
MPF06ACT DC    CL8'       '        ACTION:                                      
MPF06X   EQU   *                                                                
* PF07 = PREV DEMO                                                              
         DC    AL1(MPF07X-*,07)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0,PFTRETRN)                                                  
         DC    CL3' '              NOT SELECTABLE                               
         DC    CL8'       '        RECORD:                                      
MPF07ACT DC    CL8'       '        ACTION:                                      
MPF07X   EQU   *                                                                
* PF08 = NEXT DEMO                                                              
         DC    AL1(MPF08X-*,08)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0,PFTRETRN)                                                  
         DC    CL3' '              NOT SELECTABLE                               
         DC    CL8'       '        RECORD:                                      
MPF08ACT DC    CL8'       '        ACTION:                                      
MPF08X   EQU   *                                                                
*                                                                               
MPFTABLE DS    0X                  MAINT PFKEYS                                 
* PF04 = DEMDEF                                                                 
         DC    AL1(MPF04X-*,04)                                                 
MPF04ST1 DC    AL1(PFTCPROG)                                                    
         DC    AL1((MPF04X-MPF04K)/KEYLNQ,0)                                    
         DC    CL3'   '                                                         
         DC    CL8'DEMDEF '        RECORD: DEMDEF                               
MPF04ACT DC    CL8'       '        ACTION:                                      
MPF04K   DC    AL1(KEYTYTWA,L'DMVNTWK-1),AL2(DMVNTWK-T217FFD)                   
         DC    AL1(KEYTYTWA,L'DMVSHOW-1),AL2(DMVSHOW-T217FFD)                   
         DC    AL1(KEYTYTWA,L'DMVRATS-1),AL2(DMVRATS-T217FFD)                   
         DC    AL1(KEYTYTWA,L'DMVCLT-1),AL2(DMVCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'DMVSEQ-1),AL2(DMVSEQ-T217FFD)                     
MPF04X   EQU   *                                                                
* PF12 = RETURN TO CALLER                                                       
         DC    AL1(RETCALL-*,12,PFTRPROG,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
RETCALL  EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
* PFKEY TEXT MIXCASE CONSTANTS                                                  
PF04DDEF DC    X'D7C6F47EC48594C48586'       PF4=DEMDEF                         
PF06FRST DC    X'D7C6F67EF1A2A340C4859496'   PF6=1ST DEMO                       
PF07PREV DC    X'D7C6F77ED79985A540C4859496' PF7=PREV DEMO                      
PF08NEXT DC    X'D7C6F87ED585A7A340C4859496' PF8=NEXT DEMO                      
PF12RET  DC    X'D7C6F1F27ED985A3A49995'     PF12=RETURN                        
         EJECT                                                                  
*==============================================================*                
         EJECT                                                                  
MAXSTLIN DC    F'11'                                                            
LISTMSG1 DC    X'E2A381A3899695A2408199854086999694408187859583A8409385>        
               A5859340D585A3C485864B'                                          
* M/C 'STATIONS ARE FROM AGENCY LEVEL NETDEF.'                                  
LISTMSG2 DC    X'405CD5D5D5D54081998540A297899393409481999285A3A2'              
* M/C ' *NNNN ARE SPILL MARKETS'                                                
LISTMSG3 DC    X'E2A4868689A785A2408199854086999694408187859583A8409385>        
               A5859340C38293D492A34B'                                          
* M/C 'SUFFIXES ARE FROM AGENCY LEVEL CBLMKT.'                                  
         LTORG                                                                  
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMA4D     LIST SCREEN                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMA1D     MAINT SCREEN                                      
         EJECT                                                                  
         ORG   DMVWORK                                                          
*==============================================================*                
*                  WORK IN TWA                                 *                
*==============================================================*                
STATAB   DS    89XL3                                                            
NETTAB   DS    63CL5                                                            
*==============================================================*                
       ++INCLUDE DDGENTWA                                                       
*==============================================================*                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - -                                         
       ++INCLUDE SPSFMWORKD                                                     
         ORG   SYSSPARE                                                         
SAVEKEY  DS    XL(L'KEY)                                                        
SAVEKEY2 DS    XL(L'KEY)                                                        
SAVEKEY3 DS    XL(L'KEY)                                                        
SVFULLSH DS    CL17                FOR SHOW NAME/DESCRIPTION                    
SVDAY    DS    CL8                 FOR DAYS OF SHOW                             
SVTIME   DS    CL11                FOR SHOW START TIME                          
SVNETSEQ DS    X                                                                
SVRATS   DS    C                                                                
SVDEMOCD DS    XL3                                                              
SVDEMOC2 DS    XL3                                                              
SV1DEMCD DS    XL3                 1ST (DEFINED) DEMO - PF SCROLL               
SVPDEMCD DS    XL3                 PREV DEMO          - PF SCROLL               
SVNDEMCD DS    XL3                 NEXT DEMO          - PF SCROLL               
SVDEMO   DS    XL7                                                              
SVDEMNUM DS    H                                                                
SVDEMVAL DS    H                                                                
SVDVALIX DS    H                                                                
SVESTA   DS    CL5                                                              
DEMONUM  DS    X                                                                
SV00UDEM DS    CL1                 Y FOR SPECIAL USER DEMO OPTION               
MYQSTA   DS    CL8                                                              
MYQMKT   DS    CL4                                                              
*                                                                               
ERRNUM   DS    XL2                                                              
TMPFLD   DS    CL9                                                              
SVADDR   DS    A                                                                
FILTFLAG DS    X                   RECORD FILTER FLAG                           
VALYES   EQU   X'20'                                                            
DEMOFILT EQU   X'01'                                                            
KEYFLAG  DS    0X                                                               
DEMFLG   DS    X                   RECORD FILTER FLAG                           
DEMFLT   EQU   X'80'               FILTER THIS RECORD OUT                       
KEYDIFF  EQU   X'04'                                                            
SVDFLTOV DS    CL3                 OVERRIDE DEFAULT VALUE AND (+(+))            
NEWDEMO  DS    C                                                                
* - - - - - - - - - - - - - - - - - - -                                         
         EJECT                                                                  
       ++INCLUDE SPGENDOV         NETWORK DEFINITION DSECT                      
* SPGENCLT                                                                      
* SPGENNDEF                                                                     
* SPGENNPGM                                                                     
* SPGENSDEF                                                                     
* DDFLDIND                                                                      
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* DEDBLOCK                                                                      
* DDCOMFACS                                                                     
* FATIOB                                                                        
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT               CLIENT DSECT                            
       ++INCLUDE SPGENNDEF                                                      
       ++INCLUDE SPGENNPGM                                                      
       ++INCLUDE SPGENSDEF               SPILL DEFN RECORD                      
       ++INCLUDE DDFH                                                           
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAXTRAINF                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
* - - - - - - - - - -                                                           
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL5                                                              
LLNET    DS    CL4                                                              
         DS    CL5                                                              
LLSHOW   DS    CL4                                                              
         DS    CL4                                                              
LLRATS   DS    CL3                                                              
         DS    CL7                                                              
LLDEMO   DS    CL7                                                              
         DS    CL4                                                              
LLCLNT   DS    CL3                                                              
         DS    CL6                                                              
LLSEQ    DS    CL1                                                              
* - - - - - - - - - -                                                           
*                                                                               
PGMDETSD DSECT                                                                  
PGMDNAME DS    CL17                                                             
         DS    CL1                                                              
PGMDDAY  DS    CL8                                                              
         DS    CL1                                                              
PGMDTIME DS    CL11                                                             
*                                                                               
LN24D    DSECT                                                                  
LN24PFKT DS    0CL14                                                            
LN24SWAP DS    CL(L'LN24PFKT)                                                   
         DS    CL1                                                              
LN24SCRL DS    0C                                                               
LN24SCRF DS    CL(L'LN24PFKT)                                                   
         DS    CL1                                                              
LN24SCRP DS    CL(L'LN24PFKT)                                                   
         DS    CL1                                                              
LN24SCRN DS    CL(L'LN24PFKT)                                                   
LN24SCLQ EQU   *-LN24SCRL                                                       
         DS    CL1                                                              
LN24RETN DS    CL(L'LN24PFKT)                                                   
         ORG   LN24D+(L'DMVLN24-(*-LN24D)) DON'T ASSEMBLE IF OVERFLOW           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035SPSFM2B   08/11/11'                                      
         END                                                                    
