*          DATA SET TAGEN5E    AT LEVEL 010 AS OF 04/28/15                      
*PHASE T7025EA,*                                                                
         TITLE 'T7025E - EMPLOYEE MAINTENANCE/LIST'                             
T7025E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T7025E,R7,R6                                              
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R6,TWAHOLE          R6=TWAHOLE                                   
         USING WORKD,R6                                                         
         ST    RE,ATATDTAB                                                      
         AHI   RE,L'TATDTAB                                                     
         ST    RE,AUPDTSKS                                                      
         AHI   RE,L'UPDTSKS                                                     
         ST    RE,AUPDPTYS                                                      
                                                                                
         NI    GENSTAT4,ALL-CONFDEL TURN OFF CONFIRM DELETE                     
                                                                                
         CLI   MODE,SETFILE        IGNORE THIS MODE                             
         BE    XIT                                                              
                                                                                
         GOTO1 INITIAL,DMCB,(X'40',0)                                           
                                                                                
         L     RF,=A(LPFTAB-T7025E)                                             
         AR    RF,RB                                                            
         ST    RF,ALPFTAB                                                       
         LR    R2,RF                                                            
*                                  SAVE A(DELETED LIST PF TABLE)                
         L     RF,=A(DPFTAB-T7025E)                                             
         AR    RF,RB                                                            
         ST    RF,ADPFTAB                                                       
*                                                                               
         L     RF,=A(EPFTAB-T7025E)                                             
         AR    RF,RB                                                            
         ST    RF,AEPFTAB                                                       
*                                  SAVE A(FAKE POP PF TABLE)                    
         L     RF,=A(POPTAB-T7025E)                                             
         AR    RF,RB                                                            
         ST    RF,APOPTAB                                                       
*                                  SAVE A(STATUS CODES TABLE)                   
         L     RF,=A(STATTAB-T7025E)                                            
         AR    RF,RB                                                            
         ST    RF,ASTATTAB                                                      
*                                  SAVE A(FAKE POP PF TABLE)                    
         L     RF,=A(BLDLIST-T7025E)                                            
         AR    RF,RB                                                            
         ST    RF,ABLDLIST                                                      
*                                  SAVE A(BUILD SORT KEY ROUTINE)               
         L     RF,=A(BLDSORT-T7025E)                                            
         AR    RF,RB                                                            
         ST    RF,ABLDSORT                                                      
*                                  SAVE A(LIST SCREEN LINE ADDRS TABLE)         
         L     RF,=A(LINATAB-T7025E)                                            
         AR    RF,RB                                                            
         ST    RF,ALINATAB                                                      
*                                  SAVE A(FAKE POP PF TABLE)                    
         MVI   SCRFLAG,C'E'        IF SCREEN IS EXTENSION SCREEN THEN           
         CLI   TWASCR,SCR69            SCREEN FLAG = 'E'                        
         JE    *+8                                                              
         MVI   SCRFLAG,C'L'        ELSE SCREEN FLAG = 'L'                       
                                                                                
         BRAS  RE,TESTFRST                                                      
                                                                                
         CLI   SCRFLAG,C'L'        IF SCREEN IS LIST SCREEN THEN                
         JE    LDRIVER               GO TO LIST DRIVER                          
         J     EDRIVER               ELSE GO TO EXTENSION SCREEN DRIVER         
         EJECT                                                                  
***********************************************************************         
* EXTENSION SCREEN DRIVER                                                       
***********************************************************************         
EDRIVER  DS    0H                                                               
         GOTO1 INITIAL,DMCB,AEPFTAB                                             
*                                                                               
         BRAS  RE,EPOINTER         SAVE OR ADD POINTERS                         
*                                                                               
         CLI   MODE,RECDEL         IF MODE IS RECDEL                            
         JNE   ED5                                                              
         L     R4,AIO              THEN DOUBLE DELETE RECORD                    
         USING TLCAD,R4                                                         
         OI    TLCASTAT,X'40'                                                   
         LA    R3,KEY              AND DOUBLE DELETE KEY                        
         USING TLDRD,R3                                                         
         OI    TLDRSTAT,X'40'                                                   
*                                                                               
ED5      CLI   MODE,RECREST        IF MODE IS RECREST                           
         JNE   ED10                                                             
         L     R4,AIO              THEN DOUBLE UNDELETE RECORD                  
         USING TLCAD,R4                                                         
         NI    TLCASTAT,X'BF'                                                   
         LA    R3,KEY              AND DOUBLE UNDELETE KEY                      
         USING TLDRD,R3                                                         
         NI    TLDRSTAT,X'BF'                                                   
         DROP  R3                                                               
*                                                                               
ED10     CLI   PUSHED,C'Y'         IF CAST LIST CALLED THIS SCREEN              
         JNE   *+8                                                              
         BAS   RE,ECLIST           THEN TAKE CARE OF SPECIAL SITUATIONS         
*                                                                               
         BRAS  RE,ESETADDR         SET ADDRESSES OF SCREEN FIELDS               
*                                                                               
         CLI   MODE,VALKEY         IF MODE IS NOT VALKEY                        
         JE    ED50                                                             
         CLI   MODE,DISPKEY        OR DISPKEY                                   
         JE    ED50                                                             
*                                  AND SSN RECORD WASN'T FOUND                  
         CLC   =C'** NOT FOUND **',EMPPIDN                                      
         JE    EDX                 THEN RETURN TO GENCON                        
*                                                                               
ED50     CLI   MODE,VALKEY         VALIDATE KEY                                 
         JE    ED100                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JE    ED110                                                            
         CLI   MODE,DISPREC        DISPLAY THE RECORD                           
         JE    ED120                                                            
         CLI   MODE,VALREC         VALIDATE THE RECORD                          
         JE    ED130                                                            
         CLI   MODE,XRECPUT        DISPLAY THE RECORD                           
         JE    ED120                                                            
         CLI   MODE,XRECDEL        DISPLAY THE RECORD                           
         JE    ED120                                                            
         CLI   MODE,XRECREST       DISPLAY THE RECORD                           
         JE    ED120                                                            
         J     EDX                                                              
*                                                                               
ED100    BRAS  RE,EVALKEY          VALIDATE KEY                                 
         J     EDX                                                              
*                                                                               
ED110    BRAS  RE,EDISPKEY         DISPLAY KEY                                  
         J     EDX                                                              
*                                                                               
ED120    BRAS  RE,EDISPREC         DISPLAY THE RECORD                           
         J     EDX                                                              
*                                                                               
ED130    BRAS  RE,EVALREC          VALIDATE THE RECORD                          
*                                                                               
EDX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET ADDRESSES OF FIELDS ON EXTENSION SCREEN.                                  
***********************************************************************         
ESETADDR NTR1                                                                   
         LA    R2,EMPCORPH                                                      
         ST    R2,ACORPFLD                                                      
         LA    R2,EMPCORNH                                                      
         ST    R2,ACORNFLD                                                      
         LA    R2,EMPPAYEH                                                      
         ST    R2,ANCDNFLD                                                      
         LA    R2,EMPSTATH                                                      
         ST    R2,ASTATFLD                                                      
         LA    R2,EMPCMH                                                        
         ST    R2,ACMFLD                                                        
ESAX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE EITHER SAVES OR ADDS PASSIVE POINTERS DEPENDING ON THE           
* GENCON MODE.                                                                  
***********************************************************************         
EPOINTER NTR1                                                                   
         CLI   MODE,VALREC         IF MODE VALREC, RECDEL, OR RECREST           
         JE    EP10                                                             
         CLI   MODE,RECDEL                                                      
         JE    EP10                                                             
         CLI   MODE,RECREST                                                     
         JNE   EP20                                                             
*                                  THEN SAVE CURRENT PASSIVE POINTER(S)         
EP10     GOTO1 SAVPTRS,DMCB,PPBLOCK                                             
         J     EPX                                                              
*                                                                               
EP20     CLI   MODE,XRECPUT        ELSE IF MODE IS XRECPUT                      
         JNE   EP40                                                             
*                                  THEN UPDATE PASSIVE POINTER(S)               
         MVI   BYTE,X'88'          PROCESS ACTIVE POINTER                       
         USING TLDRD,R2                                                         
         LA    R2,PPBLOCK                                                       
         USING TLCAD,R4                                                         
         L     R4,AIO                                                           
         CLC   TLCASTAT,TLDRSTAT   IF THE STATUS CHANGED                        
         JE    *+8                                                              
         MVI   BYTE,X'A8'          FORCE WRITE NEW PTRS                         
         GOTO1 ADDPTRS,DMCB,(BYTE,PPBLOCK),UPBLOCK                              
*                                                                               
         L     RF,AIO              SAVE NEW KEY IN TWAKEYSV                     
         MVC   TWAKEYSV,0(RF)                                                   
         MVC   EDISKADD,DMDSKADD   SAVE NEW D/A IN EDISKADD                     
         J     EPX                                                              
*                                                                               
EP40     CLI   MODE,XRECADD        ELSE IF MODE XRECADD, XRECDEL, OR            
         JE    EP50                    XRECREST                                 
         CLI   MODE,XRECDEL                                                     
         JE    EP50                                                             
         CLI   MODE,XRECREST                                                    
         JNE   EP60                                                             
*                                  THEN ADD PASSIVE POINTER(S)                  
EP50     GOTO1 ADDPTRS,DMCB,(8,PPBLOCK),UPBLOCK                                 
*                                                                               
EP60     DS    0H                                                               
*                                                                               
EPX      J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE HANDLES PROBLEMS THAT PERTAIN ONLY TO THE SITUATION              
* WHERE THE CAST LIST CALLS THE EXTENSION SCREEN USING THE 'PUSH' ENTRY         
* FOUND IN THE CAST LIST'S PF TABLE.  SINCE THE SCREEN HAS BEEN CALLED          
* WITH A 'PUSH' AS OPPOSED TO GENCON'S STANDARD LIST LOGIC, CERTAIN             
* TEST'S WILL BE MADE TO DETERMINE IF IT IS TIME TO RETURN TO THE CAST          
* LIST VIA A 'POP'.                                                             
***********************************************************************         
ECLIST   NTR1                                                                   
         CLI   ACTNUM,ACTCHA       IF ACTION IS NOT CHANGE                      
         JE    ECL10                                                            
         CLI   SCRCHANG,C'N'       AND THIS IS SECOND TIME FOR SCREEN           
         JE    ECL20                                                            
*                                                                               
ECL10    CLI   MODE,XRECPUT        OF IF THE MODE IS XRECPUT                    
         JNE   ECLX                                                             
         TM    TRNSTAT,USERCHA     AND USER DIDN'T SET ACTION TO 'CHA'          
         JO    ECLX                                                             
*                                                                               
ECL20    CLI   ACTNUM,ACTCHA       THEN IF ACTION IS CHANGE                     
         JNE   ECL50                                                            
         MVC   DALPTR,EDALPTR      THEN SAVE POINTER TO D/A LIST ENTRY          
         MVC   CASTDA,EDISKADD     AND NEW DISK ADDRESS TO PUT THERE            
*                                                                               
ECL50    MVI   PFAID,21            POP BACK TO LIST SCREEN                      
         GOTO1 INITIAL,DMCB,APOPTAB                                             
         DC    H'0'                *** ROUTINE SHOULD NEVER RETURN ***          
*                                                                               
ECLX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* EXTENSION SCREEN MODE VALKEY ROUTINE.  THE ROUTINE BUILDS THE KEY             
* BY READING THE RECORD WITH THE DISK ADDRESS FOUND IN EDISKADD AND             
* EXTRACTING ITS KEY.                                                           
***********************************************************************         
EVALKEY  NTR1                                                                   
         LA    R3,KEY              R3 = A(KEY)                                  
         USING TLDRD,R3                                                         
*                                                                               
         MVC   TLDRDA,EDISKADD     READ RECORD POINTED TO BY EDISKADD           
         OI    DMINBTS,X'08'       PASS BACK DELETES                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO              R4 = A(RECORD)                               
         USING TLCAD,R4                                                         
*                                                                               
         XC    KEY,KEY             EXTRACT KEY FROM RECORD                      
         MVC   TLDRKEY,TLCAKEY                                                  
*                                                                               
         TM    TLCASTAT,X'80'      IF RECORD IS NOT DELETED                     
         JO    *+8                                                              
         NI    DMINBTS,X'F7'       THEN RESET PASS BACK DELETED FLAG            
*                                                                               
         BAS   RE,ESCRKEY          DISPLAY THE KEY OF THE RECORD                
*                                                                               
EVKX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* EXTENSION SCREEN MODE DISPKEY ROUTINE.  THE ROUTINE SAVES THE DISK            
* ADDRESS OF THE RECORD IN CASE THE USER PRESSES PF2 CAUSING THE ACTION         
* TO BE CHANGE AND IN TURN CAUSING GENCON TO CALL THIS OVERLAY WITH             
* MODE VALKEY (THE ROUTINE EVALKEY ASSUMES THE DISK ADDRESS WAS                 
* RETRIEVED THE FIRST TIME THROUGH).  THEN THE ROUTINE CALLS THE LOWER          
* LEVEL ROUTINE TO DISPLAY THE KEY.                                             
***********************************************************************         
EDISPKEY NTR1                                                                   
         LA    R3,KEY              R3 = A(CAST KEY)                             
         USING TLDRD,R3                                                         
         MVC   EDISKADD,TLDRDA     SAVE D/A IN CASE USER PRESSES PF2            
*                                                                               
         BAS   RE,ESCRKEY          DISPLAY THE RECORD'S KEY TO THE SCR          
*                                                                               
EDKX     J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE INFORMATION FOUND IN THE KEY OF THE RECORD          
* TO THE SCREEN.  IT MUST READ THE COMMERCIAL RECORD TO DISPLAY THE             
* COMMERCIAL INFO.                                                              
***********************************************************************         
ESCRKEY  NTR1                                                                   
         L     R4,AIO              R4 = A(ACTIVE KEY)                           
         USING TLCAD,R4                                                         
         MVC   EMPPID,TLCASSN      DISPLAY SSN                                  
*                                                                               
         GOTO1 HEXOUT,DMCB,TLCASORT,EMPSORT+1,6,0 DISPLAY CAST SORT KEY         
         OI    EMPSORTH+6,X'80'                                                 
         CLI   TGCTSTTY,TASTTYPP   IF CONNECTED STAFF IS A PROGRAMMER           
         JNE   *+8                                                              
         NI    EMPSORTH+1,X'FB'    SET TO HIGH INTENSITY (ELSE ZERO)            
*                                                                               
         MVC   AIO,AIO2            READ COMMERCIAL RECORD AND DISP NAME         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A8',TLCACOM),EMPEVTNH                    
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   COMSTAT,0                                                        
*                                                                               
         L     R4,AIO              R4=A(COMM'L RECORD)                          
         USING TLCOD,R4                                                         
         MVC   EMPAGY,TLCOAGY      DISPLAY AGENCY                               
         MVC   TGAGY,TLCOAGY       SAVE IN GLOBAL TOO                           
*                                                                               
         USING TACOD,R4                                                         
         L     R4,AIO              R4 = A(COMMERCIAL DETAILS ELEMENT)           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   EMPEVT,TACOCID      DISPLAY COMMERCIAL ID                        
         MVC   TGCID,TACOCID       SAVE IN GLOBAL                               
         OI    EMPEVTH+6,X'80'                                                  
*                                                                               
         MVC   AIO,AIO3            DISPLAY W4 NAME                              
         DROP  R4                                                               
*                                                                               
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',EMPPID),EMPPIDNH                      
         GOTO1 SSNPACK,DMCB,EMPPID,TGPID                                        
         MVC   EMPPID,=CL9' '                                                   
         MVC   EMPPID(L'TGPID),TGPID                                            
         MVI   EMPPIDH+5,6                                                      
         OI    EMPPIDH+6,X'80'                                                  
*                                                                               
ESCRK20  MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         CLI   ERROR,0             IF W4 RECORD NOT FOUND                       
         JE    *+10                THEN INDICATE THAT IN NAME FIELD             
         MVC   EMPPIDN(16),=C'** NOT FOUND ** '                                 
*                                                                               
         L     RF,AIO              RESTORE KEY FROM RECORD                      
         MVC   KEY(L'TLDRKEY),0(RF)                                             
*                                                                               
ESKX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* EXTENSION SCREEN MODE DISPREC ROUTINE.                                        
***********************************************************************         
EDISPREC NTR1                                                                   
         XC    NAMESH,NAMESH       CLEAR NAMES FIELD                            
         MVI   NAMESH,8+16         SET LENGTH OF FAKE NAMES FIELD               
*                                                                               
         L     R2,ARECFLD          CLEAR ALL RECORD FIELDS                      
         TWAXC (R2)                                                             
*                                                                               
         XC    EMPCORN,EMPCORN     CLEAR NAME FLDS                              
         OI    EMPCORNH+6,X'80'                                                 
*                                                                               
         TM    OPTS,OPTOTH         WANT TO SEE OTHER DETAILS?                   
         BZ    EDR01               NO, SKIP SHOWING IT                          
         XR    R1,R1                                                            
         LA    R3,EMPNEGRH         SHOW HEADLINE TO STATUS                      
         LA    RF,EMPSTATH                                                      
EDR00    CR    R3,RF                                                            
         BH    EDR01                                                            
         NI    1(R3),X'F3'         NORMAL INTENSITY                             
         OI    6(R3),X'80'         TRANSMIT                                     
         IC    R1,0(R3)                                                         
         AR    R3,R1                                                            
         B     EDR00                                                            
*                                                                               
EDR01    L     R4,AIO              SET A(CAST ELEMENT)                          
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R4,ACAELEM                                                       
*                                                                               
         MVC   AIO,AIO3            READ W4 RECORD INTO IO3 FOR DRCORP           
         CLI   EMPPIDH+5,6                                                      
         BH    EDR02                                                            
         MVC   TGPID,EMPPID                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   EDR02                                                            
         MVC   EMPPID,TGSSN                                                     
         MVI   EMPPIDH+5,9                                                      
EDR02    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',EMPPID)                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   EMPPID,=CL9' '                                                   
         MVC   EMPPID(L'TGPID),TGPID                                            
         MVI   EMPPIDH+5,6                                                      
         OI    EMPPIDH+6,X'80'                                                  
EDR04    MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         BAS   RE,DRCORP                   CORPORATION FIELD                    
         BAS   RE,DRPAYE                   PAYEE FIELD                          
         BAS   RE,DRNEGR                   NEGOTIATED RATES                     
         BAS   RE,DREVTD                   EVENT TIME DETAILS                   
         BAS   RE,DRSTAT                   STATUS FIELD                         
         BAS   RE,DRCM                     COMMENTS FIELD                       
*                                                                               
*                                  DISPLAY LAST CHANGED ELEMENT                 
         GOTO1 ACTVOUT,DMCB,EMPLCHGH                                            
*                                  PROTECT ALL KEY FIELDS                       
         GOTO1 FLDVAL,DMCB,(X'08',EMPAGYH),EMPPIDH                              
*                                                                               
*                                  SET ALL RECORD DATA FIELDS VALID             
         GOTO1 FLDVAL,DMCB,(X'20',ARECFLD),(X'80',999)                          
*                                                                               
         CLI   ACTNUM,ACTDIS       IF ACTION DISPLAY                            
         BNE   EDR10                                                            
         MVI   MYMSGNO1,11         THEN GIVE SELECTION DISPLAYED MSG            
         B     EDR30                                                            
*                                                                               
EDR10    CLI   ACTNUM,ACTDEL       ELSE IF ACTION DELETE                        
         BNE   EDR20                                                            
         MVI   MYMSGNO1,13         THEN GIVE SELECTION DELETED MESSAGE          
         NI    EMPLCHGH+1,X'DF'                                                 
         NI    EMPLCHGH+1,X'F3'                                                 
*                                                                               
EDR20    CLI   ACTNUM,ACTREST      ELSE IF ACTION RESTORE                       
         BNE   EDRX                                                             
         MVI   MYMSGNO1,18         THEN GIVE SELECTION RESTORED MESSAGE         
*                                                                               
EDR30    MVI   MYMSYS,X'FF'        SET GENCON TO USE GETTXT                     
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
*                                                                               
EDRX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY CORPORATION NUMBER FIELD.                                             
***********************************************************************         
DRCORP   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ACORPFLD         R2 = A(CORPORATION FIELD)                    
*                                                                               
         MVC   8(1,R2),TACACORP    DISPLAY CORPORATION NUMBER                   
*                                                                               
         L     R2,ACORNFLD         R2 = A(CORPORATION NAME FIELD DATA)          
         LA    R2,8(R2)                                                         
*                                                                               
         CLI   SCRFLAG,C'L'        IF LIST SCREEN                               
         BNE   DRCORP10                                                         
         L     R3,ANAMEFLD         THEN POINT R2 WITHIN NAMES FIELD             
         USING NAMESD,R3                                                        
         LA    R2,NAMCORP                                                       
         DROP  R3                                                               
*                                                                               
DRCORP10 MVC   AIO,AIO3            W4 RECORD IS IN IO3                          
         MVI   ELCODE,TATIELQ      CORPORATION IS IN TAX ID ELEMENT             
*                                                                               
         CLI   TACACORP,C' '       IF CAST HAS NO CORP                          
         BH    DRCORP20                                                         
*                                                                               
         GOTO1 GETL,DMCB,(1,=AL1(TATITYCO)) THEN IF PERF HAS NO CORP            
         BNE   DRCORP90                     THEN DISPLAY NOTHING                
*                                                                               
*                                  ELSE DISPLAY 'EMPLOYEE HAS CORP'             
         MVC   0(19,R2),=C'*EMPLOYEE HAS CORP*'                                 
         B     DRCORP90                                                         
*                                                                               
DRCORP20 MVI   HALF,TATITYCO       ELSE IF PERF DOESN'T HAVE CAST CORP          
         MVC   HALF+1(1),TACACORP                                               
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   DRCORP30                                                         
*                                                                               
         L     R4,TGELEM           OR CAST CORP W4 RECORD NOT FOUND             
         USING TATID,R4                                                         
         MVC   AIO,AIO2                                                         
         MVC   0(L'TGSSN,R2),TGSSN SAVE SSN TEMPORARILY IN OUTPUT AREA          
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',TATIID),NAMESH                        
         MVC   TGSSN,0(R2)         RESTORE SSN                                  
         BE    DRCORP40                                                         
*                                  THEN DISPLAY 'NOT FOUND'                     
DRCORP30 MVC   0(16,R2),=C'** NOT FOUND ** '                                    
         B     DRCORP90                                                         
*                                                                               
DRCORP40 MVC   0(16,R2),NAMESH+8   ELSE MOVE CAST CORP NAME TO SCREEN           
*                                                                               
DRCORP90 MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
DRCORPX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY PAYEE ON W4                                                           
***********************************************************************         
         USING TAPED,R4                                                         
DRPAYE   NTR1                                                                   
         L     R2,ANCDNFLD         R2 = A(PAYEE FIELD)                          
         L     R4,AIO3                                                          
         MVI   ELCODE,TAPEELQ      LOOK FOR PAYEE ELEMENT ON W4                 
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   8(16,R2),=CL16'*PAYEE ON W4*'                                    
         OI    6(R2),X'80'                                                      
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY NEGOTIATED RATE INDICATOR                                             
***********************************************************************         
DRNEGR   NTR1                                                                   
         MVC   EMPNEGR,=CL19'Negotiated Rates: N'                               
         L     R4,AIO                                                           
         MVI   ELCODE,TATUELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   DRNX                                                             
         MVC   EMPNEGR,=CL19'Negotiated Rates: Y'                               
         OI    EMPNEGRH+6,X'80'                                                 
                                                                                
DRNX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY EVENT TIME DETAILS                                                    
***********************************************************************         
         USING EVTDD,R3                                                         
         USING TATDD,R4                                                         
DREVTD   NTR1                                                                   
         L     R4,AIO                                                           
         LA    R3,EMPFRSTH         FIRST EVENT TIME DETAILS LINE                
         LA    R5,EMPLSTH          LAST EVENT TIME DETAILS LINE                 
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         J     DRE200                                                           
DRE100   BRAS  RE,NEXTEL                                                        
DRE200   JNE   DREX                                                             
         MVC   EVTDTSK,TATDTASK    TASK                                         
         MVC   EVTDPTP,TATDPMTY    PAYMENT TYPE                                 
         EDIT  TATDAMNT,(12,EVTDAMT),2                                          
         OI    EVTDTSKH+6,X'80'                                                 
         OI    EVTDPTPH+6,X'80'                                                 
         OI    EVTDAMTH+6,X'80'                                                 
                                                                                
         MVC   AIO,AIO3            TEMP FOR READING TASK + PAY TYPES            
                                                                                
         GOTO1 RECVAL,DMCB,TLTKCDQ,(X'A8',TATDTASK),                   X        
               ('TLTKSCDQ',EVTDTKNH)                                            
         GOTO1 RECVAL,DMCB,TLPMCDQ,(X'A8',TATDPMTY),                   X        
               ('TLPMSCDQ',EVTDPTNH)                                            
                                                                                
         MVC   AIO,AIO1            RESET AIO                                    
         MVI   ELCODE,TATDELQ                                                   
                                                                                
         AHI   R3,EVTDLNQ                                                       
         CR    R3,R5               NO MORE THAN 5 EVENT TIME DETAILS            
         BH    DREX                                                             
         B     DRE100                                                           
                                                                                
DREX     J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY STATUS ELEMENT.                                                       
***********************************************************************         
DRSTAT   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ASTATFLD         R2 = A(STATUS FIELD DATA)                    
         LA    R2,8(R2)                                                         
         MVC   0(L'EMPSTAT,R2),SPACES                                           
*                                                                               
         L     RF,ASTATTAB         RF = A(STATUS CODE TABLE)                    
*                                                                               
DRS10    CLI   0(RF),X'FF'         WHILE NOT END OF TABLE                       
         BE    DRS40                                                            
*                                                                               
         OC    0(2,RF),0(RF)                                                    
         BZ    DRS11                                                            
         MVC   HALF,0(RF)          IF BIT IS ON IN STATUS BYTE                  
         NC    HALF,TACASTAT                                                    
         BZ    DRS30                                                            
*                                                                               
DRS11    MVC   BYTE,2(RF)                                                       
         NC    BYTE,TACASTA3                                                    
         BZ    DRS30                                                            
*                                                                               
         CLI   0(R2),C' '          IF NOT FIRST CODE                            
         BNH   DRS20                                                            
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
*                                                                               
DRS20    MVC   0(10,R2),3(RF)      DISPLAY CODE                                 
*                                                                               
         LA    R2,9(R2)            POINT R2 TO LAST NON-SPACE                   
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
DRS30    LA    RF,L'STATTAB(RF)    BUMP TO NEXT STATUS CODE TABLE ENTRY         
         B     DRS10               LOOP BACK                                    
*                                                                               
DRS40    MVC   DUB,=CL8'ACTOR=Y'                                                
         TM    TACASTA3,TACASACT                                                
         BO    DRS50                                                            
         MVC   DUB,=CL8'ACTOR=N'                                                
         TM    TACASTA3,TACASNAC                                                
         BZ    XIT                                                              
                                                                                
DRS50    CLI   0(R2),C' '          IF NOT FIRST CODE                            
         BNH   DRS60                                                            
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
DRS60    MVC   0(L'DUB,R2),DUB                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY COMMENTS.                                                             
***********************************************************************         
DRCM     NTR1                                                                   
         USING TACMD,R4                                                         
         L     R2,ACMFLD           R2 = A(COMMENTS FIELD)                       
*                                                                               
         GOTO1 CHAROUT,DMCB,TACMELQ,(R2),TACMTYPG                               
*                                                                               
DRCMX    B     XIT                                                              
         SPACE 3                                                                
***********************************************************************         
* EXTENSION SCREEN MODE VALREC ROUTINE.                                         
***********************************************************************         
EVALREC  NTR1                                                                   
         MVI   RECCHANG,C'N'                                                    
         MVI   HLDCHANG,0                                                       
         MVI   SVCASTA3,0                                                       
         L     R2,ARECFLD                                                       
         GOTOR TESTCHG,DMCB,(0,999)    DETERMINE IF ANYTHING CHANGED            
*                                                                               
         CLI   ACCESSED,C'G'       IF ONLY ACCESSED PARTIALLY                   
         BNE   EVALR10                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         USING TACAD,R4                                                         
         BAS   RE,GETEL            IF NO CAST DETAILS ELEMENT                   
         BE    *+12                                                             
         BAS   RE,INITCAEL         INIT CAELEM & RETURN R4=A(CAELEM)            
         B     EVALR5                                                           
         XC    CAELEM,CAELEM       ELSE SET TACAD TO CAELEM                     
         ZIC   RF,TACALEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CAELEM(0),0(R4)                                                  
         MVI   CAELEM+1,TACALNQ                                                 
         GOTO1 REMELEM                                                          
         LA    R4,CAELEM                                                        
EVALR5   ST    R4,ACAELEM          SAVE A(NEW CAST DETAILS ELEMENT)             
         B     EVALR50                                                          
*                                                                               
EVALR10  L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVCASTA3,TACASTA3                                                
         GOTO1 REMELEM                                                          
*                                                                               
         BAS   RE,INITCAEL         INIT CAELEM & RETURN R4=A(CAELEM)            
         ST    R4,ACAELEM          SAVE A(CAST ELEMENT)                         
*                                                                               
         MVC   AIO,AIO3            READ W4 RECORD INTO IO3 FOR VRCORP           
         CLI   EMPPIDH+5,6                                                      
         BH    EVALR20                                                          
         MVC   TGPID,EMPPID                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   EVALR20                                                          
         MVC   EMPPID,TGSSN                                                     
         MVI   EMPPIDH+5,9                                                      
EVALR20  GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',EMPPID)                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   EMPPID,=CL9' '                                                   
         MVC   EMPPID(L'TGPID),TGPID                                            
         MVI   EMPPIDH+5,6                                                      
         OI    EMPPIDH+6,X'80'                                                  
EVALR30  MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         BRAS  RE,VRCORP                    CORPORATION                         
         BAS   RE,VRSTAT                    STATUS FIELD                        
         BRAS  RE,VRCM                      COMMENTS FIELD                      
*                                                                               
*                                  ADD NEW CAST ELEMENT                         
EVALR50  MVC   TACAUN,=C'NON'                                                   
         MVC   TACALOCL,=C'NON'                                                 
         OI    TACASTA3,TACASPPL   ALWAYS SET PAYROLL PLUS                      
         MVC   ELEM(TACALNQ),CAELEM                                             
         GOTO1 ADDELEM                                                          
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TANMELQ      IF WE HAVE A NAME ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    EVALR90             SKIP ADDING ONE                              
         BRAS  RE,BLDNMEL          BUILD AND ADD NAME ELEMENT                   
*                                                                               
*                                  UPDATE LAST CHANGED ELEMENT                  
EVALR90  GOTO1 ACTVIN,DMCB,EMPLCHGH                                             
*                                                                               
         GOTO1 ABLDSORT,DMCB,(RC)  UPDATE SORT KEY                              
*                                                                               
         LA    R3,KEY              REREAD CAST RECORD FOR UPDATE BEFORE         
         USING TLDRD,R3                RETURNING TO GENCON                      
         MVC   TLDRDA,GLOBDA                                                    
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         DROP  R3                                                               
*                                                                               
         L     RE,AIO3             TRAP BUG WHERE CAST KEY GETS                 
         L     RF,AIO1             CHANGED!!!!                                  
         CLC   0(L'TLCAKEY,RE),0(RF)                                            
         BE    XIT                                                              
         DC    H'00'                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATUS CODES AND SET CORRESPONDING BITS IN EITHER AGENCY OR          
* BILLING RULES STATUS BYTE.  ALSO VALIDATE OVERRIDE EMPLOYER OF RECORD         
* IF ONE IS GIVEN.                                                              
***********************************************************************         
VRSTAT   NTR1                                                                   
         L     R4,AIO                                                           
         USING TLCAD,R4                                                         
         NI    TLCASTAT,X'DF'                                                   
*                                                                               
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ASTATFLD         R2 = A(STATUS FIELD)                         
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
*                                                                               
         CLI   5(R2),0                                                          
         BE    VRSX                                                             
*                                  ELSE SCAN FIELD INTO SCANNER BLOCK           
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         CLI   4(R1),0                                                          
         BE    ERRINV                                                           
*                                                                               
         LA    R3,BLOCK            R3 = A(SCANNER BLOCK)                        
         USING SCAND,R3                                                         
         ZIC   RF,4(R1)            FULL = NUMBER OF SCAN BLOCK ENTRIES          
         ST    RF,FULL                                                          
         SR    R0,R0               R0 = DISPLACEMENT INTO FIELD                 
*                                                                               
VRS10    L     RF,ASTATTAB         LOOK IN STATUS TABLE FOR MATCH               
*                                                                               
VRS20    CLI   0(RF),X'FF'         ERROR IF END OF TABLE FOUND                  
         BE    VRS50                                                            
*                                                                               
         ZIC   RE,SCLEN1           IF MATCH THEN SET BIT                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),3(RF)                                                 
         BE    VRS30                                                            
*                                                                               
         LA    RF,L'STATTAB(RF)    ELSE TRY NEXT TABLE ENTRY                    
         B     VRS20                                                            
*                                                                               
VRS30    CLI   SCLEN2,0            ERROR IF RHS EXISTS                          
         BNE   ERRFLD                                                           
*                                                                               
         OC    0(2,RF),0(RF)                                                    
         BZ    VRS40                                                            
         OC    TACASTAT(2),0(RF)   SET APPROPRIATE BIT IN APPR BYTE             
         B     VRS90                                                            
*                                                                               
VRS40    OC    TACASTA3,2(RF)                                                   
         B     VRS90                                                            
*                                                                               
VRS50    CLI   SCLEN1,5                                                         
         BNE   ERRFLD                                                           
         CLC   =C'ACTOR',SCDATA1                                                
         BNE   ERRFLD                                                           
         TM    TACASTA3,TACASACT+TACASNAC                                       
         BNZ   ERRFLD                                                           
         CLI   SCLEN2,1                                                         
         BNE   ERRFLD                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   VRS60                                                            
         OI    TACASTA3,TACASACT                                                
         B     VRS90                                                            
VRS60    CLI   SCDATA2,C'N'                                                     
         BNE   ERRFLD                                                           
         OI    TACASTA3,TACASNAC                                                
*                                                                               
VRS90    ZIC   RF,SCLEN1           BUMP R0 TO NEXT STATUS CODE                  
         LA    RF,1(RF)                                                         
         AR    R0,RF                                                            
*                                                                               
         LA    R3,SCANNEXT         BUMP R3 TO NEXT SCANNER ENTRY                
*                                                                               
         L     RF,FULL             REPEAT UNTIL NO MORE SCANNER FIELDS          
         BCTR  RF,0                                                             
         ST    RF,FULL                                                          
         LTR   RF,RF                                                            
         BNZ   VRS10                                                            
         DROP  R3                                                               
                                                                                
VRSX     TM    SVCASTA3,TACASACT+TACASNAC                                       
         BZ    XIT                                                              
         TM    TACASTA3,TACASACT+TACASNAC                                       
         BZ    ERMIS                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LIST SCREEN DRIVER                                                            
***********************************************************************         
LDRIVER  DS    0H                                                               
         MVI   VALMODE,0           CLEAR VALIDATION MODE                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JE    LD10                                                             
         CLI   MODE,VALREC         LISTRECS                                     
         JE    LD50                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         JE    LD70                                                             
         J     LDX                                                              
*                                                                               
LD10     BRAS  RE,LVKEY            VALIDATE KEY                                 
         J     LDX                                                              
*                                                                               
LD50     BRAS  RE,LVREC            ELSE VALIDATE RECORD                         
         J     LDX                                                              
*                                                                               
LD70     BRAS  RE,LPRINTR          PRINT REPORT                                 
         J     LDX                                                              
LDX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUMPS R2 TO THE NEXT SCREEN FIELD.                               
***********************************************************************         
BUMP     ZIC   R0,0(R2)            BUMP R2 TO NEXT FIELD                        
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 4                                                                
                                                                                
INITCAEL DS    0H                  INIT. CAELEM & RETURN R4=A(CAELEM)           
         LA    R4,CAELEM                                                        
         XC    CAELEM,CAELEM                                                    
         USING TACAD,R4                                                         
         MVI   TACAEL,TACAELQ                                                   
         MVI   TACALEN,TACALNQ                                                  
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES AND EXITS                                     *         
***********************************************************************         
                                                                                
ERRINV   MVI   ERROR,INVALID       INVALID INPUT                                
         J     END                                                              
                                                                                
ERMIS    MVI   ERROR,MISSING       MISSING INPUT                                
         J     END                                                              
                                                                                
ERRRNF   MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         J     END                                                              
                                                                                
ERIST    MVI   ERROR,ERRECCTY      INVALID SCREEN FOR COMML TYPE                
         J     END                                                              
                                                                                
ERW4LCK  MVI   ERROR,ERW4SLCK      W4 RECORD LOCKED                             
         J     END                                                              
                                                                                
ERRNOIP  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         J     END                                                              
                                                                                
ERRCRP   MVI   ERROR,ERGT1CRP      GT 1 CORP - NEED EXACT CODE                  
         J     END                                                              
                                                                                
ERRFLD   MVI   ERROR,INVALID       INVALID INPUT FIELD IN MIDDLE                
         STC   R0,ERRDISP                                                       
         J     END                                                              
                                                                                
ERRLOCK  MVI   ERROR,ERAGYLCK      AGENCY IS LOCKED                             
         LA    R2,EMLAGYH                                                       
         J     END                                                              
                                                                                
ERRPFK   MVI   ERROR,ERINVPFK      INVALID PFKEY PRESSED                        
         LA    R2,EMLL1H                                                        
         J     END                                                              
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
INFEND   MVI   MYMTYP,GTMINF       INFORMATION MESSAGE EXIT                     
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
END      GOTO1 EXIT,DMCB,0                                                      
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
XFFS     DC    16X'FF'                                                          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLE OF STATUS CODES AND THERE DISPLAYABLE FORM.                             
***********************************************************************         
*                                                                               
STATTAB  DS    0CL13                                                            
******** DC    AL1(0,0,TACASLCK),CL10'LOCKED'                                   
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* PFTABLE FOR EMPLOYEE LIST IF DELETE OPTION NOT REQUESTED.                     
***********************************************************************         
LPFTAB   DS    0H                                                               
         DC    AL1(LPF3X-*,3,0,(LPF3X-LPF3)/KEYLNQ,0)                           
         DC    CL3'W4 ',CL8'W4      ',CL8'DISPLAY'                              
LPF3     DC    AL1(KEYTYCUR,L'EMLPID-1),AL2(EMLPID-EMLPID)                      
LPF3X    EQU   *                                                                
*                                                                               
         DC    AL1(LPF13DX-*,13,0,(LPF13DX-LPF13D)/KEYLNQ,0)                    
         DC    CL3'CH ',CL8'CHECK   ',CL8'LIST'                                 
LPF13D   DC    AL1(KEYTYCUR,L'EMLPID-1),AL2(EMLPID-EMLPID)                      
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
LPF13DX  EQU   *                                                                
*                                                                               
         DC    AL1(LPF14X-*,14,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'        ',CL8'       '                              
LPF14X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF22X-*,22,PFTINT+PFTCPROG,(LPF22X-LPF22)/KEYLNQ,0)         
         DC    CL3'S  ',CL8'        ',CL8'DISPLAY'                              
LPF22    DC    AL1(KEYTYCOM,0),AL2(0)                                           
LPF22X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF23X-*,23,PFTINT+PFTCPROG,(LPF23X-LPF23)/KEYLNQ,0)         
         DC    CL3'C  ',CL8'        ',CL8'CHANGE'                               
LPF23    DC    AL1(KEYTYCOM,0),AL2(0)                                           
LPF23X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF24X-*,24,PFTINT+PFTCPROG,(LPF24X-LPF24)/KEYLNQ,0)         
         DC    CL3'DE ',CL8'        ',CL8'DELETE'                               
LPF24    DC    AL1(KEYTYCOM,0),AL2(0)                                           
LPF24X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* PFTABLE FOR EMPLOYEE LIST IF DELETE OPTION REQUESTED.                         
***********************************************************************         
DPFTAB   DS    0H                                                               
         DC    AL1(DPF3X-*,3,0,(DPF3X-DPF3)/KEYLNQ,0)                           
         DC    CL3'W4 ',CL8'W4      ',CL8'DISPLAY'                              
DPF3     DC    AL1(KEYTYCUR,L'EMLPID-1),AL2(EMLPID-EMLPID)                      
DPF3X    EQU   *                                                                
*                                                                               
         DC    AL1(DPF13X-*,13,0,(DPF13X-DPF13)/KEYLNQ,0)                       
         DC    CL3'CH ',CL8'CHECK   ',CL8'LIST'                                 
DPF13    DC    AL1(KEYTYCUR,L'EMLPID-1),AL2(EMLPID-EMLPID)                      
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
DPF13X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF14X-*,14,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'        ',CL8'       '                              
DPF14X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF22X-*,22,PFTINT+PFTCPROG,(DPF22X-DPF22)/KEYLNQ,0)         
         DC    CL3'S  ',CL8'        ',CL8'DISPLAY'                              
DPF22    DC    AL1(KEYTYCOM,0),AL2(0)                                           
DPF22X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF23X-*,23,PFTINT+PFTCPROG,(DPF23X-DPF23)/KEYLNQ,0)         
         DC    CL3'R  ',CL8'        ',CL8'RESTORE'                              
DPF23    DC    AL1(KEYTYCOM,0),AL2(0)                                           
DPF23X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
EPFTAB   DS    0H                                                               
         DC    AL1(EPF13X-*,13,0,(EPF13X-EPF13)/KEYLNQ,0)                       
         DC    CL3'CH ',CL8'CHECK   ',CL8'LIST'                                 
EPF13    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
EPF13X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
LINATAB  DS    0H                  DISPLACEMENT TO LIST LINES 1-8               
         DC    AL2(EMLL1H-T702FFD)                                              
         DC    AL2(EMLL2H-T702FFD)                                              
         DC    AL2(EMLL3H-T702FFD)                                              
         DC    AL2(EMLL4H-T702FFD)                                              
         DC    AL2(EMLL5H-T702FFD)                                              
         DC    AL2(EMLL6H-T702FFD)                                              
         DC    AL2(EMLL7H-T702FFD)                                              
         DC    AL2(EMLL8H-T702FFD)                                              
         DC    AL2(0)                                                           
         EJECT                                                                  
POPTAB   DS    0H                                                               
         DC    AL1(PT21X-*,21,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PT21X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        LITERALS FOR VALIDATE REC ROUTINES                           *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
LPRINTR  NTR1                                                                   
                                                                                
*                                  SET REPORT SPECS                             
         L     RF,=A(MYSPECS-T7025E)                                            
         AR    RF,RB                                                            
         ST    RF,SPECS                                                         
                                                                                
         L     RF,=A(HOOK-T7025E)      SET A(HEADLINE HOOK)                     
         AR    RF,RB                                                            
         ST    RF,HEADHOOK                                                      
                                                                                
         GOTO1 ABLDLIST,DMCB,1,(RC)                                             
                                                                                
         LA    R3,DALIST           R3 = A(FIRST DISK ADDRESS)                   
         LA    R2,EMLL1H           R2 = A(FIRST SCREEN LINE)                    
         BRAS  RE,LSETADDR         SET ADDRESSES OF SCREEN FIELDS               
                                                                                
         L     RE,ASELFLD          SAVE FIRST SCREEN LINE TO RESTORE            
         L     RF,ANEXTLIN             WHEN DONE REPORT                         
         SR    RF,RE                                                            
         LA    R0,SVFIRSTL                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R0,NUMITEMS         R0 = NUMBER OF ITEMS IN LIST                 
                                                                                
LPR10    BRAS  RE,READREC          READ IN RECORD                               
         BNE   LPR50               IF RECORD DISAPPEARED THEN SKIP              
*                                                                               
         BRAS  RE,DISPLINE         DISPLAY LINE TO SCREEN                       
                                                                                
         LA    RF,P1               FILL FIRST PRINT LINE FROM SCREEN            
         USING PLINED,RF                                                        
         MVC   PLSSN,EMLPID                                                     
         MVC   PLCORP,EMLCORP                                                   
         DROP  RF                                                               
*                                  FILL SECOND PRINT LINE FROM NAME FLD         
         MVC   P2+4(L'EMLNAME),EMLNAME                                          
                                                                                
         GOTO1 CATCHIOS            PRINT BOTH PRINT LINES                       
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
                                                                                
LPR50    LA    R3,4(R3)            BUMP R3 TO NEXT DISK ADDRESS                 
                                                                                
         BCT   R0,LPR10            REPEAT UNTIL NO MORE RECORDS                 
                                                                                
         L     RE,ASELFLD          RESTORE FIRST SCREEN LINE                    
         L     RF,ANEXTLIN                                                      
         SR    RF,RE                                                            
         LA    R0,SVFIRSTL                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                  PRINT TOTAL NUMBER OF CAST MEMBERS           
         MVC   P+4(24),=C'NUMBER OF CAST MEMBERS : '                            
         EDIT  (4,NUMITEMS),(7,P+27),ALIGN=LEFT,ZERO=NOBLANK                    
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
         XC    CONSERV,CONSERV     AUTO CALL $DQU                               
         MVC   CONSERV(4),=C'$DQU'                                              
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
LPRX     J     XIT                                                              
         EJECT                                                                  
                                                                                
         XIT1                                                                   
HOOK     NTR1                                                                   
         GOTO1 PRTSCRN,DMCB,CONTAGH,EMLCRPH,H4-1                                
*                                                                               
         J     XIT                                                              
         SPACE 2                                                                
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H8,1,C'--- --------- ---'                                        
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        ROUTINE TO INITIALIZE PROGRAM                                *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINE                      *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        ROUTINE TO BUILD NAME ELEMENT                                *         
***********************************************************************         
                                                                                
BLDNMEL  NTR1  BASE=*,LABEL=*                                                   
         USING TAW4D,R4                                                         
         L     R4,AIO3             W4 RECORD THAT WAS READ BEFORE               
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
         USING TANMD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TANMEL,TANMELQ                                                   
         MVI   TANMLEN,TANMLNQ                                                  
         MVC   TANMCRPN,TAW4CRPN                                                
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R2,R4                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINE                      *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY (LISTS)                          *         
***********************************************************************         
LVKEY    NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   KEYCHANG,C'N'                                                    
         TM    EMLAGYH+4,X'20'     IF AGENCY FIELD HAS CHANGED                  
         JZ    LVK10                                                            
         TM    EMLEVTH+4,X'20'     IF EVENT ID FIELD HAS CHANGED                
         JZ    LVK10                                                            
         TM    EMLOPTH+4,X'20'     IF OPTION FIELD HAS CHANGED                  
         JZ    LVK10                                                            
         TM    EMLPIDSH+4,X'20'    IF PID SEARCH FIELD HAS CHANGED              
         JZ    LVK10                                                            
                                                                                
         CLC   ACTNUM,TWALACT      OR ACTION HAS CHANGED                        
         JE    LVK20                                                            
         TM    TRNSTAT,RETURNED    AND WE HAVEN'T JUST POPPED                   
         JO    LVK20                                                            
                                                                                
LVK10    MVI   KEYCHANG,C'Y'       KEY HAS CHANGED                              
                                                                                
LVK20    LA    R2,EMLAGYH                                                       
         GOTO1 RECVAL,DMCB,(BYTE,TLAYCDQ),(X'20',EMLAGYH)                       
         TM    TGAYSTA7,TAAYSPPL   PAYROLL PLUS AGENCY                          
         JZ    ERIATMA2            NO ERROR OUT                                 
         OI    EMLAGYH+4,X'20'                                                  
                                                                                
         LA    R2,EMLEVTH          KEY HAS CHANGED                              
         GOTO1 RECVAL,DMCB,(BYTE,TLCOICDQ),(X'28',EMLEVTH),EMLEVTNH             
         TM    TGMEEQU,EVENT       ONLY EVENTS ALLOWED                          
         JZ    ERRINV              OTHERWISE, INVALID                           
         OI    EMLEVTH+4,X'20'                                                  
                                                                                
         USING TLCOPD,R3                                                        
         LA    R3,KEY                                                           
         MVC   TGCOM,TLCOICOM      SAVE INTERNAL COMM'L NUMBER                  
         DROP  R3                                                               
                                                                                
         GOTO1 CHAROUT,DMCB,TACMELQ,EMLCOMMH,TACMTYPG  BOTTOM COMMENTS          
                                                                                
LVK60    BRAS  RE,VALOPTS          VALIDATE OPTIONS FIELD                       
                                                                                
         XC    SVSSNFLT,SVSSNFLT                                                
         CLI   EMLPIDSH+5,0                                                     
         JE    LVK69                                                            
         LA    R2,EMLPIDSH                                                      
         CLI   5(R2),6             PID ENTERED                                  
         JL    ERRINV              LESS THAN 6 CHARS, INVALID                   
         JE    LVK63                                                            
         MVC   TGSSN,EMLPIDS                                                    
         OC    TGSSN,SPACES                                                     
         J     LVK65                                                            
LVK63    MVC   TGPID,EMLPIDS                                                    
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         JNE   ERRINV                                                           
                                                                                
LVK65    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TGSSN)                                
         JNE   EREMYNF2                                                         
         BRAS  RE,INEVENT          SEE IF IN EVENT                              
         JNE   EREMYNF2                                                         
                                                                                
LVK68    MVC   SVSSNFLT,TGSSN                                                   
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   EMLPIDS,SPACES                                                   
         MVC   EMLPIDS(6),TGPID                                                 
         OI    EMLPIDSH+6,X'80'                                                 
LVK69    OI    EMLPIDSH+4,X'20'    MARK VALIDATED                               
                                                                                
LVK80    CLI   CONWHENH+5,0        IF PRINT FIELD HAS SOMETHING IN IT           
         BE    *+8                                                              
         NI    WHENOK,X'FE'        CLEAR BIT THAT PREVENTS PRINTREP             
         OI    EMLNEGHH+1,X'0C'                                                 
         TM    OPTS,OPTOTH                                                      
         JZ    *+12                                                             
         OI    EMLNEGHH+1,X'08'                                                 
         NI    EMLNEGHH+1,X'FB'                                                 
         OI    EMLNEGHH+6,X'80'                                                 
         J     XIT                                                              
                                                                                
ERIATMA2 MVC   MYMSGNO,=Y(ERRIATMA) INVALID ACTION FOR TRAD MEDIA               
         J     ERREND                                                           
                                                                                
EREMYNF2 MVC   MYMSGNO,=Y(ERREMYNF) EMPLOYEE NOT FOUND                          
         J     ERREND                                                           
         EJECT                                                                  
***********************************************************************         
*        LITERALS FOR VALIDATE KEY ROUTINES                           *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE RECORD (LISTS)                       *         
***********************************************************************         
LVREC    NTR1  BASE=*,LABEL=*                                                   
         XC    NAMESH,NAMESH       CLEAR NAMES FIELD                            
         MVI   NAMESH,8+16         SET LENGTH OF FAKE NAMES FIELD               
         MVI   DISAPPR,C'N'        SET RECORD HAS NOT DISAPPEARED               
                                                                                
         CLI   ACTNUM,ACTLDEL      IF USER WANTS DELETED RECORDS                
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       THEN TELL DATAMGR TO PASS BACK DELS          
                                                                                
         CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED                           
         JNE   LVR10                                                            
*                                  BUILD DISK ADDRESS LIST                      
         GOTO1 ABLDLIST,DMCB,1                                                  
                                                                                
         BRAS  RE,DISPLIST         DISPLAY LIST                                 
         J     LVR100                                                           
                                                                                
LVR10    MVI   RECCHANG,C'N'       ELSE SET RECORD HAS NOT CHANGED              
         MVI   HLDCHANG,0                                                       
                                                                                
*                                  LOCK LIST                                    
         GOTO1 ABLDLIST,DMCB,0                                                  
         BAS   RE,VALILST          VALIDATE LIST                                
*                                                                               
         L     R2,ALPFTAB          R2 = A(PF TABLE)                             
         CLI   ACTNUM,ACTLDEL                                                   
         BNE   *+8                                                              
         L     R2,ADPFTAB                                                       
         GOTO1 INITIAL,DMCB,(R2)   TEST FOR SELECT CODES                        
*                                                                               
         CLI   RECCHANG,C'N'       IF NO RECORD HAS CHANGED                     
         BNE   LVR90                                                            
         TM    TRNSTAT,RETURNED    THEN IF WE HAVEN'T JUST POPPED               
         BO    *+8                                                              
         BRAS  RE,TESTPFKS         TEST DIRECTIONAL PFKEYS                      
         BRAS  RE,DISPLIST         DISPLAY LIST                                 
         B     LVR100                                                           
                                                                                
LVR90    CLI   DISAPPR,C'Y'        ELSE IF SOME RECORD HAS DISAPPEARED          
         BNE   LVR100                                                           
         BRAS  RE,DISPLIST         THEN RE-DISPLAY LIST                         
                                                                                
LVR100   LA    R2,EMLL1H           POSITION CURSOR TO FIRST SEL FIELD           
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
                                                                                
         CLI   ACTNUM,ACTLIST      IF ACTION IS LIST                            
         BNE   LVR120                                                           
                                                                                
         OC    NUMITEMS,NUMITEMS   THEN IF NUMBER OF ITEMS = 0                  
         BNZ   LVR110                                                           
                                                                                
         ZIC   R0,0(R2)            THEN BUMP R2 TO FIRST SSN FIELD              
         AR    R2,R0                                                            
         MVI   MYMSGNO1,2          AND GIVE PLEASE ENTER FIELDS MESSAGE         
         J     LVRX                                                             
                                                                                
LVR110   CLI   PFAID,14            ELSE IF PFKEY FOR ADD PERF PRESSED           
         JNE   LVR120                                                           
         BRAS  RE,LSETADDR         THEN BUMP R2 TO SECOND SSN FIELD             
         L     R2,ANEXTLIN                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVI   MYMSGNO1,2          AND GIVE PLEASE ENTER FIELDS MESSAGE         
         J     LVRX                                                             
                                                                                
LVR120   L     RF,ITEM             ELSE IF DISPLAYING LAST PAGE                 
         A     RF,=A(NUMSCR)                                                    
         C     RF,NUMITEMS                                                      
         JL    LVR130                                                           
         MVI   MYMSGNO1,10         THEN GIVE END OF LIST MESSAGE                
         J     LVRX                                                             
                                                                                
LVR130   MVI   MYMSGNO1,9          ELSE MIDDLE OF LIST MESSAGE                  
                                                                                
LVRX     J     END                 TAKE ERROR EXIT                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE LIST OF RECORDS ON THE SCREEN.  FOR EACH LINE THAT HAD           
* A RECORD THERE BEFORE THERE WILL BE A DISK ADDRESS TABLE ENTRY FOR            
* THAT RECORD.                                                                  
***********************************************************************         
VALILST  NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'02',EMLL1H),999                                   
*                                                                               
         L     R3,ITEM             R3 = A(TOP OF PAGE DISK ADDRESS)             
         SLL   R3,2                                                             
         LA    R3,DALIST(R3)                                                    
         LA    R2,EMLL1H           R2 = A(FIRST LINE ON SCREEN)                 
*                                                                               
VL10     BRAS  RE,LSETADDR         SET ADDRESSES OF FIELDS ON THIS LINE         
         NI    HLDCHANG,X'FF'-ONTLINE                                           
*                                                                               
         BRAS  RE,VALISEL          TEST VALID DATA IN SELECT FIELD              
*                                                                               
         BAS   RE,TESTLINE         IF DISPLAY LINE EMPTY OR UNCHANGED           
         JE    VL20                                                             
*                                                                               
         L     R2,ARECFLD          THEN IF DISPLAY LINE IS EMPTY AND            
         CLI   8(R2),C' '              THERE WAS A RECORD THERE BEFORE          
         JH    VL90                    AND IT HASN'T DISAPPEARED                
         OC    0(4,R3),0(R3)                                                    
         JZ    VL90                                                             
         BRAS  RE,READREC                                                       
         JE    VL80                THEN REDISPLAY RECORD                        
*                                                                               
         J     VL90                ELSE SKIP DISPLAY LINE                       
*                                                                               
VL20     OC    0(4,R3),0(R3)       ELSE IF RECORD WAS THERE BEFORE              
         JZ    VL50                                                             
*                                                                               
         MVI   VALMODE,C'C'        SET VALIDATION MODE TO CHANGE                
*                                                                               
         BRAS  RE,READREC          READ OLD CAST RECORD                         
         JNE   VL90                SKIP THIS LINE IF REC DISAPPEARED            
*                                                                               
         MVC   AIO,AIO3            READ W4 RECORD INTO IO3                      
         L     R2,ASSNFLD          R2 = A(SSN FIELD)                            
         BRAS  RE,UNPKPID                                                       
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',8(R2))                                
         MVC   AIO,AIO1                                                         
         JNE   VL80                SKIP THIS LINE IF W4 REC DISAPPEARED         
         BRAS  RE,PKSSN            CONVERT SSN TO PID                           
*                                                                               
         BAS   RE,VALILINE         VALIDATE AND BUILD RECORD                    
         JNE   VL80                SKIP IF CHANGES NOT ALLOWED                  
*                                                                               
         MVI   RDUPDATE,C'Y'       READ AND LOCK OLD CAST RECORD INTO           
         MVC   AIO,AIO2                VOID                                     
         BRAS  RE,READREC                                                       
         MVC   AIO,AIO1                                                         
         JNE   VL90                SKIP THIS LINE IF REC DISAPPEARED            
*                                                                               
         BRAS  RE,WRITENEW         WRITE BACK NEW CAST RECORD                   
*                                                                               
         J     VL80                                                             
*                                                                               
VL50     MVI   VALMODE,C'A'        ELSE SET VALIDATION MODE TO ADD              
*                                                                               
         BAS   RE,VRSSN            VALIDATE SSN                                 
*                                                                               
         BAS   RE,INITADD          INITIALIZE IOAREA FOR NEW RECORD             
*                                                                               
         BAS   RE,VALILINE         VALIDATE AND BUILD RECORD                    
*                                                                               
         BAS   RE,ADDNEW           ADD NEW CAST RECORD TO FILE                  
*                                                                               
VL80     L     R2,ASSNFLD                                                       
         BRAS  RE,PKSSN            CONVERT SSN TO PID                           
         BRAS  RE,DISPLINE         DISPLAY BACK RECORD                          
*                                                                               
VL90     LA    R3,4(R3)            BUMP TO NEXT D/A IN LIST                     
*                                                                               
*                                  MAKE ALL FIELDS VALID                        
         GOTO1 FLDVAL,DMCB,(X'20',ASSNFLD),(X'80',ALASTUNP)                     
*                                                                               
         L     R2,ANEXTLIN         R2 = A(NEXT DISPLAY LINE)                    
*                                                                               
         LA    R1,EMLCOMMH                                                      
         CR    R2,R1               REPEAT UNTIL END OF SCREEN                   
         JL    VL10                                                             
*                                                                               
VLX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE SSN FIELD.                                                           
***********************************************************************         
VRSSN    NTR1                                                                   
         MVC   AIO,AIO3            VALIDATE SSN (READ W4 REC INTO IO3)          
         L     R2,ASSNFLD                                                       
         BRAS  RE,UNPKPID          CONVERT TO SSN FOR RECVAL CALL               
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'24',ASSNFLD)                              
         JE    VRSSN10                                                          
         MVC   8(L'TGSSN,R2),BLOCK   RESTORE WHAT WAS ENTERED                   
         OI    6(R2),X'80'                                                      
         J     ERRRNF                                                           
                                                                                
VRSSN10  BRAS  RE,PKSSN            CONVERT BACK TO PID                          
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         MVI   ELCODE,TAW4ELQ      GET W4 ELEMENT                               
         L     R4,AIO3                                                          
         USING TAW4D,R4                                                         
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF THIS W4 IS LOCKED                         
         JO    ERW4LCK             EXIT WITH ERROR MESSAGE                      
         CLI   TAW4TYPE,TAW4TYTR   IF THIS W4 RECORD IS TRUSTEE                 
         JE    ERNOTRS             EXIT WITH ERROR MESSAGE                      
                                                                                
VRSSNX   J     XIT                                                              
                                                                                
ERNOTRS  MVC   MYMSGNO,=Y(ERNOTRST) TRUSTEE NOT ALLOWED ON CAST                 
         J     ERREND                                                           
                                                                                
         EJECT                                                                  
***********************************************************************         
* TEST IF DISPLAY LINE HAS CHANGED.  IF IT HAS THEN SET RECCHANG TO 'Y'         
* AND RETURN 'YES'.  OTHERWISE RETURN 'NO'.                                     
***********************************************************************         
TESTLINE NTR1                                                                   
         CLI   ACTNUM,ACTLDEL      IF USER WANTS DELETED RECORDS                
         JE    TLNO                THEN DISPLAY LINE CAN'T CHANGE               
*                                                                               
         L     R2,ASSNFLD          R2 = A(FIRST UNPROTECTED FIELD)              
         TM    1(R2),X'20'                                                      
         JO    TL10                                                             
         GOTOR TESTCHG,DMCB,(0,ALASTUNP) DETERMINE IF ANYTHING CHANGED          
         J     TL20                                                             
                                                                                
TL10     L     R2,ARECFLD                                                       
         GOTOR TESTCHG,DMCB,(1,ALASTUNP)  DETERMINE IF ANYTHING CHANGED         
TL20     JNE   TLNO                                                             
*                                                                               
TLYES    J     YES                 RETURN 'YES'                                 
*                                                                               
TLNO     J     NO                  RETURN 'NO'                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISPLAY LINE AND BUILD RECORD.                                       
***********************************************************************         
VALILINE NTR1                                                                   
         CLI   ACCESSED,C'Y'       IF NOT FULLY ACCESSED                        
         JE    VLIN05                                                           
         CLI   VALMODE,C'A'        AND ADDING                                   
         JE    ERRNOIP             THEN INPUT NOT ALLOWED                       
*                                                                               
         CLI   ACCESSED,C'N'       IF NOT ACCESSED AT ALL                       
         JE    NO                  THEN GET OUT NOW                             
*                                                                               
VLIN05   CLI   VALMODE,C'C'        IF VALIDATION MODE IS CHANGE                 
         JNE   VLIN10                                                           
*                                  SAVE POINTER BLOCK                           
         GOTO1 SAVPTRS,DMCB,PPBLOCK                                             
         J     VLIN20                                                           
*                                                                               
VLIN10   LA    R0,PPBLOCK          ELSE CLEAR POINTER BLOCK                     
         LHI   R1,L'PPBLOCK                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
VLIN20   L     R4,AIO              IF OLD CAST ELEMENT NOT FOUND                
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    VLIN30                                                           
         USING TACAD,R4                                                         
*                                                                               
         LA    R4,CAELEM           THEN BUILD NEW CAST ELEM IN CAELEM           
         XC    CAELEM,CAELEM                                                    
         MVI   TACAEL,TACAELQ                                                   
         MVI   TACALEN,TACALNQ                                                  
         J     VLIN50                                                           
*                                                                               
VLIN30   XC    CAELEM,CAELEM       ELSE COPY OLD CAST ELEMENT FOR THE           
         ZIC   RF,TACALEN              OLD LENGTH TO CAELEM                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   CAELEM(0),0(R4)                                                  
*                                                                               
         MVI   CAELEM+1,TACALNQ    SET NEW LENGTH IN CAELEM                     
*                                                                               
         GOTO1 REMELEM             REMOVE OLD CAST ELEMENT                      
*                                                                               
VLIN50   LA    R4,CAELEM           SAVE A(CAST ELEMENT)                         
         MVC   TACAUN,=C'NON'                                                   
         MVC   TACALOCL,=C'NON'                                                 
         ST    R4,ACAELEM                                                       
*                                                                               
VLIN60   BRAS  RE,VRCORP                                                        
*                                                                               
VLIN70   MVC   TACAUN,=C'NON'                                                   
         MVC   TACALOCL,=C'NON'                                                 
         OI    TACASTA3,TACASPPL   ALWAYS SET PAYROLL PLUS                      
         MVC   ELEM(TACALNQ),CAELEM                                             
         GOTO1 ADDELEM                                                          
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TANMELQ      IF WE HAVE A NAME ELEMENT                    
         BRAS  RE,GETEL                                                         
         BE    VLIN80              SKIP ADDING ONE                              
         BRAS  RE,BLDNMEL          BUILD AND ADD NAME ELEMENT                   
*                                                                               
VLIN80   GOTO1 ACTVIN,DMCB,0       UPDATE LAST CHANGED ELEMENT                  
*                                                                               
         GOTO1 ABLDSORT,DMCB,(RC)  UPDATE SORT KEY                              
*                                                                               
VLINX    J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE LIST OF RECORDS ON THE SCREEN.                                    
***********************************************************************         
DISPLIST NTR1                                                                   
         XC    VERTAB,VERTAB       CLEAR EXTENTION LIST TABLE                   
         L     R3,ITEM             R3 = A(TOP OF PAGE DISK ADDRESS)             
         SLL   R3,2                                                             
         LA    R3,DALIST(R3)                                                    
         LA    R2,EMLL1H           R2 = A(FIRST LINE ON SCREEN)                 
*                                                                               
DL10     BRAS  RE,LSETADDR         SET ADDRESSES OF FIELDS ON THIS LINE         
*                                                                               
         L     R2,ASELFLD          CLEAR SELECT FIELD                           
         XC    8(L'EMLL1,R2),8(R2)                                              
         OI    6(R2),X'80'                                                      
*                                                                               
DL20     OC    0(4,R3),0(R3)       IF THERE IS A RECORD TO DISPLAY              
         JZ    DL50                                                             
*                                                                               
         BRAS  RE,READREC          THEN READ IN RECORD                          
         JE    DL30                                                             
*                                                                               
         LR    RE,R3               RECORD DISAPPEARED - REMOVE DISKADD          
         LA    RF,DALIST+DALISTL-4     FROM LIST                                
         SR    RF,R3                                                            
         LA    R0,4(RE)                                                         
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RF,NUMITEMS         DECREMENT NUMBER OF ITEMS                    
         BCTR  RF,0                                                             
         ST    RF,NUMITEMS                                                      
         J     DL20                LOOP BACK                                    
*                                                                               
DL30     DS    0C                                                               
         BRAS  RE,DISPLINE         DISPLAY RECORD                               
*        BRAS  RE,CKEXT            CHECK IF EXTENSION SCREEN HAS                
*                                  INFO NOT ON LIST                             
         AHI   R3,4                BUMP TO NEXT D/A IN LIST                     
         J     DL90                                                             
*                                                                               
DL50     BRAS  RE,CLRLINE          ELSE CLEAR LINE OUT FOR ADDING               
*                                                                               
DL90     L     R2,ANEXTLIN         R2 = A(NEXT DISPLAY LINE)                    
*                                                                               
         LA    R1,EMLCOMMH                                                      
         CR    R2,R1               REPEAT UNTIL END OF SCREEN                   
         JL    DL10                                                             
*                                                                               
DLX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE IOAREA WITH KEY AND ZERO ELEMENTS.                                 
***********************************************************************         
INITADD  NTR1                                                                   
         XC    TGCSORT,TGCSORT     BUILD ACTIVE KEY                             
         XC    TGCAT,TGCAT                                                      
         GOTO1 RECVAL,DMCB,TLCACDQ,(X'C0',TGCAT)                                
*                                                                               
         L     R4,AIO              INITIALIZE RECORD WITH KEY                   
         USING TLCAD,R4                                                         
         XC    TLCAKEY(TLCAELEM+1-TLCAKEY),TLCAKEY                              
         MVC   TLCAKEY,KEY                                                      
         OI    TLCASORT,TLCASRGQ   THEN SET NEXT HIGHEST BIT                    
*                                                                               
         MVC   TLCALEN,DATADISP    SET RECORD HAS NO ELEMS                      
*                                                                               
IAX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD NEW CAST RECORD.  FIRST ADD THE NEXT AVAILABLE SEQUENCE NUMBER TO         
* THE KEY.  THEN ADD THE RECORD AND ADD ITS DISK ADDRESS TO THE DISK            
* ADDRESS LIST.  THEN ADD THE PASSIVE POINTERS.  FINALLY, INCREMENT THE         
* NEXT AVAILABLE SEQUENCE NUMBER.                                               
***********************************************************************         
ADDNEW   NTR1                                                                   
         USING TLFTD,R4                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY              IF CAST MEMBER ALREADY HAS FTRACK            
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'B4',0)     GET COMML, RDUPDATE           
*                                                                               
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSEQ))                                     
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R4,TGELEM                                                        
         MVC   NEXTSEQ,TANUNXTC    SAVE NEXT CAST SEQUENCE NUMBER               
*                                                                               
         ZICM  RE,TANUNXTC,2                                                    
         AHI   RE,1                                                             
         STCM  RE,3,TANUNXTC                                                    
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R4,AIO              INSERT SEQUENCE NUMBER INTO SORT             
         USING TLCAKEY,R4                                                       
         MVC   TLCASEQ,NEXTSEQ                                                  
         GOTO1 ADDREC              ELSE ADD NEW CAST RECORD                     
         OI    HLDCHANG,ONSCREEN                                                
         DROP  R4                                                               
*                                                                               
         MVC   0(4,R3),DMDSKADD    MOVE DISK ADDRESS TO LIST                    
*                                                                               
         L     RF,NUMITEMS         INCREMENT NUMBER OF ITEMS                    
         LA    RF,1(RF)                                                         
         ST    RF,NUMITEMS                                                      
*                                  ADD NEW POINTERS                             
         GOTO1 ADDPTRS,DMCB,(8,PPBLOCK),UPBLOCK                                 
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEE IF TGSSN IS IN EVENT                                                      
***********************************************************************         
         USING TLCAPD,R3                                                        
INEVENT  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVI   TLCAPCD,TLCACCDQ                                                 
         MVC   TLCACSSN,TGSSN                                                   
         MVC   TLCACCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLCACCAT-TLCAPD),KEYSAVE                                     
         JNE   NO                                                               
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE CORPORATION NUMBER FIELD (OPTIONAL).                                 
***********************************************************************         
VRCORP   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ACORPFLD         R2 = A(CORPORATION FIELD)                    
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
*                                                                               
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
*                                                                               
         MVI   TACACORP,0          IF FIELD IS EMPTY THEN SET TO ZERO           
         CLI   5(R2),0                                                          
         JE    VRCORPX                                                          
*                                                                               
         MVC   TACACORP,8(R2)      SAVE IN ELEMENT                              
*                                                                               
         CLI   TACACORP,C'Y'       IF FIELD HAD 'Y'                             
         JNE   *+8                                                              
         MVI   TACACORP,C'1'       THEN SET TO '1' IN ELEMENT                   
*                                                                               
         MVI   HALF,TATITYCO       BUILD GETL SEARCH ARGUMENT                   
         MVC   HALF+1(1),TACACORP                                               
*                                                                               
         MVC   AIO,AIO3            SEARCH IO3 FOR TAX UNIT ELEMENT              
         MVI   ELCODE,TATIELQ                                                   
         GOTO1 GETL,DMCB,(2,HALF)                                               
         JNE   ERRINV              ERROR IF NOT FOUND                           
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         CLI   8(R2),C'Y'          IF 'Y' WAS INPUT                             
         JNE   VRCORPX                                                          
         L     R4,TGELEM                                                        
         BRAS  RE,NEXTEL           SEARCH FOR ADDL CORPS ON W4 REC.             
         JE    ERRCRP              NEED PRECISE CORP CODE                       
*                                                                               
VRCORPX  MVC   CASTCORP,TACACORP   SAVE CAST CORP                               
                                                                                
         CLI   5(R2),0                                                          
         JE    XIT                                                              
         L     RF,TGELEM                                                        
         GOTOR W4LCKCRP,DMCB,TATIID-TATID(RF),AIO3,AIO1                         
         JE    XIT                                                              
         NI    4(R2),X'DF'         UNVALIDATE                                   
         MVC   MYMSGNO,=Y(ERCPSLCK) CORP RECORD LOCKED - NON-PAY                
         J     ERREND                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD TO DISPLAY LINE.                                               
***********************************************************************         
* have to spool                                                                 
DISPLINE NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    ACAELEM,ACAELEM                                                  
         XC    ATDELEM,ATDELEM                                                  
*                                                                               
         L     R4,AIO              SET A(CAST ELEMENT)                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   *+8                                                              
         ST    R4,ACAELEM                                                       
*                                  MAKE SSN FIELD PROT/NORM INTENSITY           
         L     R4,AIO              SET A(TASK DETAILS ELEMENT)                  
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   *+8                                                              
         ST    R4,ATDELEM                                                       
*                                  MAKE SSN FIELD PROT/NORM INTENSITY           
         GOTO1 FLDVAL,DMCB,(X'08',ASSNFLD),(X'10',ASSNFLD)                      
*                                                                               
         L     R2,ASSNFLD          CLEAR THIS LINE'S PROTECTED FLDS             
         L     R3,ANAMEFLD                                                      
         TWAXC (R2),(R3),PROT=Y                                                 
*                                                                               
         L     R2,ARECFLD          CLEAR THIS LINE'S UNPROTECTED FLDS           
         L     R3,ALASTUNP                                                      
*        TWAXC (R2),(R3)                                                        
*                                                                               
         BAS   RE,DRSSN            DISPLAY SSN FIELD                            
         JNE   DLIN5               ABORT IF W4 RECORD NOT FOUND                 
*                                                                               
DLIN5    CLI   ACTNUM,ACTLDEL      IF USER WANTS DELETED RECORDS                
         JNE   DLIN10                                                           
*                                  THEN MAKE ALL DATA FIELDS PROTECTED          
         GOTO1 FLDVAL,DMCB,(8,ARECFLD),ALASTUNP                                 
         J     DLIN30                                                           
                                                                                
DLIN10   GOTO1 FLDVAL,DMCB,(4,ARECFLD),ALASTUNP                                 
*                                                                               
*                                  SET ALL DATA FIELDS VALID                    
DLIN30   GOTO1 FLDVAL,DMCB,(X'20',ARECFLD),ALASTUNP                             
*                                                                               
DLINX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY SS NUMBER FIELD.  SAVE RECORD IN IO3 FOR LATER USE.                   
***********************************************************************         
DRSSN    NTR1                                                                   
         USING TLCAD,R4                                                         
         L     R4,AIO              R4 = A(RECORD)                               
         L     R2,ASSNFLD          R2 = A(SSN FIELD)                            
         L     R3,ANAMEFLD         R3 = A(NAME FIELD)                           
*                                                                               
         MVC   8(9,R2),TLCASSN     DISPLAY SSN                                  
*                                                                               
*                                  DISPLAY W4 NAME                              
         MVC   AIO,AIO3            USE IO3 FOR RECVAL CALL                      
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',8(R2)),NAMESH                         
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         JE    DRSSN10             IF NO ERROR THEN SKIP                        
*                                                                               
         BRAS  RE,PKSSN            CONVERT SSN TO PID                           
         MVC   8(16,R3),=C'** NOT FOUND ** ' ELSE REC MUST BE MISSING           
*                                                                               
         OI    CVERSTAT,CVERSNW4   SET NO W4 RECORD IN CVERSTAT                 
         J     DRSSNNO             AND RETURN 'NO'                              
*                                                                               
DRSSN10  BRAS  RE,PKSSN            CONVERT SSN TO PID                           
         MVI   ELCODE,TAW4ELQ      GET W4 ELEMENT                               
         L     R4,AIO3                                                          
         USING TAW4D,R4                                                         
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF THIS W4 IS LOCKED                         
         JNO   DRSSN20                                                          
         MVC   8(16,R3),=C'** W4 LOCKED ** ' PUT IT OUT                         
         J     DRSSNYES            AND RETURN 'YES'                             
*                                                                               
DRSSN20  MVC   8(16,R3),NAMESH+8     ELSE MOVE NAME TO SCREEN                   
*                                                                               
         L     R3,ANGRTFLD         NEGOTIATED RATE FIELD                        
         OI    1(R3),X'0C'         DEFAULT IS HIDDEN                            
         MVI   8(R3),C'N'          DEFAULT NO                                   
                                                                                
         TM    OPTS,OPTOTH                                                      
         BZ    DRSSN30                                                          
         NI    1(R3),X'F3'         MAKE NORMAL INTENSITY                        
                                                                                
         OC    ATDELEM,ATDELEM     IF WE HAVE TATD ELEMENTS                     
         JZ    DRSSN30                                                          
         MVI   8(R3),C'Y'                                                       
         OI    6(R3),X'80'                                                      
*                                                                               
         USING TACAD,R4                                                         
DRSSN30  L     R3,ANAMEFLD         R3 = A(NAME FIELD)                           
         OC    ACAELEM,ACAELEM     ANY CORP CODE?                               
         JZ    DRSSN60                                                          
         L     R4,ACAELEM                                                       
                                                                                
         MVC   AIO,AIO3            USE IO3 FOR GETL                             
         MVI   ELCODE,TATIELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TATITYCO)) THEN IF PERF HAS NO CORP            
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         BNE   DRSSN60                      THEN DISPLAY NOTHING                
         MVC   30(19,R3),=C'*EMPLOYEE HAS CORP*' THERE ARE CORPS                
                                                                                
         CLI   TACACORP,C' '                                                    
         JNH   DRSSN60             NO, SKIP                                     
         L     R3,ACORPFLD         CORP CODE FIELD                              
         MVC   8(1,R3),TACACORP    MOVE IN CORP CODE                            
         MVC   BYTE,TACACORP       SAVE IT FOR LATER                            
         L     R3,ANAMEFLD         R3 = A(NAME FIELD)                           
*                                                                               
         USING TATID,R4                                                         
         MVI   ELCODE,TATIELQ      GET TAX INFO ELEMENT                         
         L     R4,AIO3                                                          
         BRAS  RE,GETEL                                                         
         J     DRSSN45                                                          
DRSSN40  BRAS  RE,NEXTEL                                                        
DRSSN45  JNE   DRSSN49                                                          
         CLI   TATITYPE,TATITYCO   HAS TO BE A CORP                             
         JNE   DRSSN40                                                          
         CLC   TATICRPN,BYTE       MATCH ON CORP CODE                           
         JNE   DRSSN40                                                          
                                                                                
         MVC   AIO,AIO2            USE IO3 FOR RECVAL CALL                      
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',TATIID),NAMESH                        
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         JE    DRSSN50             IF NO ERROR THEN SKIP                        
*                                                                               
DRSSN49  MVC   30(16,R3),=C'** NOT FOUND ** ' ELSE REC MUST BE MISSING          
         J     DRSSNNO             AND RETURN 'NO'                              
*                                                                               
DRSSN50  MVI   ELCODE,TAW4ELQ      GET W4 ELEMENT                               
         L     R4,AIO2                                                          
         USING TAW4D,R4                                                         
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF THIS W4 IS LOCKED                         
         JNO   DRSSN55                                                          
         MVC   30(17,R3),=C'** CORP LOCKED **' PUT IT OUT                       
         J     DRSSNYES            AND RETURN 'YES'                             
*                                                                               
DRSSN55  MVC   30(32,R3),TAW4CRPN  ELSE MOVE NAME TO SCREEN                     
                                                                                
DRSSN60  L     R4,AIO3                                                          
         MVI   ELCODE,TAPEELQ      LOOK FOR PAYEE ELEMENT ON W4                 
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   50(16,R3),=CL16'*PAYEE ON W4*'                                   
                                                                                
DRSSN90  DS    0H                                                               
*                                                                               
DRSSNYES J     YES                 RETURN 'YES'                                 
*                                                                               
DRSSNNO  J     NO                  RETURN 'NO'                                  
*        DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CLEAR SCREEN LINE AND SET SSN FIELD TO UNPROTECTED TO ALLOW  *         
*        A RECORD TO BE ADDED ON THIS LINE                            *         
***********************************************************************         
                                                                                
CLRLINE  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTLDEL      IF USER WANTS DELETED RECORDS                
         JNE   CL10                THEN PROTECT ALL RECORD DATA FIELDS          
         GOTO1 FLDVAL,DMCB,(X'08',ASSNFLD),(X'10',ALASTUNP)                     
         J     CL20                                                             
*                                  ELSE UNPROTECT ALL DATA FIELDS AND           
*                                      SET TO NORMAL INTENSITY                  
CL10     GOTO1 FLDVAL,DMCB,(X'04',ASSNFLD),(X'10',ALASTUNP)                     
                                                                                
*                                  SET ALL DATA FIELDS VALID                    
         GOTO1 FLDVAL,DMCB,(X'20',ASSNFLD),ALASTUNP                             
                                                                                
CL20     L     R2,ASSNFLD          CLEAR UNPROTECTED FIELDS                     
         L     R3,ANAMEFLD                                                      
         TWAXC (R2),(R3),PROT=Y                                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE TESTS IF THE DATA IN THE SELECT FIELD FOR THIS LINE IS           
* VALID BY LOOKING FOR A MATCHING PFTAB ENTRY.  IF NO MATCH IS FOUND AN         
* ERROR IS GIVEN.                                                               
***********************************************************************         
VALISEL  NTR1  BASE=*,LABEL=*                                                   
         LR    R0,R3               R0=D/A OF THIS RECORD                        
*                                                                               
         L     R2,ASELFLD          R2 = A(SELECT FIELD)                         
         OC    8(L'EMLL1,R2),=CL7' '  PAD WITH SPACES                           
*                                                                               
         L     R3,ALPFTAB          R3 = A(FIRST PFTAB ENTRY)                    
         CLI   ACTNUM,ACTLDEL                                                   
         JNE   *+8                                                              
         L     R3,ADPFTAB                                                       
         USING PFTABD,R3                                                        
*                                                                               
VS10     CLI   0(R3),X'FF'         IF END OF PFTAB THEN ERROR                   
         JE    ERRINV                                                           
         CLC   8(L'EMLL1,R2),PFTSEL  IF MATCH FOUND THEN VALID                  
         JE    VS20                                                             
         ZIC   RF,PFTLEN           BUMP TO NEXT PFTAB ENTRY                     
         AR    R3,RF                                                            
         J     VS10                LOOP BACK                                    
*                                                                               
VS20     CLC   PFTSEL(2),=C'DE'    IF SELECTING FOR DELETE                      
         JE    *+14                                                             
         CLC   PFTSEL(2),=C'R '    OR RESTORE                                   
         JNE   VS30                                                             
         CLI   ACCESSED,C'Y'       IF NOT FULLY ACCESSED                        
         JNE   ERRINV              THEN GIVE ERROR                              
*                                                                               
VS30     CLC   PFTSEL(2),=C'DE'    IF SELECTING FOR DELETE                      
         JNE   XIT                                                              
*                                                                               
         CLI   TWASCR,SCR6A        IF ON LIST SCREEN                            
         JNE   VS40                                                             
         L     RE,ANAMEFLD         CAN ONLY DELETE A CAST RECORD                
         CLI   8(RE),0             THAT IS ACTUALLY ON FILE                     
         JE    ERRINV                                                           
*                                                                               
         USING TLCAD,R4                                                         
VS40     LR    R3,R0               R3=D/A OF CAST RECORD                        
         BRAS  RE,READREC          READ CAST RECORD INTO AIO                    
                                                                                
         L     R4,AIO              R4 = A(CAST RECORD)                          
                                                                                
         USING TLTMD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY             READ ALL TIMESHEET KEYS                      
         MVI   TLTMCD,TLTMCDQ      ATTACHED TO THIS COMMERCIAL                  
         OI    TLTMSTA,TLTMSPPL                                                 
         MVC   TLTMCOM,TLCACOM                                                  
         GOTO1 HIGH                                                             
         J     VS60                                                             
VS50     GOTO1 SEQ                                                              
VS60     CLC   TLTMKEY(TLTMINV-TLTMKEY),KEYSAVE                                 
         JNE   XIT                                                              
         CLC   TLCASEQ,TLTMSORT+4  ERROR IF ONE FOUND FOR THIS CAST             
         JE    ERRNOTIM            SEQUENCE NUMBER                              
         J     VS50                                                             
         DROP  R3                                                               
                                                                                
ERRNOTIM MVC   MYMSGNO,=Y(EREMPTIM) CAN'T DELETE IF TIMESHEETS ATTACHED         
         J     ERREND                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS THE CAST RECORD WITH THE DISK ADDRESS POINTED TO           
* BY R3.  IF THE DELETE STATUS BIT DOES NOT MATCH THE ACTION TYPE (LIST         
* OR LIST DELETED) THEN THE ROUTINE SETS THE DISAPPR FLAG TO 'Y' AND            
* RETURNS 'NO'. OTHERWISE IT RETURNS 'YES'.                                     
***********************************************************************         
READREC  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,KEY              READ CAST RECORD                             
         USING TLDRD,R4                                                         
         MVC   TLDRDA,0(R3)                                                     
         OI    DMINBTS,X'08'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   ACTNUM,ACTLDEL      IF ACTION IS NOT LDELETE                     
         JE    *+8                                                              
         NI    DMINBTS,X'F7'       THEN RESET PASS BACK DELETED FLAG            
*                                                                               
         L     R4,AIO              R4 = A(RECORD KEY)                           
         USING TLCAD,R4                                                         
*                                                                               
         CLI   ACTNUM,ACTLDEL      IF ACTION IS LIST DELETED                    
         JNE   RR10                                                             
         TM    TLCASTAT,X'40'      AND RECORD IS NOT DELETED                    
         JZ    RR20                                                             
         J     YES                                                              
*                                                                               
RR10     TM    TLCASTAT,X'80'      OR ACTION IS LIST OR VERIFY AND              
         JZ    YES                     RECORD IS DELETED                        
*                                                                               
RR20     MVI   DISAPPR,C'Y'        THEN SET RECORD HAS DISAPPEARED              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UNPACK THE PID NUMBER TO A SS NUMBER                                          
* R2 POINTS TO THE SSN FIELD HEADER                                             
***********************************************************************         
         SPACE 1                                                                
UNPKPID  NTR1  BASE=*,LABEL=*                                                   
         MVC   BLOCK(L'TGSSN),8(R2)       SAVE PID/SSN                          
         OC    BLOCK(L'TGSSN),SSNSPACE                                          
         CLI   5(R2),9                                                          
         JE    UNPIDX                                                           
         CLI   5(R2),6                                                          
         JH    ERRINV                                                           
         MVC   TGPID,8(R2)                                                      
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         JNE   UNPIDX                                                           
         MVC   8(L'TGSSN,R2),TGSSN                                              
         MVI   5(R2),9                                                          
                                                                                
UNPIDX   J     XIT                                                              
                                                                                
SSNSPACE DC    CL9' '                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PACK THE SS NUMBER INTO A PID NUMBER                                          
* R2 POINTS TO THE SSN FIELD HEADER                                             
***********************************************************************         
         SPACE 1                                                                
PKSSN    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   8(L'TGSSN,R2),SSNSPAC2                                           
         MVC   8(L'TGPID,R2),TGPID                                              
         MVI   5(R2),6                                                          
         OI    6(R2),X'80'                                                      
                                                                                
PKSSNX   J     XIT                                                              
                                                                                
SSNSPAC2 DC    CL9' '                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              ROUTINE DETERMINES IF ANY FIELDS CHANGED                         
*                                  R2=A(FIRST FIELD)                            
***********************************************************************         
TESTCHG  NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            R3=A(LAST FIELD)                             
*                                                                               
         CLI   0(R1),0                                                          
         JNE   TC10                                                             
         GOTO1 FLDVAL,DMCB,(X'80',(R2)),(X'80',(R3))                            
         JE    TCNO                                                             
*                                  AND SOME FIELD HAS CHANGED                   
TC10     GOTO1 FLDVAL,DMCB,(X'40',(R2)),(X'80',(R3))                            
         JE    TCNO                                                             
         L     R2,8(R1)            R2=A(FIRST CHANGED FIELD)                    
         TM    AYSTAT,TAAYSLCK     IF AGENCY IS LOCKED                          
         JO    ERRLOCK             SET ERROR                                    
*                                                                               
TCYES    J     YES                                                              
TCNO     J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET ADDRESSES OF FIELDS ON CURRENT DISPLAY LINE.                              
***********************************************************************         
LSETADDR NTR1  BASE=*,LABEL=*                                                   
         ST    R2,ASELFLD          SAVE A(SEL FIELD)                            
         BRAS  RE,BUMP                                                          
         ST    R2,ASSNFLD          SAVE A(SSN FIELD)                            
         BRAS  RE,BUMP                                                          
         ST    R2,ACORPFLD         SAVE A(CORPORATION FIELD)                    
         ST    R2,ARECFLD          SAVE A(FIRST RECORD DATA FIELD)              
         ST    R2,ALASTUNP         SAVE A(LAST UNPROTECTED FLD OF LINE)         
         BRAS  RE,BUMP                                                          
         ST    R2,ANGRTFLD         SAVE A(NEGOTIATED RATE FIELD)                
         BRAS  RE,BUMP                                                          
         ST    R2,ANAMEFLD         SAVE A(NAMES FIELD)                          
         BRAS  RE,BUMP                                                          
         ST    R2,ANEXTLIN         SAVE A(NEXT SCREEN LINE)                     
*                                                                               
LSAX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* WRITE BACK NEW CAST RECORD.  UPDATE PASSIVE POINTERS.                         
***********************************************************************         
WRITENEW NTR1  BASE=*,LABEL=*                                                   
         USING TLCAD,R4                                                         
         L     R4,AIO              R4=A(CAST RECORD)                            
         CLC   TLCAKEY,PPBLOCK     TRAP BUG WHERE CAST KEY GETS                 
******** JNE   WNEW10              CHANGED!!!!                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 PUTREC              WRITE BACK CAST RECORD                       
*                                                                               
         USING TLDRD,R2                                                         
         LA    R2,PPBLOCK                                                       
         MVI   BYTE,X'88'          PROCESS ACTIVE POINTER                       
         CLC   TLCASTAT,TLDRSTAT   IF THE STATUS CHANGED                        
         JE    *+8                                                              
         MVI   BYTE,X'A8'          FORCE WRITE NEW PTRS                         
         GOTO1 ADDPTRS,DMCB,(BYTE,PPBLOCK),UPBLOCK                              
         MVC   0(4,R3),DMDSKADD    UPDATE DISK ADDRESS OF RECORD                
         J     XIT                                                              
         DROP  R2,R4                                                            
*                                                                               
WNEW10   XC    KEY,KEY             IF KEY HAS CHANGED, REREAD ORIGINAL          
         MVC   KEY(L'TLCAKEY),PPBLOCK                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCAKEY),KEYSAVE                                           
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING TLRCD,R2                                                         
         L     R2,AIO2                                                          
         MVC   WORK(L'TLTMKEY),0(R2)                                            
         OI    TLRCSTAT,X'80'      DELETE ORIGINAL AND ITS POINTERS             
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'E8',PPBLOCK),UPBLOCK                             
         DROP  R2                                                               
*                                                                               
         USING TLTMD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY             READ ALL TIMESHEET KEYS FOR                  
         MVI   TLTMCD,TLTMCDQ      THE COMMERCIAL                               
         MVC   TLTMCOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         J     WNEW30                                                           
WNEW20   GOTO1 SEQ                                                              
WNEW30   CLC   KEY(TLTMINV-TLTMD),KEYSAVE                                       
         JNE   WNEW40                                                           
*                                                                               
         USING TLCAD,R1                                                         
         LA    R1,WORK                                                          
         CLC   TLTMSORT,TLCASORT   IF WE FIND A TIMESHEET THAT MATCHES          
         JNE   WNEW20              THE ORIGIANL SORT KEY                        
         CLC   TLTMCAT,TLCACAT     AND ORIGINAL CATEGORY                        
         JNE   WNEW20                                                           
         MVC   TGINV,TLTMINV       SAVE THE INVOICE NUMBER                      
         DROP  R1,R4                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'       READ THE TIMESHEET RECORD FOR                
         GOTO1 GETREC              UPDATE                                       
*                                                                               
         USING TLDRD,R4                                                         
         OI    TLDRSTAT,X'80'      MARK THE KEY DELETED                         
         GOTO1 WRITE                                                            
         DROP  R4                                                               
*                                                                               
         USING TLTMD,R2                                                         
         OI    TLTMSTAT,X'80'                                                   
         GOTO1 PUTREC                                                           
         DROP  R2                                                               
*                                                                               
         USING TLTMD,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLTMCD,TLTMCDQ      IF THERE ARE NO TIMESHEETS                   
         MVC   TLTMCOM,TGCOM       LEFT FOR THE INVOICE                         
         MVC   TLTMINV,TGINV                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLTMSSN-TLTMD),KEYSAVE                                       
         JE    WNEW40                                                           
         DROP  R4                                                               
*                                                                               
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B4',0)                                    
         JNE   WNEW40                                                           
*                                                                               
         USING TAIND,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAINELQ      DELETE TIMESHEET COMMERCIAL                  
         BRAS  RE,GETEL            FROM THE INVOICE                             
         JE    *+6                                                              
         DC    H'00'                                                            
         XC    TAINTMCO,TAINTMCO                                                
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
*                                                                               
WNEW40   MVC   AIO,AIO1            RESTORE AIO TO UPDATED CAST                  
         LA    R0,PPBLOCK          AND CLEAR POINTER POINTERS                   
         LHI   R1,L'PPBLOCK                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING TLDRD,R4                                                         
         L     R2,AIO1                                                          
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   TLDRKEY,0(R2)       MOVE RECORD KEY TO DIRECTORY KEY             
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                LOOK FOR RECORD ALREADY ON FILE              
*                                                                               
         CLC   TLDRKEY,KEYSAVE     IF RECORD EXISTS                             
         JNE   WNEW50                                                           
         TM    TLDRSTAT,X'80'      IT HAD BETTER BE DELETED                     
         JO    *+6                                                              
         DC    H'0'                                                             
         NI    TLDRSTAT,X'3F'      TURN OFF CAST DELETED BITS                   
         GOTO1 WRITE               AND WRITE IT BACK                            
*                                                                               
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         MVC   AIO,AIO2            SET ALT. I/O AREA                            
         GOTO1 GETREC              GET THE RECORD SO THAT WE CAN ...            
         ST    R2,AIO                                                           
         GOTO1 PUTREC              WRITE NEW ONE BACK OVER DELETED ONE          
         J     WNEW60                                                           
*                                                                               
WNEW50   MVC   TLDRKEY,KEYSAVE                                                  
         GOTO1 ADDREC              OK TO ADD THE RECORD                         
*                                                                               
WNEW60   GOTO1 ADDPTRS,DMCB,(X'28',PPBLOCK),UPBLOCK                             
         MVC   0(4,R3),DMDSKADD    UPDATE DISK ADDRESS OF RECORD                
         NI    DMINBTS,X'F7'                                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO BUILD EMPLOYEE LIST                               *         
* THIS ROUTINE READS THROUGH THE ACTIVE CAST KEYS FOR THIS COMM'L,    *         
* LOCKING THE FIRST, AND DETERMINES THE NEXT AVAIL SEQUENCE NUMBER.   *         
* IF PARMAMETER 1 <> ZERO, THEN THE ROUTINE WILL BUILD THE LIST OF    *         
* CAST RECORD DISK ADDRESSES.                                         *         
***********************************************************************         
BLDLIST  NTR1  BASE=*,LABEL=*                                                   
         MVC   FULL,0(R1)          SAVE PARAMETER 1                             
         OI    DMINBTS,X'08'       SET TO PASS BACK DELETED KEYS                
                                                                                
         OC    FULL,FULL           IF CALLER WANTS DISK ADDRESS LIST            
         JZ    BL10                                                             
         LA    RE,DALIST           THEN CLEAR DISK ADDRESS LIST                 
         LA    RF,DALISTL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
BL10     LA    R4,KEY              BUILD KEY WITH CODE/COMM ID NUMBER           
         XC    KEY,KEY                                                          
                                                                                
         USING TLCAPD,R4                                                        
         MVI   TLCAPCD,TLCANCDQ                                                 
         MVC   TLCANCOM,TGCOM                                                   
         J     BL19                                                             
         DROP  R4                                                               
                                                                                
         USING TLCAD,R4                                                         
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
*                                                                               
BL19     LA    R3,DALIST           R3 = A(DISK ADDRESS LIST)                    
         SR    R0,R0               R0 = NUMBER OF ITEMS                         
*                                                                               
         MVI   RDUPDATE,C'Y'       READ AND LOCK FIRST KEY                      
         GOTO1 HIGH                                                             
*                                                                               
*                                  WHILE SAME COMM'L                            
BL20     LHI   RE,TLCANNAM-TLCAPD-1                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         JNE   BL100                                                            
                                                                                
         OC    SVSSNFLT,SVSSNFLT                                                
         JZ    BL25                                                             
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         USING TLCAD,RE                                                         
         L     RE,AIO2                                                          
         CLC   TLCASSN,SVSSNFLT                                                 
         JNE   BL90                                                             
         DROP  RE                                                               
                                                                                
         USING TLDRD,R4                                                         
BL25     CLI   ACTNUM,ACTLDEL      IF USES WANTS DELETED RECORDS                
         JNE   BL30                                                             
         TM    TLDRSTAT,X'40'      THEN SKIP RECORDS THAT AREN'T                
         JZ    BL90                                                             
         J     BL40                                                             
*                                                                               
BL30     TM    TLDRSTAT,X'80'      ELSE SKIP RECORDS THAT ARE                   
         JO    BL90                                                             
*                                                                               
BL40     TM    OPTS,OPTREL         IF RELEASED CAST OPTION NOT SET,             
         JO    BL49                                                             
         BRAS  RE,FILTREL          FILTER OUT RELEASE CAST                      
         JE    BL90                IF RELEASED, SKIP                            
*                                                                               
BL49     OC    FULL,FULL           IF CALLER WANTS DISK ADDRESS LIST            
         JZ    *+10                                                             
         MVC   0(4,R3),TLDRDA      THEN SAVE DISK ADDRESS                       
*                                                                               
BL60     AH    R0,=H'1'            INCREMENT NUMBER OF ITEMS                    
         LA    R4,KEY              AND RESET KEY POINTER                        
*                                                                               
         LA    R3,4(R3)            BUMP TO NEXT DISK ADDRESS                    
*                                                                               
BL90     GOTO1 SEQ                 READ NEXT KEY                                
         J     BL20                LOOP BACK                                    
*                                                                               
BL100    OC    FULL,FULL           IF CALLER WANTS DISK ADDRESS LIST            
         JZ    BL110                                                            
         ST    R0,NUMITEMS         SAVE NUMBER OF ITEMS                         
         XC    ITEM,ITEM           SET ITEM NUMBER TO FIRST PAGE                
*                                                                               
BL110    CLI   ACTNUM,ACTLDEL      IF NOT ACTION LDELETE                        
         JE    *+8                                                              
         NI    DMINBTS,X'F7'       THEN CLEAR PASS BACK DELETED FLAG            
         J     BLX                                                              
*                                                                               
BLYES    SR    RC,RC                                                            
BLNO     LTR   RC,RC                                                            
BLX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LITERALS FOR BLDLIST                                         *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE TESTS IF GENCON HAS CALLED THE PROGRAM WITH ITS FIRST  *         
* MODE.  IF SO THE ROUTINE WILL ESTABLISH IF THE SCREEN HAS CHANGED             
* AND HANDLE SPECIAL INITIALIZATION LOGIC.                                      
***********************************************************************         
TESTFRST NTR1  BASE=*,LABEL=*                                                   
         TM    TRNSTAT,FRSTMODE    IF THIS IS FIRST MODE PASSED BY GENC         
         JZ    TFX                                                              
         NI    TRNSTAT,ALL-FRSTMODE   THEN RESET FIRST MODE BIT                 
*                                                                               
         MVI   ACCESSED,C'N'       CLIENTS NO LONGER HAVE CHANGE                
         CLI   TGCTSTTY,TASTTYPC   ACCESS TO ANY FIELDS                         
         JE    TF20                                                             
         CLI   TGCTSTTY,TASTTYPF                                                
         JE    TF20                                                             
         CLI   TGCTSTTY,TASTTYPD                                                
         JE    TF20                                                             
*                                                                               
         MVI   ACCESSED,C'Y'       ELSE, SET ACCESSED TO EVERYTHING             
******** MVC   FULL,CASTCMSK       VALID STAFF ACCESS BITS FOR ADD/CHG          
******** NC    FULL,SECMASKS       MASK BIT FOR THIS STAFF TYPE                 
******** JNZ   TF20                                                             
******** CLC   TWAAGY,=CL2'D2'                                                  
******** JE    TF20                                                             
******** CLC   TWAAGY,=CL2'D3'                                                  
******** JE    TF20                                                             
******** CLC   TWAAGY,=CL2'D8'                                                  
******** JE    TF20                                                             
******** MVI   ACCESSED,C'N'                                                    
*                                                                               
TF20     BAS   RE,TESTLIST         TEST SCREEN CALLED FROM CAST LIST            
*                                                                               
         MVI   SCRCHANG,C'N'       IF SCREEN HAS CHANGED THEN                   
         TM    SCRSTAT,SCRCHG          SCRCHANG = 'N'                           
         BZ    *+8                                                              
         MVI   SCRCHANG,C'Y'       ELSE SCRCHANG = 'Y'                          
*                                                                               
         CLI   SCRFLAG,C'L'        IF THIS IS THE LIST SCREEN                   
         BNE   TF50                                                             
*                                                                               
         TM    TRNSTAT,RETURNED    THEN IF JUST RETURNED FROM EXT SCR           
         BZ    TFX                                                              
         OC    CASTDA,CASTDA       AND RECORD WAS BEING CHANGED                 
         BZ    TFX                                                              
         LA    RF,DALIST           SAVE D/A OF SELECTED RECORD IN               
         A     RF,DALPTR               D/A LIST                                 
         MVC   0(4,RF),CASTDA                                                   
         B     TFX                                                              
*                                                                               
TF50     BRAS  RE,TRAPEXT                                                       
*                                  LOCK LIST AND GET ANNONLY STATUS             
         GOTO1 ABLDLIST,DMCB,0                                                  
*                                                                               
         CLI   PUSHED,C'Y'         IF EXTENSION CALLED FROM LIST                
         BNE   TFX                                                              
*                                                                               
         CLI   SCRCHANG,C'Y'       THEN IF THIS IS FIRST TIME                   
         BNE   *+8                                                              
         BAS   RE,EFINDREC         THEN GET DISK ADDRESS OF RECORD              
*                                                                               
*                                  IF USER HASN'T CHANGED REC/ACT               
         TM    TRNSTAT,RACHANG+USERCHA                                          
         BNZ   *+10                                                             
         MVC   SVACTION,CONACT     SAVE ACTION FIELD CONTENTS                   
*                                                                               
         MVC   CONACT,=CL8'SELECT' DISPLAY ACTION 'SELECT' TO USER              
*                                                                               
TFX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE USES THE CALL STACK TO DETERMINE IF THIS PROGRAM WAS             
* CALLED BY WAY OF A SELECTION FROM THE CAST LIST.  THIS KNOWLEDGE IS           
* USED BY THE EXTENSION SCREEN IN VARIOUS PLACES.  THE ROUTINE WILL             
* SET THE FLAG, PUSHED, TO 'Y' IF THE SCREEN WAS SELECTED FROM THE CAST         
* LIST AND 'N' OTHERWISE.                                                       
***********************************************************************         
TESTLIST NTR1                                                                   
         MVI   PUSHED,C'N'         SET PUSHED FLAG TO 'N'                       
*                                                                               
         CLI   CALLSP,0            IF THE STACK IS CLEAR                        
         JE    TLSX                THEN DONE                                    
*                                                                               
         ZIC   RF,CALLSP           IF THE PREV SCREEN IS NOT CAST LIST          
         LA    RF,CALLSTCK-1(RF)                                                
         CLI   0(RF),SCR6A                                                      
         JNE   TLSX                THEN DONE                                    
*                                                                               
         MVI   PUSHED,C'Y'         SET PUSHED FLAG TO 'Y'                       
*                                                                               
TLSX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE FINDS THE DISK ADDRESS LIST ENTRY FOR THE RECORD THAT            
* WAS SELECTED AND SAVES A POIONTER TO IT, ITS DISPLACEMENT FROM THE            
* BEGINNING OF DALIST, IN EDALPTR.  ADDITIONALLY IT SAVES THE DISK              
* ADDRESS IN EDISKADD FOR USE BY THE EXTENSION SCREEN CODE.                     
***********************************************************************         
EFINDREC NTR1                                                                   
         L     R0,ROWADDR          R0 = DISPLACEMENT TO LIST LINE               
         SR    R0,RA                                                            
         L     R2,ALINATAB         R2 = A(DISPLACEMENTS TO LIST LINES)          
         L     R3,ITEM             R3 = A(FIRST DISK ADDRESS OF LIST)           
         SLL   R3,2                                                             
         LA    R3,DALIST(R3)                                                    
*                                                                               
EFR10    OC    0(2,R2),0(R2)       DIE IF END OF DISPLACEMENT TABLE             
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLM   R0,3,0(R2)          IF MATCH FOUND THEN DONE                     
         JE    EFR20                                                            
*                                                                               
         LA    R2,2(R2)            ELSE BUMP R2 TO NEXT DISPLACEMENT            
         LA    R3,4(R3)            BUMP R3 TO NEXT DISK ADDRESS                 
         J     EFR10               LOOP BACK                                    
*                                                                               
EFR20    MVC   EDISKADD,0(R3)      SAVE DISK ADDRESS IN EDISKADD                
*                                                                               
         LA    RF,DALIST           SAVE POINTER IN EDALPTR                      
         SR    R3,RF                                                            
         ST    R3,EDALPTR                                                       
*                                                                               
EFRX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE TRAPS TWO UNEXCEPTABLE CONDITIONS ON THE CAST EXTENSION          
* SCREEN:                                                                       
*                                                                               
*     1) THE ACTION IS ADD.                                                     
*     2) THIS IS THE FIRST TIME THROUGH THE EXTENSION SCREEN, THE               
*        ACTION IS NOT SELECT, AND THE SCREEN WAS NOT SELECTED FROM THE         
*        CAST LIST.                                                             
*                                                                               
* IF EITHER OF THE TWO CONDITIONS ARE TRUE THE ROUTINE WILL RESTORE THE         
* SCREEN AND GIVE THE 'INVALID RECORD/ACTION COMBINATION' MESSAGE TO            
* THE USER AND EXIT.  OTHERWISE, IT RETURNS CONTROL TO THE CALLING              
* ROUTINE.                                                                      
***********************************************************************         
TRAPEXT  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF THE ACTION IS ADD                         
         JE    TE10                                                             
*                                                                               
         CLI   ACTNUM,ACTSEL       OR IF THE ACTION IS NOT SELECT               
         JE    TEX                                                              
         CLI   PUSHED,C'Y'         AND THE PREV SCREEN IS NOT CAST LIST         
         JE    TEX                                                              
         CLI   ACTNUM,ACTVER       OR IF NOT COMING FROM CAST VERIFY            
         JE    TEX                                                              
*                                  THEN READ OLD SCREEN INTO TIA                
TE10     GOTO1 GETTWA,DMCB,(X'00',ATIA)                                         
*                                                                               
         LA    R0,T702FFD          MOVE FIRST 64 BYTES TO BEGIN OF TWA          
         LA    R1,64                                                            
         L     RE,ATIA                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,=A(5010)         MOVE FROM CONTAGH TO END                     
         LA    R0,CONTAGH-T702FFD                                               
         SR    R1,R0                                                            
         LA    R0,CONTAGH                                                       
         L     RE,ATIA                                                          
         LA    RE,CONTAGH-T702FFD(RE)                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                  CLR INSERT CURSOR BIT FROM ALL FLDS          
         GOTO1 FLDVAL,DMCB,CONSERVH,(X'04',999)                                 
*                                                                               
         MVI   SVSCR,0             CLEAR PREVIOUS SCREEN NUMBER                 
         MVI   CALLSP,0            CLEAR CALLPROG STACK POINTER                 
         XC    SVACTION,SVACTION   CLEAR SAVED ACTION                           
*                                                                               
         MVI   ERROR,INVRCACT      GIVE INVALID RECORD/ACTION COMBO             
         LA    R2,CONRECH              MESSAGE AND EXIT                         
         J     END                                                              
*                                                                               
TEX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS FOR TESTFRST ROUTINE                  *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS POSITIVE CONDITION CODE IF PERFORMER IS      *         
*        RELEASED                                                               
***********************************************************************         
FILTREL  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         USING TARLD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TARLELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         CLI   TARLSTAT,C'A'                                                    
         JL    NO                                                               
         DROP  R4                                                               
         TM    OPTS,OPTLSC                                                      
         JZ    YES                                                              
         USING TACAD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACALAST,TACALAST                                                
         JZ    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
*        ON ENTRY ... AIO = A(PRIMARY EMPLOYEE RECORD)                *         
***********************************************************************         
                                                                                
DREC     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING TLCAD,R3                                                         
         L     R3,AIO                                                           
         USING TATUD,R4                                                         
         LR    R4,R3                                                            
                                                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY RECORD ROUTINES                         *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMENTS (OPTIONAL).                                                 
***********************************************************************         
VRCM     NTR1  BASE=*,LABEL=*                                                   
         L     R2,ACMFLD           R2 = A(COMMENTS FIELD)                       
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',(R2)),TACMTYPG                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SET LENGTH OF DATA IN PROTECTED FIELD AND            *         
*        RETURNS VALUE IN WORK                                                  
***********************************************************************         
                                                                                
STLENPRO NTR1  BASE=*,LABEL=*                                                   
         TM    1(R2),X'20'         IF FIELD IS PROECTED                         
         JZ    NO                                                               
                                                                                
         XC    WORK,WORK           COPY IT INTO WORK                            
         ZIC   RF,0(R2)                                                         
         SHI   RF,9                                                             
         TM    1(R2),X'02'                                                      
         JZ    *+8                                                              
         SHI   RF,8                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         OC    WORK,=64C' '                                                     
                                                                                
         LHI   R3,64                                                            
         LA    RE,WORK             PREPARE TO SET FIELD LENGTH                  
         LR    RF,R3                                                            
         LA    R4,0(RF,RE)                                                      
         SHI   R4,1                                                             
                                                                                
SLP10    CLI   0(R4),C' '          FINDS LAST NON-BLANK CHAR                    
         JH    SLP20               THAT'S THE NEW LENGTH                        
         SHI   R4,1                PREVIOUS CHAR                                
         SHI   RF,1                DECREMENT LENGTH                             
         JNZ   SLP10                                                            
                                                                                
SLP20    STC   RF,5(R2)            SET FIELD LENGTH                             
         J     YES                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD THE FIRST FOUR BYTES OF THE SORT KEY FROM THE RECORD DATA.              
***********************************************************************         
BLDSORT  NTR1  BASE=*,LABEL=*                                                   
*&&DO                                                                           
         L     R3,AIO              R3 = A(CAST RECORD)                          
         USING TLCAD,R3                                                         
*                                  PRE-CLEAR FIRST 2 BYTES OF SORT KEY          
         XC    TLCASORT(2),TLCASORT                                             
         CLI   VALMODE,C'A'        IF ADDING CAST                               
         JNE   *+10                                                             
         XC    TLCASRT,TLCASRT     PRE-CLEAR FIRST 4 BYTES OF SORT KEY          
*                                                                               
         L     R4,AIO              R4 = A(CAST ELEMENT)                         
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
*                                                                               
BS10     DS    0H                                                               
*                                                                               
         L     R4,AIO              R4 = A(CAST DETAILS ELEMENT)                 
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
*                                                                               
         OI    TLCASORT,TLCASRGQ   THEN SET NEXT HIGHEST BIT                    
*                                                                               
*                                  SAVE CATEGORY SORT CODE                      
         MVC   TLCASRSC,TGCASORT                                                
*                                                                               
*&&                                                                             
BSX      J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TEST DIRECTIONAL PFKEYS PRESSED.                                              
***********************************************************************         
TESTPFKS NTR1  BASE=*,LABEL=*                                                   
         L     R3,ITEM             R3 = ITEM NUMBER                             
         L     R4,NUMITEMS         R4 = NUMBER OF ITEMS - 1                     
         BCTR  R4,0                                                             
*                                                                               
         CLI   PFAID,8             IF PFKEY = FIRST                             
         JNE   TP10                                                             
*                                  THEN REBUILD DISK ADDRESS LIST               
         GOTO1 ABLDLIST,DMCB,1,(RC)                                             
*                                                                               
         SR    R3,R3               SET ITEM TO FIRST PAGE                       
         J     TP100                                                            
*                                                                               
TP10     CLI   PFAID,7             ELSE IF PFKEY = PREVIOUS                     
         JNE   TP20                                                             
         S     R3,=A(NUMSCR)       SET ITEM TO PREVIOUS PAGE                    
         J     TP100                                                            
*                                                                               
TP20     CLI   PFAID,0             ELSE IF PFKEY = NEXT (ENTER)                 
         JNE   TP30                                                             
         A     R3,=A(NUMSCR)       SET ITEM TO NEXT PAGE                        
*                                                                               
         CR    R3,R4               IF ITEM IS PAST LAST ITEM                    
         JNH   TP100                                                            
*                                  THEN REBUILD DISK ADDRESS LIST               
         GOTO1 ABLDLIST,DMCB,1,(RC)                                             
*                                                                               
         SR    R3,R3               SET ITEM TO FIRST PAGE                       
         J     TP100                                                            
*                                                                               
TP30     CLI   PFAID,14            ELSE IF PFKEY = LAST                         
         JNE   ERRPFK                                                           
         LR    R3,R4               THEN SET ITEM TO LAST ITEM                   
*                                                                               
TP100    LTR   R3,R3               IF ITEM IS LESS THAN ZERO                    
         JNM   *+6                                                              
         SR    R3,R3               THEN SET ITEM TO FIRST PAGE                  
*                                                                               
         ST    R3,ITEM             SAVE ITEM NUMBER                             
*                                                                               
TPX      J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* VALIDATE OPTIONS FIELD                                                        
*                                                                               
VALOPTS  NTR1  BASE=*,LABEL=*                                                   
         MVI   OPTS,0              CLEAR OPTIONS                                
                                                                                
         LA    R2,EMLOPTH          OPTIONS FIELD                                
         CLI   5(R2),0                                                          
         JE    VOPTX                                                            
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0               INVALID INPUT                                
         JZ    ERRINV                                                           
                                                                                
VOPT20   MVC   ERRDISP,SCDISP1                                                  
         CLI   SCLEN1,0            LHS IS REQUIRED                              
         JE    ERRINV                                                           
         CLC   =C'OTH',SCDATA1                                                  
         JNE   ERRINV                                                           
         OI    OPTS,OPTOTH         LIST OTHER INFORMATION                       
         J     VOPT60                                                           
                                                                                
VOPT60   LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT20                                                        
                                                                                
VOPTX    OI    4(R2),X'20'                                                      
         MVI   ERRDISP,0                                                        
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAUSEPT                                                        
       ++INCLUDE TAW4LCRP                                                       
         EJECT                                                                  
**********************************************************************          
*        DSECT FOR NMOD STORAGE GRAB AREA                            *          
**********************************************************************          
                                                                                
TMPD     DSECT                                                                  
TATDTAB  DS    XL(5*TATDLNQ+1)                                                  
UPDTSKS  DS    XL(16*L'TLTKTASK+1)                                              
UPDPTYS  DS    XL(16*L'TLPMPTYP+1)                                              
TMPLNQ   EQU   *-TMPD                                                           
         SPACE 3                                                                
**********************************************************************          
*        SAVED STORAGE                                               *          
**********************************************************************          
                                                                                
NAMESD   DSECT                                                                  
         DS    XL8                                                              
NAMSSN   DS    CL23                W4 NAME                                      
         DS    XL2                                                              
NAMCORP  DS    CL23                CORPORATION NAME                             
         DS    XL2                                                              
NAMPAYE  DS    CL23                EXPIRATION DATE                              
         SPACE 3                                                                
**********************************************************************          
*        EVENT TIME DETAILS                                          *          
**********************************************************************          
EVTDD    DSECT                                                                  
EVTDTSKH DS    XL8                                                              
EVTDTSK  DS    CL6                 TASK                                         
EVTDPTPH DS    XL8                                                              
EVTDPTP  DS    CL6                 PAY TYPE                                     
EVTDAMTH DS    XL8                                                              
EVTDAMT  DS    CL12                AMOUNT                                       
EVTDTKNH DS    XL8                                                              
EVTDTKN  DS    CL24                TASK NAME                                    
EVTDPTNH DS    XL8                                                              
EVTDPTN  DS    CL24                PAYTYPE NAME                                 
EVTDLNQ  EQU   *-EVTDD                                                          
                                                                                
**********************************************************************          
HEAD3D   DSECT                     HEADLINE 3 DSECT                             
H3AGYT   DS    CL(L'EMLAGYT)        AGENCY TAG                                  
         DS    CL3                                                              
H3AGY    DS    CL(L'EMLAGY)        AGENCY TAG                                   
         DS    CL3                                                              
H3EVID   DS    CL(L'EMLEVID)       EVENT ID                                     
         DS    CL3                                                              
H3EVT    DS    CL(L'EMLEVT)        EVENT                                        
         DS    CL1                                                              
H3EVTN   DS    CL(L'EMLEVTN)       EVENT NAME                                   
         SPACE 3                                                                
**********************************************************************          
*        PRINT LINE DSECT                                            *          
**********************************************************************          
PLINED   DSECT                     PRINT LINE DSECT                             
         DS    CL4                                                              
PLSSN    DS    CL9                 SSN                                          
         DS    CL2                                                              
PLCORP   DS    CL1                 CORP                                         
         DS    CL5                                                              
         SPACE 3                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR69D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR6AD                                                       
         DS    D                                                                
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
       ++INCLUDE TAGEN5ED                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TAGEN5E   04/28/15'                                      
         END                                                                    
