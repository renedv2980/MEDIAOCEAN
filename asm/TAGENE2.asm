*          DATA SET TAGENE2    AT LEVEL 101 AS OF 05/12/15                      
*PHASE T702E2A,*                                                                
*INCLUDE TWABLD                                                                 
         TITLE 'T702E2 - STEREO TRANSFER'                                       
T702E2   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKIOL,T702E2,R7,RR=R2,CLEAR=YES                                 
         SPACE 2                                                                
         LR    R3,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R6,TWAHOLE          R6=A(LOCAL SAVED STORAGE)                    
         USING STEREOD,R6                                                       
         USING FAWSSVRD,WORK                                                    
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 3                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         ST    R2,RELO             RELOCATION FACTOR                            
         ST    R3,AWRKIO           R3=A(WORKING IO AREA)                        
         LH    R1,=AL2(L'IO*3)                                                  
         AR    R3,R1                                                            
         ST    R3,AMAINIO          A(MAIN IO)                                   
         LH    R1,=AL2(L'IO)                                                    
         AR    R3,R1                                                            
         ST    R3,AMISCTAB         A(MISCTAB)                                   
         LH    R1,=AL2(MISCTABL)                                                
         AR    R3,R1                                                            
         ST    R3,ATMPIO           A(TEMP IO)                                   
         LH    R1,=AL2(L'IO)                                                    
         AR    R3,R1                                                            
         ST    R3,AKEYUHM          A(AREA FOR KEY)                              
*&&DO                                                                           
         TM    PRGSTAT,TESTSYS     IF NOT THE TEST SYSTEM                       
         BO    *+12                                                             
         TM    TRNSTAT2,CTSTEREO   AND STEREO IS NOT ACTIVE                     
         BZ    ERRECINV            GIVE ERROR RECORD INVALID                    
*&&                                                                             
         OI    PRGSTAT,EASY32      SET USER EASYEST32 FOR EST/REPORT            
         BAS   RE,INITRTN          RELOCATE ROUTINES                            
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE KEY                                 
         BNE   XIT                                                              
         MVC   CONHEAD,SCONHEAD    RESTORE MESSAGES                             
         BAS   RE,INIFALNK         INITIALIZE FALINK BLOCK                      
         TM    TWACOMMN+1,X'40'    TEST FALINK SAVE AREA SAVED                  
         JZ    INIT02                                                           
         XC    FAWSSVRD(FAWSSVRL),FAWSSVRD                                      
         MVC   FAWSTOKN,FALKTOKN                                                
         MVI   FAWSACTN,FAWSURST   SET TO RESTORE LAST SAVE AREA                
         LA    R0,SVFALINK                                                      
         ST    R0,FAWSADR                                                       
         GOTOR WSSVR,FAWSSVRD      RESTORE LAST FALINK SAVE AREA                
         CLI   FAWSRTN,0                                                        
         JE    INIT02                                                           
         DC    H'0'                DIE IF CAN'T RESTORE SAVE AREA               
*                                                                               
INIT02   GOTO1 FALINK,DMCB,FABLK   GIVE FALINK CONTROL                          
*                                                                               
         XC    FAWSSVRD(FAWSSVRL),FAWSSVRD                                      
         MVC   FAWSTOKN,FALKTOKN                                                
         MVI   FAWSACTN,FAWSUSVE   SET TO SAVE FALINK SAVE AREA                 
         MVC   FAWSLEN,=AL2(L'SVFALINK)                                         
         LA    R0,SVFALINK                                                      
         ST    R0,FAWSADR                                                       
         GOTOR WSSVR,FAWSSVRD      RESTORE LAST FALINK SAVE AREA                
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    TWACOMMN+1,X'40'    SET FALINK SAVE AREA SAVED                   
         J     XIT                                                              
         EJECT                                                                  
*              RELOCATE ROUTINES                                                
         SPACE 1                                                                
INITRTN  NTR1                                                                   
         L     R1,=V(TWABLD)                                                    
         A     R1,RELO                                                          
         ST    R1,ATWABLD                                                       
*                                                                               
         LA    R1,RTNTAB           R1=A(ROUTINES TO RELOCATE)                   
         LA    R2,ARTNS            R2=A(ADDRESS OF ROUTINES)                    
         LA    R0,NRTNS            R0=(NUMBER OF ROUTINES)                      
*                                                                               
INITRTN2 L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(R2)                                                         
         LA    R1,4(R1)            BUMP TO NEXT TABLE ENTRY                     
         LA    R2,4(R2)            BUMP TO NEXT ADDRESS                         
         BCT   R0,INITRTN2                                                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SETUP FALINK BLOCK                                    
         SPACE 1                                                                
INIFALNK NTR1                                                                   
         LA    R2,FABLK                                                         
         USING FALINKD,R2                                                       
         LA    R1,STERECH          SET A(FIRST SCREEN POSITION)                 
         ST    R1,FALABLD                                                       
         MVC   FALTBLD,ATWABLD         A(TWABLD)                                
         MVC   FALASWCH,SWITCH         A(SWITCH)                                
         LA    R1,RECEIVE              A(MY RECEIVE ROUTINE)                    
         ST    R1,FALARCV                                                       
         LA    R1,SEND                 A(MY SEND ROUTINE)                       
         ST    R1,FALASND                                                       
         LA    R1,ELTRANS              A(ELEMENT TRANSLATION ROUTINE)           
         ST    R1,FALATRN                                                       
         LA    R1,BREAK                A(MY BREAK ROUTINE)                      
         ST    R1,FALASTP                                                       
         LA    R1,RESUME               A(MY RESUME ROUTINE)                     
         ST    R1,FALARSM                                                       
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R1,FAMSGBLK             A(MESSAGE BLOCK)                         
         ST    R1,FALAMSG                                                       
         LA    R1,FACON                A(CONTROL FIELD BUFFER)                  
         ST    R1,FALACON                                                       
         LA    R1,SVFALINK             A(FALINK SAVED STORAGE)                  
         ST    R1,FALASVE                                                       
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)                                                         
         ST    RE,FALATIOB             A(TIOB)                                  
         MVC   FALAPGS,=AL4(FALATMS)   USE TEMPEST PAGES                        
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO RECEIVE DATA FROM FALINK                              
         SPACE 1                                                                
RECEIVE  NTR1                                                                   
         MVC   AGETDATA,0(R1)      SAVE A(GETDATA RTN) IN FALINK                
         BRAS  RE,INITRCV          INITIALIZE SOME VARIABLES FOR RCV            
*                                  GET FIRST(/NEXT) DATA FROM FALINK            
RCV5     GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BL    XIT                 FALINK ERROR - GET OUT                       
         BNE   RCV20               NO MORE DATA TO RECEIVE                      
*                                                                               
         L     R5,FPARMS           R5=A(CURRENT ENTRY)                          
         CLI   FPARMS,0            IF HEADER TYPE DATA                          
         BNE   *+12                                                             
         BAS   RE,PRHDR            PROCESS IT                                   
         B     *+8                                                              
         BAS   RE,PRFLD            ELSE, PROCESS FIELD DATA                     
         BL    XIT                 ERROR DETECTED - TELL FALINK                 
         B     RCV5                GET NEXT PIECE OF DATA                       
*                                                                               
RCV20    LARL  RE,FLTTAB           NO MORE DATA - CHECK FOR LAST RTN            
RCV22    CLI   0(RE),X'FF'                                                      
         BE    YES                                                              
         CLC   FLTACTN,0(RE)       BASED ON FILTER ACTION                       
         BE    *+12                                                             
         LA    RE,L'FLTTAB(RE)                                                  
         B     RCV22                                                            
*                                                                               
         ICM   RF,15,1(RE)         LET ROUTINE DO IT'S THING                    
         AR    RF,RB                                                            
         LA    RE,RCVX                                                          
         BR    RF                                                               
*                                                                               
RCVX     B     XIT                 CC SET                                       
         EJECT                                                                  
*              ROUTINE FINDS TABLE ENTRY FOR FALINK BASED ON TEXT               
         SPACE 1                                                                
ELTRANS  NTR1                                                                   
         XC    WORK,WORK                                                        
         L     R2,0(R1)            R2=A(ELEMENT TEXT)                           
         ZIC   RE,0(R1)            L'ELEMENT TEXT                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R2)                                                    
         OC    WORK,SPACES                                                      
*                                                                               
         L     R5,AMAPTAB          R5=A(MAP TABLE)                              
         USING MHELD,R5                                                         
ELTRANS5 CLI   MHLEN,MHLENX        IF NOT END OF TABLE                          
         BE    LOW                                                              
         TM    MHUSTAT,MHUSKEY     IF KEYDATA                                   
         BZ    ELTRANS8                                                         
         CLC   MHUKTXT,WORK        MATCH ON TEXT FOR KEY                        
         BE    ELTRANS9                                                         
         B     *+14                                                             
*                                                                               
ELTRANS8 CLC   MHUTEXT,WORK        ELSE MATCH ON TEXT FOR ELEMENT               
         BE    ELTRANS9                                                         
         XR    RE,RE               BUMP TO NEXT HDR ENTRY                       
         ICM   RE,3,MHDISP                                                      
         AR    R5,RE                                                            
         B     ELTRANS5                                                         
*                                                                               
ELTRANS9 ST    R5,4(R1)            RETURN A(ENTRY)                              
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO PROCESS HEADER DATA                                   
         SPACE 1                                                                
         USING MHELD,R5            R5=A(HEADER ENTRY)                           
PRHDR    NTR1                                                                   
         TM    MHUSTAT,MHUSKEY     IF KEY HEADER                                
         BZ    PRHDR2                                                           
         OI    STESTAT,STESKEY     SET PROCESSING KEY                           
         ST    R5,AKEYNTRY         SAVE A(THIS KEY ENTRY)                       
         MVC   MYKCODEQ,MHUKCDE    SAVE KEY EQUATE                              
         MVC   MYKCODE,MHUKTXT     SAVE TEXT                                    
         MVC   MYKLEN,MHUKLEN      SET KEY COMPARE LENGTH                       
*                                                                               
         XR    RF,RF               PASS CONTROL TO CLEAR ROUTINES               
         ICM   RF,3,MHUKCLR                                                     
         BZ    PRHDRX                                                           
         AR    RF,RB                                                            
         LA    RE,PRHDRX           THEN EXIT                                    
         NTR1                                                                   
         BR    RF                                                               
*                                                                               
PRHDR2   CLI   MHCODE+1,TAFEELQ    IF SPECIAL VERSION NUMBER ELEMENTS           
         BE    *+12                                                             
         CLI   MHCODE+1,TAFDELQ                                                 
         BNE   PRHDR4                                                           
         OI    STESTAT2,STESVERS    SET SPECIAL INDICATOR FOR DATA              
         NI    STESTAT2,X'FF'-STESCHKV  DEFAULT CHECK NOT REQUIRED              
         CLI   MHCODE+1,TAFEELQ                                                 
         BNE   *+8                                                              
         OI    STESTAT2,STESCHKV   SET VERSION CHECK REQUIRED                   
         B     PRHDRX                                                           
*                                                                               
PRHDR4   NI    STESTAT,X'FF'-STESKEY SET NO LONGER PROCESSING KEY               
         TM    STESTAT,STESINIT    IF ESTIMATE RECORD INITALIZED                
         BO    PRHDR5                                                           
         CLI   MYKCODEQ,TLESCDQ    AND ESTIMATE RECORD REQUEST                  
         BNE   ERRIKEY                                                          
         CLI   FLTACTN,C'U'        FOR UPLOAD                                   
         BNE   ERRIKEY                                                          
         CLC   TGAGY,SPACES        PROTECT AGAINST WRITING BAD RECORD           
         BNH   ERRAGY                                                           
         CLC   TGEST,SPACES                                                     
         BNH   ERRIEST                                                          
         BRAS  RE,INITES           INITIALIZE ESTIMATE RECORD                   
         OI    STESTAT,STESINIT                                                 
*                                                                               
PRHDR5   GOTO1 AMYADDL,DMCB,(RC)   ADD LAST ELEMENT TO EST. RECORD              
         BL    LOW                                                              
         XC    ANXTSUB,ANXTSUB     CLEAR A(NEXT SUB-OPTION) IN TAESD            
         CLI   MHCODE+1,TAACELQ    IF ACTIVITY ELEMENT                          
         BNE   PRHDR8                                                           
         GOTO1 ACTVIN,DMCB,(X'40',0) LET ACTVIN SET UP ELEMENT                  
         B     PRHDRX                                                           
PRHDR8   XC    ELEMENT,ELEMENT       ELSE, SET UP NEW ELEMENT                   
         MVC   ELEMENT(1),MHCODE+1   ELEMENT CODE                               
         MVC   ELEMENT+1(1),MHUBLEN  MINIMUM ELEMENT LENGTH                     
*                                                                               
PRHDRX   B     YES                                                              
         EJECT                                                                  
*              CLEAR NECESSARY GLOBAL FIELDS CALLED FROM PRHDR                  
         SPACE 2                                                                
CLRCAG   XC    TGCOM,TGCOM                                                      
         XC    TGCAT,TGCAT                                                      
         XC    TGCSORT,TGCSORT                                                  
         B     XIT                                                              
*                                                                               
CLRCA    XC    TGCSORT,TGCSORT                                                  
         XC    TGSSN,TGSSN                                                      
         XC    TGCAT,TGCAT                                                      
         B     XIT                                                              
*                                                                               
CLRGU    XC    TGGUA,TGGUA                                                      
         XC    TGAGY,TGAGY                                                      
         XC    TGSSN,TGSSN                                                      
         XC    TGCLI,TGCLI                                                      
         B     XIT                                                              
*                                                                               
CLRCOL   XC    TGAGY,TGAGY                                                      
         XC    TGCLI,TGCLI                                                      
         XC    TGPRD,TGPRD                                                      
         XC    TGCID,TGCID                                                      
         XC    TGCLG,TGCLG                                                      
         B     XIT                                                              
*                                                                               
CLRCO    XC    TGAGY,TGAGY                                                      
         XC    TGCID,TGCID                                                      
         B     XIT                                                              
*                                                                               
CLRCP    XC    TGAGY,TGAGY                                                      
         XC    TGCLI,TGCLI                                                      
         XC    TGPRD,TGPRD                                                      
         B     XIT                                                              
*                                                                               
CLRESL   XC    TGAGY,TGAGY                                                      
         XC    TGEST,TGEST                                                      
         XC    TGCLI,TGCLI                                                      
         XC    TGPRD,TGPRD                                                      
         B     XIT                                                              
*                                                                               
CLRESR   XC    TGAGY,TGAGY                                                      
         XC    TGEST,TGEST                                                      
         B     XIT                                                              
*                                                                               
CLRAY    XC    TGAGY,TGAGY                                                      
         B     XIT                                                              
*                                                                               
CLRUHL   XC    TGCOM,TGCOM                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS FIELD DATA                                    
         SPACE 1                                                                
         USING MDELD,R5            R5=A(FIELD DATA ENTRY)                       
PRFLD    NTR1                                                                   
         TM    STESTAT2,STESVERS   IF PROCESSING VERSION INFORMATION            
         BZ    *+12                                                             
         BAS   RE,SAVEVER          SAVE VERSION ID                              
         B     XIT                                                              
         SPACE 1                                                                
         TM    STESTAT,STESKEY     IF PROCESSING FIELD DATA FOR KEY             
         BZ    *+12                                                             
         BAS   RE,PRKYFLD          PROCESS IT                                   
         B     *+8                                                              
         BAS   RE,BLDDET           ELSE, BUILD ELEMENT DATA                     
         B     XIT                 CC SET (LOW ERROR - ELSE OKAY)               
         EJECT                                                                  
*              SPECIAL ROUTINE TO HANDLE SETTING VERSION NUMBER                 
         SPACE 1                                                                
SAVEVER  NTR1                                                                   
         L     RE,FPARMS+4         MOVE CONVERTED DATA APPROP. LENGTH           
         L     R1,FPARMS+8                                                      
         LTR   R1,R1               MAKE SURE FALINK GIVES YOU A LENGTH          
         BZ    ERRILEN                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VERSNO,0(RE)        SAVE VERSION NUMBER                          
         NI    STESTAT2,X'FF'-STESVERS                                          
         B     YES                                                              
         SPACE 2                                                                
*              ROUTINE SETS DATA IN APPROP. KEY FIELD & VALIDATES IT            
         SPACE 1                                                                
PRKYFLD  NTR1                                                                   
         XR    RF,RF               SET DATA TO APPROPRIATE FIELD                
         ICM   RF,3,MDUDISP                                                     
*                                                                               
         LA    R2,TGD                                                           
         TM    MDUSTAT,MDTGD                                                    
         BO    *+8                                                              
         LA    R2,FLTDATA                                                       
         AR    R2,RF                                                            
*                                                                               
         L     RE,FPARMS+4         MOVE CONVERTED DATA APPROP. LENGTH           
         L     R1,FPARMS+8                                                      
         LTR   R1,R1               MAKE SURE FALINK GIVES YOU A LENGTH          
         BZ    ERRILEN                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)                                                    
*                                                                               
         CLI   MDTYPE,MDTCHQ       IF CHARACTER DATA                            
         BNE   PRKYFLD5                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R2),SPACES      PAD WITH SPACES                              
*                                                                               
PRKYFLD5 TM    MDUSTAT2,MDCOMP     IF NEED TO COMPLEMENT DATA                   
         BZ    PRKYFLD8                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),HEXFFS                                                   
*                                                                               
PRKYFLD8 XR    RF,RF               VALIDATE THE DATA                            
         ICM   RF,3,MDURCV                                                      
         BZ    YES                                                              
         AR    RF,RB                                                            
         LA    RE,PRKYFLDX                                                      
         NTR1                                                                   
         BR    RF                  CC SET (LOW ERROR - ELSE OKAY)               
*                                                                               
PRKYFLDX B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES FIELD DATA CALLED FROM PRFLD                   
*                                                                               
         USING MDELD,R5            R5=A(DATA ENTRY)                             
         SPACE 1                                                                
VAGY     TM    MDUSTAT2,MDLOCL                                                  
         BZ    VAGY3                                                            
         MVC   TGAGY,FLTAGY        SET GLOBAL FOR READ                          
VAGY3    GOTO1 RECVAL,DMCB,TLAYCDQ,0                                            
         BNE   ERRAGY                                                           
         GOTO1 GETREC                                                           
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   ERRAGY                                                           
         MVC   TGAYSTA7,TAAYSTA7                                                
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         B     YES                                                              
         SPACE 1                                                                
VCLI     TM    MDUSTAT2,MDLOCL                                                  
         BZ    *+10                                                             
         MVC   TGCLI,FLTCLI        SET GLOBAL FOR READ                          
         GOTO1 RECVAL,DMCB,TLCLCDQ,0                                            
         BNE   ERRCLI                                                           
         B     YES                                                              
         SPACE 1                                                                
VPRD     TM    MDUSTAT2,MDLOCL                                                  
         BZ    *+10                                                             
         MVC   TGPRD,FLTPRD        SET GLOBAL FOR READ                          
         TM    MDUSTAT2,MDNOCLI    IF PRODUCT OKAY WITH CLIENT                  
         BZ    *+14                                                             
         OC    TGCLI,TGCLI         IF NO CLIENT                                 
         BZ    YES                 DON'T BOTHER VALIDATING                      
         GOTO1 RECVAL,DMCB,TLPRCDQ,0                                            
         BNE   ERRPRD                                                           
         B     YES                                                              
         SPACE 1                                                                
VSSN     GOTO1 RECVAL,DMCB,TLW4CDQ,0                                            
         BNE   ERRSSN                                                           
         B     YES                                                              
         SPACE 1                                                                
VCLG     TM    MDUSTAT2,MDLOCL                                                  
         BZ    *+10                                                             
         MVC   TGCLG,FLTCLG        SET GLOBAL FOR READ                          
         GOTO1 RECVAL,DMCB,TLCGCDQ,0                                            
         BNE   ERRCLG                                                           
         B     YES                                                              
         SPACE 1                                                                
VUHM     L     R5,ANEXTUHM                                                      
         MVC   0(4,R5),TGCOM                                                    
         LA    R5,4(R5)                                                         
         ST    R5,ANEXTUHM                                                      
         B     XIT                                                              
         SPACE 1                                                                
VACTN    LARL  RE,FLTTAB           U=UPLOAD,V=VALIDATE,D=DEL,R=RESTORE          
VACTN5   CLI   0(RE),X'FF'                                                      
         BE    ERRINV                                                           
         CLC   FLTACTN,0(RE)                                                    
         BE    YES                                                              
         LA    RE,L'FLTTAB(RE)                                                  
         B     VACTN5                                                           
         SPACE 1                                                                
VDTFLG   CLI   FLTRDFLG,C'Y'                                                    
         BNE   ERRINV                                                           
         B     YES                                                              
         SPACE 1                                                                
VDTFLG2  CLI   FLTCDFLG,C'Y'                                                    
         BNE   ERRINV                                                           
         B     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE BUILDS ELEMENT DETAILS ON ESR UPLOAD                     
         SPACE 1                                                                
         USING MDELD,R5            R5=A(ELEMENT ENTRY)                          
BLDDET   NTR1                                                                   
         TM    MDUSTAT2,MDESO      IF NOT ESTIMATE SUB-OPTION                   
         BO    BLDDET10                                                         
         LA    R4,ELEMENT          USE DISPLACEMENT OFF OF ELEMENT              
         LH    R0,MDUDISP                                                       
         AR    R4,R0               R4=A(ELEMENT FIELD)                          
         B     BLDDET20                                                         
*                                                                               
BLDDET10 OC    ANXTSUB,ANXTSUB     IF NOT FIRST SUB-OPTION IN ELEMENT           
         BZ    *+12                                                             
         L     R4,ANXTSUB          R4=A(NEXT SUB-OPTION POSITION)               
         B     BLDDET12                                                         
         LA    R4,ELEMENT          ELSE, R4=(FIRST SUB-OPTION POSITION)         
         LA    R4,TAESLNQ(R4)                                                   
*                                                                               
BLDDET12 MVC   0(1,R4),MDUCODE     SET SUB-ELEMENT CODE                         
         LA    R4,1(R4)            AND POSITION FOR SUB-ELEMENT DATA            
         ZIC   RE,ESOPTCNT         AND ADD TO ESTIMATE SUB-OPTION COUNT         
         LA    RE,1(RE)                                                         
         STC   RE,ESOPTCNT                                                      
*                                                                               
BLDDET20 L     R2,FPARMS+4         R2=A(CONVERTED DATA)                         
         CLI   MDTYPE,MDTCHQ       IF CHARACTER DATA                            
         BNE   *+8                                                              
         BAS   RE,PADIT            PAD IT W/SPACES FOR MAX TABLE LENGTH         
         L     R3,FPARMS+8                                                      
         LTR   R3,R3                                                            
         BZ    BLDDET22            I.E MAP CODE ONLY                            
         BCTR  R3,0                R3=A(CONVERTED LENGTH - 1)                   
*                                                                               
BLDDET22 XR    RF,RF               IF UPLOAD ROUTINE SPECIFIED                  
         ICM   RF,3,MDUUP                                                       
         LTR   RF,RF                                                            
         BZ    BLDDET35                                                         
         AR    RF,RB               GO LET IT PROCESS DATA                       
         LA    RE,BLDDET30                                                      
         NTR1 ,                                                                 
         BR    RF                                                               
BLDDET30 BL    XIT                 ERROR                                        
         B     BLDDET40                                                         
*                                                                               
BLDDET35 EX    R3,*+8              ELSE, JUST MOVE CONVERTED DATA               
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
*                                                                               
BLDDET40 LA    R3,1(R3)            RESTORE CONVERTED DATA LENGTH                
         TM    MDUSTAT2,MDESO      IF ESTIMATE SUB-OPTION                       
         BZ    BLDDET50                                                         
         XR    R3,R3                                                            
         CLI   MDUCODE,OPCOMNM     IF HYPO COMMERCIAL NAME                      
         BNE   *+12                                                             
         IC    R3,0(R4)            LENGTH IS IN FIRST DATA BYTE                 
         B     *+8                                                              
         IC    R3,MDULEN           ELSE, USE USER DEFINED TABLE LENGTH          
         AR    R4,R3               BUMP PAST DATA                               
         ST    R4,ANXTSUB                                                       
         LA    R3,1(R3)            ADD LENGTH FOR SUB-OPTION CODE               
         B     *+12                                                             
BLDDET50 TM    MDUSTAT,MDCLEN      OR IF NEED TO CALCULATE LENGTH               
         BZ    BLDDET60                                                         
*                                                                               
         ZIC   RF,ELEMENT+1        THEN TO ELEMENT LENGTH                       
         AR    RF,R3               ADD CONVERTED LENGTH                         
         CH    RF,=H'255'          IF LENGTH NOT MORE THAN MAX                  
         BH    ERRELEM                                                          
         STC   RF,ELEMENT+1                                                     
*                                                                               
BLDDET60 B     YES                                                              
         SPACE 2                                                                
*              ROUTINE TO PAD CHARACTER DATA WITH SPACES                        
         SPACE 1                                                                
PADIT    NTR1                                                                   
         ZIC   R3,MDDLEN                                                        
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     XIT                                                              
         OC    0(0,R2),SPACES      PAD WITH SPACES                              
         DROP  R5                                                               
         EJECT                                                                  
*              UPLOAD ELEMENT FIELD ROUTINES TO APPROPRIATE                     
*              DISPLACEMENT INTO ELEMENT                                        
*                                                                               
         USING MDELD,R5            R5=A(MAP TABLE ENTRY)                        
*                                  R2=A(CONVERTED DATA FROM FALINK)             
*                                  R3=A(CONVERTED LENGTH FROM FALINK)           
*                                  R4=A(AREA TO SET RESULT)                     
         SPACE 2                                                                
UMED     GOTO1 MEDVAL,DMCB,0(R2)   GET MEDIA EQUATE                             
         BNE   ERRIMED                                                          
         MVC   0(L'TGMEEQU,R4),TGMEEQU                                          
         B     YES                                                              
         SPACE 2                                                                
UUNI     GOTO1 UNIVAL,DMCB,0(R2)   GET UNION EQUATE                             
         BNE   ERRIUNI                                                          
         MVC   0(L'TGUNEQU,R4),TGUNEQU                                          
         B     YES                                                              
         SPACE 2                                                                
UCAT     GOTO1 CATVAL,DMCB,0(R2)  GET CATEGORY EQUATE                           
         BNE   ERRICAT                                                          
         MVC   0(L'TGCAEQU,R4),TGCAEQU                                          
         B     YES                                                              
*                                                                               
UTANU    EDIT  (C3,0(R2)),(3,0(R4)),FILL=0                                      
         B     YES                                                              
*                                                                               
UCID     EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
         EX    R3,*+8                                                           
         B     YES                                                              
         MVC   TGCID(0),0(R2)                                                   
*                                                                               
UHCID    CLC   10(2,R2),SPACES     HYPO CID MORE THAN 10 CHARS?                 
         BH    UHCID2              YES, COMPRESS IT                             
         EX    R3,*+8              NO, OLD WAY                                  
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
         EX    R3,*+8                                                           
         B     YES                                                              
         MVC   TGCID(0),0(R2)                                                   
*                                                                               
UHCID2   GOTO1 =A(CHPACK),DMCB,(C'P',(R2)),(R4),RR=RELO                         
         MVC   TGCID(L'TAESHCOM),0(R4)                                          
         LR    RF,R4                                                            
         AHI   RF,-1                                                            
         B     YES                                                              
         EJECT                                                                  
*              UPLOAD ELEMENT FIELD ROUTINES TO APPROPRIATE                     
*              DISPLACEMENT INTO ELEMENT (CONTINUED)                            
*                                                                               
*                                  R2=A(CONVERTED DATA FROM FALINK)             
*                                  R3=A(CONVERTED LENGTH FROM FALINK)           
*                                  R4=A(AREA TO SET RESULT)                     
         SPACE 1                                                                
IYR      GOTO1 YRVAL,DMCB,0(R2)    GET CONTRACT YEAR EQUATE                     
         BNE   ERRIYR                                                           
         MVC   0(L'TGYREQU,R4),TGYREQU                                          
         B     YES                                                              
         SPACE 2                                                                
IATYPE   GOTO1 CCTYPVAL,DMCB,0(R2)   GET COMMERCIAL ACTRA TYPE EQUATE           
         BNE   ERRIACTY                                                         
         MVC   0(L'TGCCTEQU,R4),TGCCTEQU                                        
         B     YES                                                              
         SPACE 2                                                                
ICOMNM   EX    R3,*+8              HYPO COMMERCIAL NAME                         
         B     *+10                                                             
         MVC   1(0,R4),0(R2)                                                    
         LA    R3,2(R3)                                                         
         STC   R3,0(R4)            AND LENGTH                                   
         B     YES                                                              
         SPACE 2                                                                
IUSE     BRAS  RE,ADJUSE                                                        
         SPACE 1                                                                
         GOTO1 USEVAL,DMCB,(X'10',0(R2)),3(R2)                                  
         BNE   ERRIUSE                                                          
         MVC   0(1,R4),TGUSEQU     RETURN USE EQUATE                            
         MVC   1(1,R4),TGUSTYP        AND USE TYPE EQUATE                       
         B     YES                                                              
         SPACE 2                                                                
IEXPDT   MVC   0(LOPDATE,R4),0(R2)                                              
         B     *+8                                                              
IEXPY    MVI   0(R4),X'FF'                                                      
         BCTR  R4,0                                                             
         MVI   0(R4),OPEXP                                                      
         B     YES                                                              
         EJECT                                                                  
*              UPLOAD ELEMENT FIELD ROUTINES TO APPROPRIATE                     
*              DISPLACEMENT INTO ELEMENT (CONTINUED)                            
*                                                                               
*                                  R2=A(CONVERTED DATA FROM FALINK)             
*                                  R3=A(CONVERTED LENGTH FROM FALINK)           
*                                  R4=A(AREA TO SET RESULT)                     
         SPACE 2                                                                
ICSF     L     R1,0(R2)            CSF                                          
         C     R1,=F'-2'           -2=YES                                       
         BNE   *+12                                                             
         MVI   0(R4),C'Y'                                                       
         B     ICSFX                                                            
         C     R1,=F'-3'           -3=NO                                        
         BNE   *+12                                                             
         MVI   0(R4),C'N'                                                       
         B     ICSFX                                                            
         ST    R1,0(R4)                                                         
ICSFX    B     YES                                                              
         SPACE 2                                                                
IUSES    MVC   0(2,R4),0(R2)       SET START USE NUMBER                         
         GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BE    *+8                                                              
         B     ERRIUSRG                                                         
         L     R5,FPARMS                                                        
         CLI   MDUCODE,OPUSES      NEXT ITEM IS # OF USES                       
         BNE   ERRIUSRG                                                         
         L     R2,FPARMS+4                                                      
         LH    R1,0(R4)            START USE                                    
         LH    RE,0(R2)            + (NUMBER OF USES -1)                        
         BCTR  RE,0                                                             
         AR    R1,RE                                                            
         STH   R1,2(R4)            = END USE NUMBER                             
         BCTR  R4,0                RESET SUB-ELEMENT CODE                       
         MVI   0(R4),OPUSES2       TO USE RANGE                                 
         B     YES                                                              
         SPACE 2                                                                
ICYC     MVC   LOPDATE(LOPDATE,R4),0(R2) END DATE                               
         GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BE    *+8                                                              
         B     ERRICYC                                                          
         L     R5,FPARMS           NEXT ITEM MUST BE START DATE                 
         CLI   MDUCODE,OPDATE                                                   
         BNE   ERRICYC                                                          
         L     R2,FPARMS+4                                                      
         MVC   0(LOPDATE,R4),0(R2)                                              
         BCTR  R4,0                RESET SUB-ELEMENT CODE                       
         MVI   0(R4),OPCYC         TO CYCLE DATES                               
         B     YES                                                              
         EJECT                                                                  
*              UPLOAD ELEMENT FIELD ROUTINES TO APPROPRIATE                     
*              DISPLACEMENT INTO ELEMENT (CONTINUED)                            
*                                                                               
*                                  R2=A(CONVERTED DATA FROM FALINK)             
*                                  R3=A(CONVERTED LENGTH FROM FALINK)           
*                                  R4=A(AREA TO SET RESULT)                     
         SPACE 1                                                                
IYRAS    MVC   0(LOPDATE,R4),0(R2) SET DATE                                     
         GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BE    *+8                                                              
         B     ERRIYR                                                           
         L     R5,FPARMS                                                        
         CLI   MDUCODE,OPYEAR      NEXT ITEM MUST BE YEAR                       
         BNE   ERRIYR                                                           
         L     R2,FPARMS+4                                                      
         OC    0(L'TGYRCDE,R2),SPACES                                           
         GOTO1 YRVAL,DMCB,0(R2)                                                 
         BNE   ERRIYR                                                           
         MVC   LOPDATE(L'TGYREQU,R4),TGYREQU                                    
         BCTR  R4,0                RESET SUB-ELEMENT CODE                       
         MVI   0(R4),OPYRAS        TO YEAR AS/OF A DATE                         
         B     YES                                                              
         SPACE 2                                                                
IHLD     MVC   0(LOPDATE,R4),0(R2) SET DATE                                     
         GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BE    *+8                                                              
         B     ERRIHLD                                                          
         L     R5,FPARMS                                                        
         L     R2,FPARMS+4         FOLLOWED BY RATE/AMOUNT                      
         MVC   LOPDATE(LOPHLD,R4),0(R2)                                         
         LA    R1,OPHLDRAS                                                      
         CLI   MDUCODE,OPHLDR                                                   
         BE    *+8                                                              
         LA    R1,OPHLDAS                                                       
         BCTR  R4,0                RESET SUB-ELEMENT CODE                       
         STC   R1,0(R4)            TO HLDING FEE AMT/RATE AS/OF A DATE          
         B     YES                                                              
         EJECT                                                                  
         EJECT                                                                  
*              UPLOAD ELEMENT FIELD ROUTINES TO APPROPRIATE                     
*              DISPLACEMENT INTO ELEMENT (CONTINUED)                            
*                                                                               
*                                  R2=A(CONVERTED DATA FROM FALINK)             
*                                  R3=A(CONVERTED LENGTH FROM FALINK)           
*                                  R4=A(AREA TO SET RESULT)                     
         SPACE 1                                                                
IOV      MVC   0(LOPDATE,R4),0(R2) SET DATE                                     
         GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BE    *+8                                                              
         B     ERRIOV                                                           
         L     R5,FPARMS           NEXT ITEM MUST BE OV1 OR OV2                 
         CLI   MDUCODE,OPOVER                                                   
         BE    *+8                                                              
         CLI   MDUCODE,OPOVER2                                                  
         BNE   ERRIOV                                                           
         L     R2,FPARMS+4                                                      
         MVC   LOPDATE(LOPOVER,R4),0(R2)                                        
         LA    R1,OPOVAS                                                        
         CLI   MDUCODE,OPOVER                                                   
         BE    *+8                                                              
         LA    R1,OPOV2AS                                                       
         BCTR  R4,0                RESET SUB-ELEMENT CODE                       
         STC   R1,0(R4)            TO OVERSCALE 1/2 AS/OF A DATE                
         B     YES                                                              
         SPACE 2                                                                
IOV12AS  MVC   0(LOPDATE,R4),0(R2)                                              
         GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BE    *+8                                                              
         B     ERRIOV                                                           
         L     R5,FPARMS                                                        
         CLI   MDUCODE,OPOVER                                                   
         BNE   ERRIOV                                                           
         L     R2,FPARMS+4         FOLLOWED BY OVERSCALE PERCENTAGE             
         MVC   LOPDATE(LOPOVER,R4),0(R2)                                        
         GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BE    *+8                                                              
         B     ERRIOV                                                           
         L     R5,FPARMS                                                        
         CLI   MDUCODE,OPOVER2                                                  
         BNE   ERRIOV                                                           
         L     R2,FPARMS+4         FOLLOWED BY OVERSCALE PERCENTAGE             
         MVC   LOPDATE+LOPOVER(LOPOVER,R4),0(R2)                                
         BCTR  R4,0                RESET SUB-ELEMENT CODE                       
         MVI   0(R4),OPOV12AS      TO OV1&2 AS/OF A DATE                        
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO WRITE ESTIMATE RECORD(S)                              
         SPACE 1                                                                
WRITEST  NTR1                                                                   
         GOTO1 AMYADDL,DMCB,(RC)   ADD LAST ELEMENT TO EST. RECORD              
         BL    LOW                                                              
         GOTO1 ASPLIT,DMCB,(RC)    AND WRITE ESTIMATE RECORD TO FILE            
         B     YES                                                              
         SPACE 2                                                                
*        ROUTINE TO DELETE ESTIMATE REQUESTED                                   
         SPACE 1                                                                
DELES    NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLESCDQ,0                                            
         BNE   ERRNTFND                                                         
*                                                                               
         MVI   RDUPDATE,C'Y'       DELETE THE FIRST MAIN RECORD                 
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING TLESD,R3                                                         
         OI    TLESSTAT,X'80'                                                   
         GOTO1 PUTREC                                                           
         OI    KEY+TLDRSTAT-TLDRKEY,X'80'                                       
         GOTO1 WRITE                                                            
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 ADELETE,DMCB,(RC)   DELETE ANY REMAINING RECORDS                 
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
*        ROUTINE TO CHECK NEW ESTIMATE OKAY                                     
         SPACE 1                                                                
NEWES    NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLESCDQ,0                                            
         BNE   YES                                                              
         B     ERREXIST                                                         
         SPACE 2                                                                
*              ROUTINE TO RESTORE ESTIMATE RECORD(S)                            
         SPACE 1                                                                
RESTES   NTR1                                                                   
         MVI   DMINBTS,X'08'       SET READ HIGH FOR DELETED                    
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 RECVAL,DMCB,TLESCDQ,0                                            
         B     REST10                                                           
*                                                                               
REST5    GOTO1 HIGH                RE-READ RECORD JUST RESTORED                 
         MVI   DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                 CHECK FOR ANOTHER RECORD                     
*                                                                               
REST10   CLC   KEY(TLESSEQ-TLESD),KEYSAVE                                       
         BNE   RESTX                                                            
*                                                                               
         MVI   DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING TLESD,R4                                                         
         NI    TLESSTAT,X'7F'                                                   
         GOTO1 PUTREC              UNMARK RECORD AND WRITE IT BACK              
         NI    KEY+TLDRSTAT-TLDRD,X'7F'                                         
         GOTO1 WRITE               UNMARK DIRECTORY AND WRITE IT BACK           
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL            IF FIND ACTIVITY ELEMENT                     
         BE    RESTX               THEN DONE WITH RESTORE                       
         B     REST5               ELSE, KEEP LOOKING FOR MORE RECS             
*                                                                               
RESTX    NI    DMINBTS,X'F7'                                                    
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              SEND DATA TO FALINK FOR DISPLAY                                  
SEND     NTR1                                                                   
         MVC   ASETELEM,0(R1)      SAVE A(SET ELEMENT RTN) IN FALINK            
         MVC   AADDDATA,4(R1)      SAVE A(ADD FIELD RTN) IN FALINK              
*                                                                               
         TM    STESTAT,STEIOBRK    IF RESUMING FROM I/O BREAK                   
         BO    SEND5               CONTINUE WHERE WE LEFT OFF                   
         XC    SVLSTINF,SVLSTINF   CLEAR SPECIFIC REC INFO FOR LISTING          
         XC    SVCODES,SVCODES     CLEAR COL/CAL/UHM OPTIMIZATION CODES         
         XC    AELEM,AELEM                                                      
*                                                                               
         CLI   MYKCODEQ,TLESCDQ    IF ESTIMATE RECORD                           
         BNE   *+12                                                             
         CLI   FLTACTN,0           AND D,R,V OR U                               
         BNE   SENDX               WE'RE DONE                                   
*                                                                               
         BAS   RE,MYHIGH           READ HIGH FOR KEY                            
         BL    LOW                 ERROR                                        
         BNE   SENDX               NOTHING FOUND GET OUT                        
         MVC   FRSTKEY,KEY         SAVE FIRST KEY                               
         XC    FRSTKEYA,FRSTKEYA   AND CLR FIRST KEY OF SECONDARY READ          
         B     SEND10                                                           
*                                                                               
SEND5    BAS   RE,MYSEQ            READ FOR NEXT RECORD                         
         BL    LOW                                                              
         BNE   SEND15                                                           
         TM    STESTAT,STEIOBRK    IF REQUESTED I/O BREAK                       
         BO    YES                 UNWIND NOW                                   
*                                                                               
SEND10   MVC   SMYKEY,KEY          SAVE CURRENT KEY                             
         BAS   RE,FILTKEY          FILTER SOME KEYS                             
         BNE   SEND5                                                            
         BAS   RE,GETDWNRC         GET RECORD FOR DOWNLOAD                      
         BNE   SEND5                                                            
         GOTO1 AFILTREC,DMCB,(RC)  FILTER RECORD & SET GLOBALS                  
         BNE   SEND5                                                            
         MVC   SVAIO,AIO1          SET SAVED IOAREA                             
         BAS   RE,PROCREC          DOWNLOAD RECORD INFORMATION                  
         B     SEND5               AND GET NEXT RECORD                          
*                                                                               
SEND15   CLC   =C'INI',MYKCODE     IF INITIALIZATION                            
         BNE   SEND17                                                           
         XR    R4,R4               INDICATE NO RECORD                           
         GOTO1 AADDTAB,DMCB,(RC)   ADD INFO TO MISCTAB                          
         BNE   ERRITAB                                                          
*                                                                               
SEND17   L     R4,AMISCTAB         COMB. MUS FOR CAL OR MISC. FOR INI           
         ST    R4,AIO                                                           
         OC    0(1,R4),0(R4)       IF DATA                                      
         BZ    SENDX                                                            
         OI    STESTAT,STETAB      SET IN MIDDLE OF TABLE                       
         MVC   SVAIO,AIO           SET SAVED IOAREA                             
         BAS   RE,PROCREC          PROCESS TABLE OF ELEMENTS                    
         NI    STESTAT,X'FF'-STETAB DONE WITH TABLE                             
*                                                                               
SENDX    TM    STESTAT,STEDATA     IF PASSED DATA TO FALINK                     
         BZ    YES                                                              
         GOTO1 AADDDATA,DMCB,FABLK,FALADNE,FALADNE  TELL FALINK NO MORE         
         B     XIT                                                              
**no-op**DC    H'0'                                 SHOULDN'T RETURN            
         EJECT                                                                  
*              ROUTINE READS HIGH FOR KEY                                       
*              IF KEY EQUATE USES RECVAL, ELSE BUILDS KEYS                      
*              COMML KEYS ON COL, USAGE HIST KEYS ON UHL & UHM,                 
*              POSSIBLE AGENCY KEYS ON INI                                      
         SPACE 1                                                                
MYHIGH   NTR1                                                                   
         L     R5,AKEYNTRY                                                      
         USING MHELD,R5            R5=A(HEADER ENTRY)                           
*                                                                               
         ZIC   R2,MYKCODEQ         IF KEY EQUATE SET                            
         LTR   R2,R2                                                            
         BZ    MYHIGH5                                                          
         GOTO1 RECVAL,DMCB,(R2),0  READ HIGH                                    
         B     MYHIGH8                                                          
*                                                                               
MYHIGH5  BAS   RE,BLDKEY           ELSE, BUILD KEY & READ HIGH MYSELF           
         SPACE 1                                                                
MYHIGH8  ZIC   RF,MYKLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE      IF KEY NOT FOUND                             
         BE    YES                                                              
         SPACE 1                                                                
         CLC   =C'CP ',MYKCODE     AND CLIENT/PRODUCT READ                      
         BE    ERRCLI              (GIVE SPECIFIC ERROR)                        
         TM    MHUSTAT,KEYSL       OR READING A RECORD                          
         BZ    ERRNTFND            RECORD MUST EXIST                            
         XC    KEY,KEY                                                          
         B     NO                                                               
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO BUILD KEY & READ HIGH MYSELF                          
         SPACE 1                                                                
BLDKEY   NTR1                                                                   
         LA    R4,KEY              R4=A(KEY TO BUILD)                           
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         CLC   =C'INI',MYKCODE     IF INITIALZATION                             
         BNE   BLDKEY1                                                          
*                                                                               
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'A0',0),0                                  
         BNE   XIT                                                              
*                                                                               
         USING TAVAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS AGENCY LIMITS                   
         BZ    XIT                                                              
         MVC   TGAGY,TAVAAGY       SET FIRST LIMITED AGENCY                     
         MVC   FRSTAGY,TAVAAGY                                                  
         XC    LASTAGY,LASTAGY                                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,0                                            
         B     XIT                                                              
*                                                                               
         USING TLUHD,R4                                                         
BLDKEY1  CLC   =C'UHM',MYKCODE     IF MULTIPLE USAGE HISTORY LIST               
         BNE   BLDKEY2                                                          
         OI    STESTAT,STENEWC     SET NEW COMMERCIAL NUMBER                    
         L     R5,AKEYUHM                                                       
         MVC   TGCOM,0(R5)                                                      
         LA    R5,4(R5)                                                         
         ST    R5,ANEXTUHM                                                      
         B     *+14                                                             
*                                                                               
BLDKEY2  CLC   =C'UHL',MYKCODE     IF USAGE HISTORY LIST                        
         BNE   BLDKEY4                                                          
         MVI   MYKCODEQ,TLUHCDQ                                                 
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         MVC   MYKLEN,LUHCOM                                                    
         B     BLDKEYX                                                          
*                                                                               
BLDKEY4  CLI   FLTKEYQ,C'C'        IF COMML LIST BY CODE                        
         BNE   BLDKEY5                                                          
*                                                                               
         CLC   FLTCLG,SPACES       AND CLIENT GROUP SPECIFIED                   
         BNH   *+12                                                             
         BRAS  RE,BLDKCOG          READ TLCOGCDQ                                
         B     *+8                                                              
         BRAS  RE,BLDKCO           ELSE READ TLCOCDQ                            
         B     BLDKEYX                                                          
*                                                                               
BLDKEY5  DS    0H                  ASSUME BY TITLE                              
         CLC   FLTCLG,SPACES       IF CLIENT GROUP SPECIFIED                    
         BNH   *+12                                                             
         BRAS  RE,BLDKCOL          READ TLCOLCDQ                                
         B     *+8                                                              
         BRAS  RE,BLDKCON          ELSE READ TLCONCDQ                           
*                                                                               
BLDKEYX  GOTO1 HIGH                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILTER AT DIRECTORY KEY LEVEL                         
         SPACE 1                                                                
FILTKEY  NTR1                                                                   
         CLI   KEY,TLCACDQ         IF CAST KEY                                  
         BNE   FILTK20                                                          
         TM    KEY+TLCASORT-TLCAD,X'20'                                         
         BO    NO                  SKIP EXTRAS                                  
         B     YES                                                              
*                                                                               
FILTK20  CLI   KEY,TLCOCDQ         IF READING COMMLS                            
         BNE   FILTK30                                                          
         TM    KEY+TLDRSTAT-TLDRD,X'40'                                         
         BO    NO                                                               
         CLC   FLTPRD,SPACES                                                    
         BNH   *+14                                                             
         CLC   FLTPRD,KEY+TLCOPRD-TLCOD                                         
         BNE   NO                                                               
         B     YES                 CC SET                                       
*                                                                               
FILTK30  CLI   KEY,TLCONCDQ        IF READING COMMLS                            
         BNE   YES                                                              
         TM    KEY+TLDRSTAT-TLDRD,X'40'                                         
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE READS RECORD INTO AIO                                    
         SPACE 1                                                                
GETDWNRC NTR1                                                                   
         OC    KEY,KEY             IF NO KEY                                    
         BNZ   GETDWNR2                                                         
         L     R4,AIO                                                           
         XC    0(10,R4),0(R4)      CLEAR BEGINNING                              
         B     NO                                                               
*                                                                               
GETDWNR2 CLI   KEY,TLESCDQ         IF ESTIMATE                                  
         BNE   GETDWNR4                                                         
         GOTO1 AGETEREC,DMCB,(RC)  GET WHOLE REC INTO AIO (FOR 1,2,&3)          
         B     GETDWNRX                                                         
GETDWNR4 GOTO1 GETREC              ELSE, JUST DO REGULAR GET INTO IO1           
*                                                                               
GETDWNRX B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO DO SEQUENTIAL READ IN LIST SITUATION                  
         USING MHELD,R5            R5=A(DATA ENTRY)                             
         SPACE 1                                                                
MYSEQ    NTR1                                                                   
         L     R5,AKEYNTRY                                                      
         USING MHELD,R5            R5=A(HEADER ENTRY)                           
*                                                                               
         TM    STESTAT,STEIOBRK                                                 
         BO    MYSEQNO2                                                         
*                                                                               
         TM    MHUSTAT,KEYSL       IF NOT LISTING RECORDS                       
         BO    *+12                                                             
         BAS   RE,NXTNSEQ          CHECK FOR SECOND NON-SEQUENTIAL READ         
         B     XIT                 CC SET                                       
*                                                                               
         TM    STESTAT,STERERD     IF NEED TO REREAD KEY                        
         BZ    MYSEQ5                                                           
         NI    STESTAT,X'FF'-STERERD SET TO REREAD DIRECTORY POINTER            
         XC    KEY,KEY             RE-ESTABLISH DIRECTORY KEY                   
         MVC   KEY,SMYKEY                                                       
         GOTO1 HIGH                                                             
*                                                                               
MYSEQ5   GOTO1 SEQ                                                              
         LA    RE,FRSTKEY                                                       
         OC    FRSTKEYA,FRSTKEYA   IF FIRST KEY OF SECOND READ SET              
         BZ    *+8                                                              
         LA    RE,FRSTKEYA         USE IT                                       
*                                                                               
         ZIC   RF,MYKLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),0(RE)        IF STILL GOOD                                
         BNE   MYSEQNO                                                          
*                                                                               
         CLI   KEY,TLESCDQ         IF READING ESTIMATE RECORDS                  
         BNE   *+12                                                             
         CLI   KEY+TLESSEQ-TLESD,0 SKIP NON-BASE RECORDS                        
         BNE   MYSEQ5                                                           
*                                                                               
         CLI   KEY,TLUHCDQ         AND USAGE HISTORY                            
         BNE   YES                                                              
         OC    KEY+TLUHCSEQ-TLUHD(L'TLUHCSEQ),KEY+TLUHCSEQ-TLUHD                
         BZ    YES                                                              
*                                                                               
MYSEQNO  CLC   =C'UHM',MHUKTXT     OR IT IS A UHM REQUEST                       
         BNE   NO                                                               
MYSEQNO2 BAS   RE,MYSEQUHM         PROCESS CA AND UH RECORDS                    
         B     XIT                 CC SET                                       
         EJECT                                                                  
*              ROUTINE SEES IF NEED TO READ DIFFERENT RECORD                    
         SPACE 1                                                                
NXTNSEQ  NTR1                                                                   
         CLC   =C'CP',MHUKTXT      OR IT IS A CP (CLI THEN PRD) REQUEST         
         BNE   NXTNSEQ4                                                         
         OC    TGPRD,TGPRD         AND PRODUCT REQUESTED                        
         BZ    NO                                                               
         GOTO1 RECVAL,DMCB,TLPRCDQ,0  READ HIGH FOR TLPRD KEY                   
         CLC   KEY(L'TLDRKEY),KEYSAVE                                           
         BNE   ERRPRD                                                           
         XC    TGPRD,TGPRD         CLEAR SO DON'T LOOP                          
         B     YES                                                              
*                                                                               
NXTNSEQ4 CLC   =C'COH',MHUKTXT     OR IT IS A COH REQUEST                       
         BNE   NXTNSEQ6                                                         
         MVC   AKEYNTRY,AKEYSUHL                                                
         MVI   MYKLEN,TLUHCSEQ-TLUHD-1                                          
         XC    KEY,KEY                                                          
         MVI   KEY,TLUHCDQ                                                      
         MVC   KEY+TLUHCOM-TLUHD(L'TGCOM),TGCOM                                 
         GOTO1 HIGH                READ HIGH FOR TLUHD KEY                      
         MVC   FRSTKEYA,KEY                                                     
         CLC   KEY(TLUHCSEQ-TLUHD),KEYSAVE                                      
         B     XIT                                                              
*                                                                               
NXTNSEQ6 CLC   =C'GUL',MHUKTXT      OR IT IS A GUL REQUEST                      
         BNE   NXTNSEQ8                                                         
         MVI   MYKLEN,TLGUGUA-TLGUD-1                                           
         MVC   AKEYNTRY,AKEYSGUL                                                
         GOTO1 RECVAL,DMCB,TLGUCDQ,0  READ HIGH FOR TLGUD KEY                   
         MVC   FRSTKEYA,KEY                                                     
         CLC   KEY(TLGUGUA-TLGUD),KEYSAVE                                       
         B     XIT                                                              
*                                                                               
NXTNSEQ8 CLC   =C'INI',MHUKTXT      OR IT IS AN INI REQUEST                     
         BNE   NO                                                               
*                                                                               
         XR    R2,R2                                                            
*                                                                               
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'C0',0),0                                  
         GOTO1 HIGH                                                             
         B     NXTNSEQA                                                         
NXTNSEQ9 GOTO1 SEQ                                                              
NXTNSEQA CLC   KEY(TLSSPR3-TLSTD),KEYSAVE                                       
         BNE   NO                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING TAVAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVAELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
NXTNSEQB BAS   RE,NEXTEL                                                        
         BNE   NXTNSEQ9                                                         
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS AGENCY LIMITS                   
         BZ    NO                                                               
         CLC   TAVAAGY,FRSTAGY                                                  
         BE    NXTNSEQB                                                         
         CLC   TAVAAGY,LASTAGY     LOOK FOR NEXT AGENCY                         
         BNH   NXTNSEQB                                                         
         MVC   TGAGY,TAVAAGY       AND GO READ AGENCY RECORD                    
         MVC   LASTAGY,TAVAAGY                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'84',0)                                    
         BNE   NXTNSEQB                                                         
         B     XIT                                                              
         DROP  R4,R5                                                            
         SPACE 2                                                                
*              ROUTINE TO PROCESS CA AND UH RECS ON UHM REQUEST                 
         SPACE 1                                                                
MYSEQUHM NTR1                                                                   
         TM    STESTAT,STEIOBRK    IF RESUMING FROM I/O BREAK                   
         BO    MYSQUHM8                                                         
         CLI   KEYSAVE,TLCACDQ     IF JUST FINISHED CAST RECORDS                
         BE    MYSQUHM8            PROCESS NEXT COMMERCIAL                      
*                                                                               
MYSQUHM7 CLI   KEYSAVE,TLUHCDQ     IF JUST FINISHED USAGE HISTORY REC'S         
         BNE   NO                                                               
         MVI   MYKCODEQ,TLCACDQ    READ CAST                                    
         MVC   MYKLEN,LCACOM                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,TLCACDQ                                                      
         MVC   KEY+TLCACOM-TLCAD(L'TGCOM),TGCOM                                 
         GOTO1 HIGH                                                             
         CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         BNE   MYSQUHM8                                                         
         MVC   FRSTKEYA,KEY                                                     
         B     YES                                                              
*                                                                               
MYSQUHM8 NI    STESTAT,X'FF'-STEIOBRK TURN OFF I/O BREAK FLAG                   
         L     R5,ANEXTUHM         LOOK FOR NEXT INTERNAL COMML NO.             
         OC    0(4,R5),0(R5)                                                    
         BZ    NO                                                               
         BAS   RE,CHKBREAK         IF I/O BREAK NEEDED                          
         BNE   YES                 NEED TO UNWIND THROUGH SEND                  
         MVC   TGCOM,0(R5)                                                      
         LA    R5,4(R5)                                                         
         ST    R5,ANEXTUHM                                                      
         OI    STESTAT,STENEWC     SET NEW COMMERCIAL NUMBER                    
         MVI   MYKCODEQ,TLUHCDQ                                                 
         MVC   MYKLEN,LUHCOM                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,TLUHCDQ                                                      
         MVC   KEY+TLUHCOM-TLUHD(L'TGCOM),TGCOM                                 
         GOTO1 HIGH                READ HIGH FOR TLUHD KEY                      
         CLC   KEY(TLUHCSEQ-TLUHD),KEYSAVE  IF NONE                             
         BNE   MYSQUHM7            TRY FOR CAST                                 
         MVC   FRSTKEYA,KEY        ELSE, SAVE FIRST KEY                         
         B     YES                 AND PROCESS                                  
         EJECT                                                                  
*              ROUTINE TO DOWNLOAD ONE RECORD - LOOPS THROUGH ELEMENTS,         
*              AND CALLS SETELEM/ADDDATA                                        
*                                  R4=A(RECORD)                                 
         SPACE 1                                                                
PROCREC  NTR1                                                                   
         TM    STESTAT,STENEWC     IF PROCESSING NEW COMML (UHM)                
         BZ    PROCRC1                                                          
         GOTO1 HEXOUT,DMCB,TGCOM,ELFDATA,4,0                                    
         GOTO1 ADOSETEL,DMCB,(RC)  CALL SETELEM TO ESTABLISH RECORD HDR         
         NI    STESTAT,X'FF'-STENEWC                                            
*                                                                               
PROCRC1  MVI   ELCODE,0            CLEAR ELEMENT CODE                           
         TM    STESTAT,STETAB      IF RECORD                                    
         BO    PROCRC2                                                          
         L     R4,AIO                                                           
         AH    R4,DATADISP         POINT TO FIRST ELEMENT                       
*                                                                               
         USING TAESD,R4                                                         
PROCRC2  CLI   0(R4),TAESELQ       IF ESTIMATE ELE. FOR ACTUAL COMML            
         BNE   PROCRC6                                                          
         CLI   TAESTYPE,TAESTCOM                                                
         BNE   PROCRC6                                                          
         GOTO1 APRESCID,DMCB,(RC)  PROCESS SEPERATELY                           
         BL    YES                 COMML NOT ON FILE AND NO MORE ELES           
         BNE   PROCRC2             COMML NOT ON FILE- R4=A(NEXT ELE.)           
         B     PROCRC9                                                          
*                                                                               
PROCRC6  BRAS  RE,PROTAO2          PROCESS OVERSCALE 2ND PCT ELEMENT            
         BRAS  RE,PROVERS          PROCESS VERSIONS INTO LIFT FORMAT            
*                                                                               
         GOTO1 AGETNTRY,DMCB,(RC),(0,(R4))                                      
         L     R5,AELENTRY         IF TABLE ENTRY FOR ELEMENT FOUND             
         LTR   R5,R5                                                            
         BZ    PROCRC9                                                          
         USING MHELD,R5            R5=A(ELEMENT ENTRY)                          
*                                                                               
         CLI   MHCODE+1,TAOPELQ    IF ELEMENT IS TAOAD OR TAOPD                 
         BE    *+12                                                             
         CLI   MHCODE+1,TAOAELQ                                                 
         BNE   *+12                                                             
         BAS   RE,PROCOAOP         NEED TO PROCESS BY SUB-ELEMENT               
         B     PROCRC9                                                          
*                                                                               
         GOTO1 ADOSETEL,DMCB,(RC)  CALL SETELEM TO ESTABLISH ELE HDR            
         GOTO1 APROCEL,DMCB,(RC)   PROCESS THE FIELDS IN THE ELEMENT            
*                                                                               
         USING TAGUD,R4                                                         
         CLI   MHCODE+1,TAGUELQ    IF GUARANTEE ELEMENT                         
         BNE   PROCRC8                                                          
         CLC   =C'GUL',MYKCODE     AND GUL DOWNLOAD                             
         BNE   PROCRC8                                                          
         OC    TAGUCOM,TAGUCOM     AND COMML DEFINED                            
         BZ    PROCRC8                                                          
         ST    R4,AELEM                                                         
         BAS   RE,RDTLCAG          READ CAST RECORD INTO AMAINIO                
         BNE   *+8                                                              
         BAS   RE,PROCREC          PROCESS IT                                   
         MVC   AIO,AIO1                                                         
         MVC   SVAIO,AIO1                                                       
         MVI   ELCODE,0                                                         
         L     R4,AELEM                                                         
         B     PROCRC9                                                          
*                                                                               
         USING TAESD,R4                                                         
PROCRC8  CLI   MHCODE+1,TAESELQ    IF ACTUAL PERF. ESTIMATE ELEMENT             
         BNE   PROCRC9                                                          
         CLI   TAESTYPE,TAESTPER                                                
         BNE   PROCRC9                                                          
         ST    R4,AELEM            SAVE A(TAESD) ELEMENT                        
         BAS   RE,RDTLCA           READ CAST RECORD INTO AMAINIO                
         BNE   *+8                                                              
         BAS   RE,PROCREC          PROCESS IT                                   
         MVC   AIO,AIO1            RESET IOAREA                                 
         MVC   SVAIO,AIO1          RESET SAVED IOAREA                           
         MVI   ELCODE,0            RESET ELEMENT CODE                           
         L     R4,AELEM            RESTORE A(TAESD) ELEMENT                     
*                                                                               
PROCRC9  BAS   RE,NEXTEL           IF MORE ELEMENTS                             
         BE    PROCRC2             PROCESS THEM                                 
*                                                                               
         L     R4,AIO                                                           
         CLI   0(R4),TLCACDQ       IF FINISHED PROCESSING CAST REC              
         BNE   YES                                                              
         CLC   =C'GUL',MYKCODE     AND NOT GUL DOWNLOAD                         
         BE    *+8                                                              
         BAS   RE,RDTLGU           PROCESS GU ELE IF GUAR. ON CAST              
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PROCESS TAOAD &TAOPD BY SUB-ELEMENT DATA              
         SPACE 2                                                                
         USING TAOAD,R4                                                         
PROCOAOP NTR1                                                                   
         MVI   WRKFLAG,C'N'        ELEMENT HDR NOT SET YET                      
         ZIC   R0,TAOANUM          R0=N'SUB-ELEMENTS                            
         LTR   R0,R0               ZERO?                                        
         BZ    XIT                 LEAVE                                        
         LA    R4,TAOALNQ(R4)      PT TO SUB-ELEMENT                            
*                                                                               
PROCOAP5 BAS   RE,FILTOAOP         FILTER SUB-ELEMENT                           
         BNE   PROCOAP7                                                         
*                                                                               
         CLI   WRKFLAG,C'N'        IF ELEMENT HDR NOT SET YET                   
         BNE   PROCOAP6                                                         
         GOTO1 ADOSETEL,DMCB,(RC)  CALL SETELEM TO ESTABLISH ELE HDR            
         MVI   WRKFLAG,C'Y'                                                     
PROCOAP6 GOTO1 APROCEL,DMCB,(RC)   PROCESS THE FIELDS IN THE ELEMENT            
*                                                                               
PROCOAP7 LA    R4,L'TAOASBEL(R4)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R0,PROCOAP5                                                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE FILTERS TAOAD/TAOPD ELEMENTS                             
         SPACE 1                                                                
         USING TAOASBEL,R4                                                      
FILTOAOP NTR1                                                                   
         TM    TAOAAMT,X'80'       IF AMT (TAOAD) OR % (TAOPD) NEG              
         BO    NO                  DON'T USE                                    
         CLC   =C'HLD',TAOAUSE     PROCESS HLDS                                 
         BE    YES                                                              
         CLC   =C'SHL',TAOAUSE                                                  
         BE    YES                                                              
         CLC   =C'ADH',TAOAUSE                                                  
         BE    YES                                                              
         CLI   MHCODE+1,TAOAELQ    IF AMOUNT OVERRIDE ELEMENT OR                
         BE    FILTOAP4                                                         
         L     RE,AELEM                                                         
         CLI   0(RE),TAGUELQ       IF COMING FROM GUARANTEE ELEMENT             
         BNE   FILTOAP6                                                         
FILTOAP4 CLC   =C'BSS',TAOAUSE     ALSO PROCESS BSS                             
         BE    YES                                                              
         B     NO                                                               
*                                                                               
FILTOAP6 CLC   TAOAUSE,SPACES      ELSE, ALSO PROCESS ALL                       
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO READ CAST RECORD THROUGH TLCACDQ POINTER              
         SPACE 1                                                                
RDTLCA   NTR1                                                                   
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,AMAINIO                                                      
         MVC   SVAIO,AIO           SET NEW SAVED AIO                            
         MVC   TGNAME,=CL36'** NOT FOUND **'                                    
         MVI   TGTYPE,TAW4TYCO                                                  
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',0)                                    
         BNE   RDTLCA5                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   RDTLCA5                                                          
         USING TAW4D,R4                                                         
         MVC   TGNAME(L'TAW4CRPN),TAW4NAM2                                      
         MVC   TGTYPE,TAW4TYPE                                                  
*                                                                               
RDTLCA5  XC    TGCAT,TGCAT                                                      
         GOTO1 RECVAL,DMCB,TLCACCDQ,0                                           
         B     RDTLCA8                                                          
RDTLCA6  GOTO1 SEQ                                                              
*                                                                               
RDTLCA8  CLC   KEY(TLCACCAT-TLCAPD),KEYSAVE                                     
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         USING TLCAPD,R4                                                        
         CLC   TLCACSEQ,TGCSORT+4                                               
         BNE   RDTLCA6                                                          
         MVC   TGCAT,TLCACCAT                                                   
         GOTO1 GETREC                                                           
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO READ CAST RECORD THROUGH TLCAGDQ POINTER              
         SPACE 1                                                                
         USING TAGUD,R4                                                         
RDTLCAG  NTR1                                                                   
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,AMAINIO                                                      
         MVC   SVAIO,AIO           SET NEW SAVED AIO                            
         MVC   TGCOM,TAGUCOM                                                    
         GOTO1 RECVAL,DMCB,TLCAGCDQ,0  READ HIGH FOR TLCAGD KEY                 
         CLC   KEY(TLCACCAT-TLCAPD),KEYSAVE                                     
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         B     YES                                                              
         SPACE 2                                                                
*              ROUTINE TO READ GUARANTEE RECORD OF CAST                         
         SPACE 1                                                                
RDTLGU   NTR1                                                                   
         CLC   TGGUA,=4X'FF'       IF CAST HAD A GUARANTEE CODE                 
         BE    XIT                                                              
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,AMAINIO                                                      
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'A0',0)                                    
         BNE   XIT                                                              
         L     R4,AIO                                                           
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         GOTO1 AGETNTRY,DMCB,(RC),(0,(R4))                                      
         L     R5,AELENTRY                                                      
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         GOTO1 ADOSETEL,DMCB,(RC)  CALL SETELM TO ESTABLISH ELE HDR             
         GOTO1 APROCEL,DMCB,(RC)   PROCESS EL=GU ELEMENT                        
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              CHECK BREAK NEEDED, IF YES CALLS FALINK                          
         SPACE 1                                                                
CHKBREAK NTR1                                                                   
         OI    GENSTAT1,CATCHIOR   RETURN TO USER                               
         GOTO1 CATCHIOS                                                         
         CLI   ERROR,NOTONLIN      IF NO MORE I/O'S                             
         BNE   YES                                                              
         OI    STESTAT,STEIOBRK    SET I'M BREAKING                             
         GOTO1 AADDDATA,DMCB,FABLK,FALABRK,FALABRK TELL FALINK TO BREAK         
         B     NO                  NEED TO UNWIND THROUGH SEND                  
         SPACE 2                                                                
*              ROUTINE CALLED BY FALINK TO SAY BREAK                            
*                                  AFTER THIS, SHOULD GET CONTROL               
*                                  IN RESUME HOOK                               
         SPACE 1                                                                
BREAK    NTR1                                                                   
         TM    STESTAT,STEIOBRK    IF I REQUESTED BREAK                         
         BZ    NO                                                               
         CLC   MYKCODE,=C'UHM'     MUST BE UHM REQUEST                          
         BNE   NO                                                               
*                                  SAVE OFF DETAILS                             
         LA    R2,X'82'            WRITE TWA RECORD # 2                         
         GOTO1 GETTWA,DMCB,((R2),AKEYUHM)                                       
*                                                                               
         GOTO1 AADDDATA,DMCB,FABLK,FALADNE,FALADNE LET FALINK FINISH            
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE CALLED BY FALINK TO SAY RESUME                           
*                                  AFTER THIS, SHOULD GET CONTROL FROM          
*                                  TOP OF SEND                                  
         SPACE 1                                                                
RESUME   NTR1                                                                   
         LA    R2,X'22'            READ TWA RECORD # 2                          
         LA    R0,1200             LENGTH TO RESTORE                            
         GOTO1 GETTWA,DMCB,((R2),AKEYUHM),(R0)                                  
         B     XIT                                                              
         EJECT                                                                  
*              EXITS, CONSTANTS, ETC.                                           
         SPACE 2                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 2                                                                
HEXFFS   DC    6X'FF'                                                           
TWAPGS   DC    X'0102030400'                                                    
LCOCLI   DC    AL1(TLCOCLI+L'TLCOCLI-TLCOD-1)                                   
LCOPRD   DC    AL1(TLCOPRD+L'TLCOPRD-TLCOD-1)                                   
LCONCLI  DC    AL1(TLCONCLI+L'TLCONCLI-TLCOPD-1)                                
LCOGCLG  DC    AL1(TLCOGCLG+L'TLCOGCLG-TLCOPD-1)                                
LCOGAGY  DC    AL1(TLCOGAGY+L'TLCOGAGY-TLCOPD-1)                                
LCOLCLG  DC    AL1(TLCOLCLG+L'TLCOLCLG-TLCOPD-1)                                
LUHCOM   DC    AL1(TLUHCOM+L'TLUHCOM-TLUHD-1)                                   
LCACOM   DC    AL1(TLCACOM+L'TLCACOM-TLCAD-1)                                   
         SPACE 2                                                                
LOW      LNR   RC,RC               RETURN CC NOW                                
         B     NO                                                               
YES      XR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT      XIT1                      RETURN                                       
         EJECT                                                                  
ERRECINV MVI   ERROR,INVREC           RECORD INVALID                            
         LA    R2,CONRECH                                                       
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
ERRNTFND MVC   MYMSGNO,=AL2(NOTFOUND) RECORD NOT FOUND                          
         B     ERRXIT                                                           
ERREXIST MVC   MYMSGNO,=AL2(RECEXIST) RECORD ALREADY EXISTS                     
         B     ERRXIT                                                           
ERRINV   MVC   MYMSGNO,=AL2(INVALID)  INVALID INPUT FIELD                       
         B     ERRXIT                                                           
ERRCLI   MVC   MYMSGNO,=Y(ERCLINFD)   CLIENT NOT FOUND                          
         B     ERRXIT                                                           
ERRPRD   MVC   MYMSGNO,=Y(ERPRDNFD)   PRODUCT NOT FOUND                         
         B     ERRXIT                                                           
ERRAGY   MVC   MYMSGNO,=Y(ERAGYNFD)   AGENCY NOT FOUND                          
         B     ERRXIT                                                           
ERRSSN   MVC   MYMSGNO,=Y(ERPERNFD)   PERFORMER NOT FOUND                       
         B     ERRXIT                                                           
ERRCLG   MVC   MYMSGNO,=Y(ERCLGNFD)   CLIENT GROUP NOT FOUND                    
         B     ERRXIT                                                           
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         B     ERRXIT                                                           
ERRABND  MVC   MYMSGNO,=X'11F'        #287, ABEND MESSAGE NOT H'0'              
         B     ERRXIT                                                           
*                                                                               
ERRILEN  MVI   BYTE,1                                                           
         B     *+8                                                              
ERRIEST  MVI   BYTE,2                                                           
         B     *+8                                                              
ERRITAB  MVI   BYTE,3                                                           
         B     *+8                                                              
ERRIKEY  MVI   BYTE,4                                                           
         BAS   RE,GETERR                                                        
         MVC   MYMSGNO,=AL2(ERINVSUB)                                           
         B     ERRELXIT                                                         
*                                                                               
ERRIMED  MVI   BYTE,5                                                           
         B     *+8                                                              
ERRIUNI  MVI   BYTE,6                                                           
         B     *+8                                                              
ERRICAT  MVI   BYTE,7                                                           
         B     *+8                                                              
ERRIYR   MVI   BYTE,8                                                           
         B     *+8                                                              
ERRIACTY MVI   BYTE,9                                                           
         B     *+8                                                              
ERRIUSE  MVI   BYTE,10                                                          
         B     *+8                                                              
ERRIOV   MVI   BYTE,11                                                          
         B     *+8                                                              
ERRIHLD  MVI   BYTE,12                                                          
         B     *+8                                                              
ERRICYC  MVI   BYTE,13                                                          
         B     *+8                                                              
ERRIUSRG MVI   BYTE,14                                                          
         MVC   MYMSGNO,=AL2(ERINVSUB) INVALID                                   
         XC    BLOCK,BLOCK                                                      
         BAS   RE,GETERR           R3=A(NXT POSITION IN BLOCK)                  
         B     ERRELEM5                                                         
*                                                                               
ERRELEM  MVC   MYMSGNO,=AL2(ERELEBIG) ELEMENT TOO BIG                           
         LA    R4,ELEMENT             IF ESTIMATE ELEMENT                       
         CLI   0(R4),TAESELQ                                                    
         BNE   ERRELXIT                                                         
         MVC   MYMSGNO,=AL2(ERTOOMNY) TOO MANY SUB-OPTIONS                      
         XC    BLOCK,BLOCK                                                      
         LA    R3,BLOCK                                                         
ERRELEM5 BAS   RE,ESXTRA              ADD EXTRA TEXT FOR TAESD ELEMENT          
*                                                                               
ERRELXIT LA    R2,FAMSGBLK                                                      
         USING FAMSGD,R2                                                        
         GOTO1 SQUASHER,DMCB,BLOCK,50                                           
         MVC   FAMSGXTR,BLOCK                                                   
*                                                                               
ERRXIT   LA    R2,FAMSGBLK                                                      
         USING FAMSGD,R2                                                        
         MVC   FAMSGNO,MYMSGNO                                                  
         MVI   FAMSGSYS,70         SET TALENT SYSTEM                            
         CLC   FAMSGNO,=AL2(60)    IF NOT HIGHER THAN 60                        
         BH    *+8                                                              
         MVI   FAMSGSYS,X'FF'      SET OVERRIDE TO GENERAL SYSTEM               
         B     LOW                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE SETS ADDITIONAL TEXT INTO BLOCK                          
*                                  BYTE - ERROR #                               
*                                  XIT - R3=A(NEXT POSITION IN BLOCK)           
         SPACE 1                                                                
GETERR   NTR1                                                                   
         LARL  RE,ERRTAB                                                        
GETERR5  CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   1(1,RE),BYTE                                                     
         BE    GETERR8                                                          
         ZIC   R1,0(RE)                                                         
         AR    RE,R1                                                            
         B     GETERR5                                                          
*                                                                               
GETERR8  XC    BLOCK,BLOCK                                                      
         ZIC   R1,0(RE)                                                         
         AHI   R1,-3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK(0),2(RE)                                                   
         LA    R1,1(R1)                                                         
         LA    R3,BLOCK                                                         
         AR    R3,R1                                                            
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*              EXTRA TEXT FOR TAESD ELEMENT                                     
*                                  R3=A(BLOCK)                                  
         USING TAESD,R4            R4=A(ESTIMATE ELEMENT)                       
ESXTRA   NTR1                                                                   
         MVC   0(2,R3),=C': '                                                   
         LA    R3,2(R3)                                                         
         CLI   TAESTYPE,TAESTCLI                                                
         BNE   *+14                                                             
         MVC   0(4,R3),=C'Cli='                                                 
         B     *+18                                                             
         CLI   TAESTYPE,TAESTPRD                                                
         BNE   ESXTRA1                                                          
         MVC   0(4,R3),=C'Prd='                                                 
         MVC   4(L'TAESDATA,R3),TAESDATA                                        
         B     ESXTRAX                                                          
         SPACE 1                                                                
ESXTRA1  MVC   0(6,R3),=C'Comml='                                               
         MVC   6(L'TGCID,R3),TGCID                                              
         LA    R3,6+L'TGCID(R3)                                                 
         SPACE 1                                                                
         CLI   TAESTYPE,TAESTPER                                                
         BNE   ESXTRA6                                                          
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         MVC   0(5,R3),=C'Cast='                                                
         LA    R3,5(R3)                                                         
         EDIT  TAESSSN,(9,0(R3)),ALIGN=LEFT,FILL=0                              
         B     ESXTRAX                                                          
         SPACE 1                                                                
ESXTRA6  CLI   TAESTYPE,TAESTHPE                                                
         BNE   ESXTRAX                                                          
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         MVC   0(5,R3),=C'Cast='                                                
         LA    R3,5(R3)                                                         
         CLI   TAESHNPR,0                                                       
         BNE   *+8                                                              
         MVI   TAESHNPR,1                                                       
         EDIT  TAESHNPR,(3,0(R3)),ALIGN=LEFT                                    
         AR    R3,R0                                                            
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'TGCAT,R3),TGCAT                                              
         LA    R3,L'TGCAT(R3)                                                   
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVC   0(3,R3),=C'OFF'                                                  
         TM    TAESHSTA,X'80'                                                   
         BZ    *+10                                                             
         MVC   0(3,R3),=C'ON '                                                  
         LA    R3,3(R3)                                                         
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'TGUNI,R3),TGUNI                                              
         TM    TAESHSTA,X'80'                                                   
         BZ    ESXTRAX                                                          
         MVI   3(R3),C' '                                                       
         LA    R3,4(R3)                                                         
         MVC   0(4,R3),=C'CORP'                                                 
ESXTRAX  B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
                                                                                
FALKTOKN DC    C'FALK'                                                          
         EJECT                                                                  
*              TABLE OF ROUTINES TO RELOCATE                                    
*                                                                               
RTNTAB   DS    0A                                                               
         DC    AL4(GETEREC)                                                     
         DC    AL4(FILTREC)                                                     
         DC    AL4(PRESCID)                                                     
         DC    AL4(DOSETEL)                                                     
         DC    AL4(PROCEL)                                                      
         DC    AL4(GETNTRY)                                                     
         DC    AL4(ADDTAB)                                                      
         DC    AL4(MYADDL)                                                      
         DC    AL4(SPLIT)                                                       
         DC    AL4(DELETE)                                                      
         DC    AL4(ELKYUHL)                                                     
         DC    AL4(ELKYGUL)                                                     
         DC    AL4(MAPTAB)                                                      
         DC    AL4(ELTAESO)                                                     
NRTNS    EQU   (*-RTNTAB)/L'RTNTAB                                              
         SPACE 2                                                                
*        VALIDATE ESTIMATE ACTIONS AND ROUTINES                                 
         SPACE 1                                                                
         DS    0H                                                               
FLTTAB   DS    0CL5                                                             
         DC    C'N',AL4(NEWES-T702E2)                                           
         DC    C'D',AL4(DELES-T702E2)                                           
         DC    C'R',AL4(RESTES-T702E2)                                          
         DC    C'U',AL4(WRITEST-T702E2)                                         
         DC    X'FF'                                                            
         SPACE 2                                                                
*              ERROR TABLE  - LENGTH OF NTRY, ERROR CODE, TEXT                  
         SPACE 1                                                                
         DS    0H                                                               
ERRTAB   DS    0C                                                               
         DC    AL1(15),AL1(1),C'Falink length'                                  
         DC    AL1(15),AL1(2),C'estimate code'                                  
         DC    AL1(16),AL1(3),C'initialization'                                 
         DC    AL1(13),AL1(4),C'key request'                                    
         DC    AL1(7),AL1(5),C'media'                                           
         DC    AL1(7),AL1(6),C'union'                                           
         DC    AL1(10),AL1(7),C'category'                                       
         DC    AL1(6),AL1(8),C'year'                                            
         DC    AL1(18),AL1(9),C'comml actra type'                               
         DC    AL1(5),AL1(10),C'use'                                            
         DC    AL1(11),AL1(11),C'overscale'                                     
         DC    AL1(6),AL1(12),C'hold'                                           
         DC    AL1(7),AL1(13),C'cycle'                                          
         DC    AL1(9),AL1(14),C'use range'                                      
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE READS WHOLE ESTIMATE RECORD IN IO 1,2 & 3                
         SPACE 1                                                                
         DS    0D                                                               
GETEREC  NMOD1 0,*GETEREC                                                       
         L     RC,0(R1)                                                         
*                                                                               
         GOTO1 GETREC              GET RECORD                                   
         MVC   AIO,AWRKIO                                                       
GREC4    GOTO1 SEQ                 LOOK FOR ANOTHER RECORD                      
         SPACE 1                                                                
         LA    R3,KEY                                                           
         USING TLESD,R3                                                         
         CLC   TLESKEY(TLESSEQ-TLESD),SMYKEY   WITH ALL SAME UP TO SEQ.         
         BNE   GRECX                                                            
         GOTO1 GETREC              GET NEW RECORD INTO TEMPORARY IO             
         SPACE 1                                                                
         L     RE,AIO              NOW COMBINE THEM - RE=A(NEW RECORD)          
         L     R3,AIO1                                R3=A(MAIN RECORD)         
         LH    RF,TLESLEN-TLESD(RE)  L'NEW RECORD                               
         SH    RF,DATADISP           LESS DATADISP                              
         SPACE 1                                                                
         LH    R1,TLESLEN                                                       
         BCTR  R1,0                                                             
         LR    R0,R1               R0=R1=L'MAIN RECORD (-1 FOR EOR)             
         SPACE 1                                                                
         AR    R1,RF               PLUS L'NEW RECORD                            
         STH   R1,TLESLEN          IS L'COMBINED RECORD                         
         SPACE 1                                                                
         AR    R0,R3               R0=A(END OF MAIN RECORD)                     
         AH    RE,DATADISP         RE=A(1ST EL. IN NEW RECORD)                  
         LR    R1,RF               R1=RF=L'NEW RECORD ELEMENTS                  
         SPACE 1                                                                
         MVCL  R0,RE               MOVE NEW RECORD AFTER MAIN                   
         B     GREC4               LOOK FOR ANOTHER                             
         SPACE 1                                                                
GRECX    MVC   AIO,AIO1            RESET IO AREA                                
         OI    STESTAT,STERERD     SET REREAD STATUS                            
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FILTER AT RECORD LEVEL                                
*              IN ESL DOWNLOAD - FUDGING IOAREA TO CONTAIN 05 EL ONLY           
         SPACE 1                                                                
         DS    0D                                                               
FILTREC  NMOD1 0,*FILTREC                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R4,AIO              R4=A(RECORD TO PROCESS)                      
         BAS   RE,SETGBL           SET GLOBAL FIELDS                            
*                                                                               
         CLI   0(R4),TLVRCDQ       NEVER PROCESS VERSION RECORDS                
         BE    FILTRNO                                                          
*                                                                               
         CLI   0(R4),TLGUCDQ       IF GUARANTEE RECORD                          
         BNE   FILTR5                                                           
         BAS   RE,FLTTLGU          FILTER AGENCY AND/OR CLIENT                  
         B     FILTRX                                                           
*                                                                               
FILTR5   CLI   0(R4),TLCACDQ       IF CAST RECORD                               
         BNE   FILTR10                                                          
         CLC   =C'UHM',MYKCODE     IF UHM DOWNLOAD                              
         BNE   FILTR8                                                           
         TM    TGCSORT,X'80'       SKIP MUSICIANS                               
         BO    FILTRNO                                                          
         TM    TGCATYPE,NOHLD      SKIP CATS THAT DON'T GET HLDS                
         BO    FILTRNO                                                          
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL6                                                        
         BNE   FILTRNO                                                          
         USING TACAD,R4                                                         
         CLC   TACAONOF,=C'OFF'    OR HLDS IF OFF CAMERA                        
         BNE   FILTRYES                                                         
         TM    TGCATYPE,NOHLDOFF                                                
         BO    FILTRNO                                                          
         CLI   TGYREQU,CN88        OR HLDS IF OFF CAM AS/OF'88 CONTRACT         
         BL    FILTRYES                                                         
         TM    TGCATYPE,NHLDOF88                                                
         BO    FILTRNO                                                          
         B     FILTRYES                                                         
FILTR8   TM    TGCSORT,X'80'       AND IF CATEGORY IS MUSICIAN                  
         BZ    FILTRYES                                                         
         CLI   FLTMIND,C'M'        IF ASKING FOR MUSICIANS INDIVIDUALLY         
         BE    FILTRYES            THEN PROCESS THE MUSICIAN                    
         GOTO1 AADDTAB,DMCB,(RC)   ELSE, ADD TO MISCTAB                         
         BNE   ERRITAB                                                          
         B     FILTRNO             AND SKIP                                     
*                                                                               
FILTR10  CLI   0(R4),TLUHCDQ       IF USAGE HISTORY RECORD                      
         BNE   FILTR20                                                          
         BAS   RE,FLTTLUH          FILTER USE BY CYCLE DATES                    
         B     FILTRX                                                           
*                                                                               
FILTR20  CLI   0(R4),TLESCDQ       IF ESTIMATE RECORD                           
         BNE   FILTR21                                                          
         CLC   =C'ESL',MYKCODE     AND DOING LIST                               
         BNE   FILTR21                                                          
         BAS   RE,FILTESL          FILTER ESTIMATE RECORD ON LIST               
         BNE   FILTRNO                                                          
         BAS   RE,FUDGREC          FUDGE RECORD TO CONTAIN 05 EL ONLY           
         B     FILTRYES                                                         
*                                                                               
FILTR21  CLI   0(R4),TLCOCDQ       IF COMMERCIAL RECORD                         
         BNE   FILTRYES                                                         
         CLC   =C'COL',MYKCODE     AND DOING LIST                               
         BNE   FILTRYES                                                         
         BAS   RE,FILTCOL          FILTER COMMERCIAL RECORD ON LIST             
         B     FILTRX                                                           
         SPACE 1                                                                
FILTRYES XR    RC,RC               RETURN CC EQUAL                              
FILTRNO  LTR   RC,RC               RETURN CC NOT EQUAL                          
FILTRX   XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO SET GLOBAL FIELDS BASED ON RECORD                     
*                                  R4=A(RECORD)                                 
         SPACE 1                                                                
SETGBL   NTR1                                                                   
         CLI   0(R4),TLGUCDQ       IF GUARANTEE RECORD                          
         BNE   SETGBL2                                                          
         USING TLGUD,R4                                                         
         MVC   TGSSN,TLGUSSN       SET SOCIAL SECURITY NUMBER                   
         MVC   TGGUA,TLGUGUA       SET GUARANTEE NUMBER                         
         B     FILTRX                                                           
*                                                                               
SETGBL2  CLI   0(R4),TLCOCDQ       IF COMMERCIAL RECORD                         
         BNE   SETGBL5                                                          
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   TGAGY,TLCOAGY       NOT SET IF TLCONCDQ                          
         MVC   TGCOM,TLCOCOM                                                    
         B     FILTRX                                                           
*                                                                               
SETGBL5  CLI   0(R4),TLW4CDQ       IF W4 RECORD                                 
         BE    SETGBL8                                                          
         CLI   0(R4),TLCACDQ       IF CAST RECORD                               
         BNE   SETGBL10                                                         
         USING TLCAD,R4                                                         
         MVC   TGSSN,TLCASSN       SET SOCIAL SECURITY NUMBER                   
         MVC   TGCSORT,TLCASORT    SET CAST SORT                                
         GOTO1 CATVAL,DMCB,TLCACAT SET CATEGORY CODE                            
         MVC   TGNAME,=CL36'** NOT FOUND **'                                    
         MVI   TGTYPE,TAW4TYCO                                                  
*                                                                               
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL6                                                        
         BNE   FILTRX                                                           
         USING TACAD,R4                                                         
         GOTO1 YRVAL,DMCB,TACAYEAR SET YEAR EQUATE                              
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,AWRKIO                                                       
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',0)                                    
         BNE   FILTRX                                                           
         MVC   AIO,AIO1                                                         
         L     R4,AWRKIO                                                        
*                                                                               
SETGBL8  BAS   RE,SETGNM           SET NAME AND TYPE                            
         B     FILTRX                                                           
*                                                                               
SETGBL10 CLI   0(R4),TLUHCDQ       IF USAGE HISTORY                             
         BNE   FILTRX                                                           
         USING TLUHD,R4                                                         
         GOTO1 USEVAL,DMCB,TLUHUSE,0  SET USE CODE                              
         B     FILTRX                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET TYPE AND NAME FROM W4 RECORD                      
         SPACE 1                                                                
SETGNM   NTR1                                                                   
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL6                                                        
         BNE   FILTRX                                                           
         USING TAW4D,R4                                                         
         MVC   TGTYPE,TAW4TYPE                                                  
         MVC   TGNAME(L'TAW4CRPN),TAW4NAM2     SAVE FULL NAME                   
         B     FILTRX                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE FILTERS RECORD BY CHECKING TAGUD ELEMENT                 
*              BASED ON REQUESTED KEY FIELDS                                    
         SPACE 1                                                                
         USING TLGUD,R4            R4=A(GUARANTEE RECORD)                       
FLTTLGU  NTR1                                                                   
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL6                                                        
         BNE   FILTRNO                                                          
*                                                                               
         USING TAGUD,R4                                                         
         CLC   FLTAGY,SPACES                                                    
         BNH   FTG10                                                            
         CLC   FLTAGY,TAGUAGY                                                   
         BNE   FTG20                                                            
*                                                                               
FTG10    CLC   FLTCLI,SPACES                                                    
         BNH   FILTRYES                                                         
         CLC   FLTCLI,TAGUCLI                                                   
         BE    FILTRYES                                                         
         DROP  R4                                                               
*                                                                               
         USING TAVAD,R4                                                         
FTG20    L     R4,AIO                                                           
         MVI   ELCODE,TAVAELQ      GET FIRST VALID AGENCY/CLIENT                
         BAS   RE,GETEL6           ELEMENT                                      
         BNE   FILTRNO             IF NOT FOUND, ACCESS IS GRANTED              
*                                                                               
FTG30    CLC   FLTAGY,SPACES       IF AGENCY IS FOUND IN GRT LIMITS             
         BNH   FTG40                                                            
         CLC   FLTAGY,TAVAAGY      AND CLIENT LIMITS ARE NOT DEFINED            
         BNE   FTG60               ACCESS IS GRANTED                            
*                                                                               
FTG40    CLC   FLTCLI,SPACES                                                    
         BNH   FILTRYES                                                         
         CLI   TAVALEN,TAVALNQ                                                  
         BE    FTG60                                                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
FTG50    CLC   FLTCLI,0(RF)        IF CLIENT IS FOUND IN GRT LIMITS             
         BE    FILTRYES            ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   FTG50                                                            
*                                                                               
FTG60    BAS   RE,NEXTEL6          GET NEXT VALID AGENCY/CLIENT                 
         BE    FTG30               ELEMENT                                      
         B     FILTRNO             IF NOT FOUND, RETURN ERROR                   
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE FILTERS USAGE HISTORY RECORDS - PASS ONLY LATEST         
*              CYCLE FOR EACH USE                                               
         SPACE 1                                                                
         USING TLUHD,R4            R4=A(USAGE RECORD)                           
FLTTLUH  NTR1                                                                   
         OC    SVUHUSE,SVUHUSE     IF SAVED USE                                 
         BZ    *+14                                                             
         CLC   SVUHUSE,TLUHUSE     IF STILL ON SAME USE                         
         BE    FLTTLUH5            CONTINUE                                     
         CLC   =C'GRT',TLUHUSE     IF NOT GRT PAYMENT                           
         BE    FILTRNO                                                          
         MVC   SVUHUSE,TLUHUSE     SAVE NEW USE                                 
         XC    SVUHCYC,SVUHCYC                                                  
*                                                                               
FLTTLUH5 MVI   ELCODE,TAUHELQ                                                   
         BAS   RE,GETEL6                                                        
         BNE   FILTRNO                                                          
         USING TAUHD,R4                                                         
         OC    TAUHSTRT(6),TAUHSTRT IF NO CYCLE DATES                           
         BZ    FILTRNO                                                          
         OC    SVUHCYC,SVUHCYC     OR SAME CYCLE DATES                          
         BZ    *+14                                                             
         CLC   SVUHCYC,TAUHSTRT                                                 
         BE    FILTRNO             DON'T BOTHER READING IT                      
         MVC   SVUHCYC,TAUHSTRT    ELSE, SAVE CYCLE                             
         B     FILTRYES            AND SET TO READ                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE FILTERS ESTIMATES ON LIST                                
         SPACE 1                                                                
         USING TLESD,R4                                                         
FILTESL  NTR1                                                                   
         CLC   FLTSTRT,SPACES      IF START FILTER                              
         BNH   FLTESL2                                                          
         CLC   TLESEST,FLTSTRT                                                  
         BL    FILTRNO                                                          
*                                                                               
FLTESL2  CLC   FLTCLI,SPACES       IF CLIENT FILTER                             
         BNH   FLTESL4                                                          
         MVI   BYTE,TAESTCLI                                                    
         MVC   WORK(L'FLTCLI),FLTCLI                                            
         BAS   RE,CKMATCH                                                       
         BNE   FILTRNO                                                          
*                                                                               
FLTESL4  CLC   FLTPRD,SPACES       IF PRODUCT FILTER                            
         BNH   FLTESL6                                                          
         MVI   BYTE,TAESTPRD                                                    
         MVC   WORK(L'FLTPRD),FLTPRD                                            
         BAS   RE,CKMATCH                                                       
         BNE   FILTRNO                                                          
*                                                                               
FLTESL6  OC    FLTRDTE,FLTRDTE    IF REPORT DATE FILTER                         
         BZ    FLTESL8                                                          
         MVI   BYTE,SCR40                                                       
         MVC   WORK(L'FLTRDTE),FLTRDTE                                          
         MVC   WORK+L'FLTRDTE(L'FLTRDFLG),FLTRDFLG                              
         BAS   RE,CKDATE                                                        
         BNE   FILTRNO                                                          
*                                                                               
FLTESL8  OC    FLTCDTE,FLTCDTE    IF CHANGED DATE FILTER                        
         BZ    FILTRYES                                                         
         MVI   BYTE,0                                                           
         MVC   WORK(L'FLTCDTE),FLTCDTE                                          
         MVC   WORK+L'FLTCDTE(L'FLTCDFLG),FLTCDFLG                              
         BAS   RE,CKDATE                                                        
         B     FILTRX                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK MATCH ON CLIENT OR PRODUCT                      
*                                  NTRY - BYTE = TAESTCLI OR TAESTPRD           
*                                         WORK = CLIENT OR PRODUCT CODE         
*                                  XIT -  CC SET                                
CKMATCH  NTR1                                                                   
         L     R4,AIO              R4=A(FULL ESTIMATE RECORD)                   
         MVI   ELCODE,TAESELQ                                                   
         BAS   RE,GETEL6                                                        
         B     *+8                                                              
CKMATCH5 BAS   RE,NEXTEL6                                                       
         BNE   FILTRX                                                           
         SPACE 1                                                                
         USING TAESD,R4                                                         
         CLC   TAESTYPE,BYTE             MATCH ON ELEMENT TYPE                  
         BNE   CKMATCH5                                                         
         CLC   TAESDATA(L'FLTCLI),WORK   MATCH ON ELEMENT DATA                  
         BNE   CKMATCH5                                                         
         B     FILTRYES                                                         
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO CHECK MATCH ON REPORT OR CHANGED DATE                 
*                                  NTRY - BYTE = SCREEN CODE IN ELE             
*                                         WORK(3)- DATE                         
*                                         WORK+3 - FLAG FOR <= MATCH            
*                                  XIT -  CC SET                                
CKDATE   NTR1                                                                   
         L     R4,AIO              R4=A(FULL ESTIMATE RECORD)                   
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL6                                                        
         B     *+8                                                              
CKDATE5  BAS   RE,NEXTEL6                                                       
         BNE   FILTRX                                                           
         SPACE 1                                                                
         USING TAACD,R4                                                         
         CLC   TAACSCR,BYTE        MATCH ON SCREEN CODE                         
         BNE   CKDATE5                                                          
         SPACE 1                                                                
         CLI   WORK+3,C'Y'         IF WANT ESTIMATES <= DATE                    
         BNE   CKDATE8                                                          
         CLC   TAACCDTE,WORK       REJECT DATES W/HIGH ACTIVITY                 
         BH    CKDATE5                                                          
         B     FILTRYES                                                         
*                                                                               
CKDATE8  CLC   TAACCDTE,WORK       REJECT DATES W/LOWER ACTIVITY                
         BL    CKDATE5                                                          
         B     FILTRYES                                                         
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE CREAMS ESTIMATE RECORD DETAILS WITH JUST                 
*              ONE FUDGED ELEMENT X'05'                                         
         SPACE 1                                                                
FUDGREC  NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TANAELQ,0,0                                         
         BE    *+10                                                             
         XC    TGNAME,TGNAME                                                    
*                                                                               
         L     R4,AIO                                                           
         USING TLESD,R4                                                         
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING TA05D,R3                                                         
         MVI   TA05EL,TA05ELQ                                                   
         MVI   TA05LEN,TA05LNQ                                                  
         MVC   TA05ID,TLESEST                                                   
         MVC   TA05NAME,TGNAME                                                  
*                                                                               
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL6                                                        
         B     *+8                                                              
FUDGR2   BAS   RE,NEXTEL6                                                       
         BNE   FUDGR8                                                           
*                                                                               
         USING TAACD,R4                                                         
         CLI   TAACSCR,SCR40                                                    
         BNE   FUDGR5                                                           
         CLC   TA05RDTE,TAACCDTE                                                
         BH    *+10                                                             
         MVC   TA05RDTE,TAACCDTE                                                
         B     FUDGR2                                                           
*                                                                               
FUDGR5   CLI   TAACSCR,0                                                        
         BNE   FUDGR2                                                           
         CLC   TA05CDTE,TAACCDTE                                                
         BH    *+10                                                             
         MVC   TA05CDTE,TAACCDTE                                                
         B     FUDGR2                                                           
*                                                                               
FUDGR8   L     R4,AIO                                                           
         USING TLESD,R4                                                         
         LH    R1,DATADISP                                                      
         LA    R1,TA05LNQ(R1)                                                   
         STH   R1,TLESLEN                                                       
*                                                                               
         LR    R1,R4                                                            
         AH    R1,DATADISP                                                      
         MVC   0(TA05LNQ,R1),ELEMENT                                            
         LA    R1,TA05LNQ(R1)                                                   
         MVI   0(R1),0                                                          
         B     FILTRX                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FILTER COMMERCIAL RECORD ON LIST                      
         SPACE 1                                                                
         USING TLCOD,R4                                                         
FILTCOL  NTR1                                                                   
         CLC   FLTAGY,SPACES                                                    
         BNH   *+14                                                             
         CLC   FLTAGY,TLCOAGY                                                   
         BNE   FILTRNO                                                          
*                                                                               
         CLC   FLTPRD,SPACES                                                    
         BNH   *+14                                                             
         CLC   FLTPRD,TLCOPRD                                                   
         BNE   FILTRNO                                                          
*                                                                               
         MVI   ELCODE,TAOCELQ                                                   
         USING TAOCD,R4                                                         
         BAS   RE,GETEL6                                                        
         B     *+8                                                              
FLTCOL2  BAS   RE,NEXTEL6                                                       
         BNE   FLTCOL5                                                          
         TM    TAOCSTAT,TAOCSTO                                                 
         BO    FILTRNO                                                          
         B     FLTCOL2                                                          
*                                                                               
FLTCOL5  L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL6                                                        
         BNE   FILTRNO                                                          
         USING TACOD,R4                                                         
         SPACE 1                                                                
         BAS   RE,COLOCK           CHECK PASS LOCK TEST                         
         BNE   FILTRNO                                                          
         BAS   RE,CORELEAS         CHECK PASS RELEASED TEST                     
         BNE   FILTRNO                                                          
         BAS   RE,COMUSIC          CHECK PASS MUSIC TEST                        
         BNE   FILTRNO                                                          
         BAS   RE,COACTV           CHECK PASS ACTIVITY TEST                     
         BNE   FILTRNO                                                          
*                                                                               
         CLC   FLTMED,SPACES                                                    
         BNH   *+14                                                             
         CLC   FLTMED,TACOMED                                                   
         BNE   FILTRNO                                                          
*                                                                               
         CLC   FLTCLG,SPACES                                                    
         BNH   FILTRYES                                                         
         CLC   FLTCLG,TACOCLG                                                   
         B     FILTRX                                                           
         EJECT                                                                  
*              ROUTINE CHECKS COMMERCIAL LOCKED FILTER                          
*                                                                               
COLOCK   NTR1                                                                   
         CLI   FLTLOCK,C'O'        IF ONLY LOCKED COMMERCIALS                   
         BNE   *+16                                                             
         TM    TACOSTAT,TACOSTLO   JUST SHOW LOCKED COMMERCIALS                 
         BZ    FILTRNO                                                          
         B     FILTRYES                                                         
         CLI   FLTLOCK,C'Y'        ELSE, UNLESS REQUESTED TO INCLUDE            
         BE    *+12                                                             
         TM    TACOSTAT,TACOSTLO   IGNORE LOCKED COMMERCIALS                    
         BO    FILTRNO                                                          
         B     FILTRYES                                                         
         SPACE 2                                                                
*              ROUTINE CHECKS COMMERCIAL RELEASED FILTER                        
*                                                                               
CORELEAS NTR1                                                                   
         CLI   FLTREL,C'O'         IF ONLY RELEASED COMMERCIALS                 
         BNE   *+16                                                             
         TM    TACOSTAT,TACOSTRL   JUST SHOW RELEASED COMMERCIALS               
         BZ    FILTRNO                                                          
         B     FILTRYES                                                         
         CLI   FLTREL,C'Y'         ELSE, UNLESS REQUESTED TO INCLUDE            
         BE    *+12                                                             
         TM    TACOSTAT,TACOSTRL   IGNORE RELEASED COMMERCIALS                  
         BO    FILTRNO                                                          
         B     FILTRYES                                                         
         EJECT                                                                  
*              ROUTINE CHECKS COMMERCIAL ACTIVITY FILTER                        
*                                                                               
COMUSIC  NTR1                                                                   
         CLI   FLTMUS,C'N'         IF EXCLUDING MUSIC COMMLS                    
         BNE   FILTRYES                                                         
         CLI   TACOTYPE,CTYMUS     IGNORE THEM                                  
         BE    FILTRNO                                                          
         B     FILTRYES                                                         
         SPACE 2                                                                
*              ROUTINE CHECKS COMMERCIAL ACTIVITY FILTER                        
*                                                                               
COACTV   NTR1                                                                   
         OC    TACOPDTE,TACOPDTE   IF LAST PAID DATE                            
         BZ    COACT10                                                          
         CLC   TACOPDTE,FLTPDTE    CHECK AGAINST REQUESTED DATE                 
         BL    FILTRNO                                                          
         B     FILTRYES                                                         
*                                                                               
COACT10  MVI   ELCODE,TAACELQ      ELSE, GET LAST ACTIVITY ELE                  
         L     R4,AIO                                                           
         BAS   RE,GETEL6                                                        
         BNE   FILTRNO                                                          
         USING TAACD,R4                                                         
         CLC   TAACCDTE,FLTPDTE    CHECK ACTIVITY SINCE LAST ACTV DATE          
         BL    FILTRNO                                                          
         B     FILTRYES                                                         
         DROP  R4                                                               
         GETELN (R4),DATADISP,ELCODE,6                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALLS SETELEM IN FALINK                                  
         SPACE 1                                                                
         USING MHELD,R5            R5=A(ELEMENT HDR)                            
         DS    0D                                                               
DOSETEL  NMOD1 0,*DOSETEL                                                       
         L     RC,0(R1)                                                         
         OI    STESTAT,STEDATA     PASSED DATA TO FALINK                        
*                                                                               
         TM    STESTAT,STENEWC     IF PROCESSING NEW COMMERCIAL                 
         BZ    DOSETEL5                                                         
         GOTO1 ASETELEM,DMCB,FABLK,0,(8,ELFDATA)                                
         B     DOSETELX                                                         
*                                                                               
DOSETEL5 GOTO1 ASETELEM,DMCB,FABLK,(R5),(2,MHUTEXT)                             
DOSETELX XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*        ROUTINE PROCESSES ONE ELEMENT                                          
*                                  NTRY - R4=A(ELEMENT)                         
         USING MHELD,R5                   R5=A(HEADER TYPE ENTRY)               
         SPACE 1                                                                
         DS    0D                                                               
PROCEL   NMOD1 0,*PROCEL*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   SVELCODE,ELCODE     SAVE ELEMENT CODE                            
*                                                                               
         MVC   ELBLEN,MHUBLEN      SAVE BASE LENGTH                             
         ZIC   R0,MHLEN            R0=L'HEADER                                  
         AR    R5,R0                                                            
         USING MDELD,R5            R5=A(FIRST ELEMENT FIELD)                    
*                                                                               
PROCEL1  CLI   MDLEN,0             IF ANY MORE ELEMENT FIELDS                   
         BE    PROCELX                                                          
         TM    MDUSTAT,MDXDWN      IF EXCLUDING ON DOWNLOAD                     
         BO    PROCEL6             THEN SKIP                                    
*                                                                               
         TM    MDUSTAT,MDXUHM      IF NEEDS TO BE EXCLUDED FROM UHM             
         BZ    *+14                                                             
         CLC   =C'UHM',MYKCODE                                                  
         BE    PROCEL6             THEN SKIP                                    
*                                                                               
         TM    MDUSTAT,MDINI       IF NEEDS TO BE INCLUDED IN INI ONLY          
         BZ    *+14                                                             
         CLC   =C'INI',MYKCODE                                                  
         BNE   PROCEL6             THEN SKIP IF NOT INI                         
*                                                                               
         TM    MDUSTAT,MDCOMB      IF COMBINATION SUB-OPTION                    
         BO    PROCEL2             MDUTYPE2 IS REUSED                           
         CLI   MDUTYPE2,0          IF NOT FOR ALL TYPES                         
         BE    PROCEL2                                                          
         CLC   ELETYPE,MDUTYPE2    MATCH ON FIELD TYPE                          
         BNE   PROCEL6                                                          
*                                                                               
PROCEL2  XC    ELFDATA,ELFDATA     CLEAR OVERRIDE DATA AREA                     
         LH    R3,MDUDISP                                                       
         TM    MDUSTAT,MDTGD                                                    
         BO    *+10                                                             
         AR    R3,R4                                                            
         B     PROCEL3                                                          
         LA    R1,TGD                                                           
         AR    R3,R1               R3=A(DATA)                                   
*                                                                               
PROCEL3  ZIC   R2,MDDLEN           R2=DATA LENGTH                               
         TM    MDUSTAT2,MDESO                                                   
         BZ    *+8                                                              
         IC    R2,MDULEN                                                        
         TM    MDUSTAT,MDCLEN      IF NEED TO CALCULATE DATA LENGTH             
         BZ    PROCEL4                                                          
         IC    R2,1(R4)            TAKE ELEMENT LENGTH                          
         ZIC   RE,ELBLEN           LESS BASE LENGTH                             
         SR    R2,RE                                                            
*                                                                               
PROCEL4  TM    MDUSTAT,MDZERO      IF FORCING ZERO TO PRINT                     
         BO    PROCEL4X            SKIP NO DATA CHECK                           
         LTR   R2,R2                                                            
         BZ    PROCEL6                                                          
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    0(0,R3),0(R3)                                                    
         BZ    PROCEL6                                                          
         LA    R2,1(R2)                                                         
*                                                                               
PROCEL4X XR    R1,R1               FLAG - CONTINUE WITH ELEMENT FIELDS          
         XR    RF,RF                                                            
         ICM   RF,3,MDURCV         IF SPECIFIC DATA ROUTINE                     
         BZ    PROCEL5                                                          
         AR    RF,RB                                                            
         LA    RE,PROCEL5                                                       
         NTR1                                                                   
         BR    RF                  RETURNS R1                                   
*                                                                               
PROCEL5  CH    R1,=H'0'            CONTINUE TO NEXT ELEMENT FIELDS              
         BL    PROCEL6                                                          
         BH    PROCELX                                                          
         TM    MDUSTAT,MDCLEN      IF DATA LENGTH NOT CALCULATED                
         BO    *+6                                                              
         XR    R2,R2               NO NEED TO PASS TO FALINK                    
         OC    MDURCV,MDURCV                                                    
         BZ    *+8                                                              
         LA    R3,ELFDATA                                                       
         GOTO1 AADDDATA,DMCB,FABLK,(R5),(R3),(R2)                               
*                                                                               
PROCEL6  ZIC   R0,MDLEN            BUMP TO NEXT FIELD ELEMENT ENTRY             
         AR    R5,R0                                                            
         B     PROCEL1                                                          
*                                                                               
PROCELX  MVC   ELCODE,SVELCODE     RESET ELEMENT CODE                           
         MVC   AIO,SVAIO           RESET AIO TO MAIN RECORD/TABLE               
*                                                                               
PROCXIT  XIT1                                                                   
         SPACE 1                                                                
PLOWXIT1 LH    R1,=H'-1'           REG1 LOW SKIPS FIELD                         
         B     *+8                                                              
PHIXIT1  LH    R1,=H'1'            REG1 LOW SKIPS REST OF ELEMENT               
         XIT1  REGS=(R1)                                                        
         DROP  R5                                                               
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD ROUTINES TO ELFDATA                       
*                                  R3=A(ELEMENT DATA)                           
*                                  R4=A(ELEMENT)                                
         USING MDELD,R5            R5=A(ELEMENT FIELD ENTRY)                    
         SPACE 2                                                                
DCNT     XR    R1,R1               RTN FOR # OF HYPO PERFS, # OF MUS            
         CLI   MDDLEN,1                                                         
         BNE   *+12                                                             
         IC    R1,0(R3)                                                         
         B     *+8                                                              
         ICM   R1,3,0(R3)                                                       
         CH    R1,=H'1'            DEFAULT IF NOT MORE THAN ONE                 
         BNH   PLOWXIT1                                                         
         ZIC   R1,MDDLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELFDATA(0),0(R3)                                                 
         B     PROCXIT                                                          
         SPACE 2                                                                
DMEDQ    GOTO1 MEDVAL,DMCB,(X'80',0(R3))                                        
         OC    TGMENAME,TGMENAME                                                
         BZ    PLOWXIT1                                                         
         B     DMED5                                                            
         SPACE 2                                                                
DMED     GOTO1 MEDVAL,DMCB,0(R3)                                                
         BNE   PLOWXIT1                                                         
DMED5    MVC   ELFDATA(1),TGMENAME                                              
         BAS   RE,IFOPTIM          IF COL/CAL/UHM OPTIMIZATION NEC.             
         BNE   PROCXIT                                                          
         CLC   SVMED,ELFDATA       IF NO CHANGE IN MEDIA CODE                   
         BE    PLOWXIT1            SKIP SHOWING                                 
         MVC   SVMED,ELFDATA       ELSE, SAVE NEW CODE & DISPLAY                
         B     PROCXIT                                                          
         SPACE 2                                                                
DUSET    GOTO1 USEVAL,DMCB,TGUSCDE,0(R3)                                        
         CLC   TGUSTYCD,SPACES                                                  
         BNH   PLOWXIT1                                                         
         MVC   ELFDATA(L'TGUSTYCD),TGUSTYCD                                     
         B     PROCXIT                                                          
         SPACE 2                                                                
DUHST    TM    0(R3),TAUHSUPN+TAUHSUPW IF UPGRADED TO SNW FROM SNT/SWS          
         BZ    PLOWXIT1                SKIP                                     
         MVC   ELFDATA(1),0(R3)        ELSE, PASS ALONG STATUS                  
         B     PROCXIT                                                          
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DCATQ    GOTO1 CATVAL,DMCB,(X'80',0(R3))                                        
*                                                                               
DCAT     MVC   ELFDATA(L'TGCAT),TGCAT                                           
         BAS   RE,IFOPTIM          IF COL/CAL/UHM OPTIMIZATION NEC.             
         BNE   PROCXIT                                                          
         CLC   SVCAT,ELFDATA       IF NO CHANGE IN CODE                         
         BE    PLOWXIT1            SKIP SHOWING                                 
         MVC   SVCAT,ELFDATA       ELSE, SAVE NEW CODE & DISPLAY                
         B     PROCXIT                                                          
         SPACE 2                                                                
DUNIQ    GOTO1 UNIVAL,DMCB,(X'20',0(R3))                                        
         MVC   ELFDATA(L'TGUNI),TGUNI                                           
         B     PROCXIT                                                          
         SPACE 2                                                                
DUNI     MVC   ELFDATA(L'TACAUN),0(R3)                                          
         BAS   RE,IFOPTIM          IF COL/CAL/UHM OPTIMIZATION NEC.             
         BNE   PROCXIT                                                          
         CLC   SVUNI,ELFDATA       IF NO CHANGE IN MEDIA CODE                   
         BE    PLOWXIT1            SKIP SHOWING                                 
         MVC   SVUNI,ELFDATA       ELSE, SAVE NEW CODE & DISPLAY                
         B     PROCXIT                                                          
         SPACE 2                                                                
DYES     MVI   ELFDATA,C' '        YES SHOULD ONLY SHOW MAPCODE                 
         B     PROCXIT                                                          
         SPACE 2                                                                
DREL     TM    0(R3),TACOSTRL      RELEASED COMMERCIAL                          
         BZ    PLOWXIT1                                                         
         MVI   ELFDATA,C' '        YES SHOULD ONLY SHOW LEFT HAND SIDE          
         B     PROCXIT                                                          
         SPACE 2                                                                
DLIFT    L     R4,AIO              LIFT DETAILS                                 
         MVI   ELCODE,TALFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DL10                                                             
         MVI   ELFDATA,C' '        YES SHOULD ONLY SHOW LEFT HAND SIDE          
         B     PROCXIT                                                          
         SPACE 1                                                                
DLIFT2   L     R4,AIO              2ND LIFT DETAILS                             
         MVI   ELCODE,TAL2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DL10                                                             
         MVI   ELFDATA,C' '        YES SHOULD ONLY SHOW LEFT HAND SIDE          
         B     PROCXIT                                                          
         SPACE 1                                                                
         USING TAVRD,R4                                                         
DL10     L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TAVRELQ      READ VERSION ELEMENTS                        
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DL20     BRAS  RE,NEXTEL                                                        
         BNE   PLOWXIT1                                                         
         SPACE 1                                                                
         CLI   TAVRVERS,2          IF ELEMENT FOR VERSION 2                     
         BNE   *+12                IS FOUND ...                                 
         MVI   ELFDATA,C'Y'        SAVE LIFT DETAILS                            
         B     DL20                                                             
         SPACE 1                                                                
         CLI   TAVRVERS,3          IF ELEMENT FOR VERSION 3                     
         BNE   DL20                IS FOUND                                     
         CLI   ELFDATA,C'Y'        ALREADY FOUND A 2ND LIFT?                    
         BNE   *+12                                                             
         MVI   ELFDATA,C'B'                                                     
         B     *+8                                                              
         MVI   ELFDATA,C'S'        SAVE LIFT DETAILS                            
         B     DL20                                                             
         DROP  R4                                                               
         SPACE 2                                                                
DCLI     L     R4,AIO                                                           
         MVC   ELFDATA(L'TLCOCLI),TLCOCLI-TLCOD(R4)                             
         BAS   RE,IFOPTIM          IF COL/CAL/UHM OPTIMIZATION NEC.             
         BNE   PROCXIT                                                          
         CLC   SVCLI,ELFDATA       IF NO CHANGE IN CODE                         
         BE    PLOWXIT1            SKIP SHOWING                                 
         MVC   SVCLI,ELFDATA       ELSE, SAVE NEW CODE & DISPLAY                
         B     PROCXIT                                                          
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DPRD     L     R4,AIO                                                           
         MVC   ELFDATA(L'TLCOPRD),TLCOPRD-TLCOD(R4)                             
         OC    ELFDATA,SPACES                                                   
         BAS   RE,IFOPTIM          IF COL/CAL/UHM OPTIMIZATION NEC.             
         BNE   DPRD5                                                            
         CLC   SVPRD,ELFDATA       IF NO CHANGE IN CODE                         
         BE    PLOWXIT1            SKIP SHOWING                                 
         MVC   SVPRD,ELFDATA       ELSE, SAVE NEW CODE & DISPLAY                
         B     PROCXIT                                                          
*                                                                               
DPRD5    CLC   ELFDATA(L'TLCOPRD),SPACES   SKIP IF NO PRD                       
         BNH   PLOWXIT1                                                         
         B     PROCXIT                                                          
         SPACE 2                                                                
DCIDN    GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         BNE   PLOWXIT1                                                         
         MVC   ELFDATA(L'TGNAME),TGNAME                                         
         B     PROCXIT                                                          
         SPACE 2                                                                
DDBL     CLI   0(R3),C' '          DON'T SHOW BLANKS                            
         BE    PLOWXIT1                                                         
         MVC   ELFDATA(L'TA01DBL),0(R3)                                         
         B     PROCXIT                                                          
         SPACE 2                                                                
DYEAR    MVC   ELFDATA(L'TACAYEAR),0(R3)                                        
         BAS   RE,IFOPTIM          IF COL/CAL/UHM OPTIMIZATION NEC.             
         BNE   PROCXIT                                                          
         CLC   SVYEAR,ELFDATA      IF NO CHANGE IN CODE                         
         BE    PLOWXIT1            SKIP SHOWING                                 
         MVC   SVYEAR,ELFDATA       ELSE, SAVE NEW CODE & DISPLAY               
         B     PROCXIT                                                          
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DCAM     MVC   ELFDATA(L'TACAONOF),0(R3)                                        
         BAS   RE,IFOPTIM          IF COL/CAL/UHM OPTIMIZATION NEC.             
         BNE   DCAM5                                                            
         CLC   SVCAM,ELFDATA       IF NO CHANGE IN CODE                         
         BE    PLOWXIT1            SKIP SHOWING                                 
         MVC   SVCAM,ELFDATA       ELSE, SAVE NEW CODE & DISPLAY                
         B     PROCXIT                                                          
*                                                                               
DCAM5    CLC   =C'OFF',0(R3)       ELSE, DON'T SHOW OFF CAMERA                  
         BE    PLOWXIT1                                                         
         B     PROCXIT                                                          
         SPACE 2                                                                
DCOMG    CLI   0(R4),TAGUELQ       IF NOT GUARANTEE ELEMENT                     
         BE    *+14                                                             
         MVC   TGCOM,0(R3)         SET GLOBAL                                   
         B     PLOWXIT1                                                         
         OC    0(L'TGCOM,R3),0(R3) ELSE, JUST CHK INTERNAL COMML NO             
         BZ    PHIXIT1             IF NONE, SKIP REST OF ELEMENT                
         B     PLOWXIT1            ELSE, PROCESS THE REST                       
         SPACE 2                                                                
DSORTG   MVC   TGCSORT,0(R3)                                                    
         B     PLOWXIT1                                                         
         SPACE 2                                                                
DSSNG    EDIT  (4,0(R3)),(9,TGSSN),FILL=0                                       
         B     PLOWXIT1                                                         
         SPACE 2                                                                
DUSE     MVC   ELFDATA(3),0(R3)                                                 
         CLC   0(3,R3),SPACES                                                   
         BNE   *+10                                                             
         MVC   ELFDATA(3),=C'ALL'                                               
         B     PROCXIT                                                          
         SPACE 2                                                                
DSSNF    CLI   TGTYPE,TAW4TYCO     SHOW FIRST NAME FROM W4 RECORD               
         BE    *+20                                                             
         CLI   TGTYPE,TAW4TYTR                                                  
         BE    *+12                                                             
         CLI   TGTYPE,TAW4TYES                                                  
         BNE   *+14                                                             
         MVC   ELFDATA(L'TGNAME),TGNAME                                         
         B     *+10                                                             
         MVC   ELFDATA(L'TAW4NAM1),TGNAME+L'TAW4NAM2                            
         B     PROCXIT                                                          
         SPACE 2                                                                
DSSNL    CLI   TGTYPE,TAW4TYCO     SHOW LAST NAME FROM W4 RECORD                
         BE    PLOWXIT1                                                         
         CLI   TGTYPE,TAW4TYTR                                                  
         BE    PLOWXIT1                                                         
         CLI   TGTYPE,TAW4TYES                                                  
         BE    PLOWXIT1                                                         
         MVC   ELFDATA(L'TAW4NAM2),TGNAME                                       
         B     PROCXIT                                                          
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 1                                                                
DCIDF    L     R4,AIO              FIRST FIXED CYCLE FROM COMML                 
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PLOWXIT1                                                         
         USING TACOD,R4                                                         
         LA    R3,TACOFCYC         USE FIRST FIXED CYCLE                        
         CLI   TACOTYPE,CTYMUS     IF MUSIC                                     
         BNE   DCIDF2                                                           
         MVI   ELCODE,TACSELQ                                                   
         MVI   WORK,TACSTYPM       USE MUSIC DATE                               
         B     DCIDF4                                                           
DCIDF2   CLI   TACOMED,TACOMEDR    IF RADIO                                     
         BNE   DCIDF6                                                           
         MVI   ELCODE,TACSELQ                                                   
         MVI   WORK,TACSTYPR       USE RECORDING DATE                           
DCIDF4   GOTO1 GETL,DMCB,(1,WORK)                                               
         BNE   PLOWXIT1                                                         
*                                                                               
         L     R4,TGELEM                                                        
         USING TACSD,R4                                                         
         LA    R3,TACSDATE                                                      
DCIDF6   OC    0(3,R3),0(R3)       MAKE SURE THERE IS A DATE                    
         BZ    PLOWXIT1                                                         
         MVC   ELFDATA(L'TGDATE),0(R3)                                          
         B     PROCXIT                                                          
         SPACE 2                                                                
DGRTG    MVC   TGGUA,0(R3)         SET GOBAL GUARANTEE AND SHOW IT              
         XC    TGGUA,=4X'FF'                                                    
         CLC   TGGUA,=4X'FF'                                                    
         BE    PLOWXIT1                                                         
         SPACE 2                                                                
DGRT     MVC   WORK(L'TGGUA),TGGUA REMOVE LEADING ZEROES FROM GRT #             
         XC    WORK(L'TGGUA),=4X'FF'                                            
         LA    R1,L'TGGUA          REMOVE LEADING ZEROS                         
         LA    RE,WORK                                                          
DGRT5    CLI   0(RE),C'0'                                                       
         BNE   DGRT8                                                            
         LA    RE,1(RE)                                                         
         BCT   R1,DGRT5                                                         
*                                                                               
         LA    R1,L'TGGUA                                                       
DGRT8    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     PROCXIT                                                          
         MVC   ELFDATA(0),0(RE)                                                 
         EJECT                                                                  
DHCID    DS    0H                                                               
         CLI   9(R3),0             HYPO COMML MORE THAN 10 CHARS?               
         BE    DHCID5                                                           
         MVC   ELFDATA(L'TAESHCOM),0(R3)                                        
         MVC   ELFDATA+10(2),=C'  '                                             
         B     PROCXIT                                                          
*                                                                               
DHCID5   GOTO1 =A(CHPACK),DMCB,(C'U',(R3)),ELFDATA,RR=RELO                      
         B     PROCXIT                                                          
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 1                                                                
DGCDTE   OC    TGCOM,TGCOM         IF THERE WAS A GUARANTEE COMMERCIAL          
         BZ    PLOWXIT1                                                         
         XC    FULL,FULL           GUARANTEE END DATE                           
         L     R4,AIO              R4=A(GUARANTEE RECORD)                       
         USING TAGCD,R4                                                         
         MVI   ELCODE,TAGCELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DGCDTE5  BAS   RE,NEXTEL                                                        
         BNE   *+14                                                             
         MVC   FULL(L'TAGCEND),TAGCEND                                          
         B     DGCDTE5                                                          
*                                                                               
         OC    FULL,FULL                                                        
         BZ    PLOWXIT1                                                         
         MVC   ELFDATA(L'TGDATE),FULL                                           
         B     PROCXIT                                                          
         SPACE 2                                                                
DSTAF    MVC   TGUSER,0(R3)        READ STAFF RECORD FOR NAME                   
         MVC   TGSTAF,L'TGUSER(R3)                                              
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,AWRKIO                                                       
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'A8',0),0                                  
         BE    *+10                                                             
         MVC   TGNAME,=CL36'** NOT FOUND **'                                    
         MVC   ELFDATA(L'TGNAME),TGNAME                                         
         B     PROCXIT                                                          
         SPACE 2                                                                
DCAST    XC    TGCSORT,TGCSORT     ANY ACTUAL CAST                              
         XC    TGSSN,TGSSN                                                      
         XC    TGCAT,TGCAT                                                      
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         GOTO1 RECVAL,DMCB,TLCACDQ,0                                            
         CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         BE    PLOWXIT1                                                         
         MVI   ELFDATA,C' '        SHOW LEFT HAND SIDE ONLY                     
         B     PROCXIT                                                          
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 1                                                                
DCID     OC    0(L'TGCOM,R3),0(R3) IF THERE WAS A GUARANTEE COMMERCIAL          
         BZ    PLOWXIT1                                                         
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,AWRKIO                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,TLCOCCDQ                                                     
         MVC   KEY+TLCOCCOM-TLCOPD(L'TLCOCCOM),0(R3)                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   PLOWXIT1                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO              SHOW CID                                     
         MVC   ELFDATA(L'TLCOCID),TLCOCID-TLCOD(R1)                             
         B     PROCXIT                                                          
         SPACE 2                                                                
DTIME    GOTO1 TIMECON,DMCB,3(R3),0(R3),(8,ELFDATA)                             
         B     PROCXIT                                                          
         SPACE 2                                                                
DOPTS    ZIC   R0,0(R3)            R0=N'SUB ENTRIES                             
         LTR   R0,R0                                                            
         BZ    PHIXIT1                                                          
         LA    R5,0(R3)                                                         
         LA    R5,1(R5)            R5=A(FIRST SUB ENTRY)                        
         BAS   RE,OPTELS                                                        
         B     PHIXIT1                                                          
         SPACE 2                                                                
*              ROUTINE CHECKS FOR COL/CAL/UHM OPTIMIZATION                      
         SPACE 2                                                                
IFOPTIM  NTR1                                                                   
         CLC   =C'COL',MYKCODE                                                  
         BE    YES                                                              
         CLC   =C'CAL',MYKCODE                                                  
         BE    YES                                                              
         CLC   =C'UHM',MYKCODE                                                  
         BE    YES                                                              
         B     NO                                                               
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO HANDLE SUB-EL OPTIONS ON TAESD FOR SEND               
*                                  R0=(N'SUB-ENTRIES)                           
         USING TAESSBCD,R5         R5=A(FIRST SUB-ENTRY)                        
*                                                                               
OPTELS   NTR1                                                                   
         XC    ANXTCOMB,ANXTCOMB                                                
*                                                                               
OPTELS4  BAS   RE,GETOP            GET TABLE ENTRY FOR SUB-OPTION               
         BNE   OPTELS9                                                          
         USING MDELD,R4            R4=A(TABLE ENTRY) & R3=A(DATA)               
*                                                                               
         XC    ELFDATA,ELFDATA     PRE-CLEAR OVERRIDE DATA AREA                 
         XR    R2,R2               R2=LENGTH OF DATA PRE-CLEARED                
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,MDURCV         RF=DISPLACEMENT TO O/P ROUTINE               
         BZ    OPTELS8                                                          
         AR    RF,RB                                                            
         LA    RE,OPTELS7                                                       
         NTR1 ,                                                                 
         BR    RF                                                               
         SPACE 1                                                                
OPTELS7  BL    OPTELS9             IF ERROR -SKIP TO NEXT SUB-OPTION            
         SPACE 1                                                                
         LA    R3,ELFDATA          R3=A(OVERRIDE DATA)                          
         CLI   TAESSBCD,OPEXP                                                   
         BNE   OPTELS7X                                                         
         CLI   ELFDATA,C' '                                                     
         BE    OPTELS7X                                                         
         ZIC   RE,MDLEN                                                         
         AR    R4,RE                                                            
OPTELS7X ZIC   R2,MDDLEN           R2=LENGTH OF DATA                            
         CLI   TAESSBCD,OPCOMNM                                                 
         BNE   *+10                                                             
         IC    R2,TAESSBDT                                                      
         BCTR  R2,0                                                             
         SPACE 1                                                                
OPTELS8  GOTO1 AADDDATA,DMCB,FABLK,(R4),(R3),(R2)                               
         SPACE 1                                                                
OPTELS9  OC    ANXTCOMB,ANXTCOMB   IF PROCESSING COMB. SUB-OPTION               
         BZ    OPTELS10                                                         
         L     RE,ANXTCOMB         AND NO MORE DETAIL FOR SUB-OPTION            
         OC    0(L'COMBDNUM,RE),0(RE)                                           
         BNZ   OPTELS4                                                          
         XC    ANXTCOMB,ANXTCOMB                                                
         L     RE,ACOMBTAB                                                      
         ZIC   RF,COMBOLEN-COMBTLEN(RE)                                         
         B     OPTELS12                                                         
*                                                                               
OPTELS10 ZIC   RF,MDULEN           BUMP TO NEXT ELEMENT SUB-DATA                
         CLI   TAESSBCD,OPCOMNM    IF THIS IS COMML NAME                        
         BNE   *+8                                                              
         IC    RF,TAESSBDT         LENGTH IS IN FIRST DATA BYTE                 
OPTELS12 LA    R5,1(RF,R5)                                                      
         BCT   R0,OPTELS4          LOOP                                         
         B     OPTELSX                                                          
         SPACE 1                                                                
OPTELSL  LNR   RC,RC               RETURN CC NOW                                
         B     OPTELSN                                                          
OPTELSY  XR    RC,RC               RETURN CC EQUAL                              
OPTELSN  LTR   RC,RC               RETURN CC NOT EQUAL                          
OPTELSX  B     PROCXIT             RETURN                                       
         EJECT                                                                  
*              ROUTINE TO GET TABLE ENTRY FOR ESTIMATE SUB-OPTION               
GETOP    NTR1                                                                   
         LA    R2,COMBTAB          CHECK COMBINATION SUB-OPTION FIRST           
         USING COMBTABD,R2                                                      
GETOP5   CLI   COMBTLEN,X'FF'                                                   
         BE    GETOP15                                                          
         CLC   COMBOPT,0(R5)                                                    
         BE    GETOP6                                                           
         ZIC   R1,COMBTLEN                                                      
         AR    R2,R1                                                            
         B     GETOP5                                                           
*                                                                               
GETOP6   ST    R2,ACOMBTAB         SAVE A(COMBINATION ENTRY)                    
         LA    RE,COMBDET                                                       
         USING COMBDET,RE                                                       
         OC    ANXTCOMB,ANXTCOMB   IF ALREADY PROCESSING DETAIL                 
         BZ    *+8                                                              
         L     RE,ANXTCOMB         CONTINUE WHERE WE LEFT OFF                   
         MVC   HALF,COMBDNUM       HALF=MAPCODE OF ENTRY TO FIND                
         ZIC   R1,COMBDDSP                                                      
         AR    R1,R5                                                            
         LR    R3,R1               R3=A(DATA)                                   
         LA    RE,L'COMBDET(RE)                                                 
         ST    RE,ANXTCOMB         SAVE A(NXT DETAIL) IN COMB. SUB-OPT          
*                                                                               
         L     R4,AOPTS            FIND ITS ENTRY                               
GETOP10  CLI   MDLEN,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MDCODE,HALF                                                      
         BE    GETOPY                                                           
         ZIC   RE,MDLEN                                                         
         AR    R4,RE                                                            
         B     GETOP10                                                          
*                                                                               
GETOP15  XC    ANXTCOMB,ANXTCOMB   NOT COMBINATION SUB-OPTION                   
         L     R4,AOPTS            FIND ENTRY BASED ON OPTION                   
GETOP18  CLI   MDLEN,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MDUCODE,0(R5)        TEST MATCH                                  
         BE    GETOP20                                                          
         ZIC   RE,MDLEN                                                         
         AR    R4,RE                                                            
         B     GETOP18                                                          
*                                                                               
GETOP20  TM    MDUSTAT,MDXDWN                                                   
         BO    GETOPN                                                           
         LA    R3,1(R5)            R3=A(DATA)                                   
*                                                                               
GETOPY   CR    RB,RB                                                            
         B     *+6                                                              
GETOPN   LTR   RB,RB                                                            
         XIT1  REGS=(R3,R4)                                                     
         DROP  R4,R5                                                            
         EJECT                                                                  
*        OPTION OUTPUT ROUTINES FOR DOWNLOAD                                    
         SPACE 1                                                                
         USING MDELD,R4            R4=A(MAP TABLE ENTRY)                        
*                                  R3=A(DATA)                                   
         SPACE 1                                                                
OUSE     GOTO1 USEVAL,DMCB,(X'80',0(R3)),1(R3)    CVT TO CHAR.                  
         BE    OUSE1                                                            
         GOTO1 USEVAL,DMCB,(X'C0',0(R3))          TRY W/O TYPE                  
         BNE   OPTELSL             ERROR - DON'T DISPLAY                        
OUSE1    MVC   ELFDATA(L'TGUSCDE),TGUSCDE                                       
         MVC   ELFDATA+L'TGUSCDE(L'TGUSTYCD),TGUSTYCD                           
         B     OPTELSY                                                          
         SPACE 2                                                                
OYR      GOTO1 YRVAL,DMCB,(X'80',0(R3))                                         
         BNE   OPTELSL             ERROR - DON'T DISPLAY                        
         MVC   ELFDATA(L'TGYRCDE),TGYRCDE                                       
         B     OPTELSY                                                          
         SPACE 2                                                                
OCSF     LA    RE,ELFDATA                                                       
         CLI   0(R3),C'Y'          CSF                                          
         BNE   OCSF5                                                            
         L     R1,=F'-2'                                                        
         ST    R1,0(RE)                                                         
         B     OCSFX                                                            
*                                                                               
OCSF5    CLI   0(R3),C'N'                                                       
         BNE   OCSF8                                                            
         L     R1,=F'-3'                                                        
         ST    R1,0(RE)                                                         
         B     OCSFX                                                            
*                                                                               
OCSF8    MVC   0(LOPCSF,RE),0(R3)                                               
OCSFX    B     OPTELSY                                                          
         SPACE 2                                                                
OCOMNM   ZIC   RF,0(R3)            HYPO COMMERCIAL NAME                         
         AHI   RF,-2                                                            
         EX    RF,*+8                                                           
         B     OPTELSY                                                          
         MVC   ELFDATA(0),1(R3)                                                 
         SPACE 2                                                                
OATYPE   GOTO1 CCTYPVAL,DMCB,(X'80',0(R3))                                      
         BNE   OPTELSL                                                          
         MVC   ELFDATA(L'TGCCTCDE),TGCCTCDE                                     
         B     OPTELSY                                                          
         EJECT                                                                  
*        OPTION OUTPUT ROUTINES FOR DOWNLOAD (CONTINUED)                        
*        COMBINATION OPTIONS - CALL ADDDATA HERE                                
         SPACE 1                                                                
OURNG    XR    R1,R1                                                            
         XR    RE,RE                                                            
         ICM   R1,3,0(R3)          # OF USES                                    
         OC    ANXTCOMB,ANXTCOMB   IF COMBINATION OPTION                        
         BZ    OURNGX                                                           
         ICM   R1,3,2(R3)          END RANGE                                    
         ICM   RE,3,0(R3)          - START RANGE                                
         SR    R1,RE                                                            
         LTR   R1,R1                                                            
         BZ    OPTELSL                                                          
         LA    R1,1(R1)                                                         
OURNGX   STCM  R1,3,ELFDATA        # OF USES                                    
         B     OPTELSY                                                          
         SPACE 2                                                                
OEXP     CLI   0(R3),X'FF'         IF Y                                         
         BNE   *+12                                                             
         MVI   ELFDATA,C' '        JUST WANT MAPCODE                            
         B     *+10                                                             
         MVC   ELFDATA(LOPEXP),0(R3)  ELSE WANT DATE                            
         B     OPTELSY                                                          
*                                                                               
OLCLAU   ICM   R1,3,0(R3)          LAST CLA USE NUMBER                          
         STCM  R1,3,ELFDATA                                                     
         B     OPTELSY                                                          
*                                                                               
*===================================================================            
*              COMBINATION TABLE                                                
*              ESTIMATE SUB-OPTION AND THEIR COMPONENTS                         
         SPACE 1                                                                
COMBTAB  DS    0C                                                               
*                                  HLD RATE AS/OF A DATE                        
         DC    AL1(11),AL1(OPHLDRAS,LOPHLDRA)                                   
         DC    AL2(59),AL1(1)                                                   
         DC    AL2(58),AL1(LOPDATE+1)                                           
         DC    AL2(0)                                                           
*                                  HLD AMT AS/OF A DATE                         
         DC    AL1(11),AL1(OPHLDAS,LOPHLDAS)                                    
         DC    AL2(59),AL1(1)                                                   
         DC    AL2(57),AL1(LOPDATE+1)                                           
         DC    AL2(0)                                                           
*                                  CONTRACT YEAR AS/OF A DATE                   
         DC    AL1(11),AL1(OPYRAS,LOPYRAS)                                      
         DC    AL2(30),AL1(1)                                                   
         DC    AL2(29),AL1(LOPDATE+1)                                           
         DC    AL2(0)                                                           
*                                  OVERSCALE AS/OF A DATE                       
         DC    AL1(11),AL1(OPOVAS,LOPOVAS)                                      
         DC    AL2(27),AL1(1)                                                   
         DC    AL2(25),AL1(LOPDATE+1)                                           
         DC    AL2(0)                                                           
*                                  SECOND OVERSCALE AS/OF A DATE                
         DC    AL1(11),AL1(OPOV2AS,LOPOV2AS)                                    
         DC    AL2(27),AL1(1)                                                   
         DC    AL2(26),AL1(LOPDATE+1)                                           
         DC    AL2(0)                                                           
*                                  1ST AND 2ND OVERSCALE                        
         DC    AL1(11),AL1(OPOV12,LOPOV12)                                      
         DC    AL2(25),AL1(1)                                                   
         DC    AL2(26),AL1(LOPOVER+1)                                           
         DC    AL2(0)                                                           
*                                  1ST AND 2ND OVERSCALE AS/OF A DATE           
         DC    AL1(14),AL1(OPOV12AS,LOPOV12A)                                   
         DC    AL2(28),AL1(1)                                                   
         DC    AL2(25),AL1(LOPDATE+1)                                           
         DC    AL2(26),AL1(LOPDATE+LOPOVER+1)                                   
         DC    AL2(0)                                                           
*                                  CYCLE DATES                                  
         DC    AL1(11),AL1(OPCYC,LOPCYC)                                        
         DC    AL2(48),AL1(1)                                                   
         DC    AL2(47),AL1(LOPDATE+1)                                           
         DC    AL2(0)                                                           
*                                  USE RANGE                                    
         DC    AL1(11),AL1(OPUSES2,LOPUSES2)                                    
         DC    AL2(41),AL1(1)                                                   
         DC    AL2(40),AL1(1)                                                   
         DC    AL2(0)                                                           
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE DISPLAYS EL=CO ELEMENT, POSSIBLY FOLLOWED BY             
*              EL=UH ELEMENT(S), THEN FINALLY THE EL=ES ELEMENT FOR             
*              AN ACTUAL COMMERCIAL                                             
*                                                                               
         USING TAESD,R4            R4=A(ACTUAL COMMERCIAL EST. ELEMENT)         
         DS    0D                                                               
PRESCID  NMOD1 0,*PRESCID                                                       
         L     RC,0(R1)                                                         
*                                                                               
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'80',TAESCOM)                             
         BE    PRESCID5                                                         
PRESCID2 BAS   RE,NEXTEL5          AND IT IS NOT ON FILE                        
         BNE   PRESCIDL                                                         
         CLI   0(R4),TAESELQ                                                    
         BNE   *+12                                                             
         TM    TAESTYPE-TAESD(R4),TAESTPER  SKIP IT & ANY PERF ELS W/IT         
         BO    PRESCID2                                                         
         B     PRESCIDN                                                         
*                                                                               
PRESCID5 BAS   RE,DOTACO           PROCESS EL=CO ELEMENT                        
*                                                                               
         BAS   RE,DOTAUH           PROCESS EL=UH ELEMENT(S)                     
*                                                                               
         GOTO1 AGETNTRY,DMCB,(RC),(0,(R4))                                      
         L     R5,AELENTRY         IF TABLE ENTRY FOR ELEMENT FOUND             
         LTR   R5,R5                                                            
         BZ    PRESCID8                                                         
         USING MHELD,R5            R5=A(ELEMENT ENTRY)                          
         GOTO1 ADOSETEL,DMCB,(RC)  CALL SETELEM TO ESTABLISH ELE HDR            
         GOTO1 APROCEL,DMCB,(RC)   PROCESS EL=ES (TAESTCOM) ELEMENT             
PRESCID8 MVI   ELCODE,0            RESET ELEMENT CODE                           
         B     PRESCIDY                                                         
*                                                                               
PRESCIDL LNR   RC,RC               RETURN CC LOW                                
         B     PRESCIDN                                                         
PRESCIDY XR    RC,RC               RETURN CC EQUAL                              
PRESCIDN LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1  REGS=(R4)           EXIT WITH R4 SET                             
*                                                                               
PRESCIDX XIT1                      NORMAL EXIT                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY EL=CO ELEMENT                                 
         SPACE 1                                                                
DOTACO   NTR1                                                                   
         MVC   AIO,AWRKIO          SET WORKING IOAREA                           
         GOTO1 GETREC              GET COMMERCIAL RECORD JUST READ              
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL5                                                        
         BNE   DOTACOX                                                          
*                                                                               
         GOTO1 AGETNTRY,DMCB,(RC),(0,(R4))                                      
         L     R5,AELENTRY         IF TABLE ENTRY FOR ELEMENT FOUND             
         LTR   R5,R5                                                            
         BZ    DOTACOX                                                          
         USING MHELD,R5            R5=A(ELEMENT ENTRY)                          
*                                                                               
         GOTO1 ADOSETEL,DMCB,(RC)  CALL SETELEM TO ESTABLISH ELE HDR            
*                                                                               
         GOTO1 APROCEL,DMCB,(RC)   PROCESS RECORD                               
*                                                                               
DOTACOX  MVC   ELCODE,SVELCODE                                                  
         MVC   AIO,SVAIO                                                        
         B     PRESCIDX                                                         
         EJECT                                                                  
*              ROUTINE DISPLAYS EL=UH ELEMENTS FOR EACH ESTIMATED CLA           
*              OR UPGRADED USE ON ACTUAL COMMERCIAL ESTIMATE ELEMENT            
*              AND ITS RELATED PERFORMER ELEMENTS                               
         SPACE 1                                                                
         USING TAESD,R4                                                         
DOTAUH   NTR1                                                                   
*                                                                               
DOTAUH1  ZIC   R0,TAESSBNO         R0=N'OPTIONS                                 
         LTR   R0,R0                                                            
         BZ    DOTAUH10                                                         
         LA    R5,TAESSBCD         R5=A(FIRST SUB-ELEMENT)                      
         USING TAESSBCD,R5                                                      
         SPACE 1                                                                
DOTAUH2  CLI   TAESSBCD,OPUSE      IF USE SUB-ELEMENT                           
         BNE   DOTAUH8                                                          
         GOTO1 USEVAL,DMCB,(X'80',TAESSBDT),TAESSBDT+1                          
         TM    TGUSSTA3,NWKUSE     IF NETWORK USE OR UPGRADED USE               
         BO    DOTAUH4                                                          
         TM    TGUSTYST,UPGRADE                                                 
         BO    DOTAUH4                                                          
         CLI   TGUSEQU,UFGR        OR FOREIGN TV                                
         BNE   DOTAUH8                                                          
         TM    TGMEEQU,LIKETV                                                   
         BZ    DOTAUH8                                                          
*                                                                               
DOTAUH4  AH    R5,=AL2(LOPUSE+1)   BUMP TO NEXT SUB-ENTRY                       
         BCT   R0,*+8              DECREMENT COUNT                              
         B     DOTAUH10                                                         
         BAS   RE,LOOKUSE          LOOK FOR ACTUAL CLA OR BASE USAGE            
*                                                                               
DOTAUH8  BAS   RE,LOOKUP2          RETURNS R1=L'USE SUB-ENTRY                   
         LA    R5,1(R1,R5)                                                      
         BCT   R0,DOTAUH2          CHECK NEXT SUB-ELEMENT                       
*                                                                               
DOTAUH10 MVI   ELCODE,0                                                         
         BAS   RE,NEXTEL5          ALSO PROCESS RELATED PERFORMER ELES.         
         BNE   PRESCIDX                                                         
         CLI   0(R4),TAESELQ                                                    
         BNE   PRESCIDX                                                         
         TM    TAESTYPE,TAESTPER                                                
         BO    DOTAUH1                                                          
         B     PRESCIDX                                                         
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE GETS OTHER NECESSARY INFO ESTIMATED ABOUT USE            
*                                  R5=A(SUB-ENTRY IN ELEMENT)                   
LOOKUSE  NTR1                                                                   
         XC    FULL,FULL           CLEAR DATE                                   
*                                                                               
LOOKUSE2 BAS   RE,LOOKUP           LOOK UP SUB ENTRY IN TABLE                   
         USING MDELD,R4            RETURNS R4=A(TABLE ENTRY)                    
         TM    MDUSTAT,MDUSE       MUST BE USE - RELATED                        
         BZ    LOOKUSE8                                                         
         CLI   MDUCODE,OPUSE       STOP IF WE'VE REACHED NEXT USE TYPE          
         BE    LOOKUSE8                                                         
*                                                                               
         CLI   MDUCODE,OPDATE      IF DATE SPECIFIED                            
         BE    LOOKUSE4                                                         
         CLI   MDUCODE,OPCYC                                                    
         BE    LOOKUSE4                                                         
         CLI   MDUCODE,OPASOF                                                   
         BNE   *+10                                                             
LOOKUSE4 MVC   FULL(3),TAESSBDT    SAVE START DATE                              
*                                                                               
         CLI   MDUCODE,LOPUSES2    SPECIFIC USES FOR CLA SPECIFIED              
         BNE   LOOKUSE6                                                         
         XR    RE,RE                                                            
         ICM   RE,3,TAESSBDT                                                    
         CH    RE,=H'1'            AND NOT ONE                                  
         BE    PRESCIDX                                                         
*                                                                               
LOOKUSE6 BAS   RE,LOOKUP2          RETURNS R1=L'USE SUB-ENTRY                   
         LA    R5,1(R1,R5)                                                      
         BCT   R0,LOOKUSE2         CHECK NEXT SUB-ELEMENT                       
*                                                                               
LOOKUSE8 OC    FULL(3),FULL        IF DATE DEFINED                              
         BZ    *+8                                                              
         BAS   RE,RDTLUH           READ USAGE HISTORY                           
         B     PRESCIDX                                                         
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE BUILDS EL=UH ELEMENT IF IT FINDS ACTUAL USAGE            
*              FOR SAME CYCLE AS ESTIMATED USE                                  
*                                  TGCOM AND TGUSCDE SET                        
RDTLUH   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLUHD,R4                                                         
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         MVC   TLUHUSE,TGUSCDE                                                  
         GOTO1 HIGH                                                             
         B     RDTLUH5                                                          
*                                                                               
RDTLUH2  GOTO1 SEQ                                                              
*                                                                               
RDTLUH5  OC    KEY+TLUHCSEQ-TLUHD(L'TLUHCSEQ),KEY+TLUHCSEQ-TLUHD                
         BNZ   RDTLUHX                                                          
         CLC   KEY(TLUHINV-TLUHD),KEYSAVE                                       
         BNE   RDTLUHX                                                          
*                                                                               
         MVC   AIO,AWRKIO                                                       
         GOTO1 GETREC                                                           
         L     R4,AWRKIO                                                        
         MVI   ELCODE,TAUHELQ                                                   
         BAS   RE,GETEL5                                                        
         BNE   RDTLUH2                                                          
         USING TAUHD,R4                                                         
         CLC   FULL(3),TAUHSTRT                                                 
         BL    RDTLUH2                                                          
         CLC   FULL(3),TAUHEND                                                  
         BH    RDTLUH2                                                          
*                                  PROCESS UH ELE UNLESS ALREADY SHOWN          
         LA    R0,MAXUH                                                         
         L     RE,AMISCTAB                                                      
RDTLUH8  OC    0(4,RE),0(RE)                                                    
         BZ    RDTLUH10                                                         
         CLC   0(4,RE),KEY+TLDRDA-TLDRD                                         
         BE    RDTLUHX                                                          
         LA    RE,4(RE)                                                         
         BCT   R0,RDTLUH8                                                       
         DC    H'0'                NEED TO MAKE TEMP. TABLE BIGGER              
*                                                                               
RDTLUH10 MVC   0(4,RE),KEY+TLDRDA-TLDRD                                         
         GOTO1 AGETNTRY,DMCB,(RC),(0,(R4))                                      
         L     R5,AELENTRY         IF TABLE ENTRY FOR ELEMENT FOUND             
         LTR   R5,R5                                                            
         BZ    RDTLUHX                                                          
         USING MHELD,R5            R5=A(ELEMENT ENTRY)                          
         GOTO1 ADOSETEL,DMCB,(RC)  CALL SETELEM TO ESTABLISH ELE HDR            
         GOTO1 APROCEL,DMCB,(RC)   PROCESS RECORD (RESETS AIO)                  
         CLI   TGUSEQU,UFGR        IF FOREIGN TV                                
         BE    RDTLUH2             PASS ALL HISTORY USAGE FALLS IN              
*                                                                               
RDTLUHX  MVC   AIO,SVAIO                                                        
         L     RE,AMISCTAB                                                      
         LH    RF,=Y(MAXUH*4)                                                   
         XCEF                                                                   
         B     PRESCIDX                                                         
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINES LOOK UP SUB-ELEMENT CODE IN TABLE AND                   
*              PASSES BACK EITHER LENGTH OR TABLE ENTRY                         
         SPACE 1                                                                
         USING TAESSBCD,R5         R5=A(SUB-ELEMENT)                            
LOOKUP   NTR1                      LOOK UP SUB ENTRY IN TABLE                   
         LA    RF,1                PASS BACK A(TABLE ENTRY) IN R4               
         B     LOOK5                                                            
         SPACE 1                                                                
LOOKUP2  NTR1                                                                   
         XR    RF,RF               PASS BACK L'USE IN SUB-ELEMENT IN R1         
*                                                                               
LOOK5    L     RE,AOPTS            RE=A(OPTIONS)                                
         USING MDELD,RE                                                         
         SPACE 1                                                                
LOOK10   CLI   MDLEN,0             IF NOT END OF OPTIONS                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MDUCODE,TAESSBCD                                                 
         BE    LOOK12                                                           
         ZIC   R1,MDLEN                                                         
         AR    RE,R1                                                            
         B     LOOK10                                                           
*                                                                               
LOOK12   LTR   RF,RF               IF ONLY WANT LENGTH                          
         BNZ   LOOK15                                                           
         ZIC   R1,MDULEN           RETURN R1=(LENGTH OF SUB-ELEMENT)            
         CLI   TAESSBCD,OPCOMNM    IF THIS IS COMML NAME                        
         BNE   *+8                                                              
         IC    R1,TAESSBDT         LENGTH IS IN FIRST DATA BYTE                 
         XIT1  REGS=(R1)                                                        
*                                                                               
LOOK15   LR    R4,RE               RETURN R4=A(TABLE ENTRY)                     
         XIT1  REGS=(R4)                                                        
         DROP  RE,R5                                                            
         SPACE 2                                                                
         GETELN (R4),DATADISP,ELCODE,5                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE SETS CORRECT ENTRY ADDRESS OF ELEMENT                    
*                                  NTRY P2 BYTE 0 X'80' MATCH ON CODE           
*                                       P2 A(ELEMENT)                           
*                                       R4=A(ELEMENT)                           
*                                  XIT - AELENTRY SET                           
         SPACE 1                                                                
         DS    0D                                                               
GETNTRY  NMOD1 0,*GETNTRY                                                       
         L     RC,0(R1)                                                         
         XC    AELENTRY,AELENTRY                                                
*                                                                               
         BAS   RE,SETETYPE         SET ELETYPE                                  
         BNE   GETNTRYX                                                         
*                                                                               
         MVC   BYTE,4(R1)                                                       
         L     R4,4(R1)                                                         
*                                                                               
         L     R5,AMAPTAB          R5=A(ELEMENT TABLE)                          
         USING MHELD,R5                                                         
*                                                                               
GETNTRY1 CLI   MHLEN,0             TEST END OF TABLE                            
         BE    GETNTRYX                                                         
*                                                                               
         TM    BYTE,X'80'          IF WANT MATCH ON CODE                        
         BZ    GETNTRY2                                                         
         CLC   MHUTEXT,0(R4)       MATCH ON TWO CHARACTER CODE                  
         BNE   GETNTRY5                                                         
         B     *+14                                                             
*                                                                               
GETNTRY2 CLC   MHCODE+1(1),0(R4)   ELSE, MATCH ON ELEMENT EQUATE                
         BNE   GETNTRY5                                                         
*                                                                               
         CLI   MHUSTAT,0           MATCH ON SPECIFIC RECORD                     
         BE    GETNTRY8                                                         
*                                                                               
         TM    MHUSTAT,MHUSESL     IF FOR EST LIST OR EST RECORD                
         BO    *+12                                                             
         TM    MHUSTAT,MHUSES                                                   
         BZ    GETNTRY3                                                         
         CLI   MYKCODEQ,TLESCDQ    MUST HAVE READ ESTIMATE RECORD               
         BNE   GETNTRY3                                                         
         L     RE,AIO                                                           
         CLI   0(RE),TLESCDQ       AND ESTIMATE IN IOAREA                       
         BE    GETNTRY8                                                         
*                                                                               
GETNTRY3 TM    MHUSTAT,MHUSCP                                                   
         BZ    *+12                                                             
         CLI   MYKCODEQ,TLCLCDQ                                                 
         BE    GETNTRY8                                                         
*                                                                               
         TM    MHUSTAT,MHUSINI                                                  
         BZ    *+12                                                             
         TM    STESTAT,STETAB                                                   
         BO    GETNTRY8                                                         
*                                                                               
         TM    MHUSTAT,MHUSXGU     IF EXCLUDING FROM GUL                        
         BZ    GETNTRY5                                                         
         CLC   =C'GUL',MYKCODE                                                  
         BE    GETNTRY5                                                         
         B     GETNTRY8                                                         
*                                                                               
GETNTRY5 XR    RF,RF               BUMP TO NEXT ELEMENT ENTRY IN TAB            
         ICM   RF,3,MHDISP         DISP. TO NEXT ELEMENT HDR ENTRY              
         AR    R5,RF                                                            
         B     GETNTRY1                                                         
*                                                                               
GETNTRY8 ST    R5,AELENTRY         SAVE A(ELEMENT ENTRY)                        
GETNTRYX XIT1                                                                   
         SPACE 2                                                                
GETNTRYY XR    RC,RC               RETURN CC EQUAL                              
GETNTRYN LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     GETNTRYX                                                         
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO SET ELETYPE FOR TABLE TRAVERSING                      
         SPACE 1                                                                
SETETYPE NTR1                                                                   
         MVI   ELETYPE,0           PRE-CLEAR ELEMENT TYPE                       
*                                                                               
         L     RE,AIO                                                           
         CLI   0(RE),TLESCDQ       IF ESTIMATE RECORD                           
         BNE   SETETYP5                                                         
         CLI   0(R4),TAESELQ       IF ESTIMATE ELEMENT                          
         BNE   GETNTRYY                                                         
         USING TAESD,R4                                                         
         CLI   TAESTYPE,TAESTCLI                                                
         BNE   *+12                                                             
         MVI   ELETYPE,MDESCL                                                   
         B     GETNTRYY                                                         
         CLI   TAESTYPE,TAESTPRD                                                
         BNE   *+12                                                             
         MVI   ELETYPE,MDESPR                                                   
         B     GETNTRYY                                                         
         CLI   TAESTYPE,TAESTCOM                                                
         BNE   *+12                                                             
         MVI   ELETYPE,MDESCO                                                   
         B     GETNTRYY                                                         
         CLI   TAESTYPE,TAESTPER                                                
         BNE   *+12                                                             
         MVI   ELETYPE,MDESPER                                                  
         B     GETNTRYY                                                         
         CLI   TAESTYPE,TAESTHCO                                                
         BNE   *+12                                                             
         MVI   ELETYPE,MDESHCO                                                  
         B     GETNTRYY                                                         
         CLI   TAESTYPE,TAESTHPE                                                
         BNE   GETNTRYN                                                         
         MVI   ELETYPE,MDESHP                                                   
         B     GETNTRYY                                                         
*                                                                               
SETETYP5 CLI   0(RE),TLUHCDQ       IF USAGE HISTORY RECORD                      
         BNE   SETETYP8                                                         
         BAS   RE,SETUHTYP                                                      
         B     GETNTRYY                                                         
*                                                                               
SETETYP8 CLI   0(RE),TLCOCDQ       IF COMMERCIAL RECORD                         
         BNE   SETETYP9                                                         
         CLI   0(R4),TACOELQ       AND COMMERCIAL ELEMENT                       
         BNE   GETNTRYY                                                         
         CLC   =C'ES',MYKCODE      AND ES OR CO READ                            
         BE    *+14                                                             
         CLC   =C'CO',MYKCODE                                                   
         BNE   GETNTRYY                                                         
         MVI   ELETYPE,MAPFCO      USE TACOD FULL TACOD                         
         B     GETNTRYY                                                         
*                                                                               
SETETYP9 CLI   0(RE),TLAYCDQ       IF AGENCY RECORD                             
         BNE   GETNTRYY                                                         
         CLC   =C'INI',MYKCODE     AND FROM INITIALIZATION                      
         BNE   GETNTRYY                                                         
         MVI   ELETYPE,C'A'                                                     
         B     GETNTRYY                                                         
         EJECT                                                                  
*              ROUTINE TO SET ELETYPE FOR TAUHD ELEMENT                         
         SPACE 1                                                                
SETUHTYP NTR1                                                                   
         TM    TGUSSTA3,NWKUSE                                                  
         BZ    *+12                                                             
         MVI   ELETYPE,MDTUHA                                                   
         B     GETNTRYY                                                         
*                                                                               
         CLI   TGUSEQU,UDEM                                                     
         BE    *+12                                                             
         CLI   TGUSEQU,UCDM                                                     
         BNE   *+12                                                             
         MVI   ELETYPE,MDTUHB                                                   
         B     GETNTRYY                                                         
*                                                                               
         CLI   TGUSEQU,UTAG                                                     
         BNE   *+12                                                             
         MVI   ELETYPE,MDTUHC                                                   
         B     GETNTRYY                                                         
*                                                                               
         CLI   TGUSEQU,URRN                                                     
         BNE   *+12                                                             
         MVI   ELETYPE,MDTUHD                                                   
         B     GETNTRYY                                                         
*                                                                               
*        CLI   TGUSEQU,UIFB                                                     
*        BNE   *+12                                                             
*        MVI   ELETYPE,MDUHE                                                    
*        B     GETNTRYY                                                         
*                                                                               
         CLI   TGUSEQU,UFGR                                                     
         BNE   *+12                                                             
         MVI   ELETYPE,MDUHF                                                    
         B     GETNTRYY                                                         
*                                                                               
         CLI   TGUSEQU,UCBL                                                     
         BE    *+12                                                             
         CLI   TGUSEQU,USCB                                                     
         BNE   *+12                                                             
         MVI   ELETYPE,MDUHG                                                    
         B     GETNTRYY                                                         
*                                                                               
         CLI   TGUSEQU,USNW                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UWSP                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UWSC                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UWSM                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UNET                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UADC                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UADW                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,USWS                                                     
         BNE   GETNTRYY                                                         
SETUHTY5 MVI   ELETYPE,MDUHI                                                    
         B     GETNTRYY                                                         
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD NECESSARY INFO TO MISCTAB                         
*                                                                               
*                                  R4=A(RECORD)                                 
         DS    0D                                                               
ADDTAB   NMOD1 0,*ADDTAB*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LTR   R4,R4               IF NO RECORD                                 
         BNZ   ADDTAB5                                                          
*                                                                               
         LA    R0,MAXTBL                                                        
         L     R2,AMISCTAB         R2=A(TABLE)                                  
         BAS   RE,ADDCTYP          ADD COMMERCIAL TYPES AS ELS                  
         BNE   ADDTABX                                                          
         BAS   RE,ADDADEM          ADD ADDENDUM STATES AS ELS                   
         BNE   ADDTABX                                                          
         BAS   RE,ADDCCTYP         ADD CANADIAN COMMERCIAL TYPES AS ELS         
         B     ADDTABX             CC SET                                       
*                                                                               
ADDTAB5  CLI   0(R4),TLCACDQ       IF CAST RECORD                               
         BNE   ADDTABX                                                          
         BAS   RE,ADDMUS           ADD MUSICIANS AS COMBINED ELS.               
ADDTABX  XIT1                      CC SET                                       
         SPACE 2                                                                
*                                                                               
ADDTBR2Y XR    RC,RC               RETURN CC EQUAL WITH R2 SET                  
ADDTBR2N LTR   RC,RC                                                            
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              ROUTINE ADDS DUMMY ELEMENT FOR COMMERCIAL TYPES                  
         SPACE 1                                                                
*                                  R2=A(FIRST AVAIL. ENTRY IN MISCTAB)          
ADDCTYP  NTR1                                                                   
         L     R1,TGACOMT          R1=A(COMMERCIAL TYPE TABLE)                  
         USING CTYD,R1                                                          
         LA    R4,ELEMENT                                                       
         USING TA02D,R4                                                         
*                                                                               
ADDCTYP2 XC    ELEMENT,ELEMENT     BUILD DUMMY ELEMENT                          
         MVI   TA02EL,TA02ELQ                                                   
         MVI   TA02LEN,TA02LNQ                                                  
*                                                                               
         CLI   0(R1),X'FF'         IF NOT END OF TABLE                          
         BE    ADDCTYPX                                                         
         MVC   TA02CTYP,CTYEQU     SET CODE IN ELEMENT                          
         MVC   TA02CTYN,CTYNAME    SET NAME IN ELEMENT                          
         BAS   RE,SETAHLD          SET TA02AHLD                                 
         MVC   0(TA02LNQ,R2),ELEMENT THEN SET ELEMENT IN TABLE                  
         LA    R2,TA02LNQ(R2)                                                   
         BCT   R0,*+8                                                           
         B     ADDTBR2N            TABLE FULL                                   
         LA    R1,CTYNEXT          PT TO NEXT ENTRY IN TABLE                    
         B     ADDCTYP2                                                         
*                                                                               
ADDCTYPX B     ADDTBR2Y            EXIT WITH R2 AT TABLE END                    
         DROP  R1                                                               
         SPACE 2                                                                
*              ROUTINE SETS Y IN ELEMENT IF COMML TYPE GENERATES                
*              AUTO HOLDING FEES                                                
         SPACE 1                                                                
         USING TA02D,R4                                                         
SETAHLD  NTR1                                                                   
         LA    RE,NAHLDTAB         RE=A(COMML'S DON'T GET HOLDING FEES)         
SETAHLD5 CLI   0(RE),0             IF COMMERCIAL TYPE NOT IN TABLE              
         BE    SETAHLD8                                                         
         CLC   TA02CTYP,0(RE)                                                   
         BE    ADDTABX                                                          
         LA    RE,1(RE)                                                         
         B     SETAHLD5                                                         
*                                                                               
SETAHLD8 MVI   TA02AHLD,C'Y'       SET Y IN ELEMENT( GETS AUTO HLDS)            
         B     ADDTABX                                                          
         EJECT                                                                  
*              ROUTINE ADDS DUMMY ELEMENT FOR ADDENDUM STATES                   
         SPACE 1                                                                
*                                  R2=A(FIRST AVAIL. ENTRY IN MISCTAB)          
ADDADEM  NTR1                                                                   
         L     R1,TGAUNITS         R1=A(UNITS TABLE)                            
         USING TALUNITD,R1                                                      
*                                                                               
         XC    ELEMENT,ELEMENT     BUILD DUMMY ELEMENT                          
         LA    R4,ELEMENT                                                       
         USING TA03D,R4                                                         
         MVI   TA03EL,TA03ELQ                                                   
         MVI   TA03LEN,TA03LNQ                                                  
*                                                                               
ADDADEM2 CLI   0(R1),X'FF'         IF NOT END OF TABLE                          
         BE    ADDTBR2Y                                                         
         TM    TALUSTAT,TALUOKAD                                                
         BZ    ADDADEM4                                                         
         MVC   TA03ADST,TALUCODE   SET STATE CODE IN ELEMENT                    
         MVC   TA03ADEM,TALUNAME   SET STATE NAME IN ELEMENT                    
         MVC   0(TA03LNQ,R2),ELEMENT THEN SET ELEMENT IN TABLE                  
         LA    R2,TA03LNQ(R2)                                                   
         BCT   R0,*+8                                                           
         B     ADDTBR2N            TABLE FULL                                   
*                                                                               
ADDADEM4 LA    R1,TALUNEXT         PT TO NEXT ENTRY IN TABLE                    
         B     ADDADEM2                                                         
         DROP  R1                                                               
         EJECT                                                                  
*              ROUTINE ADDS DUMMY ELEMENT FOR COMMERCIAL TYPES                  
         SPACE 1                                                                
*                                  R2=A(FIRST AVAIL. ENTRY IN MISCTAB)          
ADDCCTYP NTR1                                                                   
         L     R1,TGACTYPS         R1=A(CANADIAN COMML TYPE TABLE)              
         USING CCTYPD,R1                                                        
*                                                                               
         XC    ELEMENT,ELEMENT     BUILD DUMMY ELEMENT                          
         LA    R4,ELEMENT                                                       
         USING TA04D,R4                                                         
         MVI   TA04EL,TA04ELQ                                                   
         MVI   TA04LEN,TA04LNQ                                                  
*                                                                               
ADDCCTY2 CLI   0(R1),X'FF'         IF NOT END OF TABLE                          
         BE    ADDTBR2Y                                                         
         MVC   TA04CCDE,CCTYPCDE   SET CODE IN ELEMENT                          
         MVC   TA04CCN,CCTYPNME    SET NAME IN ELEMENT                          
         MVC   0(TA04LNQ,R2),ELEMENT THEN SET ELEMENT IN TABLE                  
         LA    R2,TA04LNQ(R2)                                                   
         BCT   R0,*+8                                                           
         B     ADDTBR2N            TABLE FULL                                   
*                                                                               
         LA    R1,CCTYPNXT         PT TO NEXT ENTRY IN TABLE                    
         B     ADDCCTY2                                                         
         DROP  R1                                                               
         EJECT                                                                  
*              TABLE CONSISTS OF COMBINED MUSICIANS                             
         SPACE 1                                                                
         USING TLCAD,R4            R4=A(CAST RECORD)                            
ADDMUS   NTR1                                                                   
*                                  GET OV1% FOR ALL USE INTO FULL               
         GOTO1 GETOV1,DMCB,(X'80',SPACES),FULL                                  
         BAS   RE,ADJCNT           RTNS HALF W/ADJUSTED COUNT FOR CAT           
*                                                                               
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL2                                                        
         BNE   ADDTBR2Y                                                         
         USING TACAD,R4                                                         
         GOTO1 SETOV2,DMCB,(R4),AIO,SPACES                                      
*                                                                               
         CLC   =C'AFM',TACAUN      IF UNION IS AFM                              
         BNE   *+10                                                             
         XC    TACAUN,TACAUN       DEFAULT IT                                   
         CLI   TACACORP,C' '       IF CORP                                      
         BNH   *+8                                                              
         MVI   TACACORP,C'Y'       LUMP THEM TOGETHER                           
*                                                                               
         LA    R0,MAXTBL           R0=MAX N'ENTRIES                             
         L     R2,AMISCTAB         R2=A(TABLE)                                  
         USING TA01D,R2                                                         
*                                                                               
ADDMUS2  OC    0(TA01LNQ,R2),0(R2) TEST END OF TABLE                            
         BZ    ADDMUS10                                                         
         BAS   RE,CKMUSDUP         TEST DUPLICATE MUSICIAN                      
         BE    ADDMUS20                                                         
         LA    R2,TA01LNQ(R2)      BUMP TO NEXT ENTRY                           
         BCT   R0,ADDMUS2                                                       
         B     ADDTBR2N            INCREASE MAXTBL                              
*                                                                               
ADDMUS10 MVI   TA01EL,TA01ELQ      ADD NEW MUSICIAN ENTRY                       
         MVI   TA01LEN,TA01LNQ                                                  
         MVC   TA01CAM,TACAONOF                                                 
         MVC   TA01UN,TACAUN                                                    
         MVC   TA01CORP,TACACORP                                                
         MVC   TA01DBL,TACADBL                                                  
         MVC   TA01OV1,FULL                                                     
         MVC   TA01OV2,TACAOV2                                                  
*                                                                               
ADDMUS20 XR    R1,R1               INCREMENT COUNT FOR ROW                      
         ICM   R1,3,TA01CNT                                                     
         AH    R1,HALF                                                          
         STCM  R1,3,TA01CNT                                                     
         B     ADDTBR2Y                                                         
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK CURRENT MUSICIAN DETAILS                        
*              MATCH ENTRY IN THE TABLE                                         
         SPACE 1                                                                
         USING TA01D,R2            R2=A(MUSICIAN TABLE ENTRY)                   
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
CKMUSDUP NTR1                                                                   
         CLC   TACAONOF,TA01CAM    CHECK MATCH ON CAMERA                        
         BNE   ADDTABX                                                          
         CLC   TACADBL,TA01DBL     CHECK MATCH ON DOUBLES                       
         BNE   ADDTABX                                                          
         CLC   TACACORP,TA01CORP   CHECK MATCH ON CORP                          
         BNE   ADDTABX                                                          
         CLC   TACAUN,TA01UN       CHECK MATCH ON UNION                         
         BNE   ADDTABX                                                          
         CLC   TACAOV2,TA01OV2     CHECK MATCH ON OV2                           
         BNE   ADDTABX                                                          
         CLC   FULL,TA01OV1        CHECK MATCH ON OV1                           
         B     ADDTABX                                                          
         DROP  R2,R4                                                            
         SPACE 2                                                                
*        ADJUST MUSICIAN CATEGORY COUNT - RETURNS HALF                          
         SPACE 1                                                                
ADJCNT   NTR1                                                                   
         XR    R1,R1                                                            
         LA    R2,AFMTAB           R2=A(ADJUSTED COUNTS FOR MUSICIANS)          
*                                                                               
ADJCNT2  CLI   0(R2),X'FF'         TEST END OF TABLE                            
         BE    ADJCNT8                                                          
         CLC   TGCAEQU,2(R2)       TEST MATCH ON CATEGORY EQUATE                
         BE    *+12                                                             
         LA    R2,L'AFMTAB(R2)                                                  
         B     ADJCNT2                                                          
*                                                                               
         XR    R0,R0               CALCULATE ADJUSTED COUNT                     
         LH    R1,0(R2)                                                         
         LTR   R1,R1                                                            
         BZ    ADJCNT8                                                          
         D     R0,=F'100'                                                       
*                                                                               
ADJCNT8  LA    R1,1(R1)                                                         
         STH   R1,HALF             RETURN IN HALF                               
         B     ADDTABX                                                          
         EJECT                                                                  
         GETEL2 (R4),DATADISP,ELCODE                                            
         SPACE 2                                                                
       ++INCLUDE TASYSAFM                                                       
         SPACE 2                                                                
       ++INCLUDE TASYSNAHLD                                                     
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD ELEMENT TO ESTIMATE IOAREA                        
         SPACE 1                                                                
         DS    0D                                                               
MYADDL   NMOD1 0,*MYADDL*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         OC    ELEMENT,ELEMENT     IF FIRST TIME                                
         BZ    MYADDLY             EXIT                                         
         LA    R4,ELEMENT                                                       
         CLI   0(R4),TACMELQ       IF COMMENT ELEMENT                           
         BNE   MYADDL1                                                          
         USING TACMD,R4                                                         
         MVI   TACMTYPE,TACMTYPG   SET TYPE                                     
         B     MYADDL9                                                          
*                                                                               
MYADDL1  CLI   0(R4),TANUELQ       IF ESTIMATE REVISION NUMBER ELEMENT          
         BNE   MYADDL2                                                          
         USING TANUD,R4                                                         
         MVI   TANUTYPE,TANUTREV   SET TYPE IN ELEMENT                          
         B     MYADDL9                                                          
*                                                                               
MYADDL2  CLI   0(R4),TAENELQ       IF ESTIMATE NARRATIVE ELEMENT                
         BNE   MYADDL3                                                          
         USING TAEND,R4                                                         
         MVC   TAENSEQ,ENSEQ                                                    
         ZIC   R1,ENSEQ                                                         
         LA    R1,1(R1)                                                         
         STC   R1,ENSEQ                                                         
         B     MYADDL9                                                          
*                                                                               
MYADDL3  CLI   0(R4),TAEMELQ       IF ESTIMATE MISCELLANOUS ADDITIONS           
         BNE   MYADDL4                                                          
         USING TAEMD,R4                                                         
         MVC   TAEMSEQ,EMSEQ                                                    
         ZIC   R1,EMSEQ                                                         
         LA    R1,1(R1)                                                         
         STC   R1,EMSEQ                                                         
         B     MYADDL9                                                          
*                                                                               
MYADDL4  CLI   0(R4),TAESELQ       IF ESTIMATE DETAILS ELEMENT                  
         BNE   MYADDL9                                                          
         BAS   RE,MYADDES          SPECIAL HANDLING NEEDED                      
         BNE   MYADDLER                                                         
*                                  ADD ELEMENT TO RECORD                        
MYADDL9  GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,(R4),0                              
         CLI   12(R1),5                                                         
         BE    MYADDL15                                                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
MYADDL15 L     R4,AIO                                                           
         USING TLESD,R4                                                         
         LH    RE,=H'5700'         CHECK REACHED MAX LENGTH                     
         CLI   HAVEAC40,C'Y'                                                    
         BNE   *+8                                                              
         AH    RE,=AL2(TAACLNQ)                                                 
         CH    RE,TLESLEN                                                       
         BNL   MYADDLY                                                          
*                                                                               
MYADDLER LA    R2,FAMSGBLK                                                      
         USING FAMSGD,R2                                                        
         CLC   FAMSGNO,=X'11F'                                                  
         BE    *+10                                                             
         MVC   FAMSGNO,=AL2(TOOLONG)                                            
         CLC   FAMSGNO,=AL2(60)    IF NOT HIGHER THAN 60                        
         BH    *+8                                                              
         MVI   FAMSGSYS,X'FF'      SET OVERRIDE TO GENERAL SYSTEM               
         B     MYADDLL                                                          
         SPACE 1                                                                
MYADDLL  LNR   RC,RC               RETURN CC NOW                                
         B     *+6                                                              
MYADDLY  XR    RC,RC               RETURN CC EQUAL                              
         LTR   RC,RC               RETURN CC NOT EQUAL                          
MYADDLX  XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO FIGURE OUT CORRECT SEQUENCE NUMBER TO                 
*              ADD ELEMENT WITH, SO ELEMENTS ARE IN SORTED ORDER                
         SPACE 1                                                                
         USING TAESD,R4                                                         
MYADDES  NTR1                                                                   
         ZIC   RE,ESOPTCNT         SET NUMBER OF SUB-OPTIONS                    
         STC   RE,TAESSBNO                                                      
         MVI   ESOPTCNT,0          AND CLEAR IT                                 
*                                                                               
         CLI   TAESTYPE,TAESTCLI   IF CLIENT ELEMENT                            
         BNE   *+12                                                             
         BAS   RE,MYADDCL          FIND SEQUENCE NUMBER                         
         B     MYADDLX             CC SET                                       
*                                                                               
         CLI   TAESTYPE,TAESTPRD   IF PRODUCT ELEMENT                           
         BNE   *+12                                                             
         BAS   RE,MYADDPR          FIND SEQ # UNDER CLIENT                      
         B     MYADDLX             CC SET                                       
*                                                                               
         TM    TAESTYPE,TAESTCOM   IF HYPO OR REGULAR COMMERCIAL                
         BZ    MYADDES1                                                         
         BAS   RE,TRAPDUP          TRAP DUPLICATE ESTIMATE INFO.                
         BNE   MYADDLL                                                          
         XC    HYPCNTS,HYPCNTS     CLEAR HYPO PERFORMER COUNTS                  
         BAS   RE,MYADDCO          FIND SEQUENCE # UNDER CLI/PRD                
         B     MYADDLX             CC SET                                       
*                                                                               
MYADDES1 TM    TAESTYPE,TAESTPER   IF CAST (HYPO OR REGULAR)                    
         BO    *+6                                                              
         DC    H'0'                                                             
         CLI   TAESTYPE,TAESTHPE   IF REGULAR PERFORMER                         
         BE    *+12                                                             
         BAS   RE,MYADDCA          FIND SEQUENCE # UNDER COMMERCIAL             
         B     MYADDLX             AND EXIT                                     
*                                                                               
         CLI   TAESHNPR,0          ELSE, IF N'PRFS OF THIS TYPE NOT SET         
         BNE   *+8                                                              
         MVI   TAESHNPR,1          SET TO ONE                                   
*                                                                               
         LA    R2,1                                                             
         GOTO1 AESTPRT,DMCB,(RC),(R4)                                           
         ZIC   R2,4(R1)            R2=N'LINES THIS HYPO PERFS OPTS TOOK         
*                                                                               
         ZIC   R1,HYPCNT           IF STILL UNDER PAGE MAX                      
         AR    R1,R2                                                            
         CLM   R1,1,=AL1(MAXHYP)                                                
         BH    MYADDES2                                                         
         MVC   TAESHPG,HPGCNT      SET CURRENT PAGE                             
         STC   R1,HYPCNT           SAVE NEW COUNT FOR PAGE                      
         BAS   RE,MYADDCA                                                       
         B     MYADDLX                                                          
*                                                                               
MYADDES2 ZIC   R1,HPGCNT           INCREMENT PAGE NUMBER                        
         LA    R1,1(R1)                                                         
         STC   R1,HPGCNT                                                        
         MVC   TAESHPG,HPGCNT      SET PERFORMER IN NEW PAGE                    
         STC   R2,HYPCNT           SET COUNT FOR PAGE                           
         BAS   RE,MYADDCA                                                       
         B     MYADDLX                                                          
         EJECT                                                                  
*              ROUTINE TO LOOP THROUGH RECORD TRYING TO FIND                    
*              APPROPRIATE SEQUENCE # FOR CLIENT ELEMENT                        
         SPACE 1                                                                
MYADDCL  NTR1                                                                   
         XR    R2,R2               INITIALIZE SEQUENCE NUMBER                   
         MVI   ANYMORE,C'Y'        MORE ELEMENTS TO COME                        
*                                                                               
         USING TAESD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAESELQ                                                   
         BAS   RE,GETEL3                                                        
         B     *+8                                                              
MYADDCL5 BAS   RE,NEXTEL3                                                       
         BE    *+12                                                             
         MVI   ANYMORE,C'N'        NO MORE ELEMENTS                             
         B     MYADDCL8                                                         
*                                                                               
         ZIC   R2,TAESSEQ          SET R2=(LATEST SEQUENCE NUMBER)              
         CLC   TAESTYPE,ELEMENT+TAESTYPE-TAESD                                  
         BNE   MYADDCL5                                                         
         CLC   TAESDATA,ELEMENT+TAESDATA-TAESD                                  
         BL    MYADDCL5                                                         
*                                                                               
         STC   R2,ELEMENT+TAESSEQ-TAESD USE LATEST SEQ # FOR THIS EL.           
         B     MYADDCLX                                                         
*                                                                               
MYADDCL8 LTR   R2,R2               SET SEQUENCE # IN CLIENT EL. TO ADD          
         BZ    *+8                                                              
         LA    R2,1(R2)                                                         
         STC   R2,ELEMENT+TAESSEQ-TAESD                                         
*                                                                               
MYADDCLX MVC   SVCLISEQ,ELEMENT+TAESSEQ-TAESD  SAVE LAST CLIENT SEQ #           
         XC    SVPRDSEQ(ESSEQSX-SVPRDSEQ),SVPRDSEQ  CLEAR SEQ'S BELOW           
         BAS   RE,DORESEQ               RESET SEQ #'S ON REMAINING ELS          
         B     MYADDLX                  CC SET                                  
         EJECT                                                                  
*              ROUTINE TO FIND SEQUENCE NUMBER FOR PRODUCT ELEMENT              
*              BY LOOPING THROUGH RECORD STARTING AT CLIENT ELEMENT             
         SPACE 1                                                                
MYADDPR  NTR1                                                                   
         MVI   ELCODE,TAESELQ      FIND LAST CLIENT ELEMENT PROCESSED           
         GOTO1 GETL,DMCB,(1,SVCLISEQ)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         ZIC   R2,SVCLISEQ                                                      
         MVI   ANYMORE,C'Y'        SET MORE ELEMENTS TO COME                    
*                                                                               
         USING TAESD,R4                                                         
MYADDPR5 BAS   RE,NEXTEL3          GET NEXT ELEMENT                             
         BE    *+16                                                             
         MVI   ANYMORE,C'N'        SET NO MORE ELEMENTS                         
         LA    R2,1(R2)            SET TO USE NEXT SEQUENCE NUMBER              
         B     MYADDPR8                                                         
*                                                                               
         ZIC   R2,TAESSEQ          SET R2=LATEST SEQUENCE NUMBER                
         CLI   TAESTYPE,TAESTCLI   IF NOT ANOTHER CLIENT EL.                    
         BE    MYADDPR8                                                         
         CLC   TAESTYPE,ELEMENT+TAESTYPE-TAESD  LOOP THROUGH FOR PRDS           
         BNE   MYADDPR5                                                         
         CLC   TAESDATA,ELEMENT+TAESDATA-TAESD  UNTIL FIND CORRECT SPOT         
         BL    MYADDPR5                                                         
*                                                                               
MYADDPR8 STC   R2,ELEMENT+TAESSEQ-TAESD USE LATEST SEQ # FOR THIS EL.           
         STC   R2,SVPRDSEQ              SAVE LAST PRD SEQ NUMBER                
         XC    SVCOSEQ(ESSEQSX-SVCOSEQ),SVCOSEQ CLEAR SEQ #'S BELOW             
         BAS   RE,DORESEQ               RESET SEQ #'S ON REMAINING ELS          
         B     MYADDLX                  CC SET                                  
         EJECT                                                                  
*              ROUTINE FIND TRAP DUPLICATE COMMERCIAL INFO IN HOPE              
*              OF FINDING BUG THAT DUPLICATES THE WHOLE ESTIMATE                
         SPACE 1                                                                
TRAPDUP  NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAESELQ                                                   
         BAS   RE,GETEL3                                                        
         B     *+8                                                              
TRAPDUP5 BAS   RE,NEXTEL3                                                       
         BNE   MYADDLY                                                          
         USING TAESD,R4                                                         
         CLC   TAESTYPE,ELEMENT+TAESTYPE-TAESD                                  
         BNE   TRAPDUP5                                                         
         CLC   TAESDATA,ELEMENT+TAESDATA-TAESD                                  
         BNE   TRAPDUP5                                                         
*                                  INSTEAD OF DYING, PUT IN MESSAGE             
         B     ERRABND             ADDING DUPLICATE INFORMATION                 
         EJECT                                                                  
*              ROUTINE FIND SEQUENCE NUMBER FOR ACTUAL OR HYPO COMML            
*              ELEMENT, BY LOOPING THROUGH RECORD STARTING AT LAST              
*              CLIENT AND/OR PRODUCT ELEMENT                                    
         SPACE 1                                                                
MYADDCO  NTR1                                                                   
         MVC   BYTE,SVCLISEQ       USE LAST CLIENT SEQUENCE NUMBER              
         CLI   SVPRDSEQ,0          IF PRODUCT SEQUENCE SPECIFIED                
         BE    *+10                                                             
         MVC   BYTE,SVPRDSEQ       USE LAST PRODUCT SEQUENCE NUMBER             
*                                                                               
         MVI   ELCODE,TAESELQ      FIND LAST CLIENT/PRODUCT ELEMENT             
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         ZIC   R2,BYTE                                                          
         MVI   ANYMORE,C'Y'        SET MORE ELEMENTS TO COME                    
*                                                                               
         USING TAESD,R4                                                         
MYADDCO5 BAS   RE,NEXTEL3          GET NEXT ELEMENT                             
         BE    *+16                                                             
         MVI   ANYMORE,C'N'        SET NO MORE ELEMENTS                         
         LA    R2,1(R2)            SET TO USE NEXT SEQUENCE NUMBER              
         B     MYADDCO8                                                         
*                                                                               
         ZIC   R2,TAESSEQ          SET R2=(LASTEST SEQUENCE NUMBER)             
         CLI   TAESTYPE,TAESTCLI   LOOP TILL HIT NEXT CLI/PRD EL.               
         BE    MYADDCO8                                                         
         CLI   TAESTYPE,TAESTPRD                                                
         BE    MYADDCO8                                                         
         CLI   ELEMENT+TAESTYPE-TAESD,TAESTHCO                                  
         BE    *+12                                                             
         CLI   TAESTYPE,TAESTHCO                                                
         BE    MYADDCO8                                                         
*                                                                               
         CLC   TAESTYPE,ELEMENT+TAESTYPE-TAESD LOOP THROUGH COMML               
         BNE   MYADDCO5                                                         
         CLC   TAESDATA,ELEMENT+TAESDATA-TAESD UNTIL FIND CORRECT SPOT          
         BL    MYADDCO5                                                         
*                                                                               
MYADDCO8 STC   R2,ELEMENT+TAESSEQ-TAESD USE LATEST SEQ # FOR THIS EL.           
         STC   R2,SVCOSEQ               SAVE LAST COMML SEQ NUMBER              
         MVI   SVCASEQ,0                CLEAR CAST SEQUENCE NUMBER              
         BAS   RE,DORESEQ               RESET SEQ #'S ON REMAINING ELS          
         B     MYADDLX                  CC SET                                  
         EJECT                                                                  
*              ROUTINE TO SET SEQUENCE NUMBER FOR ACTUAL OR HYPO                
*              PERFORMER EL BASED ON SEQUENCE NUMBER OF LAST COMML EL           
         SPACE 1                                                                
MYADDCA  NTR1                                                                   
         MVC   BYTE,SVCOSEQ        LAST COMML SEQUENCE NUMBER                   
         CLI   SVCASEQ,0           IF LAST CAST SEQUENCE SPECIFIED              
         BE    *+10                                                             
         MVC   BYTE,SVCASEQ        USE THAT INSTEAD                             
*                                                                               
         MVI   ELCODE,TAESELQ      FIND LAST COMML OR PERF. ELEMENT             
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         ZIC   R2,BYTE                                                          
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT SEQUENCE NUMBER                 
         STC   R2,SVCASEQ          SAVE NEW LAST CAST SEQUENCE NUMBER           
         STC   R2,ELEMENT+TAESSEQ-TAESD AND SET IN ELEMENT                      
*                                                                               
         MVI   ANYMORE,C'Y'                                                     
         BAS   RE,NEXTEL3          IF MORE ELEMENTS TO COME                     
         BE    *+8                                                              
         MVI   ANYMORE,C'N'                                                     
         BAS   RE,DORESEQ          RESET SEQ #'S ON REMAINING ELS               
         B     MYADDLX             CC SET                                       
         SPACE 2                                                                
*              ROUTINE TO RESET SEQUENCE NUMBERS AFTER ELEMENT                  
*              TO BE INSERTED - SETS CC TO LOW IF EXCEEDED MAX                  
         SPACE 1                                                                
DORESEQ  NTR1                                                                   
         CLI   ANYMORE,C'Y'        IF MORE ELEMENTS                             
         BNE   MYADDLY                                                          
         ZIC   R1,ELEMENT+TAESSEQ-TAESD                                         
         B     DORESEQ8                                                         
*                                                                               
DORESEQ5 BAS   RE,NEXTEL                                                        
         BNE   MYADDLY                                                          
         USING TAESD,R4                                                         
*                                                                               
         CLI   TAESEL,TAESELQ      IF ESTIMATE ELEMENT                          
         BNE   MYADDLY                                                          
DORESEQ8 CH    R1,=H'255'          IF MORE THAN 255 ELEMENTS                    
         BH    MYADDLL             PASS BACK LOW                                
         LA    R1,1(R1)            INCREMENT SEQUENCE NUMBER                    
         STC   R1,TAESSEQ                                                       
         B     DORESEQ5            AND LOOP FOR MORE ELEMENTS                   
         DROP  R4                                                               
         EJECT                                                                  
         GETELN (R4),DATADISP,ELCODE,3                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SPLIT UP BIG ESTIMATE RECORD TO SMALLER               
*              RECORD(S) AND WRITE IT TO THE FILE                               
         SPACE 1                                                                
         DS    0D                                                               
SPLIT    NMOD1 0,*SPLIT*                                                        
         L     RC,0(R1)                                                         
*                                                                               
         L     R4,AWRKIO           R4=A(NEW ESTIMATE RECORD)                    
         BAS   RE,PROCAC           ADD TAACELQ'S FROM ORG. REC(S)               
*                                                                               
         L     R3,AIO2             R3=A(RECORD TO BE WRITTEN BACK)              
         ST    R3,AIO                                                           
         USING TLESD,R3                                                         
         MVC   TLESKEY,0(R4)       SET KEY IN RECORD                            
         MVC   TLESLEN,DATADISP    INIT. RECORD LENGTH                          
         XC    TLESSTAT(10),TLESSTAT     STATUS, BEG. OF REC.                   
         SPACE 1                                                                
         MVI   ELCODE,0              SET TO LOOP THROUGH ALL ELEMENTS           
         LA    R4,TLESELEM-TLESD(R4) R4=A(FIRST ELEMENT)                        
         MVI   RDUPDATE,C'Y'         SET TO READ FOR UPDATE                     
         NI    GENSTAT1,ALL-RDUPAPPL THIS WILL KEEP IT ON UNTIL END             
         SPACE 1                                                                
SPL2     CLI   TLESSEQ,0           IF NOT BASE RECORD                           
         BE    SPL4                                                             
         MVC   TLESLEN,DATADISP    INIT. RECORD LENGTH                          
         XC    TLESSTAT(10),TLESSTAT     STATUS, BEG. OF REC.                   
         SPACE 1                                                                
SPL4     ZIC   R1,1(R4)            R1 = L'ELEMENT                               
         AH    R1,TLESLEN             + L'RECORD                                
         CH    R1,=H'1930'         WILL RECORD BECOME TOO LONG                  
         BH    SPL6                YES - GO RELEASE IT                          
         MVC   ELEMENT,0(R4)       NO - MOVE ELEMENT TO W/S                     
         GOTO1 ADDELEM,DMCB,,,,0   AND ADD TO NEW REC.                          
         BAS   RE,NEXTEL4          GET NEXT ELEMENT                             
         BE    SPL4                                                             
         SPACE 1                                                                
SPL6     BAS   RE,RELEASE          WRITE BACK RECORD IN IO2                     
         SPACE 1                                                                
         CLI   0(R4),0             IF THERE ARE MORE ELS. TO PROCESS            
         BNE   SPL2                GO BACK                                      
         SPACE 1                                                                
         GOTO1 ADELETE,DMCB,(RC)   DELETE ANY REMAINING RECORDS                 
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         OI    GENSTAT1,RDUPAPPL   RESET GENCON CONTROL SWITCH                  
         OI    STESTAT2,STEWRITE   WROTE BACK ESTIMATE RECORD                   
*                                                                               
SPLITX   XIT1                                                                   
         EJECT                                                                  
*              ROUTINE COPIES NON-MAINT. ACTIVITY ELEMENTS FROM                 
*              ORIGINAL ESTIMATE REC(S) TO NEW RECORD                           
         SPACE 1                                                                
PROCAC   NTR1                                                                   
         MVI   HAVEAC40,C'N'       NO ACTIVITY ELEMENT FROM 40 SCREEN           
         XC    KEY,KEY             READ HIGH FOR ESTIMATE                       
         MVC   KEY(L'TLESKEY),0(R4)                                             
         GOTO1 HIGH                                                             
         B     PROCAC5                                                          
*                                                                               
PROCAC2  GOTO1 SEQ                                                              
PROCAC5  CLC   KEY(TLESSEQ-TLESD),KEYSAVE                                       
         BNE   PROCACX                                                          
*                                                                               
         MVC   AIO,AIO1            SET AIO TO READ RECORD                       
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL4                                                        
         B     *+8                                                              
PROCAC10 BAS   RE,NEXTEL4                                                       
         BNE   PROCAC2                                                          
         USING TAACD,R4                                                         
*                                                                               
         CLI   TAACSCR,0           IF NOT MAINT. ACTIVITY ELEMENT               
         BE    PROCAC10                                                         
         CLI   TAACSCR,SCR40       IF 40 SCREEN                                 
         BNE   *+8                                                              
         MVI   HAVEAC40,C'Y'       SET HAVE ELEMENT FROM 40 SCREEN              
         MVC   ELEMENT,0(R4)                                                    
         MVC   AIO,AWRKIO                                                       
         GOTO1 AMYADDL,DMCB,(RC)   AND ADD TO NEW REC.                          
         BE    PROCAC10                                                         
         DC    H'0'                CAN'T HAVE A PROBLEM AT THE START            
*                                                                               
PROCACX  B     SPLITX                                                           
         EJECT                                                                  
*              ROUTINE RELEASES SPLIT-UP ESTIMATE RECORD                        
         SPACE 1                                                                
         USING TLESD,R3            R3=A(RECORD TO BE WRITTEN BACK)              
RELEASE  NTR1                                                                   
         MVC   KEY,TLESKEY         MOVE KEY FROM RECORD TO KEY                  
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                SEE IF RECORD ALREADY ON FILE                
         SPACE 1                                                                
         LA    R3,KEY              R3=A(DIRECTORY RECORD)                       
         CLC   TLESKEY,KEYSAVE     IS RECORD ALREADY ON FILE                    
         BE    REL8                                                             
         MVC   TLESKEY,KEYSAVE     NO, SO RESTORE SAVED KEY                     
         GOTO1 ADDREC              AND ADD NEW RECORD TO FILE                   
         B     RELX                                                             
         SPACE 1                                                                
         USING TLDRD,R3                                                         
REL8     TM    TLDRSTAT,X'80'      IF DIRECTORY MARKED DELETED                  
         BZ    REL10                                                            
         XI    TLDRSTAT,X'80'      UNMARK IT                                    
         GOTO1 WRITE               AND WRITE IT BACK                            
         SPACE 1                                                                
REL10    MVC   AIO,AIO1            RECORD EXISTS - READ IT INTO AIO1            
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2            RESET AIO TO A(NEW RECORD)                   
         GOTO1 PUTREC              WRITE BACK NEW FILE RECORD                   
         SPACE 1                                                                
RELX     NI    DMINBTS,X'F7'       TURN OFF READ FOR DELETED                    
         L     R3,AIO              R3=A(RECORD WE JUST ADDED/WROTE)             
         USING TLESD,R3                                                         
         ZIC   R1,TLESSEQ          BUMP SEQUENCE NUMBER IN KEY OF REC.          
         LA    R1,2(R1)                                                         
         STC   R1,TLESSEQ                                                       
         B     SPLITX                                                           
         EJECT                                                                  
         GETELN (R4),DATADISP,ELCODE,4                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE DELETES SUBSEQUENT ESTIMATE RECORDS                      
         SPACE 1                                                                
         DS    0D                                                               
DELETE   NMOD1 0,*DELETE*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R3,AIO              R3=A(FILE RECORD)                            
         USING TLESD,R3                                                         
         SPACE 1                                                                
DEL2     GOTO1 HIGH                RE-READ RECORD WE JUST WROTE                 
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 SEQ                 GET NEXT                                     
         SPACE 1                                                                
         CLC   KEY(TLESSEQ-TLESD),KEYSAVE  TEST STILL SAME ESTIMATE             
         BNE   DELETEX                                                          
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD                               
         OI    TLESSTAT,X'80'      MARK IT DELETED                              
         GOTO1 PUTREC              AND WRITE IT BACK                            
         SPACE 1                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  MARK DIRECTORY DELETED                 
         GOTO1 WRITE                     AND WRITE IT BACK                      
         SPACE 1                                                                
         OI    DMINBTS,X'08'       SET TO READ DELETED                          
         B     DEL2                LOOK FOR MORE                                
*                                                                               
DELETEX  XIT1                                                                   
         DROP R3                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENEOPTS        OPTION EQUATES                               
         EJECT                                                                  
*              TABLE OF ELEMENTS AND THEIR COMPONENTS                           
MAPTAB   DS    0F                                                               
         SPACE 1                                                                
ELKYINI  DC    AL1(ELKYINID-*),AL2(06),AL2(ELKYINIX-ELKYINI)                    
         DC    AL1(MHUSKEY),CL3'INI',AL1(0)                                     
         DC    AL1(0),AL2(0),AL1(0)                                             
ELKYINID DC    AL1(0)                                                           
ELKYINIX EQU   *                                                                
*                                                                               
ELKYAY   DC    AL1(ELKYAYD-*),AL2(07),AL2(ELKYAYX-ELKYAY)                       
         DC    AL1(MHUSKEY),CL3'AY',AL1(TLAYCDQ)                                
         DC    AL1(L'TLDRKEY-1),AL2(CLRAY-T702E2),AL1(0)                        
ELKYAYD  DC    AL1(MDULNQ),AL2(01),CL5'AGY',AL1(MDTCHQ,L'TGAGY)                 
         DC    AL2(TGAGY-TGD),AL1(MDTGD,0,0),AL2(VAGY-T702E2,0)                 
         DC    AL1(0)                                                           
ELKYAYX  EQU   *                                                                
*                                                                               
ELKYESR  DC    AL1(ELKYESRD-*),AL2(08),AL2(ELKYESRX-ELKYESR)                    
         DC    AL1(MHUSKEY),CL3'ESR',AL1(TLESCDQ)                               
         DC    AL1(L'TLDRKEY-1),AL2(CLRESR-T702E2),AL1(0)                       
ELKYESRD DC    AL1(MDULNQ),AL2(01),CL5'ACTN',AL1(MDTCHQ,L'FLTACTN)              
         DC    AL2(FLTACTN-FLTDATA),AL1(0,MDLOCL,0)                             
         DC    AL2(VACTN-T702E2,VACTN-T702E2)                                   
         DC    AL1(MDULNQ),AL2(02),CL5'AGY',AL1(MDTCHQ,L'TGAGY)                 
         DC    AL2(TGAGY-TGD),AL1(MDTGD,0,0)                                    
         DC    AL2(VAGY-T702E2,VAGY-T702E2)                                     
         DC    AL1(MDULNQ),AL2(03),CL5'EST',AL1(MDTCHQ,L'TGEST)                 
         DC    AL2(TGEST-TGD),AL1(MDTGD,0,0)                                    
         DC    AL2(0,0)                                                         
         DC    AL1(0)                                                           
ELKYESRX EQU   *                                                                
*                                                                               
ELKYESL  DC    AL1(ELKYESLD-*),AL2(09),AL2(ELKYESLX-ELKYESL)                    
         DC    AL1(MHUSKEY+KEYSL),CL3'ESL',AL1(TLESCDQ)                         
         DC    AL1(TLESEST-TLESD-1),AL2(CLRESL-T702E2),AL1(0)                   
ELKYESLD DC    AL1(MDULNQ),AL2(01),CL5'AGY',AL1(MDTCHQ,L'TGAGY)                 
         DC    AL2(TGAGY-TGD),AL1(MDTGD,0,0),AL2(VAGY-T702E2,0)                 
         DC    AL1(MDULNQ),AL2(02),CL5'START',AL1(MDTCHQ,L'FLTSTRT)             
         DC    AL2(FLTSTRT-FLTDATA),AL1(0,MDLOCL,0),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(03),CL5'CLI',AL1(MDTCHQ,L'FLTCLI)                
         DC    AL2(FLTCLI-FLTDATA),AL1(0,MDLOCL,0),AL2(VCLI-T702E2,0)           
         DC    AL1(MDULNQ),AL2(04),CL5'PRD',AL1(MDTCHQ,L'FLTPRD)                
         DC    AL2(FLTPRD-FLTDATA),AL1(0,MDLOCL+MDNOCLI,0)                      
         DC    AL2(VPRD-T702E2,0)                                               
         DC    AL1(MDULNQ),AL2(05),CL5'RUNDT',AL1(MDTDTQ,L'FLTRDTE)             
         DC    AL2(FLTRDTE-FLTDATA),AL1(0,MDLOCL,0),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(06),CL5'CHGDT',AL1(MDTDTQ,L'FLTCDTE)             
         DC    AL2(FLTCDTE-FLTDATA),AL1(0,MDLOCL,0),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(07),CL5'RFLAG',AL1(MDTCHQ,L'FLTRDFLG)            
         DC    AL2(FLTRDFLG-FLTDATA),AL1(0,MDLOCL,0)                            
         DC    AL2(VDTFLG-T702E2,0)                                             
         DC    AL1(MDULNQ),AL2(08),CL5'CFLAG',AL1(MDTCHQ,L'FLTCDFLG)            
         DC    AL2(FLTCDFLG-FLTDATA),AL1(0,MDLOCL,0)                            
         DC    AL2(VDTFLG2-T702E2,0)                                            
         DC    AL1(0)                                                           
ELKYESLX EQU   *                                                                
*                                                                               
ELKYCAL  DC    AL1(ELKYCALD-*),AL2(10),AL2(ELKYCALX-ELKYCAL)                    
         DC    AL1(MHUSKEY+KEYSL),CL3'CAL',AL1(TLCACDQ)                         
         DC    AL1(TLCASORT-TLCAD-1),AL2(CLRCA-T702E2),AL1(0)                   
ELKYCALD DC    AL1(MDULNQ),AL2(01),CL5'COMML',AL1(MDTHXQ,L'TGCOM)               
         DC    AL2(TGCOM-TGD),AL1(MDTGD,0,0),AL2(0,0)                           
         DC    AL1(MDULNQ),AL2(02),CL5'MUS',AL1(MDTCHQ,L'FLTMIND)               
         DC    AL2(FLTMIND-FLTDATA),AL1(0,MDLOCL,0),AL2(0,0)                    
         DC    AL1(0)                                                           
ELKYCALX EQU   *                                                                
*                                                                               
ELKYCO   DC    AL1(ELKYCOD-*),AL2(11),AL2(ELKYCOX-ELKYCO)                       
         DC    AL1(MHUSKEY),CL3'CO',AL1(TLCOICDQ)                               
         DC    AL1(TLCOICOM-TLCOPD-1),AL2(CLRCO-T702E2),AL1(0)                  
ELKYCOD  DC    AL1(MDULNQ),AL2(01),CL5'AGY',AL1(MDTCHQ,L'TGAGY)                 
         DC    AL2(TGAGY-TGD),AL1(MDTGD,0,0),AL2(VAGY-T702E2,0)                 
         DC    AL1(MDULNQ),AL2(02),CL5'CID',AL1(MDTCHQ,L'TGCID)                 
         DC    AL2(TGCID-TGD),AL1(MDTGD,0,0),AL2(0,0)                           
         DC    AL1(0)                                                           
ELKYCOX  EQU   *                                                                
*                                                                               
ELKYCP   DC    AL1(ELKYCPD-*),AL2(12),AL2(ELKYCPX-ELKYCP)                       
         DC    AL1(MHUSKEY),CL3'CP',AL1(TLCLCDQ)                                
         DC    AL1(L'TLDRKEY-1),AL2(CLRCP-T702E2),AL1(0)                        
ELKYCPD  DC    AL1(MDULNQ),AL2(01),CL5'AGY',AL1(MDTCHQ,L'TGAGY)                 
         DC    AL2(TGAGY-TGD),AL1(MDTGD,0,0),AL2(VAGY-T702E2,0)                 
         DC    AL1(MDULNQ),AL2(02),CL5'CLI',AL1(MDTCHQ,L'TGCLI)                 
         DC    AL2(TGCLI-TGD),AL1(MDTGD,0,0),AL2(0,0)                           
         DC    AL1(MDULNQ),AL2(03),CL5'PRD',AL1(MDTCHQ,L'TGPRD)                 
         DC    AL2(TGPRD-TGD),AL1(MDTGD,0,0),AL2(0,0)                           
         DC    AL1(0)                                                           
ELKYCPX  EQU   *                                                                
*                                                                               
ELKYCOH  DC    AL1(ELKYCOHD-*),AL2(13),AL2(ELKYCOHX-ELKYCOH)                    
         DC    AL1(MHUSKEY),CL3'COH',AL1(TLCOICDQ)                              
         DC    AL1(TLCOICOM-TLCOPD-1),AL2(CLRCO-T702E2),AL1(0)                  
ELKYCOHD DC    AL1(MDULNQ),AL2(01),CL5'AGY',AL1(MDTCHQ,L'TGAGY)                 
         DC    AL2(TGAGY-TGD),AL1(MDTGD,0,0),AL2(VAGY-T702E2,0)                 
         DC    AL1(MDULNQ),AL2(02),CL5'CID',AL1(MDTCHQ,L'TGCID)                 
         DC    AL2(TGCID-TGD),AL1(MDTGD,0,0),AL2(0,0)                           
         DC    AL1(0)                                                           
ELKYCOHX EQU   *                                                                
*                                                                               
ELKYUHL  DC    AL1(ELKYUHLD-*),AL2(14),AL2(ELKYUHLX-ELKYUHL)                    
         DC    AL1(MHUSKEY+KEYSL),CL3'UHL',AL1(0)                               
         DC    AL1(TLUHCSEQ-TLUHD-1),AL2(CLRUHL-T702E2),AL1(0)                  
ELKYUHLD DC    AL1(MDULNQ),AL2(01),CL5'COMML',AL1(MDTHXQ,L'TGCOM)               
         DC    AL2(TGCOM-TGD),AL1(MDTGD,0,0),AL2(0,0)                           
         DC    AL1(0)                                                           
ELKYUHLX EQU   *                                                                
*                                                                               
ELKYCOL  DC    AL1(ELKYCOLD-*),AL2(15),AL2(ELKYCOLX-ELKYCOL)                    
         DC    AL1(MHUSKEY+KEYSL),CL3'COL',AL1(0)                               
         DC    AL1(0),AL2(CLRCOL-T702E2),AL1(0)                                 
ELKYCOLD DC    AL1(MDULNQ),AL2(01),CL5'FORMT',AL1(MDTCHQ,L'FLTKEYQ)             
         DC    AL2(FLTKEYQ-FLTDATA),AL1(0,MDLOCL,0),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(02),CL5'AGY',AL1(MDTCHQ,L'FLTAGY)                
         DC    AL2(FLTAGY-FLTDATA),AL1(0,MDLOCL,0),AL2(VAGY-T702E2,0)           
         DC    AL1(MDULNQ),AL2(03),CL5'CLI',AL1(MDTCHQ,L'FLTCLI)                
         DC    AL2(FLTCLI-FLTDATA),AL1(0,MDLOCL,0),AL2(VCLI-T702E2,0)           
         DC    AL1(MDULNQ),AL2(04),CL5'PRD',AL1(MDTCHQ,L'FLTPRD)                
         DC    AL2(FLTPRD-FLTDATA),AL1(0,MDLOCL,0),AL2(VPRD-T702E2,0)           
         DC    AL1(MDULNQ),AL2(05),CL5'MEDIA',AL1(MDTCHQ,L'FLTMED)              
         DC    AL2(FLTMED-FLTDATA),AL1(0,MDLOCL,0),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(06),CL5'CLG',AL1(MDTCHQ,L'FLTCLG)                
         DC    AL2(FLTCLG-FLTDATA),AL1(0,MDLOCL,0),AL2(VCLG-T702E2,0)           
         DC    AL1(MDULNQ),AL2(07),CL5'START',AL1(MDTCHQ,L'FLTSTRT)             
         DC    AL2(FLTSTRT-FLTDATA),AL1(0,MDLOCL,0),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(08),CL5'PDATE',AL1(MDTDTQ,L'FLTPDTE)             
         DC    AL2(FLTPDTE-FLTDATA),AL1(0,MDLOCL,0),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(09),CL5'LOCK',AL1(MDTCHQ,L'FLTLOCK)              
         DC    AL2(FLTLOCK-FLTDATA),AL1(0,MDLOCL,0),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(10),CL5'REL',AL1(MDTCHQ,L'FLTREL)                
         DC    AL2(FLTREL-FLTDATA),AL1(0,MDLOCL,0),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(11),CL5'MUSIC',AL1(MDTCHQ,L'FLTMUS)              
         DC    AL2(FLTMUS-FLTDATA),AL1(0,MDLOCL,0),AL2(0,0)                     
ELKYCOLX EQU   *                                                                
*                                                                               
ELKYUHM  DC    AL1(ELKYUHMD-*),AL2(16),AL2(ELKYUHMX-ELKYUHM)                    
         DC    AL1(MHUSKEY+KEYSL),CL3'UHM',AL1(0)                               
         DC    AL1(0),AL2(0),AL1(0)                                             
ELKYUHMD DC    AL1(MDULNQ),AL2(01),CL5'COMML',AL1(MDTHXQ,L'TGCOM)               
         DC    AL2(TGCOM-TGD),AL1(MDTGD,0,0),AL2(VUHM-T702E2,0)                 
         DC    AL1(0)                                                           
ELKYUHMX EQU   *                                                                
*                                                                               
ELKYW4   DC    AL1(ELKYW4D-*),AL2(17),AL2(ELKYW4X-ELKYW4)                       
         DC    AL1(MHUSKEY),CL3'GUL',AL1(TLW4CDQ)                               
         DC    AL1(L'TLDRKEY),AL2(CLRGU-T702E2),AL1(0)                          
ELKYW4D  DC    AL1(MDULNQ),AL2(01),CL5'SSN',AL1(MDTCHQ,L'TGSSN)                 
         DC    AL2(TGSSN-TGD),AL1(MDTGD,0,0),AL2(VSSN-T702E2,0)                 
         DC    AL1(MDULNQ),AL2(02),CL5'AGY',AL1(MDTCHQ,L'FLTAGY)                
         DC    AL2(FLTAGY-FLTDATA),AL1(0,MDLOCL,0),AL2(VAGY-T702E2,0)           
         DC    AL1(MDULNQ),AL2(03),CL5'CLI',AL1(MDTCHQ,L'FLTCLI)                
         DC    AL2(FLTCLI-FLTDATA),AL1(0,MDLOCL,0),AL2(VCLI-T702E2,0)           
         DC    AL1(0)                                                           
ELKYW4X  EQU   *                                                                
*                                                                               
ELKYGUL  DC    AL1(ELKYGULD-*),AL2(18),AL2(ELKYGULX-ELKYGUL)                    
         DC    AL1(MHUSKEY+KEYSL),CL3'GUL',AL1(TLGUCDQ)                         
         DC    AL1(TLGUGUA-TLGUD-1),AL2(CLRGU-T702E2),AL1(0)                    
ELKYGULD DC    AL1(MDULNQ),AL2(01),CL5'SSN',AL1(MDTCHQ,L'TGSSN)                 
         DC    AL2(TGSSN-TGD),AL1(MDTGD,0,0),AL2(VSSN-T702E2,0)                 
         DC    AL1(MDULNQ),AL2(02),CL5'AGY',AL1(MDTCHQ,L'FLTAGY)                
         DC    AL2(FLTAGY-FLTDATA),AL1(0,MDLOCL,0),AL2(VAGY-T702E2,0)           
         DC    AL1(MDULNQ),AL2(03),CL5'CLI',AL1(MDTCHQ,L'FLTCLI)                
         DC    AL2(FLTCLI-FLTDATA),AL1(0,MDLOCL,0),AL2(VCLI-T702E2,0)           
         DC    AL1(0)                                                           
ELKYGULX EQU   *                                                                
*                                                                               
*                                                                               
ELTA01   DC    AL1(ELTA01D-*),AL2(TA01ELQ),AL2(ELTA01X-ELTA01)                  
         DC    AL1(MHUSINI),CL2'01',AL1(TA01LNQ)                                
ELTA01D  DC    AL1(MDULNQ),AL2(01),CL5'CNT',AL1(MDTBIQ,L'TA01CNT)               
         DC    AL2(TA01CNT-TA01D),AL1(0,0,0),AL2(DCNT-PROCEL,0)                 
         DC    AL1(MDULNQ),AL2(02),CL5'CAM',AL1(MDTCHQ,L'TA01CAM)               
         DC    AL2(TA01CAM-TA01D),AL1(0,0,0),AL2(DCAM-PROCEL,0)                 
         DC    AL1(MDULNQ),AL2(03),CL5'UNI',AL1(MDTCHQ,L'TA01UN)                
         DC    AL2(TA01UN-TA01D),AL1(0,0,0),AL2(0,0)                            
         DC    AL1(MDULNQ),AL2(04),CL5'CORP',AL1(MDTMDQ,L'TA01CORP)             
         DC    AL2(TA01CORP-TA01D),AL1(0,0,0),AL2(DYES-PROCEL,0)                
         DC    AL1(MDULNQ),AL2(05),CL5'DBL',AL1(MDTCHQ,L'TA01DBL)               
         DC    AL2(TA01DBL-TA01D),AL1(0,0,0),AL2(DDBL-PROCEL,0)                 
         DC    AL1(MDULNQ),AL2(06),CL5'OV1',AL1(MDTBCQ,L'TA01OV1)               
         DC    AL2(TA01OV1-TA01D),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(MDULNQ),AL2(07),CL5'OV2',AL1(MDTBCQ,L'TA01OV2)               
         DC    AL2(TA01OV2-TA01D),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(0)                                                           
ELTA01X  EQU   *                                                                
*                                                                               
ELTA02   DC    AL1(ELTA02D-*),AL2(TA02ELQ),AL2(ELTA02X-ELTA02)                  
         DC    AL1(MHUSINI),CL2'02',AL1(TA02LNQ)                                
ELTA02D  DC    AL1(MDULNQ),AL2(01),CL5'CODE',AL1(MDTCHQ,L'TA02CTYP)             
         DC    AL2(TA02CTYP-TA02D),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(02),CL5'CTYP',AL1(MDTCHQ,L'TA02CTYN)             
         DC    AL2(TA02CTYN-TA02D),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(03),CL5'HLD',AL1(MDTMDQ,L'TA02AHLD)              
         DC    AL2(TA02AHLD-TA02D),AL1(0,0,0),AL2(DYES-PROCEL,0)                
         DC    AL1(0)                                                           
ELTA02X  EQU   *                                                                
*                                                                               
ELTA03   DC    AL1(ELTA03D-*),AL2(TA03ELQ),AL2(ELTA03X-ELTA03)                  
         DC    AL1(MHUSINI),CL2'03',AL1(TA03LNQ)                                
ELTA03D  DC    AL1(MDULNQ),AL2(01),CL5'ADST',AL1(MDTCHQ,L'TA03ADST)             
         DC    AL2(TA03ADST-TA03D),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(02),CL5'ADEM',AL1(MDTCHQ,L'TA03ADEM)             
         DC    AL2(TA03ADEM-TA03D),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(0)                                                           
ELTA03X  EQU   *                                                                
*                                                                               
ELTA04   DC    AL1(ELTA04D-*),AL2(TA04ELQ),AL2(ELTA04X-ELTA04)                  
         DC    AL1(MHUSINI),CL2'04',AL1(TA04LNQ)                                
ELTA04D  DC    AL1(MDULNQ),AL2(01),CL5'CCDE',AL1(MDTCHQ,L'TA04CCDE)             
         DC    AL2(TA04CCDE-TA04D),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(02),CL5'CCNM',AL1(MDTCHQ,L'TA04CCN)              
         DC    AL2(TA04CCN-TA04D),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(0)                                                           
ELTA04X  EQU   *                                                                
*                                                                               
ELTA05   DC    AL1(ELTA05D-*),AL2(TA05ELQ),AL2(ELTA05X-ELTA05)                  
         DC    AL1(MHUSESL),CL2'05',AL1(TA05LNQ)                                
ELTA05D  DC    AL1(MDULNQ),AL2(01),CL5'ID',AL1(MDTCHQ,L'TA05ID)                 
         DC    AL2(TA05ID-TA05D),AL1(0,0,0),AL2(0,0)                            
         DC    AL1(MDULNQ),AL2(02),CL5'NAME',AL1(MDTCHQ,L'TA05NAME)             
         DC    AL2(TA05NAME-TA05D),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(03),CL5'RDTE',AL1(MDTDTQ,L'TA05RDTE)             
         DC    AL2(TA05RDTE-TA05D),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(04),CL5'CDTE',AL1(MDTDTQ,L'TA05CDTE)             
         DC    AL2(TA05CDTE-TA05D),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(0)                                                           
ELTA05X  EQU   *                                                                
*                                                                               
ELTAFE   DC    AL1(ELTAFED-*),AL2(TAFEELQ),AL2(ELTAFEX-ELTAFE)                  
         DC    AL1(MHUSINI),CL2'FE',AL1(TAFELNQ)                                
ELTAFED  DC    AL1(MDULNQ),AL2(01),CL5'VER#',AL1(MDTHXQ,L'TAFEVER)              
         DC    AL2(TAFEVER-TAFED),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(0)                                                           
ELTAFEX  EQU   *                                                                
*                                                                               
ELTAFD   DC    AL1(ELTAFDD-*),AL2(TAFDELQ),AL2(ELTAFDX-ELTAFD)                  
         DC    AL1(0),CL2'FD',AL1(TAFDLNQ)                                      
ELTAFDD  DC    AL1(MDULNQ),AL2(01),CL5'VER#',AL1(MDTHXQ,L'TAFDVER)              
         DC    AL2(TAFDVER-TAFDD),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(0)                                                           
ELTAFDX  EQU   *                                                                
*                                                                               
ELTAAC   DC    AL1(ELTAACD-*),AL2(TAACELQ),AL2(ELTAACX-ELTAAC)                  
         DC    AL1(MHUSES),CL2'AC',AL1(TAACLNQ)                                 
ELTAACD  DC    AL1(MDULNQ),AL2(01),CL5'STAF',AL1(MDTCHQ,L'TAACSTAF)             
         DC    AL2(TAACSTAF-TAACD),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(02),CL5'DATE',AL1(MDTDTQ,L'TAACCDTE)             
         DC    AL2(TAACCDTE-TAACD),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(03),CL5'TIME',AL1(MDTCHQ,8)                      
         DC    AL2(TAACCDTE-TAACD),AL1(0,0,0),AL2(DTIME-PROCEL,0)               
         DC    AL1(MDULNQ),AL2(04),CL5'SCRN',AL1(MDTHXQ,L'TAACSCR)              
         DC    AL2(TAACSCR-TAACD),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(MDULNQ),AL2(05),CL5'NAME',AL1(MDTCHQ,L'TGNAME)               
         DC    AL2(TAACID-TAACD),AL1(0,0,0),AL2(DSTAF-PROCEL,0)                 
         DC    AL1(MDULNQ),AL2(06),CL5'MAJOR',AL1(MDTBIQ,L'TAACMAJ)             
         DC    AL2(TAACMAJ-TAACD),AL1(MDXDWN,0,0),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(07),CL5'MINOR',AL1(MDTBIQ,L'TAACMIN)             
         DC    AL2(TAACMIN-TAACD),AL1(MDXDWN,0,0),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(08),CL5'BLDNO',AL1(MDTBIQ,L'TAACBLD)             
         DC    AL2(TAACBLD-TAACD),AL1(MDXDWN,0,0),AL2(0,0)                      
         DC    AL1(0)                                                           
ELTAACX  EQU   *                                                                
*                                                                               
ELTAAY   DC    AL1(ELTAAYD-*),AL2(TAAYELQ),AL2(ELTAAYX-ELTAAY)                  
         DC    AL1(0),CL2'AY',AL1(TAAYLNQ)                                      
ELTAAYD  DC    AL1(MDULNQ),AL2(01),CL5'STAT',AL1(MDTBIQ,L'TAAYSTAT)             
         DC    AL2(TAAYSTAT-TAAYD),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(02),CL5'AGY',AL1(MDTCHQ,L'TGAGY)                 
         DC    AL2(TGAGY-TGD),AL1(MDINI+MDTGD,0,0),AL2(0,0)                     
         DC    AL1(0)                                                           
ELTAAYX  EQU   *                                                                
*                                                                               
ELTACA   DC    AL1(ELTACAD-*),AL2(TACAELQ),AL2(ELTACAX-ELTACA)                  
         DC    AL1(MHUSXGU),CL2'CA',AL1(TACALNQ)                                
ELTACAD  DC    AL1(MDULNQ),AL2(01),CL5'SSN',AL1(MDTCHQ,L'TGSSN)                 
         DC    AL2(TGSSN-TGD),AL1(MDXUHM+MDTGD,0,0),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(02),CL5'FRST',AL1(MDTCHQ,L'TGNAME)               
         DC    AL2(0),AL1(MDXUHM,0,0),AL2(DSSNF-PROCEL,0)                       
         DC    AL1(MDULNQ),AL2(03),CL5'LAST',AL1(MDTCHQ,L'TGNAME)               
         DC    AL2(0),AL1(MDXUHM,0,0),AL2(DSSNL-PROCEL,0)                       
         DC    AL1(MDULNQ),AL2(04),CL5'SEQ',AL1(MDTHXQ,L'TGCSORT)               
         DC    AL2(TGCSORT-TGD),AL1(MDTGD,0,0),AL2(0,0)                         
         DC    AL1(MDULNQ),AL2(05),CL5'CAT',AL1(MDTCHQ,L'TGCAT)                 
         DC    AL2(TGCAT-TGD),AL1(MDTGD,0,0),AL2(DCAT-PROCEL,0)                 
         DC    AL1(MDULNQ),AL2(06),CL5'CAM',AL1(MDTCHQ,L'TACAONOF)              
         DC    AL2(TACAONOF-TACAD),AL1(0,0,0),AL2(DCAM-PROCEL,0)                
         DC    AL1(MDULNQ),AL2(07),CL5'UNI',AL1(MDTCHQ,L'TACAUN)                
         DC    AL2(TACAUN-TACAD),AL1(0,0,0),AL2(DUNI-PROCEL,0)                  
         DC    AL1(MDULNQ),AL2(08),CL5'OV2',AL1(MDTBCQ,L'TACAOV2)               
         DC    AL2(TACAOV2-TACAD),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(MDULNQ),AL2(09),CL5'STAT',AL1(MDTBIQ,L'TACASTAT)             
         DC    AL2(TACASTAT-TACAD),AL1(MDXUHM,0,0),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(10),CL5'GUA',AL1(MDTCHQ,L'TGGUA)                 
         DC    AL2(TACAGUA-TACAD),AL1(MDXUHM+MDZERO,0,0)                        
         DC    AL2(DGRTG-PROCEL,0)                                              
         DC    AL1(MDULNQ),AL2(11),CL5'YEAR',AL1(MDTCHQ,L'TACAYEAR)             
         DC    AL2(TACAYEAR-TACAD),AL1(0,0,0),AL2(DYEAR-PROCEL,0)               
         DC    AL1(MDULNQ),AL2(12),CL5'EXP',AL1(MDTDTQ,L'TACAEXP)               
         DC    AL2(TACAEXP-TACAD),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(MDULNQ),AL2(13),CL5'DBL',AL1(MDTCHQ,L'TACADBL)               
         DC    AL2(TACADBL-TACAD),AL1(0,0,0),AL2(DDBL-PROCEL,0)                 
         DC    AL1(MDULNQ),AL2(14),CL5'STA2',AL1(MDTBIQ,L'TACASTA4)             
         DC    AL2(TACASTA4-TACAD),AL1(MDXUHM,0,0),AL2(0,0)                     
         DC    AL1(0)                                                           
ELTACAX  EQU   *                                                                
*                                                                               
ELTACM   DC    AL1(ELTACMD-*),AL2(TACMELQ),AL2(ELTACMX-ELTACM)                  
         DC    AL1(MHUSES),CL2'CM',AL1(TACMLNQ)                                 
ELTACMD  DC    AL1(MDULNQ),AL2(01),CL5'COMM',AL1(MDTCHQ),AL1(0)                 
         DC    AL2(TACMCOMM-TACMD),AL1(MDCLEN,0,0),AL2(0,0)                     
         DC    AL1(0)                                                           
ELTACMX  EQU   *                                                                
*                                                                               
ELTACO   DC    AL1(ELTACOD-*),AL2(TACOELQ),AL2(ELTACOX-ELTACO)                  
         DC    AL1(0),CL2'CO',AL1(TACMLNQ)                                      
ELTACOD  DC    AL1(MDULNQ),AL2(01),CL5'CID',AL1(MDTCHQ,L'TACOCID)               
         DC    AL2(TACOCID-TACOD),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(MDULNQ),AL2(02),CL5'NAME',AL1(MDTCHQ,L'TGNAME)               
         DC    AL2(0),AL1(0,0,MAPFCO),AL2(DCIDN-PROCEL,0)                       
         DC    AL1(MDULNQ),AL2(03),CL5'MED',AL1(MDTCHQ,L'TACOMED)               
         DC    AL2(TACOMED-TACOD),AL1(0,0,MAPFCO),AL2(DMED-PROCEL,0)            
         DC    AL1(MDULNQ),AL2(04),CL5'FCYC',AL1(MDTDTQ,L'TGDATE)               
         DC    AL2(0),AL1(0,0,MAPFCO),AL2(DCIDF-PROCEL,0)                       
         DC    AL1(MDULNQ),AL2(05),CL5'EXP',AL1(MDTDTQ,L'TACOEXP)               
         DC    AL2(TACOEXP-TACOD),AL1(0,0,MAPFCO),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(06),CL5'COM',AL1(MDTHXQ,L'TGCOM)                 
         DC    AL2(TGCOM-TGD),AL1(MDTGD,0,MAPFCO),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(07),CL5'CLI',AL1(MDTCHQ,L'TGCLI)                 
         DC    AL2(0),AL1(0,0,MAPFCO),AL2(DCLI-PROCEL,0)                        
         DC    AL1(MDULNQ),AL2(08),CL5'PRD',AL1(MDTCHQ,L'TGPRD)                 
         DC    AL2(0),AL1(0,0,MAPFCO),AL2(DPRD-PROCEL,0)                        
         DC    AL1(MDULNQ),AL2(09),CL5'CAST',AL1(MDTMDQ,1)                      
         DC    AL2(0),AL1(0,0,MAPFCO),AL2(DCAST-PROCEL,0)                       
         DC    AL1(MDULNQ),AL2(10),CL5'LIFT',AL1(MDTMDQ,1)                      
         DC    AL2(0),AL1(0,0,MAPFCO),AL2(DLIFT-PROCEL,0)                       
         DC    AL1(MDULNQ),AL2(11),CL5'REL',AL1(MDTMDQ,1)                       
         DC    AL2(TACOSTAT-TACOD),AL1(0,0,MAPFCO),AL2(DREL-PROCEL,0)           
         DC    AL1(MDULNQ),AL2(12),CL5'TYPE',AL1(MDTCHQ,L'TACOTYPE)             
         DC    AL2(TACOTYPE-TACOD),AL1(0,0,MAPFCO),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(13),CL5'ADST',AL1(MDTCHQ,L'TACOADST)             
         DC    AL2(TACOADST-TACOD),AL1(0,0,MAPFCO),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(14),CL5'LIF2',AL1(MDTMDQ,1)                      
         DC    AL2(0),AL1(0,0,MAPFCO),AL2(DLIFT2-PROCEL,0)                      
         DC    AL1(0)                                                           
ELTACOX  EQU   *                                                                
*                                                                               
ELTAEM   DC    AL1(ELTAEMD-*),AL2(TAEMELQ),AL2(ELTAEMX-ELTAEM)                  
         DC    AL1(MHUSES),CL2'EM',AL1(TAEMLNQ)                                 
ELTAEMD  DC    AL1(MDULNQ),AL2(01),CL5'CASH',AL1(MDTBCQ,L'TAEMAMT)              
         DC    AL2(TAEMAMT-TAEMD),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(MDULNQ),AL2(02),CL5'TXT',AL1(MDTCHQ,0)                       
         DC    AL2(TAEMTXT-TAEMD),AL1(MDCLEN,0,0),AL2(0,0)                      
         DC    AL1(0)                                                           
ELTAEMX  EQU   *                                                                
*                                                                               
ELTAEN   DC    AL1(ELTAEND-*),AL2(TAENELQ),AL2(ELTAENX-ELTAEN)                  
         DC    AL1(MHUSES),CL2'EN',AL1(TAENLNQ)                                 
ELTAEND  DC    AL1(MDULNQ),AL2(01),CL5'NARR',AL1(MDTCHQ,0)                      
         DC    AL2(TAENNARR-TAEND),AL1(MDCLEN,0,0),AL2(0,0)                     
         DC    AL1(0)                                                           
ELTAENX  EQU   *                                                                
*                                                                               
ELTANA   DC    AL1(ELTANAD-*),AL2(TANAELQ),AL2(ELTANAX-ELTANA)                  
         DC    AL1(MHUSES+MHUSCP),CL2'NA',AL1(TANALNQ)                          
ELTANAD  DC    AL1(MDULNQ),AL2(01),CL5'NAME',AL1(MDTCHQ,0)                      
         DC    AL2(TANANAME-TANAD),AL1(MDCLEN,0,0),AL2(0,0)                     
         DC    AL1(0)                                                           
ELTANAX  EQU   *                                                                
*                                                                               
ELTANU   DC    AL1(ELTANUD-*),AL2(TANUELQ),AL2(ELTANUX-ELTANU)                  
         DC    AL1(MHUSES),CL2'NU',AL1(TANULNQ)                                 
ELTANUD  DC    AL1(MDULNQ),AL2(01),CL5'NUM',AL1(MDTCHQ,0)                       
         DC    AL2(TANUMBER-TANUD),AL1(MDCLEN,0,0)                              
         DC    AL2(0,UTANU-T702E2)                                              
         DC    AL1(0)                                                           
ELTANUX  EQU   *                                                                
*                                                                               
ELTAES   DC    AL1(ELTAESD-*),AL2(TAESELQ),AL2(ELTAESX-ELTAES)                  
         DC    AL1(MHUSES),CL2'ES',AL1(TAESLNQ)                                 
ELTAESD  DC    AL1(MDULNQ),AL2(01),CL5'TYPE',AL1(MDTBIQ,L'TAESTYPE)             
         DC    AL2(TAESTYPE-TAESD),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(02),CL5'CLI',AL1(MDTCHQ,L'TAESCLI)               
         DC    AL2(TAESDATA-TAESD),AL1(0,0,MDESCL),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(03),CL5'PRD',AL1(MDTCHQ,L'TAESPRD)               
         DC    AL2(TAESDATA-TAESD),AL1(0,0,MDESPR),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(04),CL5'COM',AL1(MDTHXQ,L'TAESCOM)               
         DC    AL2(TAESCOM-TAESD),AL1(0,0,MDESCO),AL2(DCOMG-PROCEL,0)           
         DC    AL1(MDULNQ),AL2(05),CL5'CID',AL1(MDTCHQ,L'TAESCID)               
         DC    AL2(TAESCID-TAESD),AL1(MDXDWN,0,MDESCO)                          
         DC    AL2(0,UCID-T702E2)                                               
         DC    AL1(MDULNQ),AL2(06),CL5'SORT',AL1(MDTHXQ,L'TAESSORT)             
         DC    AL2(TAESSORT-TAESD),AL1(0,0,MDESPER)                             
         DC    AL2(DSORTG-PROCEL,0)                                             
         DC    AL1(MDULNQ),AL2(07),CL5'SSN',AL1(MDTBIQ,L'TAESSSN)               
         DC    AL2(TAESSSN-TAESD),AL1(0,0,MDESPER)                              
         DC    AL2(DSSNG-PROCEL,0)                                              
         DC    AL1(MDULNQ),AL2(08),CL5'HCOM',AL1(MDTCHQ,L'TGCID)                
         DC    AL2(TAESHCOM-TAESD),AL1(0,0,MDESHCO)                             
         DC    AL2(DHCID-PROCEL,UHCID-T702E2)                                   
         DC    AL1(MDULNQ),AL2(09),CL5'HMED',AL1(MDTCHQ,1)                      
         DC    AL2(TAESHMED-TAESD),AL1(0,0,MDESHCO)                             
         DC    AL2(DMEDQ-PROCEL,UMED-T702E2)                                    
         DC    AL1(MDULNQ),AL2(10),CL5'HLEN',AL1(MDTBIQ,L'TAESHLEN)             
         DC    AL2(TAESHLEN-TAESD),AL1(0,0,MDESHCO),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(11),CL5'HSTA',AL1(MDTBIQ,L'TAESHSTA)             
         DC    AL2(TAESHSTA-TAESD),AL1(0,0,MDESHP),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(12),CL5'HUNI',AL1(MDTCHQ,L'TGUNI)                
         DC    AL2(TAESHUNI-TAESD),AL1(0,0,MDESHP)                              
         DC    AL2(DUNIQ-PROCEL,UUNI-T702E2)                                    
         DC    AL1(MDULNQ),AL2(13),CL5'HCAT',AL1(MDTCHQ,L'TGCAT)                
         DC    AL2(TAESHCAT-TAESD),AL1(0,0,MDESHP)                              
         DC    AL2(DCATQ-PROCEL,UCAT-T702E2)                                    
         DC    AL1(MDULNQ),AL2(14),CL5'HNPR',AL1(MDTBIQ,L'TAESHNPR)             
         DC    AL2(TAESHNPR-TAESD),AL1(0,0,MDESHP),AL2(DCNT-PROCEL,0)           
         DC    AL1(MDULNQ),AL2(15),CL5'HLFT',AL1(MDTCHQ,L'TAESHLFT)             
         DC    AL2(TAESHLFT-TAESD),AL1(0,0,MDESHP),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(16),CL5'HDBL',AL1(MDTCHQ,L'TAESHDBL)             
         DC    AL2(TAESHDBL-TAESD),AL1(0,0,MDESHP),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(17),CL5'Z',AL1(MDTBIQ,1)                         
         DC    AL2(TAESSBNO-TAESD),AL1(MDZERO,0,0)                              
         DC    AL2(DOPTS-PROCEL,0)                                              
*                                                                               
ELTAESO  DS    0C                                                               
         DC    AL1(MDULNQ),AL2(18),CL5' ',AL1(MDTCHQ,1)     ADD PERFS           
         DC    AL1(LOPADDL,0),AL1(MDXDWN,MDESO,OPADDL),AL2(0,0)                 
         DC    AL1(MDULNQ),AL2(19),CL5' ',AL1(MDTCHQ,1)     PERF LIST           
         DC    AL1(LOPPERF,0),AL1(MDXDWN,MDESO,OPPERF),AL2(0,0)                 
         DC    AL1(MDULNQ),AL2(20),CL5'XPERF',AL1(MDTMDQ,1) XPERF               
         DC    AL1(LOPX,0),AL1(0,MDESO,OPX),AL2(0,0)                            
         DC    AL1(MDULNQ),AL2(21),CL5' ',AL1(MDTCHQ,1)     INCL COML           
         DC    AL1(LOPYES,0),AL1(MDXDWN,MDESO,OPYES),AL2(0,0)                   
*                                                   ***EXPIRY=Y OR DT**         
         DC    AL1(MDULNQ),AL2(22),CL5'EXPY',AL1(MDTMDQ,1) *KEEP TOG**          
         DC    AL1(LOPEXP,0),AL1(0,MDESO,OPEXP)                                 
         DC    AL2(OEXP-PROCEL,IEXPY-T702E2)                                    
         DC    AL1(MDULNQ),AL2(23),CL5'EXPDT',AL1(MDTDTQ,3)                     
         DC    AL1(LOPEXP,0),AL1(0,MDESO,OPEXP)                                 
         DC    AL2(0,IEXPDT-T702E2)                                             
*                                                                               
         DC    AL1(MDULNQ),AL2(24),CL5'LLFT',AL1(MDTBIQ,1)   L'LIFT             
         DC    AL1(LOPLLIFT,0),AL1(0,MDESO,OPLLIFT),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(84),CL5'LLF2',AL1(MDTBIQ,1)   L'2ND LIFT         
         DC    AL1(LOPLLIF2,0),AL1(0,MDESO,OPLLIF2),AL2(0,0)                    
*                                                                               
         DC    AL1(MDULNQ),AL2(25),CL5'OV1',AL1(MDTBCQ,4)    1ST OV             
         DC    AL1(LOPOVER,0),AL1(0,MDESO,OPOVER),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(26),CL5'OV2',AL1(MDTBCQ,4)    2ND OV             
         DC    AL1(LOPOV2,0),AL1(0,MDESO,OPOVER2),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(27),CL5'OVDT',AL1(MDTDTQ,3)   DT FOR OV          
         DC    AL1(LOPOVAS,0),AL1(MDCOMB,MDESO,OPOVAS)       OR OV2             
         DC    AL2(0,IOV-T702E2)                                                
         DC    AL1(MDULNQ),AL2(27),CL5'OV2DT',AL1(MDTDTQ,3)  DT FOR OV          
         DC    AL1(LOPOV2AS,0),AL1(MDCOMB,MDESO,OPOV2AS)     OR OV2             
         DC    AL2(0,IOV-T702E2)                                                
         DC    AL1(MDULNQ),AL2(28),CL5'OV12D',AL1(MDTDTQ,3) DT FOR OV12         
         DC    AL1(LOPOV12A,0),AL1(MDCOMB,MDESO,OPOV12AS)   (AS/OF)             
         DC    AL2(0,IOV12AS-T702E2)                                            
         DC    AL1(MDULNQ),AL2(0),CL5'OV12',AL1(0,0)         OV1/OV2            
         DC    AL1(LOPOV12,0),AL1(MDCOMB,MDESO,OPOV12) **ONLY HERE FOR          
         DC    AL2(0,0)                                ** BUMPING ****          
*                                                                               
         DC    AL1(MDULNQ),AL2(30),CL5'YRDT',AL1(MDTDTQ,3)   DT FOR YR          
         DC    AL1(LOPYRAS,0),AL1(MDCOMB,MDESO,OPYRAS)       (AS/OF)            
         DC    AL2(0,IYRAS-T702E2)                                              
         DC    AL1(MDULNQ),AL2(29),CL5'YEAR',AL1(MDTCHQ,3)   YEAR               
         DC    AL1(LOPYEAR,0),AL1(0,MDESO,OPYEAR)                               
         DC    AL2(OYR-PROCEL,IYR-T702E2)                                       
*                                                                               
         DC    AL1(MDULNQ),AL2(31),CL5'CTYP',AL1(MDTCHQ,1)  COMML TYPE          
         DC    AL1(LOPCTYP,0),AL1(0,MDESO,OPCTYP),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(32),CL5'USE',AL1(MDTCHQ,3+5) USE                 
         DC    AL1(LOPUSE,0),AL1(MDUSE,MDESO,OPUSE)                             
         DC    AL2(OUSE-PROCEL,IUSE-T702E2)                                     
         DC    AL1(MDULNQ),AL2(33),CL5'DATE',AL1(MDTDTQ,3)  AS/OF DATE          
         DC    AL1(LOPASOF,0),AL1(MDUSE,MDESO,OPASOF),AL2(0,0)                  
         DC    AL1(MDULNQ),AL2(34),CL5'GRT',AL1(MDTBIQ,2)   GRT #               
         DC    AL1(LOPGRTNO,0),AL1(0,MDESO,OPGRTNO),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(35),CL5'PRI',AL1(MDTMDQ,1)   PRI COML            
         DC    AL1(LOPPRI,0),AL1(0,MDESO,OPPRI),AL2(0,0)                        
         DC    AL1(MDULNQ),AL2(36),CL5'SSN',AL1(MDTBIQ,4)   SSN #               
         DC    AL1(LOPSSN,0),AL1(0,MDESO,OPSSN),AL2(0,0)                        
         DC    AL1(MDULNQ),AL2(37),CL5'GRTST',AL1(MDTCHQ,1) GRT ST              
         DC    AL1(LOPGUAR,0),AL1(0,MDESO,OPGUAR),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(38),CL5'MAJS',AL1(MDTBIQ,1)  MAJORS              
         DC    AL1(LOPMAJOR,0),AL1(MDUSE,MDESO,OPMAJOR),AL2(0,0)                
         DC    AL1(MDULNQ),AL2(39),CL5'UNTS',AL1(MDTBIQ,2)  UNITS               
         DC    AL1(LOPUNITS,0),AL1(MDUSE,MDESO,OPUNITS),AL2(0,0)                
*                                                                               
         DC    AL1(MDULNQ),AL2(41),CL5'URNG',AL1(MDTBIQ,2)  USE RANGE           
         DC    AL1(LOPUSES2,0),AL1(MDUSE+MDCOMB,MDESO,OPUSES2)                  
         DC    AL2(0,IUSES-T702E2)                                              
         DC    AL1(MDULNQ),AL2(40),CL5'#USES',AL1(MDTBIQ,2) # USES              
         DC    AL1(LOPUSES,0),AL1(MDUSE,MDESO,OPUSES)                           
         DC    AL2(OURNG-PROCEL,0)                                              
         DC    AL1(MDULNQ),AL2(83),CL5'PAY1',AL1(MDTMDQ,1)  PAY 1ST VAR         
         DC    AL1(LOPPAY1,0),AL1(MDUSE,MDESO,OPPAY1),AL2(0,0)                  
*                                                                               
         DC    AL1(MDULNQ),AL2(42),CL5'INSRT',AL1(MDTBIQ,2) INSERTS             
         DC    AL1(LOPINS,0),AL1(MDUSE,MDESO,OPINS),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(43),CL5'TAGS',AL1(MDTBIQ,1)  TAGS                
         DC    AL1(LOPTAG,0),AL1(MDUSE,MDESO,OPTAG),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(44),CL5'DEMOS',AL1(MDTBIQ,1) DEMO                
         DC    AL1(LOPDEMO,0),AL1(MDUSE,MDESO,OPDEMO),AL2(0,0)                  
         DC    AL1(MDULNQ),AL2(45),CL5'PCT',AL1(MDTBCQ,2)   PERCENT             
         DC    AL1(LOPPCT,0),AL1(0,MDESO,OPPCT),AL2(0,0)                        
         DC    AL1(MDULNQ),AL2(46),CL5'AMT',AL1(MDTBCQ,4)   AMOUNTS             
         DC    AL1(LOPAMT,0),AL1(0,MDESO,OPAMT),AL2(0,0)                        
*                                                                               
         DC    AL1(MDULNQ),AL2(47),CL5'CYC',AL1(MDTDTQ,3)   CYCLE               
         DC    AL1(LOPCYC,0),AL1(MDCOMB,MDESO,OPCYC)                            
         DC    AL2(0,ICYC-T702E2)                                               
         DC    AL1(MDULNQ),AL2(48),CL5'DATE',AL1(MDTDTQ,3)  DATE                
         DC    AL1(LOPDATE,0),AL1(MDUSE,MDESO,OPDATE),AL2(0,0)                  
*                                                                               
         DC    AL1(MDULNQ),AL2(49),CL5'PLIFT',AL1(MDTMDQ,1) PYMT TO LFT         
         DC    AL1(LOPL,0),AL1(MDUSE,MDESO,OPL),AL2(0,0)                        
         DC    AL1(MDULNQ),AL2(78),CL5'PLIF2',AL1(MDTMDQ,1) PYMT TO 2ND         
         DC    AL1(LOPS,0),AL1(MDUSE,MDESO,OPS),AL2(0,0)            LFT         
         DC    AL1(MDULNQ),AL2(50),CL5'LIFT ',AL1(MDTMDQ,1) LIFT                
         DC    AL1(LOPLIFT,0),AL1(0,MDESO,OPLIFT),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(51),CL5'LIFTO',AL1(MDTMDQ,1) LIFT ONLY           
         DC    AL1(LOPLONLY,0),AL1(0,MDESO,OPLONLY),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(52),CL5'XCLDE',AL1(MDTMDQ,1) NO                  
         DC    AL1(LOPNO,0),AL1(0,MDESO,OPNO),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(53),CL5'APPLY',AL1(MDTCHQ,1) APP SES/HLD         
         DC    AL1(LOPAPPLY,0),AL1(0,MDESO,OPAPPLY),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(54),CL5'LAST',AL1(MDTDTQ,3)  LAST SERV           
         DC    AL1(LOPLAST,0),AL1(0,MDESO,OPLAST),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(55),CL5'ADDUM',AL1(MDTCHQ,2) ADDEN ST            
         DC    AL1(LOPSTATE,0),AL1(0,MDESO,OPSTATE),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(56),CL5'COMNM',AL1(MDTCHQ,0) HYPO NAME           
         DC    AL1(LOPCOMNM,0),AL1(0,MDESO,OPCOMNM)                             
         DC    AL2(OCOMNM-PROCEL,ICOMNM-T702E2)                                 
*                                                                               
         DC    AL1(MDULNQ),AL2(59),CL5'HLDDT',AL1(MDTDTQ,3) HLD DT              
         DC    AL1(LOPHLDAS,0),AL1(MDCOMB,MDESO,OPHLDAS)    (AS/OF)             
         DC    AL2(0,IHLD-T702E2)                                               
         DC    AL1(MDULNQ),AL2(59),CL5'HLDDT',AL1(MDTDTQ,3) HLD DT              
         DC    AL1(LOPHLDRA,0),AL1(MDCOMB,MDESO,OPHLDRAS)   (AS/OF)             
         DC    AL2(0,IHLD-T702E2)                                               
         DC    AL1(MDULNQ),AL2(57),CL5'HLD',AL1(MDTBCQ,4)   HLD AMT             
         DC    AL1(LOPHLD,0),AL1(0,MDESO,OPHLD),AL2(0,0)                        
         DC    AL1(MDULNQ),AL2(58),CL5'HLDRT',AL1(MDTBCQ,4) HLD RT              
         DC    AL1(LOPHLDR,0),AL1(0,MDESO,OPHLDR),AL2(0,0)                      
*                                                                               
         DC    AL1(MDULNQ),AL2(60),CL5'FIRST',AL1(MDTDTQ,3) FIRST               
         DC    AL1(LOP1ST,0),AL1(0,MDESO,OP1ST),AL2(0,0)                        
         DC    AL1(MDULNQ),AL2(61),CL5'PNH',AL1(MDTBCQ,4)   PNH                 
         DC    AL1(LOPPNH,0),AL1(0,MDESO,OPPNH),AL2(0,0)                        
         DC    AL1(MDULNQ),AL2(62),CL5'PNHRT',AL1(MDTBCQ,4) PNH RATE            
         DC    AL1(LOPPNHR,0),AL1(0,MDESO,OPPNHR),AL2(0,0)                      
*                                                                               
         DC    AL1(MDULNQ),AL2(63),CL5'SPNH',AL1(MDTBCQ,4)  SPNH                
         DC    AL1(LOPSPNH,0),AL1(0,MDESO,OPSPNH),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(64),CL5'CAN',AL1(MDTBCQ,4)   CAN RATE            
         DC    AL1(LOPCANTX,0),AL1(0,MDESO,OPCANTX),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(65),CL5'COMM',AL1(MDTBCQ,4)  COMM RATE           
         DC    AL1(LOPCOMM,0),AL1(0,MDESO,OPCOMM),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(66),CL5'EOR',AL1(MDTBCQ,4)   EOR RATE            
         DC    AL1(LOPEORTX,0),AL1(0,MDESO,OPEORTX),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(67),CL5'EXCH',AL1(MDTBCQ,4)  EXCHG RATE          
         DC    AL1(LOPEXCH,0),AL1(0,MDESO,OPEXCH),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(68),CL5'INCRP',AL1(MDTBCQ,4) INCORP RATE         
         DC    AL1(LOPINCTX,0),AL1(0,MDESO,OPINCTX),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(69),CL5'MULT',AL1(MDTBCQ,4)  MULTIPLIER          
         DC    AL1(LOPMULT,0),AL1(0,MDESO,OPMULT),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(70),CL5'INT',AL1(MDTBCQ,4)   INT FEES            
         DC    AL1(LOPINTEG,0),AL1(0,MDESO,OPINTEG),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(71),CL5'AFM',AL1(MDTCHQ,1)   AFM RATE            
         DC    AL1(LOPAFM,0),AL1(0,MDESO,OPAFM),AL2(0,0)                        
         DC    AL1(MDULNQ),AL2(72),CL5'HNDTX',AL1(MDTBCQ,4) HAND TAX            
         DC    AL1(LOPHNDTX,0),AL1(0,MDESO,OPHNDTX),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(73),CL5'CSF',AL1(MDTBCQ,4)   CSF                 
         DC    AL1(LOPCSF,0),AL1(MDUSE,MDESO,OPCSF)                             
         DC    AL2(OCSF-PROCEL,ICSF-T702E2)                                     
         DC    AL1(MDULNQ),AL2(74),CL5'HIST',AL1(MDTMDQ,1)  HIST ONLY           
         DC    AL1(LOPHIST,0),AL1(MDUSE,MDESO,OPHIST),AL2(0,0)                  
         DC    AL1(MDULNQ),AL2(75),CL5'ATYP',AL1(MDTCHQ,4)  COMML ATYPE         
         DC    AL1(LOPATYP,0),AL1(0,MDESO,OPATYP)                               
         DC    AL2(OATYPE-PROCEL,IATYPE-T702E2)                                 
         DC    AL1(MDULNQ),AL2(76),CL5'NAP',AL1(MDTDTQ,3)   NAP DATE            
         DC    AL1(LOPNAP,0),AL1(0,MDESO,OPNAP),AL2(0,0)                        
         DC    AL1(MDULNQ),AL2(77),CL5'LCLAU',AL1(MDTBIQ,2) LAST CLAUSE         
         DC    AL1(LOPLCLAU,0),AL1(MDUSE,MDESO,OPLCLAU)                         
         DC    AL2(OLCLAU-PROCEL,0)                                             
         DC    AL1(MDULNQ),AL2(79),CL5'2LIFT',AL1(MDTMDQ,1) 2ND LIFT            
         DC    AL1(LOPSONLY,0),AL1(0,MDESO,OPSONLY),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(80),CL5'2LIFT',AL1(MDTMDQ,1) 2ND LIFT            
         DC    AL1(LOPLONLY,0),AL1(0,MDESO,OPLONLY),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(81),CL5'2LIFT',AL1(MDTMDQ,1) 2LIFT ONLY          
         DC    AL1(LOPSLIFT,0),AL1(0,MDESO,OPSLIFT),AL2(0,0)                    
         DC    AL1(MDULNQ),AL2(82),CL5'ALL  ',AL1(MDTMDQ,1) ALL                 
         DC    AL1(LOPALLIN,0),AL1(0,MDESO,OPALLIN),AL2(0,0)                    
         DC    AL1(0)                                                           
ELTAESX  EQU   *                                                                
*                                                                               
ELTAGU   DC    AL1(ELTAGUD-*),AL2(TAGUELQ),AL2(ELTAGUX-ELTAGU)                  
         DC    AL1(0),CL2'GU',AL1(TAGULNQ)                                      
ELTAGUD  DC    AL1(MDULNQ),AL2(01),CL5'GUAR',AL1(MDTCHQ,L'TGGUA)                
         DC    AL2(0),AL1(0,0,0),AL2(DGRT-PROCEL,0)                             
         DC    AL1(MDULNQ),AL2(02),CL5'STRT',AL1(MDTDTQ,L'TAGUSTRT)             
         DC    AL2(TAGUSTRT-TAGUD),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(03),CL5'END',AL1(MDTDTQ,L'TAGUEND)               
         DC    AL2(TAGUEND-TAGUD),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(MDULNQ),AL2(04),CL5'BAL',AL1(MDTBCQ,L'TAGUBAL)               
         DC    AL2(TAGUBAL-TAGUD),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(MDULNQ),AL2(0),CL5' ',AL1(0,0)                               
         DC    AL2(TAGUCOM-TAGUD),AL1(MDZERO,0,0)                               
         DC    AL2(DCOMG-PROCEL,0)                                              
         DC    AL1(MDULNQ),AL2(05),CL5'EDAT',AL1(MDTDTQ,L'TGDATE)               
         DC    AL2(0),AL1(0,0,0),AL2(DGCDTE-PROCEL,0)                           
         DC    AL1(MDULNQ),AL2(06),CL5'CID',AL1(MDTCHQ,L'TGCID)                 
         DC    AL2(TAGUCOM-TAGUD),AL1(0,0,0),AL2(DCID-PROCEL,0)                 
         DC    AL1(0)                                                           
ELTAGUX  EQU   *                                                                
*                                                                               
ELTAOA   DC    AL1(ELTAOAD-*),AL2(TAOAELQ),AL2(ELTAOAX-ELTAOA)                  
         DC    AL1(0),CL2'OA',AL1(TAOALNQ)                                      
ELTAOAD  DC    AL1(MDULNQ),AL2(01),CL5'USE',AL1(MDTCHQ,L'TAOAUSE)               
         DC    AL2(TAOAUSE-TAOASBEL),AL1(0,0,0),AL2(DUSE-PROCEL,0)              
         DC    AL1(MDULNQ),AL2(02),CL5'AMT',AL1(MDTBCQ,L'TAOAAMT)               
         DC    AL2(TAOAAMT-TAOASBEL),AL1(0,0,0),AL2(0,0)                        
         DC    AL1(0)                                                           
ELTAOAX  EQU   *                                                                
*                                                                               
ELTAOP   DC    AL1(ELTAOPD-*),AL2(TAOPELQ),AL2(ELTAOPX-ELTAOP)                  
         DC    AL1(MHUSXGU),CL2'OP',AL1(TAOPLNQ)                                
ELTAOPD  DC    AL1(MDULNQ),AL2(01),CL5'USE',AL1(MDTCHQ,L'TAOPUSE)               
         DC    AL2(TAOPUSE-TAOPSBEL),AL1(0,0,0),AL2(DUSE-PROCEL,0)              
         DC    AL1(MDULNQ),AL2(02),CL5'PCT',AL1(MDTBCQ,L'TAOPPCT)               
         DC    AL2(TAOPPCT-TAOPSBEL),AL1(0,0,0),AL2(0,0)                        
         DC    AL1(0)                                                           
ELTAOPX  EQU   *                                                                
*                                                                               
ELTAUH   DC    AL1(ELTAUHD-*),AL2(TAUHELQ),AL2(ELTAUHX-ELTAUH)                  
         DC    AL1(0),CL2'UH',AL1(TAUHLNQ)                                      
ELTAUHD  DC    AL1(MDULNQ),AL2(01),CL5'USE',AL1(MDTCHQ,L'TGUSE)                 
         DC    AL2(TGUSCDE-TGD),AL1(MDTGD,0,0),AL2(0,0)                         
         DC    AL1(MDULNQ),AL2(02),CL5'TYPE',AL1(MDTCHQ,L'TGUSTYCD)             
         DC    AL2(TAUHTYPE-TAUHD),AL1(0,0,0),AL2(DUSET-PROCEL,0)               
         DC    AL1(MDULNQ),AL2(03),CL5'CYCS',AL1(MDTDTQ,L'TAUHSTRT)             
         DC    AL2(TAUHSTRT-TAUHD),AL1(0,0,0),AL2(0,0)                          
         DC    AL1(MDULNQ),AL2(04),CL5'CYCE',AL1(MDTDTQ,L'TAUHEND)              
         DC    AL2(TAUHEND-TAUHD),AL1(0,0,0),AL2(0,0)                           
         DC    AL1(MDULNQ),AL2(05),CL5'USE',AL1(MDTBIQ,L'TAUHUSN)               
         DC    AL2(TAUHUSN-TAUHD),AL1(0,0,MDTUHA),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(06),CL5'STAT',AL1(MDTBIQ,L'TAUHSTAT)             
         DC    AL2(TAUHSTAT-TAUHD),AL1(0,0,MDUHI),AL2(DUHST-PROCEL,0)           
         DC    AL1(MDULNQ),AL2(07),CL5'DEMS',AL1(MDTBIQ,L'TAUHDEMS)             
         DC    AL2(TAUHDEMS-TAUHD),AL1(0,0,MDTUHB),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(08),CL5'TAGS',AL1(MDTBIQ,L'TAUHTAGS)             
         DC    AL2(TAUHTAGS-TAUHD),AL1(0,0,MDTUHC),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(09),CL5'RMAJ',AL1(MDTBIQ,L'TAUHRMAJ)             
         DC    AL2(TAUHRMAJ-TAUHD),AL1(0,0,MDTUHD),AL2(0,0)                     
         DC    AL1(MDULNQ),AL2(10),CL5'INS',AL1(MDTBIQ,L'TAUHINS)               
         DC    AL2(TAUHINS-TAUHD),AL1(0,0,MDUHE),AL2(0,0)                       
         DC    AL1(MDULNQ),AL2(12),CL5'FMAJ',AL1(MDTBIQ,L'TAUHFMAJ)             
         DC    AL2(TAUHFMAJ-TAUHD),AL1(0,0,MDUHF),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(13),CL5'CBUN',AL1(MDTBIQ,L'TAUHCBUN)             
         DC    AL2(TAUHCBUN-TAUHD),AL1(0,0,MDUHG),AL2(0,0)                      
         DC    AL1(MDULNQ),AL2(14),CL5'UNIT',AL1(MDTBIQ,L'TAUHUNT)              
         DC    AL2(TAUHUNT-TAUHD),AL1(MDZERO,0,MDUHI),AL2(0,0)                  
         DC    AL1(MDULNQ),AL2(15),CL5'MAJ',AL1(MDTBIQ,L'TAUHMAJ)               
         DC    AL2(TAUHMAJ-TAUHD),AL1(0,0,MDUHI),AL2(0,0)                       
         DC    AL1(0)                                                           
ELTAUHX  EQU   *                                                                
*                                                                               
ELTAW4   DC    AL1(ELTAW4D-*),AL2(TAW4ELQ),AL2(ELTAW4X-ELTAW4)                  
         DC    AL1(0),CL2'W4',AL1(TAW4LNQ)                                      
ELTAW4D  DC    AL1(MDULNQ),AL2(01),CL5'FRST',AL1(MDTCHQ,L'TGNAME)               
         DC    AL2(0),AL1(0,0,0),AL2(DSSNF-PROCEL,0)                            
         DC    AL1(MDULNQ),AL2(02),CL5'LAST',AL1(MDTCHQ,L'TGNAME)               
         DC    AL2(0),AL1(0,0,0),AL2(DSSNL-PROCEL,0)                            
         DC    AL1(0)                                                           
ELTAW4X  EQU   *                                                                
*                                                                               
         DC    AL1(0)              END OF ELEMENT TABLE                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE INITIALIZES ESTIMATE RECORD FOR UPLOAD                   
         SPACE 1                                                                
INITES   NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AWRKIO          SET IOAREA                                   
         MVI   EMSEQ,X'F1'         START TAEMSEQ AT 241 LIKE MAINT. PGM         
         MVI   ENSEQ,X'01'         START TAENSEQ AT 1 LIKE MAINT. PGM           
         XC    ESSEQS,ESSEQS       START ALL TAESSEQ'S TO ZERO                  
         MVI   ESOPTCNT,0          START # OF OPTIONS AT ZERO                   
         SPACE 1                                                                
         L     R4,AIO              INITIALIZE ESTIMATE RECORD                   
         USING TLESD,R4                                                         
         XC    TLESKEY,TLESKEY                                                  
         MVI   TLESCD,TLESCDQ                                                   
         MVC   TLESAGY,TGAGY                                                    
         MVC   TLESEST,TGEST                                                    
         MVC   TLESLEN,DATADISP    SET STARTING LENGTH                          
         XC    TLESSTAT(10),TLESSTAT                                            
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TRANSLATES VERSION INFORMATION INTO LIFT FORMAT      *         
*        ON ENTRY ... R4=A(CURRENT ELEMENT)                           *         
***********************************************************************         
                                                                                
PROVERS  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R4),TACAELQ       AND CURRENT ELEMENT IS CAST DETAILS          
         BNE   PVERSX                                                           
         BAS   RE,PROVTACO         PROCESS COMMERCIAL LEVEL VERSIONS            
         BAS   RE,PROVTACA         PROCESS CAST LEVEL VERSIONS                  
PVERSX   XIT1                                                                   
                                                                                
***********************************************************************         
*        ROUTINE TRANSLATES COMMERCIAL-LEVEL VERSION INFORMATION      *         
*        INTO LIFT FORMAT                                             *         
***********************************************************************         
                                                                                
PROVTACO NTR1                                                                   
         MVI   TGVER,0                                                          
                                                                                
         USING TLCAD,R4                                                         
         L     R4,AIO                                                           
         MVC   TGCOM,TLCACOM                                                    
         DROP  R4                                                               
                                                                                
         MVC   SVKEY,KEY                                                        
         MVC   AIO,ATMPIO                                                       
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)                                   
         BNE   PVTACO20                                                         
                                                                                
         USING TAVRD,R4                                                         
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TAVRELQ      READ VERSION ELEMENTS                        
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
PVTACO10 BRAS  RE,NEXTEL                                                        
         BNE   PVTACO20                                                         
                                                                                
         CLI   TAVRVERS,2          IF ELEMENT FOR VERSION 2                     
         BNE   *+8                 IS FOUND ...                                 
         OI    TGVER,TAVRV2Q       VERSION 2                                    
         CLI   TAVRVERS,3          IF ELEMENT FOR VERSION 2                     
         BNE   PVTACO10            IS FOUND ...                                 
         OI    TGVER,TAVRV3Q       VERSION 3                                    
         B     PVTACO10                                                         
         DROP  R4                                                               
                                                                                
PVTACO20 MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         MVC   AIO,SVAIO                                                        
         B     PVERSX                                                           
                                                                                
***********************************************************************         
*        ROUTINE TRANSLATES CAST-LEVEL VERSION INFORMATION INTO       *         
*        LIFT FORMAT                                                  *         
***********************************************************************         
                                                                                
PROVTACA CLI   TGVER,0             EXIT IF NOT TREATING COMMERCIAL WITH         
         BE    PVERSX              VERSIONS LIKE COMMERCIAL WITH LIFT           
                                                                                
         MVI   BYTE1,0                                                          
         MVI   BYTE2,0                                                          
                                                                                
         USING TAFND,R4                                                         
         L     R4,AIO              AIO=A(CAST RECORD)                           
         MVI   ELCODE,TAFNELQ      GET CAST'S VERSION ELEMENT                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
PVTACA10 BRAS  RE,NEXTEL                                                        
         BNE   PVTACA70                                                         
         CLI   TAFNTYPE,TAFNTVER                                                
         BNE   PVTACA10                                                         
                                                                                
         CLI   TAFNNAME,251        IF CAST IS ON ALL VERSIONS                   
         BNE   *+12                                                             
         OI    BYTE2,TACASALL                                                   
         B     PVTACA70            CAST IS ON MASTER AND LIFT                   
                                                                                
         MVI   TGBYTE,0                                                         
                                                                                
         ZIC   R3,TAFNLEN                                                       
         SHI   R3,3                                                             
         LA    RF,TAFNNAME                                                      
PVTACA30 CLI   0(RF),1                                                          
         BNE   *+12                                                             
         OI    TGBYTE,MASTERQ      ON MASTER                                    
         B     PVTACA40                                                         
         CLI   0(RF),2                                                          
         BNE   *+12                                                             
         OI    TGBYTE,FIRSTQ       ON 1ST LIFT                                  
         B     PVTACA40                                                         
         CLI   0(RF),3                                                          
         BNE   *+8                                                              
         OI    TGBYTE,SECONDQ      ON 2ND LIFT                                  
PVTACA40 AHI   RF,1                                                             
         BCT   R3,PVTACA30                                                      
                                                                                
         TM    TGBYTE,MASTERQ+FIRSTQ+SECONDQ ON MASTER+1ST+2ND LIFT?            
         BNO   *+12                                                             
         MVI   BYTE2,TACASALL      ON MASTER AND ALL LIFTS                      
         B     PVTACA70                                                         
                                                                                
         TM    TGBYTE,MASTERQ      ON MASTER?                                   
         BZ    PVTACA50                                                         
         TM    TGBYTE,FIRSTQ       AND 1ST LIFT?                                
         BZ    *+12                                                             
         MVI   BYTE1,TACASTLF      ON MASTER + 1ST LIFT                         
         B     PVTACA70                                                         
         TM    TGBYTE,SECONDQ      AND 2ND LIFT?                                
         BZ    *+12                                                             
         MVI   BYTE2,TACAS2LF      ON MASTER + 2ND LIFT                         
         B     PVTACA70                                                         
                                                                                
PVTACA50 TM    TGBYTE,FIRSTQ       ON 1ST LIFT?                                 
         BZ    PVTACA60                                                         
         TM    TGBYTE,SECONDQ      AND 2ND LIFT?                                
         BZ    *+12                                                             
         MVI   BYTE2,TACASL2O      ON BOTH                                      
         B     *+8                                                              
         MVI   BYTE1,TACASTLO      ONLY ON 1ST LIFT                             
         B     PVTACA70                                                         
                                                                                
PVTACA60 TM    TGBYTE,SECONDQ      ON 2ND LIFT?                                 
         BZ    *+8                                                              
         OI    BYTE2,TACAST2O      ON 2ND LIFT ONLY                             
                                                                                
         USING TACAD,R4                                                         
PVTACA70 L     R4,AIO              GET CAST DETAILS ELEMENT                     
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         OC    TACASTAT,BYTE1      AND SET LIFT STATUS                          
         OC    TACASTA4,BYTE2      AND SET LIFT STATUS                          
         B     PVERSX                                                           
         DROP  R4                                                               
                                                                                
BYTE1    DS    XL1                                                              
BYTE2    DS    XL1                                                              
                                                                                
MASTERQ  EQU   X'80'               ON MASTER                                    
FIRSTQ   EQU   X'40'               ON 1ST LIFT                                  
SECONDQ  EQU   X'20'               ON 2ND LIFT                                  
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO INITIALIZE SOME STUFF ON RECEIVE                      
         SPACE 1                                                                
INITRCV  NTR1  BASE=*,LABEL=*                                                   
         MVI   OVERLAY,X'06'       GET A(TAESTPRT) FOR ESR UPLOAD               
         GOTO1 LOADSOPH,DMCB,0                                                  
         ST    R3,AESTPRT                                                       
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT USED FOR ESR UPLOAD            
         XC    FLTDATA,FLTDATA           KEY FILTERING DATA                     
         MVI   STESTAT,0                 PROCESSING STATUS                      
         MVI   STESTAT2,0                PROCESSING STATUS #2                   
         MVC   ANEXTUHM,AKEYUHM                                                 
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TRANSLATES FIRST PERCENTAGE FROM NEW STYLE           *         
*        OVERSCALE 2ND PERCENTAGE ELEMENT INTO CAST DETAILS ELEMENT   *         
*        ON ENTRY ... AIO = A(CAST RECORD)                            *         
*                     R4  = A(CURRENT ELEMENT)                        *         
***********************************************************************         
                                                                                
         USING TAO2D,R4                                                         
PROTAO2  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R4),TAO2ELQ       IF CURRENT ELEMENT IS OVERSCALE              
         JNE   XIT                 2ND PERCENTAGE ...                           
         MVC   FULL,TAO2PCT        SAVE THE 1ST PERCENTAGE ...                  
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   TACAOV2,FULL        ... INTO THE CAST DETAILS ELEMENT            
         J     XIT                                                              
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*        ROUTINE TO READ HIGH FOR ACTIVE COMML PTR                              
         SPACE 1                                                                
         USING TLCOD,R4                                                         
BLDKCO   NTR1  BASE=*,LABEL=*                                                   
         MVI   MYKCODEQ,TLCOCDQ     SAVE KEY READING                            
         MVI   TLCOCD,TLCOCDQ                                                   
         MVC   TLCOAGY,FLTAGY      SET AGENCY                                   
         MVC   TLCOCLI,FLTCLI      SET CLIENT                                   
         MVC   MYKLEN,LCOCLI       MINIMUM KEY EXPECTED                         
         CLC   FLTSTRT,SPACES                                                   
         BNH   *+10                                                             
         MVC   TLCOCID,FLTSTRT                                                  
         CLC   FLTPRD,SPACES       IF PRODUCT                                   
         JNH   XIT                                                              
         MVC   TLCOPRD,FLTPRD      SET IT                                       
         MVC   MYKLEN,LCOPRD                                                    
         J     XIT                                                              
         SPACE 2                                                                
*        ROUTINE TO READ HIGH FOR CLIENT GROUP COMML PTR                        
         SPACE 1                                                                
         USING TLCOPD,R4                                                        
BLDKCOG  NTR1  BASE=*,LABEL=*                                                   
         MVI   MYKCODEQ,TLCOGCDQ                                                
         MVI   TLCOPCD,TLCOGCDQ                                                 
         MVC   TLCOGCLG,FLTCLG     SET CLIENT GROUP                             
         MVC   MYKLEN,LCOGCLG                                                   
         CLC   FLTSTRT,SPACES                                                   
         BNH   *+10                                                             
         MVC   TLCOGCID,FLTSTRT                                                 
         CLC   FLTAGY,SPACES       IF AGENCY SPECIFIED                          
         JNH   XIT                                                              
         MVC   TLCOGAGY,FLTAGY     SET IT                                       
         J     XIT                                                              
         EJECT                                                                  
*        ROUTINE TO READ HIGH FOR COMML NAME PTR                                
         SPACE 1                                                                
         USING TLCOPD,R4                                                        
BLDKCON  NTR1  BASE=*,LABEL=*                                                   
         MVI   MYKCODEQ,TLCONCDQ                                                
         MVI   TLCOPCD,TLCONCDQ                                                 
         MVC   TLCONCLI,FLTCLI     SET CLIENT                                   
         MVC   MYKLEN,LCONCLI      MINIMUM KEY EXPECTED                         
         CLC   FLTSTRT,SPACES                                                   
         JNH   *+10                                                             
         MVC   TLCONAME,FLTSTRT                                                 
         J     XIT                                                              
         SPACE 2                                                                
*        ROUTINE TO READ HIGH FOR COMML CLIENT GROUP NAME PTR                   
         SPACE 1                                                                
         USING TLCOPD,R4                                                        
BLDKCOL  NTR1  BASE=*,LABEL=*                                                   
         MVI   MYKCODEQ,TLCOLCDQ                                                
         MVI   TLCOPCD,TLCOLCDQ                                                 
         MVC   TLCOLCLG,FLTCLG     SET CLIENT GROUP                             
         MVC   MYKLEN,LCOLCLG      MINIMUM KEY EXPECTED                         
         CLC   FLTSTRT,SPACES                                                   
         JNH   *+10                                                             
         MVC   TLCOLNAM,FLTSTRT                                                 
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*        ROUTINE TO ADJUST USE                                                  
                                                                                
ADJUSE   NTR1  BASE=*,LABEL=*                                                   
         CLC   0(3,R2),=C'LFT'     IF USE IS LIFT                               
         JE    AUSE10                                                           
         CLC   0(3,R2),=C'ALF'     ADDENDUM LIFT                                
         JE    AUSE10                                                           
         CLC   0(3,R2),=C'SLF'     OR SPANISH LIFT                              
         JNE   AUSE20                                                           
AUSE10   MVC   3(5,R2),=CL5'FFC'   CHANGE TO FFC                                
         J     XIT                                                              
                                                                                
AUSE20   CLC   0(3,R2),=C'MVM'     IF USE IS MVM                                
         JNE   AUSE30                                                           
         CLC   3(5,R2),=CL5'1YR'   AND TYPE IS 1 YEAR                           
         JNE   XIT                                                              
         MVC   3(5,R2),=CL5'52WK'  CHANGE TO 52 WEEKS                           
         J     XIT                                                              
                                                                                
AUSE30   CLC   0(3,R2),=C'NIM'     IF USE IS NIM                                
         JNE   AUSE40                                                           
         CLC   3(5,R2),=CL5'6MO'   AND TYPE IS 6 MONTHS                         
         JNE   AUSE30A                                                          
         MVC   3(5,R2),=CL5'26WK'  CHANGE TO 26 WEEKS                           
         J     XIT                                                              
                                                                                
AUSE30A  CLC   3(5,R2),=CL5'1YR'   IF USE IS NIM                                
         JNE   XIT                 AND TYPE IS 1 YEAR                           
         MVC   3(5,R2),=CL5'52WK'  CHANGE TO 52 WEEKS                           
         J     XIT                                                              
                                                                                
AUSE40   CLC   0(3,R2),=C'MUS'     IF USE IS MUS                                
         JNE   AUSE50                                                           
         CLC   3(5,R2),=CL5'D6'    AND TYPE IS DUB 6 MONTHS                     
         JNE   AUSE40A                                                          
         MVC   3(5,R2),=CL5'D26WK' CHANGE TO 26 WEEKS                           
         J     XIT                                                              
                                                                                
AUSE40A  CLC   3(5,R2),=CL5'D1'    IF USE IS MUS                                
         JNE   AUSE40B             AND TYPE IS DUB 1 YEAR                       
         MVC   3(5,R2),=CL5'D52WK' CHANGE TO 52 WEEKS                           
         J     XIT                                                              
                                                                                
AUSE40B  CLC   3(5,R2),=CL5'DS6'   IF USE IS MUS                                
         JNE   AUSE40C             AND TYPE IS DUB SHORT 6 MONTHS               
         MVC   3(5,R2),=CL5'DS26'  CHANGE TO DUB SHORT 26 WEEKS                 
         J     XIT                                                              
                                                                                
AUSE40C  CLC   3(5,R2),=CL5'DS1'   IF USE IS MUS                                
         JNE   XIT                 AND TYPE IS DUB SHORT 1 YEAR                 
         MVC   3(5,R2),=CL5'DS52'  CHANGE TO DUB SHORT 52 WEEKS                 
         J     XIT                                                              
                                                                                
AUSE50   CLC   0(3,R2),=C'NBM'     IF USE IS NBM                                
         JNE   XIT                                                              
         CLC   3(5,R2),=CL5'1YR'   AND TYPE IS 1 YEAR                           
         JNE   AUSE50A                                                          
         MVC   3(5,R2),=CL5'52WK'  CHANGE TO 52 WEEKS                           
         J     XIT                                                              
                                                                                
AUSE50A  CLC   3(5,R2),=CL5'2YR'   IF USE IS NBM                                
         JNE   XIT                 AND TYPE IS 2 YEARS                          
         MVC   3(5,R2),=CL5'2-52W' CHANGE TO 2 X 52 WEEKS                       
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TACHPACK                                                       
       ++INCLUDE FALINKBLK                                                      
                                                                                
TAVRV2Q  EQU   X'80'               VERSION 2                                    
TAVRV3Q  EQU   X'40'               VERSION 3                                    
         EJECT                                                                  
                                                                                
*              DSECT TO COVER USER DEFINED AREAS OF MAPTAB                      
         SPACE 1                                                                
MHELD    DSECT                                                                  
         ORG   MHUSER                                                           
MHUSTAT  DS    XL1                 STAT                                         
MHUSKEY  EQU   X'80'               FAKE ELEMENT FOR KEY                         
KEYSL    EQU   X'40'               THIS INVOLVES A LIST                         
MHUSCP   EQU   X'20'               FOR CLI/PROD RECORD ONLY                     
MHUSES   EQU   X'10'               FOR ESTIMATE RECORD ONLY                     
MHUSESL  EQU   X'08'               FOR ESTIMATE LIST ONLY                       
MHUSINI  EQU   X'04'               FOR MISCTAB PROCESSING ONLY                  
MHUSXGU  EQU   X'02'               EXCLUDE FROM GUL PROCESSING                  
*                                                                               
MHUUEL   DS    0C                  TRUE ELEMENT HEADER                          
MHUTEXT  DS    CL2                 TEXT INSTEAD OF ELEMENT CODE                 
MHUBLEN  DS    XL1                 ELEMENT LENGTH (BASE)                        
         ORG   MHUUEL              FAKE ELEMENT HEADER (MHUSKEY ON)             
MHUKTXT  DS    CL3                 TEXT INSTEAD OF KEY CODE                     
MHUKCDE  DS    XL1                 REAL KEY EQUATE                              
MHUKLEN  DS    XL1                 LENGTH OF KEY COMPARE ON HIGH                
MHUKCLR  DS    XL2                 DISP TO RTN TO CLEAR TGD FIELDS              
         DS    XL1                 SPARE                                        
         SPACE 2                                                                
MDELD    DSECT                                                                  
         ORG   MDUSER                                                           
MDUDISP  DS    XL2                 DISP. OF FIELD INTO ELEMENT                  
         ORG   MDUDISP                                                          
MDULEN   DS    XL1                 L'ESTIMATE SUB-OPTION                        
         DS    XL1                                                              
MDUSTAT  DS    XL1                 ELEMENT STATUS                               
MDCOMB   EQU   X'80'               COMBINATION TAESD SUB-OPTION                 
MDXDWN   EQU   X'40'               SKIP ON DOWNLOAD                             
MDZERO   EQU   X'20'               FORCE ZERO                                   
MDCLEN   EQU   X'10'               CALCULATE DATA LENGTH                        
MDXUHM   EQU   X'08'               SKIP ON UHM DOWNLOAD                         
MDTGD    EQU   X'04'               DISPLACEMENT OFF OF TGD                      
MDINI    EQU   X'02'               FOR INI DOWNLOAD ONLY                        
MDUSE    EQU   X'01'               USE RELATED SUBELEMENT                       
MDUSTAT2 DS    XL1                 SECOND ELEMENT STATUS                        
MDLOCL   EQU   X'80'               DISPLACEMENT OFF OF FLTDATA                  
MDCOMP   EQU   X'40'               COMPLEMENT DATA                              
MDNOCLI  EQU   X'20'               NO CLIENT REQUIRED FOR PRD INPUT             
MDESO    EQU   X'10'               ESTIMATE SUB-OPTION                          
MDUCODE  DS    XL1                 TRUE ESTIMATE OPTIONS CODE                   
         ORG   MDUCODE                                                          
MDUTYPE2 DS    XL1                 OR ELEMENT FIELD PROCESSING TYPE             
MDTUHA   EQU   C'A'                                                             
MDTUHB   EQU   C'B'                                                             
MDTUHC   EQU   C'C'                                                             
MDTUHD   EQU   C'D'                                                             
MDUHE    EQU   C'E'                                                             
MDUHF    EQU   C'F'                                                             
MDUHG    EQU   C'G'                                                             
MDUHI    EQU   C'I'                                                             
MDESCL   EQU   C'S'                                                             
MDESPR   EQU   C'T'                                                             
MDESCO   EQU   C'U'                                                             
MDESPER  EQU   C'V'                                                             
MDESHCO  EQU   C'W'                                                             
MDESHP   EQU   C'X'                                                             
MAPFCO   EQU   C'Z'                                                             
MDURCV   DS    XL2                 DISP. OF VAL RTN ON MF SEND                  
MDUUP    DS    XL2                 DISP. OF FIELD RTN ON MF RECEIVE             
MDULNQ   EQU   (*-MDUSER)+MDELDL                                                
         EJECT                                                                  
*              DSECT TO COVER COMBINATION OPTION TABLE                          
         SPACE 1                                                                
COMBTABD DSECT                                                                  
COMBTLEN DS    XL1                 LENGTH OF ENTRY                              
COMBOPT  DS    XL1                 ESTIMATE OPTION                              
COMBOLEN DS    XL1                 L'ESTIMATE OPTION                            
COMBDET  DS    0XL3                DETAIL OF OPTION                             
COMBDNUM DS    XL2                 MAPCODE OF DETAIL                            
COMBDDSP DS    XL1                 DISPLACEMENT INTO OPTION OF DETAIL           
         SPACE 2                                                                
*              DUMMY ELEMENT FOR COMBINED MUSICIANS RECORD                      
         SPACE 1                                                                
TA01D    DSECT                                                                  
TA01EL   DS    XL1                 ELEMENT CODE                                 
TA01ELQ  EQU   X'01'                                                            
TA01LEN  DS    XL1                 ELEMENT LENGTH                               
TA01CAM  DS    CL3                 CAMERA                                       
TA01UN   DS    CL3                 UNION                                        
TA01CORP DS    CL1                 CORP NUMBER                                  
TA01DBL  DS    CL1                 DOUBLES                                      
TA01OV1  DS    XL4                 FIRST OVERSCALE                              
TA01OV2  DS    XL4                 SECOND OVERSCALE                             
TA01CNT  DS    XL2                 COUNT OF MUSICIANS                           
TA01LNQ  EQU   *-TA01D             ELEMENT LENGTH EQUATE                        
         SPACE 2                                                                
*              DUMMY ELEMENT FOR COMMERCIAL TYPES KEPT IN TABLE                 
         SPACE 1                                                                
TA02D    DSECT                                                                  
TA02EL   DS    XL1                 ELEMENT CODE                                 
TA02ELQ  EQU   X'02'                                                            
TA02LEN  DS    XL1                 ELEMENT LENGTH                               
TA02CTYP DS    CL1                 COMMERCIAL TYPE CODE                         
TA02CTYN DS    CL(L'TGCTNAME-1)    COMMERCIAL TYPE NAME                         
TA02AHLD DS    CL1                 Y=COMM'L GET AUTO HOLDING FEES               
TA02LNQ  EQU   *-TA02D             ELEMENT LENGTH EQUATE                        
         SPACE 2                                                                
*              DUMMY ELEMENT FOR COMMERCIAL TYPES KEPT IN TABLE                 
         SPACE 1                                                                
TA03D    DSECT                                                                  
TA03EL   DS    XL1                 ELEMENT CODE                                 
TA03ELQ  EQU   X'03'                                                            
TA03LEN  DS    XL1                 ELEMENT LENGTH                               
TA03ADST DS    CL2                 ADDENDUM STATE CODE                          
TA03ADEM DS    CL(L'TGTANAME)      ADDENDUM STATE NAME                          
TA03LNQ  EQU   *-TA03D             ELEMENT LENGTH EQUATE                        
         EJECT                                                                  
*              DUMMY ELEMENT FOR CAN. COMMERCIAL TYPES KEPT IN TABLE            
         SPACE 1                                                                
TA04D    DSECT                                                                  
TA04EL   DS    XL1                 ELEMENT CODE                                 
TA04ELQ  EQU   X'04'                                                            
TA04LEN  DS    XL1                 ELEMENT LENGTH                               
TA04CCDE DS    CL4                 CANADIAN COMML TYPE CODE                     
TA04CCN  DS    CL(L'TGCCTNM)       CANADIAN COMML NAME                          
TA04LNQ  EQU   *-TA04D             ELEMENT LENGTH EQUATE                        
         SPACE 2                                                                
*              DUMMY ELEMENT FOR ESTIMATE RECORD IN ESL DOWNLOAD                
         SPACE 1                                                                
TA05D    DSECT                                                                  
TA05EL   DS    XL1                 ELEMENT CODE                                 
TA05ELQ  EQU   X'05'                                                            
TA05LEN  DS    XL1                 ELEMENT LENGTH                               
TA05ID   DS    CL20                ESTIMATE ID                                  
TA05NAME DS    CL36                ESTIMATE NAME                                
TA05RDTE DS    XL3                 REPORT DATE                                  
TA05CDTE DS    XL3                 LAST CHANGED DATE                            
TA05LNQ  EQU   *-TA05D             ELEMENT LENGTH EQUATE                        
         SPACE 2                                                                
*              DUMMY ELEMENT FOR WITH VERSION INFO FOR WARNING OR               
*              OR ERROR OF HIGHER VERSION REQUIRED                              
         SPACE 1                                                                
TAFED    DSECT                                                                  
TAFEEL   DS    XL1                 ELEMENT CODE                                 
TAFEELQ  EQU   X'FE'                                                            
TAFELEN  DS    XL1                 ELEMENT LENGTH                               
TAFEVER  DS    XL4                 VERSION ID (XYZZ)                            
TAFELNQ  EQU   *-TAFED             ELEMENT LENGTH EQUATE                        
         SPACE 2                                                                
*              DUMMY ELEMENT FOR WITH VERSION INFO W/O WARNING OR ERR           
         SPACE 1                                                                
TAFDD    DSECT                                                                  
TAFDEL   DS    XL1                 ELEMENT CODE                                 
TAFDELQ  EQU   X'FD'                                                            
TAFDLEN  DS    XL1                 ELEMENT LENGTH                               
TAFDVER  DS    XL4                 VERSION ID (XYZZ)                            
TAFDLNQ  EQU   *-TAFDD             ELEMENT LENGTH EQUATE                        
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         SPACE                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRE2D                                                       
         EJECT                                                                  
*              DSECT TO COVER LOCAL SAVED STORAGE                               
         SPACE 2                                                                
STEREOD  DSECT                                                                  
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
AWRKIO   DS    A                   A(WORKING IO AREA)                           
AMAINIO  DS    A                   A(MAIN IO AREA)                              
AMISCTAB DS    A                   A(MISCTAB)                                   
AKEYUHM  DS    A                   A(AREA FOR KEY)                              
ATMPIO   DS    A                   A(TEMP IO)                                   
WRKIOL   EQU   (L'IO*3)+L'IO+MISCTABL+COMTABL+L'IO                              
*                                                                               
ARTNS    DS    0A                  RELO'D RTNS (SET EACH HIT OF ENTER)          
AGETEREC DS    A                                                                
AFILTREC DS    A                                                                
APRESCID DS    A                                                                
ADOSETEL DS    A                                                                
APROCEL  DS    A                                                                
AGETNTRY DS    A                                                                
AADDTAB  DS    A                                                                
AMYADDL  DS    A                                                                
ASPLIT   DS    A                                                                
ADELETE  DS    A                                                                
AKEYSUHL DS    A                                                                
AKEYSGUL DS    A                                                                
AMAPTAB  DS    A                                                                
AOPTS    DS    A                                                                
*                                                                               
ATWABLD  DS    A                   A(TWABLD)                                    
AFALINK  DS    A                   A(FALINK)                                    
AESTPRT  DS    A                   A(TAESTPRT)                                  
*                                                                               
ASETELEM DS    A                   A(SETELEM) IN FALINK                         
AADDDATA DS    A                   A(ADDDATA) IN FALINK                         
AGETDATA DS    A                   A(GETDATA) IN FALINK                         
*                                                                               
ANEXTUHM DS    A                   POINTER INTO AKEYUHM                         
AKEYNTRY DS    A                   A(CURRENT KEY ENTRY IN TABLE)                
AELENTRY DS    A                   A(CURRENT ELEMENT ENTRY IN TABLE)            
ACOMBTAB DS    A                   A(CURRENT ENTRY IN COMBTAB)                  
ANXTCOMB DS    A                   A(DETAIL IN CURRENT COMBTAB ENTRY)           
SVAIO    DS    A                   A(SAVED IOAREA)                              
AELEM    DS    A                   A(SAVED ELEMENT)                             
ANXTSUB  DS    A                   A(NXT POSITION FOR ESTIMATE SUB-OPT)         
*                                                                               
MYKLEN   DS    XL1                 CURRENT KEY COMPARE LENGTH                   
ELETYPE  DS    CL1                 ELEMENT TYPE FOR TRAVERSING ELTAB            
ELBLEN   DS    XL1                 BASE ELEMENT LENGTH                          
SVELCODE DS    XL1                 SAVED ELEMENT CODE                           
HAVEAC40 DS    CL1                 HAVE 40 ACTIVITY ELEMENT                     
ANYMORE  DS    CL1                 Y=MORE ELEMENTS TO COME                      
WRKFLAG  DS    CL1                 MISC. FLAG                                   
SVKEY    DS    XL(L'KEY)           SAVED CAST KEY                               
*                                                                               
ELFDATA  DS    CL100               ELEMENT FIELD DATA                           
*                                                                               
MYKCODEQ DS    XL1                 KEY EQUATE (MAIN READ)                       
MYKCODE  DS    CL3                 KEY CODE   (MAIN READ)                       
*                                                                               
SMYKEY   DS    CL(L'TLDRKEY)       SAVED KEY FOR CONTINUATION                   
FRSTKEY  DS    CL(L'TLDRKEY)       FIRST KEY                                    
FRSTKEYA DS    CL(L'TLDRKEY)       FIRST KEY OF SECOND READ                     
SVLSTINF DS    0XL9                SAVED RECORD INFO FOR LISTING                
SVUHUSE  DS    CL3                 USE                                          
SVUHCYC  DS    CL6                 CYCLE DATES                                  
*                                                                               
FLTDATA  DS    0CL(FLTLNQ)         FILTER RECORD ELEMENTS                       
FLTKEYQ  DS    XL1                 CODE OR NAME                                 
FLTAGY   DS    CL6                 AGENCY                                       
FLTCLI   DS    CL6                 CLIENT                                       
FLTPRD   DS    CL6                 PRODUCT                                      
FLTMED   DS    CL1                 MEDIA                                        
FLTCLG   DS    CL6                 CLIENT GROUP                                 
FLTSTRT  DS    CL20                START CODE OR NAME                           
FLTREL   DS    CL1                 SHOW RELEASED COMMERCIALS                    
FLTLOCK  DS    CL1                 SHOW LOCKED COMMERCIALS                      
FLTPDTE  DS    XL3                 COMML ACTIVITY DATE                          
FLTRDTE  DS    XL3                 ESTIMATE REPORT DATE                         
FLTCDTE  DS    XL3                 ESTIMATE CHANGED DATE                        
FLTACTN  DS    CL1                 U=UPLD,N=OKAY NEW,D=DEL,R=RESTORE            
FLTRDFLG DS    XL1                 Y = <=REPORT DATE                            
FLTCDFLG DS    XL1                 Y = <=CHANGED DATE                           
FLTMUS   DS    CL1                 EXCLUDE MUSIC COMMERCIALS                    
FLTMIND  DS    CL1                 INDIVIDUAL MUSICIANS                         
FLTLNQ   EQU   *-FLTKEYQ                                                        
*                                                                               
STESTAT  DS    XL1                 STATUS OPTIONS                               
STEDATA  EQU   X'80'               PASSED DATA TO FALINK                        
STERERD  EQU   X'40'               RE-ESTABLISH DIRECTORY PTR                   
STETAB   EQU   X'20'               PROCESSING TABLE                             
STENEWC  EQU   X'08'               PROCESSING NEW COMML ON UHM                  
STESKEY  EQU   X'04'               PROCESSING KEY DATA                          
STESINIT EQU   X'02'               INITIALIZED ESTIMATE FOR UPLOAD              
STEIOBRK EQU   X'01'               SET I'M BREAKING                             
*                                                                               
STESTAT2 DS    XL1                 2ND STATUS OPTIONS                           
STEWRITE EQU   X'80'                                                            
STESVERS EQU   X'40'               PROCESSING VERSION INFO                      
STESCHKV EQU   X'20'               CHECK VERSION NUMBERS                        
*                                                                               
TAESCNTS DS    0X                  SEQUENCE NUMBERS & COUNTS FOR TAESD          
EMSEQ    DS    XL1                 FOR TAEMD                                    
ENSEQ    DS    XL1                 FOR TAEND                                    
ESOPTCNT DS    XL1                 FOR TAESD                                    
HYPCNTS  DS    0XL2                                                             
HPGCNT   DS    XL1                 PAGE NUMBER HYPO PERF DISPLAYED ON           
HYPCNT   DS    XL1                 COUNT OF HYPO PERFORMERS PER PAGE            
MAXHYP   EQU   7                   MAX NUM OF HYP PERFS PER PAGE                
*                                                                               
ESSEQS   DS    0XL4                SEQUENCE NUMBERS FOR ALL TAESD ELS           
SVCLISEQ DS    XL1                                                              
SVPRDSEQ DS    XL1                                                              
SVCOSEQ  DS    XL1                                                              
SVCASEQ  DS    XL1                                                              
ESSEQSX  EQU   *                                                                
*                                                                               
FRSTAGY  DS    CL6                 FIRST LIMITED AGENCY READ                    
LASTAGY  DS    CL6                 LAST LIMITED AGENCY READ                     
*                                                                               
SVCODES  DS    0XL25               SAVED CODES FOR COL/CAL/UHM OPTIM            
SVMED    DS    XL1                                                              
SVCLI    DS    CL6                                                              
SVPRD    DS    CL6                                                              
SVCAT    DS    CL3                                                              
SVCAM    DS    CL3                                                              
SVUNI    DS    CL3                                                              
SVYEAR   DS    CL3                                                              
SVCODESX EQU   *                                                                
*                                                                               
FABLK    DS    CL(FALINKDL)        BLOCK FOR FALINK                             
FPARMS   DS    6F                  RETURN PARMETER BLK FROM GETDATA             
FAMSGBLK DS    CL(FAMSGDL)         BLOCK FOR FALINK ERROR MESSAGES              
FACON    DS    CL(L'FALCON)        BLOCK FOR FALINK CONTROL FIELD               
*                                                                               
MISCTABL EQU   1200                                                             
COMTABL  EQU   1200                                                             
MAXCOM   EQU   300                                                              
MAXTBL   EQU   50                                                               
MAXUH    EQU   300                 USED FOR EL=ES TY CO                         
*                                                                               
VERSNO   DS    XL4                 CURRENT VERSION NUMBER                       
*                                                                               
SVFALINK DS    XL1024              FALINK'S SAVED STORAGE                       
*              ALL ADDITIONS MUST BE BEFORE THIS POINT                          
MYSPARE  DS    CL(L'TWAHOLE-(*-STEREOD))                                        
         DS    0X                                                               
         SPACE 2                                                                
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'101TAGENE2   05/12/15'                                      
         END                                                                    
