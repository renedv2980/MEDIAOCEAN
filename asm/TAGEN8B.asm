*          DATA SET TAGEN8B    AT LEVEL 058 AS OF 02/04/10                      
*PHASE T7028BA,*                                                                
         TITLE 'T7028B - FIXED CYCLE TRACKING MANUAL ADJUSTMENTS'               
T7028B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7028B                                                         
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROL                                                     
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    FT10                                                             
         MVC   SFTSHED(7),=C'Pid Num'                                           
         OI    SFTSHEDH+6,X'80'                                                 
*                                                                               
FT10     CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   *+12                                                             
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   *+12                                                             
         BAS   RE,DELETE                                                        
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    FT20                                                             
         CLI   MODE,XRECPUT        OR AFTER CHANGING RECORD                     
         BE    FT20                                                             
         CLI   MODE,XRECADD        AFTER ADDING RECORD                          
         BNE   XIT                                                              
         MVC   CONACT,=CL8'CHANGE'                                              
         OI    CONACTH+5,X'80'                                                  
         SPACE 1                                                                
FT20     BAS   RE,DISPLAY          (RE-)DISPLAY IT                              
         SPACE 1                                                                
         CLI   MODE,DISPREC        IF NOT JUST DISPLAYING                       
         BE    XIT                                                              
         BRAS  RE,CCAST            UPDATE BALANCE IN CAST RECORD                
         BAS   RE,DISTACR          RE-DISPLAY TACREL DETAILS                    
         BAS   RE,DISCGRR          DISPLAY COVERED GRR DETAILS                  
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
*******  CLI   ACTNUM,ACTDEL       IF ACTION DELETE                             
*******  BNE   VK10                                                             
*******  TM    TGCTSTLV,X'E0'      ONLY ALLOW IF LEVEL 3 OR HIGHER              
*******  BZ    ERRSEC                                                           
VK10     CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   *+12                                                             
         TM    SFTSSNH+4,X'20'     OR SSN HAS CHANGED                           
         BO    VK40                                                             
*                                                                               
         LA    R2,SFTSSNH          S/S NUM                                      
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK20                                                             
         CLI   SFTSSNH+5,0                                                      
         BE    FLDMISS                                                          
         CLI   SFTSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    VK20                RECVAL CALL DOES NOT CHECK FOR               
         CLI   SFTSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SFTSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK20                                                             
         MVC   SFTSSN,TGSSN                                                     
         MVI   SFTSSNH+5,9                                                      
*                                                                               
VK20     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SFTSSNH),SFTSSNNH                     
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK30                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SFTSSN,SPACES                                                    
         MVC   SFTSSN(L'TGPID),TGPID                                            
         MVI   SFTSSNH+5,6                                                      
         OI    SFTSSNH+6,X'80'                                                  
*                                                                               
VK30     NI    SFTAGYH+4,X'DF'     FORCE AGENCY RE-VALIDATION                   
         SPACE 1                                                                
VK40     TM    SFTAGYH+4,X'20'     TEST AGENCY CHANGED                          
         BO    VK50                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SFTAGYH),SFTAGYNH                     
         NI    SFTCIDH+4,X'DF'     FORCE CID RE-VALIDATION                      
         SPACE 1                                                                
VK50     TM    SFTCIDH+4,X'20'     TEST COMMERCIAL ID CHANGED                   
         BO    VK60                                                             
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',SFTCIDH),SFTCIDNH                    
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   TGCOM,TLCOCOM       SAVE INTERNAL COMMERCIAL NUMBER              
         NI    SFTCATH+4,X'DF'     FORCE CATEGORY RE-VALIDATION                 
         SPACE 1                                                                
VK60     LA    R2,SFTCATH                                                       
         TM    4(R2),X'20'                                                      
         BZ    *+12                                                             
         TM    SFTPDH+4,X'20'                                                   
         BO    *+8                                                              
         NI    STATUS,X'FF'-DELPEND-NOTRACK                                     
         BAS   RE,VALCAT           ALWAYS VALIDATE CATEGORY                     
         SPACE 1                                                                
         BAS   RE,VALGCU           VALIDATE GRR COVERED USE                     
         SPACE 1                                                                
         BAS   RE,VALPD            VALIDATE PERIOD                              
         SPACE 1                                                                
         BAS   RE,VALNPD           VALIDATE NEW PERIOD                          
         SPACE 1                                                                
         CLI   ACTNUM,ACTDEL       IF DELETING LEAVE CAST KEY                   
         BE    *+8                                                              
         BAS   RE,BLDKEY           ELSE BUILD TRACKING KEY                      
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CATEGORY FIELD                               
         SPACE 1                                                                
VALCAT   NTR1                                                                   
         LA    R2,SFTCATH          R2=A(FIELD)                                  
         GOTO1 ANY                                                              
         GOTO1 CATVAL,DMCB,WORK    VALIDATE CATEGORY CODE                       
         BE    VC10                                                             
         XC    TGCAT,TGCAT                                                      
         SPACE 1                                                                
         MVC   FULL(3),=3C'0'      IF NOT VALID TEST IF VALID HEX               
         ZIC   R1,5(R2)                                                         
         LA    RF,L'FULL                                                        
         SR    RF,R1                                                            
         LA    RF,FULL(RF)         SET TO RIGHT-ALIGN IN FULL                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
         SPACE 1                                                                
         GOTO1 HEXIN,DMCB,FULL,TGFULL,4                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    FLDINV                                                           
         SPACE 1                                                                
VC10     XC    KEY,KEY             BUILD CAST KEY                               
         LA    R3,KEY                                                           
         USING TLCAPD,R3                                                        
         MVI   TLCAPCD,TLCACCDQ                                                 
         MVC   TLCACSSN,TGSSN      SOCIAL SECURITY NUMBER                       
         MVC   TLCACCOM,TGCOM      INTERNAL COMMERCIAL NUMBER                   
         MVC   TLCACCAT,TGCAT      CATEGORY                                     
         GOTO1 HIGH                                                             
*                                                                               
VC20     CLC   TLCAPKEY(TLCACCAT-TLCAPD),KEYSAVE  STILL SAME COMML              
         BNE   FLDINV                                                           
         OC    TGCAT,TGCAT         IF CATEGORY INPUT                            
         BZ    VC30                                                             
         CLC   TLCACCAT,TGCAT      TAKE FIRST MATCH                             
         BNE   VC40                                                             
         OC    TGCSORT+4(2),TGCSORT+4    ANY CAST SEQ?                          
         BZ    VC50                      NO, USE FIRST MATCH                    
         CLC   TLCACSEQ,TGCSORT+4        YES, MAKE SURE THEY MATCH THEN         
         BE    VC50                                                             
         B     VC40                                                             
*                                                                               
VC30     CLC   TLCACSEQ,TGFULL     ELSE SCAN FOR MATCHING SEQUENCE NO.          
         BE    VC50                                                             
*                                                                               
VC40     GOTO1 SEQ                 GET NEXT CAST RECORD                         
         B     VC20                AND KEEP ON TRYING                           
*                                                                               
VC50     MVI   RDUPDATE,C'Y'       SET TO LOCK CAST RECORD                      
         GOTO1 GETREC              GET CAST RECORD FOR PD VAL.                  
*                                                                               
         MVC   CSTDA,DMDSKADD      SAVE D/A FOR LATER                           
*                                                                               
         GOTO1 HEXOUT,DMCB,TLCACSEQ,FULL,2,0  CVT SEQ TO CHAR.                  
         MVC   CSTSEQ,FULL+1                  SAVE 3 LOBS                       
*                                                                               
         MVC   TGCSORT+4(2),TLCACSEQ  SAVE SEQUENCE NUMBER FOR KEY              
*                                                                               
         MVC   8(3,R2),TLCACCAT    DISPLAY ACTUAL CATEGORY                      
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'         SET VALIDATED                                
         NI    SFTPDH+4,X'DF'      FORCE PERIOD RE-VALIDATION                   
*                                                                               
         USING TACAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   VC60                                                             
         TM    TACASTA2,TACASPUS   IF GRR AMOUNT IS DUE ONCE PER USE            
         BZ    VC60                PER CYCLE                                    
         NI    SFTCU1HH+1,X'FB'                                                 
         OI    SFTCU1HH+6,X'80'                                                 
         NI    SFTCUS1H+1,X'D3'    PRESENT FIELDS TO SPECIFY COVERED            
         NI    SFTCU2HH+1,X'F3'    USE                                          
         OI    SFTCU2HH+6,X'80'                                                 
         NI    SFTCUS2H+1,X'D3'                                                 
         OI    SFTCUS2H+6,X'80'                                                 
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
VC60     GOTO1 FLDVAL,DMCB,(8,SFTCU1HH),(8,SFTCUS1H)                            
         GOTO1 FLDVAL,DMCB,(8,SFTCU2HH),(8,SFTCUS2H)                            
         XC    SFTCUS1,SFTCUS1                                                  
         MVI   SFTCUS1H+5,0                                                     
         XC    SFTCUS2,SFTCUS2     ELSE, HIDE AND CLEAR FIELDS TO               
         MVI   SFTCUS2H+5,0        SPECIFY COVERED USE                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE GRR COVERED USE                              
VALGCU   NTR1                                                                   
         CLI   SFTNPDH+5,0         IF ADDING A NEW PERIOD                       
         BNE   XIT                 SKIP VALIDATION OF THIS FIELD                
                                                                                
         XC    APPGRRCD,APPGRRCD                                                
                                                                                
         TM    SFTCUS1H+1,X'20'    EXIT IF GRR COVERED USE IS PROTECTED         
         BO    XIT                                                              
                                                                                
         LA    R2,SFTCUS1H         R2=A(GRR COVERED USE FIELD)                  
         CLI   5(R2),0             IF THERE IS INPUT                            
         BE    XIT                                                              
         CLI   5(R2),3             IT MUST BE 3 CHARACTERS LONG                 
         BNE   FLDINV                                                           
                                                                                
         USING USETABD,RE                                                       
         L     RE,TGAUSES          RE=A(USE TABLE)                              
VGCU10   CLI   0(RE),X'FF'         IF END OF TABLE REACHED, INPUT               
         BE    FLDINV              IS NOT A VALID USE                           
         CLC   USECDE,8(R2)                                                     
         BE    VGCU20                                                           
         LH    RF,USELEN           BUMP TO NEXT USE ENTRY                       
         AR    RE,RF                                                            
         B     VGCU10                                                           
                                                                                
VGCU20   TM    USEMEDS,RADIO       WHEN USE IS FOUND, ENSURE IT IS              
         BZ    FLDINV              VALID FOR RADIO                              
         MVC   APPGRRCD,USECDE     AND SAVE COVERED USE                         
         MVC   APPGRREQ,USEEQU                                                  
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE PERIOD FIELD                                 
         SPACE 1                                                                
VALPD    NTR1                                                                   
         LA    R2,SFTPDH                                                        
         CLI   5(R2),0             IF NOTHING INPUT                             
         BNE   VP1                                                              
         CLI   SFTNPDH+5,0         AND NEW PERIOD INPUT                         
         BNE   VPX                 PERIOD NOT REQUIRED                          
         B     VP2                 ELSE LOOK FOR LAST CYCLE ON RECORD           
         SPACE 1                                                                
VP1      LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)     VALIDATE PERIOD                              
         USING PERVALD,R3                                                       
         SPACE 1                                                                
VP2      L     R4,AIO              USING CAST RECORD                            
         MVI   ELCODE,TACRELQ      LOOK FOR APPROPRIATE TACREL                  
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         XC    LSTINV(L'LSTINV+L'LSTINVPD),LSTINV                               
         BAS   RE,GETEL                                                         
         BNE   NOTRK                                                            
         USING TACRD,R4                                                         
         SPACE 1                                                                
VP4      OC    APPGRRCD,APPGRRCD   IF FILTERING ON USE                          
         BZ    VP4A                                                             
         CLC   APPGRRCD,TACRUSE    ACCEPT FTRACKS FOR THE USE                   
         BE    VP4A                                                             
         CLC   TACRUSE,=C'GRR'     AND RADIO GUARANTEES THAT COVER              
         BNE   VP6                 THE USE                                      
         CLC   APPGRREQ,TACRTYPE                                                
         BNE   VP6                                                              
         SPACE 1                                                                
VP4A     CLC   LSTINV,TACRINV      TEST THIS INV LATER THAN SAVED               
         BH    VP5                                                              
         LR    R0,R4               SAVE R0=A(VERY LAST ONE FOUND)               
         MVC   LSTINV,TACRINV      AND ITS INV NUM                              
VP5      CLI   5(R2),0             IF HAVE INPUT                                
         BE    VP6                                                              
         CLC   PVALPSTA,TACRSTRT   MATCH ON START DATE                          
         BE    VP8                                                              
VP6      BAS   RE,NEXTEL           TRY NEXT ELEMENT                             
         BE    VP4                                                              
         CLI   5(R2),0             NO MORE - IF NO PD INPUT                     
         BNE   VP10                                                             
         LR    R4,R0               USE VERY LAST ELEMENT FOUND                  
         B     VP15                                                             
         SPACE 1                                                                
VP8      CLC   LSTINVPD,TACRINV    TEST THIS INV LATER THAN SAVED               
         BH    VP6                                                              
         LR    R1,R4               SAVE R1=A(LAST EL WITH RIGHT DATES)          
         MVC   LSTINVPD,TACRINV    AND ITS INV NUM                              
         B     VP6                                                              
         SPACE 1                                                                
VP10     LTR   R4,R1               HAVE PD INPUT                                
         BZ    NOTRK               TEST IF FOUND EL WITH CORRECT DATES          
         SPACE 1                                                                
VP15     GOTO1 DATCON,DMCB,(X'11',TACRSTRT),(8,8(R2))  DISPLAY DATES            
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         EDIT  (4,TACRAPPL),(12,SFTAMT),2,MINUS=YES,ALIGN=LEFT                  
         OI    SFTAMTH+6,X'80'                                                  
         SPACE 1                                                                
         MVC   BALANCE,TACRBAL     SAVE BALANCE                                 
         MVC   CRSTAT,TACRSTAT     AND STATUS                                   
         BAS   RE,DISTACR          RE-DISPLAY TACREL DETAILS                    
         SPACE 1                                                                
         MVC   PCYC,TACRSTRT       SAVE PWOS DATES                              
         SPACE 1                                                                
         ST    R4,ATACREL          SAVE A(DISPLAYED TACR ELEMENT)               
         BAS   RE,DISCGRR          DISPLAY COVERED GRR DETAILS                  
VPX      OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS APPLIED CREDI HISTORY EL. DETAILS               
         SPACE 1                                                                
DISTACR  DS    0H                                                               
         EDIT  (4,BALANCE),(12,SFTBAL),2,MINUS=YES,ALIGN=LEFT                   
         NI    SFTBALH+1,X'F3'     SET TO NORMAL INTENSITY                      
         OI    SFTBALH+6,X'80'                                                  
         SPACE 1                                                                
         TM    BALANCE,X'80'       IF BALANCE IS NEGATIVE                       
         BZ    *+8                                                              
         OI    SFTBALH+1,X'08'     SET TO HIGH INTENSITY                        
         SPACE 1                                                                
         MVI   SFTGUAR,C'N'                                                     
         TM    CRSTAT,TACRSGUA                                                  
         BZ    *+8                                                              
         MVI   SFTGUAR,C'Y'                                                     
         OI    SFTGUARH+6,X'80'                                                 
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE DISPLAYS GRR COVERED USE DETAILS                         
         SPACE 1                                                                
DISCGRR  NTR1                                                                   
         USING TACRD,R4                                                         
         L     R4,ATACREL          R4=A(DISPLAYED TACR ELEMENT)                 
                                                                                
         CLC   TACRUSE,=C'GRR'     IF TACR IS A GRR                             
         BNE   XIT                                                              
         CLI   TACRTYPE,0          THAT COVERS A SPECIFIC USE                   
         BE    XIT                                                              
         CLI   SFTCUS1H+5,3        AND COVERED USE FIELD DOES                   
         BE    XIT                 NOT CONTAIN INPUT ...                        
                                                                                
         USING USETABD,RE                                                       
         L     RE,TGAUSES          RE=A(USE TABLE)                              
DCGRR10  CLI   0(RE),X'FF'         IF END OF TABLE REACHED, INPUT               
         BE    XIT                 IS NOT A VALID USE                           
         CLC   USEEQU,TACRTYPE                                                  
         BE    DCGRR20                                                          
         LH    RF,USELEN           BUMP TO NEXT USE ENTRY                       
         AR    RE,RF                                                            
         B     DCGRR10                                                          
         DROP  R4                                                               
                                                                                
DCGRR20  MVC   SFTCUS1,USECDE      FILL IN COVERED USE                          
         MVI   SFTCUS1H+5,3                                                     
         OI    SFTCUS1H+6,X'80'                                                 
         MVC   APPGRRCD,USECDE     AND SAVE COVERED USE                         
         MVC   APPGRREQ,USEEQU                                                  
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              ROUTINE VALIDATES NEW PERIOD FIELD                               
         SPACE 1                                                                
VALNPD   NTR1                                                                   
         MVI   NEWGRRTY,0          INITIALIZE COVERED USE                       
         SPACE 1                                                                
         LA    R2,SFTNPDH          R2=A(NEW PERIOD FIELD)                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         CLI   ACTNUM,ACTADD       MUST BE ACTION ADD                           
         BNE   NOINPUT                                                          
         SPACE 1                                                                
         LA    R3,BLOCK            R4=A(PERVAL BLOCK)                           
         GOTO1 PDVAL,DMCB,(R3)     VALIDATE PERIOD                              
         USING PERVALD,R3                                                       
         MVC   PCYC,PVALPSTA       SAVE PERIOD                                  
         SPACE 1                                                                
         LA    R2,SFTUSEH          VALID SOURCE USE TYPE                        
         GOTO1 ANY                                                              
         GOTO1 USEVAL,DMCB,(X'40',8(R2))                                        
         BNE   FLDINV                                                           
         TM    TGUSSTA2,APPREUSE   MUST BE APPLICABLE TOWARDS REUSE             
         BZ    FLDINV                                                           
         SPACE 1                                                                
         BAS   RE,ADDCUSE          VALIDATE THE "ADD COVERED USE" FIELD         
         SPACE 1                                                                
         LA    R2,SFTNPDH          R2=A(NEW PERIOD FIELD)                       
         SPACE 1                                                                
         L     R4,AIO              R3=A(CAST RECORD)                            
         MVI   ELCODE,TACRELQ      INSURE THIS CYCLE DOESN'T OVERLAP            
         BAS   RE,GETEL            ANY EXISTING CYCLES                          
         BNE   VNPD8                                                            
         USING TACRD,R4            R3=A(APPLIED CREDITS HISTORY EL.)            
         SPACE 1                                                                
VNPD2    CLI   NEWGRRTY,0          IF GRR COVERED USE IS ENTERED                
         BE    VNPD3                                                            
         CLC   NEWGRRTY,TACRTYPE   ONLY CONSIDER EXISTING CYCLES                
         BNE   VNPD6               THAT COVER THAT SAME USE                     
         SPACE 1                                                                
VNPD3    CLC   PCYCS,TACRSTRT      COMPARE NEW START TO EXISTING                
         BE    OVERLAP             ERROR IF SAME                                
         BH    VNPD4               SKIP IF LATER                                
         CLC   PCYCE,TACRSTRT      IF EARLIER, THEN SO MUST END DATE            
         BNL   OVERLAP                                                          
         B     VNPD6               OK - LOOK FOR ANOTHER CYCLE                  
         SPACE 1                                                                
VNPD4    CLC   PCYCS,TACREND       START BEGINS AFTER EXISTING START            
         BNH   OVERLAP             SO IT MUST START AFTER EXISTING END          
         SPACE 1                                                                
VNPD6    BAS   RE,NEXTEL           LOOK FOR ANOTHER CYCLE ELEMENT               
         BE    VNPD2                                                            
         SPACE 1                                                                
VNPD8    LA    R2,SFTVERH          INSURE INPUT IS VERIFIED                     
         GOTO1 ANY                                                              
         CLI   WORK,C'Y'                                                        
         BNE   FLDINV                                                           
         XC    BALANCE,BALANCE     SET NO CURRENT BALANCE                       
         LA    R2,SFTAPPLH                                                      
         GOTO1 ANY                                                              
         SPACE                                                                  
         TM    SFTCUS2H+1,X'20'    IF GRR COVERED USE IS NOT PROTECTED          
         BO    XIT                                                              
         MVI   SFTCUS1H+5,3                                                     
         OI    SFTCUS1H+6,X'80'                                                 
         MVC   SFTCUS1,SFTCUS2     COPY IT TO TOP OF SCREEN                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES THE "GRR COVERED USE FIELD" WHEN               
*              ADDING AN FTRACK                                                 
                                                                                
ADDCUSE  NTR1                                                                   
         TM    SFTCUS2H+1,X'20'    EXIT IF GRR COVERED USE IS PROTECTED         
         BO    XIT                                                              
                                                                                
         LA    R2,SFTCUS2H         R2=A(GRR COVERED USE FIELD)                  
                                                                                
         CLI   TGUSEQU,UGRR        IF ADDING A NON-GRR USE                      
         BE    ACU10                                                            
         CLI   5(R2),0             FIELD MUST BE LEFT EMPTY                     
         BNE   FLDINV                                                           
         B     XIT                                                              
                                                                                
ACU10    CLI   5(R2),0             IF ADDING GRR USE                            
         BE    FLDMISS             INPUT IS REQUIRED                            
         CLI   5(R2),3                                                          
         BNE   FLDINV              AND MUST BE 3 CHARACTERS LONG                
                                                                                
         USING USETABD,RE                                                       
         L     RE,TGAUSES          RE=A(USE TABLE)                              
ACU20    CLI   0(RE),X'FF'         IF END OF TABLE REACHED, INPUT               
         BE    FLDINV              IS NOT A VALID USE                           
         CLC   USECDE,8(R2)                                                     
         BE    ACU30                                                            
         LH    RF,USELEN           BUMP TO NEXT USE ENTRY                       
         AR    RE,RF                                                            
         B     ACU20                                                            
                                                                                
ACU30    TM    USEMEDS,RADIO       WHEN USE IS FOUND, ENSURE IT IS              
         BZ    FLDINV              VALID FOR RADIO                              
         MVC   NEWGRRTY,USEEQU     AND SAVE COVERED USE                         
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              ROUTINE BUILDS APPROPRIATE TRACKING KEY                          
         SPACE 1                                                                
BLDKEY   NTR1                                                                   
         LA    R4,KEY              BUILD TRACKING KEY                           
         USING TLFTD,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLFTCD,TLFTCDQ      RECORD CODE                                  
         MVC   TLFTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLFTCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLFTCAST,TGCSORT+4  CAST INPUT SEQUENCE NUMBER                   
         MVC   TLFTSTRT(6),PCYC    CYCLE DATES                                  
         XC    TLFTSTRT(6),HEXFFS              (COMPLEMENTED)                   
         SPACE 1                                                                
         GOTO1 HIGH                GET LAST LOGICAL TRACKING RECORD             
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    BK10                                                             
         CLC   TLFTKEY(TLFTTRK-TLFTD),KEYSAVE  MUST HAVE TRACKING REC           
         BNE   *+14                                                             
         OC    TLFTINV,TLFTINV     CAN ONLY DISPLAY/CHANGE ADJUSTMENTS          
         BZ    *+8                                                              
         OI    STATUS,NOTRACK      SET NO TRACKING RECORD FOUND                 
         B     BKX                                                              
         SPACE 1                                                                
BK10     CLC   TLFTKEY(TLFTTRK-TLFTD),KEYSAVE  IF WE DIDN'T FIND IT             
         BE    *+16                                                             
         MVC   TLFTKEY,KEYSAVE                 RESTORE SAVED KEY                
         MVC   TLFTTRK,HEXFFS                  SET DUMMY LAST TRK NUMB.         
         SPACE 1                                                                
         LH    RF,TLFTTRK          LAST TRACKING NUMBER                         
         STH   RF,LASTTRK          SAVE IT                                      
         LCR   RF,RF               UN-COMPLEMENTED                              
         LA    RF,1(RF)            ADD 1                                        
         LCR   RF,RF               COMPLEMENT IT AGAIN                          
         STH   RF,NEXTTRK          SAVE NEXT NUMBER                             
         SPACE 1                                                                
         MVC   TLFTTRK,NEXTTRK     AND MOVE INTO KEY                            
         XC    TLFTINV,TLFTINV     INSURE THERE'S NO INVOICE NUMBER             
         SPACE 1                                                                
BKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS RECORD                                          
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SFTCYCH             CLEAR THE SCREEN                             
         SPACE 1                                                                
         TM    STATUS,NOTRACK      IF NO TRACKING RECORD FOUND                  
         BO    DISPX               GET OUT                                      
         SPACE 1                                                                
         XC    PREVCRD,PREVCRD                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TAGTELQ      GET GUAR. TRACKING EL.                       
         BAS   RE,GETEL                                                         
         BNE   DISP6               OK TO NOT BE AROUND                          
         USING TAGTD,R4            R4=A(GUARANTEE TRACKING EL.)                 
         SPACE 1                                                                
         OC    TAGTSTRT,TAGTSTRT   CYCLE DATES                                  
         BZ    DISP4                                                            
         GOTO1 DATCON,DMCB,(X'11',TAGTSTRT),(8,SFTCYC)                          
         SPACE 1                                                                
DISP4    MVC   PREVCRD,TAGTCRD     SAVE CREDIT AMOUNT                           
         EDIT  (4,TAGTCRD),(12,SFTAPPL),2,ALIGN=LEFT,FLOAT=-,          X        
               ZERO=BLANK                                                       
         SPACE 1                                                                
DISP6    GOTO1 CHAROUT,DMCB,TACMELQ,SFTCMNTH,TACMTYPH  COMMENT                  
         GOTO1 ACTVOUT,DMCB,SFTLCHGH                                            
*                                                                               
DISPX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILDS RECORD                                            
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         TM    STATUS,NOTRACK      IF NO TRACKING RECORD FOUND                  
         BZ    *+12                                                             
         MVI   IOOPT,C'Y'          SET WE'RE HANDLING I/O                       
         B     BLDRX               AND GET OUT                                  
         SPACE 1                                                                
         MVI   ELCODE,TAGTELQ      REMOVE EXISTING ELEMENT (IF AROUND)          
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     BUILD GUARANTEE TRACKING ELEMENT             
         LA    R4,ELEMENT                                                       
         USING TAGTD,R4            R4=A(GUARANTEE TRACKING ELEMENT)             
         SPACE 1                                                                
         LA    R2,SFTCYCH          VALIDATE CYCLE DATES                         
         LA    R3,BLOCK            R3=A(RETURN BLOCK FROM PERVAL)               
         GOTO1 PDVAL,DMCB,(X'80',(R3))  (INPUT OPTIONAL)                        
         USING PERVALD,R3                                                       
         MVC   TAGTSTRT(6),PVALPSTA  SAVE PWOS CYCLE DATES                      
         SPACE 1                                                                
         LA    R2,SFTAPPLH         GUARANTEE CREDITS                            
         GOTO1 ANY                                                              
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),X'FF'                                                      
         BE    AMTINV                                                           
         MVC   TAGTCRD,4(R1)                                                    
         SPACE 1                                                                
         CLI   SFTVER,C'Y'         IF ADDING A NEW FIXED CYCLE                  
         BNE   BLDR4                                                            
         CLC   SFTUSE,=C'GRR'      (FOR A NON-GRR USE)                          
         BE    BLDR4                                                            
         L     R1,TAGTCRD          CREDIT AMOUNT MUST BE POSITIVE               
         C     R1,=F'0'                                                         
         BNH   AMTINV                                                           
         SPACE 1                                                                
BLDR4    L     R1,BALANCE          PREVIOUS BALANCE                             
         A     R1,TAGTCRD          + APPLIED AMOUNT                             
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    *+8                                                              
         S     R1,PREVCRD          - PREVIOUS APPLIED AMOUNT                    
         ST    R1,BALANCE          = NEW BALANCE                                
         ST    R1,TAGTBAL                                                       
         SPACE 1                                                                
         CLI   SFTVER,C'Y'         IF ADDING AN ADJUSTMENT TO AN                
         BE    BLDR5               EXISTING CYCLE ...                           
         C     R1,=F'0'            BALANCE CANNOT GO BELOW ZERO                 
         BL    AMTINV                                                           
         SPACE 1                                                                
BLDR5    TM    SFTCUS1H+1,X'20'    IF ADDING APPLICATION TO GRR CYCLE           
         BO    BLDR6                                                            
         CLI   SFTNPDH+5,0         AND NOT ADDING A NEW PERIOD                  
         BNE   BLDR6                                                            
         MVC   TAGTUSE,APPGRRCD    SAVE COVERED USE                             
         OI    TAGTSTAT,TAGTSGRR   AND STATUS                                   
         SPACE 1                                                                
BLDR6    OC    ELEMENT,ELEMENT     ANYTHING IN ELEMENT                          
         BZ    BLDR8                                                            
         MVI   TAGTEL,TAGTELQ      YES - FINISH IT UP                           
         MVI   TAGTLEN,TAGTLNQ                                                  
         SPACE 1                                                                
         CLI   SFTNPDH+5,0         IF ADDING A NEW PERIOD                       
         BE    BLDR7                                                            
         CLI   NEWGRRTY,0          FOR A GRR COVERING A SPECIFIC USE            
         BE    BLDR7                                                            
         MVC   TAGTUSE,=C'GRR'     SAVE THE COVERED USE AND CYCLE               
         MVC   TAGTTYPE,NEWGRRTY   IN THE TRACKING RECORD                       
         MVC   TAGTSTRT(6),PCYC                                                 
         SPACE 1                                                                
BLDR7    GOTO1 ADDELEM             AND ADD IT                                   
         SPACE 1                                                                
BLDR8    GOTO1 NAMIN,DMCB,(2,TACMELQ),(X'80',SFTCMNTH),TACMTYPH COMMENT         
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY INFO                            
BLDRX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DELETE AN FTRACK CYCLE                                
         SPACE 1                                                                
DELETE   NTR1                                                                   
         TM    STATUS,DELPEND      IS DELETE PENDING                            
         BZ    PFTODEL             NO, SO SET NOW                               
         SPACE 1                                                                
         CLI   PFAID,20            TEST IT WAS PRESSED                          
         BNE   PFTODEL                                                          
         GOTO1 SAVPTRS,DMCB,PTRBLK  SAVE PASSIVE POINTERS                       
         SPACE 1                                                                
         MVI   ELCODE,TACRELQ                                                   
         GOTO1 GETL,DMCB,(6,PCYC)  INSURE APPLIED CREDIT EL AROUND              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DELL,DMCB,(6,PCYC)  DELETE IT (THEM)                             
         SPACE 1                                                                
*                                                                               
         ZIC   R0,TWASCR           SAVE THE VALUE IN TWASCR                     
         MVI   TWASCR,X'FF'                                                     
         GOTO1 ACTVIN,DMCB,(X'C0',0) CREATE TAACD EL WITHOUT ADDING IT          
         STC   R0,TWASCR           RESTORE THE VALUE  OF TWASCR                 
         LA    R4,ELEMENT                                                       
         USING TAACD,R4                                                         
         MVC   TAACTSCR,TWASCR     SAVE THE SCRN # IN TAACD ELEMENT             
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
         GOTO1 PUTREC               WRITE BACK THE RECORD                       
         GOTO1 ADDPTRS,DMCB,PTRBLK  UPDATE PASSIVE POINTERS                     
         SPACE 1                                                                
         LA    R4,KEY              BUILD KEY FOR TRACKING RECORDS               
         USING TLFTD,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLFTCD,TLFTCDQ      RECORD CODE                                  
         MVC   TLFTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLFTCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLFTCAST,TGCSORT+4  CAST INPUT SEQUENCE NUMBER                   
         MVC   TLFTSTRT(6),PCYC    CYCLE DATES                                  
         XC    TLFTSTRT(6),HEXFFS  (COMPLEMENTED)                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                GET FIRST RECORD                             
         SPACE 1                                                                
DEL4     CLC   TLFTKEY(TLFTTRK-TLFTD),KEYSAVE  TEST STILL CORRECT CYCLE         
         BNE   DELETED                                                          
         SPACE 1                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              GET THE RECORD                               
         L     R4,AIO                                                           
         OI    TLFTSTAT,X'80'      DELETE FILE RECORD                           
         GOTO1 PUTREC                                                           
         SPACE 1                                                                
         LA    R4,KEY                                                           
         OI    TLDRSTAT-TLDRD(R4),X'80'  DELETE DIRECTORY RECORD                
         GOTO1 WRITE                                                            
         SPACE 1                                                                
         OI    DMINBTS,X'08'       RE-READ RECORD I JUST DELETED                
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         SPACE 1                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                 LOOK FOR ANOTHER                             
         B     DEL4                                                             
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
NTFOUND  MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
         SPACE 1                                                                
ERRSEC   MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     THEEND                                                           
         SPACE 1                                                                
DELETED  MVI   MYMSGNO1,7          RECORD HAS BEEN DELETED                      
         MVI   MYMSYS,X'FF'                                                     
         B     INFEND                                                           
         SPACE 1                                                                
PFTODEL  OI    STATUS,DELPEND      SET DELETE PENDING                           
         LA    R2,CONACTH                                                       
         MVI   MYMSGNO1,56         PRESS PF20 TO CONFIRM DELETE                 
         B     INFEND                                                           
         SPACE 1                                                                
NOTADJ   MVI   ERROR,ERNOTADJ      ONLY MANUAL ADJS. ALLOWED                    
         B     THEEND                                                           
         SPACE 1                                                                
NOTRK    MVI   ERROR,ERNOTRK       NO TRACKING FOR PERF.                        
         B     THEEND                                                           
         SPACE 1                                                                
AMTINV   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     THEEND                                                           
         SPACE 1                                                                
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED IN THIS FIELD              
         B     THEEND                                                           
         SPACE 1                                                                
OVERLAP  MVI   ERROR,ERPDOVLP      NEW PERIOD OVERLAPS WITH EXISTING            
         B     THEEND                                                           
         SPACE 1                                                                
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CHECK   ',CL8'LIST    '                               
PF13     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'FTRACK  ',CL8'REPORT  '                               
PF14     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYTWA,L'CSTSEQ-1),AL2(CSTSEQ-T702FFD)                     
         DC    AL1(KEYTYTWA,8-1),AL2(SFTPD-T702FFD)                             
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3' ',CL8'FTRACK  ',CL8'LIST    '                               
PF15     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF15X    EQU   *                                                                
         DC    AL1(PF20X-*,20,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF20X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
HEXFFS   DC    6X'FF'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE ADJUSTS APPLIED CREDIT ELEMENT IN CAST RECORD            
         SPACE 1                                                                
CCAST    NTR1  BASE=*,LABEL=*                                                   
         MVC   KEY+TLDRDA-TLDRD(4),CSTDA   SET D/A OF CAST RECORD               
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2            USE ALT. I/O AREA                            
         GOTO1 GETREC              GET IT                                       
         SPACE 1                                                                
         CLI   SFTNPDH+5,0         IF ADDING NEW PERIOD                         
         JE    CCA4                                                             
         GOTO1 AGENPTRS,DMCB,PTRBLK  SAVE PASSIVE POINTERS                      
         XC    ELEMENT,ELEMENT     BUILD NEW APPLIED CREDIT EL.                 
         LA    R4,ELEMENT                                                       
         USING TACRD,R4                                                         
         MVI   TACREL,TACRELQ                                                   
         MVI   TACRLEN,TACRLNQ                                                  
         MVC   TACRSTRT(6),PCYC    SET NEW PERIOD DATES                         
         MVC   TACRAPPL,BALANCE        AMOUNT TO BE APPLIED                     
         OI    TACRSTAT,TACRSTRK       TRACKING ENABLED                         
         MVC   TACRUSE,TGUSCDE         USE CODE                                 
         MVC   TACRTYPE,NEWGRRTY       GRR COVERED USE                          
         SPACE 1                                                                
         CLC   TACRUSE,=C'GRR'     IF GRR USE                                   
         JNE   *+8                                                              
         MVI   SFTGUAR,C'Y'        SET "IS A GUARANTEE" INDICATOR               
         SPACE 1                                                                
         CLC   TACRUSE,=C'ADT'     IF ADT USE                                   
         JE    CCA3                                                             
         CLC   TACRUSE,=C'ADC'     OR ADC USE                                   
         JE    CCA3                                                             
         CLC   TACRUSE,=C'ADO'     OR ADO USE                                   
         JNE   *+8                                                              
CCA3     MVI   TACRTYPE,UADT13W    ASSUME 13W TYPE (04)                         
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
CCA4     MVI   ELCODE,TACRELQ      SET TO GET APPLIED CREDIT EL.                
         L     R4,AIO                                                           
         XR    R1,R1                                                            
         XC    LSTINVPD,LSTINVPD                                                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TACRD,R4                                                         
CCA5     CLC   TACRSTRT(6),PCYC    TEST DATES MATCH                             
         JNE   CCA8                                                             
         CLC   LSTINVPD,TACRINV                                                 
         JH    CCA8                                                             
         LR    R1,R4               SAVE A(LAST EL WITH CORRECT DATES)           
         MVC   LSTINVPD,TACRINV    AND ITS INV NUM                              
CCA8     BRAS  RE,NEXTEL                                                        
         JE    CCA5                                                             
         LTR   R4,R1               MUST'VE FOUND AT LEAST ONE                   
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TACRBAL,BALANCE     UPDATE BALANCE                               
         SPACE 1                                                                
         NI    TACRSTAT,X'FF'-TACRSGUA                                          
         CLI   SFTGUAR,C'Y'        IF THIS IS GUARANTEE                         
         JNE   *+8                                                              
         OI    TACRSTAT,TACRSGUA   SET STATUS BIT IN ELEMENT                    
         MVC   CRSTAT,TACRSTAT     SAVE STATUS                                  
         ST    R4,ATACREL          SAVE A(DISPLAYED TACR ELEMENT)               
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE IT BACK FROM I/O 2                     
         SPACE 1                                                                
         CLI   SFTNPDH+5,0         IF ADDING NEW PERIOD                         
         JE    CCAX                                                             
         GOTO1 ADDPTRS,DMCB,PTRBLK  UPDATE PASSIVE POINTERS                     
         SPACE 1                                                                
         EDIT  (4,TACRAPPL),(12,SFTAMT),2,MINUS=YES,ALIGN=LEFT                  
         OI    SFTAMTH+6,X'80'                                                  
         GOTO1 DATCON,DMCB,(X'11',TACRSTRT),(8,SFTPD)                           
         OI    SFTPDH+6,X'80'                                                   
         SPACE 1                                                                
CCAX     MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR8BD                                                       
         SPACE 3                                                                
CSTDA    DS    F                   SAVED D/A OF CAST RECORD                     
CSTSEQ   DS    CL3                 CAST INPUT SEQ. NUMBER (EBCDIC)              
PCYC     DS    0PL6                PWOS FIXED CYCLE DATES                       
PCYCS    DS    PL3                                                              
PCYCE    DS    PL3                                                              
STATUS   DS    XL1                                                              
DELPEND  EQU   X'80'               DELETE PENDING                               
NOTRACK  EQU   X'40'               NO TRACKING RECORD FOUND                     
BALANCE  DS    F                   FIXED CYCLE BALANCE                          
PREVCRD  DS    F                   PREV CREDITS TAKEN THIS TRACKING REC         
LASTTRK  DS    H                   LAST TRACKING NUMBER (COMPLEMENTED)          
NEXTTRK  DS    H                   NEXT TRACKING NUMBER (COMPLEMENTED)          
LSTINV   DS    CL6                 INVOICE NUMBER OF VERY LAST TACR EL          
LSTINVPD DS    CL6                 INV NUM OF LAST TACR EL W/RIGHT PD           
CRSTAT   DS    XL1                 FIXED CYCLE EL. STATUS                       
NEWGRRTY DS    XL1                 NEW GRR CYCLE COVERED USE                    
APPGRRCD DS    XL3                 APPLY TO GRR SPECIFIC USE (NAME)             
APPGRREQ DS    XL1                 APPLY TO GRR SPECIFIC USE (EQUATE)           
ATACREL  DS    A                   SAVED A(APPLIED CREDIT HIST. EL.)            
PTRBLK   DS    CL(5*L'TLDRREC+1)                                                
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
* PERVALD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058TAGEN8B   02/04/10'                                      
         END                                                                    
