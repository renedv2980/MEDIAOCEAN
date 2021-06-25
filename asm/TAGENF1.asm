*          DATA SET TAGENF1    AT LEVEL 010 AS OF 10/10/14                      
*PHASE T702F1A,*                                                                
         TITLE 'T702F1 - GUARANTEE MAINTENANCE'                                 
T702F1   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 ACTBLNQ+ECTBLNQ,T702F1,R7,R6                                     
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    RE,ACTBL            SAVE ADDR OF INITIAL AGY/CLI TABLE           
         AHI   RE,ACTBLNQ                                                       
         ST    RE,ACTBLX                                                        
         AHI   RE,1                                                             
         ST    RE,ECTBL                                                         
         AHI   RE,ECTBLNQ                                                       
         ST    RE,ECTBLX                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
                                                                                
         GOTO1 INITIAL,DMCB,0                                                   
                                                                                
         MVC   AIO,AIO1                                                         
         MVI   PROSTAT,0                                                        
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         NI    GRTGUAH+6,X'BF'                                                  
                                                                                
         CLI   PFAID,20                                                         
         JNE   *+8                                                              
         MVI   MYPFKEY,20                                                       
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS NOT ADD                         
         JE    *+8                                                              
         BRAS  RE,PROPFK           PROCESS PF KEY NOW                           
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+8                                                              
         BRAS  RE,DK                                                            
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         JNE   *+8                                                              
         BRAS  RE,VR                                                            
                                                                                
         CLI   MODE,RECDEL         DELETE RECORD                                
         JNE   *+8                                                              
         BRAS  RE,DELCHK                                                        
                                                                                
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         JNE   *+12                                                             
         BRAS  RE,DR                                                            
         BRAS  RE,UPDCKMSC         UPDATE CHECK RECORDS W/ MSC STATUS           
                                                                                
         CLI   MODE,XRECREST       RECORD RESTORED                              
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,XRECADD        RECORD ADDED                                 
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(X'40',GRTPIDH),(X'80',GRTGUAH)                      
         JNE   VK10                                                             
         CLC   RECNUM,TWALREC                                                   
         JNE   VK10                                                             
         CLC   ACTNUM,TWALACT                                                   
         JE    VK80                                                             
         CLI   ACTNUM,ACTADD                                                    
         JE    VK10                                                             
         CLI   ACTNUM,ACTDIS                                                    
         JE    VK10                                                             
         CLI   TWALACT,ACTSEL                                                   
         JNE   XIT                                                              
                                                                                
VK10     CLC   RECNUM,TWALREC                                                   
         JE    *+8                                                              
         MVI   LOADSCRN,0                                                       
                                                                                
         LA    R2,GRTPIDH          R2=A(PID/SSN PID)                            
         BAS   RE,VALPID           VALIDATE PID AND DISPLAY NAME                
                                                                                
         BAS   RE,SETSCRN          PERFORM SOME SCREEN SETUP                    
                                                                                
         BAS   RE,VALPID           VALIDATE PID AND DISPLAY NAME                
         GOTO1 CHAROUT,DMCB,(X'80',TANAELQ),GRTPIDNH                            
                                                                                
         LA    R2,GRTGUAH          R2=A(GUARANTEE CODE)                         
         CLI   RECNUM,GO           IF ON GUARANTEE COMMENT SCREEN               
         JE    VK20                                                             
         CLI   ACTNUM,ACTADD       OR GUARANTEE SCREEN AND ACTION               
         JE    VK40                IS NOT ADD                                   
VK20     CLI   5(R2),0             AND FIELD IS EMPTY                           
         JNE   VK30                                                             
         OC    TGGUA,TGGUA         SEE IF GLOBAL FIELD IS SET                   
         JZ    VK30                                                             
         MVC   8(4,R2),TGGUA       IF SO, MOVE IT INTO FIELD                    
         XC    8(4,R2),VKHEXFFS    AND UNCOMPLEMENT IT                          
         OI    6(R2),X'80'                                                      
         J     VK60                                                             
                                                                                
VK30     GOTO1 ANY                 ELSE, REQUIRE THE GUARANTEE CODE             
         MVC   TGGUA,WORK          SAVE COMPLEMENTED IN GLOBAL                  
         XC    TGGUA,VKHEXFFS      GO BUILD KEY                                 
         J     VK60                                                             
                                                                                
VK40     XC    TGGUA,TGGUA         IF ACTION IS ADD                             
         ZAP   DUB,=P'0'           DEFAULT NEXT GUAR CODE TO ZERO               
                                                                                
         USING TLGUD,R3                                                         
         LA    R3,KEY              IF PID HAS GUARANTEES                        
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'C0',TGGUA)                                
         GOTO1 HIGH                                                             
         CLC   TLGUKEY(TLGUGUA-TLGUD),KEYSAVE                                   
         BNE   VK50                                                             
         MVC   FULL,TLGUGUA        SAVE THE LAST CODE                           
         XC    FULL,VKHEXFFS       UNCOMPLEMENT IT                              
         PACK  DUB,FULL            AND PACK IT                                  
         DROP  R3                                                               
                                                                                
VK50     AP    DUB,=P'1'           ADD 1 TO GET THE NEW CODE                    
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL,DUB+5(3)                                                    
         MVC   TGGUA,FULL                                                       
         XC    TGGUA,VKHEXFFS      COMPLEMENT IT FOR GLOBAL                     
         MVC   GRTGUA,FULL         DISPLAY FOR USER                             
         OI    GRTGUAH+6,X'80'                                                  
                                                                                
VK60     OI    GRTGUAH+6,X'40'     POSITION CURSOR AT GRT FIELD                 
                                                                                
         CLI   RECNUM,GO           IF ON GUARANTEE SCREEN                       
         JE    VK70                BUILD KEY                                    
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'40',0)                                    
         XC    LSTDSPAC,LSTDSPAC                                                
         XC    LSTDSPEC,LSTDSPEC                                                
         OI    GRTGUAH+6,X'40'                                                  
         XC    GRTCMTI,GRTCMTI                                                  
         OI    GRTCMTIH+6,X'80'                                                 
         GOTO1 FLDVAL,DMCB,(X'20',GRTPIDH),(X'80',GRTGUAH)                      
         J     XIT                                                              
                                                                                
VK70     XC    TGAGY,TGAGY         IF ON GUARANTEE COMMENT SCREEN               
         MVI   TGTYPE,TLCMTGUA     BUILD KEY                                    
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'C0',TGTYPE)                               
         J     XIT                                                              
                                                                                
VK80     CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JNE   XIT                 AND ALREADY HAD CHANGE TO ADD                
         CLI   GRTAAGYH+5,0        AGENCY, NOW PROMPT A LITTLE                  
         JNE   XIT                 MORE HARSHLY                                 
         CLI   GRTCRPH+5,0                                                      
         JNE   XIT                                                              
         LA    R2,GRTAAGYH                                                      
         J     VKMISS                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO SETUP GUARANTEE SCREEN                            *         
***********************************************************************         
                                                                                
SETSCRN  NTR1                                                                   
         CLI   RECNUM,GO           EXIT IF ON GUARANTEE COMMENT SCREEN          
         JE    XIT                                                              
                                                                                
         CLI   ACTNUM,ACTADD       IF ADDING GUARANTEE                          
         JNE   SSCRN20                                                          
         CLI   LOADSCRN,LOVRSCRN   AND ADD GUARANTEE SCREEN IS NOT              
         JE    SSCRN10             LOADED                                       
         CLI   LOADSCRN,PCYCSCRN                                                
         JNE   XIT                                                              
SSCRN10  MVI   OVERLAY,ADDGSCRN                                                 
         MVC   SVPIDH,GRTPIDH                                                   
         LA    R3,CONTAGH          LOAD ADD GUARANTEE SCREEN NOW                
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   LOADSCRN,ADDGSCRN                                                
         MVC   GRTPIDH(L'SVPIDH),SVPIDH                                         
         J     XIT                                                              
                                                                                
SSCRN20  CLI   LOADSCRN,LOVRSCRN   IF DISPLAYING GUARANTEE                      
         JE    XIT                 AND ADD GUARANTEE SCREEN IS LOADED           
         CLI   LOADSCRN,PCYCSCRN   DO NOT DISPLAY THE PF KEYS                   
         JE    XIT                                                              
SSCRN30  GOTO1 FLDVAL,DMCB,(2,GRTPF1H),(8,GRTPFLH)                              
SSCRN40  MVI   LOADSCRN,0                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE PID INPUT                                *         
*        ON ENTRY ... R2=A(PID/SSN FIELD)                             *         
***********************************************************************         
                                                                                
VALPID   NTR1                                                                   
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         JNE   VPID10                                                           
         OC    TGPID,TGPID         GLOBAL PID MUST BE SET                       
         JNZ   VPID30              GO TRANSLATE IT TO SS#                       
         J     VKMISS                                                           
                                                                                
VPID10   CLI   5(R2),9             IF SS# INPUT, SKIP RIGHT TO RECVAL           
         JNE   VPID20                                                           
         MVC   TGSSN,8(R2)                                                      
         J     VPID40                                                           
                                                                                
VPID20   CLI   5(R2),6             IF PID INPUT                                 
         JNE   VKINV                                                            
         MVC   TGPID,8(R2)         MOVE TO GLOBAL PID FIELD                     
VPID30   GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
                                                                                
VPID40   GOTO1 RECVAL,DMCB,TLW4CDQ,(X'24',0)                                    
         JNE   VKEND                                                            
                                                                                
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   8(9,R2),SPACES                                                   
         MVC   8(L'TGPID,R2),TGPID                                              
         MVI   5(R2),L'TGPID                                                    
         OI    6(R2),X'80'                                                      
                                                                                
         MVC   SVSSN,TGSSN                                                      
         MVC   SVPID,TGPID                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR MESSAGES FOR VALKEY ROUTINE                            *         
***********************************************************************         
                                                                                
VKENTFLD MVI   MYMSGNO1,90                                                      
         OI    GENSTAT2,USGETTXT                                                
         J     VKEND                                                            
                                                                                
VKINV    MVI   ERROR,INVALID                                                    
         J     VKEND                                                            
                                                                                
VKMISS   MVI   ERROR,MISSING                                                    
         J     VKEND                                                            
                                                                                
VKEND    GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VKEY ROUTINES                     *         
***********************************************************************         
                                                                                
VKHEXFFS DC    20X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE KEY                                   *         
***********************************************************************         
                                                                                
DK       NTR1  BASE=*,LABEL=*                                                   
         MVC   GRTPID(L'TGPID),TGPID                                            
                                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'24',0)                                    
         JNE   DK10                                                             
         GOTO1 CHAROUT,DMCB,(X'80',TANAELQ),GRTPIDNH                            
                                                                                
DK10     MVC   AIO,AIO1                                                         
         MVC   SVSSN,TGSSN                                                      
         MVC   SVPID,TGPID                                                      
                                                                                
         USING TLGUD,R4                                                         
         L     R4,AIO                                                           
         MVC   TGGUA,TLGUGUA                                                    
         MVC   GRTGUA,TLGUGUA                                                   
         XC    GRTGUA,DKHEXFFS                                                  
         DROP  R4                                                               
                                                                                
         MVI   LOADSCRN,0                                                       
         XC    LSTDSPAC,LSTDSPAC                                                
         XC    LSTDSPEC,LSTDSPEC                                                
         GOTO1 FLDVAL,DMCB,(X'22',GRTPIDH),GRTGUAH                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VKEY ROUTINES                     *         
***********************************************************************         
                                                                                
DKHEXFFS DC    20X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
***********************************************************************         
                                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO1             IF COMMENT IS IN I/O AREA                    
         BAS   RE,DISCMT           DISPLAY IT                                   
         JE    DRX                                                              
                                                                                
         USING TAGUD,R4                                                         
         MVI   ELCODE,TAGUELQ      ELSE, GET GUARANTEE DETAILS ELEMENT          
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         BAS   RE,LOADTYPE         LOAD TYPE SPECIFIC SCREEN                    
                                                                                
         GOTO1 FLDVAL,DMCB,(3,GRTCRPH),GRTCRPNH                                 
         GOTO1 FLDVAL,DMCB,(3,GRTAAGYH),(X'80',999)                             
         GOTO1 FLDVAL,DMCB,(3,GRTFRSTH),GRTLSTH                                 
                                                                                
         MVC   PRIAGY,TAGUAGY      SAVE PRIMARY AGENCY                          
         MVC   PRICLI,TAGUCLI      SAVE PRIMARY CLIENT                          
                                                                                
         MVC   AIO,AIO2            ALL SUB-READS GO INTO AIO2                   
                                                                                
         BAS   RE,DISCMTIN         DISPLAY COMMENT INDICATOR                    
         BAS   RE,DISCRP           DISPLAY CORPORATION INFO                     
         BAS   RE,DISAC            DISPLAY AGENCIES AND CLIENTS                 
         BAS   RE,DISLOV           DISPLAY LARGE OVERSCALE GUARANTEE            
         BAS   RE,DISPCY           DISPLAY PER CYCLE GUARANTEE                  
         DROP  R4                                                               
                                                                                
         GOTO1 FLDVAL,DMCB,(X'22',GRTHEADH),999                                 
                                                                                
DRX      CLI   MODE,XRECADD                                                     
         JNE   XIT                                                              
         MVC   CONACT,=CL8'CHANGE'                                              
         OI    CONACTH+6,X'80'                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY COMMENT RECORD                            *         
*        ON ENTRY ... R4=A(GUARANTEE COMMENT RECORD)                  *         
***********************************************************************         
                                                                                
DISCMT   NTR1                                                                   
         CLI   RECNUM,GO           IF ON COMMENT SCREEN                         
         JNE   NO                                                               
                                                                                
         LA    R2,GCOLIN1H         R2=A(FIRST COMMENT FIELD)                    
         GOTO1 FLDVAL,DMCB,(X'01',(R2)),(X'80',GCOLCHGH)                        
                                                                                
         LHI   R3,1                R3=COMMENT COUNTER                           
                                                                                
         USING TAXCD,R4                                                         
         MVI   ELCODE,TAXCELQ                                                   
         BRAS  RE,GETEL            GET COMMENT ELEMENT                          
         J     DCMT20                                                           
DCMT10   BRAS  RE,NEXTEL                                                        
DCMT20   JNE   DCMT50                                                           
                                                                                
         XR    R0,R0                                                            
         ZIC   R5,TAXCSEQ                                                       
                                                                                
DCMT30   CR    R3,R5               IF ELEMENT'S SEQUENCE NUMBER                 
         JNE   DCMT40              MATCHES THE CURRENT FIELD                    
         ZIC   RE,TAXCLEN                                                       
         SHI   RE,4                                                             
         STC   RE,5(R2)                                                         
         OI    6(R2),X'80'                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   8(0,R2),TAXCCMNT    DISPLAY COMMENT ON THIS LINE                 
         AHI   R0,1                AND SET ELEMENT PROCESSED                    
                                                                                
DCMT40   ZIC   RE,0(R2)                                                         
         AR    R2,RE               BUMP TO NEXT COMMENT FIELD                   
                                                                                
         LA    RE,GCOLINXH                                                      
         CR    R2,RE                                                            
         JH    DCMT50                                                           
                                                                                
         AHI   R3,1                AND INCREMENT COMMENT FLD COUNTER            
                                                                                
         LTR   R0,R0               IF CURRENT ELEMENT HAS BEEN                  
         JNZ   DCMT10              PROCESSED, GO GET NEXT ELEMENT               
         J     DCMT30              ELSE, TRY TO PROCESS CURRENT ONE             
         DROP  R4                                                               
                                                                                
DCMT50   GOTO1 ACTVOUT,DMCB,GCOLCHGH                                            
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY COMMENT INDICATOR                         *         
***********************************************************************         
                                                                                
DISCMTIN NTR1                                                                   
         XC    GRTCMTI,GRTCMTI                                                  
         XC    TGAGY,TGAGY                                                      
         MVI   TGTYPE,TLCMTGUA                                                  
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'80',TGTYPE)                               
         JNE   XIT                                                              
         MVC   GRTCMTI,GRTCMTL                                                  
         CLI   LOADSCRN,LOVRSCRN                                                
         JE    XIT                                                              
         MVC   GRTCMTI,GRTCMTP                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY CORPORATION INFORMATION                   *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                 *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
DISCRP   NTR1                                                                   
         XC    GRTCRPN,GRTCRPN                                                  
                                                                                
         CLI   TAGUCRP,0           IF GUARANTEE IS FOR CORPORATION              
         JE    XIT                                                              
         MVC   GRTCRP,TAGUCRP      DISPLAY CORPORATION CODE                     
         MVI   HALF,TATITYCO       SET CORPORATION TYPE                         
         MVC   HALF+1(1),TAGUCRP   AND CORP CODE                                
         DROP  R4                                                               
                                                                                
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'24',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TATID,R4                                                         
         MVI   ELCODE,TATIELQ      GET TAX ID ELEMENT                           
         GOTO1 GETL,DMCB,(2,HALF)                                               
         JNE   XIT                 DISPLAY CORPORATION NAME                     
         L     R4,TGELEM                                                        
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',TATIID),GRTCRPNH                      
         MVC   TGSSN,SVSSN         AND RESTORE VARIABLES                        
         MVC   TGPID,SVPID                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO LOAD GUARANTEE TYPE SPECIFIC SCREEN               *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                 *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
LOADTYPE NTR1                                                                   
         OC    TAGUCOM,TAGUCOM     IF PRIMARY COMMERCIAL IS NOT                 
         JNZ   LTYPE10             DEFINED                                      
         CLI   LOADSCRN,LOVRSCRN   AND LARGE OVERSCALE SCREEN NOT               
         JE    XIT                 ALREADY LOADED                               
         MVI   LOADSCRN,LOVRSCRN   LOAD LARGE OVERSCALE SCREEN NOW              
         J     LTYPE20                                                          
                                                                                
LTYPE10  CLI   LOADSCRN,PCYCSCRN   ELSE, IF PER CYCLE SCREEN NOT                
         JE    XIT                 ALREADY LOADED                               
         MVI   LOADSCRN,PCYCSCRN   LOAD PER CYCLE SCREEN NOW                    
                                                                                
LTYPE20  BRAS  RE,LOADIT                                                        
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY AGENCIES AND CLIENTS                      *         
***********************************************************************         
                                                                                
DISAC    NTR1                                                                   
         LA    R2,GRTFRSTH         R2=A(FIRST AGENCY/CLIENT FIELD)              
         LA    R5,GRTLSTH          R5=A(LAST AGENCY/CLIENT FIELD)               
                                                                                
         XC    FSTDSPAC,FSTDSPAC   CLEAR FIRST DISPLAYED ENTRY                  
                                                                                
         USING ACTBLD,R3                                                        
         L     R3,ACTBL            R3=A(AGENCY/CLIENT TABLE)                    
         BRAS  RE,CLRTBL           CLEAR TABLE                                  
         BRAS  RE,BLDACTBL         AND BUILD IT                                 
                                                                                
         CLI   0(R3),X'FF'         IF TABLE IS EMPTY                            
         JNE   DAC10                                                            
         TM    TGCTSTST,TGCTSCLI   CLIENTS DO NOT HAVE ACCESS TO                
         JO    DRNTFND             THIS GUARANTEE                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               BUMP TO AGENCY FIELD                         
         MVC   8(3,R2),=C'ALL'     AND DISPLAY "ALL" FOR NON-CLIENTS            
         J     DACNMORE                                                         
                                                                                
DAC10    TM    PROSTAT,ACCESSYS    BLOCK ANY STAFF THAT DOES NOT                
         JZ    DRNTFND             HAVE ACCESS TO AT LEAST 1 AGY/CLI            
                                                                                
DAC20    CLI   0(R3),X'FF'         IF AT END OF TABLE, DONE                     
         JE    DACNMORE                                                         
                                                                                
         CR    R2,R5               IF PAST END OF SCREEN, DONE                  
         JH    DACMORE                                                          
                                                                                
         CLC   LSTDSPAC,0(R3)      IF DISPLAYED LAST TIME, DON'T                
         JNL   DAC60               DISPLAY AGAIN                                
                                                                                
         OC    FSTDSPAC,FSTDSPAC   IF FIRST ENTRY DISPLAYED ON SCREEN,          
         JNZ   DAC30               SAVE IT                                      
         MVC   FSTDSPAC,0(R3)                                                   
                                                                                
DAC30    CLC   ACAGY,PRIAGY        IF THIS IS THE PRIMARY AGENCY                
         JNE   DAC40                                                            
         CLC   ACCLI,PRICLI        AND PRIMARY CLIENT                           
         JNE   DAC40                                                            
         MVI   8(R2),C'*'          INDICATE AS SUCH                             
                                                                                
DAC40    ZIC   RE,0(R2)            PUT AGENCY CODE TO SCREEN                    
         AR    R2,RE                                                            
         MVC   8(L'ACAGY,R2),ACAGY                                              
         MVI   5(R2),L'ACAGY                                                    
                                                                                
         ZIC   RE,0(R2)            PUT CLIENT CODE TO SCREEN                    
         AR    R2,RE                                                            
         OC    ACCLI,ACCLI                                                      
         JZ    DAC50                                                            
         MVC   8(L'ACCLI,R2),ACCLI                                              
         MVI   5(R2),L'ACCLI                                                    
                                                                                
DAC50    MVC   LSTDSPAC,0(R3)      SAVE LAST DISPLAYED ENTRY                    
                                                                                
         ZIC   RE,0(R2)            BUMP TO NEXT INPUT FIELD                     
         AR    R2,RE                                                            
DAC60    LA    R3,ACLNQ(R3)        BUMP TO NEXT AGY/CLI TABLE ENTRY             
         J     DAC20                                                            
         DROP  R3                                                               
                                                                                
DACMORE  MVC   GRTMORE,MORE                                                     
         J     DACX                                                             
                                                                                
DACNMORE XC    LSTDSPAC,LSTDSPAC                                                
         MVC   GRTMORE,SPACES                                                   
DACX     OI    GRTMOREH+6,X'80'                                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY LARGE OVERSCALE GUARANTEE DETAILS         *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                 *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
DISLOV   NTR1                                                                   
         CLI   LOADSCRN,LOVRSCRN                                                
         JNE   XIT                                                              
                                                                                
         GOTO1 DATCON,DMCB,(X'11',TAGUPD),(8,SLOPD)                             
         MVC   SVGUPD,TAGUPD                                                    
                                                                                
         NI    SLOAMTH+1,X'FF'-X'20'                                            
         EDIT  (4,TAGUAMT),(12,SLOAMT),2,ALIGN=LEFT                             
         STC   R0,SLOAMTH+5                                                     
                                                                                
         NI    SLOBALH+1,X'FF'-X'20'                                            
         EDIT  (4,TAGUBAL),(12,SLOBAL),2,FLOAT=-,ALIGN=LEFT                     
         STC   R0,SLOBALH+5                                                     
                                                                                
         OC    TAGUGCNT,TAGUGCNT   DISPLAY GUARANTEE CONTRACT                   
         JZ    DLOV05                                                           
         MVC   FULL,TAGUGCNT                                                    
         XC    FULL,DRHEXFFS                                                    
         ICM   R1,15,FULL          UNCOMPLEMENT GRT CONTRACT CODE               
         AHI   R1,1                                                             
         STCM  R1,15,TGGCNT                                                     
         EDIT  (R1),SLOGCNT,0,ALIGN=RIGHT                                       
         OC    SLOGCNT,=C'000000'                                               
         MVC   SLOGCNT(2),=C'GC'                                                
                                                                                
DLOV05   MVI   SLOOVER,C'Y'                                                     
         TM    TAGUSTAT,TAGUSOVR   DISPLAY OVERAGE PAID STATUS                  
         JO    DLOV10                                                           
         MVI   SLOOVER,C'N'                                                     
                                                                                
DLOV10   MVI   SLOEST,C'Y'                                                      
         TM    TAGUSTA2,TAGUSIGP   DISPLAY IGNORE FOR ESTIMATE                  
         JO    DLOV20                                                           
         MVI   SLOEST,C'N'                                                      
                                                                                
DLOV20   MVI   SLOLCK,C'Y'                                                      
         TM    TAGUSTAT,TAGUSLCK   DISPLAY LOCKED STATUS                        
         JO    DLOV30                                                           
         MVI   SLOLCK,C'N'                                                      
                                                                                
DLOV30   GOTO1 FLDVAL,DMCB,(X'0A',SLODBH),(X'08',SLODBALH)                      
                                                                                
         TM    TAGUSTA2,TAGUSNEW   IF GUARANTEE WAS ADDED VIA                   
         JO    DLOV40              OLD GUARANTEE SYSTEM ...                     
         NI    SLODBH+1,X'FF'-X'0C'                                             
         NI    SLODBALH+1,X'FF'-X'2C'                                           
         MVI   SLODBALH+5,1                                                     
         MVI   SLODBAL,C'Y'                                                     
         TM    TAGUSTAT,TAGUSDES   DISPLAY DESCENDING BALANCE STATUS            
         JO    DLOV40                                                           
         MVI   SLODBAL,C'N'                                                     
                                                                                
DLOV40   MVI   SLOPNH,C'N'                                                      
         TM    TAGUSTAT,TAGUSPNH   DISPLAY PAY P&H ON USE STATUS                
         JZ    DLOV45                                                           
         MVI   SLOPNH,C'Y'                                                      
                                                                                
DLOV45   MVI   SLOMSC,C'N'                                                      
         TM    TAGUSTA2,TAGUSMSC   GUARANTEE MULTI-SERVICE CONTRACT             
         JZ    DLOV50                                                           
         MVI   SLOMSC,C'Y'                                                      
                                                                                
DLOV50   GOTOR DISEXUS,DMCB,SLOXUSEH                                            
                                                                                
         OI    SLOADDH+1,X'0C'     IF GUARANTEE ADDED BY GRT/PAY                
         OI    SLOINVH+1,X'0C'     MAKE AGENCY/INVOICE FIELD VISIBLE            
         OC    TAGUINV,TAGUINV                                                  
         JZ    DLOV90                                                           
         NI    SLOADDH+1,X'FF'-X'0C'                                            
         NI    SLOINVH+1,X'FF'-X'0C'                                            
                                                                                
         LA    R2,SLOINV                                                        
         OC    TAGUIAY,TAGUIAY     DISPLAY GRT AGENCY/INVOICE                   
         JZ    DLOV80                                                           
         MVC   0(L'TAGUIAY,R2),TAGUIAY                                          
         OC    SLOINV,SPACES                                                    
DLOV60   CLI   0(R2),C' '                                                       
         JE    DLOV70                                                           
         LA    R2,1(R2)                                                         
         J     DLOV60                                                           
DLOV70   MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
DLOV80   GOTO1 TINVCON,DMCB,TAGUINV,(R2),DATCON                                 
                                                                                
DLOV90   BRAS  RE,APPLPYMS         IF PAYMENTS HAVE APPLIED AGAINST             
         JNE   DLOV100             THIS GUARANTEE ...                           
         OI    SLOBALH+1,X'20'     PROTECT BALANCE                              
         OI    SLODBALH+1,X'20'    AND DESCENDING BALANCE FIELDS                
                                                                                
         TM    TAGUSTAT,TAGUSDES   IF GUARANTEE HAS ASCENDING BALANCE           
         JO    DLOV100             PROTECT AMOUNT FIELD                         
         OI    SLOAMTH+1,X'20'                                                  
                                                                                
DLOV100  MVC   AIO,AIO1                                                         
         GOTO1 ACTVOUT,DMCB,SLOACTVH                                            
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY PER CYCLE GUARANTEE DETAILS               *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                 *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
DISPCY   NTR1                                                                   
         CLI   LOADSCRN,PCYCSCRN                                                
         JNE   XIT                                                              
                                                                                
         GOTO1 FLDVAL,DMCB,(X'0A',SPCPCHDH),SPCCIDH                             
         GOTO1 FLDVAL,DMCB,(2,SPCHED1H),(X'20',SPCHED2H)                        
         GOTO1 FLDVAL,DMCB,(2,SPCAC1H),(8,SPCAC3H)                              
         GOTO1 FLDVAL,DMCB,(3,SPCPOT1H),(X'10',SPCPOTLH)                        
         GOTO1 FLDVAL,DMCB,(3,SPCPOMRH),SPCPOM2H                                
         GOTO1 FLDVAL,DMCB,(2,SPCPFLH),(8,SPCPFLH)                              
         BAS   RE,UNPROSEL                                                      
                                                                                
         MVC   PRICOM,TAGUCOM      SAVE PRIMARY INTERNAL COMM'L NUMBER          
                                                                                
         MVI   SPCLCK,C'Y'                                                      
         TM    TAGUSTAT,TAGUSLCK   DISPLAY LOCKED STATUS                        
         JO    DPCY10                                                           
         MVI   SPCLCK,C'N'                                                      
         DROP  R4                                                               
                                                                                
DPCY10   MVC   SPCCID,=CL12'*DELETED*'                                          
                                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',PRICOM)                              
         JNE   DPCY20                                                           
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO2             R4=A(PRIMARY COMMERCIAL RECORD)              
         MVI   ELCODE,TACOELQ      R4=A(COMMERCIAL DETAILS ELEMENT)             
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SPCCID,TACOCID      DISPLAY PRIMARY COMMERCIAL                   
         MVC   TGCID,TACOCID                                                    
                                                                                
         TM    TACOSTA2,TACOPCYC   IF COMMERCIAL'S PER CYCLE STATUS             
         JZ    DPCY20              IS ON, DISPLAY "PER CYCLE COMM'L"            
         MVC   SPCHED1,HEADPCC                                                  
         MVC   SPCHED2,SPACES                                                   
         J     DPCY40                                                           
         DROP  R4                                                               
                                                                                
DPCY20   MVC   SPCHED1,SPACES      ELSE, DISPLAY POTENTIAL HEADER               
         MVC   SPCHED2,HEADPPC                                                  
         GOTO1 FLDVAL,DMCB,SPCAC1H,(X'10',SPCAC3H)                              
                                                                                
         BAS   RE,DISEC            DISPLAY ELIGIBLE COMMERCIALS                 
                                                                                
DPCY40   GOTOR DISEXUS,DMCB,SPCXUSEH                                            
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 ACTVOUT,DMCB,SPCACTVH                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO UNPROTECT THE SELECT FIELDS                       *         
***********************************************************************         
                                                                                
UNPROSEL NTR1                                                                   
         LA    R2,SPCPOT1H         R2=A(1ST SELECT FIELD)                       
         LA    R5,SPCPOTLH         R5=A(LAST POTENTIAL PRI. COMM'L FLD)         
                                                                                
UPS10    CR    R2,R5               IF NOT AT END OF SELECT FIELDS               
         JH    XIT                                                              
         NI    1(R2),X'FF'-X'20'   UNPROTECT SELECT FIELD                       
                                                                                
         ZIC   RE,0(R2)            BUMP TO AGENCY FIELD                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)            BUMP TO COMMERCIAL FIELD                     
         AR    R2,RE                                                            
         ZIC   RE,0(R2)            BUMP TO NEXT SELECT FIELD                    
         AR    R2,RE                                                            
         J     UPS10                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY ELIGIBLE PRIMARY COMMERCIALS              *         
***********************************************************************         
                                                                                
DISEC    NTR1                                                                   
         LA    R2,SPCPOT1H         R2=A(1ST POTENTIAL PRI.COMM'L FIELD)         
         LA    R3,SPCPOTLH         R3=A(LAST POTENTIAL PRI. COMM'L FLD)         
                                                                                
         XC    FSTDSPEC,FSTDSPEC   CLEAR FIRST DISPLAYED ENTRY                  
                                                                                
         USING ECTBLD,R5                                                        
         L     R5,ECTBL            R5=A(ELIGIBLE COMMERCIALS TABLE)             
         BRAS  RE,BLDETBL          BUILD ELIGIBLE COMMERCIALS TABLE             
                                                                                
         TM    PROSTAT,PSPCBLCK    IF PAYMENTS ARE AWAITING BILLING/            
         JZ    DECNAWAT            CHECKS, RETURN MESSAGE                       
                                                                                
         CLI   0(R5),X'FF'         IF TABLE IS EMPTY                            
         JE    DECNELIG            THERE ARE NO ELIGIBLE COMMERCIALS            
                                                                                
DEC10    CLI   0(R5),X'FF'         IF AT END OF TABLE, DONE                     
         JE    DECNMORE                                                         
                                                                                
         CR    R2,R3               IF PAST END OF SCREEN, DONE                  
         JH    DECMORE                                                          
                                                                                
         CLC   LSTDSPEC,0(R5)      IF DISPLAYED LAST TIME, DON'T                
         JNL   DEC30               DISPLAY AGAIN                                
                                                                                
         OC    FSTDSPEC,FSTDSPEC   IF FIRST ENTRY DISPLAYED ON SCREEN,          
         JNZ   DEC20               SAVE IT                                      
         MVC   FSTDSPEC,0(R5)                                                   
                                                                                
DEC20    ZIC   RE,0(R2)            PUT AGENCY CODE TO SCREEN                    
         AR    R2,RE                                                            
         MVC   8(L'ECAGY,R2),ECAGY                                              
         MVI   5(R2),L'ECAGY                                                    
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               PUT COMMERCIAL ID TO SCREEN                  
         MVC   8(L'ECCID,R2),ECCID                                              
         MVI   5(R2),L'ECCID                                                    
                                                                                
         MVC   LSTDSPEC,0(R5)      SAVE LAST DISPLAYED ENTRY                    
                                                                                
         ZIC   RE,0(R2)            BUMP TO NEXT INPUT FIELD                     
         AR    R2,RE                                                            
DEC30    LA    R5,ECLNQ(R5)        AND BUMP TO NEXT TABLE ENTRY                 
         J     DEC10                                                            
         DROP  R5                                                               
                                                                                
DECMORE  MVC   SPCPOMR,MORE        ENTER FOR MORE MESSAGE                       
         J     XIT                                                              
                                                                                
DECNMORE XC    LSTDSPEC,LSTDSPEC   ALL DISPLAYED                                
         J     XIT                                                              
                                                                                
DECNAWAT MVC   SPCPOMR,PEND        DISPLAY PER CYCLE IS AWAITING                
         MVC   SPCPOM2,PEND2       BILLING/CHECKS MESSAGE                       
         J     XIT                                                              
                                                                                
DECNELIG OC    LPCYSTRT,LPCYSTRT   IF PER CYCLE PAYMENT HAS BEEN                
         JZ    XIT                 MADE TO THIS GUARANTEE                       
         TM    PROSTAT,DIFFERCY    AND THERE IS AN ACTIVE COMMERCIAL            
         JZ    XIT                 ATTACHED WITH A DIFFERENT CYCLE              
         MVC   SPCPOMR,NONE        DISPLAY NO COMMERCIALS ELIGIBLE              
         MVC   SPCPOM2,NONE2       MESSAGE                                      
         GOTO1 FLDVAL,DMCB,SPCPFLH,(X'20',SPCPFLH)                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY GUARANTEE'S EXCLUDED USES                 *         
*        ON ENTRY ... P1=A(EXCLUDED USES FIELD)                       *         
***********************************************************************         
                                                                                
DISEXUS  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(EXCLUDED USES FIELD)                    
         LA    R2,8(R2)                                                         
                                                                                
         USING TAGXD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAGXELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
         ZIC   R3,TAGXLEN                                                       
         SHI   R3,TAGXLNQ          R3=# OF EXCLUDED USES                        
         LA    R4,TAGXUSE          R4=A(LIST OF EXCLUDED USES)                  
                                                                                
DEXUS10  GOTO1 USEVAL,DMCB,(X'80',0(R4)),0                                      
         MVC   0(L'TGUSCDE,R2),TGUSCDE                                          
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         JZ    XIT                                                              
         MVI   L'TGUSCDE(R2),C','                                               
         LA    R2,L'TGUSCDE+1(R2)                                               
         LA    R4,L'TAGXUSE(R4)                                                 
         J     DEXUS10                                                          
                                                                                
***********************************************************************         
*        ERROR MESSAGES FOR DREC ROUTINE                              *         
***********************************************************************         
                                                                                
DRNTFND  LA    R2,GRTGUAH                                                       
         MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         J     DREND                                                            
                                                                                
DRINFEND LA    R2,GRTFRSTH                                                      
         MVI   MYMTYP,GTMINF                                                    
         J     DRMSGEND                                                         
                                                                                
DRERREND MVI   MYMTYP,GTMERR                                                    
         J     DRMSGEND                                                         
                                                                                
DRMSGEND MVI   BLOCK,0                                                          
         OI    GENSTAT2,USGETTXT                                                
         J     DREND                                                            
                                                                                
DREND    GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR DREC ROUTINES                     *         
***********************************************************************         
                                                                                
GRTCMTL  DC    CL(L'GRTCMTI)'    COMMENTS INCLUDED'                             
GRTCMTP  DC    CL(L'GRTCMTI)' COMMENTS INCLUDED'                                
                                                                                
HEADPCC  DC    CL(L'SPCHED1)'Per Cycle Commercial'                              
HEADPPC  DC    CL(L'SPCHED2)'Potential Primary Commercials'                     
                                                                                
MORE     DC    C'Hit enter for more...'                                         
                                                                                
PEND     DC    C'Per Cycle payment    '                                         
PEND2    DC    C'is pending           '                                         
                                                                                
NONE     DC    C'Use PERCYCLE/TRANSFER'                                         
NONE2    DC    C'to assign New Primary'                                         
                                                                                
DRHEXFFS DC    20X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*       ROUTINE DISPLAYS THE 2 DEC PLACE NUMBER FOUND IN PARM1 TO THE *         
*       DESTINATION POINTED TO BY PARM2.  IF THE DISPLAYED AMOUNT IS  *         
*       LARGER THEN THE LENGTH SPECIFIED IN PARM3 THEN IT WILL        *         
*       DISPLAY '*'S INSTEAD. THE ROUTINE WILL RETURN THE LENGTH IN   *         
*       PARM1.                                                        *         
***********************************************************************         
                                                                                
DR2DEC   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)         R2 = NUMBER                                  
*                                  R3 = A(DESTINATION)                          
         BCTR  R4,0                R4 = LENGTH OF DESTINATION - 1               
                                                                                
         EX    R4,*+8              PRE-CLEAR DESTINATION                        
         J     *+10                                                             
         XC    0(0,R3),0(R3)                                                    
                                                                                
         LTR   R2,R2               IF NUMBER = 0                                
         JNZ   *+18                                                             
         MVI   0(R3),C'0'          THEN DISPLAY '0' AND RETURN                  
         MVC   0(4,R1),=F'1'                                                    
         J     XIT                                                              
*                                  ELSE EDIT PERCENTAGE INTO BLOCK              
         EDIT  (R2),(10,BLOCK),2,ALIGN=LEFT                                     
                                                                                
         LA    RF,BLOCK            RF = A(LAST CHAR IN BLOCK)                   
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
                                                                                
DR2DEC10 CLI   0(RF),C'.'          IF CHAR IS '.' THEN DONE                     
         JE    DR2DEC20                                                         
         CLI   0(RF),C'0'          ELSE IF CHAR IS <> '0' THEN DONE             
         JNE   DR2DEC30                                                         
         BCT   RF,DR2DEC10         ELSE BACK UP POINTER AND LOOP BACK           
                                                                                
DR2DEC20 BCTR  RF,0                BACK UP ONE MORE FOR '.'                     
                                                                                
DR2DEC30 LA    RE,BLOCK            RF = LENGTH OF NUMBER - 1                    
         SR    RF,RE                                                            
                                                                                
         CR    RF,R4               IF L(NUMBER) - 1 > L(DEST) - 1               
         JH    DR2DEC40            THEN DISPLAY '*'S                            
                                                                                
         EX    RF,*+8              ELSE MOVE NUMBER TO DESTINATION              
         J     *+10                                                             
         MVC   0(0,R3),BLOCK                                                    
                                                                                
         LA    RF,1(RF)            RETURN LENGTH IN PARM1                       
         ST    RF,0(R1)                                                         
         J     XIT                                                              
                                                                                
DR2DEC40 MVI   0(R3),C'*'          DISPLAY '*'S                                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         J     *+10                                                             
         MVC   1(0,R3),0(R3)                                                    
                                                                                
         LA    RF,2(R4)            RETURN LENGTH IN PARM1                       
         ST    RF,0(R1)                                                         
*        J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE RECORD                               *         
***********************************************************************         
                                                                                
VR       NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,VALCMT           IF COMMENT IS IN I/O AREA                    
         JE    XIT                 VALIDATE IT                                  
                                                                                
         USING TAGUD,R4                                                         
VR10     L     R4,AIO1                                                          
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    VR20                                                             
                                                                                
         XC    ELEMENT,ELEMENT     IF ELEMENT IS NOT FOUND, BUILD IT            
         MVI   TAGUEL,TAGUELQ                                                   
         MVI   TAGULEN,TAGULNQ                                                  
         OI    TAGUSTA2,TAGUSNEW                                                
         BRAS  RE,MYADDEL                                                       
         J     VR10                                                             
                                                                                
VR20     CLI   ACTNUM,ACTCHA       IF ACTION CHANGE                             
         JNE   VR30                ENSURE THAT SCREEN IS SET CORRECTLY          
         MVI   LOADSCRN,LOVRSCRN                                                
         OC    TAGUCOM,TAGUCOM                                                  
         JZ    VR30                                                             
         MVI   LOADSCRN,PCYCSCRN                                                
                                                                                
VR30     MVC   PRIAGY,TAGUAGY      SAVE PRIMARY COMMERCIAL'S INITIAL            
         MVC   PRICLI,TAGUCLI      AGENCY AND CLIENT                            
                                                                                
         MVC   AIO,AIO2            ALL SUB-READS GO INTO AIO2                   
                                                                                
         BAS   RE,VALCRP           VALIDATE CORPORATION INFO                    
         BAS   RE,VACLIMS          VALIDATE AGENCY/CLIENT LIMITS                
         BAS   RE,SELTYPE          SELECT GUARANTEE TYPE                        
         BAS   RE,VALLOV           VALIDATE LARGE OVERSCALE GUARANTEE           
         BAS   RE,VALPCY           VALIDATE PER CYCLE GUARANTEE                 
         BAS   RE,BLDACELS         BUILD AGENCY/CLIENT LIMITS                   
                                                                                
         MVC   TAGUAGY,PRIAGY      SAVE NEW PRIMARY AGENCY                      
         MVC   TAGUCLI,PRICLI      AND CLIENT                                   
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION CHANGE, AVOID PUTREC DRAMA         
         JE    VR40                                                             
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'B4',TGGUA)                                
                                                                                
VR40     MVC   AIO,AIO1                                                         
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY ELEMENT                         
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         JNE   XIT                                                              
         NI    GRTGUAH+4,X'FF'-X'20'                                            
         L     RE,AIO1                                                          
         MVC   KEY(L'TLGUKEY),0(RE)                                             
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE COMMENT SCREEN                           *         
***********************************************************************         
                                                                                
VALCMT   NTR1                                                                   
         CLI   RECNUM,GO           IF ON COMMENT SCREEN                         
         JNE   NO                                                               
                                                                                
         L     R4,AIO              DELETE ALL OLD COMMENT ELEMENTS              
         MVI   ELCODE,TAXCELQ                                                   
         GOTO1 REMELEM                                                          
                                                                                
         USING TAXCD,R4                                                         
         LA    R4,ELEMENT          R4=A(ELEMENT)                                
                                                                                
         LA    R2,GCOLIN1H         R2=A(FIRST COMMENT FIELD)                    
         LA    R3,GCOLINXH         R3=A(LAST COMMENT FIELD)                     
         LA    R5,1                R5 = SEQUENCE NUMBER                         
                                                                                
VCMT10   CR    R2,R3                                                            
         JH    VCMT30                                                           
         CLI   5(R2),0             IF CURRENT FIELD IS POPULATED                
         JE    VCMT20                                                           
         XC    ELEMENT,ELEMENT     INITIALIZE ELEMENT                           
         MVI   TAXCEL,TAXCELQ                                                   
         ZIC   RE,5(R2)                                                         
         AHI   RE,4                                                             
         STC   RE,TAXCLEN          INSERT LENGTH                                
         STC   R5,TAXCSEQ          SEQUENCE NUMBER                              
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TAXCCMNT(0),8(R2)   AND COMMENT                                  
         GOTO1 ADDELEM             AND ADD ELEMENT                              
         DROP  R4                                                               
                                                                                
VCMT20   ZIC   RE,0(R2)            BUMP TO NEXT COMMENT FIELD                   
         AR    R2,RE                                                            
         LA    R5,1(R5)            AND INCREMENT SEQUENCE NUMBER                
         J     VCMT10                                                           
                                                                                
VCMT30   GOTO1 ACTVIN,DMCB,GCOLCHGH                                             
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE CORPORATION INFORMATION                  *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                 *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
VALCRP   NTR1                                                                   
         MVI   TAGUCRP,0                                                        
         XC    SVFID,SVFID                                                      
         XC    GRTCRPN,GRTCRPN                                                  
         OI    GRTCRPNH+6,X'80'                                                 
                                                                                
         LA    R2,GRTCRPH                                                       
         CLI   5(R2),0                                                          
         JE    XIT                                                              
                                                                                
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'24',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVI   ELCODE,TATIELQ                                                   
         MVI   HALF,TATITYCO                                                    
         MVC   HALF+1(1),8(R2)                                                  
         GOTO1 GETL,DMCB,(2,HALF)                                               
         JNE   VRINV                                                            
         MVC   TAGUCRP,8(R2)                                                    
         DROP  R4                                                               
                                                                                
         USING TATID,R4                                                         
         L     R4,TGELEM                                                        
         MVC   SVFID,TATIID                                                     
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',TATIID),GRTCRPNH                      
         MVC   TGSSN,SVSSN                                                      
         MVC   TGPID,SVPID                                                      
         DROP  R4                                                               
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         JE    XIT                                                              
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'B4',0)                                    
         JE    XIT                                                              
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE AGENCY/CLIENT LIMITS                     *         
***********************************************************************         
                                                                                
VACLIMS  NTR1                                                                   
         USING ACTBLD,R3                                                        
         L     R3,ACTBL            R3=A(INITIAL AGY/CLI TABLE)                  
         BRAS  RE,CLRTBL           CLEAR TABLE                                  
                                                                                
         XC    TGAGY,TGAGY         CLEAR GLOBAL AGENCY                          
         XC    TGCLI,TGCLI         CLEAR GLOBAL CLIENT                          
                                                                                
         CLI   ACTNUM,ACTADD       IF CHANGING THE GUARANTEE RECORD             
         JE    VACL10                                                           
         BRAS  RE,BLDACTBL         BUILD INITIAL AGENCY/CLIENT TABLE            
         DROP  R3                                                               
                                                                                
VACL10   XC    ADDAC,ADDAC         INITIALIZE VARIABLES                         
         MVI   ADDAC,X'FF'                                                      
                                                                                
         MVI   ELCODE,TAVAELQ      DELETE ELEMENTS                              
         BRAS  RE,MYREMEL                                                       
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO1             DELETE OLD AGENCY/CLIENT                     
         MVI   ELCODE,TAGUELQ      RESTRICTIONS                                 
         BRAS  RE,GETEL                                                         
         JNE   VACL20                                                           
         XC    TAGUAGY,TAGUAGY                                                  
         XC    TAGUCLI,TAGUCLI                                                  
         DROP  R4                                                               
                                                                                
VACL20   LA    R2,GRTAAGYH         ADD FIELDS POPULATED?                        
         GOTO1 FLDVAL,DMCB,(X'80',(R2)),(X'80',GRTACLIH)                        
         JNE   VACL30                                                           
         CLI   ACTNUM,ACTADD       INPUT REQUIRED FOR ACTION ADD                
         JNE   VACL60                                                           
         J     VKMISS                                                           
                                                                                
         USING ACTBLD,R3                                                        
VACL30   LA    R3,ADDAC                                                         
                                                                                
         CLI   5(R2),0             VALIDATE NEW AGENCY LIMIT                    
         JE    VACL40                                                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',(R2))                                 
         MVI   ACSTAT,0                                                         
         MVC   ACAGY,TGAGY                                                      
         MVI   ACLNQ(R3),X'FF'                                                  
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
VACL40   CLI   GRTACLIH+5,0        VALIDATE NEW CLIENT LIMIT                    
         JE    VACL50                                                           
         CLI   GRTAAGYH+5,0                                                     
         JE    VRMISS              AGENCY IS REQUIRED                           
         LA    R2,GRTACLIH                                                      
         GOTO1 RECVAL,DMCB,TLCLCDQ,(R2)                                         
         MVC   ACCLI,TGCLI                                                      
         DROP  R3                                                               
                                                                                
VACL50   BAS   RE,ALRDYON          AGY/CLI CANNOT ALREADY BE ON GRT             
                                                                                
VACL60   BAS   RE,PRODEL           PROCESS DELETED AGENCY/CLIENT(S)             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO CHECK IS AGENCY/CLIENT IS ALREADY ON GUARANTEE    *         
***********************************************************************         
                                                                                
ALRDYON  NTR1                                                                   
         USING ACTBLD,R3                                                        
         L     R3,ACTBL            R3=A(INITIAL AGENCY/CLIENT TABLE)            
AO10     CLI   0(R3),X'FF'         IF AT END OF TABLE, DONE                     
         JE    XIT                                                              
                                                                                
         CLC   TGAGY,ACAGY         IF AGENCY ALREADY IN TABLE                   
         JNE   AO20                                                             
         OC    TGCLI,TGCLI         AND NOT ADDING A CLIENT                      
         JZ    VRINV               RETURN ERROR                                 
                                                                                
         CLC   TGCLI,ACCLI         IF AGENCY/CLIENT ALREADY IN TABLE            
         JE    VRINV               RETURN ERROR                                 
                                                                                
         BAS   RE,OKTONAR          CHECK IF IT IS OK TO NARROW DOWN             
                                                                                
AO20     LA    R3,ACLNQ(R3)        BUMP TO NEXT AGY/CLI TABLE ENTRY             
         J     AO10                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DELETE AGENCY/CLIENT(S)                           *         
***********************************************************************         
                                                                                
PRODEL   NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'40',GRTFRSTH),GRTLSTH                             
         JE    XIT                                                              
                                                                                
         LA    R2,GRTFRSTH       R2=A(FIRST BILLING/DELETE FIELD)               
         LA    R5,GRTLSTH        R5=A(LAST BILLING/DELETE FIELD)                
PDEL10   CR    R2,R5                                                            
         JNL   XIT                                                              
         CLI   8(R2),C'D'        IF ENTRY IS NOT SET FOR DELETE                 
         JE    PDEL20                                                           
         ZIC   RE,0(R2)          BUMP TO NEXT AGENCY FIELD                      
         AR    R2,RE                                                            
         ZIC   RE,0(R2)          BUMP TO NEXT CLIENT FIELD                      
         AR    R2,RE                                                            
         ZIC   RE,0(R2)          BUMP TO NEXT BILLING/DELETE FIELD              
         AR    R2,RE                                                            
         J     PDEL10                                                           
                                                                                
PDEL20   ST    R2,ASELFLD                                                       
         ZIC   R1,0(R2)          IF ENTRY IS SET FOR DELETE                     
         AR    R1,R2             R1=A(AGENCY FIELD)                             
         CLI   5(R1),0           ENSURE AGENCY IS PRESENT                       
         JE    VRINV                                                            
         OC    8(6,R1),SPACES    PAD IT WITH SPACES                             
                                                                                
         CLC   8(6,R1),=CL6'ALL' ENSURE AGENCY IS NOT "ALL"                     
         JE    VRINV                                                            
                                                                                
         ZIC   R2,0(R1)                                                         
         AR    R2,R1             R2=A(CLIENT FIELD)                             
         CLI   5(R2),0           IF CLIENT INPUT PRESENT                        
         JE    PDEL30                                                           
         OC    8(6,R2),SPACES    PAD IT WITH SPACES                             
                                                                                
         USING ACTBLD,R3                                                        
PDEL30   L     R3,ACTBL          R3=A(INITIAL AGY/CLI TABLE)                    
PDEL40   CLI   0(R3),X'FF'       MUST FIND AGENCY/CLIENT IN THE TABLE           
         JNE   *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLC   ACAGY,8(R1)       WHEN ENTRY TO DELETE IS FOUND                  
         JNE   PDEL50                                                           
         CLC   ACCLI,8(R2)                                                      
         JNE   PDEL50                                                           
         BAS   RE,OKTODEL        CHECK IF IT IS OK TO DELETE                    
         XC    0(ACLNQ,R3),0(R3) AND ERASE IT                                   
                                                                                
         ZIC   RE,0(R2)          BUMP TO NEXT BILLING/DELETE FIELD              
         AR    R2,RE                                                            
         J     PDEL10            NOW GO LOOK FOR MORE DELETE INPUT              
                                                                                
PDEL50   LA    R3,ACLNQ(R3)                                                     
         J     PDEL40                                                           
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO CHECK IF OK TO DELETE AGENCY/CLIENT               *         
*        ON ENTRY ... R3=A(AGENCY/CLIENT ENTRY TO DELETE)                       
***********************************************************************         
                                                                                
         USING ACTBLD,R3                                                        
OKTODEL  NTR1                                                                   
         CLC   ACAGY,PRIAGY      CANNOT DELETE THE GUARANTEE'S                  
         JNE   OTD10             PRIMARY AGENCY/CLIENT                          
         CLC   ACCLI,PRICLI                                                     
         JE    VRNDLPAC                                                         
                                                                                
OTD10    LR    R5,R3             SAVE ADDRESS OF ENTRY TO DELETE                
                                                                                
         CLI   GRTAAGYH+5,0      IF ADDING AN AGENCY/CLIENT, ALWAYS             
         JNE   OTD40             OK TO DELETE ONE                               
                                                                                
         L     R3,ACTBL          ELSE, ENSURE THAT AT LEAST ONE                 
OTD20    CLI   0(R3),X'FF'       AGENCY/CLIENT WILL BE LEFT                     
         JE    VRGAL1AC                                                         
         OC    ACAGY,ACAGY                                                      
         JZ    OTD30                                                            
         CLC   0(ACLNQ,R3),0(R5)                                                
         JNE   OTD40                                                            
OTD30    LA    R3,ACLNQ(R3)                                                     
         J     OTD20                                                            
         DROP  R3                                                               
                                                                                
         USING ACTBLD,R5                                                        
OTD40    LA    R3,KEY            SET UP KEY AND IO AREA                         
         L     R4,AIO2                                                          
                                                                                
         USING TLCAPD,R3                                                        
         XC    KEY,KEY           READ ALL CAST RECORDS THAT                     
         MVI   TLCAPCD,TLCAGCDQ  ARE ATTACHED TO THIS GUARANTEE                 
         MVC   TLCAGSSN,TGSSN                                                   
         MVC   TLCAGGUA,TGGUA                                                   
         XC    TLCAGGUA,VRHEXFFS                                                
         GOTO1 HIGH                                                             
         J     OTD60                                                            
OTD50    GOTO1 SEQ                                                              
OTD60    CLC   KEY(TLCAGCOM-TLCAPCD),KEYSAVE                                    
         JNE   OTD80                                                            
         MVC   SVKEY,KEY                                                        
         DROP  R3                                                               
                                                                                
         USING TLCOPD,R3                                                        
         XC    KEY,KEY           GET COMMERCIAL RECORD FOR CAST                 
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVKEY+TLCAGCOM-TLCAPD                                   
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         JNE   OTD70                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         CLC   TLCOAGY,ACAGY     IF COMMERCIAL AGENCY MATCHES                   
         JNE   OTD70             AGENCY BEING DELETED                           
         OC    ACCLI,ACCLI       AND DELETED CLIENT IS NOT DEFINED              
         JZ    VRNDATCO                                                         
         CLC   TLCOCLI,ACCLI     OR MATCHES THE COMMERCIAL                      
         JE    VRNDATCO          DON'T ALLOW THE DELETE                         
         DROP  R4                                                               
                                                                                
OTD70    MVC   KEY,SVKEY         RESTORE CAST READ SEQUENCE                     
         GOTO1 HIGH                                                             
         J     OTD50                                                            
                                                                                
OTD80    MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKPD,R3                                                        
         XC    KEY,KEY           READ ALL CHECK RECORDS FOR THIS                
         MVI   TLCKPCD,TLCKECDQ  PERFORMER FOR THE DELETED AGENCY               
         MVC   TLCKESSN,TGSSN                                                   
         OC    SVFID,SVFID                                                      
         JZ    *+10                                                             
         MVC   TLCKESSN,SVFID                                                   
         GOTO1 HIGH                                                             
         J     OTD100                                                           
OTD90    GOTO1 SEQ                                                              
OTD100   CLC   KEY(TLCKECUR-TLCKPCD),KEYSAVE                                    
         JNE   OTDX                                                             
         CLC   TLCKEAGY,ACAGY                                                   
         JNE   OTD90                                                            
         CLI   TLCKEDTE,0                                                       
         JNE   OTD90                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         L     R4,AIO2           R4=A(CHECK RECORD)                             
                                                                                
         USING TLCKD,R4                                                         
         OC    SVFID,SVFID                                                      
         JZ    OTD105                                                           
         CLC   TLCKSSN,TGSSN                                                    
         JNE   OTD90                                                            
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
OTD105   MVI   ELCODE,TACAELQ    GET CAST DETAIL ELEMENT                        
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         XC    TACAGUA,VRHEXFFS                                                 
         CLC   TACAGUA,TGGUA     ONLY CONSIDER CHECKS WITH GUARANTEE            
         JNE   OTD90             CODE THAT MATCHES CURRENT PAYMENT              
         DROP  R4                                                               
                                                                                
         OC    ACCLI,ACCLI       IF NOT DELETING AN ENTIRE AGENCY ...           
         JZ    OTD110                                                           
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO2           R4=A(CHECK RECORD)                             
         MVI   ELCODE,TAPDELQ    GET PAYMENT DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAPDCLI,ACCLI     ONLY CONSIDER CHECKS FOR THE DELETED           
         JNE   OTD90             CLIENT                                         
         DROP  R4,R5                                                            
                                                                                
OTD110   MVC   SVKEY,KEY         SAVE CHECK KEY                                 
                                                                                
         MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR' RESET TO READ TALENT DIRECTORY               
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO2           R4=A(CHECK RECORD)                             
                                                                                
         USING TLINPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLINPCD,TLINBCDQ  IF INVOICE HAS NOT ALREADY BEEN                
         MVC   TLINBAGY,TLCKAGY  BILLED, RETURN ERROR                           
         MVC   TLINBINV,TLCKINV                                                 
         XC    TLINBINV,VRHEXFFS                                                
         GOTO1 HIGH                                                             
         CLC   TLINPKEY,KEYSAVE                                                 
         JNE   OTD120                                                           
         TM    TLINBST2,TAINSBIL                                                
         JZ    VRNDINVP                                                         
         DROP  R3,R4                                                            
                                                                                
OTD120   MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         MVC   KEY,SVKEY           RESTORE CHECK READ SEQUENCE                  
         GOTO1 HIGH                                                             
         J     OTD90                                                            
                                                                                
OTDX     MVC   SYSDIR,=CL8'TALDIR' RESTORE FILE SETTING                         
         MVC   SYSFIL,=CL8'TALFIL'                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO CHECK IF OK TO NARROW DOWN LIMIT IN AGENCY        *         
*        ON ENTRY ... R3=A(AGENCY/CLIENT ENTRY TO DELETE)                       
***********************************************************************         
                                                                                
OKTONAR  NTR1                                                                   
         USING ACTBLD,R3                                                        
         OC    ACCLI,ACCLI       IF AGENCY LIMIT IS BEING NARROWED              
         JNZ   XIT               DOWN                                           
                                                                                
         LA    R5,KEY            SET UP KEY AND IO AREA                         
         L     R4,AIO2                                                          
                                                                                
         USING TLCAPD,R5                                                        
         XC    KEY,KEY           READ ALL CAST RECORDS THAT                     
         MVI   TLCAPCD,TLCAGCDQ  ARE ATTACHED TO THIS GUARANTEE                 
         MVC   TLCAGSSN,TGSSN                                                   
         MVC   TLCAGGUA,TGGUA                                                   
         XC    TLCAGGUA,VRHEXFFS                                                
         GOTO1 HIGH                                                             
         J     OTN20                                                            
OTN10    GOTO1 SEQ                                                              
OTN20    CLC   KEY(TLCAGCOM-TLCAPCD),KEYSAVE                                    
         JNE   OTN50                                                            
         MVC   SVKEY,KEY                                                        
         DROP  R5                                                               
                                                                                
         USING TLCOPD,R5                                                        
         XC    KEY,KEY           GET COMMERCIAL RECORD FOR CAST                 
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVKEY+TLCAGCOM-TLCAPD                                   
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         JNE   OTN40                                                            
         GOTO1 GETREC                                                           
         DROP  R5                                                               
                                                                                
         USING TLCOD,R4                                                         
         CLC   TLCOAGY,TGAGY     IF COMMERCIAL AGENCY MATCHES                   
         JNE   OTN40             AGENCY BEING NARROWED DOWN                     
         CLC   TLCOCLI,TGCLI     AND CLIENT DOESN'T MATCH THE NEW               
         JNE   VRACATCH          CLIENT LIMIT, GIVE ERROR                       
         DROP  R4                                                               
                                                                                
OTN40    MVC   KEY,SVKEY         RESTORE CAST READ SEQUENCE                     
         GOTO1 HIGH                                                             
         J     OTN10                                                            
                                                                                
OTN50    MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKPD,R5                                                        
         XC    KEY,KEY           READ ALL CHECK RECORDS FOR THIS                
         MVI   TLCKPCD,TLCKECDQ  PERFORMER FOR THIS AGENCY                      
         MVC   TLCKESSN,TGSSN                                                   
         OC    SVFID,SVFID                                                      
         JZ    *+10                                                             
         MVC   TLCKESSN,SVFID                                                   
         GOTO1 HIGH                                                             
         J     OTN70                                                            
OTN60    GOTO1 SEQ                                                              
OTN70    CLC   KEY(TLCKECUR-TLCKPCD),KEYSAVE                                    
         JNE   OTNX                                                             
         CLC   TLCKEAGY,TGAGY                                                   
         JNE   OTN60                                                            
         CLI   TLCKEDTE,0                                                       
         JNE   OTN60                                                            
         GOTO1 GETREC                                                           
         DROP  R5                                                               
                                                                                
         L     R4,AIO2           R4=A(CHECK RECORD)                             
                                                                                
         USING TLCKD,R4                                                         
         OC    SVFID,SVFID                                                      
         JZ    OTN75                                                            
         CLC   TLCKSSN,TGSSN                                                    
         JNE   OTN60                                                            
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
OTN75    MVI   ELCODE,TACAELQ    GET CAST DETAIL ELEMENT                        
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         XC    TACAGUA,VRHEXFFS                                                 
         CLC   TACAGUA,TGGUA     ONLY CONSIDER CHECKS WITH GUARANTEE            
         JNE   OTN60             CODE THAT MATCHES CURRENT PAYMENT              
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO2           R4=A(CHECK RECORD)                             
         MVI   ELCODE,TAPDELQ    GET PAYMENT DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAPDCLI,TGCLI     ONLY CONSIDER CLIENTS THAT DON'T               
         JE    OTN60             MATCH THE NEW CLIENT LIMIT                     
         DROP  R4                                                               
                                                                                
         MVC   KEY,SVKEY         SAVE CHECK KEY                                 
                                                                                
         MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR' RESET TO READ TALENT DIRECTORY               
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO2           R4=A(CHECK RECORD)                             
                                                                                
         USING TLINPD,R5                                                        
         XC    KEY,KEY                                                          
         MVI   TLINPCD,TLINBCDQ  IF INVOICE HAS NOT ALREADY BEEN                
         MVC   TLINBAGY,TLCKAGY  BILLED, RETURN ERROR                           
         MVC   TLINBINV,TLCKINV                                                 
         XC    TLINBINV,VRHEXFFS                                                
         GOTO1 HIGH                                                             
         CLC   TLINPKEY,KEYSAVE                                                 
         JNE   OTN80                                                            
         TM    TLINBST2,TAINSBIL                                                
         JZ    VRACINVP                                                         
         DROP  R4,R5                                                            
                                                                                
OTN80    MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         MVC   KEY,SVKEY           RESTORE CHECK READ SEQUENCE                  
         GOTO1 HIGH                                                             
         J     OTN60                                                            
                                                                                
OTNX     MVC   ACCLI,TGCLI         MOVE NEW CLIENT INTO INITIAL TABLE           
         XC    ADDAC,ADDAC         AND CLEAR ADD ENTRY                          
         MVI   ADDAC,X'FF'                                                      
                                                                                
         MVC   SYSDIR,=CL8'TALDIR' AND RESTORE FILE SETTING                     
         MVC   SYSFIL,=CL8'TALFIL'                                              
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO BUILD AGENCY/CLIENT LIMIT ELEMENTS                *         
***********************************************************************         
                                                                                
BLDACELS NTR1                                                                   
         USING ACTBLD,R2                                                        
         L     R2,ACTBL          R2=A(INITIAL TABLE)                            
         LA    R3,ADDAC          R3=A(NEW AGENCY/CLIENT)                        
                                                                                
         USING TAVAD,R4                                                         
BACE10   LA    R4,MYELEM         R4=A(TAVA ELEMENT TO BUILD)                    
         XC    MYELEM,MYELEM                                                    
         MVI   TAVAEL,TAVAELQ    INITIALIZE AGENCY/CLIENT LIMIT ELEMENT         
         MVI   TAVALEN,TAVALNQ                                                  
                                                                                
         LA    R5,TAVACLI        R5=A(WHERE TO SAVE NXT CLI IN ELEMENT)         
                                                                                
BACE20   OC    ACTBLD(ACLNQ),ACTBLD                                             
         JNZ   BACE30                                                           
         LA    R2,ACLNQ(R2)      IF AT DELETED ENTRY FROM INITIAL TABLE         
         J     BACE20            BUMP TO NEXT INITIAL ENTRY                     
                                                                                
BACE30   CLI   0(R2),X'FF'       IF AT THE END OF BOTH TABLES                   
         JNE   BACE40            GO ADD THE FINAL AGY/CLI ELEMENT               
         CLI   0(R3),X'FF'                                                      
         JE    BACE120                                                          
                                                                                
BACE40   CLI   0(R2),X'FF'       IF AT END OF INITIAL TABLE                     
         JE    BACE60            GO ADD FROM ADDITIONS TABLE                    
                                                                                
         CLI   0(R3),X'FF'       IF AT END OF ADDITIONS TABLE                   
         JE    BACE50            GO ADD ENTRY FROM INITAL TABLE                 
                                                                                
         CLC   0(ACLNQ,R2),0(R3) COMPARE ENTRIES FROM INITIAL AND               
         JH    BACE60            ADDITIONS - ADD THE ALPHA LOWER                
                                                                                
BACE50   LR    R1,R2             SET TO ADD FROM INITIAL TABLE                  
         LA    R2,ACLNQ(R2)      AND BUMP TO NEXT INITIAL ENTRY                 
         J     BACE70                                                           
                                                                                
BACE60   LR    R1,R3             SET TO ADD FROM ADDITIONS TABLE                
         LA    R3,ACLNQ(R3)      AND BUMP TO NEXT ADDITIONS ENTRY               
                                                                                
BACE70   CLC   PRIAGY,ACAGY-ACTBLD(R1)                                          
         JNE   BACE90                                                           
         OC    PRICLI,PRICLI                                                    
         JZ    BACE80                                                           
         OC    ACCLI-ACTBLD(L'ACCLI,R1),ACCLI-ACTBLD(R1)                        
         JZ    BACE80                                                           
         CLC   PRICLI,ACCLI-ACTBLD(R1)                                          
         JNE   BACE90                                                           
BACE80   MVC   PRICLI,ACCLI-ACTBLD(R1)                                          
         XC    0(ACLNQ,R1),0(R1)                                                
         J     BACE20                                                           
                                                                                
BACE90   OC    TAVAAGY,TAVAAGY   IF ELEMENT BUILDING IN PROGRESS                
         JZ    BACE100                                                          
         CLC   TAVAAGY,ACAGY-ACTBLD(R1)                                         
         JE    BACE100           BUT CURRENT AGENCY DOES NOT MATCH              
         MVC   ELEMENT,MYELEM    THE ELEMENT, GO ADD THE ELEMENT                
         BRAS  RE,MYADDEL                                                       
         XC    MYELEM,MYELEM     AND INITIALIZE THE NEW ELEMENT                 
         MVI   TAVAEL,TAVAELQ                                                   
         MVI   TAVALEN,TAVALNQ                                                  
         LA    R5,TAVACLI                                                       
                                                                                
BACE100  MVC   TAVAAGY,ACAGY-ACTBLD(R1)                                         
                                                                                
         OC    ACCLI-ACTBLD(L'ACCLI,R1),ACCLI-ACTBLD(R1)                        
         JZ    BACE20                                                           
         MVC   0(L'TAVACLI,R5),ACCLI-ACTBLD(R1)                                 
         ZIC   RE,TAVALEN                                                       
         AHI   RE,L'TAVACLI     IF NEW CLIENT IS BEING ADDED                    
         STC   RE,TAVALEN       ADD CLIENT AND BUMP UP ELEMENT LENGTH           
                                                                                
         CLI   TAVALEN,255      IF ELEMENT IS NOW AT MAXIMUM LENGTH             
         JL    BACE110                                                          
         MVC   ELEMENT,MYELEM   ADD ELEMENT TO RECORD                           
         BRAS  RE,MYADDEL                                                       
         J     BACE10           AND REINITIALIZE THE ELEMENT                    
                                                                                
BACE110  LA    R5,L'ACCLI(R5)   BUMP TO SPOT IN ELEMENT FOR THE                 
         J     BACE20           NEXT CLIENT                                     
         DROP  R2                                                               
                                                                                
BACE120  OC    TAVAAGY,TAVAAGY  WHEN END OF BOTH TABLES IS REACHED              
         JZ    XIT                                                              
         MVC   ELEMENT,MYELEM   ADD THE FINAL ELEMENT                           
         BRAS  RE,MYADDEL                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO SELECT GUARANTEE TYPE AND LOAD SCREEN             *         
***********************************************************************         
                                                                                
SELTYPE  NTR1                                                                   
         CLI   ACTNUM,ACTADD     IF ACTION IS ADD                               
         JNE   XIT                                                              
         CLI   LOADSCRN,LOVRSCRN AND NOT ON LARGE OVERSCALE                     
         JE    XIT                                                              
         CLI   LOADSCRN,PCYCSCRN OR PER CYCLE SCREEN                            
         JE    XIT                                                              
         GOTO1 INITIAL,DMCB,(X'40',SELTYTAB)                                    
                                                                                
         CLI   PFAID,13          IF PF13 IS HIT                                 
         JNE   STYPE10                                                          
         MVI   LOADSCRN,LOVRSCRN LOAD LARGE OVERSCALE SCREEN                    
         J     STYPE20                                                          
                                                                                
STYPE10  CLI   PFAID,14          IF PF14 IS HIT                                 
         JNE   PFSELTYP                                                         
         MVI   LOADSCRN,PCYCSCRN LOAD PER CYCLE SCREEN                          
                                                                                
STYPE20  BRAS  RE,LOADIT                                                        
                                                                                
         CLI   LOADSCRN,PCYCSCRN IF PER CYCLE SCREEN LOADED                     
         JNE   XIT               HIDE POTENTIAL FIELDS                          
         GOTO1 FLDVAL,DMCB,(X'0A',SPCHED1H),(8,SPCPOM2H)                        
         GOTO1 FLDVAL,DMCB,(1,SPCPOT1H),SPCPOM2H                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE LARGE OVERSCALE GUARANTEE DETAILS        *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                           
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
VALLOV   NTR1                                                                   
         CLI   LOADSCRN,LOVRSCRN                                                
         JNE   XIT                                                              
                                                                                
         BAS   RE,SETPRIAC       SET PRIMARY AGENCY/CLIENT                      
                                                                                
         USING PERVALD,R5                                                       
         LA    R2,SLOPDH         VALIDATE GUARANTEE PERIOD                      
         LA    R5,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R5)                                                  
         TM    4(R1),PVRCONE     MUST INCLUDE START AND END DATES               
         BO    VRENDMIS                                                         
         BAS   RE,VALPD          MUST REMAIN INCLUSIVE OF ALL PAYMENTS          
         MVC   TAGUPD,PVALPSTA                                                  
         DROP  R5                                                               
                                                                                
         LA    R2,SLOAMTH        VALIDATE GUARANTEE AMOUNT                      
         GOTO1 ANY               (MUST BE ZERO OR POSITIVE)                     
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,WORK,(R3)                                           
         CLI   0(R1),X'FF'                                                      
         JE    VRINVAMT                                                         
         L     R5,4(R1)                                                         
         C     R5,=F'0'                                                         
         JL    VRINVAMT                                                         
                                                                                
         BAS   RE,PRO0AMT        PROCESS $0 AMOUNT                              
         BAS   RE,PRON0AMT       PROCESS NON-$0 AMOUNT                          
                                                                                
VLOV20   ST    R5,TAGUAMT        SAVE GUARANTEE AMOUNT                          
                                                                                
         XC    TAGUBAL,TAGUBAL                                                  
                                                                                
         TM    TAGUSTA2,TAGUSNEW IF GUARANTEE WAS ADDED VIA                     
         JZ    VLOV30            NEW GUARANTEE SYSTEM                           
         TM    TAGUSTAT,TAGUSDES AND BALANCE IS ASCENDING                       
         JO    VLOV30            OK TO LEAVE "BALANCE" BLANK                    
         CLI   SLOBALH+5,0       BECAUSE IT MUST BE ZERO                        
         JE    VLOV40                                                           
                                                                                
VLOV30   LA    R2,SLOBALH        VALIDATE BALANCE                               
         GOTO1 ANY               (CAN BE POSITIVE, ZERO OR NEGATIVE)            
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,WORK,(R3)                                           
         CLI   0(R1),X'FF'                                                      
         JE    VRINVAMT                                                         
         MVC   TAGUBAL,4(R1)                                                    
                                                                                
         TM    SLOBALH+1,X'20'   IF BALANCE FIELD IS NOT PROTECTED              
         JO    VLOV40            AND GUARANTEE WAS ADDED VIA                    
         TM    TAGUSTA2,TAGUSNEW NEW GUARANTEE SYSTEM                           
         JZ    VLOV40            AND BALANCE IS ASCENDING                       
         TM    TAGUSTAT,TAGUSDES BALANCE MUST BE ZERO                           
         JO    VLOV40                                                           
         OC    TAGUBAL,TAGUBAL                                                  
         JNZ   VRINVAMT                                                         
                                                                                
VLOV40   NI    TAGUSTAT,X'FF'-TAGUSOVR-TAGUSLCK                                 
         NI    TAGUSTA2,X'FF'-TAGUSIGP                                          
                                                                                
         TM    TAGUSTA2,TAGUSNEW IF GUARANTEE WAS ADDED VIA                     
         JO    VLOV45            OLD GUARANTEE SYSTEM ...                       
         LA    R2,SLODBALH       R2=A(DESCENDING BALANCE FIELD)                 
         CLI   5(R2),0           INPUT IS REQUIRED                              
         JE    VRMISS                                                           
         NI    TAGUSTAT,X'FF'-TAGUSDES                                          
         CLI   8(R2),C'N'        MUST BE A "Y" OR "N"                           
         JE    VLOV50                                                           
         CLI   8(R2),C'Y'                                                       
         JNE   VRINV                                                            
         OI    TAGUSTAT,TAGUSDES                                                
                                                                                
VLOV45   LA    R2,SLOGCNTH                                                      
         XC    TAGUGCNT,TAGUGCNT                                                
         CLI   5(R2),0                                                          
         JE    VLOV50                                                           
         CLI   5(R2),L'SLOGCNT                                                  
         JNE   VRINV                                                            
         CLC   8(2,R2),=C'GC'                                                   
         JNE   VRINV                                                            
                                                                                
***      TM    TAGUSTAT,TAGUSDES   GCON ONLY VALID FOR DESCENDING BAL           
***      JZ    VRINV                                                            
                                                                                
         ZIC   R1,5(R2)                                                         
         SHI   R1,3                2 FOR 'GC' AND 1 FOR EXPACK                  
         EX    R1,*+8                                                           
         J     *+10                                                             
         PACK  DUB,10(0,R2)                                                     
         CVB   R1,DUB                                                           
         LNR   R1,R1               COMPLEMENT GRT CONTRACT CODE                 
         STCM  R1,15,TAGUGCNT      GRT CONTRACT CODE                            
                                                                                
         LA    R3,KEY              VALIDATE GUARANTEE CONTRACT CODE             
         USING TLGCD,R3                                                         
         XC    TLGCKEY,TLGCKEY                                                  
         MVI   TLGCCD,TLGCCDQ                                                   
         MVC   TLGCSSN,TGSSN                                                    
         STCM  R1,15,TLGCGCNT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLGCKEY),KEYSAVE                                           
         JNE   VRINV                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         GOTO1 HELLO,DMCB,(C'G',=C'TALFIL'),(X'63',AIO),0                       
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RF,12(R1)                                                        
         USING TAGDD,RF                                                         
                                                                                
         CLC   TAGUSTRT,TAGDSTRT   TEST GRT PERIOD WITHIN GCON TERM             
         JL    VRINV                                                            
         CLC   TAGUEND,TAGDEND                                                  
         JH    VRINV                                                            
         DROP  RF                                                               
                                                                                
VLOV50   TM    TAGUSTAT,TAGUSDES IF BALANCE IS ASCENDING                        
         JO    VLOV60            OK TO LEAVE "PAY OVERAGE" BLANK                
         CLI   SLOOVERH+5,0      BECAUSE IT MUST BE NO                          
         JE    VLOV70                                                           
                                                                                
VLOV60   GOTO1 VALSTAT,DMCB,SLOOVERH,('TAGUSOVR',TAGUSTAT)                      
                                                                                
         TM    TAGUSTAT,TAGUSOVR IF PAYING OVERAGE                              
         JZ    VLOV70                                                           
         LA    R2,SLOOVERH       GUARANTEE CANNOT HAVE AN                       
         TM    TAGUSTAT,TAGUSDES ASCENDING BALANCE                              
         JZ    VRINV                                                            
*                                IF PAYING OVERAGE, OK TO LEAVE                 
         CLI   SLOESTH+5,0       "IGNORE PAYING OVERAGE FOR ESTIMATE"           
         JE    VLOV80            BLANK BECAUSE IT MUST BE NO                    
                                                                                
VLOV70   GOTO1 VALSTAT,DMCB,SLOESTH,('TAGUSIGP',TAGUSTA2)                       
                                                                                
         TM    TAGUSTA2,TAGUSIGP IF IGNORING PAYING OVERAGE FOR                 
         JZ    VLOV80            ESTIMATING                                     
         LA    R2,SLOESTH                                                       
         TM    TAGUSTAT,TAGUSOVR OVERAGE PAID MUST BE NO                        
         JO    VRINV                                                            
                                                                                
VLOV80   CLI   SLOLCKH+5,0       VALIDATE LOCKED STATUS                         
         JE    VLOV90                                                           
         GOTO1 VALSTAT,DMCB,SLOLCKH,('TAGUSLCK',TAGUSTAT)                       
                                                                                
VLOV90   TM    TAGUSTA2,TAGUSNEW IF GUARANTEE WAS ADDED VIA                     
         JO    VLOV100           OLD GUARANTEE SYSTEM                           
         TM    TAGUSTAT,TAGUSPNH AND PAY P&H ON USE OPTION ALREADY SET          
         JZ    VLOV100                                                          
         CLI   SLOPNH,C'Y'       OK TO CONTINUE PAYING P&H ON USE               
         JE    VLOV110                                                          
                                                                                
VLOV100  NI    TAGUSTAT,X'FF'-TAGUSPNH                                          
                                                                                
         CLI   SLOPNHH+5,0                                                      
         JE    VLOV110                                                          
         GOTO1 VALSTAT,DMCB,SLOPNHH,('TAGUSPNH',TAGUSTAT)                       
                                                                                
         TM    TAGUSTAT,TAGUSPNH IF PAYING P&H ON USE                           
         JZ    VLOV110                                                          
         LA    R2,SLOPNHH                                                       
         OC    TAGUINV,TAGUINV   GUARANTEE MUST HAVE BEEN ADDED                 
         JNZ   VRINV             VIA GRT/ADD                                    
                                                                                
VLOV110  MVI   MSCFLAG,0                                                        
         LA    R2,SLOMSCH                                                       
         CLI   SLOMSCH+5,0                                                      
         JE    *+12                                                             
         CLI   SLOMSC,C'N'                                                      
         JNE   VLOV115                                                          
         CLI   MYPFKEY,20        CHANGE CONFIRMED                               
         JNE   VLOV112                                                          
         NI    TAGUSTA2,X'FF'-TAGUSMSC                                          
         MVI   MSCFLAG,MSCFLCHG                                                 
         MVI   MYPFKEY,0                                                        
         J     VLOV120                                                          
                                                                                
VLOV112  TM    TAGUSTA2,TAGUSMSC TEST IF CHANGING FROM Y TO N/BLANK             
         JZ    VLOV120                                                          
         BAS   RE,CHKMSC         CHECK IF MSC CHECKS EXIST                      
         NI    TAGUSTA2,X'FF'-TAGUSMSC                                          
         MVI   MSCFLAG,MSCFLCHG                                                 
         J     VLOV120                                                          
                                                                                
VLOV115  CLI   SLOMSC,C'N'                                                      
         JE    VLOV120                                                          
         CLI   SLOMSC,C'Y'                                                      
         JNE   VRINV                                                            
         OI    TAGUSTA2,TAGUSMSC GUARANTEE MULTI-SERVICE CONTRACT               
         MVI   MSCFLAG,MSCFLCHG                                                 
                                                                                
VLOV120  GOTOR VALEXUS,DMCB,SLOXUSEH                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO SET PRIMARY AGENCY/CLIENT                         *         
***********************************************************************         
                                                                                
SETPRIAC NTR1                                                                   
         OC    PRIAGY(L'PRIAGY+L'PRICLI),PRIAGY                                 
         JNZ   SPAC10                                                           
         MVC   PRIAGY,TGAGY      SET PRIMARY AGENCY                             
         MVC   PRICLI,TGCLI      AND PRIMARY CLIENT                             
         J     XIT                                                              
                                                                                
SPAC10   XR    R1,R1             R1=NUMBER OF ENTRIES MARKED PRIMARY            
                                                                                
         LA    R2,GRTFRSTH       R2=A(FIRST PRIMARY/DELETE FIELD)               
         LA    R5,GRTLSTH        R5=A(LAST PRIMARY/DELETE FIELD)                
                                                                                
SPAC20   CR    R2,R5             CHECK ALL PRIMARY/DELETE FIELDS                
         JNL   SPAC50                                                           
                                                                                
         LR    R3,R2             BUMP R3 TO AGENCY FIELD                        
         ZIC   RE,0(R3)                                                         
         AR    R3,RE                                                            
                                                                                
         LR    R4,R3             BUMP R4 TO CLIENT FIELD                        
         ZIC   RE,0(R4)                                                         
         AR    R4,RE                                                            
                                                                                
         CLI   8(R2),C'*'        IF ENTRY IS SET TO BE PRIMARY                  
         JNE   SPAC40                                                           
         LTR   R1,R1             ENSURE PRIMARY IS NOT ALREADY SET              
         JNE   VRINV             ON THIS SCREEN                                 
         CLI   5(R3),0           ENSURE AGENCY IS PRESENT                       
         JE    VRINV                                                            
         MVC   PRIAGY,8(R3)      SAVE NEW PRIMARY AGENCY                        
         OC    PRIAGY,SPACES                                                    
         LHI   R1,1                                                             
                                                                                
         MVC   PRICLI,8(R4)                                                     
         OC    PRICLI,PRICLI                                                    
         JZ    SPAC40                                                           
         OC    PRICLI,SPACES                                                    
                                                                                
SPAC40   LR    R2,R4                                                            
         ZIC   RE,0(R2)          BUMP TO NEXT PRIMARY/DELETE FIELD              
         AR    R2,RE                                                            
         J     SPAC20                                                           
                                                                                
SPAC50   OC    PRIAGY,PRIAGY     PRIMARY AGENCY/CLIENT MUST BE SET              
         JNZ   XIT                                                              
         LA    R2,GRTFRSTH                                                      
         J     VRMISS                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO ENSURE PERIOD REMAINS INCLUSIVE OF ALL PAYMENTS   *         
*        ON ENTRY ... R5=A(PERVAL BLOCK)                              *         
***********************************************************************         
                                                                                
         USING PERVALD,R5                                                       
VALPD    NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JE    XIT                                                              
         CLC   SVGUPD,PVALPSTA     OR PERIOD FIELD HAS NOT CHANGED              
         JE    XIT                                                              
                                                                                
         XR    R0,R0               COUNT NUMBER OF CHECKS FOR THIS GRT          
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY              READ ALL CHECK RECORDS FOR THIS              
         XC    KEY,KEY             PERFORMER                                    
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,TGSSN                                                   
         OC    SVFID,SVFID                                                      
         JZ    *+10                                                             
         MVC   TLCKESSN,SVFID                                                   
         GOTO1 HIGH                                                             
         J     VPD20                                                            
VPD10    GOTO1 SEQ                                                              
VPD20    CLC   KEY(TLCKECUR-TLCKPCD),KEYSAVE                                    
         JNE   VPD40                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         L     R4,AIO2                                                          
                                                                                
         USING TLCKD,R4                                                         
         OC    SVFID,SVFID                                                      
         JZ    VPD25                                                            
         CLC   TLCKSSN,TGSSN                                                    
         JNE   VPD10                                                            
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
VPD25    MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,GRTGUA      IGNORE IF PAYMENT IS NOT FOR                 
         JNE   VPD10               THIS GUARANTEE                               
         DROP  R4                                                               
                                                                                
         AHI   R0,1                ADD TO GRT CHECKS COUNTER                    
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO2             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAPDCYCS,TAPDCYCS   IF PAYMENT HAS CYCLE DATES                   
         JZ    VPD10                                                            
         MVC   TGDATE,TAPDCYCS     SAVE PAYMENT'S APPLY DATE                    
         TM    TAPDOPT4,TAPDGRTE   INTO TGDATE                                  
         JZ    VPD30                                                            
         MVC   TGDATE,TAPDCYCE                                                  
         DROP  R4                                                               
                                                                                
VPD30    CLC   TGDATE,PVALPSTA                                                  
         JL    VRINVPD                                                          
         CLC   TGDATE,PVALPEND     IF APPLY DATE DOES NOT FIT WITHIN            
         JNH   VPD10               GUARANTEE CYCLE, ERROR                       
         J     VRINVPD                                                          
         DROP  R5                                                               
                                                                                
VPD40    MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR'                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO PROCESS $0 AMOUNT INPUT                           *         
*        ON ENTRY ... R2=A(AMOUNT FIELD)                              *         
*                     R4=A(GUARANTEE DETAILS ELEMENT)                 *         
*                     R5=INPUTTED AMOUNT                              *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
PRO0AMT  NTR1                                                                   
         LTR   R5,R5             IF AMOUNT IS $0                                
         JNZ   XIT                                                              
                                                                                
         OC    TAGUINV,TAGUINV   GUARANTEE CANNOT BE ADDED VIA GRT/PAY          
         JNZ   VRINVAMT                                                         
                                                                                
         OC    TAGUAMT,TAGUAMT   IF GUARANTEE WAS ORIGINALLY SET UP             
         JZ    P0A10             WITH A NON-$0 AMOUNT                           
         BRAS  RE,APPLPYMS       AND PAYMENTS HAVE APPLIED AGAINST              
         BE    VRINVAMT          THE GUARANTEE, AMOUNT CANNOT BE $0             
                                                                                
P0A10    NI    TAGUSTAT,X'FF'-TAGUSDES  SET FOR ASCENDING BALANCE               
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO PROCESS NON-$0 AMOUNT INPUT                       *         
*        ON ENTRY ... R2=A(AMOUNT FIELD)                              *         
*                     R4=A(GUARANTEE DETAILS ELEMENT)                 *         
*                     R5=INPUTTED AMOUNT                              *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
PRON0AMT NTR1                                                                   
         LTR   R5,R5             IF AMOUNT IS GREATER THAN $0                   
         JZ    XIT                                                              
         TM    TAGUSTA2,TAGUSNEW AND GUARANTEE WAS ADDED BY                     
         JZ    XIT               NEW GUARANTEE SYSTEM                           
         C     R5,=F'25000'      AMOUNT MUST BE $250 OR MORE                    
         JL    VRINVAMT                                                         
         OI    TAGUSTAT,TAGUSDES BALANCE WILL AUTOMATICALLY DESCEND             
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE PER CYCLE GUARANTEE DETAILS              *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                 *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
VALPCY   NTR1                                                                   
         CLI   LOADSCRN,PCYCSCRN                                                
         JNE   XIT                                                              
                                                                                
         MVC   ORIGPRI,TAGUCOM     SAVE ORIGINAL PRIMARY COMMERCIAL             
         MVC   PRICOM,TAGUCOM      DEFAULT NEW PRIMARY TO THE ORIGINAL          
                                                                                
         CLI   ACTNUM,ACTADD       IF ADDING GUARANTEE                          
         JNE   VPCY10                                                           
                                                                                
         LA    R2,SPCCIDH          VALIDATE PRIMARY COMMERCIAL ID               
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'20',(R2))                                
                                                                                
         BRAS  RE,ELIGPRI          ELIGIBLE TO BE PRIMARY?                      
         JNE   VRINV                                                            
                                                                                
         USING TLCOD,RE                                                         
         L     RE,AIO2                                                          
         MVC   PRIAGY,TLCOAGY      SAVE PRIMARY COMMERCIAL'S AGENCY             
         MVC   PRICLI,TLCOCLI      CLIENT                                       
         MVC   PRICOM,TLCOCOM      AND INTERNAL COMMERCIAL NUMBER               
         DROP  RE                                                               
                                                                                
         OI    TAGUSTAT,TAGUSDES+TAGUSOVR  SAVE DEFAULT VALUES                  
                                                                                
VPCY10   BAS   RE,SETNPRI          SET NEW PRIMARY COMMERCIAL                   
         MVC   TAGUCOM,PRICOM                                                   
                                                                                
         CLI   SPCLCKH+5,0                                                      
         JE    VPCY20                                                           
         NI    TAGUSTAT,X'FF'-TAGUSLCK                                          
         GOTO1 VALSTAT,DMCB,SPCLCKH,('TAGUSLCK',TAGUSTAT)                       
         DROP  R4                                                               
                                                                                
VPCY20   GOTOR VALEXUS,DMCB,SPCXUSEH                                            
*                                                                               
         BAS   RE,UPDCAST          UPDATE CAST FOR OLD/NEW PRIMARY              
         J     XIT                                                              
***********************************************************************         
*        ROUTINE TO CHECK IF MSC CHECKS EXIST WHEN TURNING OFF        *         
*        MSC ON GRT RECORD                                            *         
***********************************************************************         
                                                                                
CHKMSC   NTR1                                                                   
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY              READ ALL CHECK RECORDS FOR THIS              
         XC    KEY,KEY             PERFORMER                                    
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,TGSSN                                                   
         OC    SVFID,SVFID                                                      
         JZ    *+10                                                             
         MVC   TLCKESSN,SVFID                                                   
         GOTO1 HIGH                                                             
         J     CMSC20                                                           
CMSCSEQ  GOTO1 SEQ                                                              
CMSC20   CLC   KEY(TLCKECUR-TLCKPCD),KEYSAVE                                    
         JNE   CMSCX                                                            
         LA    R3,KEY                                                           
         TM    TLCKESTA,TLCKEMSC   TEST IF MULTI-CONTRACT SERVICE               
         JZ    CMSCSEQ                                                          
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         L     R4,AIO2           R4=A(CHECK RECORD)                             
                                                                                
         USING TLCKD,R4                                                         
         OC    SVFID,SVFID                                                      
         JZ    CMSC30                                                           
         CLC   TLCKSSN,TGSSN                                                    
         JNE   CMSCSEQ                                                          
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
CMSC30   MVI   ELCODE,TACAELQ    GET CAST ELEMENT TO MATCH GUAR CODE            
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   FULL,TGGUA                                                       
         XC    FULL,=X'FFFFFFFF'                                                
         CLC   TACAGUA,FULL      ONLY CONSIDER CHECKS WITH GUARANTEE            
         JNE   CMSCSEQ                                                          
         DROP  R4                                                               
                                                                                
         J     MSCERR            RETURN WARNING                                 
                                                                                
CMSCX    MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR'                                              
         J     XIT                                                              
***********************************************************************         
*        ROUTINE TO UDPATE CHECK RECORDS WITH MSC STATUS              *         
***********************************************************************         
                                                                                
UPDCKMSC NTR1                                                                   
         CLI   MSCFLAG,MSCFLCHG    DID MSC CHANGE?                              
         JNE   XIT                 NO - NO NEED TO UPDATE CHECKS                
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   AIO,AIO2                                                         
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY              READ ALL CHECK RECORDS FOR THIS              
         XC    KEY,KEY             PERFORMER                                    
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,TGSSN                                                   
         OC    SVFID,SVFID                                                      
         JZ    *+10                                                             
         MVC   TLCKESSN,SVFID                                                   
         GOTO1 HIGH                                                             
         J     UCMSC20                                                          
UCMSCSEQ GOTO1 SEQ                                                              
UCMSC20  CLC   KEY(TLCKECUR-TLCKPCD),KEYSAVE                                    
         JNE   UCMSCX                                                           
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         L     R4,AIO2           R4=A(CHECK RECORD)                             
                                                                                
         USING TLCKD,R4                                                         
         OC    SVFID,SVFID                                                      
         JZ    UCMSC30                                                          
         CLC   TLCKSSN,TGSSN                                                    
         JNE   UCMSCSEQ                                                         
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
UCMSC30  MVI   ELCODE,TACAELQ    GET CAST ELEMENT TO MATCH GUAR CODE            
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   FULL,TGGUA                                                       
         XC    FULL,=X'FFFFFFFF'                                                
         CLC   TACAGUA,FULL      ONLY CONSIDER CHECKS WITH GUARANTEE            
         JNE   UCMSCSEQ                                                         
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVC   SVKEY,KEY                                                        
         GOTO1 SAVPTRS,DMCB,PTRBLK     HANDLE PASSIVE POINTERS                  
                                                                                
         USING TACDD,R4                                                         
         L     R4,AIO2           R4=A(CHECK RECORD)                             
         MVI   ELCODE,TACDELQ    GET CHECK DETAILS ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         NI    TACDSTA2,X'FF'-TACDSMSC                                          
         CLI   SLOMSC,C'N'         MARK CHECK MSC OR NOT MSC                    
         JE    *+8                                                              
         OI    TACDSTA2,TACDSMSC                                                
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         GOTO1 ADDPTRS,DMCB,(8,PTRBLK),UPDPTRS   UPDATE PASSIVES                
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY                                                           
         MVC   KEY,SVKEY                                                        
         MVI   TLCKESTA,0                                                       
         GOTO1 HIGH                                                             
         J     UCMSCSEQ                                                         
         DROP  R3,R4                                                            
                                                                                
UCMSCX   MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR'                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE STATUS FIELD                             *         
*        PARAMETERS ... P1=A(STATUS SCREEN FIELD)                     *         
*                       P2=A(STATUS BYE)                              *         
*                       P2 BYTE 0 = STATUS EQUATE                     *         
***********************************************************************         
                                                                                
VALSTAT  NTR1                                                                   
         L     R2,0(R1)            R2=A(STATUS SCREEN FIELD)                    
         CLI   5(R2),0             INPUT MUST BE PRESENT                        
         JE    VRMISS                                                           
         CLI   8(R2),C'N'          INPUT MUST BE "N" OR "Y"                     
         JE    XIT                                                              
         CLI   8(R2),C'Y'                                                       
         JNE   VRINV                                                            
         L     R3,4(R1)            IF INPUT IS "Y" TURN ON BIT                  
         OC    0(1,R3),4(R1)       IN STATUS BYTE                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE EXCLUDED USES                            *         
*        ON ENTRY ... P1 = A(EXCLUDED USES FIELD)                     *         
***********************************************************************         
                                                                                
VALEXUS  NTR1                                                                   
         L     R2,0(R1)            R2=A(EXCLUDED USES FIELD)                    
                                                                                
         MVI   ELCODE,TAGXELQ      DELETE EXISTING EXCLUDED USES                
         BRAS  RE,MYREMEL          ELEMENT                                      
                                                                                
         CLI   5(R2),0             EXIT IF NO INPUT                             
         JE    XIT                                                              
                                                                                
         USING TAGXD,R4                                                         
         LA    R4,ELEMENT          INITIALIZE NEW ELEMENT                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAGXEL,TAGXELQ                                                   
                                                                                
         GOTO1 SCANNER,DMCB,(R2),(13,BLOCK),0                                   
         CLI   4(R1),0                                                          
         JE    VRINV               SCAN FIELD INTO SCANNER BLOCK                
         MVC   BYTE,4(R1)          SAVE NUMBER OF ENTRIES IN BYTE               
                                                                                
         USING SCAND,R3                                                         
         LA    R3,BLOCK            R3 = A(SCANNER BLOCK)                        
         ZIC   RF,4(R1)            FULL = NUMBER OF SCAN BLOCK ENTRIES          
         ST    RF,FULL                                                          
         SR    R0,R0               R0 = DISPLACEMENT INTO FIELD                 
         LA    R4,TAGXSBEL         R4 = A(FIRST SUB ELEMENT)                    
                                                                                
VREU10   CLI   SCLEN1,3            INPUT MUST BE 3 CHARACTERS LONG              
         JNE   VRINVDIS                                                         
                                                                                
         GOTO1 USEVAL,DMCB,(X'40',SCDATA1),0                                    
         JNE   VRINVDIS            VALIDATE USE CODE                            
                                                                                
         CLI   LOADSCRN,LOVRSCRN   IF GUARANTEE IS LARGE OVERSCALE              
         JNE   VREU20                                                           
         CLI   TGUSEQU,UGRT        CANNOT EXCLUDE GRT PAYMENTS                  
         JE    VRINVDIS                                                         
                                                                                
VREU20   MVC   0(3,R4),TGUSEQU     AND SAVE IN ELEMENT                          
                                                                                
         SR    RF,RF               RIGHT HAND SIDE INPUT IS NOT                 
         ICM   RF,1,SCLEN2         ALLOWED                                      
         JNZ   VRINVDIS                                                         
                                                                                
         ZIC   RF,SCLEN1           BUMP R0 TO NEXT FIELD                        
         LA    RF,1(RF)                                                         
         AR    R0,RF                                                            
         ZIC   RF,SCLEN2                                                        
         LA    RF,1(RF)                                                         
         AR    R0,RF                                                            
         LA    R4,L'TAGXUSE(R4)    BUMP R4 TO NEXT SUB ELEMENT                  
         LA    R3,SCANNEXT         BUMP R3 TO NEXT SCANNER FIELD                
                                                                                
         L     RF,FULL             REPEAT UNTIL NO MORE SCANNER FIELDS          
         BCTR  RF,0                                                             
         ST    RF,FULL                                                          
         LTR   RF,RF                                                            
         JNZ   VREU10                                                           
         DROP  R3                                                               
                                                                                
         LR    RF,R4               SET LENGTH OF ELEMENT                        
         LA    R4,ELEMENT                                                       
         SR    RF,R4                                                            
         STC   RF,TAGXLEN                                                       
         BRAS  RE,MYADDEL                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO SET NEW PRIMARY COMMERCIAL                        *         
***********************************************************************         
                                                                                
SETNPRI  NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'80',SPCPOT1H),(X'80',SPCPOTLH)                    
         JE    XIT                                                              
                                                                                
         L     R5,ECTBL          BUILD ELIGIBLE COMMERCIALS TABLE               
         BRAS  RE,BLDETBL                                                       
                                                                                
         XC    PRICOM,PRICOM     CLEAR PRIMARY COMMERCIAL                       
                                                                                
         LA    R2,SPCPOT1H       R2=A(FIRST POTENTIAL PRIMARY FIELD)            
         LA    R3,SPCPOTLH       R3=A(LAST POTENTIAL PRIMARY FIELD)             
SNP10    CR    R2,R3                                                            
         JNL   XIT                                                              
         CLI   8(R2),C'S'        IF ENTRY IS NOT SET TO BE NEW PRIMARY          
         JE    SNP20                                                            
         CLI   8(R2),0                                                          
         JNE   VRINV                                                            
         ZIC   RE,0(R2)          BUMP TO NEXT AGENCY FIELD                      
         AR    R2,RE                                                            
         ZIC   RE,0(R2)          BUMP TO NEXT COMMERCIAL FIELD                  
         AR    R2,RE                                                            
         ZIC   RE,0(R2)          BUMP TO NEXT SELECT FIELD                      
         AR    R2,RE                                                            
         J     SNP10                                                            
                                                                                
SNP20    ST    R2,ASELFLD                                                       
                                                                                
         OC    PRICOM,PRICOM     CAN ONLY SET ONE COMMERCIAL AS                 
         JNZ   VRINVSEL          THE PRIMARY                                    
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         CLI   5(RE),0           ENSURE AGENCY IS PRESENT                       
         JE    VRINV                                                            
                                                                                
         LR    R1,RE             R1=A(AGENCY FIELD)                             
         OC    8(6,R1),SPACES    PAD IT WITH SPACES                             
                                                                                
         ZIC   R2,0(R1)                                                         
         AR    R2,R1             R2=A(COMMERCIAL FIELD)                         
         OC    8(12,R2),SPACES   PAD IT WITH SPACES                             
                                                                                
         USING ECTBLD,R5                                                        
         L     R5,ECTBL          R5=A(ELIGIBLE COMMERCIALS TABLE)               
SNP30    CLI   0(R5),X'FF'       MUST FIND AGENCY/COMM'L IN THE TABLE           
         JNE   *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLC   ECAGY,8(R1)       WHEN ENTRY TO SET AS NEW PRIMARY               
         JNE   SNP40             IS FOUND                                       
         CLC   ECCID,8(R2)                                                      
         JNE   SNP40                                                            
         MVC   PRIAGY,ECAGY      SAVE NEW PRIMARY COMMERCIAL'S AGENCY           
         MVC   PRICLI,ECCLI      CLIENT                                         
         MVC   PRICOM,ECCOM      AND INTERNAL COMMERCIAL NUMBER                 
                                                                                
         ZIC   RE,0(R2)          BUMP TO SELECT FIELD                           
         AR    R2,RE                                                            
         J     SNP10                                                            
                                                                                
SNP40    LA    R5,ECLNQ(R5)                                                     
         J     SNP30                                                            
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE UPDATES CAST/COMMERCIAL RECORDS OF ORIGINAL PRIMARY  *         
*        COMMERCIAL AND CAST OF NEW PRIMARY COMMERCIAL                *         
***********************************************************************         
                                                                                
UPDCAST  NTR1                                                                   
         CLI   ACTNUM,ACTADD     IF ACTION IS NOT ADD                           
         JE    XIT                                                              
         CLC   ORIGPRI,PRICOM    AND THE PRIMARY COMMERCIAL HAS CHANGED         
         JE    XIT                                                              
                                                                                
         USING TLCAPD,R3                                                        
         LA    R3,KEY            READ CAST KEYS FOR THE ORIGINAL                
         XC    KEY,KEY           PRIMARY COMMERCIAL                             
         MVI   TLCAPCD,TLCAHCDQ                                                 
         MVC   TLCAHCOM,ORIGPRI                                                 
         GOTO1 HIGH                                                             
         J     UCAST20                                                          
UCAST10  GOTO1 SEQ                                                              
UCAST20  CLC   KEY(TLCAHSRT-TLCAPD),KEYSAVE                                     
         JNE   XIT                                                              
         OC    TLCAHSRT,TLCAHSRT                                                
         JZ    UCAST10                                                          
         CLC   TLCAHSSN,TGSSN                                                   
         JNE   UCAST10                                                          
         DROP  R3                                                               
                                                                                
         MVI   RDUPDATE,C'Y'     READ CAST RECORD                               
         GOTO1 GETREC                                                           
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ    ENSURE THAT GUARANTEE CODE IN                  
         BRAS  RE,GETEL          CAST DETAILS MATCHES THIS GUARANTEE            
         JNE   UCAST10                                                          
         XC    TACAGUA,VRHEXFFS                                                 
         CLC   TACAGUA,TGGUA                                                    
         JNE   UCAST10                                                          
         XC    TACAGUA,VRHEXFFS                                                 
         DROP  R4                                                               
                                                                                
         USING TAOAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAOAELQ    SAVE CAST OVERSCALE AMOUNT ELEMENT             
         BRAS  RE,GETEL          FROM ORIGINAL PRIMARY COMMERCIAL               
         JNE   XIT                                                              
         MVC   WORK,TAOAD                                                       
         GOTO1 REMELEM           THEN DELETE IT                                 
                                                                                
         LA    R4,ELEMENT        INITIALIZE NEW CAST OVERSCALE AMOUNT           
         XC    ELEMENT,ELEMENT   ELEMENT FOR OLD PRIMARY COMMERCIAL             
         MVI   TAOAEL,TAOAELQ                                                   
         MVI   TAOALEN,TAOALNQ                                                  
                                                                                
         LA    RE,WORK+TAOAUSE-TAOAD   RE=A(INITIAL OVERSCALE AMOUNTS)          
         ZIC   RF,WORK+TAOANUM-TAOAD   RF=# OF OVERSCALE AMOUNTS                
                                                                                
         LA    R1,TAOAUSE        R1=A(NEW OVERSCALE AMOUNTS)                    
                                                                                
UCAST30  CLC   =C'HLD',0(RE)     STRIP HOLDING FEE TYPES                        
         JE    UCAST40           OUT OF THE INITIAL OVERSCALE AMOUNTS           
         CLC   =C'SHL',0(RE)                                                    
         JE    UCAST40                                                          
         CLC   =C'ADH',0(RE)                                                    
         JE    UCAST40                                                          
                                                                                
         ZIC   R0,TAOALEN                                                       
         AHI   R0,L'TAOASBEL                                                    
         STC   R0,TAOALEN        BUMP ELEMENT LENGTH                            
                                                                                
         ZIC   R0,TAOANUM                                                       
         AHI   R0,1                                                             
         STC   R0,TAOANUM        BUMP # OF OVERSCALE AMOUNTS                    
                                                                                
         MVC   0(L'TAOASBEL,R1),0(RE)                                           
                                                                                
         LA    R1,L'TAOASBEL(R1)                                                
                                                                                
UCAST40  LA    RE,L'TAOASBEL(RE)                                                
         BCT   RF,UCAST30                                                       
                                                                                
         CLI   TAOANUM,0         IF ANY OVERSCALE AMOUNTS IN ELEMENT            
         JE    UCAST50                                                          
         GOTO1 ADDELEM           ADD NEW OVERSCALE AMOUNTS ELEMENT              
         DROP  R4                                                               
                                                                                
UCAST50  GOTO1 PUTREC            AND PUT CAST RECORD                            
                                                                                
         USING TLCAPD,R3                                                        
         XC    KEY,KEY           READ CAST KEYS FOR THE NEW PRIMARY             
         MVI   TLCAPCD,TLCAHCDQ  COMMERCIAL                                     
         MVC   TLCAHCOM,PRICOM                                                  
         GOTO1 HIGH                                                             
         J     UCAST70                                                          
UCAST60  GOTO1 SEQ                                                              
UCAST70  CLC   KEY(TLCAHSRT-TLCAPD),KEYSAVE                                     
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TLCAHSRT,TLCAHSRT                                                
         JZ    UCAST60                                                          
         CLC   TLCAHSSN,TGSSN                                                   
         JNE   UCAST60                                                          
         DROP  R3                                                               
                                                                                
         MVI   RDUPDATE,C'Y'     READ CAST RECORD                               
         GOTO1 GETREC                                                           
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ    ENSURE THAT GUARANTEE CODE IN                  
         BRAS  RE,GETEL          CAST DETAILS MATCHES THIS GUARANTEE            
         JNE   UCAST60                                                          
         XC    TACAGUA,VRHEXFFS                                                 
         CLC   TACAGUA,TGGUA                                                    
         JNE   UCAST60                                                          
         XC    TACAGUA,VRHEXFFS                                                 
         DROP  R4                                                               
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         XC    ATAOAHLD,ATAOAHLD                                                
         XC    ATAOASHL,ATAOASHL                                                
         XC    ATAOAADH,ATAOAADH                                                
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAOAELQ    DELETE THE EXISTING CAST OVERSCALE             
         GOTO1 REMELEM           AMOUNT ELEMENT                                 
                                                                                
         USING TAOAD,R4                                                         
         LA    R4,ELEMENT                                                       
         LA    RE,TAOAUSE                                                       
         ZIC   RF,TAOANUM                                                       
                                                                                
         CLI   0(R4),TAOAELQ     IF ELEMENT WASN'T ON RECORD                    
         JE    UCAST80           INITIALIZE IT NOW                              
         MVI   TAOAEL,TAOAELQ                                                   
         MVI   TAOALEN,TAOALNQ                                                  
         MVI   TAOANUM,0                                                        
         B     UCAST120                                                         
                                                                                
UCAST80  CLC   =C'HLD',0(RE)     IF ELEMENT WAS FOUND                           
         JNE   UCAST90           SAVE THE ADDRESS OF EACH HOLDING               
         ST    RE,ATAOAHLD       FEE OVERSCALE AMOUNT                           
UCAST90  CLC   =C'SHL',0(RE)                                                    
         JNE   UCAST100                                                         
         ST    RE,ATAOASHL                                                      
UCAST100 CLC   =C'ADH',0(RE)                                                    
         JNE   UCAST110                                                         
         ST    RE,ATAOAADH                                                      
UCAST110 LA    RE,L'TAOASBEL(RE)                                                
         BCT   RF,UCAST80                                                       
                                                                                
UCAST120 ZIC   R1,TAOANUM                                                       
         MHI   R1,L'TAOASBEL                                                    
         AHI   R1,TAOALNQ                                                       
         AR    R1,R4                                                            
                                                                                
         LA    RE,WORK+TAOAUSE-TAOAD   RE=A(INITIAL OVERSCALE AMOUNTS)          
         ZIC   RF,WORK+TAOANUM-TAOAD   RF=# OF OVERSCALE AMOUNTS                
                                                                                
*                                TAKE ONLY THE HOLDING FEE TYPES                
*                                OUT OF THE INITIAL OVERSCALE AMOUNTS           
                                                                                
UCAST130 CLC   =C'HLD',0(RE)     IF COPYING THE HLD AMOUNT                      
         JNE   UCAST140                                                         
         OC    ATAOAHLD,ATAOAHLD AND HLD IS ALREADY IN THE ELEMENT              
         JZ    UCAST160                                                         
         L     R2,ATAOAHLD       WRITE OVER IT                                  
         MVC   0(L'TAOASBEL,R2),0(RE)                                           
         J     UCAST170                                                         
                                                                                
UCAST140 CLC   =C'SHL',0(RE)     IF COPYING THE SHL AMOUNT                      
         JNE   UCAST150                                                         
         OC    ATAOASHL,ATAOASHL AND SHL IS ALREADY IN THE ELEMENT              
         JZ    UCAST160                                                         
         L     R2,ATAOASHL       WRITE OVER IT                                  
         MVC   0(L'TAOASBEL,R2),0(RE)                                           
         J     UCAST170                                                         
                                                                                
UCAST150 CLC   =C'ADH',0(RE)     IF COPYING THE ADH AMOUNT                      
         JNE   UCAST170                                                         
         OC    ATAOAADH,ATAOAADH AND ADH IS ALREADY IN THE ELEMENT              
         JZ    UCAST160                                                         
         L     R2,ATAOAADH       WRITE OVER IT                                  
         MVC   0(L'TAOASBEL,R2),0(RE)                                           
         J     UCAST170                                                         
                                                                                
UCAST160 ZIC   R0,TAOALEN        IF AMOUNT IS NOT ALREADY IN ELEMENT            
         AHI   R0,L'TAOASBEL                                                    
         STC   R0,TAOALEN        BUMP ELEMENT LENGTH                            
                                                                                
         ZIC   R0,TAOANUM                                                       
         AHI   R0,1                                                             
         STC   R0,TAOANUM        BUMP # OF OVERSCALE AMOUNTS                    
                                                                                
         MVC   0(L'TAOASBEL,R1),0(RE)                                           
                                                                                
         LA    R1,L'TAOASBEL(R1)                                                
                                                                                
UCAST170 LA    RE,L'TAOASBEL(RE)                                                
         BCT   RF,UCAST130                                                      
                                                                                
         GOTO1 ADDELEM           ADD NEW OVERSCALE AMOUNTS ELEMENT              
         GOTO1 PUTREC            AND PUT CAST RECORD                            
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
         OC    TLCAHNXT,TLCAHNXT IF HOLDING FEE FOR NEW PRIMARY                 
         JZ    XIT               HAS ALREADY BEEN GENERATED                     
         CLC   TLCAHDTE,TLCAHNXT AND HAS NOT BEEN PAID ...                      
         JH    XIT                                                              
         DROP  R3                                                               
                                                                                
         USING TLCOPD,R3                                                        
         XC    KEY,KEY           READ NEW COMMERCIAL RECORD                     
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,PRICOM                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLCOCCOM+L'TLCOCCOM-TLCOPD),KEYSAVE                          
         JE    *+6                                                              
         DC    H'00'                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ    GET COMMERCIAL DETAILS RECORD                  
         BRAS  RE,GETEL          AND TURN ON "COMM'L/CAST CHANGED               
         JE    *+6               SINCE LAST HF" BIT                             
         DC    H'00'                                                            
         OI    TACOSTA2,TACOCHHF                                                
         GOTOR SNDMQHFR,DMCB,(PRGSTAT,PRICOM),(TGSYSTA2,HEXOUT),MQIO            
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC            PUT COMMERCIAL RECORD                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR MESSAGES FOR VREC ROUTINE                              *         
***********************************************************************         
                                                                                
VRINVSEL L     R2,ASELFLD            INVALID INPUT IN SELECT FIELD              
         J     VRINV                                                            
                                                                                
VRINVDIS STC   R0,ERRDISP            INVALID INPUT WITH DISPLACEMENT            
         J     VRINV                                                            
                                                                                
VRINV    MVI   ERROR,INVALID         INVALID INPUT                              
         J     VREND                                                            
                                                                                
VRMISS   MVI   ERROR,MISSING         MISSING INPUT                              
         J     VREND                                                            
                                                                                
VRENDMIS MVI   SLOPDH+5,9                                                       
         MVC   SLOPD+9(8),SPACES                                                
         MVI   ERROR,ERMISEDT        MISSING END DATE                           
         MVI   ERRDISP,9                                                        
         J     VREND                                                            
                                                                                
VRACCESS LA    R2,CONACTH                                                       
         MVC   MYMSGNO,=Y(ERRGINAC)  CANNOT CHANGE GRT UNLESS USER              
         J     VRENDEXT              HAS ACCESS TO ALL AGY/CLIS                 
                                                                                
VRACATCH MVC   MYMSGNO,=Y(ERRACATC)  GRT. ATTACHED TO ADDITIONAL                
         J     VRENDEXT              CLIENTS UNDER THIS AGENCY                  
                                                                                
VRACINVP MVC   MYMSGNO,=Y(ERRACINV)  INVOICE PENDING FOR ADDITIONAL             
         J     VRENDEXT              CLIENTS UNDER THIS AGENCY                  
                                                                                
VRNDLPAC L     R2,ASELFLD            CANNOT DELETE PRIMARY COMMERCIAL'S         
         MVC   MYMSGNO,=Y(ERRNDPAC)  AGENCY/CLIENT                              
         J     VRENDEXT                                                         
                                                                                
VRGAL1AC L     R2,ASELFLD            GUARANTEE MUST HAVE AT LEAST 1             
         MVC   MYMSGNO,=Y(ERRGL1AC)  LIMITED AGENCY/CLIENT                      
         J     VRENDEXT                                                         
                                                                                
VRNDATCO L     R2,ASELFLD            CANNOT DELETE AGY/CLI IF COMM'L            
         MVC   MYMSGNO,=Y(ERRNDATC)  IS ATTACHED TO GUARANTEE                   
         J     VRENDEXT                                                         
                                                                                
VRNDINVP L     R2,ASELFLD            CANNOT DELETE AGY/CLI IF INVOICE           
         MVC   MYMSGNO,=Y(ERRNDINV)  PENDING FOR AGY/CLI                        
         J     VRENDEXT                                                         
                                                                                
VRINVAMT MVI   ERROR,ERINVAMT        INVALID AMOUNT                             
         J     VREND                                                            
                                                                                
VRINVPD  MVC   MYMSGNO,=Y(ERRGRTPD)  GRT PERIOD MUST BE INCLUSIVE               
         J     VRENDEXT              OF ALL APPLYING PAYMENTS                   
                                                                                
MSCERR   MVC   MYMSGNO,=Y(ERMUS500)  EXISTING MSC CHKS-PFXX TO PROCEED          
         J     VRENDEXT                                                         
                                                                                
PFSELTYP MVC   MYMSGNO,=H'270'                                                  
         J     VRINFEND                                                         
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     VRENDEXT                                                         
                                                                                
VRENDEXT MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         J     VREND                                                            
                                                                                
VRINFEND MVI   MYMTYP,GTMINF                                                    
         OI    GENSTAT2,USGETTXT                                                
         J     VREND                                                            
                                                                                
VREND    GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VREC ROUTINES                     *         
***********************************************************************         
                                                                                
SELTYTAB DS    0C                                                               
         DC    AL1(STT13X-*,13,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8'        ',CL8'        '                               
STT13X   EQU   *                                                                
         DC    AL1(STT14X-*,14,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8'        ',CL8'        '                               
STT14X   EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
VRHEXFFS DC    20X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DETERMINES IF GUARANTEE IS ELIGIBLE FOR DELETE       *         
***********************************************************************         
                                                                                
DELCHK   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2                                                         
                                                                                
         USING TLGTD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY             GUARANTEE CANNOT HAVE ANY TRACKING           
         MVI   TLGTCD,TLGTCDQ                                                   
         MVC   TLGTSSN,TGSSN                                                    
         MVC   TLGTGUA,TGGUA                                                    
         XC    TLGTGUA,DCHEXFFS                                                 
         GOTO1 HIGH                                                             
         CLC   TLGTKEY(TLGTSTRT-TLGTD),KEYSAVE                                  
         JE    VCNODEL                                                          
         DROP  R3                                                               
                                                                                
         USING TLCAPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCAPCD,TLCAGCDQ    AND MAY NOT BE ATTACHED TO ANY               
         MVC   TLCAGSSN,TGSSN      CAST RECORDS                                 
         MVC   TLCAGGUA,TGGUA      GUARANTEE CODE                               
         XC    TLCAGGUA,DCHEXFFS   (UNCOMPLEMENTED)                             
         GOTO1 HIGH                                                             
         CLC   TLCAPKEY(TLCAGCOM-TLCAPD),KEYSAVE                                
         JE    VCNODEL                                                          
                                                                                
         MVC   KEY,SVKEY           RESTORE SAVED KEY                            
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR MESSAGES FOR DELCHK ROUTINE                            *         
***********************************************************************         
                                                                                
VCNODEL  MVI   ERROR,ERINVDEL      DELETE NOT ALLOWED                           
         J     VCEND                                                            
                                                                                
VCEND    GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR DELCHK ROUTINE                    *         
***********************************************************************         
                                                                                
DCHEXFFS DC    20X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO LOAD GUARANTEE SPECIFIC SCREEN                    *         
***********************************************************************         
                                                                                
LOADIT   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,GRTTAGH                                                       
         MVC   OVERLAY,LOADSCRN                                                 
         GOTO1 LOADSOPH,DMCB,1                                                  
                                                                                
         MVC   GRTHEAD,LOVHEAD                                                  
         CLI   LOADSCRN,LOVRSCRN   DISPLAY HEADINGS                             
         JE    XIT                                                              
         MVC   GRTHEAD,PCYHEAD                                                  
         J     XIT                                                              
                                                                                
LOVHEAD  DC    CL(L'GRTHEAD)'Large Overscale Guarantee'                         
PCYHEAD  DC    CL(L'GRTHEAD)'Per Cycle Guarantee'                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CLEAR GUARANTEE'S AGENCY/CLIENT TABLE             *         
*        ON ENTRY ... R3=A(GUARANTEE'S AGENCY/CLIENT TABLE)           *         
***********************************************************************         
                                                                                
CLRTBL   NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
CTBL10   XC    0(250,RE),0(RE)                                                  
         LA    RE,250(RE)                                                       
         C     RE,ACTBLX                                                        
         JL    CTBL10                                                           
         MVI   0(R3),X'FF'                                                      
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO BUILD GUARANTEE'S AGENCY/CLIENT TABLE             *         
*        ON ENTRY ... R3=A(AGENCY/CLIENT TABLE AREA)                  *         
***********************************************************************         
                                                                                
         USING ACTBLD,R3                                                        
BLDACTBL NTR1  BASE=*,LABEL=*                                                   
         OC    PRIAGY(L'PRIAGY+L'PRICLI),PRIAGY                                 
         JZ    XIT                                                              
         GOTO1 CHKLIM,DMCB,PRIAGY,PRICLI                                        
                                                                                
         USING TAVAD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAVAELQ                                                   
         BRAS  RE,GETEL                                                         
         J     BTBL20                                                           
BTBL10   BRAS  RE,NEXTEL                                                        
BTBL20   JNE   BTBL90                                                           
                                                                                
         XR    RE,RE                                                            
         ZIC   RF,TAVALEN          CALCULATE NUMBER OF CLIENTS                  
         SHI   RF,TAVALNQ          IN ELEMENT                                   
         LTR   RF,RF                                                            
         JNZ   BTBL30                                                           
         LHI   RF,TAVALNQ                                                       
BTBL30   D     RE,=A(L'TAVACLI)                                                 
                                                                                
         LR    R2,RF               R2=(NUMBER OF CLIENTS IN ELEMENT)            
         LA    R5,TAVACLI          RE=A(CURRENT CLIENT IN ELEMENT)              
                                                                                
BTBL40   XC    TGCLI,TGCLI                                                      
         GOTO1 CHKLIM,DMCB,TAVAAGY,TGCLI                                        
         CLI   TAVALEN,TAVALNQ                                                  
         JE    BTBL70                                                           
                                                                                
         TM    PROSTAT,PRIADDED                                                 
         JO    BTBL60                                                           
         CLC   PRIAGY,TAVAAGY                                                   
         JH    BTBL60                                                           
         JL    BTBL50                                                           
         CLC   PRICLI,0(R5)                                                     
         JH    BTBL60                                                           
BTBL50   MVI   ACSTAT,0                                                         
         MVC   ACAGY-ACTBLD(L'ACAGY,R3),PRIAGY                                  
         MVC   ACCLI-ACTBLD(L'ACCLI,R3),PRICLI                                  
         LA    R3,ACLNQ(R3)                                                     
         OI    PROSTAT,PRIADDED                                                 
                                                                                
BTBL60   GOTO1 CHKLIM,DMCB,TAVAAGY,(R5)                                         
         MVC   ACCLI-ACTBLD(L'ACCLI,R3),0(R5)                                   
         J     BTBL80                                                           
                                                                                
BTBL70   TM    PROSTAT,PRIADDED                                                 
         JO    BTBL80                                                           
         CLC   PRIAGY,TAVAAGY                                                   
         JH    BTBL80                                                           
         MVI   ACSTAT,0                                                         
         MVC   ACAGY-ACTBLD(L'ACAGY,R3),PRIAGY                                  
         MVC   ACCLI-ACTBLD(L'ACCLI,R3),PRICLI                                  
         LA    R3,ACLNQ(R3)                                                     
         OI    PROSTAT,PRIADDED                                                 
                                                                                
BTBL80   MVC   0(ACCLI-ACTBLD,R3),TAVASTAT                                      
         LA    R3,ACLNQ(R3)                                                     
         MVI   0(R3),X'FF'         ADD CURRENT CLIENT TO TABLE                  
                                                                                
         LA    R5,L'TAVACLI(R5)                                                 
         BCT   R2,BTBL40           BUMP TO NEXT CLIENT IN ELEMENT               
         J     BTBL10                                                           
                                                                                
BTBL90   TM    PROSTAT,PRIADDED                                                 
         JO    XIT                                                              
         MVI   ACSTAT,0                                                         
         MVC   ACAGY-ACTBLD(L'ACAGY,R3),PRIAGY                                  
         MVC   ACCLI-ACTBLD(L'ACCLI,R3),PRICLI                                  
         MVI   ACLNQ(R3),X'FF'                                                  
         J     XIT                                                              
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
*        ROUTINE CHECKS IF STAFF HAS ACCESS TO AGENCY/CLIENT          *         
*        ON ENTRY ... P1=A(GUARANTEE AGENCY)                          *         
*                     P2=A(GUARANTEE CLIENT)                          *         
***********************************************************************         
                                                                                
CHKLIM   NTR1                                                                   
         TM    PROSTAT,ACCESSYS    IF STAFF ALREADY HAS ACCESS                  
         JO    XIT                 NO NEED TO CHECK AGAIN                       
                                                                                
         L     R2,0(R1)            R2=A(GUARANTEE AGENCY)                       
         L     R3,4(R1)            R3=A(GUARANTEE CLIENT)                       
                                                                                
         XC    TGAGY,TGAGY                                                      
                                                                                
         OC    0(L'TAVAAGY,R2),0(R2)                                            
         JZ    CLIM10                                                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',(R2))                                 
         JNE   XIT                                                              
                                                                                
CLIM10   OC    0(L'TAVACLI,R3),0(R3)                                            
         JZ    CLIM20                                                           
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'80',(R3))                                 
         JNE   XIT                                                              
                                                                                
CLIM20   OI    PROSTAT,ACCESSYS                                                 
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO BUILD ELIGIBLE PRIMARY COMMERCIALS TABLE          *         
*        ON ENTRY ... R5=A(ELIGIBLE PRIMARY COMM'LS TABLE)            *         
***********************************************************************         
                                                                                
         USING ECTBLD,R5                                                        
BLDETBL  NTR1  BASE=*,LABEL=*                                                   
         MVI   0(R5),X'FF'         INITIALIZE ELIGIBLE COMMERCIALS TBL          
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY           READ ALL CHECK RECORDS FOR THIS                
         MVI   TLCKPCD,TLCKHCDQ  PERFORMER FOR THIS COMMERCIAL                  
         MVC   TLCKHCOM,PRICOM                                                  
         MVC   TLCKHSSN,TGSSN                                                   
         GOTO1 HIGH                                                             
         J     BET20                                                            
BET10    GOTO1 SEQ                                                              
BET20    CLC   KEY(TLCKHCAT-TLCKPCD),KEYSAVE                                    
         JNE   BET40                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TACDD,R4                                                         
         L     R4,AIO2           R4=A(CHECK RECORD)                             
         MVI   ELCODE,TACDELQ    GET CHECK DETAILS ELEMENT                      
         BRAS  RE,GETEL                                                         
         JNE   BET10                                                            
         OC    TACDDTE,TACDDTE   ONLY CONSIDER CHECKS THAT HAVE                 
         JNZ   BET10             NOT PROCESSED YET                              
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO2           R4=A(CHECK RECORD)                             
         MVI   ELCODE,TACAELQ    GET CAST DETAIL ELEMENT                        
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         XC    TACAGUA,VRHEXFFS                                                 
         CLC   TACAGUA,TGGUA     ONLY CONSIDER CHECKS WITH GUARANTEE            
         JNE   BET10             CODE THAT MATCHES CURRENT PAYMENT              
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO2           R4=A(CHECK RECORD)                             
         MVI   ELCODE,TAPDELQ    GET PAYMENT DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         JE    *+6               ONLY CONSIDER PER CYCLE PAYMENTS               
         DC    H'00'                                                            
         GOTO1 USEVAL,DMCB,(X'40',TAPDUSE)                                      
         TM    TGUSSTA2,APPREUSE                                                
         JZ    BET10                                                            
         DROP  R4                                                               
                                                                                
         MVC   SVKEY,KEY         SAVE CHECK KEY                                 
                                                                                
         MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR' RESET TO READ TALENT DIRECTORY               
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO2           R4=A(CHECK RECORD)                             
                                                                                
         USING TLINPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLINPCD,TLINBCDQ  CHECK IF INVOICE HAS ALREADY BEEN              
         MVC   TLINBAGY,TLCKAGY  BILLED                                         
         MVC   TLINBINV,TLCKINV                                                 
         XC    TLINBINV,VRHEXFFS                                                
         GOTO1 HIGH                                                             
         CLC   TLINPKEY,KEYSAVE  IF NOT, CANNOT SWITCH THE PRIMARY              
         JNE   BET30                                                            
         TM    TLINBST2,TAINSBIL                                                
         JZ    XIT                                                              
         DROP  R3,R4                                                            
                                                                                
BET30    MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         MVC   KEY,SVKEY           RESTORE CHECK READ SEQUENCE                  
         GOTO1 HIGH                                                             
         J     BET10                                                            
                                                                                
BET40    MVC   SYSDIR,=CL8'TALDIR' RESTORE FILE SETTING                         
         MVC   SYSFIL,=CL8'TALFIL' AND INDICATE THAT NO PER CYCLE               
         OI    PROSTAT,PSPCBLCK    PAYMENTS ARE AWAITING BILLING/CHECKS         
                                                                                
         XC    FPCYSTRT,FPCYSTRT   INITIALIZE 1ST PER CYCLE START DATE          
         XC    LPCYSTRT,LPCYSTRT   AND LAST PER CYCLE START DATE                
                                                                                
         USING TAGCD,R4                                                         
         L     R4,AIO1             R4=A(GUARANTEE RECORD)                       
         MVI   ELCODE,TAGCELQ      SAVE LATEST PER CYCLE PAYMENT                
         BRAS  RE,GETEL            START DATE                                   
         J     *+8                                                              
BET50    BRAS  RE,NEXTEL                                                        
         JNE   BET60                                                            
         MVC   LPCYSTRT,TAGCSTRT                                                
         J     BET50                                                            
         DROP  R4                                                               
                                                                                
BET60    OC    LPCYSTRT,LPCYSTRT   IF NO PER CYCLE PAYMENT HAS BEEN             
         JNZ   BET80               MADE YET                                     
                                                                                
         USING TLCAPD,R3                                                        
         LA    R3,KEY              READ CAST RECORD FOR PRIMARY                 
         XC    KEY,KEY             COMMERCIAL                                   
         MVI   TLCAPCD,TLCAGCDQ                                                 
         MVC   TLCAGSSN,TGSSN                                                   
         MVC   TLCAGGUA,TGGUA                                                   
         XC    TLCAGGUA,BETHEXFF                                                
         MVC   TLCAGCOM,PRICOM                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLCAGCAT-TLCAPCD),KEYSAVE                                    
         JNE   BET70                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAFCYC,TACAFCYC   IF FIRST FIXED CYCLE AT CAST LEVEL           
         JZ    BET70               SET 1ST PER CYCLE START DATE                 
         MVC   FPCYSTRT,TACAFCYC                                                
         J     BET80                                                            
         DROP  R4                                                               
                                                                                
BET70    MVC   FPCYSTRT,=3X'FF'    ELSE, READ PRIMARY COMMERCIAL RECORD         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',PRICOM)                              
         JNE   BET80                                                            
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   FPCYSTRT,TACOFCYC   AND SET 1ST PER CYCLE START DATE             
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
BET80    LA    R3,KEY              READ ALL CAST KEYS ATTACHED                  
         XC    KEY,KEY             TO THIS GUARANTEE                            
         MVI   TLCAPCD,TLCAGCDQ                                                 
         MVC   TLCAGSSN,TGSSN                                                   
         MVC   TLCAGGUA,TGGUA                                                   
         XC    TLCAGGUA,BETHEXFF                                                
         GOTO1 HIGH                                                             
         J     BET100                                                           
BET90    GOTO1 SEQ                                                              
BET100   CLC   KEY(TLCAGCOM-TLCAPCD),KEYSAVE                                    
         JNE   XIT                                                              
                                                                                
         CLC   TLCAGCOM,PRICOM     SKIP CAST RECORD FOR PRIMARY COMM'L          
         JE    BET90                                                            
                                                                                
         GOTO1 GETREC              GET CAST RECORD                              
                                                                                
         XC    LFTRSTRT,LFTRSTRT   INITIALIZE LAST FTRACK START DATE            
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO2             R4=A(CAST RECORD)                            
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'               SKIP LAST SERVICED CAST                      
         OC    TACALAST,TACALAST                                                
         JNZ   BET110              INITIALIZE LAST FTRACK START DATE            
         MVC   LFTRSTRT,TACAFCYC   AS CAST'S FIRST FIXED CYCLE                  
         DROP  R4                                                               
                                                                                
         USING TACRD,R4                                                         
BET110   L     R4,AIO2             R4=A(CAST RECORD)                            
         MVI   ELCODE,TACRELQ      READ ALL APPLIED CREDIT ELEMENTS             
         BRAS  RE,GETEL                                                         
         J     BET130                                                           
BET120   BRAS  RE,NEXTEL                                                        
BET130   JNE   BET150                                                           
         GOTO1 USEVAL,DMCB,(X'40',TACRUSE)                                      
         TM    TGUSSTA2,HLDTYPE    REJECT IF NOT FOR HOLDING FEE                
         JO    BET140                                                           
         CLC   TACRUSE,=C'REN'     OR REINSTATEMENT                             
         JE    BET140                                                           
         CLC   TACRUSE,=C'SRE'     OR SPANISH REINSTATEMENT                     
         JNE   BET120                                                           
BET140   MVC   LFTRSTRT,TACRSTRT   SAVE LATEST FTRACK START DATE                
         J     BET120                                                           
         DROP  R4                                                               
                                                                                
BET150   MVC   SVKEY,KEY           SAVE CAST KEY                                
         MVC   TGCOM,TLCAGCOM      AND READ COMMERCIAL RECORD                   
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)                                   
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         USING TACOD,R4                                                         
         OC    LFTRSTRT,LFTRSTRT   IF LATEST FTRACK START DATE                  
         JNZ   BET160              STILL IS NOT SET                             
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   LFTRSTRT,TACOFCYC   SET IT AS COMMERCIAL'S FFC                   
         DROP  R4                                                               
                                                                                
BET160   BRAS  RE,ELIGPRI          IS COMM'L ELIGIBLE TO BE PRIMARY?            
         JNE   BET220                                                           
                                                                                
         CLC   LPCYSTRT,LFTRSTRT   IF LATEST PER CYCLE PAYMENT DOES             
         JE    BET170              NOT MATCH CAST'S LATEST FTRACK               
         OC    LPCYSTRT,LPCYSTRT   AND PER CYCLE PAYMENT HAS BEEN MADE          
         JNZ   BET210              COMM'L IS NOT ELIGIBLE TO BE PRIMARY         
                                                                                
         CLC   LFTRSTRT,FPCYSTRT   IF PER CYCLE PAYMENT HAS NOT BEEN            
         JH    BET210              MADE,FFC MUST BE THE SAME OR EARLIER         
                                                                                
         USING TLCOD,R4                                                         
BET170   L     R4,AIO2                                                          
         MVC   ECAGY,TLCOAGY       SAVE AGENCY CODE                             
         MVC   ECCLI,TLCOCLI       CLIENT CODE                                  
         MVC   ECCOM,TLCOCOM       AND INTERNAL COMMERCIAL NUMBER               
         DROP  R4                                                               
                                                                                
         USING ACTBLD,RE                                                        
         L     RE,ACTBL                                                         
BET180   CLI   0(RE),X'FF'         IF AGENCY IS ON GUARANTEE                    
         JE    BET200              WITHOUT A CLIENT LIMIT                       
         CLC   ACAGY,ECAGY         CLEAR THE PRIMARY COMMERCIAL'S               
         JNE   BET190              CLIENT                                       
         OC    ACCLI,ACCLI                                                      
         JZ    BET200                                                           
         XC    ECCLI,ECCLI                                                      
         J     BET200                                                           
BET190   LA    RE,ACTBLNQ(RE)                                                   
         J     BET180                                                           
         DROP  RE                                                               
                                                                                
         USING TACOD,R4                                                         
BET200   L     R4,AIO2                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL            SAVE COMMERCIAL ID                           
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   ECCID,TACOCID                                                    
         DROP  R4                                                               
                                                                                
         LA    R5,ECLNQ(R5)        MARK NEW END OF TABLE                        
         MVI   0(R5),X'FF'                                                      
         J     BET220                                                           
*                                  IF ACTIVE COMMERCIAL WITH DIFFERENT          
BET210   OI    PROSTAT,DIFFERCY    CYCLE WAS FOUND, SET STATUS                  
                                                                                
BET220   MVC   KEY,SVKEY           RESTORE CAST READ SEQUENCE                   
         GOTO1 HIGH                AND GO READ NEXT CAST                        
         J     BET90                                                            
         DROP  R5                                                               
                                                                                
BETHEXFF DC    6X'FF'                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ENSURE THAT COMMERCIAL IS ELIGIBLE TO BE A        *         
*        PRIMARY COMMERCIAL                                           *         
*        ON ENTRY ... AIO2=A(COMMERCIAL RECORD)                       *         
***********************************************************************         
                                                                                
ELIGPRI  NTR1  BASE=*,LABEL=*                                                   
         USING TACOD,R4                                                         
         L     R4,AIO2             R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JE    *+6                 MAY NOT BE LOCKED, RELEASED OR               
         DC    H'00'               SET UP FOR CANADIAN RATES                    
         TM    TACOSTAT,TACOSTLO+TACOSTRL+TACOSCRT                              
         JNZ   NO                                                               
         CLI   TACOMED,TACOMEDT    MEDIA MUST BE TELEVISION                     
         JE    YES                                                              
         CLI   TACOMED,TACOMEDI    INTERNET                                     
         JE    YES                                                              
         CLI   TACOMED,TACOMEDN    OR NEW MEDIA                                 
         JE    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS IF PAYMENTS HAVE APPLIED AGAINST THIS GRT     *         
***********************************************************************         
                                                                                
APPLPYMS NTR1  BASE=*,LABEL=*                                                   
         USING TLGTD,RE                                                         
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLGTCD,TLGTCDQ                                                   
         MVC   TLGTSSN,SVSSN                                                    
         MVC   TLGTGUA,TGGUA                                                    
         XC    TLGTGUA,VRHEXFFS                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(TLGTSTRT-TLGTD),KEYSAVE                                      
         JE    YES                                                              
         J     NO                                                               
         DROP  RE                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADD ELEMENTS TO AIO1                              *         
***********************************************************************         
                                                                                
MYADDEL  NTR1  BASE=*,LABEL=*                                                   
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO1                                                         
         GOTO1 ADDELEM                                                          
         MVC   AIO,SVAIO                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO REMOVE ELEMENTS FROM AIO1                         *         
***********************************************************************         
                                                                                
MYREMEL  NTR1  BASE=*,LABEL=*                                                   
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO1                                                         
         GOTO1 REMELEM                                                          
         MVC   AIO,SVAIO                                                        
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PROCESS PF KEYS                                   *         
***********************************************************************         
                                                                                
PROPFK   NTR1  BASE=*,LABEL=*                                                   
         CLI   PFAID,0             IF PF KEY IS HIT                             
         JE    XIT                                                              
                                                                                
         CLI   RECNUM,GO                                                        
         JNE   PPFK10                                                           
         GOTO1 INITIAL,DMCB,CMTTAB PERFORM PF KEY FUNCTION                      
                                                                                
PPFK10   LA    R2,LOVTAB                                                        
         CLI   LOADSCRN,LOVRSCRN                                                
         JE    PPFK20                                                           
         LA    R2,PCYTAB                                                        
PPFK20   MVI   MODE,SETFILE                                                     
         GOTO1 INITIAL,DMCB,(R2)                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR PROPFK ROUTINE                    *         
***********************************************************************         
                                                                                
CMTTAB   DC    AL1(CMT13X-*,13,0,0,0)                                           
         DC    CL3' ',CL8'GRT     ',CL8'DISPLAY '                               
CMT13X   DC    X'FF'                                                            
                                                                                
LOVTAB   DC    AL1(LOV13X-*,13,0,0,0)                                           
         DC    CL3' ',CL8'GRT     ',CL8'LIST    '                               
LOV13X   EQU   *                                                                
         DC    AL1(LOV14X-*,14,0,0,0)                                           
         DC    CL3' ',CL8'GTRACK  ',CL8'REPORT  '                               
LOV14X   EQU   *                                                                
         DC    AL1(LOV15X-*,15,0,0,0)                                           
         DC    CL3' ',CL8'GCAST   ',CL8'LIST    '                               
LOV15X   EQU   *                                                                
         DC    AL1(LOV16X-*,16,0,0,0)                                           
         DC    CL3' ',CL8'GRTCMT  ',CL8'DISPLAY '                               
LOV16X   EQU   *                                                                
         DC    AL1(LOV17X-*,17,0,(LOV17X-LOV17)/KEYLNQ,0)                       
         DC    CL3' ',CL8'GCON    ',CL8'DISP    '                               
LOV17    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYTWA,L'SLOGCNT-1),AL2(SLOGCNT-T702FFD)                   
LOV17X   EQU   *                                                                
         DC    AL1(LOV20X-*,20,0,0,0)                                           
         DC    CL3' ',CL8'        ',CL8'        '                               
LOV20X   DC    X'FF'                                                            
                                                                                
PCYTAB   DC    AL1(PCY13X-*,13,0,0,0)                                           
         DC    CL3' ',CL8'GRT     ',CL8'LIST    '                               
PCY13X   EQU   *                                                                
         DC    AL1(PCY14X-*,14,0,0,0)                                           
         DC    CL3' ',CL8'GTRACK  ',CL8'REPORT  '                               
PCY14X   EQU   *                                                                
         DC    AL1(PCY15X-*,15,0,0,0)                                           
         DC    CL3' ',CL8'GCAST   ',CL8'LIST    '                               
PCY15X   EQU   *                                                                
         DC    AL1(PCY16X-*,16,0,0,0)                                           
         DC    CL3' ',CL8'GRTCMT  ',CL8'DISPLAY '                               
PCY16X   EQU   *                                                                
         DC    AL1(PCY17X-*,17,0,0,0)                                           
         DC    CL3' ',CL8'PERCYCLE',CL8'TRANSFER'                               
PCY17X   DC    X'FF'                                                            
                                                                                
       ++INCLUDE TAMQHFR                                                        
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR57D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR54D                                                       
         ORG   GRTTAGH                                                          
       ++INCLUDE TASCR55D                                                       
         ORG   GRTTAGH                                                          
       ++INCLUDE TASCR56D                                                       
         EJECT                                                                  
**********************************************************************          
*        EQUATES                                                     *          
**********************************************************************          
                                                                                
GO       EQU   187                GUARANTEE COMMENT                             
                                                                                
**********************************************************************          
*        SAVED VARIABLES                                             *          
**********************************************************************          
                                                                                
         DS    D                                                                
                                                                                
LOADSCRN DS    X                   LOADED SCREEN                                
ADDGSCRN EQU   X'54'               ADD GUARANTEE SCREEN                         
LOVRSCRN EQU   X'55'               LARGE OVERSCALE SCREEN                       
PCYCSCRN EQU   X'56'               PER CYCLE SCREEN                             
                                                                                
ACTBL    DS    A                   A(INITIAL GRT AGY/CLI TABLE)                 
ACTBLX   DS    A                   A(INITIAL GRT AGY/CLI TABLE END)             
                                                                                
ADDAC    DS    XL(ACLNQ+1)         ADDED AGENCY/CLIENT LIMITATION               
                                                                                
FSTDSPAC DS    XL(ACLNQ)           1ST  DISPLAYED AGY/CLI TABLE ENTRY           
LSTDSPAC DS    XL(ACLNQ)           LAST DISPLAYED AGY/CLI TABLE ENTRY           
                                                                                
ECTBL    DS    A                   A(ELIGIBLE PRIMARY COMM'L TABLE)             
ECTBLX   DS    A                   A(ELIGIBLE PRIMARY COMM'L TABLE END)         
                                                                                
FSTDSPEC DS    XL(ACLNQ)           1ST  DISPLAYED ELIGIBLE TABLE ENTRY          
LSTDSPEC DS    XL(ACLNQ)           LAST DISPLAYED ELIGIBLE TABLE ENTRY          
                                                                                
ASELFLD  DS    F                   A(SELECT FIELD)                              
                                                                                
SVSSN    DS    CL(L'TGSSN)         SAVED SOCIAL SECURITY NUMBER                 
SVPID    DS    CL(L'TGPID)         SAVED PID                                    
SVFID    DS    CL(L'TGSSN)         SAVED CORPORATION FEDERAL ID                 
SVGUPD   DS    XL(L'TAGUPD)        SAVED GUARANTEE PERIOD                       
SVKEY    DS    CL(L'KEY)           SAVED KEY                                    
SVAIO    DS    A                   SAVED AIO                                    
SVPIDH   DS    XL(L'GRTPIDH+L'GRTPID)                                           
                                                                                
ORIGPRI  DS    XL(L'TAGUCOM)       ORIGINAL PRIMARY COMMERCIAL NUMBER           
                                                                                
PRIAGY   DS    XL(L'TAGUAGY)       PRIMARY AGENCY                               
PRICLI   DS    XL(L'TAGUCLI)       PRIMARY CLIENT                               
PRICOM   DS    XL(L'TAGUCOM)       PRIMARY INTERNAL COMMERCIAL NUMBER           
                                                                                
ATAOAHLD DS    A                   A(NEW PRIMARY HLD OVERSCALE AMOUNT)          
ATAOASHL DS    A                   A(NEW PRIMARY SHL OVERSCALE AMOUNT)          
ATAOAADH DS    A                   A(NEW PRIMARY ADH OVERSCALE AMOUNT)          
                                                                                
FPCYSTRT DS    XL(L'TACOFCYC)      FIRST PER CYCLE START DATE                   
LPCYSTRT DS    XL(L'TAGCSTRT)      LAST PER CYCLE PAYMENT START DATE            
LFTRSTRT DS    XL(L'TACRSTRT)      LAST FTRACK START DATE                       
                                                                                
MYELEM   DS    XL(L'ELEMENT)                                                    
                                                                                
MYPFKEY  DS    X                                                                
MSCFLAG  DS    X                   MSC STATUS FLAG                              
MSCFLCHG EQU   1                   MSC CHANGED                                  
                                                                                
PROSTAT  DS    X                   PROGRAM STATUS                               
ACCESSYS EQU   X'80'               ACCESS TO (AT LEAST 1) AGY/CLI               
PSPCBLCK EQU   X'40'               PER CYCLE PAYMENTS BILLED/CHECKS CUT         
PRIADDED EQU   X'20'               PRIMARY AGY/CLI ADDED TO TABLE               
DIFFERCY EQU   X'10'               DIFFERING CYCLE COMM'L FOUND                 
                                                                                
         DS    0D                                                               
PTRBLK   DS    CL((13*L'TLDRREC)+1)                                             
         DS    0D                                                               
UPDPTRS  DS    CL((13*L'TLDRREC)+1)                                             
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
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**********************************************************************          
*        DSECT FOR GUARANTEE'S AGENCY/CLIENT TABLE                   *          
**********************************************************************          
                                                                                
ACTBLD   DSECT                                                                  
ACSTAT   DS    X                                                                
ACAGY    DS    CL6                                                              
ACCLI    DS    CL6                                                              
ACLNQ    EQU   *-ACTBLD                                                         
ACTBLNQ  EQU   (ACLNQ*100)+1                                                    
                                                                                
**********************************************************************          
*        DSECT FOR ELIGIBLE PRIMARY COMMERCIAL TABLE                 *          
**********************************************************************          
                                                                                
ECTBLD   DSECT                                                                  
ECCOM    DS    XL(L'TLCOCOM)                                                    
ECAGY    DS    XL(L'TLCOAGY)                                                    
ECCLI    DS    XL(L'TLCOCLI)                                                    
ECCID    DS    XL(L'TACOCID)                                                    
ECLNQ    EQU   *-ECTBLD                                                         
ECTBLNQ  EQU   (ECLNQ*100)+1                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TAGENF1   10/10/14'                                      
         END                                                                    
