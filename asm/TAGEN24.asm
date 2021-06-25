*          DATA SET TAGEN24    AT LEVEL 051 AS OF 10/17/13                      
*PHASE T70224A,*                                                                
         TITLE 'T70224 - PAYEE MAINTENANCE'                                     
T70224   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70224                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTAB                                               
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    *+14                                                             
         MVC   SPESHED(13),=C'Performer Pid'                                    
         OI    SPESHEDH+6,X'80'                                                 
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   PAYE10                                                           
         LA    R2,SPESSNH                                                       
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    PAYE05                                                           
         CLI   SPESSNH+5,0                                                      
         BE    VK1A                                                             
         CLI   SPESSNH+5,9         SSN ENTERED?                                 
         BE    PAYE05                                                           
         CLI   SPESSNH+5,6         PID ENTERED?                                 
         BNE   FLDINV                                                           
         MVC   TGPID,SPESSN                                                     
VK1A     OC    TGPID,TGPID                                                      
         BZ    FLDMISS                                                          
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   PAYE05                                                           
         MVC   SPESSN,TGSSN                                                     
         MVI   SPESSNH+5,9                                                      
*                                                                               
PAYE05   GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SPESSNH),SPESSNNH PERFORMER           
         MVC   SVTGSSN,TGSSN                                                    
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    XIT                                                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SPESSN,SPACES                                                    
         MVC   SPESSN(L'TGPID),TGPID                                            
         MVI   SPESSNH+5,L'TGPID                                                
         OI    SPESSNH+6,X'80'                                                  
         B     XIT                                                              
         SPACE 3                                                                
PAYE10   CLI   MODE,DISPREC                                                     
         BE    PAYE15                                                           
         CLI   MODE,XRECADD                                                     
         BE    PAYE12              IF MODE IS NEW RECORD ADDED                  
         CLI   MODE,XRECDEL                                                     
         BE    PAYE12              OR RECORD DELETED                            
         CLI   MODE,XRECREST                                                    
         BE    PAYE12              OR RESTORED                                  
         CLI   MODE,XRECPUT        OR CHANGED                                   
         BNE   PAYE20                                                           
         SPACE                                                                  
PAYE12   GOTO1 ADDPTRS,DMCB,PTRS   HANDLE PASSIVE POINTERS                      
         SPACE                                                                  
PAYE15   BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
PAYE20   CLI   MODE,VALREC         IF MODE IS VALIDATE RECORD                   
         BE    PAYE25                                                           
         CLI   MODE,RECDEL         OR DELETE RECORD                             
         BE    PAYE25                                                           
         CLI   MODE,RECREST        OR RESTORE RECORD                            
         BNE   XIT                                                              
         SPACE                                                                  
PAYE25   GOTO1 SAVPTRS,DMCB,PTRS   SAVE PASSIVE POINTERS                        
         SPACE                                                                  
         CLI   MODE,VALREC                                                      
         BNE   XIT                 XIT IF MODE IS DELETE OR RESTORE             
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SPEID1H                                                          
         LA    R2,SPEID1NH         R2=A(CORP ID NAME FIELD HEADER)              
         LA    R0,6                MAX OF 6 CORPORATIONS                        
DISP2    XC    8(L'SPEID1N,R2),8(R2) CLEAR NAME                                 
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTLINE         GET NEXT NAME FIELD                          
         BCT   R0,DISP2                                                         
         SPACE                                                                  
         MVI   ELCODE,TATIELQ      GET TAX ID ELEMENT                           
         L     R4,AIO                                                           
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         BAS   RE,GETEL                                                         
         B     *+12                                                             
         SPACE                                                                  
         USING TATID,R4                                                         
DISP5    MVI   ELCODE,TATIELQ      GET TAX ID ELEMENT                           
         BAS   RE,NEXTEL           IF THERE'S AN ELEMENT                        
         BNE   DISP10                                                           
         CLI   TATITYPE,TATITYCO   AND TYPE IS CORPORATION                      
         BNE   DISP5                                                            
         BAS   RE,FINDFLD          FIND FIELD WITH MATCHING CORP NUM            
         BNE   DISP5                                                            
         MVC   8(9,R2),TATIID      MOVE CORPORATION ID NUMBER TO SCREEN         
         MVI   5(R2),9             SET LENGTH FOR RECVAL                        
         ZIC   R1,0(R2)                                                         
         LR    R3,R2                                                            
         AR    R3,R1               R3=A(NAME FIELD HEADER)                      
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'0C',(R2)),(R3) GET NAME                   
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    DISP5                                                            
         GOTO1 SSNPACK,DMCB,8(R2),SVPID                                         
         MVC   8(9,R2),SPACES                                                   
         MVC   8(L'SVPID,R2),SVPID                                              
         MVI   5(R2),L'SVPID                                                    
         B     DISP5                                                            
         SPACE                                                                  
         USING TAPED,R4                                                         
DISP10   MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   TGSSN,SVTGSSN       RESTORE GLOBAL SSN                           
         MVI   ELCODE,TAPEELQ      GET PAYEE ELEMENT                            
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DISP20                                                           
         MVC   SPENAME,TAPENAME    PAYEE NAME                                   
         MVC   SPEADD1,TAPEADD1    PAYEE ADDRESS                                
         MVC   SPEADD2,TAPEADD2                                                 
         MVC   SPEADD3,TAPEADD3                                                 
         MVC   SPEADD4,TAPEADD4                                                 
         GOTO1 DATCON,DMCB,(1,TAPEACT),(8,SPEEST) START DATE                    
         GOTO1 (RF),(R1),(1,TAPEEXP),(8,SPEEXP)   EXPIRATION DATE               
         SPACE 1                                                                
         CLI   TAPELEN,TAPELNQ     IF NEW STYLE PAYEE ELEMENT                   
         BL    DISP20                                                           
         MVC   SPEADDC,TAPECITY    PAYEE CITY                                   
         MVC   SPEADDS,TAPEST      PAYEE STATE                                  
         MVC   SPEADDZ,TAPEZIP     PAYEE ZIP                                    
         MVC   SPECTRY,TAPECTRY    PAYEE COUNTRY                                
         SPACE 1                                                                
DISP20   GOTO1 ACTVOUT,DMCB,(X'80',SPELCHGH)  LAST CHANGED                      
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
         USING TAW4D,R5                                                         
BLDREC   NTR1                                                                   
         MVC   SVDSKADD,DMDSKADD   SAVE DISK ADDRESS TO RE-GET REC              
*                                                                               
         L     R4,AIO                                                           
         USING TAWXD,R4                                                         
         MVI   ELCODE,TAWXELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   BLDR05                                                           
*        OC    TAWXTSSN,TAWXTSSN   IF TRUSTEE ASSIGNED TO W4                    
*        BNZ   ERTRUST             CANNOT ALSO HAVE PAYEE                       
         DROP  R4                                                               
*                                                                               
BLDR05   MVI   INPUTCRP,C'N'       INIT TO NOT ALLOW CORP INPUT                 
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ      GET EMPLOYEE W4 DETAILS ELEMENT              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R4                                                            
         CLI   TAW4TYPE,TAW4TYCO   IF PERF S/S NOT FOR A CORP                   
         BE    *+16                                                             
         CLI   TAW4TYPE,TAW4TYTR   OR TRUSTEE                                   
         BE    *+8                                                              
         MVI   INPUTCRP,C'Y'       SET TO ALLOW CORP INPUT                      
         SPACE                                                                  
         MVI   ELCODE,TATIELQ      TAX ID ELEMENT                               
         GOTO1 DELL,DMCB,=C'C'     DELETE ALL CURRENT CORPORATION TYPES         
         SPACE                                                                  
         USING TATID,R4                                                         
         LA    R3,SPECNUMH         R3=A(CORP NUM HEADER)                        
         LA    R2,SPEID1H          R2=A(CORP ID FIELD HEADER)                   
         LA    R0,6                MAX OF 6 CORPORATIONS                        
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT FOR EACH                   
         LA    R4,ELEMENT          R4=A(NEW ELEMENT TO BE ADDED)                
         MVI   TATIEL,TATIELQ                                                   
         MVI   TATILEN,TATILNQ                                                  
         MVI   TATITYPE,TATITYCO   TYPE IS CORPORATION                          
         MVC   TATIID,SPACES                                                    
         SPACE                                                                  
BLDR10   CLI   5(R2),0             IF THERE'S INPUT                             
         BE    BLDR15                                                           
         CLI   INPUTCRP,C'Y'       AND IT'S ALLOWED                             
         BNE   NOINPER                                                          
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    BLDR12                                                           
         CLI   5(R2),6                                                          
         BNE   BLDR12                                                           
         GOTO1 SSNUNPK,DMCB,8(R2),SVSSN                                         
         MVC   8(9,R2),SVSSN                                                    
         MVI   5(R2),L'SVSSN                                                    
*                                                                               
BLDR12   CLI   5(R2),9                                                          
         BNE   FLDINV              INVALID IF LENGTH NOT 9                      
         BAS   RE,CHKDUP           CHECK NOT DUPLICATE CORP ID NUMBER           
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'20',(R2))                                 
         L     R4,AIO                                                           
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVI   ELCODE,TAW4ELQ      GET EMPLOYEE W4 DETAILS ELEMENT              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R4                                                            
         LA    R4,ELEMENT          RESET R4                                     
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    BLDR14                                                           
         CLI   TAW4TYPE,TAW4TYTR                                                
         BE    BLDR14                                                           
         B     FLDINV              INVALID IF NOT A CORP OR TRUSTEE             
BLDR14   MVC   TATIID(9),8(R2)     CORPORATION ID NUMBER                        
         MVC   TATICRPN,8(R3)      CORPORATION NUMBER                           
         GOTO1 ADDELEM             ADD NEW ELEMENT                              
*                                                                               
         USING TLW4PD,R4                                                        
         LA    R4,KEY                                                           
         XC    KEY,KEY             IF CORPORATION HAS AN L IN                   
         MVI   TLW4PCD,TLW4LCDQ    FILTER FIELD, ENSURE THAT THIS               
         MVC   TLW4LFID,8(R2)      IS ITS ONLY ASSOCIATION                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLW4PKEY),KEYSAVE                                          
         JNE   BLDR14C                                                          
         XC    KEY,KEY                                                          
         MVI   TLW4PCD,TLW4CCDQ                                                 
         MVC   TLW4CCRP,8(R2)                                                   
         GOTO1 HIGH                                                             
         J     BLDR14B                                                          
BLDR14A  GOTO1 SEQ                                                              
BLDR14B  CLC   KEY(TLW4CSSN-TLW4PCD),KEYSAVE                                    
         JNE   BLDR14C                                                          
         CLC   TLW4CSSN,SVTGSSN                                                 
         JE    BLDR14A                                                          
         J     ERLLCAA                                                          
         DROP  R4                                                               
*                                                                               
BLDR14C  GOTO1 SSNPACK,DMCB,8(R2),SVPID                                         
         MVC   8(9,R2),SPACES                                                   
         MVC   8(6,R2),SVPID                                                    
         MVI   5(R2),L'SVPID                                                    
*                                                                               
BLDR15   LR    R2,R3               SET R2 FOR NEXTLINE ROUTINE                  
         BAS   RE,NEXTLINE                                                      
         LR    R3,R2               SET R3=A(NEXT CORP NUM FIELD HEADER)         
         ZIC   R1,0(R3)                                                         
         AR    R2,R1               SET R2=A(NEXT CORP ID FIELD HEADER)          
         BCT   R0,BLDR10           LOOP FOR NUMBER OF FIELDS                    
         SPACE                                                                  
         USING TLDRD,R4                                                         
         CLI   ACTNUM,ACTADD       IF ACTION NOT ADD                            
         BE    BLDR20                                                           
         L     R5,AIO                                                           
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLDRREC),0(R5) RESTORE KEY                                 
         MVC   TLDRDA,SVDSKADD     AND DISK ADDRESS                             
         MVC   AIO,AIO2            DON'T OVERRIDE CHANGES SO FAR                
         GOTO1 GETREC              RE-GET RECORD SO CAN WRITE CHANGES           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            RESTORE AIO                                  
         SPACE                                                                  
         USING TAPED,R4                                                         
BLDR20   MVC   TGSSN,SVTGSSN       RESTORE GLOBAL SSN                           
         LA    R4,ELEMENT          SET R4=A(ELEMENT TO BE ADDED)                
         MVI   ELCODE,TAPEELQ      PAYEE ELEMENT                                
         GOTO1 REMELEM             DELETE CURRENT                               
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         CLI   SPENAMEH+5,0                                                     
         BE    *+16                                                             
         MVC   TAPENAME,SPENAME    PAYEE NAME                                   
         OC    TAPENAME,SPACES                                                  
         SPACE                                                                  
         BRAS  RE,VALADD           VALIDATE ADDRESS                             
         SPACE                                                                  
         LA    R2,SPEESTH                                                       
         CLI   5(R2),0             IF THERE'S EFFECTIVE START DATE              
         BE    BLDR30                                                           
         GOTO1 DATVAL,DMCB,SPEEST,DUB EXPIRATION DATE                           
         OC    DMCB(4),DMCB                                                     
         BZ    DATEINV                                                          
         GOTO1 DATCON,DMCB,DUB,(1,TAPEACT)                                      
                                                                                
BLDR30   LA    R2,SPEEXPH                                                       
         CLI   5(R2),0             IF THERE'S EXPIRATION DATE INPUT             
         BE    BLDR40                                                           
         GOTO1 DATVAL,DMCB,SPEEXP,DUB EXPIRATION DATE                           
         OC    DMCB(4),DMCB                                                     
         BZ    DATEINV                                                          
         GOTO1 DATCON,DMCB,DUB,(1,TAPEEXP)                                      
         CLC   TAPEACT,TAPEEXP                                                  
         JH    FLDINV                                                           
                                                                                
BLDR40   OC    ELEMENT,ELEMENT     IF THERE'S SOMETHING IN ELEMENT              
         BZ    BLDR50                                                           
         MVI   TAPEEL,TAPEELQ                                                   
         MVI   TAPELEN,TAPELNQ                                                  
         GOTO1 ADDELEM             ADD NEW ELEMENT                              
         SPACE 1                                                                
BLDR50   GOTO1 ACTVIN,DMCB,(X'80',0)  LAST CHANGED                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FINDS THE FIELD WITH THE SAME CORP NUM                   
*              AS THE TAX ID ELEMENT AT R4                                      
*              RETURNS R2=A(CORP ID FIELD HEADER) AND CC EQUAL                  
*              IF NOT FOUND, RETURNS CC NOT EQUAL                               
         SPACE                                                                  
         USING TATID,R4                                                         
FINDFLD  NTR1                                                                   
         LA    R0,6                MAX OF 6 CORPORATIONS                        
         LA    R2,SPECNUMH         R2=A(CORP NUM FIELD HEADER)                  
FINDF5   CLC   8(1,R2),TATICRPN    IF NOT MATCHING CORP NUM                     
         BE    FINDFX                                                           
         BAS   RE,NEXTLINE         GET NEXT CORP NUM FIELD                      
         BCT   R0,FINDF5           LOOP FOR NUMBER OF CORPS                     
         B     NO                  NOT FOUND - RETURN CC NOT EQUAL              
         SPACE                                                                  
FINDFX   ZIC   R1,0(R2)            SET R2=A(CORP ID NUM FIELD HEADER)           
         AR    R2,R1                                                            
         XR    RC,RC                                                            
         LTR   RC,RC               SET CC EQUAL                                 
         XIT1  REGS=(R2)           RETURN R2                                    
         SPACE 3                                                                
*              ROUTINE GETS THE SAME FIELD ON THE NEXT LINE,                    
*              WHICH IS ASSUMED TO BE 3 FIELDS AWAY                             
*              R2=A(FIELD HEADER)                                               
         SPACE                                                                  
NEXTLINE NTR1                                                                   
         LA    R0,3                INCREMENT BY 3 FIELDS                        
         XR    R1,R1                                                            
NEXTL5   IC    R1,0(R2)                                                         
         AR    R2,R1               BUMP TO NEXT FIELD                           
         BCT   R0,NEXTL5           LOOP FOR NUMBER OF FIELDS                    
         XIT1  REGS=(R2)           RETURN R2                                    
         SPACE 3                                                                
*              ROUTINE COMPARES CORP ID NUMBER AT 8(R2) AGAINST TAX ID          
*              ELEMENTS ALREADY ADDED IN AIO TO ENSURE NO DUPLICATES            
*              R2=A(FIELD HEADER)                                               
         SPACE                                                                  
         USING TATID,R4                                                         
CHKDUP   NTR1                                                                   
         MVI   ELCODE,TATIELQ                                                   
         GOTO1 GETL,DMCB,=C'C'                                                  
         L     R4,TGELEM                                                        
         B     CHKD10                                                           
CHKD5    BAS   RE,NEXTEL                                                        
         BNE   XIT                 NO MORE ELEMENTS                             
         CLI   TATITYPE,TATITYCO                                                
CHKD10   BNE   XIT                 NO MORE CORPORATION TYPES                    
         CLC   TATIID(9),8(R2)                                                  
         BE    FLDINV              ERROR IF FIND MATCH                          
         B     CHKD5                                                            
         EJECT                                                                  
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE                                                                  
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         J     THEEND                                                           
         SPACE                                                                  
ERLLCAA  MVC   MYMSGNO,=Y(ERRLLCAA) LLC ALREADY HAS AN ASSOCIATED W4            
         J     ERRXIT                                                           
         SPACE                                                                  
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         J     THEEND                                                           
         SPACE                                                                  
DATEINV  MVI   ERROR,INVDATE       INVALID DATE                                 
         J     THEEND                                                           
         SPACE                                                                  
NOINPA3  LA    R2,SPEADD3H                                                      
         J     NOINPER                                                          
NOINPA4  LA    R2,SPEADD4H                                                      
NOINPER  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         J     THEEND                                                           
         SPACE                                                                  
ZIPERR   MVI   ERROR,ERBADZIP      ZIP CODE NOT VALID FOR STATE                 
         J     THEEND                                                           
         SPACE                                                                  
ERTRUST  MVC   MYMSGNO,=Y(ERRW4TAP)                                             
ERRXIT   MVI   BLOCK,0              CANNOT HAVE BOTH TRUSTEE AND                
         MVI   MYMTYP,GTMERR        PAYEE ON W4                                 
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
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
         SPACE 2                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CCOMM',CL8'LIST'                                      
PF14     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF15X-*,15,0,0,0)                                            
         DC    CL3' ',CL8'YTD',CL8'DISPLAY'                                     
PF15X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3' ',CL8'LIEN',CL8'LIST'                                       
PF16     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF16X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF17X-*,17,0,(PF17X-PF17)/KEYLNQ,0)                          
         DC    CL3' ',CL8'DUECOMP',CL8'LIST'                                    
PF17     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF17X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF18X-*,18,0,0,0)                                            
         DC    CL3' ',CL8'GRT',CL8'LIST'                                        
PF18X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES ADDRESS, CITY, STATE, ZIP CODE, COUNTRY    *         
*        ON ENTRY ... R4 = A(PAYEE DETAILS ELEMENT)                   *         
***********************************************************************         
                                                                                
         USING TAPED,R4                                                         
VALADD   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SPEADD1H                                                      
         GOTO1 FLDVAL,DMCB,(X'80',(R2)),(X'80',SPEADD4H)                        
         JNE   VADD10                                                           
         GOTO1 FLDVAL,DMCB,(X'80',SPEADDCH),(X'80',SPECTRYH)                    
         JNE   FLDMISS                                                          
         J     XIT                                                              
                                                                                
VADD10   CLI   5(R2),0             IF ANY FIELDS INPUT                          
         JE    FLDMISS             ADDRESS LINE 1 IS REQUIRED                   
                                                                                
         LA    R2,SPECTRYH         VALIDATE COUNTRY                             
         GOTO1 VALCTRY,DMCB,(R2)                                                
         JNE   THEEND                                                           
         MVC   TAPECTRY,TGCTRY                                                  
                                                                                
         CLC   TGCTRY,=C'US'       IF COUNTRY IS NOT US                         
         JE    VADD20                                                           
         CLI   SPEADD3H+5,0        DISALLOW INPUT IN ADDRESS LINE 3             
         JNE   NOINPA3                                                          
                                                                                
VADD20   CLI   SPEADD4H+5,0        INPUT NO LONGER ALLOWED                      
         JNE   NOINPA4             IN ADDRESS LINE 4                            
                                                                                
         LA    R3,TAPEADD1         PAYEE ADDRESS                                
         LA    R0,3                R0=MAX N'LINES                               
         LA    R2,SPEADD1H         R2=A(1ST LINE HEADER)                        
                                                                                
VADD30   CLI   5(R2),0             IF THERE'S INPUT IN THIS FIELD               
         JE    VADD40                                                           
         MVC   0(L'TAPEADD1,R3),SPACES PRE-CLEAR FIELD IN ELEMENT               
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE DATA TO ELEMENT                         
                                                                                
VADD40   IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         TM    1(R2),X'20'         BUMP TO NEXT UNPROTECTED FIELD               
         JO    VADD40                                                           
         LA    R3,L'TAPEADD1(R3)   BUMP TO NEXT FIELD IN ELEMENT                
         BCT   R0,VADD30                                                        
                                                                                
         LA    R2,SPEADDCH         CITY                                         
         CLI   5(R2),0                                                          
         JE    FLDMISS                                                          
         MVC   TAPECITY,SPEADDC                                                 
         OC    TAPECITY,SPACES                                                  
                                                                                
         LA    R2,SPEADDSH         STATE/PROVINCE                               
         MVC   WORK(2),SPEADDS                                                  
         OC    WORK(2),SPACES                                                   
         MVI   WORK+2,X'40'                                                     
         GOTO1 TAXVAL,DMCB,(X'FF',WORK)                                         
         JNE   THEEND                                                           
                                                                                
         CLC   TGCTRY,=C'US'       IF COUNTRY IS US                             
         JNE   VADD50                                                           
         CLC   TGTAZIPF,SPACES                                                  
         JE    FLDINV              INVALID STATE IF DON'T HAVE 1ST              
         CLC   TGTAZIPT,SPACES     2 ZIP DIGITS FROM AND TO                     
         JE    FLDINV                                                           
VADD50   MVC   TAPEST,SPEADDS                                                   
                                                                                
         LA    R2,SPEADDZH         ZIP CODE IS REQUIRED                         
         CLI   5(R2),0                                                          
         JE    FLDMISS                                                          
         BAS   RE,USZIPVAL         VALIDATE IF COUNTRY IS US                    
         BAS   RE,CAZIPVAL         VALIDATE IF COUNTRY IS CA                    
         MVC   TAPEZIP,SPEADDZ                                                  
         OC    TAPEZIP,SPACES                                                   
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES ZIP CODE FOR US W4S                        *         
*        ON ENTRY ... R2 = A(ZIP CODE FIELD)                          *         
***********************************************************************         
                                                                                
USZIPVAL NTR1                                                                   
         CLC   TGCTRY,=C'US'       IF COUNTRY IS US                             
         JNE   XIT                                                              
                                                                                
         MVC   DUB(9),=9C'0'       INIT DUB TO 000000000                        
                                                                                
         CLI   5(R2),5             INPUT LENGTH MUST BE 5 OR 10                 
         JE    USZV10                                                           
         CLI   5(R2),10                                                         
         JNE   FLDINV                                                           
         CLI   13(R2),C'-'         IF LNGTH IS 10, 6TH CHAR MUST BE "-"         
         JNE   FLDINV                                                           
         MVZ   DUB+5(4),14(R2)                                                  
                                                                                
USZV10   MVZ   DUB(5),8(R2)                                                     
         CLC   DUB(9),=9C'0'       CHECK VALID NUMERIC                          
         JNE   FLDINV                                                           
                                                                                
         CLC   8(3,R2),TGTAZIPF    CHECK FIRST 3 DIGITS FIT INTO RANGE          
         JL    ZIPERR                                                           
         CLC   8(3,R2),TGTAZIPT                                                 
         JNH   USZV20                                                           
                                                                                
         CLC   TGTACODE,=C'GA '    IF GEORGIA,                                  
         JNE   ZIPERR                                                           
         CLC   8(3,R2),=C'398'     398 - 399 ALSO ALLOWED                       
         JL    ZIPERR                                                           
         CLC   8(3,R2),=C'399'                                                  
         JH    ZIPERR                                                           
                                                                                
USZV20   CLC   TGTACODE,=C'PR '    IF PUERTO RICO                               
         JNE   USZV30                                                           
         CLC   8(3,R2),=C'008'     CAN'T START WITH 008                         
         JE    ZIPERR                                                           
         J     XIT                                                              
                                                                                
USZV30   CLC   TGTACODE,=C'VI '    IF VIRGIN ISLANDS                            
         JNE   XIT                                                              
         CLC   8(5,R2),=C'00851'   ONLY 00801-00851 IS VALID                    
         JH    ZIPERR                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES ZIP CODE FOR CANADIAN W4S                  *         
*        ON ENTRY ... R2 = A(ZIP CODE FIELD)                          *         
***********************************************************************         
                                                                                
CAZIPVAL NTR1                                                                   
         CLC   TGCTRY,=C'CA'       IF COUNTRY IS CANADA                         
         JNE   XIT                                                              
         CLI   5(R2),7             INPUT LENGTH MUST BE 7                       
         JNE   FLDINV                                                           
                                                                                
         GOTO1 VALCANUM,DMCB,SPEADDZ+1                                          
         GOTO1 VALCAALP,DMCB,SPEADDZ+2                                          
         CLI   SPEADDZ+3,C' '                                                   
         JNE   FLDINV                                                           
         GOTO1 VALCANUM,DMCB,SPEADDZ+4                                          
         GOTO1 VALCAALP,DMCB,SPEADDZ+5                                          
         GOTO1 VALCANUM,DMCB,SPEADDZ+6                                          
                                                                                
         USING CTRYSUBD,R1                                                      
         L     R1,TGACTRY                                                       
         LA    R1,CTRYSZIP                                                      
         LHI   R0,L'CTRYSZIP                                                    
CZV10    CLC   SPEADDZ(1),0(R1)                                                 
         JE    XIT                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,CZV10                                                         
         J     FLDINV                                                           
         DROP  R1                                                               
                                                                                
***********************************************************************         
*        ROUTINE ENSURES CHARACTER IS VALID NUMERIC                   *         
*        ON ENTRY ... P1 = A(CHARACTER TO CHECK)                      *         
***********************************************************************         
                                                                                
VALCANUM NTR1                                                                   
         L     RE,0(R1)                                                         
         CLI   0(RE),C'0'                                                       
         JL    FLDINV                                                           
         CLI   0(RE),C'9'                                                       
         JH    FLDINV                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES CHARACTER IS VALID ALPHABETIC                *         
*        ON ENTRY ... P1 = A(CHARACTER TO CHECK)                      *         
***********************************************************************         
                                                                                
VALCAALP NTR1                                                                   
         L     RE,0(R1)                                                         
         CLI   0(RE),C'A'                                                       
         JL    FLDINV                                                           
         CLI   0(RE),C'Z'                                                       
         JH    FLDINV                                                           
         CLI   0(RE),C'D'                                                       
         JE    FLDINV                                                           
         CLI   0(RE),C'F'                                                       
         JE    FLDINV                                                           
         CLI   0(RE),C'I'                                                       
         JE    FLDINV                                                           
         CLI   0(RE),C'O'                                                       
         JE    FLDINV                                                           
         CLI   0(RE),C'Q'                                                       
         JE    FLDINV                                                           
         CLI   0(RE),C'U'                                                       
         JE    FLDINV                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VALDD ROUTINES                    *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR24D                                                       
         SPACE 3                                                                
         ORG   SPEWORK                                                          
SVTGSSN  DS    CL9                 SAVED GLOBAL SSN                             
SVSSN    DS    CL9                                                              
SVPID    DS    CL6                                                              
SVDSKADD DS    XL4                 SAVED DISK ADDRESS                           
INPUTCRP DS    CL1                 Y=ALLOWED TO INPUT CORPS                     
PTRS     DS    CL(L'TLDRREC*8+1)   SAVED ACTIVE AND 7 PASSIVE PTRS              
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051TAGEN24   10/17/13'                                      
         END                                                                    
