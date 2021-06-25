*          DATA SET TAGEN23    AT LEVEL 024 AS OF 11/13/14                      
*PHASE T70223E,*                                                                
         TITLE 'T70223 - DUE COMPANY MAINTENANCE'                               
T70223   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70223,R7                                                      
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
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
DUE      GOTO1 INITIAL,DMCB,PFTABLE                                             
         TM    TGSYSTAT,TASYSPID                                                
         BZ    DUE00                                                            
         MVC   SDUSHED(13),=C'Performer Pid'                                    
         OI    SDUSHEDH+6,X'80'                                                 
*                                                                               
DUE00    MVC   SDUCUH,=C'Curr.'                                                 
         OI    SDUCUHH+6,X'80'                                                  
*                                                                               
DUE05    CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   THISLSEL,C'D'       IF DELETING FROM LIST                        
         BE    DUE10               DO NOT DISPLAY REC 1ST                       
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DUE30                                                            
*                                                                               
DUE10    CLI   MODE,RECDEL                                                      
         BE    CKDEL               CHECK OKAY TO DELETE                         
*                                                                               
         CLI   MODE,XRECDEL        RECORD DELETED                               
         BNE   DUE25                                                            
         GOTO1 ADDPTRS,DMCB,PTRS                                                
         BAS   RE,CHKDUE           CHECK IF OTHER DUE COMP RECS EXIST           
         B     DUE30                                                            
*                                                                               
DUE25    CLI   MODE,XRECREST       RECORD RESTORED                              
         BNE   DUE27                                                            
         GOTO1 ADDPTRS,DMCB,PTRS                                                
         BAS   RE,CHANGW4          SET DUE COMPANY PRESENT IN W4 FORM           
         B     DUE30                                                            
*                                                                               
DUE27    CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BNE   DUE28                                                            
         GOTO1 ADDPTRS,DMCB,PTRS                                                
         B     DUE30                                                            
*                                                                               
DUE28    CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BNE   DUE50                                                            
         GOTO1 ADDPTRS,DMCB,PTRS                                                
         BAS   RE,CHANGW4          SET DUE COMPANY PRESENT IN W4 FORM           
*                                                                               
DUE30    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     DUEX                                                             
*                                                                               
DUE50    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   DUEX                                                             
         BAS   RE,BLDREC                                                        
*                                                                               
DUEX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              VALIDATE THE KEY                                                 
*                                                                               
VK       DS    0H                                                               
         GOTO1 FLDVAL,DMCB,(X'40',SDUSSNH),(X'80',SDUREFH)                      
         BE    *+8                                                              
         MVI   PFAID,0                                                          
*                                                                               
         CLI   SCRSTAT,0           IF SCREEN CHANGE                             
         BE    *+8                                                              
         NI    SDUSSNH+4,X'DF'     FORCE KEY RE-VALIDATION                      
*                                                                               
         LA    R2,SDUSSNH          R2=A(SSN FIELD)                              
         TM    4(R2),X'20'                                                      
         BO    VK5                                                              
         NI    SDUREFH+4,X'DF'     SET TO VALIDATE NEXT FIELD                   
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK1                                                              
         CLI   SDUSSNH+5,0                                                      
         BE    VK1A                                                             
         CLI   SDUSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    VK1                 RECVAL CALL DOES NOT CHECK FOR               
         CLI   SDUSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   INVERR                                                           
         MVC   TGPID,SDUSSN                                                     
VK1A     OC    TGPID,TGPID                                                      
         BZ    MISSERR                                                          
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK1                                                              
         MVC   SDUSSN,TGSSN                                                     
         MVI   SDUSSNH+5,9                                                      
*                                                                               
VK1      GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SDUSSNMH                        
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK5                                                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SDUSSN,SPACES                                                    
         MVC   SDUSSN(L'TGPID),TGPID                                            
         MVI   SDUSSNH+5,6                                                      
         OI    SDUSSNH+6,X'80'                                                  
*                                                                               
VK5      LA    R2,SDUREFH                                                       
**ALWAYS TM    4(R2),X'20'                                                      
*VALIDATEBO    VK8                                                              
         MVI   DELSTAT,0           RESET DELETE STATUS                          
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK6                                                              
         GOTO1 RECVAL,DMCB,TLDUCDQ,(X'40',(R2))  BUILD KEY WITH GLOBALS         
         B     VKX                                                              
*                                                                               
VK6      GOTO1 ANY                                                              
         CLI   5(R2),6             TEST LENGTH IS 6                             
         BNE   VK8                                                              
         MVC   DUB(2),WORK+2       TEST IF VALID DATE                           
         MVC   DUB+2(2),=C'01'     CHANGE TO M/D/Y FORMAT                       
         MVC   DUB+4(2),WORK                                                    
         GOTO1 DATVAL,DMCB,(0,DUB),WORK+6                                       
         CLI   3(R1),0                                                          
         BE    VK8                 NOT VALID DATE                               
         MVC   WORK(4),WORK+6      YES - SO USE INTERNAL FORMAT                 
*                                                                               
VK8      GOTO1 RECVAL,DMCB,TLDUCDQ,(X'C0',WORK)  BLD KEY, NON-SCRN DATA         
VKX      OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         B     XIT                                                              
         SPACE 2                                                                
*              DISPLAY THE KEY                                                  
*                                                                               
DK       MVC   MYKEY,KEY           SAVE KEY                                     
         L     R4,AIO                                                           
         USING TLDUD,R4                                                         
         MVC   SDUSSN,TLDUSSN                                                   
         MVC   SDUREF,TLDUDUC                                                   
         CLI   TLDUDUC,X'FA'       IS THIS A YEAR 2000 DATE?                    
         BL    DK2                 NO                                           
         MVC   WORK(4),TLDUDUC     YES, CONVERT TO DISPLAY                      
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK)                                
         MVC   SDUREF(4),WORK                                                   
         MVC   SDUREF+4(2),TLDUDUC+4                                            
*                                                                               
DK2      OI    SDUREFH+6,X'80'                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',SDUSSN),SDUSSNMH                      
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    DK5                                                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SDUSSN,SPACES                                                    
         MVC   SDUSSN(L'TGPID),TGPID                                            
         MVI   SDUSSNH+5,6                                                      
         OI    SDUSSNH+6,X'80'                                                  
*                                                                               
DK5      MVC   AIO,AIO1            RESTORE IO AREA                              
         MVC   KEY,MYKEY           RESTORE KEY                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         BAS   RE,CLRSCRN          CLEAR PROTECTED SCREEN FIELDS                
*                                                                               
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         MVC   DSKADD,TLDRDA                                                    
*                                                                               
         L     R4,AIO              GET DUE COMPANY ELEMENT                      
         MVI   ELCODE,TADUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADUD,R4            R4=A(DUE COMPANY DETAILS EL.)                
*                                                                               
         MVI   SDUFLAG,C'T'                                                     
         TM    TADUSTAT,TADUSNTR   NON-TAXABLE REIMBURSEMENTS?                  
         JZ    *+8                                                              
         MVI   SDUFLAG,C'N'                                                     
         TM    TADUSTA2,TADUSTXR   TAXABLE REIMBURSEMENTS?                      
         JZ    *+8                                                              
         MVI   SDUFLAG,C'R'                                                     
*                                                                               
         MVC   ORIGFLAG,SDUFLAG                                                 
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   SDUEMP,TADUEMP                                                   
         MVI   SDUEMPH+5,3                                                      
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'0C',SDUEMPH),SDUEMPNH                     
         MVC   SDUAGY,TADUAGY                                                   
         MVI   SDUAGYH+5,6          DISPLAY AGENCY NAME                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'0C',SDUAGYH),SDUAGYNH                     
         MVC   AIO,AIO1             RESTORE IO AREA                             
*                                                                               
         MVC   SDUUNI,TADUUNI      DISPLAY UNION                                
         MVC   SDUERR,TADUTYPE                                                  
         OC    TADUINV,TADUINV     ANYTHING TO DIPLAY                           
         BZ    DR10                                                             
         GOTO1 TINVCON,DMCB,TADUINV,SDUINV,DATCON                               
*                                                                               
DR10     OC    TADUCINV,TADUCINV                                                
         BZ    DR20                                                             
         GOTO1 TINVCON,DMCB,TADUCINV,SDUCINV,DATCON                             
*                                   CREDIT INVOICE NUMBER                       
DR20     CLC   TADUFCLI,SPACES                                                  
         BNH   DR25                                                             
         MVC   SDUCLI,TADUFCLI     CLIENT FILTER                                
         MVI   SDUCLIH+5,6                                                      
         MVC   AIO,AIO2            DISPLAY CLIENT NAME                          
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'0C',SDUCLIH),SDUCLINH                     
         MVC   AIO,AIO1            RESTORE IO AREA                              
*                                                                               
DR25     BAS   RE,DRFLISTS         DISPLAY AGENCY & CLIENT FLIST CODES          
         EDIT  (4,TADUDUE),(12,SDUAMTD),2,ALIGN=LEFT,MINUS=YES                  
         EDIT  TADUPCT,(6,SDUPCT),2,ALIGN=LEFT,ZERO=BLANK                       
         EDIT  (4,TADUCOL),(12,SDUCOLL),2,ALIGN=LEFT,MINUS=YES                  
         OI    SDUCOLLH+6,X'80'    TRANSMIT FIELD                               
         ICM   R3,15,TADUDUE         AMOUNT DUE                                 
         ICM   R0,15,TADUCOL       - COLLECTED AMOUNT                           
         SR    R3,R0               = BALANCE                                    
         EDIT  (R3),(12,SDUBAL),2,ALIGN=LEFT,MINUS=YES                          
         OI    SDUBALH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
         BAS   RE,DRFILTS          DISPLAY RECOVERY FILTERS                     
         BAS   RE,DRSTAT           DISPLAY STATUS MESSAGE                       
         GOTO1 CHAROUT,DMCB,TACMELQ,(3,SDUNAR1H),TACMTYPG   NARRATIVE           
         GOTO1 ACTVOUT,DMCB,SDULCHGH                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CLEAR PROTECTED SCREEN FIELDS                         
*                                                                               
CLRSCRN  NTR1                                                                   
         TWAXC SDUEMPH                                                          
         MVC   SDUAGYN,SPACES       CLEAR AREA BEFORE DISPLAY                   
         OI    SDUAGYNH+6,X'80'                                                 
         MVC   SDUCLIN,SPACES                                                   
         OI    SDUCLINH+6,X'80'                                                 
         MVC   SDUEMPN,SPACES                                                   
         OI    SDUEMPNH+6,X'80'                                                 
         MVC   SDUAGFN,SPACES                                                   
         OI    SDUAGFNH+6,X'80'                                                 
         MVC   SDUCLFN,SPACES                                                   
         OI    SDUCLFNH+6,X'80'                                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DISPLAY AGENCY AND CLIENT FLISTS                      
*                                                                               
DRFLISTS NTR1                                                                   
         OC    TADUAYFL,TADUAYFL   IF AGENCY FLIST                              
         BZ    DRFLST5                                                          
         MVC   SDUAGFL,TADUAYFL                                                 
         MVI   SDUAGFLH+5,8                                                     
         MVC   AIO,AIO2                                                         
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'0C',SDUAGFLH),SDUAGFNH                    
         MVC   AIO,AIO1                                                         
*                                                                               
DRFLST5  OC    TADUCLFL,TADUCLFL   IF CLIENT FLIST                              
         BZ    XIT                                                              
         MVC   SDUCLFL,TADUCLFL                                                 
         MVI   SDUCLFLH+5,8                                                     
         MVC   AIO,AIO2                                                         
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'0C',SDUCLFLH),SDUCLFNH                    
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DISPLAY RECOVERY FILTERS                              
*                                                                               
DRFILTS  NTR1                                                                   
         TM    TADUSTAT,TADUSAGY   TEST COLLECT FROM AGY ONLY                   
         BNO   *+8                                                              
         MVI   SDUAGYF,C'Y'                                                     
         TM    TADUSTAT,TADUSCLI   TEST COLLECT FROM CLI ONLY                   
         BNO   *+8                                                              
         MVI   SDUCLIF,C'Y'                                                     
         SPACE 1                                                                
         MVI   SDUCUR,C'U'                                                      
         TM    TADUSTAT,TADUSCAN   TEST CANADIAN $                              
         BZ    *+8                                                              
         MVI   SDUCUR,C'C'                                                      
         TM    TADUSTAT,TADUSEUR   TEST EUROS                                   
         BZ    *+8                                                              
         MVI   SDUCUR,C'E'                                                      
         SPACE 1                                                                
         TM    TADUSTAT,TADUSLCK   TEST LOCKED                                  
         BNO   *+8                                                              
         MVI   SDULCK,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY MESSAGE STATUS                                
*                                                                               
         USING TADUD,R4            R4=A(DUE COMPANY DETAILS EL.)                
DRSTAT   NTR1                                                                   
         OI    SDUWAITH+1,X'04'    TURN MESSAGE TO LOW INTENSITY                
         OI    SDUWAITH+6,X'80'                                                 
         TM    TADUSTAT,TADUSHLD   TEST ON HOLD                                 
         BZ    *+14                                                             
         MVC   SDUWAIT,=CL21'* Awaiting Approval *'                             
         B     DRSTAT5                                                          
*                                                                               
         L     R4,AIO                                                           
         USING TLDUD,R4                                                         
         TM    TLDUSTAT,TLDUSPND   IF PRINT PENDING                             
         BZ    *+14                                                             
         MVC   SDUWAIT,=CL21'* Print Pending *'                                 
         B     DRSTAT5                                                          
*                                                                               
         MVI   ELCODE,TADRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DRSTATX                                                          
         USING TADRD,R4                                                         
         OC    TADRPDTE,TADRPDTE   TEST PRINTED                                 
         BZ    *+14                                                             
         MVC   SDUWAIT,=CL21'* Letter Printed *'                                
*                                                                               
DRSTAT5  NI    SDUWAITH+1,X'F8'    TURN MESSAGE TO HIGH INTENSITY               
         OI    SDUWAITH+6,X'80'                                                 
DRSTATX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         GOTO1 SAVPTRS,DMCB,PTRS                                                
*                                                                               
         MVI   INVSW,C'N'          DEFAULT NO INVOICES LISTED                   
         XC    SVCOL,SVCOL                                                      
         MVI   MYBYTE,0                                                         
         MVI   MYBYTE2,0                                                        
         L     R4,AIO              R4=A(DUE COMPANY RECORD)                     
         MVI   ELCODE,TADUELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         BAS   RE,SVSTAT           SAVE SOME INFORMATION                        
*                                                                               
         MVI   ELCODE,TADUELQ      DELETE THE OLD ELEMENT                       
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,ELEMENT          ADD DUE COMPANY ELEMENT                      
         USING TADUD,R4                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   TADUEL,TADUELQ                                                   
         MVI   TADULEN,TADULNQ                                                  
*                                                                               
         BAS   RE,BLDEMP           VALIDATE EMPLOYER                            
         BAS   RE,BLDTYPE          VALIDATE ERROR TYPE                          
         BAS   RE,BLDAGY           VALIDATE AGENCY AND AGENCY FLIST             
         BAS   RE,BLDUNI           VALIDATE UNION                               
         BAS   RE,BLDINV           VALIDATE INVOICE NUMBER                      
         BAS   RE,BLDCINV          VALIDATE CREDIT INVOICE NUMBER               
         BAS   RE,BLDCLI           VALIDATE CLIENT AND CLIENT FLIST             
*                                                                               
         BAS   RE,BLDDUE           AMOUNT DUE                                   
*                                                                               
         BAS   RE,BLDFLAG          VALIDATE FLAG                                
*                                                                               
         BAS   RE,BLDPCT           DEDUCT PERCENTAGE                            
*                                                                               
         BAS   RE,VALAGYF          VALIDATE AGENCY FILTER                       
         BAS   RE,VALCLIF          VALIDATE CLIENT FILTER                       
         BAS   RE,VALCANF          VALIDATE CANADIAN FILTER                     
         BAS   RE,VALLCKF          VALIDATE LOCKED FILTER                       
         MVC   TADUSTAT,MYBYTE                                                  
         MVC   TADUSTA2,MYBYTE2                                                 
*                                                                               
         MVC   TADUCOL,SVCOL      RESTORE AMOUNT COLLECTED                      
         GOTO1 ADDELEM                                                          
         GOTO1 NAMIN,DMCB,(3,TACMELQ),(X'80',SDUNAR1H),TACMTYPG                 
         GOTO1 ACTVIN,DMCB,SDULCHGH                LAST CHANGED                 
*                                                                               
BLDX     L     R1,AIO                                                           
         MVC   KEY,0(R1)           RESTORE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SAVE SOME INFO FROM DUE COMPANY DETAILS EL.           
*                                                                               
SVSTAT   NTR1                                                                   
         MVC   SVCOL,TADUCOL       SAVE AMOUNT COLLECTED                        
         TM    TADUSTAT,TADUSAUT   SAVE GENERATED BY PAY                        
         BZ    *+8                                                              
         OI    MYBYTE,TADUSAUT                                                  
         TM    TADUSTAT,TADUSHLD   SAVE AWAITING APPROVAL                       
         BZ    *+8                                                              
         OI    MYBYTE,TADUSHLD                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE EMPLOYER                                     
*                                                                               
BLDEMP   NTR1                                                                   
         LA    R2,SDUEMPH          IF NO INPUT                                  
         CLI   5(R2),0                                                          
         BNE   BLDE5               DEFAULT TO TGTPEMP                           
         MVC   SDUEMP,TGTPEMP                                                   
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
BLDE5    GOTO1 RECVAL,DMCB,TLEMCDQ,SDUEMPH                                      
         OC    SDUEMP,SPACES                 SET EMPLOYER                       
         MVC   TADUEMP,SDUEMP                                                   
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE ERROR TYPE                                   
*                                                                               
BLDTYPE  NTR1                                                                   
         LA    R2,SDUERRH          VALIDATE TYPE                                
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVI   TADUTYPE,TADUTYAY   DEFAULT                                      
         CLI   8(R2),TADUTYAY      AGENCY ERR                                   
         BE    XIT                                                              
         MVI   TADUTYPE,TADUTYDU                                                
         CLI   8(R2),TADUTYDU      DUE COMPANY                                  
         BNE   INVERR                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE AGENCY AND AGENCY FLIST FIELDS               
*                                                                               
BLDAGY   NTR1                                                                   
         LA    R2,SDUAGYH                                                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,SDUAGYH                                      
         OC    SDUAGY,SPACES                                                    
         MVC   TADUAGY,SDUAGY                                                   
*                                                                               
         LA    R2,SDUAGFLH                                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,SDUAGFLH                                     
         OC    SDUAGFL,SPACES                                                   
         MVC   TADUAYFL,SDUAGFL                                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE UNION                                        
*                                                                               
BLDUNI   NTR1                                                                   
         LA    R2,SDUUNIH          UNION                                        
         CLI   5(R2),0                                                          
         BNE   BLDU15                                                           
         CLC   SDUEMP,=C'TP '      REQUIRED ONLY IF EMPLOYER IS TP              
         BE    MISSERR                                                          
         B     XIT                                                              
*                                                                               
BLDU15   CLC   SDUEMP,=C'PP '      INPUT NOT ALLOWED IF PP (PRINT ONLY)         
         BE    NOINPUT                                                          
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING TLLOD,R3                                                         
         MVI   TLLOCD,TLLOCDQ                                                   
         MVC   TLLOUN,SDUUNI                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLLOLCL-TLLOD),KEYSAVE                                       
         BNE   INVERR                                                           
         OC    SDUUNI,SPACES                                                    
         MVC   TADUUNI,SDUUNI                                                   
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE INVOICE NUMBER                               
*                                                                               
BLDINV   NTR1                                                                   
         LA    R2,SDUINVH          INVOICE NUMBER                               
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 TINVCON,DMCB,8(R2),SAVINV,DATCON                                 
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'80',SAVINV)                               
         MVC   TADUINV,SAVINV      ERROR INVOICE NUMBER                         
         MVI   INVSW,C'Y'                                                       
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE CREDIT INVOICE NUMBER                        
*                                                                               
BLDCINV  NTR1                                                                   
         LA    R2,SDUCINVH         CREDIT INVOICE NUMBER                        
         CLI   5(R2),0                                                          
         BE    XIT                                                              
*                                                                               
         MVC   AIO,AIO2            CHANGE IO AREA FOR RECVAL                    
         GOTO1 TINVCON,DMCB,8(R2),SAVINV,DATCON                                 
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',SAVINV)                               
         MVC   AIO,AIO1            RESTORE IO AREA                              
*                                                                               
         L     R4,AIO2                                                          
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ      GET PAYMENT  ELEM & INSURE CR INV            
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         TM    TAPDPSTS,TAPDPCRD   CREDIT INVOICE                               
         BNO   INVERR                                                           
*                                                                               
         LA    R4,ELEMENT          RESTORE R4 TO ELEMENT                        
         USING TADUD,R4                                                         
         MVC   TADUCINV,SAVINV     CREDIT INVOICE NUMBER                        
         MVI   INVSW,C'Y'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CLIENT AND CLIENT FLIST FIELDS               
*                                                                               
BLDCLI   NTR1                                                                   
         LA    R2,SDUCLIH                                                       
         CLI   5(R2),0                                                          
         BE    BLDCLI5                                                          
         GOTO1 RECVAL,DMCB,TLCLCDQ,SDUCLIH                                      
         OC    SDUCLI,SPACES                                                    
         MVC   TADUFCLI,SDUCLI                                                  
*                                                                               
BLDCLI5  LA    R2,SDUCLFLH                                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,SDUCLFLH                                     
         OC    SDUCLFL,SPACES                                                   
         MVC   TADUCLFL,SDUCLFL                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE AMOUNT DUE                                   
*                                                                               
BLDDUE   NTR1                                                                   
         LA    R2,SDUAMTDH         AMOUNT DUE                                   
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   0(R1),0                                                          
         BNE   AMTINV              AMOUNT INVALID                               
         CLI   ACTNUM,ACTADD       ON ADD CANNOT ADD REC WITH AMT = 0           
         BNE   *+14                                                             
         CLC   4(4,R1),=F'00'                                                   
         BE    AMTINV                                                           
         MVC   TADUDUE,4(R1)       AMOUNT RETURNED BY CASHVAL                   
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE TAXABLE/NON-TAXABLE                          
*                                                                               
BLDFLAG  NTR1                                                                   
         NI    MYBYTE,X'FF'-TADUSNTR                                            
         NI    MYBYTE2,X'FF'-TADUSTXR                                           
         LA    R2,SDUFLAGH         TAX/NON-TAXABALE                             
*                                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA                                                    
         BNE   BFLAG05                                                          
         TM    4(R2),X'80'         FIELD INPUT AT THIS TIME?                    
         BZ    BFLAG05                                                          
         OC    SVCOL,SVCOL                                                      
         BZ    BFLAG05                                                          
         CLC   ORIGFLAG,SDUFLAG                                                 
         BNE   CHGERR                                                           
*                                                                               
BFLAG05  CLI   5(R2),0             DEFAULT TO TAXABLE                           
         BE    XIT                                                              
         CLI   8(R2),C'T'          TAXABLE?                                     
         BE    XIT                                                              
*                                                                               
         CLI   8(R2),C'N'          NON-TAXABLE REIMBURSEMENTS?                  
         BNE   BFLAG10                                                          
         OI    MYBYTE,TADUSNTR                                                  
         B     XIT                                                              
*                                                                               
BFLAG10  CLI   8(R2),C'R'          TAXABLE REIMBURSEMENTS?                      
         BNE   INVERR                                                           
         CLC   =C'P+',TADUEMP                                                   
         BNE   INVERR                                                           
         OI    MYBYTE2,TADUSTXR                                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE DEDUCT PERCENTAGE                            
*                                                                               
BLDPCT   NTR1                                                                   
         LA    R2,SDUPCTH          DEDUCT PERCENTAGE                            
         CLI   5(R2),0                                                          
         BE    BLDPCTX                                                          
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   0(R1),0                                                          
         BNE   AMTINV              AMOUNT INVALID                               
         CLC   4(4,R1),=F'00'                                                   
         BE    AMTINV                                                           
         CLC   4(4,R1),=F'10000'                                                
         BH    AMTINV              ERROR IF > 100                               
         MVC   TADUPCT,DMCB+6                                                   
BLDPCTX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE AGENCY FILTER                                
*                                                                               
VALAGYF  NTR1                                                                   
         LA    R2,SDUAGYFH         AGENCY FILTER                                
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         CLI   8(R2),C'N'                                                       
         BE    XIT                                                              
         CLI   8(R2),C'Y'                                                       
         BNE   INVERR                                                           
         OI    MYBYTE,TADUSAGY                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE CLIENT RECOVERY FILTER                       
*                                                                               
VALCLIF  NTR1                                                                   
         LA    R2,SDUCLIFH         CLIENT FILTER                                
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         CLI   8(R2),C'N'                                                       
         BE    XIT                                                              
         CLI   8(R2),C'Y'                                                       
         BNE   INVERR                                                           
         OC    TADUFCLI,TADUFCLI   MUST HAVE SPECIFIED CLI OR CFLIST            
         BNZ   *+14                                                             
         OC    TADUCLFL,TADUCLFL                                                
         BZ    INVERR                                                           
         OI    MYBYTE,TADUSCLI                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CANADIAN RECOVERY FILTER                     
*                                                                               
VALCANF  NTR1                                                                   
         LA    R2,SDUCURH          CURRENCY FILTER                              
         CLI   5(R2),0                                                          
         BE    XIT                                                              
                                                                                
         CLI   8(R2),C'C'                                                       
         BNE   *+12                                                             
         OI    MYBYTE,TADUSCAN                                                  
         B     XIT                                                              
         CLI   8(R2),C'E'                                                       
         BNE   *+12                                                             
         OI    MYBYTE,TADUSEUR                                                  
         B     XIT                                                              
         CLI   8(R2),C'U'                                                       
         BNE   INVERR                                                           
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE LOCKED RECOVERY FILTER                       
*                                                                               
VALLCKF  NTR1                                                                   
         LA    R2,SDULCKH          LOCK FILTER                                  
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         CLI   8(R2),C'N'                                                       
         BE    XIT                                                              
         CLI   8(R2),C'Y'                                                       
         BNE   INVERR                                                           
         OI    MYBYTE,TADUSLCK                                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  GET W4 RECORD AND SET DUE RECORD PRESENT                                     
*                                                                               
CHANGW4  NTR1                                                                   
*                                                                               
         MVC   AIO,AIO2            CHANGE IO AREAS                              
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    CH10                                                             
         CLI   SDUSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    CH10                RECVAL CALL DOES NOT CHECK FOR               
         MVC   TGPID,SDUSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   CH10                                                             
         MVC   SDUSSN,TGSSN                                                     
         MVI   SDUSSNH+5,9                                                      
*                                                                               
CH10     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'34',SDUSSNH)       UPDATE REC             
         BE    *+6                 RECORD WAS JUST READ - MUST BE THERE         
         DC    H'0'                                                             
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    CH20                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SDUSSN,SPACES                                                    
         MVC   SDUSSN(L'TGPID),TGPID                                            
         MVI   SDUSSNH+5,6                                                      
         OI    SDUSSNH+6,X'80'                                                  
*                                                                               
CH20     L     R4,AIO                                                           
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    TAW4STAT,TAW4STDU   TURN ON DUE COMPANY BIT                      
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
CHX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* ENSURE COLLECTED AMOUNT IS = 0 BEFORE ALLOWING DELETION OF RECORD             
* AND GIVE WARNING IF DUE COMPANY HAS LETTER PENDING PRINT                      
*                                                                               
CKDEL    DS    0H                                                               
         GOTO1 SAVPTRS,DMCB,PTRS                                                
*                                                                               
         L     R4,AIO              CHECK BALANCE AMOUNT                         
         USING TADUD,R4                                                         
         MVI   ELCODE,TADUELQ      DUE COMP DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,SDUCOLLH         R2=A(COLLECTED AMOUNT FIELD HEADER)          
         CLC   TADUCOL,=F'0'       IF RECORD HAS BEEN USED                      
         BE    *+12                                                             
         BAS   RE,DISPLAY          DISPLAY RECORD WE CANNOT DELETE              
         B     AMTINV              GIVE ERROR                                   
*                                                                               
         L     R4,AIO                                                           
         USING TLDUD,R4                                                         
         TM    TLDUSTAT,TLDUSPND   IF PRINT PENDING                             
         BZ    CKX                                                              
         TM    DELSTAT,DELSPEND    CHECK IF WARNING GIVEN                       
         BZ    DELERR                                                           
         CLI   PFAID,20            IF OVERRIDE PFKEY PRESSED                    
         BNE   DELERR                                                           
         NI    DELSTAT,X'FF'-DELSPEND                                           
         B     CKX                 THEN OKAY TO DELETE                          
*                                                                               
* ENSURE BALANCE IS = 0 BEFORE ALLOWING DELETION OF RECORD                      
*        LA    R2,SDUBALH                                                       
*        ICM   R1,15,TADUDUE       TOTAL DUE                                    
*        ICM   R3,15,TADUCOL       - TOTAL COLLECTED                            
*        SR    R1,R3               = TOTAL BALANCE                              
*        C     R1,=F'00'                                                        
*        BE    CKX                                                              
*        BAS   RE,DISPLAY          DISPLAY RECORD WE CANNOT DELETE              
*        B     AMTINV                                                           
*                                                                               
CKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*       CHECK IF OTHER DUE COMPANY RECORDS EXIST -                              
*       IF NOT - TURN OFF BIT IN W4 RECORD                                      
*                                                                               
CHKDUE   NTR1                                                                   
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED RECS                   
         XC    KEY,KEY                                                          
         MVC   AIO,AIO2                                                         
         LA    R3,AIO1                                                          
         MVC   KEY(TLDUDUC-TLDUD),0(R3)   USING SAME SS NUM AS RECORD           
         GOTO1 HIGH                                                             
*                                                                               
CHK10    CLC   KEY(TLDUDUC-TLDUD),KEYSAVE                                       
         BNE   CHK20               IF ANOTHER RECORD EXISTS                     
         L     R4,AIO                                                           
         USING TADUD,R4                                                         
         GOTO1 GETREC                                                           
         MVI   ELCODE,TADUELQ      WITH THE SAME EMPLOYER                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TADUEMP,TGEMP                                                    
         BE    CHKX                EXIT                                         
         GOTO1 SEQ                 CHECK IF NEXT REC IS SAME EMPLOYER           
         B     CHK10                                                            
*                                                                               
CHK20    MVC   AIO,AIO2                                                         
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    CHK30                                                            
         CLI   SDUSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    CHK30               RECVAL CALL DOES NOT CHECK FOR               
         MVC   TGPID,SDUSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   CHK30                                                            
         MVC   SDUSSN,TGSSN                                                     
         MVI   SDUSSNH+5,9                                                      
*                                                                               
CHK30    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'34',SDUSSNH)                              
         BE    *+6                 RECORD WAS JUST READ - MUST BE THERE         
         DC    H'0'                                                             
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    CHK40                                                            
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SDUSSN,SPACES                                                    
         MVC   SDUSSN(L'TGPID),TGPID                                            
         MVI   SDUSSNH+5,6                                                      
         OI    SDUSSNH+6,X'80'                                                  
*                                                                               
CHK40    L     R4,AIO              GET W4 RECORD/ELEMENT                        
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    TAW4STAT,X'FF'-TAW4STDU   TURN OFF DUE COMPANY BIT               
         GOTO1 PUTREC                                                           
*                                                                               
CHKX     MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
XIT      XIT1                                                                   
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
NOINPUT  MVI   ERROR,ERNOINP                                                    
         B     ERRXIT                                                           
*                                                                               
AMTINV   MVI   ERROR,ERINVAMT                                                   
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
CHGERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
DELERR   MVI   ERROR,ERDUEPND                                                   
         LA    R2,SDUSSNH           R2=A(SSN FIELD HEADER)                      
         OI    DELSTAT,DELSPEND     SET PENDING PFKEY OVERRIDE                  
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
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
         DC    AL1(PF15X-*,15,0,0,0)                                            
         DC    CL3' ',CL8'YTD     ',CL8'DISPLAY '                               
PF15X    EQU   *                                                                
*                                                                               
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3' ',CL8'LIEN    ',CL8'LIST    '                               
PF16     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF16X    EQU   *                                                                
*                                                                               
         DC    AL1(PF17X-*,17,0,0,0)                                            
         DC    CL3' ',CL8'DTRACK  ',CL8'LIST    '                               
PF17X    EQU   *                                                                
*                                                                               
         DC    AL1(PF18X-*,18,0,0,0)                                            
         DC    CL3' ',CL8'GRT     ',CL8'LIST    '                               
PF18X    EQU   *                                                                
*                                                                               
         DC    AL1(PF19X-*,19,0,0,0)                                            
         DC    CL3' ',CL8'DLETTER ',CL8'DISPLAY '                               
PF19X    EQU   *                                                                
*                                                                               
         DC    AL1(PF20X-*,20,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'LIST    '                               
PF20X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR23D                                                       
         EJECT                                                                  
         ORG   SDUWORK                                                          
*                                                                               
INVSW    DS    CL1                 SWITCH TO SEE IF INVOICES EXIST              
DELSTAT  DS    XL1                                                              
DELSPEND EQU   X'80'               SET DELETE PENDING PFKEY OVERRIDE            
MYBYTE   DS    XL1                                                              
MYBYTE2  DS    XL1                                                              
SVCOL    DS    XL4                 AMOUNT COLLECTED                             
SAVINV   DS    PL6                 INTERNAL INVOICE # REPRESENTATION            
MYKEY    DS    CL38                SAVED KEY                                    
DSKADD   DS    CL4                                                              
ORIGFLAG DS    CL1                                                              
PTRS     DS    CL(L'TLDRREC*2+1)   SAVED ACTIVE AND 1 PASSIVE PTRS              
*                                                                               
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024TAGEN23   11/13/14'                                      
         END                                                                    
