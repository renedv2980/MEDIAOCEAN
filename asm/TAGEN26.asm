*          DATA SET TAGEN26    AT LEVEL 094 AS OF 06/15/15                      
*PHASE T70226A,*                                                                
         TITLE 'T70226 - INVOICE NEW/DISPLAY/CHANGE/APPROVE/UNAPPROVE'          
T70226   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70226,R6                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=LOCAL SAVED STORAGE                       
         USING INVD,R7                                                          
         SPACE 3                                                                
         MVC   TGBYTE2,SVRECUSE                                                 
         MVI   TGBYTE3,0                                                        
         CLI   MODE,VALKEY                                                      
         BNE   INVA                                                             
         CLC   ACTNUM,TWALACT                                                   
         BE    INVA                                                             
         MVI   TGBYTE3,X'80'                                                    
INVA     GOTO1 INITIAL,DMCB,(TGBYTE3,0)   INITIALIZE                            
         MVC   SVRECUSE,TGBYTE2                                                 
         SPACE 3                                                                
         CLI   ACTNUM,ACTAPP       TEST ACTION APPROVE                          
         BE    INV20               HANDLE ELSEWHERE                             
         CLI   ACTNUM,ACTUNAPP     TEST ACTION UNAPPROVE                        
         BE    INV40               HANDLE ELSEWHERE                             
         CLI   ACTNUM,ACTNEW       TEST ACTION NEW                              
         BE    INV10               HANDLE ELSEWHERE                             
         CLI   ACTNUM,ACTDEL       TEST ACTION DELETE                           
         BE    INV60               HANDLE ELSEWHERE                             
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES FOR INVOICE MAINTENANCE                 
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BE    INV0                                                             
         CLI   MODE,PROCPFK                                                     
         BNE   INV3                                                             
INV0     CLI   ACTNUM,ACTADD       DON'T ALLOW ADD                              
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTREST      DON'T ALLOW RESTORE                          
         BE    ACTERR                                                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SIDAGYH),SIDAGYNH                     
         BRAS  RE,GTAGYST                                                       
*                                                                               
         LA    R2,SIDINVH                                                       
         CLI   5(R2),0             IF NO INVOICE INPUT                          
         BNE   INV2                USE GLOBAL INVOICE                           
         MVI   BYTE,0                                                           
         CLI   RECNUM,DI           IF DINVOICE                                  
         BNE   *+8                                                              
         OI    BYTE,X'04'          ONLY LOOK FOR SOFT DELETED INVOICE           
         GOTO1 RECVAL,DMCB,(BYTE,TLINCDQ),(X'04',SIDINVH)                       
         SPACE 1                                                                
         CLI   ERROR,MISSING       IF THERE IS A GLOBAL INVOICE                 
         BE    THEEND                                                           
         XC    SIDINV,ALLFF        UN-COMPLEMENT INVOICE NUMBER                 
         GOTO1 TINVCON,DMCB,SIDINV,INV,DATCON CONVERT FOR DISPLAY               
         MVC   SIDINV,INV                                                       
         CLI   0(R1),X'FF'         IF INVALID                                   
         BNE   INV1                                                             
         XC    SIDINV,SIDINV       CLEAR FROM SCREEN                            
         B     FLDMISS             REQUIRE INVOICE INPUT                        
         SPACE                                                                  
INV1     LA    R2,SIDAGYH                                                       
         CLI   ERROR,NOTFOUND                                                   
         BE    THEEND                                                           
         B     INV2X                                                            
         SPACE                                                                  
INV2     GOTO1 TINVCON,DMCB,SIDINV,INV,DATCON CONVERT INV INPUT FOR KEY         
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    INV,ALLFF           COMPLEMENT INVOICE NUMBER                    
         MVI   BYTE,0                                                           
         CLI   RECNUM,DI           IF DINVOICE                                  
         BNE   *+8                                                              
         OI    BYTE,X'04'          ONLY LOOK FOR SOFT DELETED INVOICE           
         GOTO1 RECVAL,DMCB,(BYTE,TLINCDQ),(X'80',INV)                           
         BE    INV2A                                                            
         CLI   ERROR,NOTFOUND                                                   
         BE    THEEND                                                           
         B     INV2X                                                            
         SPACE                                                                  
INV2A    GOTOR PFCMNT                                                           
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
INV2X    B     XIT                                                              
         EJECT                                                                  
*              DISPLAY KEY FOR SELECT                                           
         SPACE 1                                                                
         USING TLIND,R3                                                         
INV3     CLI   MODE,DISPKEY                                                     
         BNE   INV5                                                             
         L     R3,AIO                                                           
         MVC   SIDAGY,TLINAGY      MOVE AGENCY TO SCREEN                        
         OI    SIDAGYH+6,X'80'                                                  
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',SIDAGY),SIDAGYNH GET NAME             
         BRAS  RE,GTAGYST                                                       
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   KEY,SVKEY                                                        
         SPACE 1                                                                
         MVC   INV,TLININV                                                      
         XC    INV,ALLFF           UN-COMPLEMENT INVOICE NUMBER                 
         GOTO1 TINVCON,DMCB,INV,SIDINV,DATCON CONVERT FOR DISPLAY               
         OI    SIDINVH+6,X'80'                                                  
         B     XIT                                                              
         SPACE 3                                                                
INV5     CLI   MODE,DISPREC                                                     
         BE    INV6                                                             
         CLI   MODE,XRECPUT        IF MODE IS RECORD CHANGED                    
         BE    INV5B                                                            
         CLI   MODE,XRECREST       OR RESTORED                                  
         BE    INV5B                                                            
         CLI   MODE,XRECDEL        OR DELETED                                   
         BNE   INV8                                                             
INV5B    GOTO1 ADDPTRS,DMCB,INVPTRS  UPDATE PASSIVE POINTERS                    
         SPACE                                                                  
INV6     BAS   RE,DISPLAY          (RE)DISPLAY RECORD                           
         SPACE                                                                  
INV8     CLI   MODE,VALREC         TEST MODE IS VALIDATE RECORD                 
         BE    *+12                                                             
         CLI   MODE,RECDEL         OR DELETE RECORD                             
         BNE   XIT                                                              
         GOTO1 SAVPTRS,DMCB,INVPTRS  SAVE CURRENT PASSIVE POINTERS              
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   *+12                                                             
         BAS   RE,CHGREC                                                        
         B     XIT                                                              
         LA    R2,SIDINVH                                                       
         GOTOR CHKDEL,DMCB,0       IF ACT NOT CHANGE MUST BE DELETE             
         B     XIT                                                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES FOR INVOICE/NEW                         
         SPACE 1                                                                
INV10    CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   INV18                                                            
         MVI   APPSTAT,0           INSURE APPROVE STATUS BYTE CLEARED           
         SPACE                                                                  
         CLI   SCRSTAT,0           IF SCREEN HAS CHANGED                        
         BNE   *+12                                                             
         TM    SINAGYH+4,X'20'     OR AGENCY NOT PREV VALIDATED                 
         BO    *+8                                                              
         NI    SINNUMIH+4,X'DF'    SET NUMBER OF INVOICES NOT VALIDATED         
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'18',SINAGYH),SINAGYNH  GET AGENCY         
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY EL                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TAAYD,R4                                                         
         TM    TAAYSTA3,TAAYSLCK   CAN'T ADD INVOICES FOR LOCKED AGYS           
         BO    AGYLOCK                                                          
         B     XIT                                                              
         SPACE 3                                                                
INV18    CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES FOR INVOICE/APPROVE                     
         SPACE 1                                                                
INV20    CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   INV30                                                            
         TM    APPSTAT,INPROGRS                                                 
         BO    XIT                 XIT IF ALREADY IN PROGRESS                   
         TM    APPSTAT,APPDONE     IF FINISHED APPROVING THIS INVOICE           
         BZ    INV22                                                            
         MVI   APPSTAT,0           CLEAR STATUS                                 
         MVI   PFAID,21            SET PF KEY TO POP BACK TO LIST SCRN          
         OI    TRNSTAT,OKINTPFK                                                 
         GOTO1 INITIAL,DMCB,APPPFTAB                                            
         SPACE 1                                                                
INV22    BRAS  RE,APUAVKEY         VALIDATE KEY FIELDS                          
         SPACE 1                                                                
         USING TAIND,R4            RETURNS R4=A(INVOICE ELEMENT)                
         TM    TAINSTAT,TAINSAPR   TEST IT HASN'T BEEN APPROVED YET             
         BO    APRERR                                                           
         TM    TGCTSTLV,X'10'      EXCEPT FOR HIGH LEVEL ACCESS                 
         BO    INV26                                                            
         CLC   TAINPID,TWAORIG     INSURE NOT SAME AS PAYER                     
         BNE   INV26                                                            
         CLC   TAINPST,TGCTSTAF                                                 
         BE    CANTAPPR            CAN'T APPROVE WHAT YOU PAY                   
         SPACE 1                                                                
INV26    BRAS  RE,CKAPPGRT         CHECK OK IF APPLYING AGAINST GRT             
         SPACE 1                                                                
         MVC   FPAGE,TAINFPG       SAVE FIRST                                   
         MVC   LPAGE,TAINLPG       AND LAST PAGE NUMBERS                        
         SPACE 1                                                                
         TM    TAINSTA2,TAINSSCR                                                
         BZ    XIT                                                              
         MVC   SCRINV(3),TAINIDTE                                               
         MVC   SCRINV+3(3),TAINITIM                                             
         B     XIT                                                              
         SPACE 1                                                                
INV30    CLI   MODE,VALREC         MAIN PROCESS                                 
         BNE   XIT                                                              
         BRAS  RE,APPROVE                                                       
         B     XIT                                                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES FOR INVOICE/UNAPPROVE                   
         SPACE 1                                                                
INV40    CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   INV50                                                            
         TM    APPSTAT,INPROGRS    IF APPROVAL IN PROGRESS                      
         BZ    INV44                                                            
         MVI   APPSTAT,0           CLEAR APPROVAL STATUS BYTE                   
         MVI   TWASCR,0            CLEAR SAVED SCREEN                           
         MVI   GOAGAIN,C'Y'        AND SET TO GO AGAIN TO RE-LOAD SCRN          
         MVI   ERROR,X'FE'                                                      
         B     THEEND                                                           
         SPACE 1                                                                
INV44    MVI   APPSTAT,0           CLEAR APPROVAL STATUS BYTE                   
         SPACE 1                                                                
         BRAS  RE,APUAVKEY         VALIDATE KEY FIELDS                          
         SPACE 1                                                                
         USING TAIND,R4            RETURNS R4=A(INVOICE ELEMENT)                
         TM    TAINSTAT,TAINSAPR   TEST IT'S BEEN APPROVED ALREADY              
         BZ    UNAPRERR                                                         
         TM    TAINSTAT,TAINSBIL+TAINSCHK+TAINSCIN  IF BILL/PAID/CANCEL         
         BNZ   CANTUNAP                                                         
         TM    TAINSTA2,TAINSHLP   OR COD PRINTED-CAN'T UNAPPROVE               
         BNZ   CANTUNAP                                                         
         BRAS  RE,CHKUGRT          CHECK OK TO UNAPPROVE GRT PAYMENT            
         BRAS  RE,CHKUPCY          CHECK OK TO UNAPPROVE PERCYC PAYMENT         
         B     XIT                                                              
         SPACE 1                                                                
INV50    CLI   MODE,VALREC         MAIN PROCESS                                 
         BNE   XIT                                                              
         BAS   RE,UNAPPR                                                        
         B     XIT                                                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES FOR INVOICE/DELETE                      
         SPACE 1                                                                
INV60    MVC   DELPF14,SPACES                                                   
         OI    DELPF14H+6,X'80'                                                 
         CLI   SVRECUSE,C'O'                                                    
         BNE   INV61                                                            
         MVC   DELPF14,=C'PF14=Advice/Complete'                                 
         GOTO1 INITIAL,DMCB,DPFTAB                                              
         SPACE 1                                                                
INV61    CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   INV70                                                            
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',DELAGYH),DELAGYNH                     
         LA    R2,DELINV1H                                                      
         GOTO1 ANY                                                              
         GOTO1 TINVCON,DMCB,8(R2),INV,DATCON  CVT TO INTERNAL FORMAT            
         MVC   FRSTINV,INV                                                      
         XC    FRSTINV,ALLFF                  COMPLEMENT AND SAVE IT            
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',FRSTINV)  VALIDATE INVOICE            
         BNE   NTFOUND             ERROR IF NOT FOUND                           
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAAIELQ                                                   
         BAS   RE,GETEL                                                         
         BE    INV62                                                            
         MVI   SVRECUSE,0                                                       
         MVC   DELPF14,SPACES                                                   
         SPACE 1                                                                
INV62    GOTOR CHKDEL,DMCB,0       CHECK OK TO DELETE                           
         SPACE 1                                                                
INV64    MVC   LASTINV,FRSTINV     SET LAST INV = FIRST IN CASE NO LAST         
         CLI   DELINV2H+5,0        IF LAST INVOICE INPUT                        
         BE    XIT                                                              
         LA    R2,DELINV2H                                                      
         GOTO1 TINVCON,DMCB,8(R2),DUB,DATCON  CVT TO INTERNAL FORMAT            
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         CLC   INV,DUB             TEST LAST INVOICE NOT BEFORE FIRST           
         BH    BADRANGE                                                         
         CLC   INV(3),DUB          TEST SAME MONTH                              
         BNE   BADRANGE                                                         
         SPACE                                                                  
         MVC   LASTINV,DUB                                                      
         XC    LASTINV,ALLFF        COMPLEMENT AND SAVE IT                      
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',LASTINV)  VALIDATE INVOICE            
         BNE   NTFOUND                            ERROR IF NOT FOUND            
         GOTOR CHKDEL,DMCB,(X'80',0)              CHECK OK TO DELETE            
         B     XIT                                                              
         SPACE                                                                  
INV70    CLI   MODE,VALREC         MAIN PROCESS                                 
         BNE   XIT                                                              
         BAS   RE,DELETE                                                        
         B     XIT                                                              
         EJECT                                                                  
*              CHANGE THE RECORD IN AIO                                         
         SPACE 1                                                                
CHGREC   NTR1                                                                   
         MVI   INTFLAG,0                                                        
         BRAS  RE,VALRSND          VALIDATE RESEND                              
*                                                                               
         L     R4,AIO                                                           
         MVI   PAYPST1,0                                                        
         MVI   PAYPST2,0                                                        
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   CHGR5                                                            
         TM    TAPDSTA2,TAPDSSUB                                                
         BO    NOTAVSUB            CHANGE NOT ALLOWED FOR SUBS INV              
         MVC   PAYPST1,TAPDPST1    SAVE PAYMENT STATUS BYTES                    
         MVC   PAYPST2,TAPDPST2                                                 
         SPACE 1                                                                
CHGR5    L     R4,AIO                                                           
         USING TAIND,R4                                                         
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   INVSTAT,TAINSTAT    SAVE INVOICE STATUS                          
         SPACE 1                                                                
         BAS   RE,VALRETR          RETRO HOLD STATUS                            
         BAS   RE,VALRETS          RETRO SKIP STATUS                            
         BAS   RE,VALERRT          ERROR TYPE                                   
*                                                                               
         CLI   TGCTSTTY,TASTTYPP   ONLY IF PROGRAMMER                           
         BNE   *+16                                                             
         BAS   RE,VALBDTE          BILL DATE                                    
         BRAS  RE,VALCDTE          CHECK DATE                                   
         BAS   RE,VALCRUN          CHECK RUN DATE                               
*                                                                               
         TM    INVSTAT,TAINSCIN+TAINSCAN DONE IF CANCELLER OR CANCELLEE         
         BNZ   *+12                                                             
         BAS   RE,VALDDTE          DUE DATE                                     
         BAS   RE,VALURG           URGENT STATUS                                
*                                                                               
         BAS   RE,VNOINT           NO INT STATUS                                
*                                                                               
         BAS   RE,VCODHOLD         COD HOLD                                     
*                                                                               
         BRAS  RE,VALWID           WEB APPLICATION ID                           
*                                                                               
         GOTO1 ACTVIN,DMCB,SIDLCHGH  LAST CHANGED                               
*                                                                               
         TM    INTFLAG,RETYES      MADE RETRO SKIP N TO Y?                      
         BZ    CHGR10                                                           
         MVC   BYTE,TWASCR                                                      
         MVI   TWASCR,C'R'                                                      
         GOTO1 ACTVIN,DMCB,(X'80',0)                                            
         MVC   TWASCR,BYTE                                                      
         B     CHGRX                                                            
*                                                                               
         USING TAACD,R4                                                         
CHGR10   TM    INTFLAG,RETNO       MADE RETRO SKIP Y TO N?                      
         BZ    CHGRX                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TAACELQ      YES - MUST DELETE RETRO ACTIVITY             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CHGR15   BAS   RE,NEXTEL                                                        
         BNE   CHGR20                                                           
         CLI   TAACSCR,C'R'        RETRO ACTIVITY ELEMENT?                      
         BNE   CHGR15                                                           
         MVI   0(R4),X'FF'                                                      
*                                                                               
CHGR20   MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
CHGRX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES RETRO HOLD STATUS                              
         SPACE 1                                                                
         USING TAIND,R4            R4=A(INVOICE STATUS ELEMENT)                 
VALRETR  NTR1                                                                   
         LA    R2,SIDRETRH                                                      
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    VRETRX                                                           
         CLI   5(R2),0                                                          
         BE    VRETRX                                                           
         CLI   8(R2),C'N'          & IF WANT TO CHANGE IT                       
         BNE   VRETRX                                                           
         NI    TAINSTA2,X'FF'-TAINSRTH  TURN OFF RETRO BIT                      
VRETRX   B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE VALIDATES RETRO SKIP STATUS                              
         SPACE 1                                                                
         USING TAIND,R4            R4=A(INVOICE STATUS ELEMENT)                 
VALRETS  NTR1                                                                   
         CLI   ACTNUM,ACTCHA                                                    
         BNE   VRETS5                                                           
         LA    R2,SIDSKIPH                                                      
         TM    TAINSTA3,TAINSRSK   WAS IT "Y"?                                  
         BZ    *+16                                                             
         CLI   8(R2),C'N'                                                       
         BNE   *+8                                                              
         OI    INTFLAG,RETNO       CHANGED FROM YES TO NO                       
*                                                                               
         TM    TAINSTA3,TAINSRSK                                                
         BO    *+16                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   *+8                                                              
         OI    INTFLAG,RETYES      CHANGED FROM NO TO YES                       
*                                                                               
VRETS5   LA    R2,SIDSKIPH                                                      
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    VRETSX                                                           
         CLI   5(R2),0                                                          
         BE    VRETSX                                                           
         CLI   8(R2),C'N'          & IF WANT TO CHANGE IT                       
         BNE   VRETS10                                                          
         NI    TAINSTA3,X'FF'-TAINSRSK   DON'T SKIP IT                          
         B     VRETSX                                                           
VRETS10  CLI   8(R2),C'Y'                                                       
         BNE   VRETSX                                                           
         OI    TAINSTA3,TAINSRSK   SKIP INV IN RETRO GENERATE                   
VRETSX   B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE VALIDATES ERROR TYPE                                     
         SPACE 1                                                                
         USING TAIND,R4            R4=A(INVOICE STATUS ELEMENT)                 
VALERRT  NTR1                                                                   
         LA    R2,SIDERRTH         VALIDATE ERROR TYPE                          
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    VERRTX                                                           
         MVI   TAINTERR,0          CLEAR ERROR TYPE                             
         NI    TAINSTAT,X'FF'-TAINSERR  TURN OFF IN ERROR BIT                   
         SPACE                                                                  
         CLI   5(R2),0             IF THERE'S INPUT                             
         BE    VERRTX                                                           
         TM    INVSTAT,TAINSPAY                                                 
         BZ    PAYERR2             NOT ALLOWED IF NOT PAID                      
         GOTO1 VALINUM                                                          
         MVC   TAINTERR,ACTUAL     SAVE ERROR TYPE IN INVOICE STAT EL           
         OI    TAINSTAT,TAINSERR   TURN ON IN ERROR BIT                         
VERRTX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES BILL DATE                                      
         SPACE 1                                                                
         USING TAIND,R4            R4=A(INVOICE STATUS ELEMENT)                 
VALBDTE  NTR1                                                                   
         LA    R2,SIDBDTEH                                                      
         CLI   5(R2),0             TEST EMPTY FIELD                             
         BNE   *+16                                                             
         TM    TAINSTAT,TAINSBIL   OK IF NOT BILLED                             
         BZ    VBDTEX                                                           
         B     FLDMISS             ELSE REQUIRED                                
         SPACE                                                                  
         TM    TAINSTAT,TAINSBIL                                                
         BZ    NOINPUT             INPUT NOT ALLOWED IF NOT BILLED              
         GOTO1 DTVAL,DMCB,TAINBDTE                                              
VBDTEX   B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE VALIDATES CHECK RUN DATE                                 
         SPACE 1                                                                
         USING TAIND,R4            R4=A(INVOICE STATUS ELEMENT)                 
VALCRUN  NTR1                                                                   
         LA    R2,SIDCRUNH                                                      
         CLI   5(R2),0             IF FIELD EMPTY                               
         BNE   VCRUN10                                                          
         XC    TAINCKRN,TAINCKRN   CLEAR DATE                                   
         XC    TAINCKID,TAINCKID   CLEAR CHECK RUN ID TOO                       
         B     VCRUNX                                                           
         SPACE 1                                                                
VCRUN10  GOTO1 DTVAL,DMCB,TAINCKRN ELSE VALIDATE/SET DATE                       
VCRUNX   B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE VALIDATES DUE DATE                                       
         SPACE 1                                                                
         USING TAIND,R4            R4=A(INVOICE STATUS ELEMENT)                 
VALDDTE  NTR1                                                                   
         LA    R2,SIDDDTEH                                                      
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    VDDTEX                                                           
         TM    INVSTAT,TAINSCHK                                                 
         BO    NOCHANGE            ERROR IF CHECKS WRITTEN                      
         TM    PAYPST1,TAPDPCRD    OR THIS IS A CREDIT INVOICE                  
         BO    NOCHGCRD                                                         
         SPACE                                                                  
         CLI   5(R2),0             TEST EMPTY FIELD                             
         BNE   *+16                                                             
         TM    INVSTAT,TAINSPAY    OK IF NOT PAID                               
         BZ    VDDTEX                                                           
         B     FLDMISS             ELSE REQUIRED                                
         SPACE                                                                  
         TM    INVSTAT,TAINSPAY                                                 
         BZ    PAYERR2             INPUT NOT ALLOWED IF NOT PAID                
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TADDELQ      GET DUE DATE ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         USING TADDD,R4                                                         
         TM    INVSTAT,TAINSBIL    IF BILLED                                    
         BZ    VDDTE20                                                          
         OC    TADDPREV,TADDPREV   AND DON'T HAVE A PREVIOUS DUE DATE           
         BNZ   *+10                                                             
         MVC   TADDPREV,TADDDATE   SAVE ORIGINAL DUE DATE                       
         MVC   TADDCHNG,TGTODAY1   DATE OF CHANGE                               
         SPACE                                                                  
VDDTE20  GOTO1 DTVAL,DMCB,TADDDATE VALIDATE DATE                                
         SPACE                                                                  
VDDTEX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES URGENT STATUS                                  
         SPACE 1                                                                
         USING TAIND,R4            R4=A(INVOICE STATUS ELEMENT)                 
VALURG   NTR1                                                                   
         LA    R2,SIDURGH                                                       
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    VURGX                                                            
         TM    INVSTAT,TAINSCHK                                                 
         BO    NOCHANGE            ERROR IF CHECKS WRITTEN                      
         SPACE                                                                  
         CLI   5(R2),0             IGNORE IF EMPTY FIELD                        
         BE    VURGX                                                            
         SPACE                                                                  
         TM    INVSTAT,TAINSPAY                                                 
         BZ    PAYERR2             INPUT NOT ALLOWED IF NOT PAID                
         SPACE                                                                  
         CLI   8(R2),C'Y'          ONLY ALLOWED TO CHANGE TO Y                  
         BNE   FLDINV                                                           
         SPACE                                                                  
         BRAS  RE,CKAPPGRT         CHECK OK IF APPLYING AGAINST GRT             
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TADDELQ      GET DUE DATE ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TADDD,R4                                                         
         TM    INVSTAT,TAINSBIL    IF BILLED                                    
         BZ    VURG20                                                           
         OC    TADDPREV,TADDPREV   AND DON'T HAVE A PREVIOUS DUE DATE           
         BNZ   *+10                                                             
         MVC   TADDPREV,TADDDATE   SAVE ORIGINAL DUE DATE                       
         MVC   TADDCHNG,TGTODAY1   DATE OF CHANGE                               
         SPACE                                                                  
VURG20   MVC   TADDDATE,TGTODAY1   MAKE DUE DATE=TODAY                          
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TAIND,R4                                                         
         OI    TAINSTA2,TAINSURG   TURN ON URGENT BIT                           
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TAPDD,R4                                                         
         OI    TAPDOPT2,TAPDOURG   TURN ON URGENT BIT                           
         SPACE                                                                  
VURGX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES COD HOLD STATUS                                
         SPACE 1                                                                
         USING TAIND,R4            R4=A(INVOICE STATUS ELEMENT)                 
VCODHOLD NTR1                                                                   
         TM    TAINSTAT,TAINSPAY   DON'T BOTHER IF NOT PAID                     
         BZ    VCHX                                                             
         SPACE 1                                                                
         LA    R2,SIDHOLDH                                                      
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    VCHX                                                             
         SPACE 1                                                                
         CLI   8(R2),C'N'          NEW INPUT CAN BE NO                          
         BNE   VCH10                                                            
         TM    TAINSTAT,TAINSHLD   AS LONG AS STATUS ISN'T HOLD                 
         BO    FLDINV                                                           
         B     VCHX                                                             
         SPACE 1                                                                
VCH10    CLI   8(R2),C'Y'          NEW INPUT CAN BE YES                         
         BNE   FLDINV                                                           
         TM    TAINSTAT,TAINSCAN   BUT CAN'T BE CANCELLED                       
         BO    FLDINV                                                           
         TM    TAINSTAT,TAINSHLD   IF CURRENT STATUS IS HOLD                    
         BO    VCHX                                                             
         TM    TAINSTA2,TAINSHLR   OR RELEASED                                  
         BZ    FLDINV                                                           
         TM    INVSTAT,TAINSCHK    AND CHECKS HAVE ALREADY BEEN                 
         BO    NOCHANGE            WRITTEN                                      
         OI    TAINSTAT,TAINSHLD   TURN HOLD STATUS ON AND RELEASED             
         NI    TAINSTA2,X'FF'-TAINSHLR                   STATUS OFF             
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TAACELQ      GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VCH20    BAS   RE,NEXTEL                                                        
         BNE   VCHX                                                             
         USING TAACD,R4                                                         
         CLI   TAACSCR,X'A0'       FOR RELEASE SCREEN A0                        
         BNE   VCH20                                                            
         MVI   TAACEL,X'FF'        AND DELETE IT                                
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
VCHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES NO INT STATUS                                  
*                                                                               
VNOINT   NTR1                                                                   
         LA    R2,SIDNINTH                                                      
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    VNOINTX                                                          
*                                                                               
         CLI   5(R2),0             IGNORE IF EMPTY FIELD                        
         BE    VNOINTX                                                          
         TM    INVSTAT,TAINSPAY                                                 
         BZ    PAYERR2             INPUT NOT ALLOWED IF NOT PAID                
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TAIND,R4                                                         
*                                                                               
         CLI   8(R2),C'N'          NEW INPUT CAN BE NO                          
         BNE   VNOINT10                                                         
         NI    TAINSTA3,X'FF'-TAINSNI                                           
         B     VNOINT20                                                         
*                                                                               
VNOINT10 CLI   8(R2),C'Y'          NEW INPUT CAN BE YES                         
         BNE   FLDINV                                                           
         OI    TAINSTA3,TAINSNI                                                 
*                                                                               
VNOINT20 L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   VNOINT90                                                         
         USING TAPDD,R4                                                         
         NI    TAPDOPT4,X'FF'-TAPDONOI                                          
         CLI   8(R2),C'Y'                                                       
         BNE   *+8                                                              
         OI    TAPDOPT4,TAPDONOI   TURN ON NO INTERFACE                         
VNOINT90 OI    4(R2),X'20'                                                      
VNOINTX  B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SIDBDTEH            CLEAR ALL FIELDS                             
         XC    SIDERRM,SIDERRM                                                  
         OI    SIDERRMH+6,X'80'                                                 
         XC    SIDMSG,SIDMSG                                                    
         OI    SIDMSGH+1,X'0C'     MAKE ZERO INTENSITY                          
         OI    SIDMSGH+6,X'80'                                                  
         GOTO1 FLDVAL,DMCB,(X'20',SIDBDTEH),999  SET ALL PREV VALIDATED         
         MVC   SVKEY,KEY                                                        
         SPACE 1                                                                
         OI    SIDPDDTH+1,X'0C'    MAKE PREV. DUE DATE FIELDS ZERO INT.         
         OI    SIDPDDTH+6,X'80'                                                 
         OI    SIDPDDH+1,X'2C'     PROTECT DATE FIELD                           
         OI    SIDPDDH+6,X'80'                                                  
         OI    SIDDDCTH+1,X'0C'    MAKE DUE DATE CHG DATE FLDS ZERO INT         
         OI    SIDDDCTH+6,X'80'                                                 
         OI    SIDDDCH+1,X'2C'     PROTECT DATE FIELD                           
         OI    SIDDDCH+6,X'80'                                                  
*                                                                               
         OI    SIDRSSNH+1,X'0C'    MAKE RESEND FIELD ZERO INT                   
         OI    SIDRSNDH+1,X'2C'    PROTECT RESEND FIELD                         
*                                                                               
         XC    SIDBOVR,SIDBOVR                                                  
         OI    SIDBOVRH+6,X'80'                                                 
         XC    SIDBVDT,SIDBVDT                                                  
         OI    SIDBVDTH+6,X'80'                                                 
         TM    AGYSTA4,TAAYPOV     O&M PO VALIDATION?                           
         BZ    DISP3                                                            
*                                                                               
         MVI   SIDRSND,C'N'        DEFAULT NO                                   
         BRAS  RE,CHKRSND          CHECK IF INVOICE MARKED FOR RESEND           
         BNE   DISP2                                                            
         TM    KEY+TLDRSTAT-TLDRD,X'80'   KEY MARKED DELETED?                   
         BO    DISP2                                                            
         MVI   SIDRSND,C'Y'        DISPLAY AS SUCH                              
*                                                                               
DISP2    NI    SIDRSSNH+1,X'F3'    MAKE RESEND FIELD NORMAL INT                 
         NI    SIDRSNDH+1,X'D3'    UNPROTECT RESEND FIELD                       
*                                                                               
DISP3    OI    SIDRSSNH+6,X'80'                                                 
         OI    SIDRSNDH+6,X'80'                                                 
*                                                                               
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAID DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DISP5                                                            
         TM    TAPDSTA2,TAPDSSUB                                                
         BNO   DISP5                                                            
         MVC   SIDMSG(12),=CL12'(SUBSIDIARY)'                                   
         NI    SIDMSGH+1,X'F3'     TURN OFF ZERO INTENSITY                      
         OI    SIDMSGH+1,X'08'     SET HIGH INTENSITY                           
*                                                                               
DISP5    MVC   SIDODDT,SPACES                                                   
         OI    SIDODDTH+6,X'80'                                                 
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TADDELQ      GET DUE DATE ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DISP8                                                            
         USING TADDD,R4                                                         
         OC    TADDDATE,TADDDATE                                                
         BZ    DISP8                                                            
         GOTO1 DATCON,DMCB,(1,TADDDATE),(8,SIDDDTE)                             
         CLI   TADDLEN,TADDLNQ                                                  
         BL    DISP6                                                            
         OC    TADDOVRD,TADDOVRD                                                
         BZ    DISP6                                                            
         GOTO1 DATCON,DMCB,(1,TADDOVRD),(8,SIDODDT)                             
         SPACE 1                                                                
DISP6    TM    TGCTSTLV,X'C0'      IF PROGRAMMER/SYSTEM SUPERVISOR              
         BZ    DISP8                                                            
         OC    TADDPREV,TADDPREV   AND PREVIOUS DUE DATE DEFINED                
         BZ    DISP8                                                            
         NI    SIDPDDTH+1,X'F3'    MAKE PREV. DUE DATE FIELDS NORM INT.         
         NI    SIDPDDH+1,X'D3'     UNPROTECT DATE FIELD                         
         NI    SIDDDCTH+1,X'F3'    MAKE DUE DATE CHG DATE FLDS NORM INT         
         NI    SIDDDCH+1,X'D3'     UNPROTECT DATE FIELD                         
         GOTO1 DATCON,DMCB,(1,TADDPREV),(8,SIDPDD)                              
         GOTO1 (RF),(R1),(1,TADDCHNG),(8,SIDDDC)                                
         SPACE 1                                                                
*                                                                               
DISP8    MVI   ELCODE,TARAELQ      GET RATE EL.                                 
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   DISP10                                                           
         USING TARAD,R4                                                         
         MVI   RATSTAT,X'FF'                                                    
         CLI   TARASTA2,0                                                       
         BE    *+10                                                             
         MVC   SIDBOVR(5),=C'BOVER'                                             
         L     R4,AIO                                                           
         USING TAACD,R4                                                         
         MVI   ELCODE,TAACELQ      GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISP9    BAS   RE,NEXTEL                                                        
         BNE   DISP10                                                           
         CLI   TAACSCR,X'60'                                                    
         BNE   DISP9                                                            
         OC    TAACCDTE,TAACCDTE                                                
         BZ    DISP10                                                           
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,SIDBVDT)                             
*                                                                               
*                                                                               
DISP10   L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DISP160                                                          
         SPACE                                                                  
         USING TAIND,R4                                                         
         TM    TAINSTA2,TAINSPRM                                                
         BNO   DISP15                                                           
         MVC   SIDMSG(9),=CL9'(PRIMARY)'                                        
         NI    SIDMSGH+1,X'F3'     TURN OFF ZERO INTENSITY                      
         OI    SIDMSGH+1,X'08'     SET HIGH INTENSITY                           
*                                                                               
DISP15   OC    TAINBDTE,TAINBDTE                                                
         BZ    DISP20                                                           
         GOTO1 DATCON,DMCB,(1,TAINBDTE),(8,SIDBDTE) INV DATE                    
         SPACE                                                                  
DISP20   CLI   TAINLEN,X'43'       IF THIS IS NEW ELEMENT                       
         BNH   DISP70                                                           
         OC    TAINHDTE,TAINHDTE   CHECK COD PRINT DATE                         
         BZ    DISP70                                                           
         GOTO1 DATCON,DMCB,(1,TAINHDTE),(8,SIDPDTE) INV DATE                    
         SPACE                                                                  
DISP70   OC    TAINCDTE,TAINCDTE                                                
         BZ    DISP80                                                           
         GOTO1 DATCON,DMCB,(1,TAINCDTE),(8,SIDCDTE) CHECK DATE                  
         SPACE                                                                  
DISP80   OC    TAINCKRN,TAINCKRN                                                
         BZ    DISP90                                                           
         GOTO1 DATCON,DMCB,(1,TAINCKRN),(8,SIDCRUN) CHECK RUN DATE              
         SPACE                                                                  
DISP90   CLI   TAINTERR,0          IF THERE'S AN ERROR                          
         BE    DISP100                                                          
         EDIT  (1,TAINTERR),(3,SIDERRT),ALIGN=LEFT  SHOW ERROR NUMBER           
         GOTO1 ERROUT,DMCB,(TAINTERR,SIDERRM)       AND DESCRIPTION             
         BRAS  RE,DISPEPID         DISPLAY PROBLEM PID                          
         SPACE                                                                  
DISP100  BRAS  RE,DISPUSCN         DISPLAY US/CAN LINKED INVOICE                
         BRAS  RE,DISPAINF         DISPLAY ADVICE INFORMATION                   
         BRAS  RE,DISWID           DISPLAY WEB APPLICATION INFORMATION          
         BRAS  RE,DISPRINF         DISPLAY REOPEN INFORMATION                   
         BRAS  RE,STATS            SHOW STATUS                                  
         SPACE                                                                  
         LA    R2,TAINIID                                                       
         BAS   RE,GETUSRID         GET USERID                                   
         BNE   *+10                                                             
         MVC   SIDASID,TGUSERID                                                 
         LA    R2,TAINPID                                                       
         BAS   RE,GETUSRID                                                      
         BNE   *+10                                                             
         MVC   SIDPID,TGUSERID                                                  
         SPACE                                                                  
         MVC   SIDASST,TAINIST     ASSIGNER'S STAFF ID                          
         MVC   SIDPST,TAINPST      PAYER'S STAFF ID                             
         SPACE                                                                  
         OC    TAINIDTE,TAINIDTE                                                
         BZ    DISP110                                                          
         GOTO1 DATCON,DMCB,(1,TAINIDTE),(8,SIDASDA) ASSIGNMENT DATE             
         SPACE                                                                  
DISP110  OC    TAINPDTE,TAINPDTE                                                
         BZ    DISP120                                                          
         GOTO1 DATCON,DMCB,(1,TAINPDTE),(8,SIDPDA) PAYMENT DATE                 
         SPACE                                                                  
DISP120  OC    TAINITIM,TAINITIM   ASSIGNMENT TIME                              
         BZ    DISP130                                                          
         GOTO1 TIMECON,DMCB,TAINITIM,TAINIDTE,(8,SIDASTI)                       
         SPACE                                                                  
DISP130  OC    TAINPTIM,TAINPTIM   PAID TIME                                    
         BZ    DISP135                                                          
         GOTO1 TIMECON,DMCB,TAINPTIM,TAINPDTE,(8,SIDPTI)                        
         SPACE                                                                  
DISP135  OC    TAINTMCO,TAINTMCO   INVOICE HAS TIMESHEET?                       
         BZ    DISP140                                                          
         GOTOR SETTMCO             SET TIMESHEET COMM ID                        
         SPACE                                                                  
DISP140  TM    TAINSTAT,TAINSAPR   IF APPROVED                                  
         BZ    DISP160                                                          
         LA    R2,TAINQID                                                       
         BAS   RE,GETUSRID         GET USERID                                   
         BNE   *+10                                                             
         MVC   SIDAPID,TGUSERID                                                 
         MVC   SIDAPST,TAINQST     QC'S STAFF ID                                
         SPACE                                                                  
         OC    TAINQDTE,TAINQDTE                                                
         BZ    DISP150                                                          
         GOTO1 DATCON,DMCB,(1,TAINQDTE),(8,SIDAPDA) APPROVAL DATE               
         SPACE                                                                  
DISP150  OC    TAINQTIM,TAINQTIM   APPROVAL TIME                                
         BZ    DISP160                                                          
         GOTO1 TIMECON,DMCB,TAINQTIM,TAINQDTE,(8,SIDAPTI)                       
         SPACE                                                                  
         USING TAACD,R4                                                         
DISP160  L     R4,AIO                                                           
         MVI   ELCODE,TAACELQ      GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISP170  BAS   RE,NEXTEL                                                        
         BNE   DISP190                                                          
         CLI   TAACSCR,X'A0'       FROM INVOICE RELEASE SCREEN                  
         BNE   DISP170                                                          
         LA    R2,TAACID                                                        
         BAS   RE,GETUSRID         GET USERID                                   
         BNE   *+10                                                             
         MVC   SIDRID,TGUSERID                                                  
         MVC   SIDRST,TAACSTAF     QC'S STAFF ID                                
         OC    TAACCDTE,TAACCDTE                                                
         BZ    DISP180                                                          
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,SIDRDA) DATE                         
DISP180  OC    TAACCTIM,TAACCTIM                                                
         BZ    DISP190                                                          
         GOTO1 TIMECON,DMCB,TAACCTIM,TAACCDTE,(8,SIDRTI) TIME                   
         SPACE 1                                                                
DISP190  CLI   RECNUM,DI                                                        
         BE    DISP200                                                          
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,SIDLCHGH                                            
         OC    SIDLCHG,SIDLCHG                                                  
         BNZ   DISPX                                                            
         SPACE 1                                                                
         MVI   TWASCR,X'A0'                                                     
         B     DISP210                                                          
DISP200  MVI   TWASCR,X'1F'                                                     
DISP210  GOTO1 ACTVOUT,DMCB,(X'80',SIDLCHGH)                                    
         MVI   TWASCR,X'06'                                                     
DISPX    MVC   KEY,SVKEY                                                        
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         LA    R2,SINNUMIH         VALIDATE NUMBER OF INVOICES                  
         TM    1(R2),X'08'         TEST HIGH INTENSITY (NUMBER WARNING)         
         BZ    BLDR1                                                            
         NI    1(R2),X'F7'         RETURN TO NORMAL INTENSITY                   
         OI    6(R2),X'80'                                                      
         TM    4(R2),X'20'         RE-VALIDATE IF IT CHANGED                    
         BZ    BLDR1                                                            
         CLI   PFAID,13            IF OVERRIDE WARNING PFK NOT PRESSED          
         BNE   BLDR4               RE-ISSUE WARNING                             
         NI    4(R2),X'DF'         ELSE SET NOT VALIDATED                       
         B     BLDR6                    AND ACCEPT IT                           
         SPACE 1                                                                
BLDR1    TM    4(R2),X'20'         IF IT HAS CHANGED                            
         BO    BLDR6                                                            
         SPACE 1                                                                
         CLI   5(R2),0             IT'S REQUIRED                                
         BE    FLDMISS                                                          
         TM    4(R2),X'08'         AND MUST BE VALID NUMERIC                    
         BZ    FLDINV                                                           
         SPACE                                                                  
         ZIC   R1,5(R2)            CONVERT NUM OF INVOICES TO BINARY            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         MVC   PNUMINV,DUB+6       SAVE NUM OF INVOICES IN PACKED               
         CVB   R1,DUB                                                           
         STC   R1,NUMINV           SAVE NUM OF INVOICES IN BINARY               
         LTR   R1,R1                                                            
         BZ    FLDINV              ERROR IF 0                                   
         SPACE                                                                  
         CHI   R1,9                IF NUMBER > 9                                
         BNH   BLDR5                                                            
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         OI    6(R2),X'80'                                                      
BLDR4    OI    1(R2),X'08'         SET FIELD TO HIGH INTENSITY                  
         B     PFVERNUM            DISPLAY MESSAGE TO VERIFY NUMBER             
         SPACE                                                                  
BLDR5    OI    4(R2),X'20'         SET VALIDATED                                
         SPACE                                                                  
BLDR6    BAS   RE,GETINV           GET INVOICE NUMBERS TO ADD                   
         SPACE                                                                  
         XC    INVPTRS(255),INVPTRS                                             
         USING TLIND,R3                                                         
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'C0',0) BUILD THE KEY                      
         L     R3,AIO                                                           
         MVC   TLINKEY,KEY         INITIALIZE RECORD                            
         MVC   TLINLEN,DATADISP                                                 
         XC    TLINSTAT(7),TLINSTAT                                             
         SPACE                                                                  
         USING TAIND,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAINEL,TAINELQ      BUILD INVOICE STATUS ELEMENT                 
         MVI   TAINLEN,TAINLNQ                                                  
         MVC   TAINIID,TWAORIG     SAVE CONNECT USER ID NUMBER                  
         MVC   TAINIST,TGCTSTAF    AND STAFF CODE                               
         MVC   TAINIDTE,TGTODAY1   DATE INVOICE ASSIGNED                        
         TIME  DEC                                                              
         STCM  R0,14,TAINITIM      TIME INVOICE ASSIGNED                        
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
         USING TADYD,R4                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   TADYEL,TADYELQ      BUILD DUMMY ELEMENT                          
         MVI   TADYLEN,TADYLNQ                                                  
         LA    R0,3                ADD 3 TO PREVENT OVERFLOW AFTER PAY          
BLDR6C   GOTO1 ADDELEM                                                          
         BCT   R0,BLDR6C                                                        
         SPACE                                                                  
         ZIC   R0,NUMINV           R0=NUMBER OF INVOICES TO ADD                 
         LA    R4,KEY              R4=A(KEY)                                    
         B     BLDR10                                                           
         SPACE                                                                  
         USING TLDRD,R4                                                         
BLDR7    AP    PINVNUM,=P'1'       INCREMENT INVOICE NUMBER                     
         MVO   FULL(3),PINVNUM                                                  
         XC    FULL(2),ALLFF       COMPLEMENT IT                                
         MVC   TLININV+3(2),FULL   MOVE TO RECORD-SAME YR, MNTH & STAT          
         SPACE                                                                  
BLDR10   BRAS  RE,MYADDREC           ADD NEW INVOICE RECORD                     
         GOTO1 ADDPTRS,DMCB,INVPTRS  ADD PASSIVE POINTERS                       
         BCT   R0,BLDR7                                                         
         SPACE                                                                  
         B     RECADDED            DISPLAY OWN MESSAGE BEFORE XITING            
         EJECT                                                                  
*              ROUTINE GETS THE 1ST INVOICE NUMBER TO ADD AND THE NEXT          
*              INVOICE NUMBER FOR THE AGENCY                                    
*              AGENCY RECORD IS IS AIO                                          
         SPACE                                                                  
GETINV   NTR1                                                                   
         OI    SININV1H+6,X'80'    TRANSMIT ASSIGNED NUMBERS                    
         XC    SININV2,SININV2                                                  
         OI    SININV2H+6,X'80'                                                 
         OI    SINDASHH+1,X'0C'    SET "-" TO ZERO INTENSITY                    
         OI    SINDASHH+6,X'80'                                                 
         SPACE                                                                  
         LA    R2,SINAGYH                                                       
         GOTO1 CHNINV,DMCB,PNUMINV,SININV1  UPDATE AGENCY RECORD                
         BE    GETINV10                                                         
         CLI   12(R1),X'FF'        RESET AGY HAS LOWER INVOICE #?               
         BE    RESAGYER                                                         
         B     AGYERR                                                           
GETINV10 MVC   TGDUB(6),0(R1)      SAVE NEXT INVOICE NUMBER FOR AGENCY          
         MVC   PINVNUM,6(R1)       AND PACKED NUMBER OF INVOICE                 
         SPACE                                                                  
         CLI   NUMINV,1            IF ADDING MORE THAN 1 INVOICE                
         BNH   GETINX                                                           
         GOTO1 TINVCON,DMCB,TGDUB,SININV2,DATCON                                
         PACK  FULL(3),SININV2+2(4)   NEXT INVOICE NUMBER                       
         SP    FULL(3),=P'1'          - 1                                       
         UNPK  SININV2+2(4),FULL(3)   = LAST INVOICE NUMBER TO BE ADDED         
         OI    SININV2+5,X'F0'                                                  
         NI    SINDASHH+1,X'F3'    SET "-" TO NORMAL INTENSITY                  
GETINX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE INVOICE UNAPPROVAL                             
         SPACE 1                                                                
UNAPPR   NTR1                                                                   
         TM    UNAPPRST,PFTOUNAP   TEST PF13 TO APPROVE BIT ON                  
         BO    *+12                                                             
         OI    UNAPPRST,PFTOUNAP   NO, TURN IT ON                               
         B     PFUNAPPR            AND GIVE MESSAGE TO USER                     
         SPACE 1                                                                
         CLI   PFAID,13            YES, TEST IT WAS PRESSED                     
         BNE   PFUNAPPR                                                         
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B0',0) READ INVOICE RECORD                
         SPACE 1                                                                
         BRAS  RE,UNAPPIN          MARK INVOICE UNAPPROVED                      
*                                                                               
         OC    CANINV,CANINV       IS THERE ALSO A CANADIAN INVOICE?            
         BZ    UAPP5                                                            
         MVC   USINV,TGINV         SAVE US INVOICE                              
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B0',CANINV) READ INVOICE RECORD           
         SPACE 1                                                                
         BRAS  RE,UNAPPIN          MARK CANADIAN INVOICE UNAPPROVED             
         MVC   TGINV,USINV         RESTORE US INVOICE                           
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B0',0) REREAD US INVOICE RECORD           
*                                                                               
UAPP5    TM    PAYPST1,TAPDPCRD    IF THIS IS A CREDIT INVOICE                  
         BZ    UAPP10                                                           
         MVI   APPUAPP,C'U'                                                     
         BRAS  RE,GETCHK           UPDATE DUE COMPANY RECORDS                   
         DROP  R4                                                               
         SPACE 1                                                                
UAPP10   MVI   UNAPPRST,0                                                       
         B     UNAPPRVD                                                         
         EJECT                                                                  
DELETE   NTR1                                                                   
         MVC   DELPF13,SPACES                                                   
         OI    DELPF13H+6,X'80'                                                 
         SPACE 1                                                                
         TM    DELSTAT,PFTODEL     TEST PF20 TO PROCESS BIT ON                  
         BO    *+12                                                             
         OI    DELSTAT,PFTODEL     NO, TURN IT ON                               
         B     PRESSPF             AND GIVE MESSAGE TO USER                     
         SPACE 1                                                                
         CLI   PFAID,20            YES, TEST IT WAS PRESSED                     
         BNE   PRESSPF                                                          
         SPACE                                                                  
         USING TLCMD,RE                                                         
         LA    RE,KEY                                                           
         XC    TLCMKEY,TLCMKEY     GET COMMENT RECORD FOR FIRST                 
         MVI   TLCMCD,TLCMCDQ      INVOICE INTO AIO2                            
         MVC   TLCMAGY,TGAGY                                                    
         MVI   TLCMTYP,TLCMTINV                                                 
         MVC   TLCMINV,FRSTINV                                                  
         MVI   TLCMLEV,TLCMTPC                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCMKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         DROP  RE                                                               
         SPACE 1                                                                
         USING TLINPD,R3                                                        
DEL4     LA    R3,KEY              SET R3=A(KEY)                                
         XC    TLINPKEY,TLINPKEY                                                
         MVI   TLINPCD,TLINBCDQ    USE STATUS/BILLING OPEN ITEMS PTR            
         MVC   TLINBAGY,TGAGY                                                   
         MVC   TLINBINV,LASTINV    START WITH LAST INVOICE                      
DEL5     GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(TLINBINV-TLINPD),KEYSAVE  TEST RIGHT PTR W/SAME AGY          
         BNE   DELX                                                             
         CLC   TLINBINV,FRSTINV    TEST NOT PAST FIRST INVOICE                  
         BH    DELX                                                             
         MVC   MYKEY,KEY           SAVE LAST KEY FOUND                          
         TM    TLINBST2,TAINSPAY   TEST NOT PAID                                
         BO    DEL7                                                             
         SPACE                                                                  
         BRAS  RE,COMATTCH         FIND OUT IF INVOICE HAS COMMENT              
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                                                             
         SPACE                                                                  
         USING TLRCD,R4                                                         
         MVI   RDUPDATE,C'Y'         READ FOR UPDATE                            
         GOTO1 GETREC                GET THE RECORD                             
         L     R4,AIO                                                           
         GOTO1 SAVPTRS,DMCB,INVPTRS  SAVE CURRENT PASSIVE POINTERS              
         SPACE                                                                  
         CLI   COMMENT,C'L'          SKIP IF TP COMMENT TOO SHORT               
         BE    DEL7                                                             
         SPACE                                                                  
DEL6     L     R4,AIO                                                           
*        OI    TLRCSTAT,X'80'      MARK RECORD DELETED                          
         OI    TLRCSTAT,TLINSDEL                                                
         GOTO1 ACTVIN,DMCB,(X'80',0)                                            
         GOTO1 PUTREC                                                           
         SPACE                                                                  
         MVC   KEY,TLRCKEY         SET ACTIVE KEY                               
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
*        OI    TLDRSTAT,X'80'      MARK IT DELETED                              
         OI    TLDRSTAT,TLINSDEL                                                
         GOTO1 WRITE                                                            
         SPACE                                                                  
*        GOTO1 ADDPTRS,DMCB,(X'44',INVPTRS) DELETE ALL PASSIVE POINTERS         
         GOTO1 ADDPTRS,DMCB,INVPTRS                                             
         SPACE                                                                  
         USING TLCMD,RE                                                         
         CLI   COMMENT,C'N'        IF TP COMMENT WAS NOT ATTACHED               
         BNE   DEL7                                                             
         MVC   AIO,AIO2            ADD A COMMENT THAT MATCHED THE               
         L     RE,AIO              FIRST INVOICE'S COMMENT                      
         MVC   TLCMINV,TLINBINV                                                 
         GOTO1 ADDREC                                                           
         MVC   AIO,AIO1                                                         
         DROP  RE                                                               
         SPACE                                                                  
DEL7     LA    R3,MYKEY                                                         
         CLC   FRSTINV,TLINBINV    IF THIS INVOICE IS FIRST INVOICE             
         BE    DELX                THEN WE'RE DONE                              
         MVC   HALF,TLINBINV+3                                                  
         LH    R1,HALF                                                          
         LA    R1,1(R1)            ADD 1 TO INVOICE IN LAST KEY FOUND           
         STH   R1,HALF             (SIMULATE READ SEQ)                          
         MVC   TLINBINV+3(2),HALF                                               
         MVC   KEY,MYKEY           MOVE TO KEY FOR READ HIGH                    
         LA    R3,KEY              RESET R3                                     
         B     DEL5                                                             
         SPACE                                                                  
DELX     MVI   DELSTAT,0                                                        
         MVI   TGXARG,0                                                         
         B     DELETED             DISPLAY OWN MESSAGE BEFORE XITING            
         EJECT                                                                  
*              ROUTINE CALLS USERID TO VALIDATE 2 BYTE ID AT R2                 
*              SETS CC EQUAL IF VALID                                           
         SPACE                                                                  
GETUSRID NTR1                                                                   
         OC    0(2,R2),0(R2)                                                    
         BZ    NO                                                               
         XC    WORK(8),WORK                                                     
         MVC   WORK+8(2),0(R2)                                                  
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 USERVAL,DMCB,(X'A0',WORK)                                        
         MVC   AIO,AIO1            RESTORE AIO                                  
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
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
ACTERR   MVI   ERROR,INVRCACT      RECORD/ACTION COMBINATION INVALID            
         LA    R2,CONACTH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
AGYERR   MVI   ERROR,ERAGYERR      CAN'T ASSIGN NEW NUMBER - AGY ERROR          
         B     THEEND                                                           
         SPACE 1                                                                
AGYLOCK  MVI   ERROR,ERAGYLCK      ERROR - AGENCY LOCKED                        
         LA    R2,CONACTH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
NOCHANGE MVI   ERROR,ERCHKWR       CHECKS WRITTEN - NO CHANGE ALLOWED           
         B     THEEND                                                           
         SPACE 1                                                                
NOCHGCRD MVI   ERROR,ERCHGCRD      CHANGE NOT ALLOWED - CREDIT PYMNT            
         B     THEEND                                                           
         SPACE 1                                                                
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
         SPACE 1                                                                
RECADDED MVI   MYMSGNO1,11         INVOICE NUMBERS ADDED                        
         B     RECEND                                                           
PFVERNUM MVI   MYMSGNO1,16         HIT PF13 TO VERIFY # OF INVOICES REQ         
         B     INFEND                                                           
         SPACE 1                                                                
SCRNDIS  MVI   MYMSGNO1,13         PAYMENT DISPLAYED - HIT ENTER                
         TM    PAYPST1,TAPDPCHG    IF PAYMENT HAS BEEN CHANGED                  
         BZ    *+8                                                              
         MVI   MYMSGNO1,79         PYMT DISPLAYED - HIT ENTER (CHANGED)         
         B     RECEND                                                           
PFAPPR   MVI   MYMSGNO1,14         HIT PF13 TO APPROVE                          
         B     RECEND                                                           
APPRVD   MVI   MYMSGNO1,15         INVOICE APPROVED                             
         CLI   PFAID,24            TEST PRESSED PF24 TO ABORT                   
         BNE   RECEND                                                           
         MVI   ERROR,ERAABORT      APPROVAL ABORTED                             
         LA    R2,CONRECH                                                       
         B     THEEND                                                           
APPRNXT  MVI   MYMSGNO1,24         INVOICE APPROVED - HIT ENTER                 
         CLI   PFAID,24            TEST PRESSED PF24 TO ABORT                   
         BNE   RECEND                                                           
         MVI   ERROR,ERAABRTL      APPROVAL ABORTED - HIT ENTER                 
         LA    R2,CONRECH                                                       
         B     THEEND                                                           
PFUNAPPR MVI   MYMSGNO1,30         HIT PF13 TO UNAPPROVE                        
         B     RECEND                                                           
UNAPPRVD MVI   MYMSGNO1,17         INVOICE UNAPPROVED                           
         B     RECEND                                                           
PRESSPF  MVI   MYMSGNO1,56         INPUT ACCEPTED-PRESS PF20 TO DELETE          
         B     RECEND                                                           
DELETED  MVI   MYMSGNO1,65         RECORD(S) DELETED                            
         CLI   SVRECUSE,C'O'                                                    
         BNE   RECEND                                                           
         MVI   MYMSGNO1,253        PRESS PF14 FOR ADVICE COMPLETE               
         B     RECEND                                                           
         SPACE 1                                                                
PAYERR   MVI   ERROR,ERNOTPD       INVOICE NOT PAID YET                         
         B     THEEND                                                           
PAYERR2  MVI   ERROR,ERNOPAY       INVOICE HAS NOT BEEN PAID                    
         B     THEEND                                                           
APRERR   MVI   ERROR,ERAPPRVD      INVOICE ALREADY APPROVED                     
         B     THEEND                                                           
CANTAPPR MVI   ERROR,ERCANTAP      CAN'T APPROVE WHAT YOU PAY                   
         B     THEEND                                                           
UNAPRERR MVI   ERROR,ERNOTAPP      INVOICE NOT APPROVED                         
         B     THEEND                                                           
CANTUNAP MVI   ERROR,ERCANTUA      CAN'T UN-APPROVE IF BILLED OR PAID           
         B     THEEND                                                           
NOTAVSUB TM    SIDRSNDH+4,X'20'    FUNCTION NOT AVAIL FOR THIS INVOICE          
         BZ    XIT                                                              
         LA    R2,CONACTH                                                       
NOTAVAIL MVI   ERROR,ERNOTAV       FUNCTION NOT AVAIL FOR THIS INVOICE          
         B     THEEND                                                           
BADRANGE MVI   ERROR,ERBADRNG      INVALID RANGE FOR DELETING INVOICES          
         B     THEEND                                                           
RESAGYER MVC   MYMSGNO,=Y(ERMUS509) RESET AGY HAS LOWER INVOICE #               
         B     NTHEEND                                                          
SCRNLEER MVC   MYMSGNO,=Y(ERSCRNLE) SCREEN RECORDS NO LONGER EXIST              
         B     NTHEEND                                                          
NTHEEND  OI    GENSTAT2,USGETTXT   NEW THEEND FOR TWO BYTE ERROR MSGS           
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     THEEND                                                           
         SPACE 1                                                                
RECEND   LA    R2,CONRECH                                                       
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
ALLFF    DC    6X'FF'              SHOULD BE SAME LENGTH AS LONGEST             
*                                  FIELD BEING COMPLEMENTED                     
         SPACE 1                                                                
CODMSG   DC    C'(PUR',X'42',C'AGENCY)'  CAN'T USE X'40'                        
CODMSG2  DC    C'(PUR',X'42',C'CLIENT)'                                         
REOLIT   DC    CL(L'SIDRINF)'Reopened:'                                         
CANLIT   DC    CL(L'SIDRINF)'Cancelled:'                                        
         SPACE 1                                                                
FREQCARD DC    CL80'FTXXOAGENCY 0202FT 0301R 0503DDS 0609S/SNUMBER 0808X        
               INTCOMNO 0903CST 1006YYMMDD*'                                    
DELREAS  DC    C'DELETE REASON:'                                                
         SPACE 2                                                                
PFTAB    DS    0C                  TABLE TO COVER PF KEYS                       
         SPACE                                                                  
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'COMMENT ',CL8'DISPLAY'                               
PF13     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYWS,L'COMTYPI-1),AL2(COMTYPI-INVD)                       
         DC    AL1(KEYTYTWA,L'SIDINV-1),AL2(SIDINV-T702FFD)                     
         SPACE                                                                  
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'TIME    ',CL8'LIST   '                               
PF14     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'SIDINV-1),AL2(SIDINV-T702FFD)                     
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         SPACE                                                                  
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
APPPFTAB DS    0H                                                               
         DC    AL1(PF21X-*,21,PFTRPROG+PFTINT,0,0)                              
         DC    CL3' ',CL8' ',CL8' '                                             
PF21X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
DPFTAB   DS    0H                                                               
         DC    AL1(DPF13X-*,13,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8'        ',CL8'        '                               
DPF13X   EQU   *                                                                
         DC    AL1(DPF14X-*,14,0,0,0)                                           
         DC    CL3' ',CL8'ADVICE  ',CL8'COMPLETE'                               
DPF14X   EQU   *                                                                
         DC    AL1(DPF20X-*,20,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8'        ',CL8'        '                               
DPF20X   EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
DAPFTAB  DS    0C                  TABLE TO ADD COMMENT FROM INV/DEL            
         DC    AL1(DAPF13X-*,13,0,(DAPF13X-DAPF13)/KEYLNQ,0)                    
         DC    CL3'  ',CL8'COMMENT ',CL8'ADD    '                               
DAPF13   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYWS,L'COMTYPI-1),AL2(COMTYPI-INVD)                       
         DC    AL1(KEYTYWS,L'DINV-1),AL2(DINV-INVD)                             
         DC    AL1(0,0),AL2(0)                                                  
         DC    AL1(0,0),AL2(0)                                                  
         DC    AL1(0,0),AL2(0)                                                  
         DC    AL1(KEYTYWS,L'PFDR-1),AL2(PFDR-INVD)                             
DAPF13X  EQU   *                                                                
         DC    AL1(DPF23X-*,23,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8'        ',CL8'        '                               
DPF23X   EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
DCPFTAB  DS    0C                  TABLE TO CHG COMMENT FROM INV/DEL            
         DC    AL1(DCPF13X-*,13,0,(DCPF13X-DCPF13)/KEYLNQ,0)                    
         DC    CL3'  ',CL8'COMMENT ',CL8'CHANGE '                               
DCPF13   DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYWS,L'COMTYPI-1),AL2(COMTYPI-INVD)                       
         DC    AL1(KEYTYWS,L'DINV-1),AL2(DINV-INVD)                             
DCPF13X  EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE TO VALIDATE APPROVE/UNAPPROVE KEY FIELDS                 
*                                                                               
         SPACE 1                                                                
APUAVKEY NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,PREAPDAT         PRESENT APPROVE DATE FIELD                   
*                                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',APPAGYH)  VALIDATE AGENCY             
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY EL                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         USING TAAYD,R4                                                         
         MVC   AGYSTAT,TAAYSTAT    SAVE AGENCY STATUS                           
         TM    AGYSTAT,TAAYSCOD                                                 
         BZ    *+10                                                             
         MVC   CONHED2(12),CODMSG  DISPLAY COD MSG IF NECESSARY                 
         SPACE 1                                                                
         LA    R2,APPINVH          VALIDATE INVOICE NUMBER                      
         GOTO1 ANY                                                              
         MVC   DINV,WORK                                                        
         GOTO1 TINVCON,DMCB,8(R2),INV,DATCON  CVT TO INTERNAL FORMAT            
         MVC   SCRINV,INV                     SAVE AS SCREEN REC INV #          
         MVC   TGINV,INV                      MOVE TO GLOBAL STORAGE            
         MVC   MYINV,INV                      (SAVE UNCOMPLEMENTED)             
         XC    TGINV,ALLFF                    AND COMPLEMENT IT                 
                                                                                
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',0) VALIDATE/READ INVOICE REC.         
         BNE   NTFOUND                                                          
                                                                                
         USING TACOD,R4                                                         
         CLI   ACTNUM,ACTAPP       IF ACTION IS APPROVE                         
         BNE   APPUA00                                                          
         TM    TGAYSTA7,TAAYSPPL   AND THIS IS A P+ AGENCY                      
         BZ    APPUA00                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   APPUA00                                                          
         CLI   TACOMED,TACOMEDE    PAYMENT MUST BE TO AN EVENT                  
         BNE   ERPPLSI                                                          
         DROP  R4                                                               
                                                                                
APPUA00  MVI   RATSTAT,0                                                        
         MVI   ELCODE,TARAELQ      GET RATE EL.                                 
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   APPUA1                                                           
         USING TARAD,R4                                                         
         MVI   RATSTAT,X'FF'                                                    
                                                                                
APPUA0   MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS EL.                      
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   APPUA1                                                           
         USING TAPDD,R4                                                         
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',TAPDCLI) GET CLIENT RECORD            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         MVI   CLTSTAT,0                                                        
         MVI   ELCODE,TACIELQ     GET CLIENT INFO ELEMENT                       
         L     R4,AIO3                                                          
         BRAS  RE,GETEL                                                         
         BNE   APPUA1                                                           
         USING TACID,R4                                                         
         MVC   CLTSTAT,TACISTAT                                                 
         TM    CLTSTAT,TACISCOD                                                 
         BZ    *+10                                                             
         MVC   CONHED2(12),CODMSG2 DISPLAY COD MSG IF NECESSARY                 
         DROP  R4                                                               
         SPACE 1                                                                
APPUA1   MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',0) VALIDATE/READ INVOICE REC.         
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         USING TAFND,R1                                                         
         CLI   ACTNUM,ACTUNAPP     IF ACTION UNAPPROVE                          
         BNE   APPUA5                                                           
         MVI   ELCODE,TAFNELQ      PAYMENT CANNOT HAVE COME FROM CERNO          
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BNE   APPUA5                                                           
         L     R1,TGELEM                                                        
         CLC   =C'HF',TAFNNAME                                                  
         BE    NOTAVAI2                                                         
         DROP  R1                                                               
         SPACE 1                                                                
APPUA5   MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS EL.                   
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         USING TACOD,R4                                                         
         MVC   TGCID,TACOCID       SAVE COMMERCIAL ID                           
         SPACE 1                                                                
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS EL.                       
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4                                                         
         TM    TAINSTAT,TAINSPAY   INSURE INVOICE HAS BEEN PAID                 
         BZ    PAYERR3                                                          
         CLI   ACTNUM,ACTAPP       IF ACTION IS APPROVE                         
         BNE   *+12                                                             
         TM    TAINSTAT,TAINSERR   INSURE NOT IN ERROR                          
         BO    NOTAVAI2                                                         
         MVC   SVSTA2,TAINSTA2     SAVE TAINSTA2                                
*                                                                               
         XC    CANINV,CANINV                                                    
         XC    USINV,USINV                                                      
         L     R4,AIO                                                           
         MVI   ELCODE,TAUCELQ      GET US/CANADIAN INVOICE ELEMENT              
         BRAS  RE,GETEL                                                         
         BNE   APUA10                                                           
         USING TAUCD,R4                                                         
         CLC   TGINV,TAUCINU       MUST BE US INVOICE                           
         BNE   NOTAVAI2                                                         
         MVC   CANINV,TAUCINC      SAVE CANADIAN INVOICE NUMBER                 
         MVC   USINV,TGINV         SAVE US INVOICE NUMBER                       
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',TAUCINC)  READ CAN INV REC            
         MVC   TGINV,USINV         RESTORE US INVOICE NUMBER                    
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS EL.                       
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4                                                         
         TM    TAINSTAT,TAINSPAY   INSURE INVOICE HAS BEEN PAID                 
         BZ    PAYERR3                                                          
         CLI   ACTNUM,ACTAPP       IF ACTION IS APPROVE                         
         BNE   *+12                                                             
         TM    TAINSTAT,TAINSERR   INSURE NOT IN ERROR                          
         BO    NOTAVAI2                                                         
*                                                                               
APUA10   MVC   AIO,AIO1                                                         
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS EL.                       
         L     R4,AIO              R4 --> US INVOICE                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         XIT1  REGS=(R4)           RETURN A(INVOICE ELEMENT)                    
*                                                                               
PAYERR3  MVI   ERROR,ERNOTPD       INVOICE NOT PAID YET                         
         B     THEEND2                                                          
*                                                                               
NOTAVAI2 MVI   ERROR,ERNOTAV       FUNCTION NOT AVAIL FOR THIS INVOICE          
         B     THEEND2                                                          
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA) RECORD / ACTION INVALID FOR P+              
         B     NTHEEND                                                          
                                                                                
THEEND2  GOTO1 EXIT,DMCB,0                                                      
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        ROUTINE MARKS INVOICE RECORD APPROVED                                  
*                                                                               
APPRINV  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS EL.                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4                                                         
         OI    TAINSTAT,TAINSAPR   SET APPROVED                                 
         MVC   TAINQID,TWAORIG     SAVE CONNECT USER ID NUMBER                  
         MVC   TAINQST,TGCTSTAF    AND STAFF CODE                               
         MVC   TAINQDTE,TGTODAY1   QC DATE                                      
         OC    SVADATE,SVADATE                                                  
         BZ    *+10                                                             
         MVC   TAINQDTE,SVADATE    SET AS APPROVE DATE                          
         TIME  DEC                                                              
         STCM  R0,14,TAINQTIM      AND TIME                                     
         MVC   TIME,TAINQTIM                                                    
         GOTO1 PUTREC                                                           
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLINBCDQ,(X'14',0) READ STATUS POINTER               
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         OI    TLDRSTAT+1,TAINSAPR SET APPROVED                                 
         GOTO1 WRITE               AND WRITE IT BACK                            
         DROP  R4                                                               
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        ROUTINE MARKS INVOICE RECORD UNAPPROVED                                
*                                                                               
UNAPPIN  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS EL.                       
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4                                                         
         NI    TAINSTAT,ALL-TAINSAPR  TURN OFF APPROVED BIT                     
         MVC   TAINQID,TWAORIG     SAVE CONNECT USER ID NUMBER                  
         MVC   TAINQST,TGCTSTAF    AND STAFF CODE                               
         MVC   TAINQDTE,TGTODAY1   DATE                                         
         TIME  DEC                                                              
         STCM  R0,14,TAINQTIM      AND TIME                                     
         SPACE 1                                                                
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS EL.                      
         L     R4,AIO                                                           
         BRAS  RE,GETEL            IF NOT FOUND                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         MVC   PAYPST1,TAPDPST1    SAVE PAYMENT STATUS BYTES                    
         MVC   PAYPST2,TAPDPST2                                                 
         GOTO1 PUTREC                                                           
         SPACE 1                                                                
         TM    TAPDOPT3,TAPDODUM   IF DUMMY PAYMENT                             
         BO    UNAPPX              NO STATUS PTR, SO SKIP READ                  
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLINBCDQ,(X'14',0) READ STATUS POINTER               
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         NI    TLDRSTAT+1,ALL-TAINSAPR        TURN OFF APPROVED BIT             
         GOTO1 WRITE                          AND WRITE IT BACK                 
UNAPPX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE GETS AGENCY STATUS                                       
*                                                                               
GTAGYST  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY EL                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         USING TAAYD,R4                                                         
         MVC   AGYSTAT,TAAYSTAT    SAVE AGENCY STATUS                           
         MVC   AGYSTA4,TAAYSTA4                                                 
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE ADDS RECORDS (OR WRITES OVER DELETED)                    
         SPACE 1                                                                
MYADDREC NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO              R4=A(RECORD)                                 
         USING TLRCD,R4                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY              R3=A(KEY)                                    
         USING TLDRD,R3                                                         
         MVC   TLDRKEY,TLRCKEY     SET ACTIVE KEY                               
         MVC   TLDRSTAT,TLRCSTAT   AND STATUS FROM RECORD                       
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                LOOK FOR RECORD ALREADY ON FILE              
         SPACE 1                                                                
         CLC   TLDRKEY,KEYSAVE     TEST WE FOUND RECORD                         
         BNE   MYADR4                                                           
         TM    TLDRSTAT,X'80'      IT HAD BETTER BE DELETED                     
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   TLDRKEY,KEYSAVE     RESTORE ORIGINAL KEY                         
         MVC   TLDRSTAT,KEYSAVE+TLDRSTAT-TLDRD  AND STATUS                      
         GOTO1 WRITE               AND WRITE IT BACK                            
         SPACE 1                                                                
         MVC   AIO,AIO3            NOW SET TO GET THE RECORD                    
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD SO THAT WE CAN ...            
         ST    R4,AIO                                                           
         GOTO1 PUTREC              WRITE NEW ONE BACK OVER DELETED ONE          
         B     MYADRX                                                           
         SPACE 1                                                                
MYADR4   MVC   KEY,KEYSAVE                                                      
         GOTO1 ADDREC              ADD FILE RECORD                              
         SPACE 1                                                                
MYADRX   NI    DMINBTS,X'F7'                                                    
         XIT1                                                                   
         DROP  R3,R4                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO EXTRACT GUARANTEE TRACKING REQUESTS                   
         SPACE 1                                                                
         USING TAGQD,R4            R4=A(REQ. DETAILS ELEMENT)                   
SETGQEXT NTR1  BASE=*,LABEL=*                                                   
         ZIC   R2,TAGQNUM          R2=N'SUB-ELEMENTS                            
         LTR   R2,R2                                                            
         BZ    SETGQX                                                           
         GOTO1 GQEXT,DMCB,((R2),TAGQSBEL)  ADD REQUEST TO REQUEST FILE          
SETGQX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO EXTRACT FIXED CYCLE TRACKING REQUESTS                 
         SPACE 1                                                                
         USING TAFQD,R4            R4=A(REQ. DETAILS ELEMENT)                   
FQEXT    NTR1  BASE=*,LABEL=*                                                   
         ZIC   R2,TAFQNUM          R2=N'SUB-ELEMENTS                            
         LTR   R2,R2                                                            
         BZ    FQX                                                              
         LA    R4,TAFQSBEL         R4=A(FIRST SUB-ELEMENT)                      
         USING TAFQSBEL,R4                                                      
         LA    R3,BLOCK            R3=A(REQUEST RECORD)                         
         USING REQD,R3                                                          
         SPACE 1                                                                
FQ2      MVC   REQUEST,FREQCARD    INITIALIZE REQUEST CARD                      
         SPACE 1                                                                
         MVC   TGOFF,TAFQOFF       SET TP OFFICE                                
         SPACE 1                                                                
         L     R1,TAFQSSN          CVT SSN TO EBCDIC                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REQUEST+37(9),DUB+3(5)                                           
         SPACE 1                                                                
         GOTO1 HEXOUT,DMCB,TGCOM,REQUEST+51,4,0  INTERNAL COMML NO.             
         SPACE 1                                                                
         GOTO1 HEXOUT,DMCB,TAFQCAST,DUB,2,0  CAST INPUT SEQUENCE NO.            
         MVC   REQUEST+64(3),DUB+1                                              
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(1,TAFQSTRT),(0,REQUEST+72)  CYCLE START             
         SPACE 1                                                                
         BRAS  RE,ADDREQ           ADD REQUEST TO REQUEST FILE                  
         SPACE 1                                                                
         LA    R4,L'TAFQSBEL(R4)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R2,FQ2                                                           
         SPACE 1                                                                
FQX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE VALIDATES APPROVAL DATE                                  
         SPACE 1                                                                
VALADTE  NTR1  BASE=*,LABEL*                                                    
* ONLY VALIDATE THE APPROVAL DATE IF WE HAVE THE CORRECT SCREEN                 
* LOADED                                                                        
         CLC   =C'Approve Date',APPHAPP                                         
         BNE   VADTEX                                                           
         XC    SVADATE,SVADATE                                                  
         LA    R2,APPAPPH                                                       
         CLI   5(R2),0             TEST EMPTY FIELD                             
         BE    VADTEX                                                           
         GOTO1 DTVAL,DMCB,SVADATE                                               
VADTEX   B     XIT                                                              
         DROP  R4                                                               
         SPACE 3                                                                
*              ROUTINE VALIDATES CHECK DATE                                     
         SPACE 1                                                                
         USING TAIND,R4            R4=A(INVOICE STATUS ELEMENT)                 
VALCDTE  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SIDCDTEH                                                      
         CLI   5(R2),0             IF FIELD EMPTY                               
         BNE   VCDTE10                                                          
         XC    TAINCDTE,TAINCDTE   CLEAR DATE                                   
         NI    TAINSTAT,ALL-TAINSCHK AND STATUS                                 
         B     VCDTEX                                                           
         SPACE 1                                                                
VCDTE10  GOTO1 DTVAL,DMCB,TAINCDTE ELSE VALIDATE/SET DATE                       
         OI    TAINSTAT,TAINSCHK   SET STATUS TOO                               
VCDTEX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TESTS WHETHER DELETE ALLOWED FOR AN INVOICE              
*                             P1 BYTE 0  X'80'=SKIP COMMENT SEARCH              
         SPACE 1                                                                
CHKDEL   NTR1  BASE=*,LABEL=*                                                   
         MVC   TGBYTE,0(R1)                                                     
         MVC   DINV,8(R2)                                                       
         MVC   MYKEY,KEY                                                        
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TAIND,R4                                                         
         TM    TAINSTAT,TAINSPAY                                                
         BO    NODELETE                                                         
         SPACE 1                                                                
         BRAS  RE,TSATTCH          IF TIMESHEET ATTACHED, CAN'T DELETE          
         DROP  R4                                                               
         SPACE 1                                                                
         MVC   PFDR,DELREAS        SET UP PF KEY FLIPS                          
         MVI   COMTYPI,TLCMTINV                                                 
         SPACE 1                                                                
         BRAS  RE,COMATTCH         SEE IF TP COMMENT IS ATTACHED                
         SPACE 1                                                                
         CLI   COMMENT,C'N'        IF NO COMMENT ATTCHED                        
         BNE   CD10                                                             
         TM    TGBYTE,X'80'        AND WE'RE FORCING COMMENT TO                 
         BZ    NODELET2            BE ATTACHED, RETURN ERROR                    
         SPACE 1                                                                
CD10     CLI   COMMENT,C'L'        IF COMMENT IS TOO SHORT                      
         BE    NODELET3            RETURN ERROR                                 
         CLI   TGXARG,23                                                        
         BE    CDX                                                              
         L     R4,AIO                                                           
         MVI   ELCODE,TARAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   CDX                                                              
         CLI   PFAID,23                                                         
         BNE   NODELBOV                                                         
         MVC   TGXARG,PFAID                                                     
CDX      XIT1                                                                   
         SPACE 2                                                                
NODELETE MVI   ERROR,ERINVDEL      CAN'T DELETE                                 
         B     DELXIT2                                                          
         SPACE 2                                                                
NODELET2 MVC   DELPF13,=CL20'PF13=Comment/Add'                                  
         OI    DELPF13H+6,X'80'                                                 
         GOTO1 INITIAL,DMCB,DAPFTAB                                             
         MVC   MYMSGNO,=Y(ERINVCMR) INVOICE COMMENT REQUIRED                    
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     DELXIT                                                           
         SPACE 1                                                                
NODELET3 MVC   DELPF13,=CL20'PF13=Comment/Change'                               
         OI    DELPF13H+6,X'80'                                                 
         GOTO1 INITIAL,DMCB,DCPFTAB                                             
         MVC   MYMSGNO,=Y(ERINVCML) INVOICE COMMENT NOT LONG ENOUGH             
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     DELXIT                                                           
         SPACE 1                                                                
NODELBOV MVC   MYMSGNO,=Y(ERRDELBO) BOVER ATTACHED,PRESS PF23 TO CONT.          
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     DELXIT                                                           
         SPACE 2                                                                
DELXIT   OI    GENSTAT2,USGETTXT                                                
DELXIT2  GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD A REQUEST TO REQUEST FILE                         
         SPACE 1                                                                
         USING REQD,R3             R3=A(REQUEST RECORD)                         
ADDREQ   NTR1  BASE=*,LABEL=*                                                   
         XC    REQHDR,REQHDR       CLEAR HEADER                                 
         SPACE 1                                                                
         MVC   REQUEST+2(2),TGCTALPH  CONNECT ALPHA USER ID                     
         MVC   REQUEST+4(1),TGOFF     TP OFFICE                                 
         MVC   REQUEST+5(6),TGAGY     AGENCY                                    
         SPACE 1                                                                
         OC    TGOFF,TGOFF                                                      
         BNZ   *+6                                                              
         DC    H'0'                DIE IF NO OFFICE                             
         SPACE 1                                                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMADD '),REQFILE,(R3),(R3)                    
         CLI   8(R1),0                                                          
         BE    ADDREQX                                                          
         DC    H'0'                                                             
ADDREQX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES THAT AN INVOICE WITH ACTIVE TIMESHEETS       *         
*        CANNOT BE DELETED                                            *         
*        ON ENTRY ... AIO1 = A(INVOICE RECORD)                        *         
*                     R4   = A(INVOICE DETAILS ELEMENT)               *         
***********************************************************************         
                                                                                
         USING TAIND,R4                                                         
TSATTCH  NTR1  BASE=*,LABEL=*                                                   
         OC    TAINTMCO,TAINTMCO   IF THERE IS A TIMESHEET ATTACHED             
         JZ    XIT                                                              
                                                                                
         USING TLTMD,R3                                                         
         LA    R3,KEY              LOOK FOR ANY TIMESHEET KEYS                  
         XC    KEY,KEY             ATTACHED TO THIS INVOICE ...                 
         MVI   TLTMCD,TLTMCDQ                                                   
         MVC   TLTMCOM,TAINTMCO                                                 
         DROP  R4                                                               
                                                                                
         USING TLIND,R4                                                         
         L     R4,AIO1                                                          
         MVC   TLTMINV,TLININV                                                  
         DROP  R4                                                               
                                                                                
         GOTO1 HIGH                                                             
         J     TSA20                                                            
TSA10    GOTO1 SEQ                                                              
TSA20    CLC   KEY(TLTMSSN-TLTMD),KEYSAVE                                       
         JNE   XIT                                                              
                                                                                
         MVC   SVTMKEY,KEY         ... IF ANY TIMESHEET KEYS ARE                
         DROP  R3                  FOUND ...                                    
                                                                                
         USING TLCAD,R3                                                         
         XC    KEY,KEY             ... RETURN ERROR IF THERE IS STILL           
         MVI   TLCACD,TLCACDQ      A CAST KEY WITH MATCHING SORT KEY            
         MVC   TLCACOM,SVTMKEY+TLTMCOM-TLTMD                                    
         MVC   TLCASORT,SVTMKEY+TLTMSORT-TLTMD                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLCASSN-TLCAD),KEYSAVE                                       
         JE    TSANDEL                                                          
         DROP  R3                                                               
                                                                                
         MVC   KEY,SVTMKEY         ... IF NO MATCHING CAST KEY, GO              
         GOTO1 HIGH                READ NEXT TIMESHEET                          
         J     TSA10                                                            
                                                                                
TSANDEL  MVC   MYMSGNO,=Y(ERRATTIM)                                             
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         J     DELXIT                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
                                                                                
*              ROUTINE DETERMINES IF INVOICE HAS TP COMMENT WITH LENGTH         
*              OF, AT LEAST, 25 CHARACTERS ATTACHED                             
         SPACE                                                                  
COMATTCH NTR1  BASE=*,LABEL=*                                                   
         MVI   COMMENT,C'N'        INITIALIZE COMMENT INDICATOR TO NO           
         SPACE 1                                                                
         CLI   MYKEY,TLINCDQ                                                    
         BNE   CATCH10                                                          
         USING TLCMD,RE                                                         
         USING TLIND,R3                                                         
         LA    RE,KEY              RE=A(COMMENT KEY TO BUILD)                   
         LA    R3,MYKEY            R3=A(CURRENT INVOICE POINTER)                
         XC    TLCMKEY,TLCMKEY                                                  
         MVI   TLCMCD,TLCMCDQ      BUILD COMMENT KEY                            
         MVC   TLCMAGY,TLINAGY                                                  
         MVI   TLCMTYP,TLCMTINV                                                 
         MVC   TLCMINV,TLININV                                                  
         MVI   TLCMLEV,TLCMTPC                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCMKEY),KEYSAVE                                           
         BNE   CATCHX                                                           
         B     CATCH20                                                          
         DROP  RE,R3                                                            
         SPACE 1                                                                
CATCH10  CLI   MYKEY,TLINBCDQ                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TLCMD,RE                                                         
         USING TLINPD,R3                                                        
         LA    RE,KEY              RE=A(COMMENT KEY TO BUILD)                   
         LA    R3,MYKEY            R3=A(CURRENT INVOICE POINTER)                
         XC    TLCMKEY,TLCMKEY                                                  
         MVI   TLCMCD,TLCMCDQ      BUILD COMMENT KEY                            
         MVC   TLCMAGY,TLINBAGY                                                 
         MVI   TLCMTYP,TLCMTINV                                                 
         MVC   TLCMINV,TLINBINV                                                 
         MVI   TLCMLEV,TLCMTPC                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCMKEY),KEYSAVE                                           
         BNE   CATCHX                                                           
         DROP  RE,R3                                                            
         SPACE 1                                                                
CATCH20  MVC   AIO,AIO2                                                         
         GOTO1 GETREC              GET COMMENT RECORD                           
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         USING TLRCD,RE                                                         
         L     RE,AIO2                                                          
         MVI   COMMENT,C'L'                                                     
         CLC   TLRCLEN,=H'97'      IF AT LEAST 25 CHARACTERS LONG               
         BL    CATCHX              SET COMMENT INDICATOR TO YES                 
         MVI   COMMENT,C'Y'                                                     
         DROP  RE                                                               
CATCHX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY PID OF PERFORMER WHO SENT                     
*              INVOICE INTO ERROR                                               
         SPACE 1                                                                
DISPEPID NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUEPID))                                     
         BNE   DEPX                                                             
         SPACE 1                                                                
         USING TANUD,R4                                                         
         L     R4,TGELEM                                                        
         MVC   SIDERRM+37(L'TGPID),TANUMBER                                     
         OC    SIDERRM,SPACES                                                   
         GOTO1 SQUASHER,DMCB,SIDERRM,L'SIDERRM                                  
         DROP  R4                                                               
         SPACE 1                                                                
DEPX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY US/CAN LINKED INVOICE                         
         SPACE 1                                                                
DISPUSCN NTR1  BASE=*,LABEL=*                                                   
         XC    SIDUSCN,SIDUSCN                                                  
         OI    SIDUSCNH+1,X'0C'    MAKE ZERO INTENSITY                          
         OI    SIDUSCNH+6,X'80'                                                 
*                                                                               
         USING TAUCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAUCELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DUSCNX                                                           
         MVC   SIDUSCN(2),=C'U$'                                                
         MVC   USCANINV,TAUCINU    US INVOICE                                   
         CLC   USCANINV,TGINV      INVOICE OR LINKED INVOICE?                   
         BNE   DUSCN10                                                          
         MVC   SIDUSCN(2),=C'C$'                                                
         MVC   USCANINV,TAUCINC    CAN INVOICE                                  
         DROP  R4                                                               
*                                                                               
DUSCN10  XC    USCANINV,=6X'FF'    UNCOMPLEMENT                                 
         GOTO1 TINVCON,DMCB,USCANINV,SIDUSCN+2,DATCON                           
         NI    SIDUSCNH+1,X'F3'    TURN OFF ZERO INTENSITY                      
         OI    SIDUSCNH+1,X'08'    SET HIGH INTENSITY                           
DUSCNX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE TO DISPLAY ADVICE ASSIGNED INFO IF AROUND                
         SPACE 1                                                                
DISPAINF NTR1  BASE=*,LABEL=*                                                   
         MVC   SIDAHED,SPACES                                                   
         OI    SIDAHEDH+1,X'0C'    MAKE ADVICE ASSIGNED FLDS ZERO INT           
         OI    SIDAHEDH+6,X'80'                                                 
         MVC   SIDAINF,SPACES                                                   
         OI    SIDAINFH+1,X'2C'                                                 
         OI    SIDAINFH+6,X'80'                                                 
*                                                                               
         USING TAAID,R4                                                         
         L     R4,AIO              IF ADVICE/INVOICE ASSIGN ELEMENT             
         MVI   ELCODE,TAAIELQ      EXISTS, MOVE INFO TO SCREEN                  
         BRAS  RE,GETEL                                                         
         BNE   DSPAINFX                                                         
         MVC   SIDAHED(L'AINVHEAD),AINVHEAD                                     
         MVC   SIDAINF(L'TAAIACID),TAAIACID                                     
*                                                                               
         LA    RE,SIDAINF+L'SIDAINF-1                                           
DAINF10  CLI   0(RE),C' '                                                       
         BNE   DAINF20                                                          
         AHI   RE,-1                                                            
         B     DAINF10                                                          
*                                                                               
DAINF20  MVI   1(RE),C','                                                       
         MVC   2(L'TAAIAADV,RE),TAAIAADV                                        
*                                                                               
         MVC   L'TAAIAADV+3(7,RE),=C'(REUSE)'                                   
         TM    TAAIAPTY,TAAIAMUS                                                
         BZ    *+10                                                             
         MVC   L'TAAIAADV+3(7,RE),=C'(MUSIC)'                                   
         DROP  R4                                                               
*                                                                               
         NI    SIDAINFH+1,X'F3'    TURN OFF ZERO INTENSITY FOR DATA             
         NI    SIDAHEDH+1,X'F3'    TURN OFF ZERO INTENSITY FOR HEADER           
DSPAINFX XIT1                                                                   
         SPACE 2                                                                
AINVHEAD DC    CL19'Assigned Advice:'                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE GETS FIXED CYCLE TRACKING ELEMENT                        
         SPACE 1                                                                
*                                  R4=A(SCREEN RECORD)                          
GTFQEL   NTR1  BASE=*,LABEL=*                                                   
         CLI   ELTAFQ,TAFQELQ      TEST WE'VE ALREADY BEEN HERE                 
         BE    GTFQX                                                            
         MVI   ELTAFQ,TAFQELQ      SET WE'VE BEEN HERE                          
         SPACE 1                                                                
         MVI   ELCODE,TAFQELQ      SET TO GET ELEMENT                           
         BRAS  RE,GETEL                                                         
         BNE   GTFQX                                                            
         MVC   ELTAFQ,0(R4)                                                     
         BRAS  RE,NEXTEL           MAY HAVE TWO                                 
         BNE   GTFQX                                                            
         MVC   ELTAFQ2,0(R4)                                                    
GTFQX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE GETS GUARANTEE TRACKING ELEMENT                          
         SPACE 1                                                                
*                                  R4=A(SCREEN RECORD)                          
GTGQEL   NTR1  BASE=*,LABEL=*                                                   
         CLI   ELTAGQ,TAGQELQ      TEST WE'VE ALREADY BEEN HERE                 
         BE    GTGQX                                                            
         MVI   ELTAGQ,TAGQELQ      SET WE'VE BEEN HERE                          
         SPACE 1                                                                
         MVI   ELCODE,TAGQELQ      SET TO GET ELEMENT                           
         BRAS  RE,GETEL                                                         
         BNE   GTGQX                                                            
         MVC   ELTAGQ,0(R4)                                                     
         BRAS  RE,NEXTEL           MAY HAVE TWO                                 
         BNE   GTGQX                                                            
         MVC   ELTAGQ2,0(R4)                                                    
GTGQX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE DISPLAYS THE STATUS BYTES IN INVOICE STATUS              
*              ELEMENT AT R4                                                    
         SPACE                                                                  
         USING TAIND,R4                                                         
STATS    NTR1  BASE=*,LABEL=*                                                   
         TM    TAINSTAT,TAINSPAY   DON'T BOTHER IF NOT PAID                     
         BZ    STATSX                                                           
         MVI   SIDRETR,C'N'                                                     
         TM    TAINSTA2,TAINSRTH                                                
         BZ    *+8                                                              
         MVI   SIDRETR,C'Y'                                                     
         MVI   SIDSKIP,C'N'                                                     
         TM    TAINSTA3,TAINSRSK                                                
         BZ    *+8                                                              
         MVI   SIDSKIP,C'Y'                                                     
         MVI   SIDHOLD,C'N'                                                     
         TM    TAINSTAT,TAINSHLD                                                
         BZ    *+8                                                              
         MVI   SIDHOLD,C'Y'                                                     
         OI    SIDHOLDH+4,X'20'                                                 
         MVI   SIDURG,C'N'                                                      
         TM    TAINSTA2,TAINSURG                                                
         BZ    *+8                                                              
         MVI   SIDURG,C'Y'                                                      
         MVI   SIDNINT,C'N'                                                     
         TM    TAINSTA3,TAINSNI                                                 
         BZ    *+8                                                              
         MVI   SIDNINT,C'Y'                                                     
         MVI   SIDCANC,C'N'                                                     
         TM    TAINSTAT,TAINSCAN                                                
         BZ    STATSX                                                           
         MVI   SIDCANC,C'Y'                                                     
STATSX   XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY REOPEN/CANCELLED INFO IF AROUND               
*              R4=A(TAINEL)                                                     
         SPACE 1                                                                
         USING TAIND,R4                                                         
DISPRINF NTR1  BASE=*,LABEL=*                                                   
         OI    SIDRINFH+1,X'0C'    MAKE REOP/CANCEL INFO FLDS ZERO INT          
         OI    SIDRINFH+6,X'80'                                                 
         XC    SIDRCHG,SIDRCHG                                                  
         OI    SIDRCHGH+1,X'0C'                                                 
         OI    SIDRCHGH+6,X'80'                                                 
*                                                                               
         ZIC   R3,TWASCR           R3=A(CURRENT SCREEN NUMBER)                  
         MVI   TWASCR,SCR44        SET REOPEN/CANCEL SCREEN NUMBER              
         GOTO1 ACTVOUT,DMCB,(X'80',SIDRCHGH)                                    
         STC   R3,TWASCR           RESTORE SCREEN NUMBER                        
*                                                                               
         OC    SIDRCHG,SIDRCHG     IF REOPEN/CANCEL INFO DISPLAYED              
         BZ    DSPRINFX                                                         
         NI    SIDRCHGH+1,X'F3'    TURN OFF ZERO INTENSITY FOR DATA             
         NI    SIDRINFH+1,X'F3'    TURN OFF ZERO INTENSITY FOR HEADER           
*                                                                               
         MVC   SIDRINF,REOLIT              DISPLAY "REOPENED:" HEADER           
         TM    TAINSTAT,TAINSCIN+TAINSCAN  IF CANCEL INVOICE                    
         BZ    DSPRINFX                                                         
         MVC   SIDRINF,CANLIT              DISPLAY "CANCELLED:" HEADER          
DSPRINFX XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET UP COMMENT SCREEN IF PF13 HIT                     
         SPACE 1                                                                
PFCMNT   NTR1  BASE=*,LABEL=*                                                   
         CLI   PFAID,13                                                         
         BNE   PFCMNTX                                                          
         MVI   COMTYPI,TLCMTINV    COMMENT RECORD TYPE C (PF15)                 
         MVC   SVRECUSE,RECNUM     SAVE WHERE WE CAME FROM                      
PFCMNTX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET GLOBAL COMM ID IF INVOICE HAS TIMESHEET           
         SPACE 1                                                                
         USING TAIND,R4                                                         
SETTMCO  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TAINTMCO)                            
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      COMMERCIAL DETAILS ELEMENT                   
         BRAS  RE,GETEL                                                         
         BNE   STMCOX                                                           
         USING TACOD,R4                                                         
         MVC   TGCID,TACOCID       COMMERCIAL ID                                
STMCOX   MVC   AIO,AIO1                                                         
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              ROUTINE CHECKS IF INVOICE IS TO BE RE-SENT                       
*======================================================================         
CHKRSND  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLRID,R4                                                         
         MVI   TLRICD,TLRICDQ                                                   
         MVI   TLRISCD,TLRISCDQ                                                 
         MVC   TLRIAGY,TGAGY                                                    
         MVC   TLRIINV,TGINV                                                    
         XC    TLRIINV,=X'FFFFFFFFFFFF'                                         
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         LA    R3,1                DEFAULT, NOT A RESEND                        
         CLC   TLRIKEY,KEYSAVE     DID WE FIND ONE?                             
         BNE   CHKRSND5                                                         
         SR    R3,R3                                                            
*                                                                               
CHKRSND5 LTR   R3,R3               SET CC                                       
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              ROUTINE VALIDATES RESEND                                         
*======================================================================         
VALRSND  NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,KEY                                                        
         LA    R2,SIDRSNDH                                                      
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    VALRSNDX                                                         
*                                                                               
         LA    R4,KEY                                                           
         BRAS  RE,CHKRSND          DOES THE KEY EXIST?                          
         BE    VALRSND5                                                         
*                                                                               
         CLI   8(R2),C'Y'          IF KEY DOES NOT EXIST                        
         BNE   VALRSNDX            AND WE WANT TO RESEND                        
         CLC   SIDMSG(9),=CL9'(PRIMARY)'                                        
         BE    FLDINV2                                                          
         MVC   KEY,KEYSAVE         RESTORE PASSIVE KEY                          
         MVC   TLRIDA,DMDSKADD     PUT IN DISK ADDRESS                          
         GOTO1 ADD                 AND ADD IT TO DIRECTORY                      
         B     VALRSNDX                                                         
*                                                                               
VALRSND5 OI    TLRISTAT,X'80'      IF KEY DOES EXIST                            
         CLI   8(R2),C'N'          DELETE IF WE DO NOT WANT TO RESEND           
         BE    VALRSND6                                                         
         CLI   8(R2),C'Y'          RESTORE IF WE WANT TO RESEND                 
         BNE   FLDINV2                                                          
         NI    TLRISTAT,X'FF'-X'80'                                             
VALRSND6 GOTO1 WRITE               WRITE CHANGE TO DIRECTORY                    
*                                                                               
         MVC   KEY,SVKEY                                                        
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY,SVKEY                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC              RESTORE INVOICE RECORD IN AIO                
VALRSNDX XIT1                                                                   
*                                                                               
FLDINV2  MVI   ERROR,INVALID                                                    
         GOTO1 EXIT,DMCB,0                                                      
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS IF INVOICE WILL APPLY AGAINST A GUARANTEE     *         
*        AND ENSURES THIS ACTION IS VALID                             *         
***********************************************************************         
*                                                                               
CKAPPGRT NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         XR    R2,R2               COUNT NUMBER OF CHECKS ON PAYMENT            
                                                                                
         USING TLCKD,R3                                                         
         LA    R3,KEY              READ ALL CHECK RECORDS FOR THIS              
         XC    KEY,KEY             INVOICE                                      
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,TGAGY                                                    
         MVC   TLCKINV,TGINV                                                    
         XC    TLCKINV,GRTFFS                                                   
         GOTO1 HIGH                                                             
         J     CAGRT20                                                          
CAGRT10  GOTO1 SEQ                                                              
CAGRT20  CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   CAGRT40                                                          
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         AHI   R2,1                ADD 1 TO CHECK COUNTER                       
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAGUA,TACAGUA     IF CAST IS ON A GUARANTEE                    
         JZ    CAGRT10                                                          
         MVC   TGGUA,TACAGUA       SAVE GUARANTEE CODE                          
         MVC   SVKEY,KEY           AND CHECK KEY                                
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGDATE,TAPDCYCS     SAVE PAYMENT'S APPLY DATE                    
         TM    TAPDOPT4,TAPDGRTE   INTO TGDATE                                  
         JZ    CAGRT30                                                          
         MVC   TGDATE,TAPDCYCE                                                  
         DROP  R4                                                               
                                                                                
CAGRT30  MVC   SYSFIL,=CL8'TALFIL' SET TO READ TALENT FILE                      
         MVC   SYSDIR,=CL8'TALDIR'                                              
                                                                                
         USING TLGUD,R3                                                         
         LA    R3,KEY              READ GUARANTEE RECORD                        
         XC    KEY,KEY                                                          
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVKEY+TLCKSSN-TLCKD                                      
         MVC   TLGUGUA,TGGUA                                                    
         XC    TLGUGUA,GRTFFS                                                   
         GOTO1 HIGH                                                             
         CLC   TLGUKEY,KEYSAVE                                                  
         JNE   CAGRT39                                                          
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         BAS   RE,CKGRTOK          PERFORM LARGE OVERSCALE                      
         BAS   RE,CKPCYOK          OR PER CYCLE GUARANTEE CHECKS                
         DROP  R4                                                               
                                                                                
CAGRT39  MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   KEY,SVKEY           RESTORE CHECK KEY                            
         GOTO1 HIGH                AND GO READ THE NEXT CHECK FOR               
         J     CAGRT10             THIS INVOICE                                 
                                                                                
CAGRT40  ST    R2,NCHKS            SAVE NUMBER OF CHECKS ON INVOICE             
                                                                                
         MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR'                                              
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B0',0)                                    
         MVC   AIO,AIO1            AND RESTORE AIO OF INVOICE RECORD            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT IT IS OK TO TAKE ACTION ON PAYMENT      *         
*        THAT WILL APPLY AGAINST GUARANTEE                            *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                           
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
CKGRTOK  NTR1                                                                   
         OC    TAGUIAY,TAGUIAY     ONLY CARE ABOUT NEW STYLE GUARANTEES         
         JZ    XIT                 ADDED BY THE PAY PROGRAM                     
         OC    TAGUINV,TAGUINV                                                  
         JZ    XIT                                                              
                                                                                
         MVC   SVAGY,TAGUIAY       SAVE AGENCY/INVOICE THAT ADDED               
         OC    SVAGY,SVAGY                                                      
         JNZ   *+10                                                             
         MVC   SVAGY,TAGUAGY                                                    
         MVC   SVINV,TAGUINV       GUARANTEE                                    
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO1             R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAPDUSE,=C'GRT'     EXIT IF PAY TYPE IS GRT                      
         JE    XIT                                                              
                                                                                
         USING TLIND,R3                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY             READ INVOICE RECORD THAT CREATED             
         MVI   TLINCD,TLINCDQ      THE GUARANTEE                                
         MVC   TLINAGY,SVAGY                                                    
         MVC   TLININV,SVINV                                                    
         XC    TLININV,GRTFFS                                                   
         GOTO1 HIGH                                                             
         CLC   TLINKEY,KEYSAVE                                                  
         JNE   XIT                                                              
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS RECORD                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   ACTNUM,ACTAPP       IF APPROVING APPLYING PAYMENT                
         JNE   CGO10                                                            
         TM    TAINSTAT,TAINSAPR   GUARANTEE-CREATING INVOICE MUST              
         JO    XIT                 ALSO BE APPROVED                             
         MVC   MYMSGNO,=Y(ERRGNAAI)                                             
         J     CGO20                                                            
                                                                                
CGO10    OC    TAINBDTE(6),TAINBDTE  IF CHANGING APPLYING PAYMENT TO            
         JNZ   XIT                   RUN URGENT                                 
         TM    TAINSTA2,TAINSURG   GUARANTEE-CREATING INVOICE MUST ALSO         
         JO    XIT                 BE PROCESSED OR SET TO RUN URGENT            
         MVC   MYMSGNO,=Y(ERRGP1AI)                                             
                                                                                
CGO20    BRAS  RE,BLDAI            IF YES, DISPLAY AGENCY/INVOICE               
         MVI   MYMTYP,GTMERR       THAT MUST PROCESS FIRST                      
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT PER CYCLE PAYMENT WILL PROCESS BEFORE   *         
*        APPLYING PAYMENT                                             *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                           
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
CKPCYOK  NTR1  BASE=*,LABEL=*                                                   
         OC    TAGUCOM,TAGUCOM     ONLY CARE ABOUT PER CYCLE GUARANTEES         
         JZ    XIT                                                              
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO1             R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAPDSTA2,TAPDSPRI   EXIT IF PER CYCLE PAYMENT                    
         JO    XIT                                                              
         DROP  R4                                                               
                                                                                
         MVC   SYSFIL,=CL8'CHKFIL' SET TO READ CHECK FILE                       
         MVC   SYSDIR,=CL8'CHKDIR'                                              
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY              READ UNPROCESSED CHECK RECORDS               
         XC    KEY,KEY             FOR THIS PERFORMER                           
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,SVKEY+TLCKSSN-TLCKD                                     
         MVI   TLCKECUR,C'U'                                                    
         MVC   TLCKEEMP,=C'TP '                                                 
         GOTO1 HIGH                                                             
         J     CPO20                                                            
CPO10    GOTO1 SEQ                                                              
CPO20    CLC   KEY(TLCKEDTE+1-TLCKPCD),KEYSAVE                                  
         JNE   XIT                                                              
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAPDSTA2,TAPDSPRI   SKIP NON-PER CYCLE PAYMENTS                  
         JZ    CPO10                                                            
         CLC   TGDATE,TAPDCYCS                                                  
         JL    CPO10                                                            
         CLC   TGDATE,TAPDCYCE     IF APPLY DATE DOES NOT FIT WITHIN            
         JH    CPO10               GUARANTEE CYCLE, SKIP THIS PER CYCLE         
         DROP  R4                                                               
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVC   SVAGY,TLCKAGY       SAVE AGENCY                                  
         MVC   SVINV,TLCKINV       AND INVOICE NUMBER                           
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,TGGUA       IGNORE IF PAYMENT IS NOT FOR                 
         JNE   CPO10               THIS GUARANTEE                               
         DROP  R4                                                               
                                                                                
         MVC   SVKEY2,KEY          SAVE CHECK KEY                               
         MVC   SYSFIL,=CL8'TALFIL' AND SET TO READ TALENT FILE                  
         MVC   SYSDIR,=CL8'TALDIR'                                              
                                                                                
         USING TLIND,R3                                                         
         XC    KEY,KEY             READ INVOICE RECORD                          
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,SVKEY2+TLCKEAGY-TLCKPD                                   
         MVC   TLININV,SVINV                                                    
         XC    TLININV,GRTFFS                                                   
         GOTO1 HIGH                                                             
         CLC   TLINKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS RECORD                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   ACTNUM,ACTAPP       IF APPROVING APPLYING PAYMENT                
         JNE   CPO30                                                            
         TM    TAINSTAT,TAINSAPR   PER CYCLE INVOICE MUST ALSO BE               
         JO    XIT                 APPROVED                                     
         MVC   MYMSGNO,=Y(ERRPNAAI)                                             
         J     CPO40                                                            
                                                                                
CPO30    TM    TAINSTA2,TAINSURG   IF GUARANTEE IS SET TO PROCESS               
         JO    XIT                 URGENT, PER CYCLE PAYMENT MUST               
         MVC   MYMSGNO,=Y(ERRPP1AI)              ALSO BE APPROVED               
                                                                                
CPO40    BRAS  RE,BLDAI                                                         
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
         DROP  R4                                                               
                                                                                
GRTFFS   DC    20X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES LARGE SCALE GUARANTEE PAYMENT IS ELIBIBLE    *         
*        TO BE UNAPPROVED                                             *         
***********************************************************************         
                                                                                
CHKUGRT  NTR1  BASE=*,LABEL=*                                                   
         USING TAPDD,R4                                                         
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAPDUSE,=C'GRT'     EXIT IF PAY TYPE IS NOT GRT                  
         JNE   XIT                                                              
         MVC   SVCYCLE,TAPDCYCS    SAVE GUARANTEE PAYMENT'S CYCLE               
         DROP  R4                                                               
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKD,R3                                                         
         LA    R3,KEY              READ ALL CHECK RECORDS FOR THIS              
         XC    KEY,KEY             INVOICE                                      
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,TGAGY                                                    
         MVC   TLCKINV,TGINV                                                    
         XC    TLCKINV,CUGHEXFF                                                 
         GOTO1 HIGH                                                             
         J     CUGRT20                                                          
CUGRT10  GOTO1 SEQ                                                              
CUGRT20  CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   CUGRT90                                                          
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAGUA,TACAGUA     IF CAST IS ON A GUARANTEE                    
         JZ    CUGRT10                                                          
         MVC   TGGUA,TACAGUA       SAVE GUARANTEE CODE                          
         MVC   SVKEY,KEY           AND CHECK KEY                                
         DROP  R4                                                               
                                                                                
         MVC   SYSFIL,=CL8'TALFIL' SET TO READ TALENT FILE                      
         MVC   SYSDIR,=CL8'TALDIR'                                              
                                                                                
         USING TLGUD,R3                                                         
         XC    KEY,KEY             READ GUARANTEE RECORD                        
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVKEY+TLCKSSN-TLCKD                                      
         MVC   TLGUGUA,TGGUA                                                    
         XC    TLGUGUA,CUGHEXFF                                                 
         GOTO1 HIGH                                                             
         CLC   TLGUKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         OC    TAGUCOM,TAGUCOM     ONLY CARE ABOUT LARGE OVERSCALE              
         JNZ   CUGRT80             GUARANTEES                                   
         DROP  R4                                                               
                                                                                
         USING TLCKPD,R3                                                        
         XC    KEY,KEY             READ UNPROCESSED US$ CHECK RECORDS           
         MVI   TLCKPCD,TLCKECDQ    FOR THIS PERFORMER                           
         MVC   TLCKESSN,SVKEY+TLCKSSN-TLCKD                                     
         MVI   TLCKECUR,C'U'                                                    
         J     CUGRT40                                                          
                                                                                
CUGRT30  CLI   KEYSAVE+TLCKECUR-TLCKPD,C'C'                                     
         JE    CUGRT80             NEXT READ UNPROCESSED CAN$ CHECK             
         MVC   KEY,KEYSAVE         RECORDS FOR THIS PERFOMER                    
         MVI   TLCKECUR,C'C'                                                    
                                                                                
CUGRT40  GOTO1 HIGH                                                             
         J     CUGRT60                                                          
CUGRT50  GOTO1 SEQ                                                              
CUGRT60  CLC   KEY(TLCKEEMP-TLCKPCD),KEYSAVE                                    
         JNE   CUGRT30                                                          
         CLI   TLCKEDTE,0                                                       
         JNE   CUGRT50                                                          
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVC   SVAGY,TLCKAGY       SAVE AGENCY                                  
         MVC   SVINV,TLCKINV       AND INVOICE NUMBER                           
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,TGGUA       IGNORE IF PAYMENT IS NOT FOR                 
         JNE   CUGRT50             THIS GUARANTEE                               
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAPDUSE,=C'GRT'     SKIP GRT PAYMENTS                            
         JE    CUGRT50                                                          
                                                                                
         MVC   TGDATE,TAPDCYCS     SAVE PAYMENT'S APPLY DATE                    
         TM    TAPDOPT4,TAPDGRTE   INTO TGDATE                                  
         JZ    CUGRT70                                                          
         MVC   TGDATE,TAPDCYCE                                                  
         DROP  R4                                                               
                                                                                
CUGRT70  CLC   TGDATE,SVCYCLE                                                   
         JL    CUGRT50                                                          
         CLC   TGDATE,SVCYCLE+3    IF APPLY DATE DOES NOT FIT WITHIN            
         JH    CUGRT50             GUARANTEE CYCLE, SKIP THIS PAYMENT           
                                                                                
         MVC   SVKEY2,KEY          SAVE CHECK KEY                               
         MVC   SYSFIL,=CL8'TALFIL' AND SET TO READ TALENT FILE                  
         MVC   SYSDIR,=CL8'TALDIR'                                              
                                                                                
         USING TLIND,R3                                                         
         XC    KEY,KEY             READ INVOICE RECORD                          
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,SVKEY2+TLCKEAGY-TLCKPD                                   
         MVC   TLININV,SVINV                                                    
         XC    TLININV,CUGHEXFF                                                 
         GOTO1 HIGH                                                             
         CLC   TLINKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS RECORD                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAINSTAT,TAINSAPR   IF INVOICE IS NOT APPROVED                   
         JO    UGRTERR                                                          
         MVC   SYSDIR,=CL8'CHKDIR' RESTORE READ SEQUENCE FOR NEXT               
         MVC   SYSFIL,=CL8'CHKFIL' CHECK FOR THIS PERFORMER                     
         MVC   KEY,SVKEY2                                                       
         GOTO1 HIGH                                                             
         J     CUGRT50                                                          
         DROP  R4                                                               
                                                                                
CUGRT80  MVC   KEY,SVKEY           RESTORE READ SEQUENCE FOR NEXT               
         GOTO1 HIGH                CHECK ON GRT INVOICE                         
         J     CUGRT10                                                          
                                                                                
CUGRT90  MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR'                                              
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B0',0)                                    
         J     XIT                                                              
                                                                                
UGRTERR  MVC   MYMSGNO,=Y(ERRGUAAI)                                             
         BRAS  RE,BLDAI                                                         
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
                                                                                
CUGHEXFF DC    20X'FF'                                                          
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES PER CYCLE PAYMENT IS ELIBIBLE TO BE          *         
*        UNAPPROVED                                                   *         
***********************************************************************         
                                                                                
CHKUPCY  NTR1  BASE=*,LABEL=*                                                   
         USING TAPDD,R4                                                         
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAPDSTA2,TAPDSPRI   EXIT IF NOT PER CYCLE PAYMENT                
         JZ    XIT                                                              
         MVC   SVCYCLE,TAPDCYCS    SAVE GUARANTEE PAYMENT'S CYCLE               
         DROP  R4                                                               
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKD,R3                                                         
         LA    R3,KEY              READ ALL CHECK RECORDS FOR THIS              
         XC    KEY,KEY             INVOICE                                      
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,TGAGY                                                    
         MVC   TLCKINV,TGINV                                                    
         XC    TLCKINV,CUPHEXFF                                                 
         GOTO1 HIGH                                                             
         J     CUPCY20                                                          
CUPCY10  GOTO1 SEQ                                                              
CUPCY20  CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   CUPCY70                                                          
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAGUA,TACAGUA     IF CAST IS ON A GUARANTEE                    
         JZ    CUPCY10                                                          
         MVC   TGGUA,TACAGUA       SAVE GUARANTEE CODE                          
         MVC   SVKEY,KEY           AND CHECK KEY                                
         DROP  R4                                                               
                                                                                
         MVC   SYSFIL,=CL8'TALFIL' SET TO READ TALENT FILE                      
         MVC   SYSDIR,=CL8'TALDIR'                                              
                                                                                
         USING TLGUD,R3                                                         
         XC    KEY,KEY             READ GUARANTEE RECORD                        
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVKEY+TLCKSSN-TLCKD                                      
         MVC   TLGUGUA,TGGUA                                                    
         XC    TLGUGUA,CUPHEXFF                                                 
         GOTO1 HIGH                                                             
         CLC   TLGUKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         OC    TAGUCOM,TAGUCOM     ONLY CARE ABOUT PER CYCLE GUARANTEES         
         JZ    CUPCY60                                                          
         DROP  R4                                                               
                                                                                
         USING TLCKPD,R3                                                        
         XC    KEY,KEY             READ UNPROCESSED CHECK RECORDS               
         MVI   TLCKPCD,TLCKECDQ    FOR THIS PERFORMER                           
         MVC   TLCKESSN,SVKEY+TLCKSSN-TLCKD                                     
         MVI   TLCKECUR,C'U'                                                    
         MVC   TLCKEEMP,=C'TP '                                                 
         GOTO1 HIGH                                                             
         J     CUPCY40                                                          
CUPCY30  GOTO1 SEQ                                                              
CUPCY40  CLC   KEY(TLCKEDTE+1-TLCKPCD),KEYSAVE                                  
         JNE   CUPCY60                                                          
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVC   SVAGY,TLCKAGY       SAVE AGENCY                                  
         MVC   SVINV,TLCKINV       AND INVOICE NUMBER                           
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,TGGUA       IGNORE IF PAYMENT IS NOT FOR                 
         JNE   CUPCY30             THIS GUARANTEE                               
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAPDSTA2,TAPDSPRI   SKIP PER CYCLE PAYMENTS                      
         JO    CUPCY30                                                          
                                                                                
         MVC   TGDATE,TAPDCYCS     SAVE PAYMENT'S APPLY DATE                    
         TM    TAPDOPT4,TAPDGRTE   INTO TGDATE                                  
         JZ    CUPCY50                                                          
         MVC   TGDATE,TAPDCYCE                                                  
         DROP  R4                                                               
                                                                                
CUPCY50  CLC   TGDATE,SVCYCLE                                                   
         JL    CUPCY30                                                          
         CLC   TGDATE,SVCYCLE+3    IF APPLY DATE DOES NOT FIT WITHIN            
         JH    CUPCY30             GUARANTEE CYCLE, SKIP THIS PAYMENT           
                                                                                
         MVC   SVKEY2,KEY          SAVE CHECK KEY                               
         MVC   SYSFIL,=CL8'TALFIL' AND SET TO READ TALENT FILE                  
         MVC   SYSDIR,=CL8'TALDIR'                                              
                                                                                
         USING TLIND,R3                                                         
         XC    KEY,KEY             READ INVOICE RECORD                          
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,SVAGY                                                    
         MVC   TLININV,SVINV                                                    
         XC    TLININV,CUPHEXFF                                                 
         GOTO1 HIGH                                                             
         CLC   TLINKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS RECORD                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAINSTAT,TAINSAPR   IF INVOICE IS NOT APPROVED                   
         JO    UPCYERR                                                          
         MVC   SYSDIR,=CL8'CHKDIR' RESTORE READ SEQUENCE FOR NEXT               
         MVC   SYSFIL,=CL8'CHKFIL' CHECK FOR THIS PERFORMER                     
         MVC   KEY,SVKEY2                                                       
         GOTO1 HIGH                                                             
         J     CUPCY30                                                          
         DROP  R4                                                               
                                                                                
CUPCY60  MVC   KEY,SVKEY           RESTORE READ SEQUENCE FOR NEXT               
         GOTO1 HIGH                CHECK ON GRT INVOICE                         
         J     CUPCY10                                                          
                                                                                
CUPCY70  MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR'                                              
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B0',0)                                    
         J     XIT                                                              
                                                                                
UPCYERR  MVC   MYMSGNO,=Y(ERRGUAAI)                                             
         BRAS  RE,BLDAI                                                         
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
                                                                                
CUPHEXFF DC    20X'FF'                                                          
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PUT AGENCY/INVOICE INTO BLOCK FOR ERROR DISPLAY      *         
***********************************************************************         
                                                                                
BLDAI    NTR1  BASE=*,LABEL=*                                                   
         MVC   BLOCK(25),SPACES                                                 
         MVC   BLOCK+1(6),SVAGY                                                 
                                                                                
         LA    R2,BLOCK+2                                                       
         LHI   R3,2                                                             
BAI10    CLI   0(R2),C' '                                                       
         JE    BAI20                                                            
         LA    R2,1(R2)                                                         
         AHI   R3,1                                                             
         J     BAI10                                                            
                                                                                
BAI20    MVI   0(R2),C'/'                                                       
         GOTO1 TINVCON,DMCB,SVINV,1(R2),DATCON                                  
         MVI   7(R2),0                                                          
         AHI   R3,7                                                             
         STC   R3,BLOCK                                                         
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETRIVES CHECKS FOR WEB-INITIATED PAYMENTS AND       *         
*        PRESENTS THEM AS MAINFRAME-INITIATED PAYMENT SCREEN          *         
***********************************************************************         
                                                                                
GETWSCR  NTR1  BASE=*,LABEL=*                                                   
         TM    PAYPST2,TAPDFWEB    EXIT IF PAYMENT WAS NOT MADE                 
         JZ    XIT                 VIA WEB APPLICATION                          
                                                                                
         CLI   TGUSEQU,UEDS        TREAT EDS LIKE A NON-SESSION                 
         JE    GWS01                                                            
         CLI   TGUSEQU,UDEM        AND DEM                                      
         JE    GWS01                                                            
         CLI   TGUSEQU,UPRM        AND PRM                                      
         JE    GWS01                                                            
         CLI   TGUSEQU,UTAG        AND TAG LIKE A NON-SESSION                   
         JNE   GWS02                                                            
GWS01    NI    TGUSSTAT,X'FF'-SESSION                                           
                                                                                
GWS02    BAS   RE,SVLYMP           SAVE LAST PYMT/LIST PAGE                     
                                                                                
         MVI   A1FIELD,X'FF'       INIT. A(1ST FIELD)                           
                                                                                
         TM    TGUSSTAT,SESSION    REUSE                                        
         BO    GWS05                                                            
         CLI   GPAGE,0                                                          
         BE    GWS05                                                            
         CLI   GPAGE,1             2ND PAGE FOR REUSE                           
         BE    GWS03                                                            
         TWAXC DTLFRSTH,DTLLSTH,PROT=Y                                          
         B     GWS30                                                            
                                                                                
GWS03    TWAXC PAYOVLYH,PAYTOTSH,PROT=Y                                         
         LA    R3,PAYOVLYH                                                      
         MVI   OVERLAY,SCR95                                                    
         MVI   ALTPROG,X'F3'                                                    
         GOTO1 LOADSOPH,DMCB,1     LOAD REUSE SCREEN                            
         MVI   ALTPROG,X'F2'       RESET                                        
         BAS   RE,TRANSSCR         TURN ON TRANSMITS                            
         B     GWS30                                                            
                                                                                
         USING TLSCD,R3                                                         
GWS05    LA    R3,KEY                                                           
         XC    TLSCKEY,TLSCKEY                                                  
         MVI   TLSCCD,TLSCCDQ      READ ALL SCREEN KEY/RECORDS                  
         MVC   TLSCAGY,TGAGY       FOR THIS INVOICE                             
         MVC   TLSCINV,SCRINV                                                   
         GOTO1 HIGH                                                             
         CLC   TLSCKEY(TLSCPG-TLSCKEY),KEYSAVE  INSURE RECORD FOR INV.          
         JNE   SCRNLEER            SCREEN RECORDS NO LONGER EXIST               
         J     GWS20                                                            
GWS10    GOTO1 SEQ                                                              
GWS20    CLC   KEY(TLSCPG-TLSCKEY),KEYSAVE                                      
         JNE   GWS30                                                            
         CLI   KEY+TLSCSCR-TLSCKEY,SCRC0                                        
         JE    GWS10                                                            
         BAS   RE,COPY2TIA         COPY SCREEN RECORD TO TIA                    
                                                                                
         LA    RF,CONHED2X                                                      
         AHI   RF,L'CONHED2X                                                    
         OI    6(RF),X'80'         TRANSMIT                                     
         LA    RF,CONRECX                                                       
         AHI   RF,L'CONRECX                                                     
         OI    6(RF),X'80'         TRANSMIT                                     
                                                                                
         J     GWS10                                                            
         DROP  R3                                                               
                                                                                
GWS30    GOTO1 FLDVAL,DMCB,(8,BSSAGYH),999                                      
                                                                                
         L     R1,NCHKS            DIVIDE NUMBER OF CHECKS BY 6                 
         XR    R0,R0               AND SAVE TOTAL NUMBER OF PAGES               
         D     R0,=F'6'                                                         
         LTR   R1,R1                                                            
         JZ    GWS40                                                            
         LTR   R0,R0                                                            
         JNZ   GWS40                                                            
         SHI   R1,1                                                             
GWS40    STC   R1,LPAGE                                                         
         TM    TGUSSTAT,SESSION    REUSE CHECKS STARTS ON 2ND PAGE              
         BO    GWS43                                                            
         AHI   R1,1                                                             
         STC   R1,LPAGE                                                         
                                                                                
GWS43    CLC   GPAGE,LPAGE                                                      
         JH    GWS900                                                           
                                                                                
         XC    CHKSEQ,CHKSEQ                                                    
         ZIC   R1,GPAGE                                                         
         TM    TGUSSTAT,SESSION    REUSE CHECKS STARTS ON 2ND PAGE              
         BO    *+8                                                              
         AHI   R1,-1                                                            
         MHI   R1,6                                                             
         ST    R1,STCHK            SAVE STARTING CHECK FOR THIS PAGE            
                                                                                
         TM    TGUSSTAT,SESSION    REUSE                                        
         BO    GWS45                                                            
         CLI   GPAGE,0             FIRST PAGE FOR REUSE                         
         BE    GWS890                 YES, SHOW CHECKS 2ND PAGE ON              
                                                                                
GWS45    MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING LINED,R2                                                         
         LA    R2,BSSFRSTH         R2=A(FIRST PERFORMER FIELD)                  
         CLI   TGUSEQU,UBSR                                                     
         BE    GWS49                                                            
         CLI   TGUSEQU,URRR                                                     
         BE    GWS49                                                            
         LA    R2,ADOFRSTH                                                      
         CLI   TGUSEQU,UADO                                                     
         BE    GWS49                                                            
         LA    R2,BSSFRSTH                                                      
         TM    TGUSSTAT,SESSION    REUSE                                        
         BO    GWS49                                                            
         LA    R2,DTLFRSTH         R2=A(FIRST DETAIL)                           
                                                                                
         USING TLCKD,R3                                                         
GWS49    LA    R3,KEY              READ ALL CHECKS ATTACHED TO THIS             
         XC    KEY,KEY             INVOICE                                      
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,TGAGY                                                    
         MVC   TLCKINV,TGINV                                                    
         XC    TLCKINV,=6X'FF'                                                  
         GOTO1 HIGH                                                             
         J     GWS60                                                            
GWS50    GOTO1 SEQ                                                              
GWS60    CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   GWS890                                                           
         OC    STCHK,STCHK         SKIP ALL PREVIOUSLY DISPLAYED                
         JZ    GWS300              CHECKS                                       
         L     R0,CHKSEQ                                                        
         C     R0,STCHK                                                         
         JE    GWS300                                                           
         AHI   R0,1                                                             
         ST    R0,CHKSEQ                                                        
         J     GWS50                                                            
                                                                                
GWS300   CLI   TGUSEQU,UBSR                                                     
         BE    GWS500                                                           
         CLI   TGUSEQU,URRR                                                     
         BE    GWS500                                                           
         TM    TGUSSTAT,SESSION    REUSE HAS DIFF SCREEN                        
         BZ    GWS700                                                           
*---------------------------------------------------------------------*         
*              SESSION SECTION                                        *         
*---------------------------------------------------------------------*         
         XC    STCHK,STCHK                                                      
         CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   GWS890                                                           
         MVC   TGSSN,TLCKSSN                                                    
         DROP  R3                                                               
                                                                                
         MVC   BLOCK(L'KEY),KEY    DISPLAY PERFORMER NAME                       
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'AC',0),LNAMEH                             
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   LNAMEX(20),SPACES                                                
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKD,R3                                                         
         MVC   KEY,BLOCK                                                        
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
         GOTO1 FLDVAL,DMCB,(4,LSPOTH),LAGTH                                     
                                                                                
         USING TASDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPSELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    GWS330                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
GWS330   EDIT  (1,TASDSP),(2,LSPOT),ALIGN=LEFT                                  
         EDIT  (1,TASDDAY),(2,LDAY),ALIGN=LEFT                                  
         EDIT  (1,TASDOT),(2,LOT),ALIGN=LEFT                                    
         EDIT  (1,TASDDT),(2,LDT),ALIGN=LEFT                                    
         EDIT  (2,TASDTRV),(5,LTT),2,ALIGN=LEFT,ZERO=BLANK                      
         EDIT  (2,TASDPDW),(5,LPDW),2,ALIGN=LEFT,ZERO=BLANK                     
         EDIT  (1,TASDTAG),(2,LTAG),ALIGN=LEFT                                  
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
GWS350   L     R4,AIO                                                           
         MVI   ELCODE,TAPPELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    GWS370                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
GWS370   MVC   TGTYPE,TAPDW4TY     SAVE TYPE                                    
         XC    TGDUB,TGDUB         AND 1ST OVERSCALE RATE                       
         MVC   TGDUB(4),TAPDOV1                                                 
                                                                                
         MVC   LREI,TAPDICDE       DISPLAY REMIMBURSED EXPENSE CODE             
                                                                                
         OC    TAPDREXP,TAPDREXP                                                
         JZ    GWS380                                                           
         EDIT  TAPDREXP,LRE,2      DISPLAY REIMBURSED EXPENSE AMOUNT            
                                                                                
GWS380   L     RF,TAPDPAYI                                                      
         A     RF,TAPDPAYC                                                      
         LTR   RF,RF                                                            
         JZ    GWS390                                                           
         EDIT  (RF),LPAY,2         DISPLAY PAYMENT AMOUNT                       
                                                                                
GWS390   TM    TAPDPST2,TAPDPOPA   IF PAYMENT AMOUNT WAS OVERRIDDEN             
         JZ    GWS400                                                           
         OI    LPAYH+1,X'08'       DISPLAY IN HIGH INTENSITY                    
                                                                                
GWS400   OC    TAPDSPNH,TAPDSPNH                                                
         JZ    GWS410                                                           
         EDIT  TAPDSPNH,LSPNH,2    DISPLAY SUBJECT TO P&H AMOUNT                
                                                                                
GWS410   TM    TAPDPST2,TAPDPOPH   IF SUBJECT TO P&H AMOUNT WAS                 
         JZ    GWS420              OVERRIDDEN                                   
         OI    LSPNHH+1,X'08'      DISPLAY IN HIGH INTENSITY                    
                                                                                
GWS420   L     RF,TAPDMDED                                                      
         A     RF,TAPDDUES                                                      
         LTR   RF,RF                                                            
         JZ    GWS430                                                           
         EDIT  (RF),LMDED,2        DISPLAY MISCELLANEOUS DEDUCTIONS             
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
GWS430   L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         TM    TACASTAT,TACASTAO   IF AGENT WAS OVERRIDDEN                      
         JZ    GWS440                                                           
         OC    TACANCDE,TACANCDE   DISPLAY AGENT                                
         JZ    GWS440                                                           
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),LAGT                               
         OI    LAGTH+1,X'08'                                                    
                                                                                
GWS440   MVC   LCAT,TLCKCAT        DISPLAY CATEGORY                             
         DROP  R3                                                               
                                                                                
         MVI   LCAM,C'Y'                                                        
         CLI   TACAONOF+1,C'F'                                                  
         JNE   *+8                                                              
         MVI   LCAM,C'N'           DISPLAY ON/OFF CAMERA                        
                                                                                
         MVC   LCYR,TACAYEAR       DISPLAY CONTRACT YEAR                        
                                                                                
         LA    R5,LOPT                                                          
                                                                                
         CLI   TGTYPE,TAW4TYIN     IF THIS ISN'T AN INDIVIDUAL                  
         JE    GWS450                                                           
         MVC   0(1,R5),TGTYPE      DISPLAY W4 TYPE                              
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
                                                                                
GWS450   OC    TACAGUA,TACAGUA     IF ON A GUARANTEE                            
         JZ    GWS460                                                           
         MVC   0(3,R5),TACAGUA+1   DISPLAY LAST 3 CHARS OF CODE                 
         MVI   3(R5),C','                                                       
         LA    R5,4(R5)                                                         
                                                                                
GWS460   CLI   TACADBL,C' '        IF DOUBLES PRESENT                           
         JNH   GWS470                                                           
         MVC   0(1,R5),TACADBL     DISPLAY THEM                                 
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
                                                                                
GWS470   MVC   TGDUB+4(4),TACAOV2  SAVE 2ND OVERSCALE RATE                      
                                                                                
         OC    TGDUB,TGDUB         IF THERE'S ANY OVERSCALE                     
         JZ    GWS480                                                           
         MVC   0(2,R5),=C'O='      DISPLAY TAG                                  
         LA    R5,2(R5)                                                         
         L     R1,TGDUB            1ST OVERSCALE RATE                           
         BAS   RE,EDITOV                                                        
         ICM   R1,15,TGDUB+4       2ND OVERSCALE RATE IF PRESENT                
         JZ    *+8                                                              
         BAS   RE,EDITOV                                                        
                                                                                
GWS480   BCTR  R5,0                SHUFFLE BACK ONE                             
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '          ERASE TRAILING COMMA                         
                                                                                
         MVC   LCTAG,=C'^Cmnt'     DISPLAY COMMENT                              
         GOTO1 CHAROUT,DMCB,TACMELQ,LCMTH,TACMTYPC                              
                                                                                
         LA    R2,LINELNQ(R2)                                                   
         LA    R4,BSSLSTH                                                       
         CR    R2,R4                                                            
         JL    GWS50                                                            
         J     GWS890                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              RADIO SECTION                                          *         
*---------------------------------------------------------------------*         
         USING LINRDD,R2                                                        
         USING TLCKD,R3                                                         
GWS500   CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   GWS890                                                           
         MVC   TGSSN,TLCKSSN                                                    
         DROP  R3                                                               
                                                                                
         MVC   BLOCK(L'KEY),KEY    DISPLAY PERFORMER NAME                       
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'AC',0),LRDNAME-8                          
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   LRDNAMEX(20),SPACES                                              
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKD,R3                                                         
         MVC   KEY,BLOCK                                                        
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
         GOTO1 FLDVAL,DMCB,(4,LRDSPTH),LRDAGTH                                  
                                                                                
         USING TASDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPSELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    GWS510                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
GWS510   EDIT  (1,TASDRSP),(2,LRDSPT),ALIGN=LEFT                                
         EDIT  (2,TASDRHM),(5,LRDHRMN),2,ALIGN=LEFT                             
         EDIT  (1,TASDRTG),(2,LRDTAG),ALIGN=LEFT                                
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPPELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    GWS570                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
GWS570   MVC   TGTYPE,TAPDW4TY     SAVE TYPE                                    
         XC    TGDUB,TGDUB         AND 1ST OVERSCALE RATE                       
         MVC   TGDUB(4),TAPDOV1                                                 
                                                                                
         MVC   LRDAPC,TAPDACDE     DISPLAY APPLIED AMOUNT CODE                  
         OC    TAPDAPPL,TAPDAPPL                                                
         JZ    GWS575                                                           
         L     RF,TAPDAPPL                                                      
         LNR   RF,RF                                                            
         EDIT  (RF),LRDAP,2        DISPLAY APPLIED AMOUNT                       
                                                                                
GWS575   MVC   LRDREI,TAPDICDE     DISPLAY REMIMBURSED EXPENSE CODE             
         OC    TAPDREXP,TAPDREXP                                                
         JZ    GWS580                                                           
         EDIT  TAPDREXP,LRDRE,2    DISPLAY REIMBURSED EXPENSE AMOUNT            
                                                                                
GWS580   L     RF,TAPDPAYI                                                      
         A     RF,TAPDPAYC                                                      
         LTR   RF,RF                                                            
         JZ    GWS590                                                           
         EDIT  (RF),LRDPAY,2       DISPLAY PAYMENT AMOUNT                       
                                                                                
GWS590   TM    TAPDPST2,TAPDPOPA   IF PAYMENT AMOUNT WAS OVERRIDDEN             
         JZ    GWS600                                                           
         OI    LRDPAYH+1,X'08'     DISPLAY IN HIGH INTENSITY                    
                                                                                
GWS600   OC    TAPDSPNH,TAPDSPNH                                                
         JZ    GWS610                                                           
         EDIT  TAPDSPNH,LRDSPNH,2  DISPLAY SUBJECT TO P&H AMOUNT                
                                                                                
GWS610   TM    TAPDPST2,TAPDPOPH   IF SUBJECT TO P&H AMOUNT WAS                 
         JZ    GWS620              OVERRIDDEN                                   
         OI    LRDSPNHH+1,X'08'    DISPLAY IN HIGH INTENSITY                    
                                                                                
GWS620   L     RF,TAPDMDED                                                      
         A     RF,TAPDDUES                                                      
         LTR   RF,RF                                                            
         JZ    GWS630                                                           
         EDIT  (RF),LRDMDED,2      DISPLAY MISCELLANEOUS DEDUCTIONS             
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
GWS630   L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         TM    TACASTAT,TACASTAO   IF AGENT WAS OVERRIDDEN                      
         JZ    GWS640                                                           
         OC    TACANCDE,TACANCDE   DISPLAY AGENT                                
         JZ    GWS640                                                           
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),LRDAGT                             
         OI    LRDAGTH+1,X'08'                                                  
                                                                                
GWS640   MVC   LRDCAT,TLCKCAT      DISPLAY CATEGORY                             
         DROP  R3                                                               
                                                                                
         MVI   LRDCAM,C'Y'                                                      
         CLI   TACAONOF+1,C'F'                                                  
         JNE   *+8                                                              
         MVI   LRDCAM,C'N'         DISPLAY ON/OFF CAMERA                        
                                                                                
         MVC   LRDCYR,TACAYEAR     DISPLAY CONTRACT YEAR                        
                                                                                
         LA    R5,LRDOPT                                                        
         BRAS  RE,GWSCAST                                                       
                                                                                
         MVC   LRDCTAG,=C'^Cmnt'   DISPLAY COMMENT                              
         GOTO1 CHAROUT,DMCB,TACMELQ,LRDCMTH,TACMTYPC                            
         DROP  R4                                                               
                                                                                
         LA    R2,LRDLNQ(R2)                                                    
         LA    R4,BSRLSTH                                                       
         CLI   TGUSEQU,UADO                                                     
         BNE   *+8                                                              
         LA    R4,ADOLSTH                                                       
         CR    R2,R4                                                            
         JL    GWS50                                                            
         J     GWS890                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              REUSE SECTION                                          *         
*---------------------------------------------------------------------*         
         USING LINB5D,R2                                                        
         USING TLCKD,R3                                                         
GWS700   CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   GWS890                                                           
         GOTO1 DATCON,DMCB,(1,SVDDTE),(8,DTLDUE+5)                              
         OI    DTLDUEH+6,X'80'                                                  
         MVC   TGSSN,TLCKSSN                                                    
         DROP  R3                                                               
                                                                                
         MVC   BLOCK(L'KEY),KEY    DISPLAY PERFORMER NAME                       
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'AC',0),LB5NAME-8                          
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   LB5CAT,SPACES                                                    
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKD,R3                                                         
         MVC   KEY,BLOCK                                                        
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
         GOTO1 FLDVAL,DMCB,(4,LB5NPH),LB5AGTH                                   
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPPELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    GWS770                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
GWS770   MVC   TGTYPE,TAPDW4TY     SAVE TYPE                                    
         XC    TGDUB,TGDUB         AND 1ST OVERSCALE RATE                       
         MVC   TGDUB(4),TAPDOV1                                                 
                                                                                
         MVC   LB5ICDE,TAPDICDE    DISPLAY REMIMBURSED EXPENSE CODE             
                                                                                
         OC    TAPDREXP,TAPDREXP                                                
         JZ    GWS780                                                           
         EDIT  TAPDREXP,LB5REIM,2  DISPLAY REIMBURSED EXPENSE AMOUNT            
                                                                                
GWS780   L     RF,TAPDPAYI                                                      
         A     RF,TAPDPAYC                                                      
         LTR   RF,RF                                                            
         JZ    GWS790                                                           
         EDIT  (RF),LB5PAY,2       DISPLAY PAYMENT AMOUNT                       
                                                                                
GWS790   TM    TAPDPST2,TAPDPOPA   IF PAYMENT AMOUNT WAS OVERRIDDEN             
         JZ    GWS800                                                           
         OI    LB5PAYH+1,X'08'     DISPLAY IN HIGH INTENSITY                    
                                                                                
GWS800   OC    TAPDSPNH,TAPDSPNH                                                
         JZ    GWS810                                                           
         EDIT  TAPDSPNH,LB5SPNH,2  DISPLAY SUBJECT TO P&H AMOUNT                
                                                                                
GWS810   TM    TAPDPST2,TAPDPOPH   IF SUBJECT TO P&H AMOUNT WAS                 
         JZ    GWS820              OVERRIDDEN                                   
         OI    LB5SPNHH+1,X'08'    DISPLAY IN HIGH INTENSITY                    
                                                                                
GWS820   L     RF,TAPDMDED                                                      
         A     RF,TAPDDUES                                                      
         LTR   RF,RF                                                            
         JZ    GWS830                                                           
         EDIT  (RF),LB5MDED,2        DISPLAY MISCELLANEOUS DEDUCTIONS           
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
GWS830   L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         TM    TACASTAT,TACASTAO   IF AGENT WAS OVERRIDDEN                      
         JZ    GWS840                                                           
         OC    TACANCDE,TACANCDE   DISPLAY AGENT                                
         JZ    GWS840                                                           
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),LB5AGT                             
         OI    LB5AGTH+1,X'08'                                                  
                                                                                
GWS840   MVC   LB5CAT,TLCKCAT      DISPLAY CATEGORY                             
         DROP  R3                                                               
                                                                                
         MVI   LB5CAM,C'Y'                                                      
         CLI   TACAONOF+1,C'F'                                                  
         JNE   *+8                                                              
         MVI   LB5CAM,C'N'         DISPLAY ON/OFF CAMERA                        
                                                                                
         MVC   LB5YR,TACAYEAR      DISPLAY CONTRACT YEAR                        
                                                                                
         LA    R5,LB5OPTS                                                       
         BRAS  RE,GWSCAST                                                       
                                                                                
         MVC   LB5TAG,=C'^Cmnt'     DISPLAY COMMENT                             
         GOTO1 CHAROUT,DMCB,TACMELQ,LB5CMNTH,TACMTYPC                           
                                                                                
         LA    R2,LB5LNQ(R2)                                                    
         LA    R4,DTLLSTH                                                       
         CR    R2,R4                                                            
         JL    GWS50                                                            
         J     GWS890                                                           
         EJECT                                                                  
***********************************************************************         
         DROP  R2                                                               
GWS890   ZIC   RE,GPAGE            BUMP PAGE FOR NEXT TIME                      
         AHI   RE,1                                                             
         STC   RE,GPAGE                                                         
                                                                                
         MVC   SYSDIR,=CL8'TALDIR' RESTORE DIRECTORY                            
         MVC   SYSFIL,=CL8'TALFIL' AND FILE SETTINGS                            
         J     GWSX                                                             
                                                                                
***********************************************************************         
                                                                                
         USING TLSCD,R3                                                         
GWS900   LA    R3,KEY              IF PROCESSING THE PYMT/LIST                  
         XC    TLSCKEY,TLSCKEY     SCREENS ...                                  
         MVI   TLSCCD,TLSCCDQ      BUILD PYMT/LIST SCREEN RECORD                
         MVC   TLSCAGY,TGAGY       KEY FOR THIS INVOICE AND PAGE                
         MVC   TLSCINV,SCRINV      NUMBER                                       
         MVC   TLSCPG,GPYMP                                                     
         MVI   TLSCSCR,SCRC0                                                    
         GOTO1 HIGH                                                             
         CLC   TLSCKEY(TLSCPG-TLSCKEY),KEYSAVE  INSURE RECORD FOR INV.          
         JNE   SCRNLEER            SCREEN RECORDS NO LONGER EXIST               
         J     GWS920                                                           
GWS910   GOTO1 SEQ                                                              
GWS920   CLC   KEY(TLSCSEQ-TLSCKEY),KEYSAVE                                     
         JNE   GWS930                                                           
         DROP  R3                                                               
                                                                                
         BAS   RE,COPY2TIA         COPY SCREEN RECORD TO TIA                    
         J     GWS910                                                           
                                                                                
GWS930   ZIC   RE,GPYMP                                                         
         AHI   RE,1                                                             
         STC   RE,GPYMP                                                         
                                                                                
***********************************************************************         
                                                                                
GWSX     L     RF,A1FIELD          TRANSMIT AND REBUILD RESTORED FIELDS         
         XR    R1,R1                                                            
         XR    RE,RE                                                            
GWSX10   OI    6(RF),X'80'         TRANSMIT                                     
         AHI   RE,1                                                             
         CHI   RE,11                                                            
         JL    *+8                                                              
         OI    7(RF),X'80'         REBUILD                                      
         ICM   R1,1,0(RF)                                                       
         JZ    XIT                 UNTIL REACH E-O-S                            
         AR    RF,R1                                                            
         J     GWSX10                                                           
                                                                                
EDITOV   DS    0H                                                               
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(4,(R5)),ALIGN=LEFT                                         
         AR    R5,R0                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         BR    RE                                                               
                                                                                
TRANSSCR DS    0H                                                               
         XR    R0,R0                                                            
         OI    6(R3),X'80'         TRANSMIT                                     
         OI    7(R3),X'80'         AND RE-BUILD                                 
TRSX10   ICM   R0,1,0(R3)                                                       
         BZR   RE                                                               
         AR    R3,R0                                                            
         J     TRSX10                                                           
                                                                                
***********************************************************************         
*        ROUTINE SAVED LAST PYMT/LIST PAGE                            *         
***********************************************************************         
                                                                                
SVLYMP   NTR1                                                                   
         MVI   LPYMP,0             INITIALIZE LAST PYMT/LIST PAGE               
                                                                                
         TM    PAYPST1,TAPDPCHG    IF PAYMENT WAS ALTERED VIA                   
         JZ    XIT                 PYMT/LIST ...                                
                                                                                
         USING TLSCD,R3                                                         
         LA    R3,KEY                                                           
         XC    TLSCKEY,TLSCKEY                                                  
         MVI   TLSCCD,TLSCCDQ      READ PYMT/LIST SCREEN KEYS FOR               
         MVC   TLSCAGY,TGAGY       THIS INVOICE                                 
         MVC   TLSCINV,SCRINV                                                   
         GOTO1 HIGH                                                             
         CLC   TLSCKEY(TLSCPG-TLSCKEY),KEYSAVE  INSURE RECORD FOR INV.          
         JNE   SCRNLEER            SCREEN RECORDS NO LONGER EXIST               
         J     SVLYMP20                                                         
SVLYMP10 GOTO1 SEQ                                                              
SVLYMP20 CLC   KEY(TLSCPG-TLSCKEY),KEYSAVE                                      
         JNE   XIT                                                              
         CLI   TLSCSCR,SCRC0                                                    
         JNE   SVLYMP10                                                         
         MVC   LPYMP,TLSCPG        SAVE LAST PYMT/LIST PAGE                     
         J     SVLYMP10                                                         
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE COPIES SCREEN ELEMENTS TO SCREEN                     *         
***********************************************************************         
                                                                                
COPY2TIA NTR1                                                                   
         GOTO1 GETREC                                                           
                                                                                
         USING TASCD,R4                                                         
         L     R4,AIO              R4=A(SCREEN RECORD)                          
         MVI   ELCODE,TASCELQ      SET TO GET SCREEN ELEMENTS                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
C2T10    BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
                                                                                
         L     R2,ATWA                                                          
         AH    R2,TASCDISP         R2=A(LOCATION IN SCREEN)                     
                                                                                
         CLI   A1FIELD,X'FF'                                                    
         JNE   *+8                                                              
         ST    R2,A1FIELD          SAVE A(FIRST FIELD RESTORED)                 
                                                                                
         LA    R0,TASCDATA                                                      
         ZIC   R1,TASCLEN                                                       
         SHI   R1,TASCLNQ                                                       
         LA    R3,1(R1)                                                         
         MVCL  R2,R0               MOVE ELEMENT DATA TO TIA                     
         J     C2T10               LOOK FOR ANOTHER ELEMENT                     
         EJECT                                                                  
*---------------------------------------------------------------------          
         USING TACAD,R4                                                         
GWSCAST  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGTYPE,TAW4TYIN     IF THIS ISN'T AN INDIVIDUAL                  
         JE    GWSC650                                                          
         MVC   0(1,R5),TGTYPE      DISPLAY W4 TYPE                              
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
                                                                                
GWSC650  OC    TACAGUA,TACAGUA     IF ON A GUARANTEE                            
         JZ    GWSC660                                                          
         MVC   0(3,R5),TACAGUA+1   DISPLAY LAST 3 CHARS OF CODE                 
         MVI   3(R5),C','                                                       
         LA    R5,4(R5)                                                         
                                                                                
GWSC660  CLI   TACADBL,C' '        IF DOUBLES PRESENT                           
         JNH   GWSC670                                                          
         MVC   0(1,R5),TACADBL     DISPLAY THEM                                 
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
                                                                                
GWSC670  MVC   TGDUB+4(4),TACAOV2  SAVE 2ND OVERSCALE RATE                      
                                                                                
         OC    TGDUB,TGDUB         IF THERE'S ANY OVERSCALE                     
         JZ    GWSC680                                                          
         MVC   0(2,R5),=C'O='      DISPLAY TAG                                  
         LA    R5,2(R5)                                                         
         L     R1,TGDUB            1ST OVERSCALE RATE                           
         BAS   RE,EDITOV2                                                       
         ICM   R1,15,TGDUB+4       2ND OVERSCALE RATE IF PRESENT                
         JZ    *+8                                                              
         BAS   RE,EDITOV2                                                       
                                                                                
GWSC680  BCTR  R5,0                SHUFFLE BACK ONE                             
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '          ERASE TRAILING COMMA                         
                                                                                
         J     XIT                                                              
                                                                                
EDITOV2  DS    0H                                                               
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(4,(R5)),ALIGN=LEFT                                         
         AR    R5,R0                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         BR    RE                                                               
                                                                                
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY WEB APPLICATION ID                        *         
***********************************************************************         
                                                                                
DISWID   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO              IF WEB ID ELEMENT EXISTS                     
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JE    DWID10                                                           
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTOWB))                                     
         JNE   XIT                                                              
                                                                                
         USING TAFND,R4                                                         
DWID10   L     R4,TGELEM                                                        
         CLC   =C'HF',TAFNNAME     DISPLAY CERNO HEADER                         
         JNE   DWID20                                                           
         MVC   SIDAHED(L'PVCHEAD),PVCHEAD                                       
         J     DWID50                                                           
                                                                                
DWID20   CLC   =C'NP',TAFNNAME     OR NABLE HEADER                              
         JNE   DWID30                                                           
         MVC   SIDAHED(L'PVNHEAD),PVNHEAD                                       
         J     DWID50                                                           
                                                                                
DWID30   CLC   =C'VS',TAFNNAME     OR VITA HEADER                               
         JE    DWID40                                                           
         CLC   =C'TS',TAFNNAME                                                  
         JE    DWID40                                                           
         CLC   =C'VC',TAFNNAME                                                  
         JE    DWID40                                                           
         CLC   =C'TC',TAFNNAME                                                  
         JE    DWID40                                                           
         CLC   =C'RS',TAFNNAME     OR VITA RADIO                                
         JE    DWID40                                                           
         CLC   =C'RC',TAFNNAME                                                  
         JE    DWID40                                                           
         DC    H'00'                                                            
DWID40   MVC   SIDAHED(L'AVVHEAD),AVVHEAD                                       
                                                                                
         CLI   TAFNTYPE,TAFNTWEB   IF CURRENT WEB APPLICATION ID                
         JNE   DWID50              ELEMENT, UNPROTECT THE FIELD                 
         NI    SIDAINFH+1,X'DF'                                                 
                                                                                
DWID50   MVC   SIDAINF(18),TAFNNAME   AND DISPLAY WEB ID                        
         DROP  R4                                                               
                                                                                
         NI    SIDAINFH+1,X'F3'    TURN OFF ZERO INTENSITY FOR DATA             
         NI    SIDAHEDH+1,X'F3'    TURN OFF ZERO INTENSITY FOR HEADER           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
PVCHEAD  DC    CL19'Paid via Cerno'                                             
PVNHEAD  DC    CL19'Paid via nAble'                                             
AVVHEAD  DC    CL19'Authorized via Vita'                                        
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE FOR SUPER USER PASSWORD IN WEB           *         
*        APPLICATION ID FIELD. IF PRESENT, REMOVE WEB APPLICATION     *         
*        ID FROM CAST RECORD                                          *         
*        ON ENTRY ... AIO = (INVOICE RECORD)                          *         
***********************************************************************         
                                                                                
VALWID   NTR1  BASE=*,LABEL=*                                                   
         OC    SIDAINF,SPACES                                                   
         CLC   SIDAINF,=CL30'VIDA' IF PASSWORD IS IN WID FIELD ...              
         JE    VWID20                                                           
         TM    PRGSTAT,TESTSYS     TST STYSTEM                                  
         JO    VWID10                                                           
         CLC   TGUSER,=H'7538'     AND USER-IDS TALFQA                          
         JE    VWID10                                                           
         CLC   TGUSER,=H'7697'     AND CLIFQA                                   
         JNE   XIT                                                              
VWID10   CLC   SIDAINF,=CL30'ATIV' STILL RECOGNIZE OLD PASSWORD                 
         JNE   XIT                                                              
                                                                                
VWID20   MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   XIT                                                              
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         MVI   TAFNTYPE,TAFNTOWB   CHANGE WEB APPLICATION ELEMENT TYPE          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DISPLAYS ADVICE NUMBER                               *         
***********************************************************************         
                                                                                
DISADV   NTR1  BASE=*,LABEL=*                                                   
         OC    TGADV,TGADV         IF DISPLAYING ADVICE                         
         JZ    XIT                                                              
                                                                                
         L     R2,TGFULL                                                        
DADV10   TM    1(R2),X'20'         FIND AUTH/PO FIELD AND POINT                 
         JZ    DADV20              R2 AT IT                                     
         CLC   =C'Auth/po',8(R2)                                                
         JE    DADV30                                                           
DADV20   ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CLI   0(R2),0                                                          
         JNE   DADV10                                                           
         DC    H'00'                                                            
                                                                                
DADV30   ZIC   RE,0(R2)            CLEAR ALL UNPROECTED FIELDS                  
         AR    R2,RE               FROM AUTH/PO DOWN                            
         GOTO1 FLDVAL,DMCB,(1,(R2)),(X'80',999)                                 
                                                                                
         MVC   8(2,R2),=C'A='      INSERT ADVICE NUMBER                         
         MVC   10(L'TGADV,R2),TGADV                                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO HANDLE INVOICE APPROVAL                               
***********************************************************************         
APPROVE  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,VALADTE               VALIDATE APPROVAL DATE                  
         TM    AGYSTAT,TAAYSCOD                                                 
         JZ    *+14                                                             
         MVC   CONHED2(12),CODMSG  DISPLAY COD MSG IF NECESSARY                 
         J     APP100                                                           
         TM    CLTSTAT,TACISCOD   COD CLIENT?                                   
         JZ    APP100                                                           
         MVC   CONHED2(12),CODMSG2                                              
                                                                                
APP100   TM    APPSTAT,INPROGRS    TEST ALREADY IN PROGRESS                     
         JO    APP130                                                           
         MVI   GPAGE,0             INITIALIZE PAGE NUMBER                       
         MVI   GSCRN,0             THIS PAGE NUMBER                             
         MVI   LSCRN,0             LAST PAGE NUMBER                             
         MVI   GPYMP,1             AND THIS PYMT/LIST PAGE NUMBER               
         XC    ELTAGQ,ELTAGQ                                                    
         XC    ELTAGQ2,ELTAGQ2                                                  
         XC    ELTAFQ,ELTAFQ                                                    
         XC    ELTAFQ2,ELTAFQ2                                                  
         OI    APPSTAT,INPROGRS                                                 
                                                                                
APP130   CLI   PFAID,24            TEST PRESSED PF24 TO ABORT                   
         JE    APP300                                                           
                                                                                
         TM    APPSTAT,APPISPDN    DISPLAY ISPLIT SCREEN IF NECESSARY           
         JO    APP140                                                           
         TM    SVSTA2,TAINSPRM     IF PRIMARY INVOICE                           
         JNO   APP140                                                           
         BAS   RE,GOISPLT          SHOW ISPLIT INFO                             
         OI    APPSTAT,APPISPDN    INDICATE DONE WITH ISPLIT                    
         J     SCRNIDIS            EXIT - ISPLIT SCREEN DISPLAYED               
                                                                                
APP140   TM    APPSTAT,APPBOVDN    DISPLAY BOVER SCREEN IF NECESSARY            
         JO    APP150                                                           
         CLI   RATSTAT,0           DO RATES EXIST?                              
         JE    APP150              NO, SKIP SHOWING BOVER                       
         BAS   RE,GOBOVER          SHOW BOVER INFO                              
         OI    APPSTAT,APPBOVDN    INDICATE DONE WITH BOVER                     
         J     SCRNBDIS            EXIT - BOVER SCREEN DISPLAYED                
                                                                                
APP150   GOTO1 RECVAL,DMCB,TLINCDQ,(X'30',0) READ INV REC FOR UPDATE            
                                                                                
         XC    TGADV,TGADV                                                      
         TM    APPSTAT,APPADVDN                                                 
         JO    APP170                                                           
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTADV                                  
         JNE   APP170                                                           
         MVC   TGADV,TGNAME                                                     
         OI    APPSTAT,APPADVDN                                                 
                                                                                
         USING TADDD,R4                                                         
APP170   XC    SVDDTE,SVDDTE                                                    
         MVI   ELCODE,TADDELQ      GET DUE DATE EL.                             
         L     R4,AIO                                                           
         BRAS  RE,GETEL            IF NOT FOUND                                 
         JNE   APP180                                                           
         MVC   SVDDTE,TADDDATE                                                  
                                                                                
APP180   MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS EL.                      
         L     R4,AIO                                                           
         BRAS  RE,GETEL            IF NOT FOUND                                 
         JE    APP200                                                           
         MVI   PFAID,24            SET PRESSED PF24 TO ABORT                    
         J     APP300                                                           
                                                                                
         USING TAPDD,R4                                                         
APP200   GOTO1 USEVAL,DMCB,(X'40',TAPDUSE)  VALIDATE USE CODE                   
         TM    TAPDPST1,TAPDPSEL   IF CAST WAS SELECTED                         
         JZ    APP230                                                           
         XC    TGADV,TGADV         THEN DO NOT DISPLAY ADVICE NUMBER            
         NI    APPSTAT,X'FF'-APPADVDN                                           
                                                                                
APP230   MVC   CONREC+1(6),=C' (   )'  SET TO MOVE USE TYPE TO REC.             
         MVC   CONREC+3(3),TAPDUSE     MOVE IT IN                               
         OI    CONRECH+6,X'80'         AND TRANSMIT                             
         MVC   TGCOM,TAPDCOM       SAVE INTERNAL COMMERCIAL NUMBER              
         MVC   PAYPST1,TAPDPST1    SAVE PAYMENT STATUS BYTES                    
         MVC   PAYPST2,TAPDPST2                                                 
                                                                                
         OI    CONRECH+1,X'20'     PROTECT REC/ACT                              
         NI    CONRECH+6,X'FE'     TURN OFF MODIFIED                            
         OI    CONRECH+6,X'80'     (00 UNPROTECTS EACH TIME IN CASE             
         OI    CONACTH+1,X'20'      GLOBAL PF KEY HIT)                          
         NI    CONACTH+6,X'FE'                                                  
         OI    CONACTH+6,X'80'                                                  
                                                                                
         TM    APPSTAT,PFTOAPP     TEST PF13 TO APPROVE BIT ON                  
         JO    APP280                                                           
                                                                                
         BAS   RE,GETSCRN          GET (FIRST/)NEXT SCREEN                      
         BRAS  RE,GETWSCR          OR GET (FIRST/) NEXT WEB SCREEN              
         CLC   GPAGE,LPAGE         GPAGE NOW HAS NEXT PAGE NUMBER               
         JNH   SCRNDIS             MORE TO COME                                 
         TM    PAYPST2,TAPDFWEB                                                 
         JZ    APP250                                                           
         TM    PAYPST1,TAPDPCHG                                                 
         JZ    APP250                                                           
         CLC   GPYMP,LPYMP                                                      
         JNH   SCRNDIS                                                          
APP250   OI    APPSTAT,PFTOAPP     ELSE SET OK TO APPROVE IF PFKEY              
         J     PFAPPR              AND GIVE MESSAGE                             
                                                                                
APP280   CLI   PFAID,13            TEST THEY HIT PF13                           
         JNE   PFAPPR              NO - RE-DISPLAY MESSAGE                      
                                                                                
         NI    CONRECH+1,X'DF'     UNPROTECT REC/ACT                            
         OI    CONRECH+6,X'80'                                                  
         NI    CONACTH+1,X'DF'                                                  
         OI    CONACTH+6,X'80'                                                  
                                                                                
APP300   MVI   OVERLAY,SCR44       RE-LOAD APPROVE BASE SCREEN                  
         LA    R3,CONTAGH                                                       
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVC   CONREC(7),=C'INVOICE'                                            
         MVC   APPAGY,TGAGY        DISPLAY AGENCY                               
         OI    APPAGYH+4,X'20'                                                  
         MVC   APPINV,DINV         AND INVOICE NO.                              
         OI    APPINVH+4,X'20'                                                  
         CLI   PFAID,24            TEST PRESSED PF24 TO ABORT                   
         JE    APP400                                                           
                                                                                
         BRAS  RE,APPRINV          MARK INVOICE APPROVED                        
                                                                                
         OC    CANINV,CANINV       IS THERE ALSO A CANADIAN INVOICE?            
         JZ    APP330                                                           
         MVC   USINV,TGINV         SAVE US INVOICE NUMBER                       
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B0',CANINV) READ INV FOR UPDATE           
                                                                                
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS EL.                      
         L     R4,AIO                                                           
         BRAS  RE,GETEL            IF NOT FOUND                                 
         JE    *+18                                                             
         MVI   PFAID,24            SET PRESSED PF24 TO ABORT                    
         MVC   TGINV,USINV                                                      
         J     APP400                                                           
*                                                                               
         BRAS  RE,APPRINV          MARK CANADIAN INVOICE APPROVED               
         MVC   TGINV,USINV         RESTORE US INVOICE NUMBER                    
*                                                                               
         USING TACOD,R4                                                         
APP330   GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'30',0)  READ COMM REC FOR UPDATE         
         JNE   APP350                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   APP350                                                           
         CLC   TACOPDTE,TGTODAY1   IF LAST PAID DATE NOT SET TO TODAY           
         JE    APP350                                                           
         MVC   TACOPDTE,TGTODAY1   SET LAST PAID DATE                           
         GOTO1 PUTREC              WRITE BACK CHANGED RECORD                    
                                                                                
APP350   LA    R4,ELTAGQ           R4=A(REQ. DETAILS ELEMENT)                   
         BRAS  RE,SETGQEXT         EXTRACT GUAR. TRACKING REQUESTS              
         LA    R4,ELTAGQ2          R4=A(2ND REQ. DETAILS ELEMENT)               
         BRAS  RE,SETGQEXT         EXTRACT GUAR. TRACKING REQUESTS              
                                                                                
         LA    R4,ELTAFQ           R4=A(REQ. DETAILS ELEMENT)                   
         BRAS  RE,FQEXT            EXTRACT FIXED CYCLE TRACKING REQS.           
         LA    R4,ELTAFQ2          R4=A(2ND REQ. DETAILS ELEMENT)               
         BRAS  RE,FQEXT            EXTRACT FIXED CYCLE TRACKING REQS.           
                                                                                
         TM    PAYPST1,TAPDPCRD    IF THIS IS A CREDIT INVOICE                  
         JZ    APP380                                                           
         MVI   APPUAPP,C'A'                                                     
         BRAS  RE,GETCHK           UPDATE DUE COMPANY RECORDS                   
                                                                                
APP380   MVI   BLOCK,9             BUILD SUBSTITUTION BLOCK FOR GETTXT          
         MVI   BLOCK+9,0           WITH APPROVAL TIME                           
         GOTO1 TIMECON,DMCB,TIME,TGTODAY1,(8,BLOCK+1)                           
                                                                                
APP400   ZIC   R1,CALLSP                                                        
         SHI   R1,1                IF HERE BECAUSE OF PUSH                      
         JM    APP500                                                           
         LA    R1,CALLSTCK(R1)                                                  
         CLI   0(R1),SCR46         FROM INVOICE LIST                            
         JNE   APP500                                                           
         MVI   APPSTAT,APPDONE     SET BIT TO POP BACK NEXT TIME                
         J     APPRNXT                                                          
                                                                                
APP500   MVI   APPSTAT,0           CLEAR STATUS BYTE                            
         BRAS  RE,PREAPDAT         PRESENT APPROVE DATE FIELD                   
         OC    SVADATE,SVADATE                                                  
         JZ    APPRVD                                                           
         GOTO1 DATCON,DMCB,(1,SVADATE),(8,APPAPP)                               
         J     APPRVD                                                           
         EJECT                                                                  
                                                                                
SCRNIDIS MVI   MYMSGNO1,86         ISPLIT DISPLAYED - HIT ENTER                 
         B     MSGOUT                                                           
                                                                                
SCRNBDIS MVI   MYMSGNO1,120        BOVER DISPLAYED - HIT ENTER                  
         LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
                                                                                
MSGOUT   LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
*              ROUTINE SHOWS ISPLIT INFORMATION                                 
         SPACE                                                                  
GOISPLT  NTR1                                                                   
         MVI   OVERLAY,SCRD0       LOAD ISPLIT SCREEN                           
         L     R3,EFHTAG                                                        
         GOTO1 LOADSOPH,DMCB,1                                                  
                                                                                
         MVI   OVERLAY,X'CF'       LOAD ISPLIT PHASE                            
         GOTO1 LOADSOPH,DMCB,0                                                  
*                                                                               
         MVI   MODE,VALKEY                                                      
         GOTO1 (R3),DMCB,(RC)      GO TO ISPLIT WITH MODE VALKEY                
*                                                                               
         MVI   MODE,DISPREC                                                     
         GOTO1 (R3),DMCB,(RC)      GO TO ISPLIT WITH MODE DISPREC               
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE SHOWS BOVER INFORMATION                                  
         SPACE                                                                  
GOBOVER  NTR1                                                                   
         MVI   OVERLAY,SCR60       LOAD BOVER SCREEN                            
         L     R3,EFHTAG                                                        
         GOTO1 LOADSOPH,DMCB,1                                                  
                                                                                
         MVI   OVERLAY,X'5A'       LOAD BOVER PHASE                             
         GOTO1 LOADSOPH,DMCB,0                                                  
*                                                                               
         MVI   MODE,VALKEY                                                      
         GOTO1 (R3),DMCB,(RC)      GO TO BOVER WITH MODE VALKEY                 
*                                                                               
         MVI   MODE,DISPREC                                                     
         GOTO1 (R3),DMCB,(RC)      GO TO BOVER WITH MODE DISPREC                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE RETRIEVES SCREEN RECORDS AND REBUILDS IN TWA             
         SPACE 1                                                                
GETSCRN  NTR1                                                                   
         TM    PAYPST2,TAPDFWEB    EXIT IF PAYMENT WAS MADE VIA                 
         JO    XIT                 WEB APPLICATION                              
         SPACE 1                                                                
         LA    R3,KEY              BUILD KEY                                    
         USING TLSCD,R3                                                         
         XC    TLSCKEY,TLSCKEY                                                  
         MVI   TLSCCD,TLSCCDQ      SCREEN RECORD CODE                           
         MVC   TLSCAGY,TGAGY       AGENCY                                       
         MVC   TLSCINV,SCRINV      INVOICE NUMBER                               
         MVC   TLSCPG,GPAGE        PAGE NUMBER                                  
         SPACE 1                                                                
         MVI   TGFULL,X'FF'        INIT. A(1ST FIELD)                           
         SPACE 1                                                                
         GOTO1 HIGH                READ DIRECTORY                               
         CLC   TLSCKEY(TLSCPG-TLSCKEY),KEYSAVE  INSURE RECORD FOR INV.          
         JNE   SCRNLEER            SCREEN RECORDS NO LONGER EXIST               
         SPACE 1                                                                
         MVC   KEYSAVE,TLSCKEY     SAVE KEY WITH PAGE NUMBER FOUND              
         SPACE 1                                                                
         OC    TGADV,TGADV         IF NOT JUST DISPLAYING ADVICE                
         JNZ   GTSC4                                                            
         ZIC   R1,TLSCPG                                                        
         LA    R1,1(R1)            BUMP PAGE FOR NEXT TIME                      
         STC   R1,GPAGE                                                         
         SPACE 1                                                                
GTSC4    GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         L     R4,AIO              R4=A(SCREEN RECORD)                          
         MVC   GSCRN,TLSCSCR       SAVE THIS SCREEN CODE                        
         SPACE 1                                                                
         OC    TGADV,TGADV         IF NOT JUST DISPLAYING ADVICE                
         JNZ   GTSC5                                                            
         CLC   TLSCPG,FPAGE        AND THIS IS FIRST NON-SEL PAGE               
         JNE   GTSC5                                                            
         BRAS  RE,GTGQEL           GET GUAR TRACKING REQ DTLS                   
         BRAS  RE,GTFQEL           GET FIXED CYCLE TRACKING REQ DTLS            
         SPACE 1                                                                
GTSC5    MVI   ELCODE,TASCELQ      SET TO GET SCREEN ELEMENTS                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
GTSC6    BRAS  RE,NEXTEL                                                        
         JNE   GTSC8                                                            
         SPACE 1                                                                
         USING TASCD,R4                                                         
         LA    RE,TASCDATA         RE=A(DATA IN ELEMENT)                        
         L     R0,ATWA                                                          
         AH    R0,TASCDISP         R0=A(LOCATION IN SCREEN)                     
         SPACE 1                                                                
         CLI   TGFULL,X'FF'                                                     
         JNE   *+8                                                              
         ST    R0,TGFULL           SAVE A(FIRST FIELD RESTORED)                 
         SPACE 1                                                                
         ZIC   RF,TASCLEN                                                       
         SHI   RF,TASCLNQ          RF=L'DATA IN ELEMENT                         
         LA    R1,1(RF)            R1=RF+1 (TO MARK EOS)                        
         MVCL  R0,RE               MOVE ELEMENT DATA TO TIA                     
         J     GTSC6               LOOK FOR ANOTHER ELEMENT                     
         SPACE 1                                                                
GTSC8    TM    TLDRSTAT-TLDRD(R3),TLSCSCON  TEST RECORD IS CONTINUED            
         JZ    GTSCX                                                            
         GOTO1 SEQ                                                              
         CLC   TLSCKEY(TLSCSCR-TLSCKEY),KEYSAVE  INSURE SAME PAGE NO.           
         JE    GTSC4                                                            
         DC    H'0'                MISSING CONTINUATION RECORD                  
         SPACE 1                                                                
GTSCX    BRAS  RE,DISADV           DISPLAY ADVICE NUMBER                        
         SPACE 1                                                                
         L     RF,TGFULL           TRANSMIT AND REBUILD RESTORED FIELDS         
         XR    R1,R1                                                            
         XR    RE,RE                                                            
GTSCX2   OI    6(RF),X'80'         TRANSMIT                                     
         AHI   RE,1                                                             
         CLC   GPAGE,FPAGE         IF NOT CAST SELECT SCREEN                    
         JNH   GTSCX3                                                           
         CHI   RE,11                                                            
         JL    GTSCX3                                                           
         OI    7(RF),X'80'         REBUILD                                      
GTSCX3   ICM   R1,1,0(RF)                                                       
         JZ    *+10                UNTIL REACH E-O-S                            
         AR    RF,R1                                                            
         J     GTSCX2                                                           
         SPACE 1                                                                
         CLC   GSCRN,LSCRN         IF SCREEN CHANGED                            
         JE    *+16                                                             
         MVC   1(2,RF),=X'0101'    TURN ON GLOBAL TRANSMIT BITS                 
         MVC   LSCRN,GSCRN         AND SAVE NEW SCREEN CODE                     
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GET ALL CHECKS FOR INVOICE - TO GET DUE COMPANY RECORDS                
***********************************************************************         
         SPACE 1                                                                
GETCHK   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY              BUILD INITIAL CHECK RECORD KEY               
         USING TLCKD,R3                                                         
         MVI   TLCKCD,TLCKCDQ      RECORD CODE                                  
         MVC   TLCKAGY,TGAGY       AGENCY                                       
         MVC   TLCKINV,MYINV       INVOICE NUMBER (UN-COMPLEMENTED)             
         BAS   RE,SETCHK           SET CHECK FILES                              
         GOTO1 HIGH                GET FIRST RECORD                             
         J     CH20                                                             
         SPACE 1                                                                
CH10     GOTO1 SEQ                 GET NEXT RECORD                              
         SPACE 1                                                                
CH20     CLC   TLCKKEY(TLCKSORT-TLCKD),KEYSAVE  IF NO LONGER SAME INV.          
         JE    *+12                                                             
         BAS   RE,SETTAL                        SET TALENT FILES                
         J     XIT                              AND GET OUT                     
         SPACE 1                                                                
         MVC   MYKEY,KEYSAVE       SAVE KEY                                     
         MVC   TGSSN,TLCKSSN       SET GLOBAL SS NUMBER                         
         GOTO1 GETREC              GET FILE RECORD                              
         SPACE 1                                                                
         XC    CORPID,CORPID       THERE MAY BE CORP ID AROUND                  
         L     R4,AIO                                                           
         MVI   ELCODE,TATIELQ      LOOK FOR TAX ID ELEMENT                      
         BRAS  RE,GETEL                                                         
         JNE   *+10                                                             
         USING TATID,R4                                                         
         MVC   CORPID,TATIID       SAVE IT FOR DUE COMPANY LOOK-UP              
         BAS   RE,SETDUE                                                        
         MVC   KEYSAVE,MYKEY       RESTORE KEY                                  
         J     CH10                                                             
         EJECT                                                                  
*        GET DUE COMPAMY RECORD & UPDATE THE STATUS BYTE (TADUSTAT)             
*        APPUAPP - C'A' COMING FROM APPROVE                                     
*                - C'U' COMING FROM UNAPPROVE                                   
         SPACE 1                                                                
SETDUE   NTR1                                                                   
         BAS   RE,SETTAL           SET TALENT FILES                             
         XC    KEY,KEY                                                          
         LA    R3,KEY              INITIALIZE KEY                               
         USING TLDUD,R3                                                         
         MVI   TLDUCD,TLDUCDQ      RECORD CODE                                  
         MVC   TLDUSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         OC    CORPID,CORPID       IF WE HAVE CORP ID                           
         JZ    *+10                                                             
         MVC   TLDUSSN,CORPID      USE IT                                       
         GOTO1 HIGH                                                             
         J     SD20                                                             
         SPACE 1                                                                
SD10     GOTO1 SEQ                 TRY NEXT                                     
         LA    R3,KEY                                                           
         SPACE 1                                                                
SD20     CLC   TLDUKEY(TLDUDUC-TLDUD),KEYSAVE  DUE COMPANY FOUND                
         JNE   SDX                                                              
         SPACE 1                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              GET THE RECORD                               
         L     R4,AIO                                                           
         MVI   ELCODE,TADUELQ      GET DUE COMPANY DETAILS ELEMENT              
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TADUD,R4                                                         
         TM    TADUSTAT,TADUSAUT   TEST THIS RECORD WAS AN AUTO                 
         JZ    SD10                                                             
         CLC   TADUCINV,MYINV      & THIS IS THE CORRECT CRD INV NUMBER         
         JNE   SD10                                                             
         CLI   APPUAPP,C'U'        IF FROM UNAPPROVE                            
         JNE   SD30                                                             
         OI    TADUSTAT,TADUSHLD   TURN ON HOLD STATUS                          
         J     SD40                                                             
         SPACE 1                                                                
SD30     NI    TADUSTAT,X'FF'-TADUSHLD  ELSE TURN OFF HOLD STATUS               
         SPACE 1                                                                
SD40     GOTO1 PUTREC              WRITE BACK RECORD                            
         J     SD10                                                             
         SPACE 1                                                                
SDX      BAS   RE,SETCHK           SET CHECK FILES                              
         J     XIT                                                              
         EJECT                                                                  
*              UTILITIES                                                        
         SPACE 2                                                                
SETCHK   DS    0H                  SET TO USE CHECK FILES                       
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         BR    RE                                                               
         SPACE 2                                                                
SETTAL   DS    0H                  SET TO USE TALENT FILES                      
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         BR    RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PRESENTS APPROVE DATE FIELDS                         *         
***********************************************************************         
                                                                                
PREAPDAT NTR1  BASE=*,LABEL=*                                                   
         TM    PRGSTAT,TESTSYS+FQASYS+CSCSYS                                    
         JZ    XIT                                                              
         CLC   =C'Approve Date',APPHAPP                                         
         JNE   XIT                                                              
         NI    APPHAPPH+1,X'F3'         MAKE NORMAL INTENSITY                   
         NI    APPAPPH+1,X'F3'                                                  
         NI    APPAPPH+1,X'FF'-X'28'    TURN OFF FIELD PROTECTION               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL SAVED STORAGE                               
         SPACE 1                                                                
INVD     DSECT                                                                  
SVRECUSE DS    CL1               SAVED REC/USE                                  
INV      DS    CL6                 FIRST INVOICE NUM FOR RECORD TO ADD          
SCRINV   DS    CL6                 INVOICE NUMBER FOR SCREEN RECORDS            
PINVNUM  DS    PL3                 PACKED INV NUM W/O YEAR,MONTH,& STAT         
NUMINV   DS    XL1                 BINARY NUMBER OF INVOICES TO ADD             
PNUMINV  DS    PL2                 PACKED                                       
*                                                                               
DINV     DS    CL6                 DISPLAY INVOICE NUMBER                       
APPSTAT  DS    XL1                 APPROVAL STATUS BYTE                         
INPROGRS EQU   X'80'               APPROVAL IN PROGRESS                         
PFTOAPP  EQU   X'40'               HIT PFKEY TO APPROVE                         
APPDONE  EQU   X'20'               DONE APPROVING THIS INV - POP BACK           
APPISPDN EQU   X'10'               DONE WITH ISPLIT DISPLAY                     
APPADVDN EQU   X'08'               DONE WITH ADVICE DISPLAY                     
APPBOVDN EQU   X'04'               DONE WITH BOVER DISPLAY                      
NCHKS    DS    F                   NUMBER OF CHECKS ON INVOICE                  
STCHK    DS    F                   STARTING CHECK NUMBER FOR THIS PAGE          
FPAGE    DS    XL1                 FIRST NON-SELECT PAGE NUMBER                 
LPAGE    DS    XL1                 LAST PAGE NUMBER                             
GPAGE    DS    XL1                 CURRENT PAGE NUMBER                          
GSCRN    DS    CL1                 THIS SCREEN CODE                             
LSCRN    DS    CL1                 SAVED LAST SCREEN CODE                       
GPYMP    DS    XL1                 CURRENT PYMT/LIST SCREEN                     
LPYMP    DS    CL1                 LAST PYMT/LIST SCREEN                        
UNAPPRST DS    XL1                 UNAPPROVAL STATUS BYTE                       
PFTOUNAP EQU   X'80'               HIT PFKEY TO UNAPPROVE                       
SVCMNT   DS    CL60                SAVED COMMENT                                
SVDDTE   DS    XL3                 SAVED DUE DATE                               
SVADATE  DS    XL3                 SAVE APPROVE DATE                            
*                                                                               
ELTAGQ   DS    CL256               SAVED GUAR. TRACKING REQ. DTLS EL.           
ELTAGQ2  DS    CL256                                                            
*                                                                               
ELTAFQ   DS    CL256               SAVED FIXED CYCLE TRK REQ DTLS EL.           
ELTAFQ2  DS    CL256                                                            
*                                                                               
INVPTRS  DS    CL((8*L'TLDRREC)+1) INVOICE POINTER BLOCK                        
*                                                                               
SVKEY    DS    CL(L'KEY)           SAVED KEY                                    
SVKEY2   DS    CL(L'KEY)           2ND SAVED KEY                                
SVAGY    DS    CL(L'TGAGY)         SAVED AGENCY                                 
SVINV    DS    XL(L'TGINV)         SAVED INVOICE NUMBER                         
SVSTA2   DS    XL1                 SAVED TAINSTA2                               
AGYSTAT  DS    CL1                 AGENCY STATUS                                
AGYSTA4  DS    CL1                 AGENCY STATUS 4                              
INVSTAT  DS    CL1                 INVOICE STATUS                               
RATSTAT  DS    CL1                 RATES STATUS                                 
*                                                                               
DELSTAT  DS    XL1                 DELETE STATUS                                
PFTODEL  EQU   X'80'               HIT PFKEY TO DELETE                          
FRSTINV  DS    CL6                 FIRST INVOICE TO DELETE                      
LASTINV  DS    CL6                 LAST INVOICE TO DELETE                       
MYKEY    DS    CL(L'KEY)           LAST KEY FOUND FOR DELETE                    
APPUAPP  DS    CL1                 COMING FROM APP/UNAPP                        
PAYPST1  DS    XL1                 PAYMENT STATUS (TAPDPST1)                    
PAYPST2  DS    XL1                 PAYMENT STATUS (TAPDPST2)                    
CORPID   DS    CL9                 CORP ID FOR DUE COMPANY RECORDS              
MYINV    DS    XL6                 UNCOMPLEMENTED INVOICE NUMBER                
COMMENT  DS    CL1                 COMMENT ATTACHED?                            
*                                                                               
TIME     DS    PL3                 APPROVAL TIME                                
CLTSTAT  DS    XL1                 CLIENT STATUS                                
*                                                                               
COMTYPI  DS    C                   COMMENT RECORD TYPE I (PF13)                 
CANINV   DS    XL6                 CANADIAN INVOICE NUMBER                      
USINV    DS    XL6                 US INVOICE NUMBER                            
         ORG   USINV                                                            
USCANINV DS    XL6                 US/CANADIAN INVOICE NUMBER                   
PFDR     DS    CL(L'DELREAS)                                                    
*                                                                               
SVCYCLE  DS    XL6                 SAVED GRT PAYMENT CYCLE                      
*                                                                               
INTFLAG  DS    XL1                 INTERNAL FLAGS                               
RETNO    EQU   X'80'               RETRO SKIP CHANGED FROM Y TO N               
RETYES   EQU   X'40'               RETRO SKIP CHANGED FROM N TO Y               
*                                                                               
A1FIELD  DS    A                   A(FIRST LOADED SCREEN FIELD)                 
*                                                                               
DELFLAG  DS    X                                                                
DELPF23  EQU   X'80'               PF23 WAS PRESSED                             
*                                                                               
CHKSEQ   DS    F                                                                
*                                                                               
SVTMKEY  DS    XL(L'KEY)           SAVED TIMESHEET KEY                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR26D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR44D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR06D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR1FD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS50D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS51D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS58D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS65D                                                       
         EJECT                                                                  
*        ORG   PAYUSNMH                                                         
         ORG   PAYOVLYH                                                         
       ++INCLUDE TAPYS95D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRCFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR60D                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
*                                  SESSION                                      
LINED    DSECT                                                                  
LSPOTH   DS    CL8                                                              
LSPOT    DS    CL2                 N'SPOTS                                      
LDAYH    DS    CL8                                                              
LDAY     DS    CL2                 N'DAYS                                       
LOTH     DS    CL8                                                              
LOT      DS    CL2                 N'OVERTIME HOURS                             
LDTH     DS    CL8                                                              
LDT      DS    CL2                 N'DOUBLE-TIME HOURS                          
LTTH     DS    CL8                                                              
LTT      DS    CL5                 N'TRAVEL TIME HOURS                          
LPDWH    DS    CL8                                                              
LPDW     DS    CL5                 N'PRIOR-DAY WARDROBE HOURS                   
LTAGH    DS    CL8                                                              
LTAG     DS    CL2                 N'TAGS                                       
LREIH    DS    CL8                                                              
LREI     DS    CL1                 REIMBURSED EXPENSES INDICATOR                
LREH     DS    CL8                                                              
LRE      DS    CL10                REIMBURSED EXPENSES                          
LPAYH    DS    CL8                                                              
LPAY     DS    CL10                PAYMENT AMOUNT                               
LSPNHH   DS    CL8                                                              
LSPNH    DS    CL10                AMOUNT SUBJECT TO P&H                        
LMDEDH   DS    CL8                                                              
LMDED    DS    CL10                MISCELLANEOUS DEDUCTION                      
LAGTH    DS    CL8                                                              
LAGT     DS    CL4                 AGENT                                        
LNAMEH   DS    CL8                                                              
LNAME    DS    CL16                NAME                                         
LNAMEX   DS    CL1                                                              
LCAT     DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
LCAM     DS    CL1                 Y=ON, N=OFF CAMERA                           
         DS    CL1                                                              
LCYR     DS    CL3                 CONTRACT YEAR                                
LOPT     DS    CL16                OPTIONS                                      
LCTAG    DS    CL5                 '^CMNT' TAG                                  
LCMTH    DS    CL8                                                              
LCMT     DS    CL30                CHECK COMMENT                                
LINELNQ  EQU   *-LINED                                                          
         EJECT                                                                  
*                                  RADIO PAYMENTS                               
LINRDD   DSECT                                                                  
LRDSPTH  DS    CL8                                                              
LRDSPT   DS    CL2                 N'SPOTS                                      
         DS    CL8                                                              
LRDHRMNH DS    CL8                                                              
LRDHRMN  DS    CL5                 HR.MN                                        
         DS    CL8                                                              
LRDTAGH  DS    CL8                                                              
LRDTAG   DS    CL2                 N'TAGS                                       
         DS    CL8                                                              
LRDAPCH  DS    CL8                                                              
LRDAPC   DS    CL1                 APPLY CODE                                   
         DS    CL8                                                              
LRDAPH   DS    CL8                                                              
LRDAP    DS    CL10                APPLIED AMOUNT                               
         DS    CL8                                                              
LRDREIH  DS    CL8                                                              
LRDREI   DS    CL1                 REIMBURSED EXPENSES INDICATOR                
         DS    CL8                                                              
LRDREH   DS    CL8                                                              
LRDRE    DS    CL10                REIMBURSED EXPENSES                          
         DS    CL8                                                              
LRDPAYH  DS    CL8                                                              
LRDPAY   DS    CL10                PAYMENT AMOUNT                               
         DS    CL8                                                              
LRDSPNHH DS    CL8                                                              
LRDSPNH  DS    CL10                AMOUNT SUBJECT TO P&H                        
         DS    CL8                                                              
LRDMDEDH DS    CL8                                                              
LRDMDED  DS    CL10                MISCELLANEOUS DEDUCTION                      
         DS    CL8                                                              
LRDAGTH  DS    CL8                                                              
LRDAGT   DS    CL4                 AGENT                                        
         DS    CL8                                                              
LRDDATAH DS    CL8                                                              
LRDDATA  DS    0CL42                                                            
LRDNAME  DS    CL16                NAME                                         
LRDNAMEX DS    CL1                                                              
LRDCAT   DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
LRDCAM   DS    CL1                 Y=ON, N=OFF CAMERA                           
         DS    CL1                                                              
LRDCYR   DS    CL3                 CONTRACT YEAR                                
LRDOPT   DS    CL16                OPTIONS                                      
         DS    CL7                                                              
LRDCTAG  DS    CL5                 '^CMNT' TAG                                  
LRDCMTH  DS    CL8                                                              
LRDCMT   DS    CL30                CHECK COMMENT                                
         DS    CL8                                                              
LRDLNQ   EQU   *-LINRDD                                                         
         EJECT                                                                  
*                                  REUSE                                        
LINB5D   DSECT                                                                  
LB5NPH   DS    CL8                                                              
LB5NP    DS    CL1                 NO PAY OPTION                                
         DS    CL8                                                              
LB5ACDEH DS    CL8                                                              
LB5ACDE  DS    CL1                 APPLIED CODE                                 
         DS    CL8                                                              
LB5APPLH DS    CL8                                                              
LB5APPL  DS    CL10                APPLIED AMOUNT                               
         DS    CL8                                                              
LB5ICDEH DS    CL8                                                              
LB5ICDE  DS    CL1                 REIMBURSED EXPENSES INDICATOR                
         DS    CL8                                                              
LB5REIMH DS    CL8                                                              
LB5REIM  DS    CL10                REIMBURSED EXPENSES                          
         DS    CL8                                                              
LB5PAYH  DS    CL8                                                              
LB5PAY   DS    CL10                PAYMENT AMOUNT                               
         DS    CL8                                                              
LB5SPNHH DS    CL8                                                              
LB5SPNH  DS    CL10                AMOUNT SUBJECT TO P&H                        
         DS    CL8                                                              
LB5MDEDH DS    CL8                                                              
LB5MDED  DS    CL10                MISCELLANEOUS DEDUCTION                      
         DS    CL8                                                              
LB5AGTH  DS    CL8                                                              
LB5AGT   DS    CL4                 AGENT                                        
         DS    CL8                                                              
         DS    CL8                                                              
LB5DATA  DS    0CL42                                                            
LB5NAME  DS    CL16                NAME                                         
         DS    CL1                                                              
LB5CAT   DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
LB5CAM   DS    CL1                 Y=ON, N=OFF CAMERA                           
         DS    CL1                                                              
LB5YR    DS    CL3                 CONTRACT YEAR                                
LB5OPTS  DS    CL16                OPTIONS                                      
         DS    CL7                                                              
LB5TAG   DS    CL5                 '^CMNT' TAG                                  
LB5CMNTH DS    CL8                                                              
LB5CMNT  DS    CL30                CHECK COMMENT                                
         DS    CL8                                                              
LB5LNQ   EQU   *-LINB5D            DISPLACEMENT TO NEXT LINE                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094TAGEN26   06/15/15'                                      
         END                                                                    
