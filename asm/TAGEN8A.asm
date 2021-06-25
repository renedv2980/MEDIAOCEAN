*          DATA SET TAGEN8A    AT LEVEL 056 AS OF 11/03/08                      
*PHASE T7028AA,*                                                                
         TITLE 'T7028A - PURCHASE ORDERS'                                       
T7028A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7028A                                                         
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
         CLI   ACTNUM,ACTLIST                                                   
         BE    FT60                                                             
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   *+12                                                             
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    FT20                                                             
         CLI   MODE,XRECPUT        OR AFTER CHANGING RECORD                     
         BE    FT20                                                             
         CLI   MODE,XRECADD        AFTER ADDING RECORD                          
         BNE   XIT                                                              
FT20     BAS   RE,DISPLAY          (RE-)DISPLAY IT                              
FTX      B     XIT                                                              
*======================================================================         
*        LISTING RECORDS                                                        
*======================================================================         
FT60     GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    LVK                                                              
         CLI   MODE,LISTRECS                                                    
         BNE   FTX                                                              
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         B     LR                                                               
*                                                                               
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         CLI   ACTNUM,ACTDEL       IF ACTION DELETE                             
         BNE   VK3                                                              
         CLI   TGCTSTTY,TASTTYPP   ONLY ALLOWED IF PROGRAMMER                   
         BE    VK3                                                              
         CLI   TGCTSTTY,TASTTYP2   SYSTEM MANAGER                               
         BE    VK3                                                              
         CLI   TGCTSTTY,TASTTYP3   EXECUTIVE                                    
         BE    VK3                                                              
         CLI   TGCTSTTY,TASTTYPC   OR CLIENT                                    
         BNE   ERRSEC                                                           
VK3      CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   *+12                                                             
         TM    SPOAGYH+4,X'20'     TEST AGENCY CHANGED                          
         BO    VK4                                                              
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'00',SPOAGYH),0                            
*                                                                               
VK4      LA    R2,SPOPOH           POINT TO PO                                  
         CLI   SPOPOH+5,0          MUST BE SOMETHING THERE                      
         BE    FLDMIS                                                           
         TM    SPOPOH+4,X'20'      TEST PO CHANGED                              
         BO    VK9                                                              
         BAS   RE,BLDKEY           ELSE BUILD TRACKING KEY                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLPUKEY),KEYSAVE                                           
         BNE   NTFOUND                                                          
*                                                                               
VK9      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 1                                                                
DKEY     NTR1                                                                   
         L     R4,AIO                                                           
         USING TLPUD,R4                                                         
         MVC   SPOAGY,TLPUAGY                                                   
         OI    SPOAGYH+6,X'80'                                                  
         MVC   SPOPO,TLPUPO                                                     
         OI    SPOPOH+6,X'80'                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILDS APPROPRIATE PURCHASE ORDER KEY                    
         SPACE 1                                                                
BLDKEY   NTR1                                                                   
         LA    R4,KEY              BUILD PURCHASE ORDER KEY                     
         USING TLPUD,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLPUCD,TLPUCDQ      RECORD CODE                                  
         MVI   TLPUSCD,TLPUSCDQ    SUB-RECORD CODE                              
         MVC   TLPUAGY,SPOAGY      AGENCY                                       
         OC    TLPUAGY,SPACES                                                   
*                                                                               
         MVC   TLPUPO,=CL10'0000000000'                                         
         SR    R1,R1                                                            
         IC    R1,SPOPOH+5                                                      
         LA    RF,10                                                            
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         LA    RF,TLPUPO(RF)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
                                                                                
         MVC   SPOPO,TLPUPO                                                     
         MVI   SPOPOH+5,L'TLPUPO                                                
         OI    SPOPOH+6,X'80'                                                   
*                                                                               
BKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS RECORD                                          
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SPOLIN1H,SPOFINH                                                 
         L     R4,AIO                                                           
         LA    R2,SPOLIN1H                                                      
         USING ITMLIND,R2                                                       
         MVI   ELCODE,TAPUELQ                                                   
         USING TAPUD,R4                                                         
         BAS   RE,GETEL                                                         
         B     DISP020                                                          
DISP010  BAS   RE,NEXTEL                                                        
DISP020  BNE   DISP050                                                          
*                                                                               
         CLI   TAPUTYPE,TAPUTITM                                                
         BNE   DISP010                                                          
         MVC   ITMCODE,TAPUICOD                                                 
         MVC   ITMDESC,TAPUIDES                                                 
         EDIT  TAPUIQTY,(4,ITMQTY)                                              
         EDIT  TAPUIAMT,(12,ITMPRITM),2,ZERO=NOBLANK                            
         L     RF,TAPUIAMT                                                      
         MH    RF,TAPUIQTY                                                      
         EDIT  (RF),(12,ITMSUBT),2,ZERO=NOBLANK                                 
*                                                                               
         OI    ITMHDR+6,X'80'                                                   
         LA    R2,ITMLINLN(R2)     BUMP TO NEXT LINE                            
         B     DISP010                                                          
*                                                                               
DISP050  L     R4,AIO                                                           
         GOTO1 GETL,DMCB,(1,=AL1(TAPUTSUM))                                     
         BNE   DISPX                                                            
*                                                                               
         L     R4,TGELEM                                                        
         MVC   SPOCURR,TAPUSCUR                                                 
         OI    SPOCURRH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(1,TAPUSCDT),(11,SPODATE)                            
         OI    SPODATEH+6,X'80'                                                 
         EDIT  TAPUSAMT,(12,SPONAMT),2,ZERO=NOBLANK                             
         OI    SPONAMTH+6,X'80'                                                 
*                                                                               
DISPX    B     XIT                                                              
         EJECT                                                                  
*=====================================================================          
LVK      DS    0H                                                               
         LA    R2,LPOAGYH                                                       
         TM    4(R2),X'20'         VALIDATED                                    
         BO    LVK2                                                             
         NI    LPOSTRH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2)   VALIDATE AGENCY                       
         MVC   AIO,AIO1                                                         
         OI    4(R2),X'20'         MARK VALIDATED                               
*                                                                               
LVK2     LA    R2,LPOSTRH                                                       
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    LVK10                                                            
*                                                                               
         CLI   5(R2),0             NO INPUT                                     
         BE    LVK10                                                            
*                                                                               
         MVC   KEY,=CL10'0000000000'                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LA    RF,10                                                            
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         LA    RF,KEY(RF)                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
         MVC   8(L'TLPUPO,R2),KEY                                               
         MVI   5(R2),L'TLPUPO                                                   
         OI    6(R2),X'80'                                                      
*        OI    4(R2),X'20'         MARK VALIDATED                               
*                                                                               
LVK10    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*=====================================================================          
LR       DS    0H                                                               
         LA    R3,KEY                                                           
         USING LSTLIND,R2                                                       
         USING TLPUD,R3                                                         
         MVI   TLPUCD,TLPUCDQ      X'24'                                        
         MVI   TLPUSCD,TLPUSCDQ    X'00'                                        
         MVC   TLPUAGY,TGAGY                                                    
         CLI   LPOSTRH+5,0                                                      
         BE    LR05                                                             
         MVC   TLPUPO,LPOSTR                                                    
LR05     GOTO1 HIGH                                                             
         B     LR20                                                             
LR10     GOTO1 SEQ                                                              
LR20     CLC   KEY(TLPUPO-TLPUD),KEYSAVE     MATCH UP TO AGENCY                 
         BNE   XIT                                                              
         MVC   LSTAGY,TLPUAGY                                                   
         MVC   LSTPO,TLPUPO                                                     
         MVC   DMDSKADD,TLDRDA-TLDRD(R3)                                        
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAPUELQ                                                   
         USING TAPUD,R4                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LR30     BAS   RE,NEXTEL                                                        
         BNE   LR90                                                             
         CLI   TAPUTYPE,TAPUTSUM                                                
         BNE   LR30                                                             
*                                                                               
         EDIT  TAPUSAMT,LSTNET,2,ZERO=NOBLANK                                   
         MVC   LPOSTR,TLPUPO                                                    
         OI    LPOSTRH+6,X'80'                                                  
*                                                                               
LR90     GOTO1 LISTMON                                                          
         B     LR10                                                             
         EJECT                                                                  
*              ROUTINE BUILDS RECORD                                            
         SPACE 1                                                                
BLDREC   NTR1                                                                   
*&&DO                                                                           
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
         CLI   5(R2),0                                                          
         BE    BLDR4                                                            
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),X'FF'                                                      
         BE    AMTINV                                                           
         MVC   TAGTCRD,4(R1)                                                    
         SPACE 1                                                                
BLDR4    L     R1,BALANCE          PREVIOUS BALANCE                             
         A     R1,TAGTCRD          + APPLIED AMOUNT                             
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    *+8                                                              
         S     R1,PREVCRD          - PREVIOUS APPLIED AMOUNT                    
         ST    R1,BALANCE          = NEW BALANCE                                
         ST    R1,TAGTBAL                                                       
         SPACE 1                                                                
         OC    ELEMENT,ELEMENT     ANYTHING IN ELEMENT                          
         BZ    BLDR8                                                            
         MVI   TAGTEL,TAGTELQ      YES - FINISH IT UP                           
         MVI   TAGTLEN,TAGTLNQ                                                  
         GOTO1 ADDELEM             AND ADD IT                                   
         SPACE 1                                                                
BLDR8    GOTO1 NAMIN,DMCB,(2,TACMELQ),(X'80',SFTCMNTH),TACMTYPH COMMENT         
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY INFO                            
*&&                                                                             
BLDRX    B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 1                                                                
FLDMIS   MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
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
         DC    X'FF'                                                            
         SPACE 1                                                                
HEXFFS   DC    6X'FF'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
LSTLIND  DSECT                     LIST LINE BREAKOUT                           
         DS    CL1                                                              
LSTAGY   DS    CL6                 AGENCY                                       
         DS    CL2                                                              
LSTPO    DS    CL10                PURCHASE ORDER                               
         DS    CL4                                                              
LSTNET   DS    CL13                NET                                          
*                                                                               
ITMLIND  DSECT                     ITEM LINE BREAKOUT                           
ITMHDR   DS    CL8                 FIELD HEADER                                 
ITMCODE  DS    CL4                 CODE                                         
         DS    CL4                                                              
ITMDESC  DS    CL30                DESCRIPTION                                  
         DS    CL2                                                              
ITMQTY   DS    CL4                 QUANTITY                                     
         DS    CL2                                                              
ITMPRITM DS    CL12                COST PER ITEM                                
         DS    CL2                                                              
ITMSUBT  DS    CL12                CALCULATED SUBTOTAL                          
ITMLINLN EQU   *-ITMHDR                                                         
         EJECT                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR9AD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR9BD                                                       
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
**PAN#1  DC    CL21'056TAGEN8A   11/03/08'                                      
         END                                                                    
