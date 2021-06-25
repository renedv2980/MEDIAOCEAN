*          DATA SET ACPRO44    AT LEVEL 004 AS OF 02/24/15                      
*PHASE T60B44A                                                                  
         TITLE 'T60B44 - PANEL LIST'                                            
T60B44   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B44**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BE    PAN2                                                             
         CLI   MODE,VALREC                                                      
         BE    PAN4                                                             
         B     XIT                                                              
*                                                                               
* VALKEY LOGIC                                                                  
*                                                                               
PAN2     LA    RE,LOCAL                                                         
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VALHED                                                        
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES                                          
         B     XIT                                                              
*                                                                               
* VALREC LOGIC-DISPLAY OR CHANGE                                                
*                                                                               
PAN4     CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    PAN6                                                             
         BAS   RE,PROCPF           PROCESS ANY PF KEYS                          
         BAS   RE,TSTEDT           TEST ANYTHING TO EDIT                        
         BE    PAN10               YES                                          
*                                                                               
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
*                                                                               
* DISPLAY LOGIC                                                                 
*                                                                               
PAN6     GOTO1 VCLEARF,DMCB,PROSEL1H,PROLAST                                    
         GOTO1 (RF),(R1),(1,PROSEL1H),PROPFH                                    
         MVI   LNLISTS,0                                                        
         LA    RE,LSELTAB                                                       
         LA    RF,L'LSELTAB                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
         XC    LLASTPAN,LLASTPAN   CLEAR OUT LAST PANEL LISTED                  
*                                                                               
PAN7     BAS   RE,LIST                                                          
         LA    R2,PROSEL1H                                                      
         CLI   LNLISTS,NLINES      TEST IF SCREEN FILLED                        
         BE    PAN8                YES                                          
         LA    R2,PROSTAH          PUT CURSOR AT FIRST KEY FIELD                
         XC    LLASTPAN,LLASTPAN   NO-MUST BE AT END-OF-LIST                    
         MVC   CONHEAD(L'LIST2MSG),LIST2MSG                                     
         B     PAN9                                                             
*                                                                               
PAN8     MVC   CONHEAD(L'LISTMSG),LISTMSG                                       
*                                                                               
PAN9     ST    R2,ACURFORC                                                      
         B     XIT                                                              
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
PAN10    BAS   RE,EDT                                                           
         LA    R2,PROSEL1H                                                      
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'EDTMSG),EDTMSG                                         
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                                   
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'         INITIALIZE KEY FIELD CHANGE SWITCH           
         LA    R2,PROSTAH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0             TEST FOR START CODE                          
         BE    VALHED2                                                          
         GOTO1 ANY                                                              
         MVC   QSTPANEL,WORK                                                    
*                                                                               
VALHED2  OI    PROSTAH+4,X'20'     SET ON PREV VALID BITS                       
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS PF KEYS - ROUTINE HANDLES SELECT INPUT                 
*                                                                               
PROCPF   NTR1                                                                   
         LA    R2,PROSEL1H         R2=A(SELECT FIELD)                           
         SR    R3,R3                                                            
         ICM   R3,1,LNLISTS                                                     
         BZ    PROCPFX             NOTHING ON SCREEN                            
         LA    R5,LSELTAB          R5=A(SELECT FIELD TABLE)                     
         USING SELTABD,R5                                                       
*                                                                               
PROCPF1  CLI   PFKEY,PF2           TEST FOR PF2=SELECT PANEL MAINT              
         BE    *+12                                                             
         CLI   PFKEY,PF3           TEST PF3=SELECT FIELD MAINT                  
         BNE   PROCPFX                                                          
*                                                                               
         L     RE,ATWA                                                          
         AH    RE,MODLAST                                                       
         LA    R1,CONACTH                                                       
         CR    RE,R1               TEST FOR ANY FIELD MODIFIED                  
         BH    PROCPFX             AFTER ACTION                                 
*                                                                               
         L     RE,ATWA                                                          
         AH    RE,CURDISP          RE=A(CURSOR FIELD)                           
         ST    RE,ACURSOR          SAVE IT                                      
*                                                                               
PROCPF2  BAS   RE,SETLIN                                                        
         L     R2,ASEL                                                          
         C     R2,ACURSOR          TEST IF CURSOR PRESSED IN SELECT             
         BNE   PROCPF6             NO                                           
*                                                                               
         MVC   PANCODE,SELKEY      EXTRACT PANEL CODE                           
         MVI   CALLSP,0                                                         
         CLI   PFKEY,PF3           TEST PF3=FIELD MAINT                         
         BE    PROCPF4                                                          
*                                                                               
         MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'PANEL',=C'CHANGE',(4,PANCODE),0                    
*                                                                               
PROCPF4  MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'FIELD',=C'MAINT',(4,PANCODE),0                     
*                                                                               
PROCPF6  L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,PROCPF2                                                       
*                                                                               
PROCPFX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                            
*                                                                               
* ON EXIT, CC=EQ TO EDIT, CC=NEQ TO CONTINUE DISPLAY                            
*                                                                               
TSTEDT   NTR1                                                                   
         LA    R2,PROSEL1H         R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         ICM   R3,1,LNLISTS                                                     
         BZ    TSTEDTN             NOTHING ON SCREEN                            
*                                                                               
TSTEDT2  CLI   5(R2),0             TEST INPUT IN SELECT FIELD                   
         BE    TSTEDT4             NO                                           
         CLI   8(R2),C'*'          TEST ALREADY PROCESSED                       
         BE    TSTEDT4             YES                                          
         B     TSTEDTY             INPUT IN A SELECT FIELD                      
*                                                                               
TSTEDT4  LA    R4,LISTFLDS-1       R4=FIELD COUNTER                             
*                                                                               
TSTEDT5  BAS   RE,BUMP                                                          
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *+12                YES                                          
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BZ    TSTEDTY             YES                                          
*                                                                               
         BCT   R4,TSTEDT5                                                       
*                                                                               
         BAS   RE,BUMP             ADVANCE TO SELECT                            
         BCT   R3,TSTEDT2          ANOTHER LINE                                 
*                                                                               
TSTEDTN  LTR   RB,RB               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CR    RB,RB               SET CC=EQ                                    
*                                                                               
TSTEDTX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO LIST PANEL RECORDS                                             
*                                                                               
* AT ENTRY, LNLISTS CONTAINS N'LIST LINES ALREADY ON SCREEN                     
*           AND LLASTPAN CONTAINS LAST KEY READ OR BINARY ZERO                  
*                                                                               
LIST     NTR1                                                                   
         LA    R2,PROSEL1H                                                      
         LA    R4,KEY                                                           
         USING ACPNKEY,R4                                                       
*                                                                               
LIST2    ST    R2,ATHISLIN         INITIALIZE LIST LINE POINTER                 
*                                                                               
         XC    ACPNKEY,ACPNKEY                                                  
         MVI   ACPNRTYP,ACPNEQU    PANEL RECORD KEY                             
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   ACPNCODE,QSTPANEL   SET START PANEL                              
*                                                                               
         OC    LLASTPAN,LLASTPAN   TEST RESUMING A READ                         
         BZ    LIST10              YES                                          
         MVC   ACPNCODE,LLASTPAN   SET LAST PANEL DISPLAYED                     
         MVC   ACPNNUM(2),=X'FFFF' AND FORCE READ PAST IT                       
*                                                                               
* FILE READING LOGIC                                                            
*                                                                               
LIST10   GOTO1 HIGH                                                             
         GOTO1 CATCHIOS                                                         
*                                                                               
LIST12   CLC   ACPNKEY(ACPNCODE-ACPNKEY),KEYSAVE  TEST SAME CUL                 
         BNE   LISTX               ALL DONE                                     
*                                                                               
* DISPLAY NEW LIST LINE ON SCREEN                                               
*                                                                               
LIST20   L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         BAS   RE,DISPAN                                                        
*                                                                               
         MVC   LLASTPAN,ACPNCODE                                                
         MVC   ATHISLIN,ANEXTSEL                                                
         ZIC   RE,LNLISTS                                                       
         LR    R1,RE                                                            
         LA    RE,1(RE)            INCREMENT LIST LINES COUNT                   
         STC   RE,LNLISTS                                                       
         MH    R1,=Y(SELTABL)                                                   
         LA    R1,LSELTAB(R1)                                                   
         USING SELTABD,R1                                                       
         MVC   SELKEY,ACPNCODE     SAVE PANEL CODE                              
         MVI   SELACT,C' '                                                      
*                                                                               
         CLI   LNLISTS,NLINES      TEST SCREEN FILLED                           
         BE    LISTX                                                            
*                                                                               
LIST30   MVC   ACPNNUM(2),=X'FFFF'  FORCE NEXT PANEL HEADER                     
         B     LIST10                                                           
*                                                                               
LISTX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY LIST LINE DATA FOR A PANEL                             
*                                                                               
DISPAN   NTR1                                                                   
         L     R4,AIO                                                           
         USING ACPNKEY,R4                                                       
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
DISPAN2  L     R2,ADATA                                                         
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING LSTPROTD,R3                                                      
         MVC   LSTCODE,ACPNCODE                                                 
         MVI   ELCODE,ACPHELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACPHD,R6                                                         
         MVC   LSTNAME,ACPHNAME                                                 
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISPANX  B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE LIST SCREEN                                           
*                                                                               
* NOTE-GENCON DISABLES READ FOR UPDATE FOR ACTION LIST (ACTNUM=ACTLIST)         
*                                                                               
EDT      NTR1  ,                                                                
         LA    R2,PROSEL1H         R2=A(SELECT FIELD)                           
         ZIC   R3,LNLISTS                                                       
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
*                                                                               
EDT2     BAS   RE,SETLIN           SET FIELD HEADER ADCONS                      
         L     R2,ASEL                                                          
*                                                                               
EDT3     TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *+12                YES                                          
         TM    4(R2),X'20'         TEST FOR CHANGED UNPROTECTED FIELD           
         BZ    EDT4                YES                                          
         BAS   RE,BUMP                                                          
         C     R2,ANEXTSEL         TEST IF PAST LINE                            
         BE    EDT8                YES-NOTHING HAS CHANGED                      
         B     EDT3                LOOK AT NEXT FIELD                           
*                                                                               
EDT4     L     R2,ASEL             RESTORE R2=A(SELECT FIELD)                   
         CLI   5(R2),0             TEST ANY SELECT INPUT                        
         BE    EDT8                                                             
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'*'          TEST ALREADY EDITED                          
         BE    EDT8                YES                                          
         CLI   8(R2),C'S'          TEST 'S'=DISPLAY                             
         BE    EDT6                                                             
         CLI   8(R2),C'F'          TEST 'F'=FIELD MAINT                         
         BNE   ERREND                                                           
*                                                                               
EDT6     MVC   SELACT,8(R2)        SAVE ACTION                                  
         MVC   PANCODE,SELKEY      EXTRACT PANEL CODE FOR SELECT                
         MVI   8(R2),C'*'          MARK SELECT FIELD                            
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         XMIT FIELD BACK                              
         MVI   PFKEY,0                                                          
         MVI   CALLSP,0                                                         
         CLI   SELACT,C'F'         TEST SELECTED FIELD MAINT                    
         BE    EDT7                                                             
*                                                                               
         GOTO1 VCALL,WORK,=C'PANEL',=C'CHANGE',(4,PANCODE),0                    
*                                                                               
EDT7     GOTO1 VCALL,WORK,=C'FIELD',=C'MAINT',(4,PANCODE),0                     
*                                                                               
EDT8     L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,EDT2                                                          
*                                                                               
EDTX     B     XIT                                                              
         EJECT                                                                  
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,LISTFLDS                                                      
         LA    R1,ASEL                                                          
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,*-12                                                          
         ST    R2,ANEXTSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
LISTMSG  DC    C'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                    
LIST2MSG DC    C'LIST DISPLAYED'                                                
EDTMSG   DC    C'CHANGES COMPLETED'                                             
         EJECT                                                                  
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0X                                                               
INTMODE  DS    X                   INTERNAL MODE                                
KEYCHG   DS    C                                                                
*                                                                               
QCONTROL DS    0C                                                               
QSTPANEL DS    CL(L'ACPNCODE)                                                   
QCONTRLN EQU   *-QCONTROL                                                       
*                                                                               
SAVERE   DS    A                                                                
ACURSOR  DS    A                                                                
*                                                                               
ATHISLIN DS    A                                                                
ASEL     DS    A                                                                
ADATA    DS    A                                                                
ANEXTSEL DS    A                                                                
*                                                                               
PANCODE  DS    CL(L'ACPNCODE)                                                   
UPDATE   DS    C                                                                
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROB4D                                                       
         SPACE 2                                                                
         ORG   T60BFFD+2304                                                     
LSAVES   DS    0D                                                               
LNLISTS  DS    X                   N'LISTS ON SCREEN                            
LLASTPAN DS    CL(L'ACPNCODE)      LAST PANEL ON SCREEN                         
LSELTAB  DS    CL(NLINES*SELTABL)                                               
         DS    CL((SAVAREA-LSAVES)-(*-LSAVES))  SPARE                           
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
NLINES   EQU   15                  N'LIST SCREEN LINES                          
LISTFLDS EQU   2                   N'FIELDS ON LIST SCREEN LINE                 
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
         SPACE 2                                                                
* DSECT TO COVER LIST LINE PROTECTED FIELD                                      
*                                                                               
LSTPROTD DSECT                                                                  
LSTCODE  DS    CL4                                                              
         DS    CL4                                                              
LSTNAME  DS    CL20                                                             
         SPACE 2                                                                
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELACT   DS    C                   SELECT ACTION                                
SELKEY   DS    CL(L'ACPNCODE)                                                   
SELTABL  EQU   *-SELTABD                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACPRO44   02/24/15'                                      
         END                                                                    
