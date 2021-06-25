*          DATA SET TAGENE0    AT LEVEL 005 AS OF 06/04/15                      
*PHASE T702E0A,*                                                                
         TITLE 'T702E0 - MARKET MAINTENANCE'                                    
T702E0   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702E0                                                         
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
         SPACE 2                                                                
         BRAS  RE,SETUP            SETUP PROGRAM AND SCREEN                     
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+8                                                              
         BRAS  RE,VK                                                            
         SPACE 1                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+8                                                              
         BRAS  RE,DK                                                            
         SPACE 1                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+8                                                              
         BRAS  RE,DR                                                            
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+8                                                              
         BRAS  RE,VR                                                            
         SPACE 1                                                                
         CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   *+8                                                              
         BRAS  RE,VR                                                            
         SPACE 1                                                                
         CLI   MODE,RECREST        RESTORE RECORD                               
         BNE   *+8                                                              
         BRAS  RE,VR                                                            
         SPACE 1                                                                
         CLI   MODE,XRECADD                                                     
         BNE   *+8                                                              
         BRAS  RE,DR                                                            
         SPACE 1                                                                
         CLI   MODE,XRECPUT                                                     
         BNE   *+8                                                              
         BRAS  RE,DR                                                            
         SPACE 1                                                                
         CLI   MODE,XRECDEL                                                     
         BNE   *+8                                                              
         BRAS  RE,DR                                                            
         SPACE 1                                                                
         CLI   MODE,XRECREST                                                    
         BNE   *+8                                                              
         BRAS  RE,DR                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              EQUATES FOR MASTER RECORD TYPES                                  
CNET     EQU   C'N'                CABLE NETWORK                                
RMKT     EQU   C'R'                RADIO MARKET                                 
CSYS     EQU   C'S'                CABLE SYSTEM                                 
TMKT     EQU   C'T'                TELEVISION MARKET                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              SET UP THE MASTER SCREEN AND PF KEY TABLE                        
         SPACE 1                                                                
SETUP    NTR1  BASE=*,LABEL=*                                                   
         MVC   TGMTTYPE,CONREC   SET MASTER RECORD TYPE                         
         CLI   TGMTTYPE,C'C'     CABLE NET, RADIO MARKET,                       
         BNE   SETUP10           TELEVISION MARKET OR CABLE SYSTEM              
         MVC   TGMTTYPE,CONREC+1                                                
         SPACE 1                                                                
SETUP10  MVC   SMTWGTH,WGHTHD    SET UP WEIGHT FIELD HEADER                     
         SPACE 1                                                                
         MVC   SMTALHD,RKEYHD    FOR TMKT, DISPLAY ALPHA FIELD                  
         NI    SMTALFDH+1,X'FF'-X'20'                                           
         SPACE 1                                                                
         MVC   SMTSWTH,SWGHTHD   FOR TMKT AND RMKT DISPLAY SPAN FIELD           
         NI    SMTSWGTH+1,X'FF'-X'20'                                           
         SPACE 1                                                                
         LA    R3,TPFTAB         SET UP PF TABLE                                
         MVC   SMTPF13,TPFK13    PF13 SCREEN FIELD                              
         MVC   SMTKHD,TKEYHD     KEY FIELD HEADER                               
         MVC   SMTNHD,TNEWHD     AND NEW FIELD HEADER                           
         CLI   TGMTTYPE,TMKT     FOR TELEVISION MARKET RECORDS                  
         BE    SETUP20                                                          
         SPACE 1                                                                
         MVC   SMTALHD,SPACES    FOR NON-TMKT, PROTECT ALPHA FIELD              
         MVC   SMTALFD,SPACES                                                   
         OI    SMTALFDH+1,X'20'                                                 
         SPACE 1                                                                
         LA    R3,RPFTAB         SET UP PF TABLE                                
         MVC   SMTPF13,RPFK13    PF13 SCREEN FIELD                              
         MVC   SMTKHD,RKEYHD     KEY FIELD HEADER                               
         MVC   SMTNHD,RNEWHD     AND NEW FIELD HEADER                           
         CLI   TGMTTYPE,RMKT     FOR RADIO MARKET RECORDS                       
         BE    SETUP20                                                          
         SPACE 1                                                                
         MVC   SMTSWTH,SPACES    FOR NON-TMKT, NON-RMKT                         
         MVC   SMTSWGT,SPACES    PROTECT SPANISH FIELD                          
         OI    SMTSWGTH+1,X'20'                                                 
         SPACE 1                                                                
         LA    R3,NPFTAB         SET UP PF TABLE                                
         MVC   SMTPF13,NPFK13    PF13 SCREEN FIELD                              
         MVC   SMTKHD,NKEYHD     KEY FIELD HEADER                               
         MVC   SMTNHD,NNEWHD     AND NEW FIELD HEADER                           
         CLI   TGMTTYPE,CNET     FOR CABLE NETWORK RECORDS                      
         BE    SETUP20                                                          
         SPACE 1                                                                
         LA    R3,SPFTAB         SET UP PF TABLE                                
         MVC   SMTPF13,SPFK13    PF13 SCREEN FIELD                              
         MVC   SMTKHD,SKEYHD     KEY FIELD HEADER                               
         MVC   SMTNHD,SNEWHD     NEW FIELD HEADER                               
         MVC   SMTWGTH,SUBSHD    AND SUBSCRIBERS HEADER                         
         CLI   TGMTTYPE,CSYS     FOR CABLE SYSTEM RECORDS                       
         BE    SETUP20                                                          
         SPACE 1                                                                
         DC    H'00'                                                            
         SPACE 1                                                                
SETUP20  OI    SMTKHDH+6,X'80'   TRANSMIT KEY HEADER                            
         OI    SMTNHDH+6,X'80'   TRANSMIT NEW HEADER                            
         OI    SMTNEWH+6,X'80'   TRANSMIT NEW FIELD                             
         OI    SMTWGTHH+6,X'80'  TRANSMIT WEIGHT HEADER                         
         OI    SMTPF13H+6,X'80'  TRANSMIT PF KEY FIELD                          
         SPACE 1                                                                
         OI    SMTALHDH+6,X'80'  TRANSMIT ALPHA HEADER                          
         OI    SMTALFDH+6,X'80'  TRANSMIT ALPHA FIELD                           
         SPACE 1                                                                
         OI    SMTSWTHH+6,X'80'  TRANSMIT SPANISH HEADER                        
         OI    SMTSWGTH+6,X'80'  TRANSMIT SPANISH FIELD                         
         SPACE 1                                                                
         NI    SMTNEWH+1,X'FF'-X'20'                                            
         CLI   ACTNUM,ACTNEW     IF NOT ACTION NEW                              
         BE    SETUP30                                                          
         OI    SMTNEWH+1,X'20'   PROTECT NEW FIELD                              
         XC    SMTNHD,SMTNHD     AND CLEAR NEW FIELDS                           
         XC    SMTNEW,SMTNEW                                                    
         MVI   SMTNEWH+5,0                                                      
         SPACE 1                                                                
SETUP30  GOTO1 INITIAL,DMCB,(R3) MAKE INITIAL CALL                              
         SPACE 1                                                                
         CLI   ACTNUM,ACTSEL                                                    
         BE    SETUPX                                                           
         CLC   TGMTTYPE,LASTTYPE   IF RECORD TYPE HAS CHANGED                   
         BE    SETUPX              MAKE GENCON THINK SCREEN                     
         NI    SMTKEYH+1,X'FF'-X'20'            CHANGED TOO                     
         OI    SMTKEYH+4,X'80'                                                  
         MVC   LASTTYPE,TGMTTYPE                                                
         BRAS  RE,VK                                                            
         SPACE 1                                                                
SETUPX   XIT1                                                                   
         SPACE 2                                                                
*              LITERALS FOR KEY FIELD HEADERS                                   
NKEYHD   DC    CL(L'SMTKHD)'Network'                                            
RKEYHD   DC    CL(L'SMTKHD)'Alpha'                                              
SKEYHD   DC    CL(L'SMTKHD)'System'                                             
TKEYHD   DC    CL(L'SMTKHD)'NSI Mkt'                                            
         SPACE 1                                                                
WGHTHD   DC    CL(L'SMTWGTH)'Weight'                                            
SUBSHD   DC    CL(L'SMTWGTH)'Subscbs'                                           
         SPACE 1                                                                
SWGHTHD  DC    CL(L'SMTSWTH)'Sp Wgt'                                            
         SPACE 1                                                                
NNEWHD   DC    CL(L'SMTNHD)'New Network'                                        
RNEWHD   DC    CL(L'SMTNHD)'New Alpha'                                          
SNEWHD   DC    CL(L'SMTNHD)'New System'                                         
TNEWHD   DC    CL(L'SMTNHD)'New NSI Mkt'                                        
         SPACE 1                                                                
NPFK13   DC    CL(L'SMTPF13)'PF13=CNet/List'                                    
RPFK13   DC    CL(L'SMTPF13)'PF13=RMkt/List'                                    
SPFK13   DC    CL(L'SMTPF13)'PF13=CSys/List'                                    
TPFK13   DC    CL(L'SMTPF13)'PF13=TMkt/List'                                    
         SPACE 2                                                                
*              PF TABLE FOR CABLE NETWORK RECORDS                               
NPFTAB   DS    0C                                                               
         DC    AL1(NPF13X-*,13,0,0,0)                                           
         DC    CL3' ',CL8'CNET    ',CL8'LIST    '                               
NPF13X   EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
*              PF TABLE FOR RMARKET RECORDS                                     
RPFTAB   DS    0C                                                               
         DC    AL1(RPF13X-*,13,0,0,0)                                           
         DC    CL3' ',CL8'RMKT    ',CL8'LIST    '                               
RPF13X   EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
*              PF TABLE FOR CABLE SYSTEM RECORDS                                
SPFTAB   DS    0C                                                               
         DC    AL1(SPF13X-*,13,0,0,0)                                           
         DC    CL3' ',CL8'CSYS    ',CL8'LIST    '                               
SPF13X   EQU   *                                                                
         DC    X'FF'                                                            
*              PF TABLE FOR TMARKET RECORDS                                     
TPFTAB   DS    0C                                                               
         DC    AL1(TPF13X-*,13,0,0,0)                                           
         DC    CL3' ',CL8'TMKT    ',CL8'LIST    '                               
TPF13X   EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              VALIDATE THE KEY                                                 
         SPACE 1                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SMTKEYH        R2=A(KEY FIELD)                                
         SPACE 1                                                                
         CLI   5(R2),0           INPUT IS REQUIRED                              
         BE    VKENTFLD                                                         
         SPACE 1                                                                
         BRAS  RE,VCODE          VALIDATE THE KEY FIELD                         
         SPACE 1                                                                
VK40     GOTO1 RECVAL,DMCB,TLMTCDQ,(X'40',(R2))                                 
         SPACE 1                                                                
         NI    DMINBTS,X'FF'-X'08'                                              
         CLI   ACTNUM,ACTREST    IF ACTION RESTORE                              
         BNE   *+8               READ DELETED KEYS                              
         OI    DMINBTS,X'08'                                                    
         SPACE 1                                                                
         GOTO1 HIGH                                                             
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD                      IF NOT ACTION ADD             
         BE    VK50                                                             
         CLC   KEY(TLMTINUM-TLMTD),KEYSAVE        CODE MUST EXIST               
         BNE   VKNTFND                                                          
         SPACE 1                                                                
         CLI   ACTNUM,ACTREST                     IF ACTION RESTORE             
         BNE   VK60                                                             
         TM    KEY+TLDRSTAT-TLDRD,X'80'           IT MUST BE DELETED            
         BZ    VKAEXST                                                          
         B     VKX                                                              
         SPACE 1                                                                
VK50     CLC   KEY(TLMTINUM-TLMTD),KEYSAVE        IF ACTION ADD                 
         BE    VKAEXST                                                          
         MVC   KEY,KEYSAVE                        CODE CANNOT EXIST             
         SPACE 1                                                                
VK60     CLI   ACTNUM,ACTNEW     IF ACTION NEW                                  
         BNE   VKX                                                              
         TM    4(R2),X'20'       AND KEY NOT PREVIOUSLY VALIDATED               
         BNZ   VKX                                                              
         GOTO1 GETREC            GET THE RECORD                                 
         BRAS  RE,DR             AND DISPLAY IT                                 
         SPACE 1                                                                
VKX      OI    4(R2),X'20'                                                      
         XIT1                                                                   
         EJECT                                                                  
*              ERROR MESSAGES                                                   
         SPACE 2                                                                
VKENTFLD MVI   MYMSGNO1,90                                                      
         OI    GENSTAT2,USGETTXT                                                
         B     VKEND                                                            
         SPACE 1                                                                
VKNTFND  MVI   ERROR,NOTFOUND                                                   
         B     VKEND                                                            
         SPACE 1                                                                
VKAEXST  MVI   ERROR,RECEXIST                                                   
         B     VKEND                                                            
         SPACE 1                                                                
VKEND    GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              VALIDATE THE CODE                                                
*              ON ENTRY ... R2=A(CODE FIELD)                                    
         SPACE 1                                                                
VCODE    NTR1  BASE=*,LABEL=*                                                   
         CLI   TGMTTYPE,CSYS                                                    
         BNE   VC10              FOR CABLE SYSTEM RECORDS                       
         CLI   5(R2),4           CODE MUST AT LEAST 4                           
         BL    VCERRINV          ALPHANUMERIC CHARACTERS LONG                   
******** TM    4(R2),X'08'                                                      
******** BZ    VCERRINV                                                         
         B     VCX                                                              
         SPACE 1                                                                
VC10     CLI   TGMTTYPE,CNET                                                    
         BNE   VC20              FOR CABLE NETWORK RECORDS                      
         CLI   5(R2),4           CODE MUST BE 1-4                               
         BH    VCERRINV          ALPHANUMERIC CHARACTERS LONG                   
         B     VCX                                                              
         SPACE 1                                                                
VC20     CLI   TGMTTYPE,RMKT                                                    
         BNE   VC30              FOR RADIO MARKET RECORDS                       
         CLI   5(R2),4           CODE MUST BE 1-4                               
         BH    VCERRINV          APLHANUMERIC CHARACTERS LONG                   
******** TM    4(R2),X'04'                                                      
******** BZ    VCERRINV                                                         
         B     VCX                                                              
         SPACE 1                                                                
VC30     CLI   TGMTTYPE,TMKT                                                    
         BE    *+6               FOR TELEVSION MARKET RECORDS                   
         DC    H'00'             CODE MUST BE 1-4                               
         CLI   5(R2),4           CHARACTERS LONG                                
         BH    VCERRINV                                                         
VCX      XIT1                                                                   
         SPACE 2                                                                
VCERRINV MVI   ERROR,INVALID                                                    
         B     VCEND                                                            
         SPACE 1                                                                
VCERRMIS MVI   ERROR,MISSING                                                    
         B     VCEND                                                            
         SPACE 1                                                                
VCEND    GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DISPLAY THE KEY                                                  
         SPACE 1                                                                
DK       NTR1  BASE=*,LABEL=*                                                   
         USING TLMTD,R3                                                         
         LA    R3,KEY                                                           
         MVI   SMTKEYH+5,4                                                      
         MVC   SMTKEY(4),TLMTCODE                                               
         CLI   TGMTTYPE,CSYS                                                    
         BNE   DK10                                                             
         OI    SMTKEYH+4,X'08'                                                  
         MVI   SMTKEYH+5,6                                                      
         MVC   SMTKEY+4(2),TLMTCODE+4                                           
         DROP  R3                                                               
         SPACE 1                                                                
DK10     CLI   TGMTTYPE,RMKT                                                    
         BNE   DK20                                                             
         OI    SMTKEYH+4,X'04'                                                  
         SPACE 1                                                                
DK20     OI    SMTKEYH+6,X'80'                                                  
         BRAS  RE,VK                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(X'03',SMTNAMEH),(X'80',SMTINTNH)                    
         GOTO1 FLDVAL,DMCB,(X'03',SMTPRE1H),SMTPRE6H                            
         SPACE 1                                                                
         GOTO1 CHAROUT,DMCB,TANAELQ,SMTNAMEH,0                                  
         GOTO1 CHAROUT,DMCB,TASNELQ,SMTALFDH,0                                  
         SPACE 1                                                                
         USING TAMSD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAMSELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DR10     BRAS  RE,NEXTEL                                                        
         BNE   DR20                                                             
         LA    R2,SMTWGHTH                                                      
         CLI   TAMSTYPE,TAMSTYSP                                                
         BNE   *+8                                                              
         LA    R2,SMTSWGTH                                                      
         EDIT  TAMSWGHT,(L'SMTSWGT,8(R2)),ZERO=NOBLANK,ALIGN=LEFT               
         OI    4(R2),X'08'                                                      
         STC   R0,5(R2)                                                         
         B     DR10                                                             
         SPACE 1                                                                
DR20     LA    R2,SMTPRE1H                                                      
         LA    R3,SMTPRE6H                                                      
         SPACE 1                                                                
         USING TAOCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAOCELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DR30     BRAS  RE,NEXTEL                                                        
         BNE   DR40                                                             
         MVC   8(L'TAOCCODE,R2),TAOCCODE                                        
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,R3                                                            
         BNH   DR30                                                             
         DROP  R4                                                               
         SPACE 1                                                                
DR40     GOTO1 ACTVOUT,DMCB,SMTACTVH LAST CHANGED                               
         SPACE 1                                                                
         USING TLMTD,R4                                                         
         CLI   TGCTSTTY,TASTTYPP   IF USER IS A PROGRAMMER                      
         BNE   DR50                DISPLAY INT CNET/MKT/CSYS NUMBER             
         L     R4,AIO                                                           
         LA    R2,SMTINTN                                                       
         MVI   0(R2),C'('                                                       
         GOTO1 HEXOUT,DMCB,TLMTINUM,1(R2),L'TLMTINUM                            
         MVI   9(R2),C')'                                                       
         OI    SMTINTNH+6,X'80'                                                 
         DROP  R4                                                               
         SPACE 1                                                                
DR50     CLI   MODE,XRECADD                                                     
         BE    DR60                                                             
         CLI   MODE,XRECPUT                                                     
         BE    DR60                                                             
         CLI   MODE,XRECDEL                                                     
         BE    DR60                                                             
         CLI   MODE,XRECREST                                                    
         BNE   DRX                                                              
DR60     GOTO1 ADDPTRS,DMCB,PTRBLK                                              
DRX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              VALIDATE THE RECORD                                              
         SPACE 1                                                                
VR       NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTNEW                                                    
         BNE   VR10                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
VR10     GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         CLI   MODE,RECDEL                                                      
         BE    VRX                                                              
         CLI   MODE,RECREST                                                     
         BE    VRX                                                              
         SPACE 1                                                                
         GOTO1 NAMIN,DMCB,TANAELQ,SMTNAMEH                                      
         SPACE 1                                                                
         CLI   TGMTTYPE,TMKT                                                    
         BNE   VR30                                                             
         GOTO1 NAMIN,DMCB,TASNELQ,SMTALFDH                                      
         SPACE 1                                                                
         L     RE,AIO1            ALPHA FIELD MUST BE UNIQUE                    
         MVC   SVKEY,0(RE)                                                      
         LA    R2,SMTALFDH                                                      
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLMTALDQ,(X'24',(R2))                                
         BNE   VR20                                                             
         L     RE,AIO2                                                          
         CLC   SVKEY,0(RE)                                                      
         BNE   VRERRINV                                                         
VR20     GOTO1 RECVAL,DMCB,TLMTCDQ,(X'34',SMTKEYH)                              
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
VR30     MVI   ELCODE,TAMSELQ    DELETE EXISTING WEIGHT ELEMENT                 
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         LA    R2,SMTWGHTH       WEIGHT IS REQUIRED                             
         GOTO1 ANY                                                              
         TM    4(R2),X'08'       MUST BE NUMERIC                                
         BZ    VRERRINV                                                         
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)       CONVERT IT TO BINARY                           
         CVB   RE,DUB                                                           
         SPACE 1                                                                
         USING TAMSD,R4                                                         
         LA    R4,ELEMENT        BUILD NEW WEIGHT DETAILS                       
         XC    ELEMENT,ELEMENT   ELEMENT                                        
         MVI   TAMSEL,TAMSELQ                                                   
         MVI   TAMSLEN,TAMSLNQ                                                  
         ST    RE,TAMSWGHT                                                      
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         CLI   TGMTTYPE,TMKT     IF TMKT                                        
         BE    VR40                                                             
         CLI   TGMTTYPE,RMKT     OR RMKT RECORD                                 
         BNE   VR50                                                             
VR40     LA    R2,SMTSWGTH       VALIDATE SPANISH WEIGHT                        
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         TM    4(R2),X'08'       MUST BE NUMERIC                                
         BZ    VRERRINV                                                         
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)       CONVERT IT TO BINARY                           
         CVB   RE,DUB                                                           
         SPACE 1                                                                
         USING TAMSD,R4                                                         
         LA    R4,ELEMENT        BUILD NEW SPANISH WEIGHT DETAILS               
         XC    ELEMENT,ELEMENT   ELEMENT                                        
         MVI   TAMSEL,TAMSELQ                                                   
         MVI   TAMSLEN,TAMSLNQ                                                  
         MVI   TAMSTYPE,TAMSTYSP                                                
         ST    RE,TAMSWGHT                                                      
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
         SPACE 1                                                                
VR50     GOTO1 ACTVIN,DMCB,0     ADD ACTIVITY ELEMENT                           
         SPACE 1                                                                
         BRAS  RE,INTPRO         DO INTERNAL NUMBER PROCESSING                  
         SPACE 1                                                                
         BRAS  RE,NEWPRO         DO ACTION NEW PROCESSING                       
VRX      XIT1                                                                   
         EJECT                                                                  
*              ERROR MESSAGES                                                   
         SPACE 2                                                                
VRERRINV MVI   ERROR,INVALID                                                    
         B     VREND                                                            
         SPACE 1                                                                
VREND    GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DO INTERNAL NUMBER PROCESSING                                    
         SPACE 1                                                                
INTPRO   NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTADD     IF ACTION ADD                                  
         BNE   IPX                                                              
         SPACE 1                                                                
         USING TLMTD,R3                                                         
         L     R3,AIO1           R3=A(CNET/CYS/MKT RECORD)                      
         SPACE 1                                                                
         MVC   AIO,AIO2          GET SYSTEM RECORD                              
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'B4',TWAAGY)                               
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         L     R4,AIO            R4=A(SYSTEM RECORD)                            
         USING TASYD,R4                                                         
         MVI   ELCODE,TASYELQ    GET SYSTEM CONTROL ELEMENT                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R1,15,TASYLMKT   ADD ONE TO LAST INTERNAL                        
         LA    R1,1(R1)         CNET/MKT/CSYS NUMBER                            
         STCM  R1,15,TASYLMKT                                                   
         GOTO1 PUTREC           AND UPDATE SYSTEM RECORD                        
         MVC   AIO,AIO1                                                         
         DROP  R4                                                               
         SPACE 1                                                                
         STCM  R1,15,TLMTINUM   PUT NUMBER INTO CNET/MKT/CSYS KEY               
         DROP  R3                                                               
IPX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DO ACTION NEW PROCESSING                                         
         SPACE 1                                                                
NEWPRO   NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTNEW    IF ACTION IS NEW                                
         BNE   NPX                                                              
         SPACE 1                                                                
         LA    R2,SMTNEWH       R2=A(NEW FIELD)                                 
         CLI   5(R2),0          INPUT IS REQUIRED                               
         BE    NPERRMIS                                                         
         BRAS  RE,VCODE         VALIDATE THE FIELD                              
         SPACE 1                                                                
         USING TAOCD,R4                                                         
         LA    R4,ELEMENT       ADD THE OLD CNET/CSYS/MKT ELEMENT               
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAOCEL,TAOCELQ                                                   
         MVI   TAOCLEN,TAOCLNQ                                                  
         MVC   TAOCDTE,TGTODAY1                                                 
         XC    TAOCDTE,=X'FFFFFF'                                               
         MVC   TAOCCODE,TGMTCODE                                                
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLMTCDQ,(X'40',(R2))                                 
         GOTO1 HIGH                                                             
         CLC   KEY(TLMTINUM-TLMTD),KEYSAVE                                      
         BE    NPAEXST          NEW CODE CANNOT ALREADY BE ONFILE               
         SPACE 1                                                                
         USING TLMTD,R4                                                         
         L     R4,AIO                                                           
         MVC   TLMTCODE,SMTNEW  PUT NEW CODE INTO THE RECORD                    
         DROP  R4                                                               
         SPACE 1                                                                
         GOTO1 PUTREC                      PUT THE RECORD                       
         GOTO1 ADDPTRS,DMCB,(X'80',PTRBLK) AND CHANGE THE KEYS                  
         SPACE 1                                                                
         MVC   SMTKEY,SMTNEW    MOVE NEW INTO KEY FIELD                         
         OI    SMTKEYH+6,X'80'                                                  
         XC    SMTNEW,SMTNEW    CLEAR NEW FIELD                                 
         OI    SMTNEWH+6,X'80'                                                  
         BRAS  RE,DR            AND DISPLAY THE RECORD                          
NPX      XIT1                                                                   
         SPACE 2                                                                
NPERRMIS MVI   ERROR,MISSING                                                    
         B     NPEND                                                            
         SPACE 1                                                                
NPAEXST  MVI   ERROR,RECEXIST                                                   
         B     NPEND                                                            
         SPACE 1                                                                
NPEND    GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ACTNEW   EQU   20                                                               
         SPACE 1                                                                
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRE0D                                                       
         ORG   SMTWORK                                                          
LASTTYPE DS    CL1                                                              
SVKEY    DS    CL32                                                             
PTRBLK   DS    CL(4*L'TLDRREC+1)                                                
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
WEIGHTSD DSECT                                                                  
WYEARH   DS    XL8                                                              
WYEAR    DS    XL3                                                              
WWEIGHTH DS    XL8                                                              
WWEIGHT  DS    XL4                                                              
WLNQ     EQU   *-WEIGHTSD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005TAGENE0   06/04/15'                                      
         END                                                                    
