*          DATA SET TAGEN16    AT LEVEL 059 AS OF 06/27/12                      
*PHASE T70216A,*                                                                
         TITLE 'T70216 - PRODUCT MAINTENANCE'                                   
T70216   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70216                                                         
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
         CLI   MODE,VALKEY         IF MODE IS VALIDATE KEY                      
         BNE   PRD10                                                            
         SPACE 1                                                                
         TM    TGCTSTST,TGCTSCLI   IF STAFF IS CLIENT                           
         BNO   PRD00                                                            
         GOTO1 FLDVAL,DMCB,(X'08',SPRADDRH),(X'80',SPRPPRDH)                    
         GOTO1 FLDVAL,DMCB,(X'08',SPRSTATH),(X'80',999) PROTECT FLDS            
*                                  DON'T PROTECT PGROUP OR PTYPE                
         SPACE 1                                                                
PRD00    MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY                           
         MVC   SVTGCLI,TGCLI                   CLIENT                           
         LA    R2,SPRAGYH                                                       
         TM    TGCTSTST,TGCTSCLI   IF STAFF IS CLIENT                           
         BNO   PRD01                                                            
         CLI   ACTNUM,ACTADD       & IF ACTION IS ADD                           
         BE    PRD0                                                             
         CLI   ACTNUM,ACTCHA       OR CHANGE                                    
         BNE   PRD01                                                            
         SPACE                                                                  
PRD0     CLI   5(R2),0             MUST HAVE AGENCY (& CLIENT)                  
         BE    FLDMISS             & GIVE ERROR - MISSING INPUT                 
         SPACE                                                                  
PRD01    TM    SCRSTAT,SCRCHG      IF FIRST TIME IN                             
         BZ    PRD3                                                             
         CLI   5(R2),0             AND THERE'S NO AGENCY INPUT                  
         BNE   PRD5                                                             
         CLI   SPRCLIH+5,0         AND THERE'S NO CLIENT INPUT                  
         BNE   FLDMISS                                                          
         CLI   SPRPRDH+5,0         AND THERE'S NO PRODUCT INPUT                 
         BNE   PRD4                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'0C',SPRCLIH),SPRCLINH CLIENT              
         BE    PRD1                BRANCH IF VALID CLIENT                       
         XC    SPRCLI,SPRCLI       ELSE CLEAR SCREEN                            
         MVI   SPRCLIH+5,0         AND SET LENGTH TO 0                          
         XC    SPRAGY,SPRAGY       IN CASE RECVAL FILLED IT IN                  
         MVI   SPRAGYH+5,0                                                      
         B     PRD9                                                             
         SPACE                                                                  
         USING TLCLD,R3                                                         
PRD1     MVC   SVTGCLI,TGCLI       SAVE GLOBAL CLIENT FOR RESTORE LATER         
         LA    R3,KEY                                                           
         OC    TLCLAGY,TLCLAGY     IF RECORD HAS AGENCY                         
         BZ    PRD2                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SPRAGYH),SPRAGYNH AGENCY              
         MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY FOR RESTORE LATER         
         B     PRD9                                                             
         SPACE                                                                  
PRD2     XC    SPRCLI,SPRCLI       IF RECORD DOESN'T HAVE AGENCY                
         XC    SPRCLIN,SPRCLIN     CLEAR CLIENT FROM SCREEN                     
         MVI   SPRCLIH+5,0                                                      
         XC    TGCLI,TGCLI         AND GLOBAL CLIENT AND AGENCY                 
         XC    TGAGY,TGAGY                                                      
         B     PRD9                BUILD KEY FOR GLOBAL PRODUCT                 
         SPACE                                                                  
PRD3     XC    SPRAGYN,SPRAGYN     NOT FIRST TIME, CLEAR AGENCY NAME            
         OI    SPRAGYNH+6,X'80'    IN CASE NO AGENCY                            
         LA    R2,SPRAGYH                                                       
         CLI   5(R2),0             IF THERE'S NO AGENCY INPUT                   
         BNE   PRD5                                                             
         CLI   SPRCLIH+5,0                                                      
         BNE   FLDMISS             ERROR IF THERE'S CLIENT INPUT                
PRD4     XC    TGAGY,TGAGY         CLEAR GLOBAL AGENCY                          
         B     PRD6                                                             
PRD5     GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SPRAGYH),SPRAGYNH AGENCY              
         MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY FOR RESTORE LATER         
PRD6     XC    SPRCLIN,SPRCLIN     CLEAR CLIENT NAME                            
         OI    SPRCLINH+6,X'80'    IN CASE NO CLIENT                            
         LA    R2,SPRCLIH                                                       
         CLI   5(R2),0             IF NO CLIENT INPUT                           
         BNE   PRD7                                                             
         CLI   SPRAGYH+5,0                                                      
         BNE   FLDMISS             ERROR IF THERE'S AGENCY INPUT                
         XC    TGCLI,TGCLI         CLEAR GLOBAL CLIENT                          
         B     PRD9                                                             
         SPACE                                                                  
         USING TLCLD,R3                                                         
PRD7     GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',SPRCLIH),SPRCLINH CLIENT              
         LA    R3,KEY                                                           
         OC    TLCLAGY,TLCLAGY                                                  
         BNZ   PRD8                                                             
         LA    R2,SPRCLIH                                                       
         XC    SPRCLIN,SPRCLIN                                                  
         B     NOREC                                                            
         SPACE                                                                  
PRD8     MVC   SVTGCLI,TGCLI       SAVE GLOBAL CLIENT FOR RESTORE LATER         
PRD9     LA    R2,SPRPRDH                                                       
         CLI   8(R2),C' '                                                       
         BE    FLDINV                                                           
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'40',SPRPRDH)                              
         MVC   TGAGY,SVTGAGY       RESTORE GLOBAL AGENCY                        
         MVC   TGCLI,SVTGCLI                      CLIENT                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
PRD10    CLI   THISLSEL,C'D'       IF DELETING FROM A LIST                      
         BE    *+12                DON'T DISPLAY TILL XRECDEL                   
         CLI   MODE,DISPREC                                                     
         BE    PRD15                                                            
         CLI   MODE,XRECADD                                                     
         BE    PRD12               IF MODE IS NEW RECORD ADDED                  
         CLI   MODE,XRECDEL                                                     
         BE    PRD12               OR RECORD DELETED                            
         CLI   MODE,XRECREST                                                    
         BE    PRD12               OR RESTORED                                  
         CLI   MODE,XRECPUT        OR CHANGED                                   
         BNE   PRD20                                                            
         SPACE                                                                  
         L     RE,AIO              IF STATUS BYTE CHANGED,                      
         USING TLPRD,RE            FORCE TO CHANGE ALL                          
         CLC   KEY+TLDRSTAT-TLDRKEY(1),TLPRSTAT                                 
         BE    PRD12                                                            
         GOTO1 ADDPTRS,DMCB,(X'A0',PTRS)                                        
         B     PRD13                                                            
         DROP  RE                                                               
*                                                                               
PRD12    GOTO1 ADDPTRS,DMCB,PTRS   HANDLE PASSIVE PTRS                          
PRD13    BRAS  RE,NFYVIT           AND NOTIFY VITA OF ACTION                    
         SPACE                                                                  
PRD15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
PRD20    CLI   MODE,VALREC         IF MODE IS VALIDATE RECORD                   
         BE    PRD25                                                            
         CLI   MODE,RECDEL         OR DELETE                                    
         BE    PRD25                                                            
         CLI   MODE,RECREST        OR RESTORE                                   
         BNE   PRD30                                                            
         SPACE                                                                  
PRD25    GOTO1 SAVPTRS,DMCB,PTRS   SAVE PASSIVE PTRS                            
         SPACE                                                                  
         CLI   MODE,RECDEL         IF DELETING                                  
         BE    CHKDEL              CHECK IF OK FOR DELETION                     
         CLI   MODE,RECREST                                                     
         BE    XIT                 XIT IF MODE IS RESTORE                       
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         EJECT                                                                  
         USING TLPRD,R3                                                         
PRD30    CLI   MODE,DISPKEY        DISPLAY KEY FOR SELECT                       
         BNE   XIT                                                              
         TM    TGCTSTST,TGCTSCLI   IF STAFF IS CLIENT                           
         BNO   PRD31                                                            
         GOTO1 FLDVAL,DMCB,(X'08',SPRADDRH),(X'80',SPRPPRDH)                    
         GOTO1 FLDVAL,DMCB,(X'08',SPRSTATH),(X'80',999) PROTECT FLDS            
*                                  DON'T PROTECT PGROUP OR PTYPE                
         SPACE 1                                                                
PRD31    MVC   SVKEY,KEY           SAVE KEY AND ADDRESS FOR DELETE              
         L     R3,AIO                                                           
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY                           
         MVC   SVTGCLI,TGCLI                   CLIENT                           
         OC    TLPRAGY,TLPRAGY     IF THERE'S AN AGENCY IN THE RECORD           
         BZ    PRD33                                                            
         MVC   SPRAGY,TLPRAGY      MOVE IT TO SCREEN                            
         OI    SPRAGYH+6,X'80'                                                  
         MVI   SPRAGYH+5,6         SET LENGTH FOR RECVAL                        
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SPRAGYH),SPRAGYNH AGENCY              
         MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY FOR RESTORE LATER         
         B     PRD35                                                            
         SPACE 1                                                                
PRD33    XC    TGAGY,TGAGY         IF NO AGENCY, CLEAR GLOBAL AGENCY            
         XC    SPRAGY,SPRAGY       CLEAR AGENCY ON SCREEN                       
         OI    SPRAGYH+6,X'80'                                                  
         XC    SPRAGYN,SPRAGYN     CLEAR AGENCY NAME ON SCREEN                  
         OI    SPRAGYNH+6,X'80'                                                 
         SPACE 1                                                                
PRD35    OC    TLPRCLI,TLPRCLI                                                  
         BZ    PRD40                                                            
         MVC   SPRCLI,TLPRCLI      CLIENT                                       
         OI    SPRCLIH+6,X'80'                                                  
         MVI   SPRCLIH+5,6         SET LENGTH FOR RECVAL                        
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',SPRCLIH),SPRCLINH CLIENT              
         MVC   SVTGCLI,TGCLI       SAVE GLOBAL CLIENT FOR RESTORE LATER         
         B     PRD45                                                            
         SPACE 1                                                                
PRD40    XC    TGCLI,TGCLI         IF NO CLIENT, CLEAR GLOBAL CLIENT            
         XC    SPRCLI,SPRCLI       CLEAR CLIENT ON SCREEN                       
         OI    SPRCLIH+6,X'80'                                                  
         XC    SPRCLIN,SPRCLIN     CLEAR CLIENT NAME ON SCREEN                  
         OI    SPRCLINH+6,X'80'                                                 
         SPACE 1                                                                
PRD45    MVC   SPRPRD,TLPRPRD      PRODUCT                                      
         OI    SPRPRDH+6,X'80'                                                  
         MVC   TGAGY,SVTGAGY       RESTORE GLOBAL AGENCY                        
         MVC   TGCLI,SVTGCLI                      CLIENT                        
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         SPACE 1                                                                
         TM    TGCTSTST,TGCTSCLI   IF STAFF IS CLIENT                           
         BNO   PRDX                                                             
         CLI   THISLSEL,C'C'       IF CHANGING FROM A LIST                      
         BNE   PRDX                                                             
         CLI   SPRAGYH+5,0         MUST HAVE AGENCY (& CLIENT)                  
         BNE   PRDX                                                             
         LA    R2,SPRAGYH          POINT TO AGENCY FIELD                        
         B     GLOBERR             & GIVE ERROR - CAN'T CHANGE GLBL PRD         
         SPACE 1                                                                
PRDX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SPRNAMEH                                                         
         XC    SPRPRGN,SPRPRGN                                                  
         OI    SPRPRGNH+6,X'80'                                                 
         XC    SPRPPRN,SPRPPRN                                                  
         OI    SPRPPRNH+6,X'80'                                                 
         XC    SPRPRTN,SPRPRTN                                                  
         OI    SPRPRTNH+6,X'80'                                                 
         XC    SPRSTAT,SPRSTAT                                                  
         OI    SPRSTATH+6,X'80'                                                 
         GOTO1 CHAROUT,DMCB,TANAELQ,SPRNAMEH        PRODUCT NAME                
         GOTO1 (RF),(R1),TASNELQ,SPRSNMEH           SHORT NAME                  
**NO-OP  GOTO1 (RF),(R1),TAADELQ,(4,SPRADDRH)       BILLING ADDRESS             
         GOTO1 (RF),(R1),TAFNELQ,SPRANMEH,TAFNTATT  ATTENTION NAME              
         GOTO1 (RF),(R1),TACMELQ,SPREMAIH,TACMTYPI  EMAIL ADDRESS               
         GOTO1 ACTVOUT,DMCB,SPRLCHGH                LAST CHANGED                
*                                                                               
         BAS   RE,DISADDR          DISPLAY BILLING ADDRESS                      
*                                                                               
         L     R4,AIO                                                           
         USING TLPRD,R4                                                         
         MVI   SPRBRND,C'N'                                                     
         TM    TLPRSTAT,TLPRSBRD                                                
         BZ    *+8                                                              
         MVI   SPRBRND,C'Y'        BRAND PRODUCT                                
         OI    SPRBRNDH+6,X'80'                                                 
         DROP  R4                                                               
         SPACE                                                                  
         USING TAISD,R4                                                         
         MVI   ELCODE,TAISELQ      GET PRODUCTION INTERFACE SUBS EL             
         LA    R1,TAISTYPP                                                      
         STC   R1,BYTE                                                          
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BNE   DISP10                                                           
         L     R4,TGELEM                                                        
         CLC   TAISCDE,SPACES      IF HAVE PRODUCTION PRD                       
         BE    DISP10                                                           
         MVC   SPRPPRD,TAISCDE     MOVE TO SCREEN                               
         CLI   SPRAGYH+5,0         IF HAVE AGENCY                               
         BE    DISP10                                                           
         CLI   SPRCLIH+5,0         AND CLIENT                                   
         BE    DISP10                                                           
         BAS   RE,GETPCLI          GET PRODUCTION CLIENT INFO                   
         BE    DISP8                                                            
         CLI   SPRCLIH+5,3         IF NO PRODUCTION CLIENT                      
         BH    DISP10              SKIP TO NEXT FIELD IF L'CLIENT > 3           
         MVC   SVPCLI,SPRCLI       ELSE TRY CLIENT AS THE PRODCTN CLI           
DISP8    BAS   RE,GETPPRD          GET PRODUCTION PRODUCT INFO                  
*                                                                               
         USING TAPID,R4                                                         
DISP10   MVI   ELCODE,TAPIELQ      GET PRODUCT INFORMATION ELEMENT              
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DISP40                                                           
         OC    TAPIPRG,TAPIPRG                                                  
         BZ    DISP15                                                           
         MVC   SPRPRG,TAPIPRG                                                   
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLPGCDQ,(X'8C',SPRPRG),SPRPRGNH                      
         MVC   AIO,AIO1            RESTORE AIO                                  
DISP15   CLI   TAPILEN,TAPILNQ2    NEW LENGTH?                                  
         BNE   DISP20                                                           
         OC    TAPIPTYP,TAPIPTYP                                                
         BZ    DISP20                                                           
         MVC   SPRPRT,TAPIPTYP                                                  
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLPTCDQ,(X'8C',SPRPRT),SPRPRTNH                      
         MVC   AIO,AIO1            RESTORE AIO                                  
DISP20   LA    R2,SPRSTAT          R2=A(STATUS FIELD)                           
         OC    TAPIAC,TAPIAC                                                    
         BZ    DISP35                                                           
         MVC   SPRSTAT(3),=C'AC='                                               
         CLI   TAPIAC,X'FF'       COMMISSION RATE OF FF MEANS 0                 
         BNE   *+14                                                             
         MVC   SPRSTAT+3(4),=C'0.00'                                            
         B     DISP30                                                           
         EDIT  (2,TAPIAC),(6,SPRSTAT+3),2,ALIGN=LEFT,ZERO=NOBLANK               
DISP30   OI    SPRSTATH+6,X'80'    COMMISSION RATE                              
         LA    R2,6(R2)                                                         
         CLI   0(R2),C' '          AND POINT R2 TO LAST NON-SPACE               
         BH    *+8                                                              
         BCT   R2,*-8                                                           
DISP35   TM    TAPISTAT,TAPISLCK   IF CLIENT IS LOCKED                          
         BZ    DISP38                                                           
         CLI   0(R2),C' '          THEN IF NOT FIRST CODE                       
         BNH   *+12                                                             
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
         MVC   0(6,R2),=C'LOCKED'                                               
*                                                                               
         LA    R2,6(R2)                                                         
         CLI   0(R2),C' '          AND POINT R2 TO LAST NON-SPACE               
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
DISP38   TM    TAPISTAT,TAPIJPCY   IF STATUS IS CSF                             
         BZ    DISP39                                                           
         CLI   0(R2),C' '          THEN IF NOT FIRST CODE                       
         BNH   *+12                                                             
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
         MVC   0(3,R2),=C'CSF'                                                  
*                                                                               
         LA    R2,6(R2)                                                         
         CLI   0(R2),C' '          AND POINT R2 TO LAST NON-SPACE               
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
DISP39   TM    TAPISTAT,TAPIJPCN   IF STATUS IS NOCSF                           
         BZ    DISP40                                                           
         CLI   0(R2),C' '          THEN IF NOT FIRST CODE                       
         BNH   *+12                                                             
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
         MVC   0(5,R2),=C'NOCSF'                                                
*                                                                               
         USING TANUD,R4                                                         
DISP40   MVI   ELCODE,TANUELQ      GET PRODUCT INFORMATION ELEMENT              
         GOTO1 GETL,DMCB,(1,=AL1(TANUTMAT))    MATERIAL CODE                    
         BNE   DISP50                                                           
         L     R4,TGELEM                                                        
         ZIC   R1,TANULEN                                                       
         SH    R1,=Y(TANULNQ+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SPRMATC(0),TANUMBER                                              
         OI    SPRMATCH+6,X'80'                                                 
*                                                                               
DISP50   GOTO1 GETL,DMCB,(1,=AL1(TANUTCTR))    COST CENTER                      
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         ZIC   R1,TANULEN                                                       
         SH    R1,=Y(TANULNQ+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SPRCCTR(0),TANUMBER                                              
         OI    SPRCCTRH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE BILLING ADDRESS                                      
*              NOTE -  WE CAN'T USE CHAROUT BECAUSE IT WON'T DISPLAY ON         
*              PROTECTED FIELDS - (CLIENT LEVEL ONLY)                           
         SPACE 1                                                                
DISADDR  NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'01',SPRADDRH),4      CLEAR ADDRESS LINES          
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ      ADDRESS ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DADDX                                                            
         USING TAADD,R4                                                         
         LA    R2,SPRADDRH                                                      
         LA    R3,TAADADD                                                       
         ZIC   R0,TAADLNES         NUMBER OF ADDRESS LINES                      
         B     DADD20                                                           
DADD10   ZIC   RE,0(R2)            BUMP TO NEXT ADDRESS LINE ON SCREEN          
         AR    R2,RE                                                            
         LA    R3,L'TAADADD(R3)    BUMP TO NEXT LINE OF ADDRESS                 
DADD20   MVC   8(L'TAADADD,R2),0(R3)     DISPLAY ADDRESS LINE                   
         OI    6(R2),X'80'         TRANSMIT                                     
         BCT   R0,DADD10           ANY MORE LINES?                              
DADDX    B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    BLDR0               SAVE VITA-NOTIFYING MQ MESSAGE               
         GOTOR BLDMQMSG,DMCB,(X'80',0)     BASED ON INITIAL STATE               
*                                                                               
BLDR0    GOTO1 NAMIN,DMCB,TANAELQ,SPRNAMEH PRODUCT NAME                         
         GOTO1 (RF),(R1),TASNELQ,(X'80',SPRSNMEH) OPTIONAL SHORT NAME           
         SPACE                                                                  
         L     R4,AIO                                                           
         USING TLPRD,R4                                                         
         NI    TLPRSTAT,X'FF'-TLPRSBRD                                          
         LA    R2,SPRBRNDH         BRAND PRODUCT?                               
         CLI   5(R2),0             DEFAULT TO NO                                
         BE    BLDR1                                                            
         CLI   8(R2),C'N'          INPUT MUST BE Y OR N                         
         BE    BLDR1                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TLPRSTAT,TLPRSBRD   INDICATE SO                                  
         DROP  R4                                                               
*                                                                               
BLDR1    TM    TGCTSTST,TGCTSCLI   IF STAFF IS CLIENT                           
         BNO   BLDR1B                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAPIELQ      GET PRODUCT INFO ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   BLDR5A                                                           
         USING TAPID,R4                                                         
         XC    ELEMENT,ELEMENT                                                  
         ZIC   RE,TAPILEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R4)   SAVE OLD ELEMENT                              
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,TAPIELQ     DELETE CURRENT PRODUCT INFORMATION            
         GOTO1 REMELEM            ELEMENT                                       
*                                                                               
         LA    R4,ELEMENT                                                       
         USING TAPID,R4                                                         
         XC    TAPIPRG,TAPIPRG    CLEAR PGROUP AND PTYPE                        
         XC    TAPIPTYP,TAPIPTYP                                                
         B     BLDR10                                                           
         DROP  R4                                                               
*                                                                               
BLDR1B   GOTO1 ADDRIN,DMCB,(X'80',SPRADDRH) BILLING ADDRESS                     
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SPRANMEH),TAFNTATT ATTENTION           
         BAS   RE,VREMAIL          VALIDATE OPTIONAL EMAIL ADDRESS              
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL            IF ADDRESS - MAKE SURE AT LEAST              
         BNE   BLDR2               TWO ADDRESS LINES                            
         USING TAADD,R4                                                         
         CLI   TAADLNES,2          THIS IS TO PREVENT ATTN NAME INPUT           
         BL    ERRADD              BY MISTAKE                                   
*                                                                               
BLDR2    MVI   ELCODE,TAISELQ      PRODUCT INTERFACE SUBSIDIARY ELEMENT         
         GOTO1 REMELEM             DELETE CURRENT                               
         SPACE                                                                  
         LA    R2,SPRPPRDH                                                      
         CLI   5(R2),0             IF THERE'S PRODUCTION PRODUCT                
         BE    BLDR5                                                            
         CLI   SPRAGYH+5,0         MUST HAVE AGENCY                             
         BE    NOINPUT                                                          
         CLI   SPRCLIH+5,0         AND CLIENT                                   
         BE    NOINPUT                                                          
         BAS   RE,GETPCLI          GET PRODUCTION CLIENT INFO                   
         BE    BLDR3                                                            
         CLI   SPRCLIH+5,3         IF NO PRODUCTION CLIENT                      
         BH    NOINPUT             ERROR IF L'CLIENT > 3                        
         MVC   SVPCLI,SPRCLI       ELSE TRY CLIENT AS THE PRODCTN CLI           
BLDR3    BAS   RE,GETPPRD          GET PRODUCTION PRODUCT INFO                  
         BNE   THEEND              ERROR SET BY READACC                         
         SPACE                                                                  
         USING TAISD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         MVI   TAISEL,TAISELQ                                                   
         MVI   TAISLEN,TAISLNQ                                                  
         MVI   TAISTYPE,TAISTYPP                                                
         MVC   TAISCDE,SPRPPRD                                                  
         OC    TAISCDE,SPACES                                                   
         GOTO1 ADDELEM             ADD NEW ELEMENT                              
         SPACE                                                                  
BLDR5    MVI   ELCODE,TAPIELQ      DELETE CURRENT PRODUCT INFORMATION           
         GOTO1 REMELEM             ELEMENT                                      
         SPACE                                                                  
         CLI   SPRSTATH+5,0        ANYTHING IN PRODUCT GROUP, TYPE, OR          
         BNE   BLDR6               STATUS FIELD?                                
BLDR5A   CLI   SPRPRTH+5,0         IF CLIENT, JUST PTYPE OR PGROUP              
         BNE   BLDR6                                                            
         CLI   SPRPRGH+5,0                                                      
         BE    BLDR50                                                           
         SPACE                                                                  
         USING TAPID,R4                                                         
BLDR6    LA    R4,ELEMENT          YES ... THEN BUILD NEW PRODUCT               
         XC    ELEMENT,ELEMENT     INFORMATION ELEMENT                          
         MVI   TAPIEL,TAPIELQ                                                   
         MVI   TAPILEN,TAPILNQ2                                                 
         SPACE                                                                  
BLDR10   LA    R2,SPRPRGH          PRODUCT GROUP?                               
         CLI   5(R2),0                                                          
         BE    BLDR20                                                           
         CLI   SPRAGYH+5,0         AGENCY REQUIRED FOR PGROUP                   
         BE    NEEDAGY                                                          
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLPGCDQ,(R2)    PRODUCT GROUP                        
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   TAPIPRG,TGPRG       COPY PRODUCT GROUP INTO ELEMENT              
         SPACE 1                                                                
BLDR20   LA    R2,SPRPRTH          PRODUCT TYPE?                                
         CLI   5(R2),0                                                          
         BE    BLDR30                                                           
         OC    SPRPRT,SPACES                                                    
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLPTCDQ,(R2)    PRODUCT TYPE                         
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   TAPIPTYP,TGPRT      COPY PRODUCT TYPE INTO ELEMENT               
*                                                                               
BLDR30   TM    TGCTSTST,TGCTSCLI   IF STAFF IS CLIENT                           
         BO    BLDR40              DON'T ALLOW CLIENT TO UPDATE STATUS          
*                                                                               
         BRAS  RE,BLDSTAT                                                       
*                                                                               
BLDR40   GOTO1 ADDELEM             ADD NEW ELEMENT                              
         DROP  R4                                                               
*                                                                               
BLDR50   DS    0H                                                               
         MVI   ELCODE,TANUELQ      DELETE OLD MATERIAL CODE + CC                
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SPRMATCH                                                      
         CLI   SPRMATCH+5,0        ANY MATERIAL CODE?                           
         BE    BLDR60              NO, CHECK COST CENTER                        
**NO-OP  BNE   BLDR55                                                           
**FEB07  CLC   TGAGY,=CL6'6548'    COKE AGENCY 6548 MUST HAVE IT                
**NO-OP  BNE   BLDR60              NO, CHECK COST CENTER                        
**NO-OP  B     FLDMISS                                                          
         USING TANUD,R4                                                         
BLDR55   LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         MVI   TANUEL,TANUELQ                                                   
         MVI   TANULEN,TANULNQ+10                                               
         MVI   TANUTYPE,TANUTMAT   MATERIAL CODE                                
         MVC   TANUMBER(L'SPRMATC),SPRMATC                                      
         OC    TANUMBER(L'SPRMATC),SPACES                                       
         GOTO1 ADDELEM             ADD NEW ELEMENT                              
*                                                                               
BLDR60   LA    R2,SPRCCTRH                                                      
         CLI   SPRCCTRH+5,0        ANY COST CENTER?                             
         BNE   BLDR65                                                           
         CLC   TGAGY,=CL6'6548'    COKE AGENCY 6548 MUST HAVE IT                
         BNE   BLDRX               NO, DONE                                     
         B     FLDMISS                                                          
BLDR65   LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         MVI   TANUEL,TANUELQ                                                   
         MVI   TANULEN,TANULNQ+10                                               
         MVI   TANUTYPE,TANUTCTR   COST CENTER                                  
         MVC   TANUMBER(L'SPRCCTR),SPRCCTR                                      
         OC    TANUMBER(L'SPRCCTR),SPACES                                       
         GOTO1 ADDELEM             ADD NEW ELEMENT                              
*                                                                               
BLDRX    GOTO1 ACTVIN,DMCB,SPRLCHGH LAST CHANGED                                
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         BNE   BLDRX2                                                           
         L     R3,AIO                                                           
         MVC   KEY,0(R3)           JUST RESTORE KEY                             
*                                                                               
         NI    KEY+TLDRSTAT-TLDRD,X'FF'-TLPRSBRD                                
         CLI   SPRBRND,C'Y'        IF THIS IS A BRAND PRODUCT                   
         BNE   *+8                                                              
         OI    KEY+TLDRSTAT-TLDRD,TLPRSBRD   SET BRAND STATUS IN KEY            
*                                                                               
         B     XIT                                                              
         SPACE                                                                  
BLDRX2   MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY                           
         MVC   SVTGCLI,TGCLI       AND GLOBAL CLIENT                            
         SPACE                                                                  
         CLI   SPRAGYH+5,0         IF NO AGENCY                                 
         BNE   *+10                                                             
         XC    TGAGY,TGAGY         CLEAR GLOBAL FOR RECVAL                      
         CLI   SPRCLIH+5,0         IF NO CLIENT                                 
         BNE   *+10                                                             
         XC    TGCLI,TGCLI         CLEAR GLOBAL FOR RECVAL                      
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            RESTORE AIO                                  
         SPACE 1                                                                
**NO-OP*                                                                        
**       MVC   AIO,AIO2                                                         
**       GOTO1 RECVAL,DMCB,TLPRCDQ,(X'30',SPRPRDH)  READ FOR UPDATE             
*                                                   TO ENABLE PUTREC            
**NO-OP* MVC   AIO,AIO1            RESTORE AIO                                  
         SPACE 1                                                                
*&&DO                                                                           
         NI    KEY+TLDRSTAT-TLDRD,X'FF'-TLPRSBRD                                
         CLI   SPRBRND,C'Y'        IF THIS IS A BRAND PRODUCT                   
         BNE   *+8                                                              
         OI    KEY+TLDRSTAT-TLDRD,TLPRSBRD   SET BRAND STATUS IN KEY            
         GOTO1 WRITE                                                            
*&&                                                                             
*                                                                               
         MVC   TGAGY,SVTGAGY       AND GLOBAL AGENCY                            
         MVC   TGCLI,SVTGCLI       AND GLOBAL CLIENT                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* VALIDATE OPTIONAL EMAIL ADDRESS                                               
*                                                                               
VREMAIL  NTR1                                                                   
         LA    R2,SPREMAIH                                                      
         CLI   5(R2),0                                                          
         BE    VREM40                                                           
         LA    RE,35                                                            
         LA    RF,SPREMAI                                                       
VREM20   CLI   0(RF),C'@'          THERE MUST BE A '@'                          
         BE    VREM30                                                           
         AHI   RF,1                                                             
         BCT   RE,VREM20                                                        
         B     FLDINV                                                           
*                                                                               
         LA    RE,35                                                            
         LA    RF,SPREMAI                                                       
VREM30   CLI   0(RF),C'.'          THERE MUST BE A '.'                          
         BE    VREM40                                                           
         AHI   RF,1                                                             
         BCT   RE,VREM30                                                        
         B     FLDINV                                                           
*                                                                               
VREM40   GOTO1 NAMIN,DMCB,TACMELQ,(X'C0',SPREMAIH),TACMTYPI                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS THAT THERE ARE NO COMMERCIALS                     
*              WITH THIS PRODUCT AND THAT THIS IS NOT A GLOBAL                  
*              PRODUCT AND IT DOES NOT HAVE A GLOBAL CLIENT                     
*              BEFORE DELETING                                                  
         SPACE                                                                  
         USING TLPRD,R3                                                         
         USING TLCOD,R4                                                         
CHKDEL   DS    0H                                                               
         MVC   SVKEY,KEY           GENCON USES KEY TO DELETE ACTIVE PTR         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         L     R3,AIO              R3=A(RECORD BEING PROCESSED)                 
**       TM    TLPRSTAT,TLPRSBRD   BRAND PRODUCT?                               
**       BO    CHKDELB                                                          
         OC    TLPRCLI,TLPRCLI                                                  
         BZ    CHKDERR             MUST HAVE CLIENT                             
         OC    TLPRAGY,TLPRAGY                                                  
         BZ    CHKDERR             AND AGENCY                                   
         SPACE                                                                  
         MVI   TLCOCD,TLCOCDQ      BUILD ACTIVE KEY FOR COMMERCIAL              
         MVC   TLCOCLI,TLPRCLI     USING SAME CLIENT                            
         MVC   TLCOAGY,TLPRAGY     AGENCY                                       
         MVC   TLCOPRD,TLPRPRD     AND PRODUCT AS RECORD                        
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLI   TLCOCD,TLCOCDQ      IF NO COMMERCIALS WITH THIS PRODUCT          
         BNE   CHKDOK                                                           
         CLC   TLCOCLI,TLPRCLI                                                  
         BNE   CHKDOK                                                           
         CLC   TLCOAGY,TLPRAGY                                                  
         BNE   CHKDOK                                                           
         CLC   TLCOPRD,TLPRPRD                                                  
         BE    CHKDERR                                                          
CHKDOK   MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     XIT                                                              
         DROP  R3,R4                                                            
         SPACE                                                                  
CHKDERR  LA    R2,SPRCLIH          ELSE                                         
         BAS   RE,DISPLAY          DISPLAY RECORD                               
         B     NODELETE            ERROR - CAN'T DELETE                         
         EJECT                                                                  
*&&DO                                                                           
*              ROUTINE CHECKS THAT THIS BRAND PRODUCT IS NOT IN                 
*              ANY COMMERCIALS BEFORE DELETING                                  
         SPACE                                                                  
         USING TLPRD,R3                                                         
         USING TLCOPCD,R4                                                       
CHKDELB  DS    0H                                                               
         MVI   TLCOPCD,TLCOBCDQ    BUILD PASSIVE KEY FOR COMMERCIAL             
         MVC   TLCOBAGY,TLPRAGY    USING SAME AGENCY AS RECORD                  
         MVC   TLCOBCLI,TLPRCLI               CLIENT                            
         MVC   TLCOBBPR,TLPRPRD               BRAND PRODUCT                     
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLCOBBPR+L'TLCOBBPR-TLCOPKEY),KEYSAVE                        
         BE    CHKDB15                                                          
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     XIT                                                              
*                                                                               
CHKDB15  LA    R2,SPRCLIH          IF BRAND PRODUCT EXISTS WITH COMM,           
         BAS   RE,DISPLAY                                                       
         B     NODELETE            ERROR - CAN'T DELETE                         
         EJECT                                                                  
*&&                                                                             
         SPACE                                                                  
*              ROUTINE GETS THE PRODUCTION INTERFACE SUBSIDIARY                 
*              ELEMENT FROM THE CLIENT RECORD AND SAVES THE PRODUCTION          
*              CLIENT IN SVPCLI,  ALSO SETS CONDITION CODE                      
         USING TAISD,R4                                                         
GETPCLI  NTR1                                                                   
         MVC   AIO,AIO2            CHANGE IO AREA                               
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'24',SPRCLIH) GET CLIENT RECORD            
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVI   ELCODE,TAISELQ      GET B2 ELEMENT                               
         LA    R1,TAISTYPC                                                      
         STC   R1,BYTE                                                          
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
         BNE   NO                                                               
         L     R4,TGELEM                                                        
         MVC   SVPCLI,TAISCDE      SAVE PRODUCTION CLIENT                       
         B     YES                                                              
         SPACE 3                                                                
*              ROUTINE GETS THE PRODUCTION PRODUCT RECORD ON THE ACC            
*              SIDE USING SVPCLI AND SPRPPRD, AND THE PUTS THE NAME             
*              IN SPRPPRN,  ALSO SETS CONDITION CODE                            
         SPACE                                                                  
GETPPRD  NTR1                                                                   
         MVC   KEY,SPACES          ACC SIDE USES SPACES                         
         MVC   KEY+1(2),=C'SJ'     U/L                                          
         MVC   KEY+3(3),SVPCLI     PRODUCTION CLIENT                            
         MVC   KEY+6(3),SPRPPRD    PRODUCTION PRODUCT                           
         OC    KEY+6(3),SPACES                                                  
         MVC   AIO,AIO3            SWITCH IO AREA                               
         GOTO1 READACC,DMCB,(X'80',SPRPPRNH) GET RECORD AND NAME                
         MVC   AIO,AIO1            RESTORE AIO                                  
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE                                                                  
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
GLOBERR  MVI   ERROR,ERGLPROD      CANNOT CHANGE A GLOBAL PRODUCT               
         B     THEEND                                                           
         SPACE                                                                  
NODELETE MVI   ERROR,ERINVDEL      RECORD NOT AVAILABLE FOR DELETION            
         B     THEEND                                                           
         SPACE                                                                  
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
         SPACE                                                                  
NOREC    MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
         SPACE                                                                  
NEEDAGY  MVI   ERROR,ERMISAGY      AGENCY REQUIRED FOR THIS FIELD               
         B     THEEND                                                           
         SPACE                                                                  
ERRADD   LA    R2,SPRADDRH                                                      
         MVC   MYMSGNO,=Y(ERPRADD) PRODUCT BILLING ADDRESS MUST BE              
         OI    GENSTAT2,USGETTXT   AT LEAST TWO LINES                           
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     THEEND                                                           
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
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'PRODUCT',CL8'LIST'                                    
PF13     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'PGROUP',CL8'DISPLAY'                                  
PF14     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
         DC    AL1(KEYTYGLB,L'TGPRG-1),AL2(TGPRG-TGD)                           
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3' ',CL8'COMMERCL',CL8'LIST'                                   
PF15     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
         DC    AL1(KEYTYGLB,L'TGPRD-1),AL2(TGPRD-TGD)                           
PF15X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE STATUS FIELD                                                  
***********************************************************************         
BLDSTAT  NTR1  BASE=*,LABEL=*                                                   
         CLI   SPRSTATH+5,0        STATUS FIELD?                                
         BE    BSTX                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   SVAGSTA6,0                                                       
         MVI   SVCLSTAT,0                                                       
         MVI   SVCLSTA2,0                                                       
         OC    TGAGY,TGAGY       IF RECORD HAS AGENCY                           
         BZ    BST05                                                            
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'24',SPRAGYH)   GET AGENCY RECORD          
         BNE   BST03                                                            
         L     R4,AIO              GET AGENCY ELEMENT                           
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAAYD,R4                                                         
         MVC   SVAGSTA6,TAAYSTA6   SAVE AGENCY STATUS 6                         
         DROP  R4                                                               
*                                                                               
BST03    GOTO1 RECVAL,DMCB,TLCLCDQ,(X'24',SPRCLIH)  GET CLIENT RECORD           
         BNE   BST05                                                            
         L     R4,AIO              GET CLIENT ELEMENT                           
         MVI   ELCODE,TACIELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   BST05                                                            
         USING TACID,R4                                                         
         MVC   SVCLSTAT,TACISTAT   SAVE CLIENT STATUS                           
         MVC   SVCLSTA2,TACISTA2   SAVE CLIENT 2 STATUS                         
         DROP  R4                                                               
BST05    MVC   AIO,AIO1                                                         
*                                                                               
         USING TAPID,R4                                                         
         LA    R4,ELEMENT                                                       
*                                                                               
         LA    R2,SPRSTATH                                                      
         XC    BLOCK(100),BLOCK                                                 
         LA    R5,BLOCK                                                         
         USING SCAND,R5                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R5))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV2                                                          
         ZIC   R0,4(R1)              R0=N'SCAN BLOCK ENTRIES                    
*                                                                               
BST10    CLI   SCLEN2,0              IF INPUT ON RHS, MUST BE AC                
         BE    BST20                                                            
         CLC   SCDATA1(2),=C'AC'                                                
         BNE   FLDINV2                                                          
*                                                                               
         MVC   ERRDISP,SCDISP2      AC MUST BE BETWEEN 0 AND 100                
         ZIC   RF,SCLEN2                                                        
         GOTO1 CASHVAL,DMCB,SCDATA2,(RF)                                        
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV2                                                          
         TM    4(R1),X'80'                                                      
         BO    FLDINV2                                                          
         CLC   4(4,R1),=F'10000'                                                
         BH    FLDINV2                                                          
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+8                                                              
         MVI   6(R1),X'FF'       IF AC RATE=0 STORE AS X'FF'                    
         MVC   TAPIAC,6(R1)                                                     
         B     BST30                                                            
*                                                                               
BST20    CLC   SCDATA1(6),=C'LOCKED'                                            
         BNE   BST25                                                            
         OI    TAPISTAT,TAPISLCK                                                
         B     BST30                                                            
*                                                                               
BST25    CLC   SCDATA1(3),=C'CSF'                                               
         BNE   BST28                                                            
         TM    SVCLSTAT,TACIJPCY   IF CLIENT HAS CSF,                           
         BO    FLDINV2             PRODUCT CANNOT HAVE CSF                      
         TM    SVCLSTA2,TACIJPCN   IF CIENT HAS NOCSF                           
         BO    BST27               PRODUCT CAN HAVE CSF                         
         TM    SVAGSTA6,TAAYJPCY   IF CLIENT HAS NONE, AND AGY HAS CSF          
         BO    FLDINV2             PRODUCT CANNOT HAVE CSF                      
BST27    OI    TAPISTAT,TAPIJPCY                                                
         B     BST30                                                            
*                                                                               
BST28    CLC   SCDATA1(5),=C'NOCSF'                                             
         BNE   FLDINV2                                                          
         TM    SVCLSTA2,TACIJPCN   IF CLIENT HAS NOCSF,                         
         BO    FLDINV2             PRODUCT CANNOT HAVE NOCSF                    
         TM    SVCLSTAT,TACIJPCY   IF CIENT HAS CSF                             
         BO    BST29               PRODUCT CAN HAVE NOCSF                       
         TM    SVAGSTA6,TAAYJPCY   IF CLI HAS NONE, AND AGY HAS NONE            
         BNO   FLDINV2             PRODUCT CANNOT HAVE NOCSF                    
BST29    OI    TAPISTAT,TAPIJPCN                                                
*                                                                               
BST30    LA    R5,SCANNEXT                                                      
         MVC   ERRDISP,SCDISP1                                                  
         BCT   R0,BST10                                                         
BSTX     J     XIT                                                              
         DROP  R4                                                               
*                                                                               
FLDINV2  MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 EXIT,DMCB,0                                                      
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING MQ MESSAGE                     *         
*        ON ENTRY ... P1 BYTE 0 = X'80' SAVE TO WSSVR BLOCK           *         
*                     AIO1      = A(PRODUCT RECORD)                   *         
***********************************************************************         
                                                                                
BLDMQMSG NTR1  BASE=*,LABEL=*                                                   
         MVC   TGBYTE,0(R1)        TGBYTE = WSSVR SAVE STATUS                   
                                                                                
         LA    R2,BLOCK            R2=A(MQ MESSAGE BLOCK)                       
         BAS   RE,BLDIMMSG         BUILD INSERT/MODIFY MESSAGE                  
         JE    BMQ10                                                            
         BAS   RE,BLDDMSG          OR BUILD DELETE MESSAGE                      
         JNE   BMQNO               LENGTH RETURNED IN R3                        
                                                                                
         USING FAWSSVRD,R1                                                      
BMQ10    TM    TGBYTE,X'80'        IF SAVING TO WSSVR                           
         JZ    BMQYES              DO SO NOW                                    
         LA    R1,WORK                                                          
         MVC   FAWSTOKN,=CL4'INIT'                                              
         MVI   FAWSACTN,FAWSASVE                                                
         ST    R2,FAWSADR                                                       
         STH   R3,FAWSLEN                                                       
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    BMQYES                                                           
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
BMQYES   XR    RC,RC                                                            
BMQNO    LTR   RC,RC                                                            
BMQX     XIT1  REGS=(R3)                                                        
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING INSERT/MODIFY MESSAGE          *         
*        ON ENTRY ... AIO1 = A(PRODUCT RECORD)                        *         
*                     R2   = A(RESERVED MQ MESSAGE BLOCK)             *         
***********************************************************************         
                                                                                
BLDIMMSG NTR1                                                                   
         CLI   MODE,XRECDEL        IF NOT DELETING RECORD                       
         JE    BMQNO                                                            
                                                                                
         USING TLPRD,R4                                                         
         L     R4,AIO1                                                          
         OC    TLPRAGY,TLPRAGY     AND NOT A GLOBAL AGENCY                      
         JZ    BMQNO                                                            
         OC    TLPRCLI,TLPRCLI     AND NOT A GLOBAL CLIENT                      
         JZ    BMQNO                                                            
                                                                                
         USING IMMSGD,R2                                                        
         LHI   R3,IMMLNQ                                                        
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         LA    R0,IMMSG                                                         
         LR    R1,R3               COPY INSERT MQ MESSAGE TEMPLATE              
         MVCL  RE,R0               INTO BLOCK                                   
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JE    BIMM10                                                           
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         JNE   BIMM20                                                           
BIMM10   MVC   IMROT,=C'<acpinsert>'                                            
         MVC   IMINS,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMMOD,BMQFALSE                                                   
         MVC   IMROX,=C'</acpinsert>'                                           
         J     BIMM30                                                           
                                                                                
BIMM20   MVC   IMROT,=C'<acpmodify>'                                            
         MVC   IMINS,BMQFALSE      IF ACTION IS CHANGE                          
         MVC   IMMOD,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMROX,=C'</acpmodify>'                                           
                                                                                
BIMM30   MVC   IMAGY,TLPRAGY       COPY AGENCY CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'IMAGY,IMAGY)                                    
         MVC   IMCLI,TLPRCLI       COPY CLIENT CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'IMCLI,IMCLI)                                    
         MVC   IMCOD,TLPRPRD       COPY PRODUCT CODE INTO XML                   
         GOTO1 ELIMCHAR,DMCB,(L'IMCOD,IMCOD)                                    
         DROP  R4                                                               
                                                                                
         MVI   IMLCK,C'N'                                                       
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO              COPY PRODUCT NAME INTO XML                   
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   BIMM40                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   IMNAM(0),TANANAME                                                
         GOTO1 ELIMCHAR,DMCB,(L'IMNAM,IMNAM)                                    
         DROP  R4                                                               
                                                                                
         USING TAPID,R4                                                         
BIMM40   L     R4,AIO                                                           
         MVI   ELCODE,TAPIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   BMQYES                                                           
         TM    TAPISTAT,TAPISLCK   COPY PRODUCT LOCKED STATUS INTO XML          
         JZ    BMQYES                                                           
         MVI   IMLCK,C'Y'                                                       
         J     BMQYES                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING DELETE MESSAGE                 *         
*        ON ENTRY ... AIO1 = A(PRODUCT RECORD)                        *         
*                     R2   = A(MQ MESSAGE BLOCK)                      *         
***********************************************************************         
                                                                                
BLDDMSG  NTR1                                                                   
         CLI   MODE,XRECDEL        IF DELETING RECORD                           
         JNE   BMQNO                                                            
         CLI   SPRAGYH+5,0         AND NOT A GLOBAL AGENCY                      
         JE    BMQNO                                                            
         CLI   SPRCLIH+5,0         AND NOT A GLOBAL CLIENT                      
         JE    BMQNO                                                            
                                                                                
         USING DMSGD,R2                                                         
         LHI   R3,DMLNQ                                                         
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         LA    R0,DMSG                                                          
         LR    R1,R3               COPY DELETE MQ MESSAGE TEMPLATE              
         MVCL  RE,R0               INTO BLOCK                                   
                                                                                
         MVC   DAGY,SPRAGY         COPY AGENCY CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'DAGY,DAGY)                                      
         MVC   DCLI,SPRCLI         COPY CLIENT CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'DCLI,DCLI)                                      
         MVC   DCOD,SPRPRD         COPY PRODUCT CODE INTO XML                   
         GOTO1 ELIMCHAR,DMCB,(L'DCOD,DCOD)                                      
         J     BMQYES                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
BMQTRUE  DC    CL5'true'                                                        
BMQFALSE DC    CL5'false'                                                       
                                                                                
IMMSG    DC    CL16' '                                                          
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    CL11' '                                                          
         DC    C'<agency isinsert="false" ismodify="false" code="'              
         DC    CL6' '                                                           
         DC    C'">'                                                            
         DC    C'<client isinsert="false" ismodify="false" code="'              
         DC    CL6' '                                                           
         DC    C'">'                                                            
         DC    C'<product isinsert="'                                           
         DC    CL5' '                                                           
         DC    C'" ismodify="'                                                  
         DC    CL5' '                                                           
         DC    C'" code="'                                                      
         DC    CL6' '                                                           
         DC    C'" name="'                                                      
         DC    CL36' '                                                          
         DC    C'" locked="'                                                    
         DC    CL1' '                                                           
         DC    C'">'                                                            
         DC    C'</product>'                                                    
         DC    C'</client>'                                                     
         DC    C'</agency>'                                                     
         DC    CL12' '                                                          
IMMLNQ   EQU   *-IMMSG                                                          
                                                                                
DMSG     DS    CL16' '                                                          
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    C'<acpdelete>'                                                   
         DC    C'<agency code="'                                                
         DS    CL6' '                                                           
         DC    C'">'                                                            
         DC    C'<client isdelete="false" code="'                               
         DS    CL6' '                                                           
         DC    C'">'                                                            
         DC    C'<product isdelete="true" code="'                               
         DS    CL6' '                                                           
         DC    C'">'                                                            
         DC    C'</product>'                                                    
         DC    C'</client>'                                                     
         DC    C'</agency>'                                                     
         DC    C'</acpdelete>'                                                  
DMLNQ    EQU   *-DMSG                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE OUTPUTS MQ MESSAGE                                   *         
*        ON ENTRY ... AIO1 = A(PRODUCT RECORD)                        *         
***********************************************************************         
                                                                                
NFYVIT   NTR1  BASE=*,LABEL=*                                                   
         GOTOR BLDMQMSG,DMCB,0     BUILD UPDATED MQ MESSAGE                     
         JNE   XIT                                                              
                                                                                
         USING FAWSSVRD,R1                                                      
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         JNE   NV10                                                             
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN,=CL4'INIT'                                              
         MVI   FAWSACTN,FAWSARST   RECALL INITIAL-STATE BASED                   
         MVC   FAWSADR,AIO3        MESSAGE INTO AIO3                            
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         LA    RE,BLOCK                                                         
         LR    RF,R3               IF CHANGE HAS BEEN MADE THAT                 
         L     R0,AIO3             WILL AFFECT THE MESSAGE                      
         LR    R1,R3               SEND THE UPDATED MESSAGE                     
         CLCL  RE,R0                                                            
         JE    XIT                                                              
                                                                                
NV10     GOTO1 NTFYVITA,DMCB,BLOCK,(R3),0                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR16D                                                       
         SPACE 3                                                                
         ORG   SPRWORK                                                          
SVTGAGY  DS    CL6                 SAVED GLOBAL AGENCY                          
SVTGCLI  DS    CL6                 SAVED GLOBAL CLIENT                          
SVPCLI   DS    CL3                 SAVED PRODUCTION CLIENT                      
SVAGSTA6 DS    XL1                 SAVED AGENCY STATUS 6                        
SVCLSTAT DS    XL1                 SAVED CLIENT STATUS                          
SVCLSTA2 DS    XL1                 SAVED CLIENT STATUS 2                        
SVKEY    DS    CL38                SAVED KEY AND ADDRESS                        
PTRS     DS    CL(L'TLDRREC*3+1)   SAVED ACTIVE AND 2 PASSIVE PTRS              
         SPACE 3                                                                
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* FAWSSVRD                                                                      
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING INSERT/UPDATE MESSAGES           *         
***********************************************************************         
                                                                                
IMMSGD   DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
IMROT    DS    CL11                                                             
         DS    CL48                                                             
IMAGY    DS    CL6                                                              
         DS    CL2                                                              
         DS    CL48                                                             
IMCLI    DS    CL6                                                              
         DS    CL2                                                              
         DS    CL19                                                             
IMINS    DS    CL5                                                              
         DS    CL12                                                             
IMMOD    DS    CL5                                                              
         DS    CL8                                                              
IMCOD    DS    CL6                                                              
         DS    CL8                                                              
IMNAM    DS    CL36                                                             
         DS    CL10                                                             
IMLCK    DS    CL1                                                              
         DS    CL2                                                              
         DS    CL10                                                             
         DS    CL9                                                              
         DS    CL9                                                              
IMROX    DS    CL12                                                             
                                                                                
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING DELETE MESSAGES                  *         
***********************************************************************         
                                                                                
DMSGD    DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
         DS    CL11                                                             
         DS    CL14                                                             
DAGY     DS    CL6                                                              
         DS    CL2                                                              
         DS    CL31                                                             
DCLI     DS    CL6                                                              
         DS    CL2                                                              
         DS    CL31                                                             
DCOD     DS    CL6                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059TAGEN16   06/27/12'                                      
         END                                                                    
