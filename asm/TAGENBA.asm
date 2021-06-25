*          DATA SET TAGENBA    AT LEVEL 036 AS OF 05/29/15                      
*PHASE T702BAE,*                                                                
         TITLE 'T702BA - PRINT COMMERCIAL MAINTENANCE'                          
T702BA   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702BA,R7                                                      
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
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     COMX                                                             
         SPACE 1                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     COMX                                                             
         SPACE 1                                                                
         CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   *+12                                                             
         BAS   RE,CHKDEL           INSURE IT'S OK TO DELETE                     
         B     COM10               IF RETURNS THEN OK                           
         SPACE 1                                                                
         CLI   MODE,RECREST        RESTORE RECORD                               
         BNE   COM20                                                            
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'84',0)  IF RECORD EXISTS                 
         BE    ADDERR                          THEN GIVE ERROR                  
         SPACE 1                                                                
COM10    GOTO1 SAVPTRS,DMCB,PTRBLK SAVE PASSIVE POINTER BLOCK                   
         B     COMX                                                             
         SPACE 1                                                                
COM20    CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BNE   *+16                                                             
         CLI   THISLSEL,C'D'       AND IF DELETING FROM LIST                    
         BE    COMX                   DON'T DISPLAY FIRST                       
         B     COM50               ELSE GO DISPLAY RECORD                       
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   COM25                                                            
         GOTO1 SAVPTRS,DMCB,PTRBLK SAVE PASSIVE PTRS                            
         BAS   RE,BLDREC           BUILD THE RECORD                             
         B     COMX                                                             
         SPACE 1                                                                
COM25    CLI   MODE,XRECPUT        IF RECORD CHANGED                            
         BNE   COM30                                                            
         GOTO1 ADDPTRS,DMCB,(X'80',PTRBLK)  UPDATE ACTIVE/PASSIVE PTRS          
         B     COM50                        GO RE-DISPLAY                       
         SPACE 1                                                                
COM30    CLI   MODE,XRECADD        IF RECORD ADDED                              
         BE    COM40                                                            
         CLI   MODE,XRECREST       OR RECORD RESTORED                           
         BE    COM40                                                            
         CLI   MODE,XRECDEL        OR RECORD DELETED                            
         BNE   COMX                                                             
COM40    GOTO1 ADDPTRS,DMCB,PTRBLK UPDATE PASSIVE POINTERS                      
         SPACE 1                                                                
COM50    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         SPACE 1                                                                
COMX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SCOAGYH),SCOAGYNH  AGENCY             
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         SPACE 1                                                                
         L     R4,AIO              FIND OUT IF AGY GROUP EXISTS                 
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL            ELEMENT MUST EXIST                           
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         SPACE 1                                                                
         CLI   TAAYTPOF,C'0'       OFFICE MUST BE A LETTER                      
         BNL   BADREC                                                           
         SPACE 1                                                                
         OC    TAAYAGG,TAAYAGG     IS THERE AN AGENCY GROUP                     
         BZ    *+10                                                             
         MVC   TGAGG,TAAYAGG       YES - MOVE INTO GLOBAL STORAGE               
         SPACE 1                                                                
         LA    R2,SCOCIDH          COMMERCIAL ID                                
         NI    4(R2),X'DF'         FORCE VALIDATION                             
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         BNE   VK20                                                             
         GOTO1 ANY                 REQUIRE MANUAL INPUT                         
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'04',SCOCIDH)  READ KEY                   
         BE    ADDERR              IF ON FILE THEN GIVE ERROR                   
         MVC   KEY,KEYSAVE         ELSE RESTORE KEY & LET GENCON HANDLE         
         NI    GENSTAT1,X'FF'-OKADDEL  DON'T ALLOW ADD OF DELETED RECS          
         B     VKX                                                              
         SPACE 1                                                                
VK20     CLI   ACTNUM,ACTREST      IF RESTORING A RECORD                        
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       SET READ DELETED RECORDS                     
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'20',SCOCIDH)  GET THE RECORD             
         NI    DMINBTS,X'F7'                                                    
         SPACE 1                                                                
         TM    TGMEEQU,PRINT       ENSURE THIS IS PRINT COMMERCIAL              
         BZ    BADCTYPE                                                         
         SPACE 1                                                                
         L     R4,AIO              SAVE INTERNAL COMMERCIAL NUMBER              
         USING TLCOD,R4                                                         
         MVC   TGCOM,TLCOCOM                                                    
         MVC   KEY,TLCOKEY         SET ACTIVE KEY FOR GENCON                    
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY THE KEY                                       
         SPACE 1                                                                
DKEY     NTR1                                                                   
         L     R4,AIO              R4=A(RECORD)                                 
         USING TLCOD,R4                                                         
         MVC   SCOAGY,TLCOAGY      AGENCY                                       
         MVI   SCOAGYH+5,L'TLCOAGY                                              
         OI    SCOAGYH+6,X'80'                                                  
         SPACE 1                                                                
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SCOAGYH),SCOAGYNH  AGY NAME           
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVKEY                                                        
         SPACE 1                                                                
         MVI   ELCODE,TACOELQ      GET COMM ID NUM FROM ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TACOD,R4                                                         
         MVC   SCOCID,TACOCID      COMMERCIAL ID                                
         OI    SCOCIDH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE ORIGINAL ACTIVE KEY                     
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS NOT ADD                         
         BE    BLDR10                                                           
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSEQ))                                     
         BE    *+6                                                              
         DC    H'00'                                                            
         L     RE,TGELEM                                                        
         MVC   SVNUTSEQ,0(RE)      SAVE NEXT CAST SEQUENCE NUMBER               
         SPACE 1                                                                
BLDR10   L     R4,AIO              INITIALIZE I/O AREA                          
         USING TLCOD,R4                                                         
         MVC   TLCOLEN,DATADISP                                                 
         XC    TLCOSTAT(10),TLCOSTAT                                            
         SPACE 1                                                                
         GOTO1 NAMIN,DMCB,TANAELQ,SCOCIDNH  COMMERCIAL NAME                     
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'20',SCOCLIH)  CLIENT CODE                 
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         BAS   RE,CHKLOCK          CHECK IF CLIENT LOCKED                       
         BNE   CLILOCK                                                          
         SPACE 1                                                                
         L     R4,AIO2                                                          
         MVI   ELCODE,TABRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   BLDR20                                                           
         USING TABRD,R4                                                         
         OC    TABROEOR,TABROEOR   IF EMPLOYER OF RECORD SPECIFIED              
         BZ    BLDR20                                                           
         CLC   TABROEOR,TGTPEMP    CAN'T ALLOW TALENT PARTNERS                  
         BE    BADCREC                                                          
         SPACE 1                                                                
BLDR20   BAS   RE,VALPRDS          PRODUCT CODE(S)                              
         SPACE 1                                                                
         LA    R2,SCOPHOH          R2=A(PHOTOGRAPHER SSN)                       
         CLI   5(R2),0                                                          
         BE    BLD40                                                            
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    BLDR30                                                           
         CLI   5(R2),6             GREATER THAN 6 CHAR, SSN ENTERED             
         BH    BLDR30                                                           
         GOTO1 SSNUNPK,DMCB,8(R2),WORK                                          
         BNE   FLDINV                                                           
         MVC   8(9,R2),WORK        MOVE IN SSN                                  
         MVI   5(R2),9                                                          
*                                                                               
BLDR30   GOTO1 RECVAL,DMCB,TLW4CDQ,(R2)          VALIDATE IT                    
         GOTO1 NAMIN,DMCB,TANUELQ,(R2),TANUTPHO  ADD TANU EL. FOR IT            
         GOTO1 SSNPACK,DMCB,8(R2),WORK                                          
         MVI   5(R2),6                                                          
         MVC   8(9,R2),SPACES                                                   
         MVC   8(6,R2),WORK                                                     
         OI    6(R2),X'80'         TRANSMIT                                     
         B     BLD42                                                            
*                                  ELSE VALIDATE NAME FIELD ALONE               
BLD40    GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SCOPHONH),TAFNTPHO                     
         SPACE 1                                                                
BLD42    LA    R2,SCOSVCH          R2=A(SERVICES FOR AGENCY CODE)               
         CLI   5(R2),0                                                          
         BE    BLD44                                                            
         MVC   SVTGAGY,TGAGY       SAVE TGAGY                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'04',(R2))  VALIDATE IT                    
         MVC   TGAGY,SVTGAGY       RESTORE TGAGY                                
         BNE   ERRXIT              GIVE ERROR IF NOT VALID                      
         SPACE                                                                  
         USING TANUD,R4                                                         
         XC    ELEMENT,ELEMENT     BUILD TANU ELEMENT                           
         LA    R4,ELEMENT                                                       
         MVI   TANUEL,TANUELQ                                                   
         MVI   TANUTYPE,TANUTSVC   SET TYPE                                     
         LA    R1,3                                                             
         LA    R1,L'SCOSVC(R1)     LENGTH OF ELEMENT IS L'SCOSVC+3              
         STC   R1,TANULEN                                                       
         MVC   TANUMBER(L'SCOSVC),SCOSVC                                        
         OC    TANUMBER(L'SCOSVC),SPACES  PAD WITH SPACES                       
         GOTO1 ADDELEM                                                          
         B     BLD50                                                            
*                                  ELSE VALIDATE NAME FIELD ALONE               
BLD44    GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SCOSVCNH),TAFNTSVC                     
         SPACE 1                                                                
BLD50    LA    R2,SCOSDTEH         R2=A(SHOOT DATE)                             
         MVI   WORK,TACSTYPS       SET SHOOT TYPE                               
         BAS   RE,BLDSTU           BUILD STUDIO ELEMENT                         
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     BUILD COMMERCIAL DETAILS ELEMENT             
         LA    R3,ELEMENT                                                       
         USING TACOD,R3                                                         
         MVI   TACOEL,TACOELQ      ELEMENT CODE                                 
         MVI   TACOLEN,TACOLNQ3    ELEMENT LENGTH                               
         MVC   TACOCID,TGCID       COMMERCIAL ID IS AROUND FROM VALKEY          
         MVI   TACOTYPE,CTYPRNT    SET PRINT COMMERCIAL TYPE                    
         MVI   TACOMED,TACOMEDP    SET PRINT MEDIA                              
         SPACE 1                                                                
         BAS   RE,VALSTAT          VALIDATE STATUS FIELD                        
         SPACE 1                                                                
         LA    R2,SCOATTH          ATTENTION CODE                               
         CLI   5(R2),0                                                          
         BE    BLD60                                                            
         GOTO1 RECVAL,DMCB,TLATCDQ,(R2)                                         
         MVC   TACOATT,TGATT                                                    
         SPACE 1                                                                
BLD60    GOTO1 ADDELEM             ADD COMMERCIAL DETAILS ELEMENT               
         SPACE 1                                                                
         CLI   SCOFILTH+5,0        IF FILTERS INPUT                             
         BE    BLD70                                                            
         XC    ELEMENT,ELEMENT     BUILD FILTERS ELEMENT                        
         LA    R4,ELEMENT                                                       
         USING TAFLD,R4                                                         
         MVI   TAFLEL,TAFLELQ                                                   
         MVI   TAFLLEN,TAFLLNQ                                                  
         OC    SCOFILT,SPACES                                                   
         MVC   TAFLFLT1(L'SCOFILT),SCOFILT                                      
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
BLD70    GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SCOCOMMH),TACMTYPG  COMMENT            
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       IF ADDING NEW RECORD                         
         BNE   BLD80                                                            
         BAS   RE,GETCOM           GET NEXT INTERNAL COMMERCIAL NUMBER          
         SPACE 1                                                                
         USING TANUD,R4                                                         
         LA    R4,SVNUTSEQ                                                      
         XC    SVNUTSEQ,SVNUTSEQ   ADD NEXT CAST SEQUENCE NUMBER                
         MVI   TANUEL,TANUELQ      ELEMENT TO COMMERCIAL RECORD                 
         MVI   TANULEN,TANULNQ1                                                 
         MVI   TANUTYPE,TANUTSEQ                                                
         MVC   TANUNXTC,=X'0001'                                                
         DROP  R4                                                               
         SPACE 1                                                                
BLD80    MVC   ELEMENT(L'SVNUTSEQ),SVNUTSEQ                                     
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCOCDQ,(X'40',0)  BUILD ACTIVE KEY                  
         SPACE 1                                                                
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   TLCOKEY,KEY         SET ACTIVE KEY IN I/O AREA                   
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       IF ADDING - EXIT                             
         BE    BLDX                                                             
         MVC   AIO,AIO2            ELSE SET TO GET RECORD AGAIN                 
         MVC   KEY,SVKEY                                                        
         LA    R2,SCOAGYH          SET CURSOR IF RECORD NOT FOUND               
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
BLDX     B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE CHECKS IF CLIENT RECORD IS LOCKED                        
*              CC NEQ - ADD NOT ALLOWED                                         
*                                                                               
CHKLOCK  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ADDING A COMMERCIAL RECORD                
         BNE   YES                                                              
         L     R4,AIO2                                                          
         MVI   ELCODE,TACIELQ      MAKE SURE CLIENT NOT LOCKED                  
         BAS   RE,GETEL                                                         
         BNE   YES                                                              
         USING TACID,R4                                                         
         TM    TACISTAT,TACISLCK                                                
         BZ    YES                                                              
         B     NO                  CLIENT IS LOCKED                             
         EJECT                                                                  
*              ROUTINE TO VALIDATE PRODUCT CODE(S)                              
         SPACE 1                                                                
VALPRDS  NTR1                                                                   
         LA    R2,SCOPRDH          R2=A(FIRST PRODUCT FIELD)                    
         LA    R0,MAXPRDS          R0=MAX N'PRODUCTS                            
         SPACE 1                                                                
         LA    R5,ELEMENT          BUILD SKELETON PRODUCT ELEMENT               
         USING TAPRD,R5                                                         
         XC    TAPREL(TAPRLNQ),TAPREL                                           
         MVI   TAPREL,TAPRELQ      ELEMENT CODE                                 
         MVI   TAPRLEN,TAPRLNQ             LENGTH                               
         SPACE 1                                                                
VPRD10   MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'20',(R2)) VALIDATE PRODUCT CODE           
         MVC   AIO,AIO1                                                         
         BAS   RE,CPRLOCK         CHECK IF PRODUCT LOCKED                       
         BNE   PRDLOCK                                                          
         SPACE 1                                                                
         L     R4,AIO              ENSURE THAT PRODUCT CODE                     
         MVI   ELCODE,TAPRELQ                                                   
         BAS   RE,GETEL            IS NOT ENTERED MORE THAN ONCE                
         B     *+8                                                              
VPRD20   BAS   RE,NEXTEL                                                        
         BNE   VPRD30                                                           
         CLC   TGPRD,TAPRPRD-TAPRD(R4)                                          
         BNE   VPRD20                                                           
         B     FLDINV                                                           
         SPACE 1                                                                
VPRD30   MVC   TAPRPRD,TGPRD       MOVE PRODUCT CODE TO ELEMENT                 
         GOTO1 ADDELEM             AND ADD IT                                   
         DROP  R5                                                               
         SPACE 1                                                                
VPRD40   BCT   R0,*+8              CONTINUE UNTIL END                           
         B     VPRDX                                                            
         BAS   RE,BUMP2            BUMP PAST NAME TO NEXT CODE                  
         CLI   5(R2),0             IF SOMETHING THERE                           
         BNE   VPRD10              GO VALIDATE                                  
         B     VPRD40              ELSE TRY NEXT FIELD                          
         SPACE 1                                                                
VPRDX    XC    TGPRD,TGPRD         CLEAR GLOBAL PRODUCT FOR KEY                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS IF PRODUCT RECORD IS LOCKED                       
*              CC NEQ - ADD NOT ALLOWED                                         
*                                                                               
CPRLOCK  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ADDING A COMMERCIAL RECORD                
         BNE   YES                                                              
         L     R4,AIO2                                                          
         MVI   ELCODE,TAPIELQ      MAKE SURE PRODUCT NOT LOCKED                 
         BAS   RE,GETEL                                                         
         BNE   YES                                                              
         USING TAPID,R4                                                         
         TM    TAPISTAT,TAPISLCK                                                
         BZ    YES                                                              
         B     NO                  PRODUCT IS LOCKED                            
         EJECT                                                                  
*              ROUTINE TO BUILD STUDIO ELEMENT                                  
         SPACE 1                                                                
*                                  R2=A(DATE FIELD), WORK=ELEMENT TYPE          
BLDSTU   NTR1                                                                   
         CLI   5(R2),0             IF DATE NOT INPUT                            
         BNE   BSTU10                                                           
         BAS   RE,BUMP2            THEN INPUT NOT ALLOWED IN NAME               
         CLI   5(R2),0                                                          
         BNE   NOINPUT                                                          
         BAS   RE,BUMP2            NOR IN CITY                                  
         CLI   5(R2),0                                                          
         BNE   NOINPUT                                                          
         B     BSTUX                                                            
         SPACE 1                                                                
BSTU10   XC    ELEMENT,ELEMENT     ELSE INITIALIZE ELEMENT                      
         LA    R4,ELEMENT                                                       
         USING TACSD,R4                                                         
         MVI   TACSEL,TACSELQ      ELEMENT CODE                                 
         MVI   TACSLEN,TACSLNQ     ELEMENT LENGTH                               
         MVC   TACSTYPE,WORK       ELEMENT TYPE                                 
         SPACE 1                                                                
         GOTO1 DTVAL,DMCB,TACSDATE VALDATE DATE                                 
         SPACE 1                                                                
         BAS   RE,BUMP2            BUMP TO STUDIO NAME                          
         GOTO1 ANY                                                              
         MVC   TACSSTUD,WORK       SAVE NAME                                    
         SPACE 1                                                                
         BAS   RE,BUMP2            BUMP TO STUDIO CITY                          
         GOTO1 ANY                                                              
         MVC   TACSCITY,WORK       SAVE CITY                                    
         SPACE 1                                                                
         MVC   TACSSTAT,SPACES     SAVE STATE                                   
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         SPACE 1                                                                
BSTUX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE STATUS FIELD                                 
         SPACE 1                                                                
VALSTAT  NTR1                                                                   
         LA    R2,SCOSTATH         STATUS                                       
         CLI   5(R2),0                                                          
         BE    VSTAX                                                            
         GOTO1 SCANNER,DMCB,(R2),(X'80',BLOCK)                                  
         MVI   ERRDISP,0                                                        
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R4,BLOCK                                                         
         USING SCAND,R4            R4=A(SCAN BLOCK)                             
         SPACE 1                                                                
VSTA10   MVC   ERRDISP,SCDISP1     SAVE DISP TO THIS FIELD                      
         SPACE 1                                                                
         LA    RF,STATTAB          LOOK UP FIELD IN TABLE                       
VSTA20   CLI   0(RF),X'FF'                                                      
         BE    FLDINV                                                           
         CLC   SCDATA1,1(RF)       EXACT MATCH TO TABLE VALUE                   
         BE    *+12                                                             
         LA    RF,L'STATTAB(RF)                                                 
         B     VSTA20                                                           
         SPACE 1                                                                
         OC    TACOSTAT,0(RF)      TURN ON BIT IN ELEMENT                       
         SPACE 1                                                                
         LA    R4,SCANNEXT         BUMP TO NEXT SCAN BLOCK ENTRY                
         BCT   R0,VSTA10           AND CONTINUE                                 
         SPACE 1                                                                
VSTAX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE GETS NEXT INTERNAL COMMERCIAL NUMBER                     
         SPACE 1                                                                
GETCOM   NTR1                                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'34',0) READ SYSTEM REC FOR UPDATE         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TASYELQ      GET A(SYSTEM CONTROL ELEMENT)                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASYD,R4            R4=A(SYSTEM CONTROL ELEMENT)                 
         SPACE 1                                                                
         ICM   R1,15,TASYLCOM      ADD ONE TO LAST COMMERCIAL NUMBER            
         AH    R1,=H'1'                                                         
         STCM  R1,15,TASYLCOM                                                   
         SPACE 1                                                                
         STCM  R1,15,TGCOM         SAVE IT FOR THIS COMMERCIAL                  
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE BACK SYSTEM RECORD                     
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SCOCIDNH            CLEAR THE SCREEN                             
         GOTO1 FLDVAL,DMCB,(X'01',SCOPRDH),2*MAXPRDS  PRODUCT NAMES             
         XC    SCOATTN,SCOATTN     ATTENTION NAME                               
         OI    SCOATTNH+6,X'80'                                                 
         SPACE 1                                                                
         GOTO1 CHAROUT,DMCB,TANAELQ,SCOCIDNH  DISPLAY TITLE                     
         SPACE 1                                                                
         L     R4,AIO              DISPLAY CLIENT FROM KEY OF RECORD            
         USING TLCOD,R4                                                         
         MVC   SCOCLI,TLCOCLI                                                   
         MVI   SCOCLIH+5,L'TLCOCLI                                              
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'0C',SCOCLIH),SCOCLINH  GET NAME           
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         BAS   RE,DISPRDS          DISPLAY PRODUCT CODE(S)                      
         SPACE 1                                                                
         GOTO1 CHAROUT,DMCB,TANUELQ,SCOPHOH,TANUTPHO  PHOTOGRAPHER SSN          
         BNE   DR30                                                             
         MVI   SCOPHOH+5,L'SCOPHO                                               
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'0C',SCOPHOH),SCOPHONH  GET NAME           
         MVC   AIO,AIO1                                                         
*                                                                               
         TM    TGSYSTAT,TASYSPID   IF USING PID,                                
         BZ    DR32                NO                                           
         GOTO1 SSNPACK,DMCB,SCOPHO,WORK         SHOW IT                         
         MVC   SCOPHO,SPACES                                                    
         MVC   SCOPHO(6),WORK                                                   
         MVI   SCOPHOH+5,6                                                      
         OI    SCOPHOH+6,X'80'                                                  
         B     DR32                                                             
*                                 ELSE GET PHOTOGRAPHER NAME FROM COMML         
DR30     GOTO1 CHAROUT,DMCB,TAFNELQ,SCOPHONH,TAFNTPHO                           
         SPACE 1                                                                
DR32     GOTO1 CHAROUT,DMCB,TANUELQ,SCOSVCH,TANUTSVC  SVC FOR AGY CODE          
         BNE   DR34                                                             
         MVI   SCOSVCH+5,L'SCOSVC                                               
         MVC   AIO,AIO2                                                         
         MVC   SVTGAGY,TGAGY       SAVE TGAGY                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'0C',SCOSVCH),SCOSVCNH  GET NAME           
         MVC   TGAGY,SVTGAGY       RESTORE TGAGY                                
         MVC   AIO,AIO1                                                         
         B     DR40                                                             
*                                 ELSE GET SVC FOR AGY NAME FROM COMML          
DR34     GOTO1 CHAROUT,DMCB,TAFNELQ,SCOSVCNH,TAFNTSVC                           
         SPACE 1                                                                
DR40     L     R4,AIO              GET COMMERCIAL DETAILS ELEMENT               
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4            R4=A(COMMERCIAL DETAILS EL.)                 
         SPACE 1                                                                
         GOTO1 CTYPVAL,DMCB,TACOTYPE  SET COMMERCIAL TYPE IN GLOBAL W/S         
         GOTO1 MEDVAL,DMCB,TACOMED    SET MEDIA IN GLOBAL W/S                   
         SPACE 1                                                                
         MVI   WORK,TACSTYPS       SET TO GET SHOOT DETAILS                     
         LA    R2,SCOSDTEH                                                      
         BAS   RE,DISSTU           DISPLAY STUDIO INFO FOR SHOOT                
         SPACE 1                                                                
         BAS   RE,DISSTAT          DISPLAY STATUS FIELD                         
         SPACE 1                                                                
         MVC   SCOATT,TACOATT      ATTENTION CODE                               
         MVI   SCOATTH+5,L'TACOATT                                              
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLATCDQ,(X'0C',SCOATTH),SCOATTNH  GET NAME           
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         L     R4,AIO              FILTERS                                      
         USING TAFLD,R4                                                         
         MVI   ELCODE,TAFLELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR60                                                             
         MVC   SCOFILT(4),TAFLFLT1                                              
         SPACE 1                                                                
DR60     GOTO1 CHAROUT,DMCB,TACMELQ,SCOCOMMH,TACMTYPG  COMML COMMENT            
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,SCOLCHGH  LAST CHANGED INFO                         
         SPACE 1                                                                
         CLI   TGCTSTTY,TASTTYPP   IF PROGRAMMER THEN DISPLAY INTERNAL#         
         BNE   DRX                                                              
         GOTO1 HEXOUT,DMCB,TGCOM,SCOCOM+1,L'TGCOM,0                             
         NI    SCOCOMH+1,X'FB'     SET HIGH INTENSITY                           
         OI    SCOCOMH+6,X'80'         TRANSMIT                                 
         SPACE 1                                                                
DRX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY PRODUCT(S)                                    
         SPACE 1                                                                
DISPRDS  NTR1                                                                   
         LA    R2,SCOPRDH          R2=A(FIRST PRODUCT CODE FIELD)               
         SPACE 1                                                                
         L     R4,AIO              LOOP THROUGH PRODUCT ELEMENTS                
         MVI   ELCODE,TAPRELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DPRD10   BAS   RE,NEXTEL                                                        
         BNE   DPRDX                                                            
         USING TAPRD,R4                                                         
         MVC   8(L'TAPRPRD,R2),TAPRPRD  DISPLAY PRODUCT CODE                    
         SPACE 1                                                                
         BAS   RE,BUMP             R2=A(PRODUCT NAME FIELD)                     
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'88',TAPRPRD),(R2)  DISPLAY NAME           
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         BAS   RE,BUMP             BUMP TO NEXT CODE FIELD                      
         MVI   ELCODE,TAPRELQ      RESET ELEMENT CODE                           
         B     DPRD10              LOOK FOR ANOTHER PRODUCT                     
         SPACE 1                                                                
DPRDX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY THE STATUS FIELD                              
         SPACE 1                                                                
         USING TACOD,R4            R4=A(COMMERCIAL DETAILS EL.)                 
DISSTAT  NTR1                                                                   
         CLI   TACOSTAT,0          GET OUT IF NOTHING IN STATUS                 
         BE    DSTAX                                                            
         LA    RF,STATTAB                                                       
         LA    R2,SCOSTAT                                                       
         SPACE 1                                                                
DSTA10   CLI   0(RF),X'FF'                                                      
         BE    DSTA30                                                           
         MVC   BYTE,0(RF)                                                       
         NC    BYTE,TACOSTAT       IS BIT ON IN ELEMENT                         
         BZ    DSTA20                                                           
         SPACE 1                                                                
         MVC   0(10,R2),1(RF)      YES - DISPLAY LITERAL                        
         LA    R2,10-1(R2)                                                      
         BAS   RE,SHUFFLE          SHUFFLE BACK TO END AND INSERT COMMA         
         SPACE 1                                                                
DSTA20   LA    RF,L'STATTAB(RF)    BUMP TO NEXT ENTRY IN TABLE                  
         B     DSTA10                                                           
         SPACE 1                                                                
DSTA30   BAS   RE,TRAIL            CLEAR TRAILING COMMA IF NECESSARY            
         SPACE 1                                                                
         LA    R1,SCOSTAT          CALCULATE THE LENGTH OF THE INPUT            
         SR    R2,R1                                                            
         STC   R2,SCOSTATH+5                                                    
         SPACE 1                                                                
DSTAX    B     XIT                                                              
         EJECT                                                                  
*              DISPLAY STUDIO DETAILS ELEMENT                                   
         SPACE 1                                                                
*                                  R2=A(DATE FIELD), WORK=ELEMENT TYPE          
DISSTU   NTR1                                                                   
         MVI   ELCODE,TACSELQ                                                   
         GOTO1 GETL,DMCB,(1,WORK)  GET THE ELEMENT                              
         BNE   DSTUX                                                            
         L     R4,TGELEM                                                        
         USING TACSD,R4            R4=A(STUDIO DETAILS ELEMENT)                 
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(1,TACSDATE),(8,8(R2))  DISPLAY DATE                 
         SPACE 1                                                                
         BAS   RE,BUMP2            BUMP TO NAME                                 
         SPACE 1                                                                
         MVC   8(L'TACSSTUD,R2),TACSSTUD  DISPLAY STUDIO NAME                   
         SPACE 1                                                                
         BAS   RE,BUMP2            BUMP TO CITY                                 
         SPACE 1                                                                
         MVC   8(L'TACSCITY,R2),TACSCITY  DISPLAY STUDIO CITY                   
         SPACE 1                                                                
DSTUX    XIT                                                                    
         EJECT                                                                  
*              ROUTINE INSURES IT'S OK TO DELETE COMMERCIAL RECORD              
         SPACE 1                                                                
CHKDEL   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE COMMERCIAL KEY                          
         SPACE 1                                                                
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         USING TLCOD,R4                                                         
         LA    R3,KEY              R3=A(CAST KEY)                               
         USING TLCAD,R3                                                         
         XC    KEY,KEY             BUILD KEY FOR CAST                           
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TLCOCOM                                                  
         GOTO1 HIGH                                                             
         CLC   TLCAKEY(TLCACOM+L'TLCACOM-TLCAKEY),KEYSAVE  IF FOUND             
         BE    NODELETE                                    GIVE ERROR           
         SPACE 1                                                                
         LA    R3,KEY              R3=A(INVOICE HISTORY KEY)                    
         USING TLINPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLINPCD,TLINHCDQ    FOR THIS COMMERCIAL                          
         MVC   TLINHCOM,TLCOCOM                                                 
         GOTO1 HIGH                                                             
         CLC   TLINPKEY(TLINHINV-TLINPD),KEYSAVE  IF FOUND                      
         BE    NODELETE                           GIVE ERROR                    
         SPACE 1                                                                
         MVC   KEY,SVKEY           ELSE RESTORE KEY                             
         B     XIT                 AND RETURN                                   
         EJECT                                                                  
* ROUTINE TO FIND END OF STRING AND INSERT A COMMA                              
         SPACE 1                                                                
SHUFFLE  DS    0H                  R2=A(FARTHEST POSS. END OF STRING)           
         CLI   0(R2),C' '          SHUFFLE BACK TO END                          
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','          INSERT COMMA                                 
         LA    R2,2(R2)            RETURN R2=A(NEXT AVAILABLE SLOT)             
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE TO CLEAR TRAILING COMMA                                  
         SPACE 1                                                                
TRAIL    DS    0H                  R2=A(1 PAST END OF STRING)                   
         BCTR  R2,0                                                             
         CLI   0(R2),C','          IF IT'S AROUND                               
         BNER  RE                                                               
         MVI   0(R2),C' '          CLEAR TRAILING COMMA                         
         BR    RE                                                               
         SPACE 2                                                                
BUMP2    ZIC   R1,0(R2)            BUMP TWO FIELDS                              
         AR    R2,R1                                                            
         CLI   0(R2),0             RETURN CC EQ IF END OF SCREEN                
         BER   RE                                                               
BUMP     ZIC   R1,0(R2)            BUMP ONE FIELD                               
         AR    R2,R1                                                            
         CLI   0(R2),0             RETURN CC EQ IF END OF SCREEN                
         BR    RE                                                               
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 1                                                                
ADDERR   MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         B     CIDXIT                                                           
         SPACE 1                                                                
NODELETE MVI   ERROR,ERINVDEL      CANNOT DELETE RECORD                         
         B     CIDXIT                                                           
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
         SPACE 1                                                                
NOINPUT  MVI   ERROR,ERNOINP                                                    
         B     ERRXIT                                                           
         SPACE 1                                                                
CLILOCK  LA    R2,SCOCLIH                                                       
         MVI   ERROR,ERCLILCK      CLIENT LOCKED - ADD NOT ALLOWED              
         B     ERRXIT                                                           
         SPACE 1                                                                
PRDLOCK  MVC   MYMSGNO,=Y(ERPRDLCK) PRODUCT LOCKED - ADD NOT ALLOWED            
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     ERRXIT                                                           
         SPACE 1                                                                
BADCTYPE MVI   ERROR,ERRECCTY                                                   
         B     CIDREC                                                           
         SPACE 1                                                                
BADREC   MVI   ERROR,ERINVRAG      INVALID RECORD FOR THIS AGENCY               
         B     CIDREC                                                           
         SPACE 1                                                                
BADCREC  MVI   ERROR,ERINVRCL      INVALID RECORD FOR THIS CLIENT               
         B     CIDREC                                                           
         SPACE 1                                                                
CIDREC   L     R2,EFHREC           CURSOR TO REC/USE FIELD                      
         B     ERRXIT                                                           
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     ERRXIT                                                           
                                                                                
CIDXIT   LA    R2,SCOCIDH          CURSOR TO COMMERCIAL ID                      
ERRXIT   GOTO1 EXIT,DMCB,0                                                      
         SPACE 3                                                                
         SPACE 1                                                                
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
STATTAB  DS    0CL11                                                            
         DC    AL1(TACOSTLO),CL10'LOCK'     LOCKED                              
         DC    AL1(TACOSTRL),CL10'REL'      RELEASED                            
         DC    X'FF'                                                            
         SPACE 1                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'PCOMML  ',CL8'LIST    '                               
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'CID     ',CL8'NEW     '                               
PF14X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
MAXPRDS  EQU   6                   MAX N'PRODUCTS ALLOWED                       
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRBAD                                                       
         SPACE 2                                                                
*              SAVED STORAGE AT END OF TWA0                                     
         SPACE 1                                                                
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
SVTGAGY  DS    CL(L'TGAGY)         SAVED TGAGY                                  
PTRBLK   DS    CL(20*L'TLDRREC+1)  PASSIVE + ACTIVE PTRS                        
SVNUTSEQ DS    XL(TANULNQ1)        SAVED NEXT CAST SEQ NUMBER ELEMENT           
         DS    0X                                                               
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036TAGENBA   05/29/15'                                      
         END                                                                    
