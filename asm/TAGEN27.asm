*          DATA SET TAGEN27    AT LEVEL 092 AS OF 08/20/15                      
*PHASE T70227C,*                                                                
         TITLE 'T70227 - HISTORY MAINTENANCE'                                   
T70227   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70227,R6                                                      
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
         TM    TGSYSTAT,TASYSBRT                                                
         BO    *+12                                                             
         OI    SINPF18H+1,X'0C'                                                 
         OI    SINPF18H+6,X'80'                                                 
         SPACE 1                                                                
         GOTOR PF2COM                                                           
HIS2     GOTOR PFCMNT              IF PF15 HIT, SET UP FOR CMNT REC             
         GOTO1 INITIAL,DMCB,PFTAB                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   HIS10                                                            
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SINAGYH),SINAGYNH  AGENCY             
         USING TAAYD,R4                                                         
         L     R4,AIO              R4=A(AGENCY RECORD)                          
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL            GET AGENCY DETAILS EL                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TGOFF,TAAYTPOF      SAVE OFFICE                                  
         MVC   AGYSTAT6,TAAYSTA6   SAVE AGY STATUS 6                            
         DROP  R4                                                               
         SPACE 1                                                                
         LA    R2,SININVH                                                       
         CLI   5(R2),0             IF NO INVOICE INPUT                          
         BNE   HIS5                                                             
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'24',SININVH) USE GLOBAL INVOICE           
         SPACE 1                                                                
         CLI   ERROR,MISSING       IF THERE IS A GLOBAL INVOICE                 
         BE    THEEND                                                           
         MVC   CINV,TGINV          COMPLEMENTED INVOICE NUMBER                  
         XC    SININV,=6X'FF'      UN-COMPLEMENT INVOICE NUMBER                 
         GOTO1 TINVCON,DMCB,SININV,INV,DATCON CONVERT FOR DISPLAY               
         MVC   SININV,INV                                                       
         CLI   0(R1),X'FF'         IF INVALID                                   
         BNE   HIS3                                                             
         XC    SININV,SININV       CLEAR FROM SCREEN                            
         B     FLDMISS             REQUIRE INVOICE INPUT                        
         SPACE                                                                  
HIS3     LA    R2,SINAGYH                                                       
         CLI   ERROR,NOTFOUND                                                   
         BE    THEEND                                                           
         B     HIS9                                                             
         SPACE                                                                  
HIS5     GOTO1 TINVCON,DMCB,SININV,INV,DATCON CONVERT INV INPUT FOR KEY         
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    INV,=6X'FF'         COMPLEMENT INVOICE NUMBER                    
         MVC   CINV,INV            COMPLEMENTED INVOICE NUMBER                  
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',INV) GET THE RECORD                   
         BNE   THEEND                                                           
         SPACE                                                                  
         USING TAIND,R4                                                         
HIS9     L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL            GET INVOICE STATUS EL                        
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         LA    R2,SININVH          R2=A(INVOICE FIELD HEADER)                   
         TM    TAINSTA2,TAINSADJ   IF NOT AN ADJUSTMENT                         
         BO    HIS9A                                                            
         TM    TAINSTAT,TAINSPAY   OK IF PAID ALREADY                           
         BO    HIS9A                                                            
         SPACE                                                                  
         USING TAPDD,R4                                                         
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL            GET PAYMENT DETAILS EL                       
         BNE   NOTPD                                                            
         TM    TAPDOPT3,TAPDOGRY   IF NOT GREY PAYMENT                          
         BZ    NOTPD               INVOICE MUST BE PAID ALREADY                 
                                                                                
HIS9A    MVI   CLISTAT2,0                                                       
         CLC   =C'999999',TGAGY    SKIP FOR ADJ INVOICES                        
         BE    HIS9C                                                            
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL            GET PAYMENT DETAILS EL                       
         BNE   HIS9C                                                            
                                                                                
         MVC   SVKEY,KEY           SAVE KEY FOR CHANGE                          
         MVC   AIO,AIO2            READ CLIENT RECORD                           
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',TAPDCLI)                              
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO1            RESET AIO                                    
         MVC   KEY,SVKEY                                                        
         DROP  R4                                                               
                                                                                
         USING TACID,R4                                                         
HIS9B    L     R4,AIO2             R4=A(CLIENT RECORD)                          
         MVI   ELCODE,TACIELQ                                                   
         BAS   RE,GETEL            GET CLIENT INFO EL                           
         BNE   HIS9C                                                            
         MVC   CLISTAT2,TACISTA2   SAVE CLI STATUS 2                            
         DROP  R4                                                               
                                                                                
HIS9C    GOTOR VALDISP                                                          
         SPACE                                                                  
         OI    SINPF16H+1,X'0C'    HIDE OPTIONS/DISPLAY PFKEY                   
         OI    SINPF16H+6,X'80'                                                 
         SPACE                                                                  
         L     R4,AIO                                                           
         USING TLIND,R4                                                         
         MVC   SVAGY2,TLINAGY      SAVE AGENCY                                  
         DROP  R4                                                               
         B     XIT                                                              
         SPACE 3                                                                
HIS10    CLI   MODE,DISPREC                                                     
         BE    HIS15                                                            
         CLI   MODE,XRECPUT        IF RECORD HAS BEEN CHANGED                   
         BNE   HIS20                                                            
         BAS   RE,TRAPEST          TRAP MISSING ESTIMATE & AUTH/PO              
*        GOTO1 ADDPTRS,DMCB,PTRS   HANDLE PASSIVE POINTERS                      
         SPACE                                                                  
HIS15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 2                                                                
TRAPEST  NTR1                                                                   
         L     R4,AIO              IF INVOICE RECORD                            
         CLI   0(R4),TLHCCDQ                                                    
         BE    XIT                                                              
         TM    INSTAT,TAINSBIL     AND WAS NOT BILLED                           
         BO    XIT                                                              
         BAS   RE,GETSTAT          RESET STATUS FROM AGENCY RECORD              
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         MVC   AIO,AIO2            RESET STATUS FROM CLIENT RECORD              
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',TAPDCLI)                              
         BE    *+14                                                             
         MVC   AIO,AIO1            RESET AIO                                    
         BNE   XIT                 MIGHT HAVE CHANGED AGY AND/OR CLI            
         BRAS  RE,GETRULES                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4                                                         
         TM    TAINSTA2,TAINSPRM                                                
         BO    TRAPEST5                                                         
*                                                                               
         MVI   ELCODE,TANUELQ      IF ESTIMATE NUMBER NOT FOUND                 
         GOTO1 GETL,DMCB,(1,=AL1(TANUTEST))                                     
         BE    TRAPEST5                                                         
         TM    PDOPT4,TAPDONOI     EST NOT REQUIRED FOR NO INTERFACE            
         BO    TRAPEST5                                                         
         CLI   INTER,C'Y'                                                       
         BNE   *+6                                                              
         DC    H'0'                DIE IF REQUESTED ON INTERFACE                
         TM    AGYSTAT,TAAYSEST                                                 
         BZ    *+6                                                              
         DC    H'0'                DIE IF REQUIRED FOR AGENCY                   
*                                                                               
TRAPEST5 MVI   ELCODE,TANUELQ      IF AUTH/PO NOT FOUND                         
         GOTO1 GETL,DMCB,(1,=AL1(TANUTAUT))                                     
         BE    XIT                                                              
         TM    AGYSTAT,TAAYSAPO                                                 
         BZ    XIT                                                              
         DC    H'0'                DIE IF AUTH/PO REQUIRED                      
         EJECT                                                                  
HIS20    CLI   MODE,DISPKEY                                                     
         BNE   HIS100                                                           
         CLI   THISLSEL,C'D'                                                    
         BNE   HIS30                                                            
         MVC   CONACT,=CL8'DELETE' REPLACE SELECT ACTION WITH DELETE            
         OI    CONACTH+6,X'80'                                                  
         B     ACTERR              DON'T ALLOW DELETE                           
         SPACE                                                                  
HIS30    MVC   SVKEY,KEY           SAVE KEY FOR CHANGE                          
         L     R4,AIO              R4=A(RECORD BEING DISPLAYED)                 
         MVC   AIO,AIO2            DON'T CREAM RECORD                           
         CLI   0(R4),TLHCCDQ                                                    
         BNE   HIS50                                                            
         SPACE                                                                  
         USING TLHCD,R4                                                         
         XC    SININV,SININV                                                    
         OI    SININVH+6,X'01'     CHANGE TO MODIFIED FOR NEXT INPUT            
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TLHCCOM)  GET COMMERCIAL REC         
         BE    *+6                                                              
         DC    H'0'                SHOULD BE THERE                              
         SPACE                                                                  
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         MVC   SINAGY,TLCOAGY      AGENCY                                       
         OI    SINAGYH+6,X'80'     TRANSMIT                                     
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL            GET COMMERCIAL DETAILS ELEMENT               
         BE    *+6                                                              
         DC    H'0'                SHOULD BE THERE                              
         SPACE                                                                  
         USING TACOD,R4                                                         
         MVC   COMMID,TACOCID          SAVE COMMERCIAL ID                       
         GOTO1 CHAROUT,DMCB,TANAELQ,0  GET COMMERCIAL TITLE                     
         MVC   COMTITLE,TGNAME         SAVE IT                                  
         GOTO1 RECVAL,DMCB,(X'80',TLAYCDQ),(X'88',SINAGY),SINAGYNH              
         B     HIS60                                                            
         SPACE                                                                  
         USING TLIND,R4                                                         
HIS50    MVC   SINAGY,TLINAGY      AGENCY WHICH INVOICE WAS PAID UNDER          
         OI    SINAGYH+6,X'80'     TRANSMIT                                     
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,(X'80',TLAYCDQ),(X'88',SINAGY),SINAGYNH              
         MVC   INV,TLININV                                                      
         MVC   CINV,TLININV                                                     
         XC    INV,=6X'FF'         UN-COMPLEMENT INVOICE NUMBER                 
         GOTO1 TINVCON,DMCB,INV,SININV,DATCON                                   
HIS60    OI    SININVH+6,X'80'     TRANSMIT                                     
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            RESTORE AIO                                  
         GOTOR VALDISP                                                          
         B     XIT                                                              
         EJECT                                                                  
HIS100   CLI   MODE,VALREC         IF MODE IS VALIDATE RECORD                   
         BNE   XIT                                                              
*        GOTO1 SAVPTRS,DMCB,PTRS   SAVE PASSIVE POINTERS                        
         SPACE                                                                  
         BAS   RE,CHGREC           MUST BE CHANGE                               
         B     XIT                                                              
         EJECT                                                                  
*              CHANGE THE RECORD                                                
         SPACE 1                                                                
CHGREC   NTR1                                                                   
         L     R4,AIO              R4=A(RECORD BEING DISPLAYED)                 
         CLI   0(R4),TLHCCDQ                                                    
         BNE   CHG10               TEST HAVE HISTORY COMMENT RECORD             
         GOTO1 NAMIN,DMCB,TACMELQ,SINHCOMH,TACMTYPH  COMMENT REQUIRED           
         B     CHGX                                                             
         SPACE                                                                  
* NO-OP  TM    INSTAT,TAINSBIL+TAINSCHK IF THIS INVOICE IS BILLED/CHK           
* NO-OP  BNZ   CHG30                      CANNOT CHANGE AUTH/PO                 
CHG10    LA    R2,SINAUTHH                                                      
         CLI   5(R2),0                                                          
         BNE   CHG15                                                            
         TM    AGYSTAT,TAAYSAPO    IS AUTH/PO REQUIRED                          
         BO    FLDMISS                                                          
         TM    AGYSTAT4,TAAYPOV    PO REQUIRED FOR AGENCY                       
         BO    FLDMISS                                                          
*                                                                               
CHG15    TM    AGYSTAT4,TAAYPOV    O+M NEEDS 10 CHAR PO                         
         BZ    CHG17                                                            
         CLI   5(R2),10                                                         
         BE    CHG17                                                            
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SINAUTHH+5                                                    
         BCTR  R1,0                                                             
         MVC   WORK(10),=CL10'0000000000'                                       
         LA    RE,WORK+9                                                        
         LA    RF,SINAUTH                                                       
         AR    RF,R1                                                            
         LA    R1,1(R1)                                                         
CHG16    MVC   0(1,RE),0(RF)                                                    
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         BCT   R1,CHG16                                                         
         MVI   SINAUTHH+5,10                                                    
         MVC   SINAUTH(10),WORK                                                 
         OI    SINAUTHH+6,X'80'                                                 
*                                                                               
CHG17    GOTO1 NAMIN,DMCB,TANUELQ,(X'80',SINAUTHH),TANUTAUT  AUTH/PO            
*                                                                               
         USING TAIND,R4                                                         
CHG30    LA    R2,SINESTH                                                       
         TM    1(R2),X'20'         IF PROTECTED                                 
         BO    CHG60               SKIP                                         
         TM    INSTAT,TAINSBIL     IF THIS INVOICE WAS NOT BILLED               
         BO    CHG40                                                            
         BAS   RE,VALEST           VALIDATE ESTIMATE NUMBER                     
         SPACE                                                                  
CHG40    GOTO1 NAMIN,DMCB,TANUELQ,(X'80',SINESTH),TANUTEST  EST #               
         SPACE                                                                  
CHG60    L     R4,AIO              R4=A(RECORD BEING DISPLAYED)                 
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ      ALLOW PERIOD CHANGE                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TAPDESPD,0          CLEAR PREVIOUS PERIOD                        
         LA    R2,SINPDH                                                        
         CLI   5(R2),0                                                          
         BE    CHG80                                                            
         GOTO1 VALINUM                                                          
         CLI   ACTUAL,13           MAXIMUM IS 13                                
         BH    FLDINV                                                           
         MVC   TAPDESPD,ACTUAL                                                  
         SPACE                                                                  
CHG80    TM    INSTAT,TAINSBIL+TAINSCHK IF THIS INVOICE IS BILLED/CHK           
         BNZ   CHGX                       CANNOT CHANGE COMMENT                 
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SINICOMH),TACMTYPG                     
         SPACE                                                                  
CHGX     GOTO1 ACTVIN,DMCB,(X'80',SINLCHGH)                                     
         L     R4,AIO1                                                          
         CLI   0(R4),TLHCCDQ       IF HISTORY COMMENT - DON'T REREAD            
         BE    CHGX20                                                           
         OC    SVAGY2,SVAGY2                                                    
         BZ    CHGX10                                                           
         MVC   TGAGY,SVAGY2         RESTORE GLOBAL AGENCY                       
         SPACE                                                                  
CHGX10   MVC   AIO,AIO2            ELSE RE-GET INV RECORD TO PROTECT            
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',CINV)                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            AGAINST GETREC/PUTREC SYNDROME               
         SPACE 1                                                                
CHGX20   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE ESTIMATE NUMBER/PRODUCTION JOB               
         SPACE 1                                                                
VALEST   NTR1                                                                   
         MVC   AIO,AIO2                                                         
         CLI   5(R2),0             TEST FOR INPUT                               
         BNE   VEST2                                                            
         TM    PDOPT4,TAPDONOI     INVOICE FORCED TO NO INTERFACE               
         BO    VESTX                                                            
         CLI   INTER,C'Y'          REQUIRED IF ON INTERFACE                     
         BE    FLDMISS                                                          
         TM    AGYSTAT,TAAYSEST    OR IF REQUIRED FOR AGENCY                    
         BO    FLDMISS                                                          
         B     VESTX                                                            
         SPACE 1                                                                
VEST2    CLI   INTER,C'Y'          IF ON INTERFACE, MUST VALIDATE JOB           
         BNE   VESTX                                                            
         CLI   5(R2),12            MAXIMUM OF 12 CHARS (CLI/PRD/JOB)            
         BH    FLDINV                                                           
         MVC   KEY,SPACES          BUILD JOB KEY FOR ACCOUNT FILE               
         MVC   KEY+1(2),=C'SJ'     UNIT/LEDGER                                  
         MVC   KEY+3(12),8(R2)     ASSUME I/P ENTIRE JOB CODE                   
         SPACE 1                                                                
         CLI   5(R2),6             IF FIELD IS LE 6 CHARS. (JOB ONLY)           
         BH    VEST4                                                            
         MVC   KEY+3(3),PRODCLI    RE-BUILD JOB - PRODUCTION CLIENT             
         MVC   KEY+6(3),PRODPRD                              PRODUCT            
         MVC   KEY+9(6),8(R2)                                JOB                
         MVC   8(12,R2),KEY+3      MOVE TO SCREEN AS WELL                       
         MVI   5(R2),12            SET LENGTH FOR NAMIN                         
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
VEST4    OC    KEY+3(12),SPACES    INSURE KEY PADDED WITH SPACES                
         SPACE 1                                                                
         GOTO1 READACC,DMCB,(X'80',0) READ ACCOUNT FILE                         
         BNE   THEEND                                                           
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         TM    ACSTATUS,X'60'      TEST FOR CLOSED/LOCKED                       
         BNZ   CLOSLOCK                                                         
         TM    TGSYSTA2,TASYSPDJ   IF PROHIBITING DRAFT JOBS                    
         BZ    VESTX                                                            
         TM    ACSTATUS,X'08'      TEST FOR DRAFT                               
         BO    DFTJOB                                                           
         SPACE 1                                                                
VESTX    OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         BRAS  RE,CLEARSCR         CLEAR SCREEN                                 
         MVC   SVAGY,TGAGY         SAVE ORIGINAL AGY                            
         L     R4,AIO                                                           
         CLI   0(R4),TLHCCDQ       TEST HAVE HISTORY COMMENT RECORD             
         BNE   DISP2                                                            
         USING TLHCD,R4                                                         
         MVC   SINCID,COMMID       COMMERCIAL ID                                
         MVC   SINCOMN,COMTITLE    COMMERCIAL TITLE                             
         MVC   FULL,TLHCCOM                                                     
         BRAS  RE,SETCID           SET GLOBAL COMMERCIAL ID                     
         B     DISPX                                                            
         SPACE                                                                  
DISP2    BAS   RE,GETSTAT          GET AGENCY STATUS                            
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ      GET VERSIONS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DISP2B                                                           
         SPACE                                                                  
         USING TAVRD,R4                                                         
         MVC   SINLID,TAVRCID      DISPLAY VERSION                              
         EDIT  TAVRVERS,SINLFT,ALIGN=LEFT                                       
         B     DISP3                                                            
         SPACE                                                                  
DISP2B   MVI   ELCODE,TALFELQ      GET LIFT DETAILS ELEMENT                     
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DISP2D                                                           
         SPACE                                                                  
         USING TALFD,R4                                                         
         MVI   SINLFT,C'Y'         SET LFT TO Y                                 
         MVC   SINLID,TALFLID      LIFT ID                                      
         B     DISP3                                                            
         SPACE                                                                  
DISP2D   L     R4,AIO                                                           
         MVI   ELCODE,TASBELQ      GET SOAP CABLE ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   DISP3                                                            
         SPACE                                                                  
         USING TASBD,R4                                                         
         MVC   SINLID,TASBCID2     2ND COMMERCIAL ID                            
         SPACE                                                                  
DISP3    MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DISP5                                                            
         SPACE                                                                  
         USING TACOD,R4                                                         
         CLI   TACOSEC,0                                                        
         BE    DISP4                                                            
         EDIT  TACOSEC,(3,SINSEC+1),ALIGN=LEFT  COMMERCIAL LENGTH               
         MVI   SINSEC,C':'                                                      
         SPACE 1                                                                
DISP4    MVC   SINCID,TACOCID      COMMERCIAL ID                                
         GOTO1 MEDVAL,DMCB,TACOMED MEDIA                                        
         BNE   *+10                                                             
         MVC   SINMEDN,TGMENAME                                                 
         SPACE                                                                  
DISP5    GOTO1 CHAROUT,DMCB,TAFNELQ,SINCOMNH,TAFNTTTL COMMERCIAL TITLE          
         GOTO1 (RF),(R1),TANUELQ,SINAUTHH,TANUTAUT AUTH/PO                      
         GOTO1 (RF),(R1),TANUELQ,SINESTH,TANUTEST  EST #                        
         GOTO1 (RF),(R1),TAFNELQ,0,TAFNTADV        ADVICE PAID                  
         BNE   DISP5B                                                           
         MVC   SINADV(2),=C'A='    DISPLAY "A=ADVICE"                           
         MVC   SINADV+2(6),TGNAME                                               
         NI    SINADVH+1,X'F3'     TURN OFF ZERO INTENSITY                      
         OI    SINADVH+1,X'08'     SET HIGH-INTENSITY                           
         SPACE                                                                  
DISP5B   MVI   ELCODE,TAINELQ                                                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DISP6                                                            
         USING TAIND,R4                                                         
         TM    TAINSTA3,TAINSNI    NOT INTERFACE                                
         BZ    *+8                                                              
         MVI   INTER,C'N'                                                       
         TM    TAINSTA2,TAINSPRM                                                
         BNO   DISP6                                                            
         LA    R2,SINESTH                                                       
         MVC   8(5,R2),=C'SPLIT'   INDICATE SO                                  
         OI    1(R2),X'20'         AND PROTECT                                  
*                                                                               
         SPACE                                                                  
DISP6    MVI   ELCODE,TAUCELQ      GET CANADIAN/US INVOICE ELEMENT              
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DISP8                                                            
         USING TAUCD,R4                                                         
         MVC   SINUSCN(2),=C'U$'                                                
         MVC   USCANINV,TAUCINU    US INVOICE                                   
         CLC   USCANINV,TGINV      INVOICE OR LINKED INVOICE?                   
         BNE   DISP7                                                            
         MVC   SINUSCN(2),=C'C$'                                                
         MVC   USCANINV,TAUCINC    CAN INVOICE                                  
         DROP  R4                                                               
*                                                                               
DISP7    XC    USCANINV,=6X'FF'    UNCOMPLEMENT                                 
         GOTO1 TINVCON,DMCB,USCANINV,SINUSCN+2,DATCON                           
         NI    SINUSCNH+1,X'F3'    TURN OFF ZERO INTENSITY                      
         OI    SINUSCNH+1,X'08'    SET HIGH INTENSITY                           
*                                                                               
DISP8    GOTOR SETEURT,DMCB,AIO    SET EURO CONVERSION RATE (IF NECC.)          
         GOTOR SETCNRT,DMCB,AIO    SET CAN CONVERSION RATE (IF NECC.)           
*                                                                               
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                 ELMNT FOUND                                  
         DC    H'0'                MUST BE THERE BECAUSE PAID ALREADY           
         SPACE                                                                  
         USING TAPDD,R4                                                         
         MVC   SINCCVH,=CL18'Canadian Conv Rate'                                
         OI    SINCCVHH+6,X'80'                                                 
         TM    TAPDPST2,TAPDPEUR                                                
         BZ    DISP9                                                            
         MVC   SINCCVH,=CL18'Euro Conv Rate'                                    
                                                                                
DISP9    TM    TAPDSTA2,TAPDSSUB                                                
         BNO   DISP10                                                           
         MVC   SINMSG(12),=CL12'(SUBSIDIARY)'                                   
         NI    SINMSGH+1,X'F3'     TURN OFF ZERO INTENSITY                      
         OI    SINMSGH+1,X'08'     SET HIGH-INTENSITY                           
*                                                                               
DISP10   MVC   TGCLI,TAPDCLI       SAVE GLOBAL INFO                             
         MVC   TGPRD,TAPDPRD                                                    
         TM    TAPDSTA2,TAPDSLFA   IF PAYMENT TO ALL ON COMM'L                  
         BZ    *+8                                                              
         MVI   SINLFT,C'A'         SET LFT TO A                                 
         MVC   PDOPT1,TAPDOPT1     SAVE PAYMENT OPTIONS                         
         MVC   PDOPT2,TAPDOPT2                                                  
         MVC   PDOPT3,TAPDOPT3                                                  
         MVC   PDOPT4,TAPDOPT4                                                  
         MVC   PDSTAT,TAPDSTAT     SAVE STATUS                                  
         MVC   PDPST2,TAPDPST2                                                  
         CLI   TAPDESPD,0                                                       
         BE    DISP12                                                           
         EDIT  TAPDESPD,(2,SINPD),ALIGN=LEFT PERIOD                             
         SPACE                                                                  
DISP12   CLI   TAPDCYCS,0          IF THERE ARE CYCLE DATES                     
         BE    DISP15                                                           
         GOTO1 DATCON,DMCB,(1,TAPDCYCS),(8,SINCYC)   START CYCLE                
         OC    TAPDCYCE,TAPDCYCE   SKIP IF NO CYCLE END DATE                    
         BZ    DISP15                                                           
         MVI   SINCYC+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(1,TAPDCYCE),(8,SINCYC+9) END CYCLE                  
*                                                                               
DISP15   OI    SINPFKYH+6,X'80'    TRANSMIT                                     
*                                                                               
         GOTO1 USEVAL,DMCB,(X'20',TAPDUSE),TAPDTYPE  GET USE INFO               
         BNE   DISP18                                                           
         BRAS  RE,SETDETS          GET USE DETAILS                              
         CLI   TAPDCYCS,0          IF THERE ARE CYCLE DATES                     
         BE    DISP20                                                           
         BRAS  RE,SETLCYC          GET LENGTH OF CYCLE                          
         BNE   *+10                                                             
         MVC   SINLCYC,TGDUB       STRING RETURNED IN TGDUB                     
         B     DISP20                                                           
*                                                                               
DISP18   GOTO1 USEVAL,DMCB,(X'60',TAPDUSE)  GET USE INFO W/O TYPE               
         BNE   *+10                                                             
         MVC   SINUSE(L'TGUSNAME),TGUSNAME  DISPLAY USE NAME                    
         SPACE                                                                  
DISP20   TM    TAPDPST1,TAPDPCRD   CREDIT PAYMENT                               
         BZ    DISP21                                                           
         MVI   SINEINV,C'Y'        FIRST MOVE IN 'Y'                            
         GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTINV IF THERE'S ERROR INVOICE         
         BNE   DISP21                                                           
         GOTO1 TINVCON,DMCB,TGNAME,SINEINV,DATCON CONVERT FOR DISPLAY           
         CLI   0(R1),X'FF'                                                      
         BNE   *+8                                                              
         MVI   SINEINV,C'Y'        MOVE 'Y' BACK IF CAN'T CONVERT               
         SPACE                                                                  
DISP21   TM    TAPDPST1,TAPDPBNP   BILL-NO-PAYROLL PAYMENT                      
         BZ    *+8                                                              
         MVI   SINBNP,C'Y'                                                      
         TM    TAPDPST1,TAPDPDTL   DISPLAY DETIAL                               
         BZ    *+8                                                              
         MVI   SINDTL,C'Y'                                                      
         SPACE                                                                  
         MVC   FULL,TAPDCOM        FULL = INT. COMM'L #(USED BY SETCID)         
         SPACE 1                                                                
         MVC   AIO,AIO2            DON'T CREAM RECORD IN AIO                    
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'88',TGCLI),SINCLINH CLIENT NAME           
         BRAS  RE,GETRULES         GET BILLING RULES FOR CLIENT                 
         SPACE                                                                  
         MVC   PRODCLI,TGCLI       INIT PRODCLI = TAL CLIENT                    
         BAS   RE,DISPCLPR         DISPLAY CLIENT/PRODUCT CODE                  
         USING TAISD,R4                                                         
         MVI   ELCODE,TAISELQ      GET INTERFACE SUBSIDIARY ELEMENT             
         GOTO1 GETL,DMCB,(1,=AL1(TAISTYPC))                                     
         BNE   *+14                                                             
         L     R4,TGELEM                                                        
         MVC   PRODCLI,TAISCDE     SAVE PRODUCTION CLIENT                       
         SPACE 1                                                                
         MVC   PRODPRD,TGPRD       INIT PRODPRD = TAL PRODUCT                   
         OC    TGPRD,TGPRD         IF WE HAVE PRD = GET RECORD                  
         BZ    DISP24                                                           
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A0',TGPRD)                                
         SPACE 1                                                                
         USING TAISD,R4                                                         
         MVI   ELCODE,TAISELQ      GET INTERFACE SUBSIDIARY ELEMENT             
         GOTO1 GETL,DMCB,(1,=AL1(TAISTYPP))                                     
         BNE   *+14                                                             
         L     R4,TGELEM                                                        
         MVC   PRODPRD,TAISCDE     SAVE PRODUCTION PRODUCT                      
         SPACE                                                                  
DISP24   MVC   AIO,AIO1            RESTORE IO AREA                              
         SPACE                                                                  
         USING TABDD,R4                                                         
         XC    SVBDACOM,SVBDACOM                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TABDELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   DISP24A                                                          
         CLI   TABDLEN,TABDLN2Q                                                 
         BL    DISP24A                                                          
         MVC   SVBDACOM,TABDACOM   SAVE AGENCY COMMISSION                       
         DROP  R4                                                               
         SPACE 1                                                                
DISP24A  L     R4,AIO                                                           
         MVC   ELCODE,HTAPDELQ     RE - GET PAYMENT DETAILS ELEMENT             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         BRAS  RE,SETCID           SET GLOBAL COMMERCIAL ID                     
         SPACE                                                                  
         BAS   RE,DISPOPTS         DISPLAY OPTIONS                              
         SPACE                                                                  
         BAS   RE,DISPA80          DISPLAY AMOUNTS FROM 80 ELEMENT              
         SPACE                                                                  
         GOTO1 CHAROUT,DMCB,TACMELQ,SINICOMH,TACMTYPG GENERAL COMMENT           
         GOTO1 CHAROUT,DMCB,TACMELQ,SINEPISH,TACMTYPS SOR EPIS INPUT            
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TASBELQ      GET SOAP CABLE ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         BRAS  RE,DISPGRSE         DISPLAY GRS/EPI                              
*                                                                               
         MVI   ELCODE,TADDELQ      GET DUE DATE ELEMENT                         
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DISP25                                                           
         SPACE                                                                  
         USING TADDD,R4                                                         
         OC    TADDDATE,TADDDATE                                                
         BZ    DISP25              SKIP IF NO DUE DATE                          
         GOTO1 DATCON,DMCB,(1,TADDDATE),(8,SINDDTE) DUE DATE                    
         SPACE                                                                  
DISP25   L     R4,AIO                                                           
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BE    DISP25A                                                          
         SPACE                                                                  
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTOWB))                                     
         BNE   DISP26                                                           
         SPACE                                                                  
         USING TAFND,R4                                                         
DISP25A  L     R4,TGELEM                                                        
         NI    SINWEBHH+1,X'FF'-X'04'                                           
         MVC   SINWEBI,TAFNNAME                                                 
         SPACE                                                                  
         MVC   SINWEBH,=C'Authorized via Cerno'                                 
         CLC   =C'HF',TAFNNAME                                                  
         BE    DISP26                                                           
         MVC   SINWEBH,=C'Authorized via nAble'                                 
         CLC   =C'NP',TAFNNAME                                                  
         BE    DISP26                                                           
         MVC   SINWEBH,=C'Authorized via Vita '                                 
         CLC   =C'VS',TAFNNAME                                                  
         BE    DISP26                                                           
         CLC   =C'TS',TAFNNAME                                                  
         BE    DISP26                                                           
         CLC   =C'VC',TAFNNAME                                                  
         BE    DISP26                                                           
         CLC   =C'TC',TAFNNAME                                                  
         BE    DISP26                                                           
         CLC   =C'RS',TAFNNAME                                                  
         BE    DISP26                                                           
         CLC   =C'RC',TAFNNAME                                                  
         BE    DISP26                                                           
         DC    H'00'                                                            
         DROP  R4                                                               
         SPACE                                                                  
DISP26   MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         USING TAIND,R4                                                         
         TM    TAINSTA2,TAINSPRM                                                
         BNO   DISP27                                                           
         MVC   SINMSG(9),=CL9'(PRIMARY)'                                        
         NI    SINMSGH+1,X'F3'     TURN OFF ZERO INTENSITY                      
         OI    SINMSGH+1,X'08'     SET HIGH-INTENSITY                           
*                                                                               
DISP27   MVC   INSTAT,TAINSTAT                                                  
         GOTO1 DATCON,DMCB,(1,TAINPDTE),(8,SINPDTE) PAYMENT DATE                
         GOTO1 DATCON,DMCB,(1,TAINCDTE),(8,SINCDTE) CHECK DATE                  
         OC    TAINBDTE,TAINBDTE   IF BILLED                                    
         BNZ   DISP27A                                                          
         OC    SVBDACOM,SVBDACOM                                                
         BZ    DISP30                                                           
         ICM   R3,15,SVBDACOM                                                   
         LA    R5,SINACOM                                                       
         BAS   RE,EDITAMT                                                       
         B     DISP30                                                           
DISP27A  GOTO1 DATCON,DMCB,(1,TAINBDTE),(8,SINIDTE) INVOICE DATE                
DISP28   LA    RF,DISPA84B         SET A(DISPLAY ROUTINE) FOR BILLED            
         ST    RF,ADISPA84                                                      
         B     DISP35                                                           
         SPACE                                                                  
DISP30   TM    TAINSTAT,TAINSCIN   IF A CANCEL INVOICE                          
         BO    DISP28                                                           
         TM    TAINSTA2,TAINSHLP   OR COD INVOICE PRINTED                       
         BO    DISP28              DISPLAY AS IF BILLED                         
         SPACE                                                                  
         LA    RF,DISPA84          SET A(DISPLAY ROUTINE)FOR NOT BILLED         
         ST    RF,ADISPA84                                                      
DISP35   MVC   ELCODE,HTABDELQ     GET BILLING DETAILS ELEMENT                  
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DISP50                                                           
         SPACE                                                                  
*&&DO                                                                           
         USING TABDD,R4                                                         
         CLC   SINMSG(12),=CL12'(SUBSIDIARY)'                                   
         BNE   DISP40                                                           
         L     RE,TABDHNDC                                                      
         A     RE,SVFEE                                                         
         ST    RE,TABDHNDC                                                      
         DROP  R4                                                               
*&&                                                                             
         SPACE 1                                                                
DISP40   L     RF,ADISPA84         GET A(DISPLAY ROUTINE)                       
         BASR  RE,RF               DISPLAY AMOUNTS FROM 84 ELEMENT              
         SPACE                                                                  
DISP50   GOTO1 FLDVAL,DMCB,(1,SINBOVRH),SINBVDTH                                
         SPACE 1                                                                
         MVI   ELCODE,TARAELQ      GET RATE EL.                                 
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   DISPX                                                            
         USING TARAD,R4                                                         
         CLI   TARASTA2,0                                                       
         BE    *+10                                                             
         MVC   SINBOVR(5),=C'BOVER'                                             
         L     R4,AIO                                                           
         USING TAACD,R4                                                         
         MVI   ELCODE,TAACELQ      GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISP55   BAS   RE,NEXTEL                                                        
         BNE   DISPX                                                            
         CLI   TAACSCR,X'60'                                                    
         BNE   DISP55                                                           
         OC    TAACCDTE,TAACCDTE                                                
         BZ    DISPX                                                            
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,SINBVDT)                             
         SPACE                                                                  
DISPX    GOTO1 CHAROUT,DMCB,TACMELQ,SINHCOMH,TACMTYPH HISTORY COMMENT           
         GOTO1 ACTVOUT,DMCB,(X'80',SINLCHGH)                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS AGENCY STATUS FOR INVOICE RECORD IN AIO             
         SPACE                                                                  
         USING TAPDD,R4                                                         
GETSTAT  NTR1                                                                   
         L     R4,AIO                                                           
         MVC   AIO,AIO2            DON'T CREAM RECORD IN AIO                    
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL            GET PAYMENT DETAILS ELEMENT                  
         BE    GETS3               IF ELMNT FOUND, OKAY                         
         L     R4,AIO1             FOR CHKIFPD SUBROUTINE                       
         BAS   RE,CHKIFPD          ELSE, IS INV MARKED AS PAID?                 
         BNE   NOTPD               IF NOT PAID, REPORT THIS ERROR               
         DC    H'0'                TRAP VANISHING EST# BUG (4/30/96)            
         SPACE                                                                  
GETS3    MVC   TGDUB(L'TGAGY),TGAGY                                             
         OC    TAPDCOM,TAPDCOM                                                  
         BZ    GETS5                                                            
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TAPDCOM)  GET COMMERCIAL REC         
         BNE   GETS5                                                            
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   TGDUB(L'TLCOAGY),TLCOAGY         FOR AGENCY IN CASE              
GETS5    GOTO1 RECVAL,DMCB,(X'80',TLAYCDQ),(X'A0',TGDUB) AGENCY CHANGED         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         USING TAAYD,R4                                                         
         MVI   INTER,C'N'          DEFAULT - NOT ON INTERFACE                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   AGYSTAT,TAAYSTAT    SAVE STATUS                                  
         MVC   AGYSTAT4,TAAYSTA4                                                
*                                                                               
         TM    TAAYSTA6,TAAYST10   TEST ON TYPE 10 JOB VALIDATION               
         BZ    GETS6                                                            
         MVI   INTER,C'Y'          SET LOCAL SWITCH                             
         B     GETSX                                                            
*                                                                               
GETS6    BRAS  RE,GETRULES         GET BILLING RULES                            
         SPACE 1                                                                
GETSX    MVC   AIO,AIO1            RESTORE AIO                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS FOR PAID STATUS OF HIST RECORD                    
*              R4 ASSUMED TO CONTAIN IO ADDRESS                                 
CHKIFPD  NTR1                                                                   
         MVI   ELCODE,TAINELQ     INVOICE STATUS ELMNT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'               MUST BE PRESENT                               
         USING TAIND,R4                                                         
         TM    TAINSTAT,TAINSPAY  MARKED AS PAID?                               
         BO    YES                                                              
         B     NO                                                               
         SPACE 3                                                                
*              ROUTINE FORMATS CLIENT AND PRODUCT CODES TO SINCLPR              
         SPACE                                                                  
DISPCLPR NTR1                                                                   
         MVC   SINCLPR(L'TGCLI),TGCLI  FIRST MOVE CLIENT                        
         OC    TGPRD,TGPRD             IF THERE'S A PRODUCT                     
         BZ    DISCPX                                                           
         LA    R1,SINCLPR+L'TGCLI-1    FIND END OF CLIENT                       
         SPACE                                                                  
DISCP5   CLI   0(R1),X'40'                                                      
         BH    DISCP10                                                          
         BCT   R1,DISCP5            KEEP BACKING UP TILL FIND NON-SPACE         
         SPACE                                                                  
DISCP10  MVI   1(R1),C'/'           MOVE A '/' IN BETWEEN CLIENT                
         MVC   2(L'TGPRD,R1),TGPRD  AND PRODUCT                                 
         SPACE                                                                  
DISCPX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY OPTIONS FIELD                                 
*              R4=A(TAPD ELEMENT)                                               
         SPACE                                                                  
         USING OPTD,R3                                                          
         USING TAPDD,R4                                                         
DISPOPTS NTR1                                                                   
         LA    R3,OPTTAB           R3=A(ENTRY IN OPTTAB)                        
         LA    R5,BLOCK            R5=A(CURRENT BLOCK ENTRY)                    
         XR    R0,R0               R0=NUMBER OF ENTRIES IN BLOCK                
         XR    R1,R1                                                            
         SPACE                                                                  
DISOPT5  CLI   0(R3),X'FF'         CHECK FOR END OF OPTTAB                      
         BE    DISOPT20                                                         
         ZIC   RE,OPTSTAT          DISPL. OF STATUS BYTE + A(TAPD EL.)          
         AR    RE,R4               = RE = A(STATUS BYTE)                        
         IC    R1,OPTBIT                                                        
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(RE),0             TEST FOR BIT MASK                            
         BO    DISOPT10            BRANCH IF MATCH                              
         LA    R3,OPTNEXT          BUMP TO NEXT OPTTAB ENTRY                    
         B     DISOPT5                                                          
         SPACE                                                                  
DISOPT10 LH    RF,OPTDIS           RF=DISP. TO DISPLAY ROUTINE                  
         AR    RF,RB                                                            
         LA    RE,DISOPT15         SET RE FOR COMMON NTR1                       
         MVI   EDITSTAT,0                                                       
DISOPT14 NTR1                                                                   
         BR    RF                  ** OFF TO DISPLAY ROUTINE **                 
         SPACE                                                                  
DISOPT15 AHI   R0,1                INCREMENT COUNT OF ENTRIES IN BLOCK          
         LA    R5,20(R5)           BUMP TO NEXT ENTRY IN BLOCK                  
         LA    R3,OPTNEXT          BUMP TO NEXT ENTRY IN OPTTAB                 
         B     DISOPT5                                                          
         SPACE                                                                  
DISOPT20 CLI   TAPDLEN,TAPDLNQ                                                  
         JL    DISOPT30                                                         
         SPACE                                                                  
         TM    TAPDOPT5,TAPDORG2                                                
         JZ    DISOPT25                                                         
         MVC   0(10,R5),=CL10'RG2' PROCESS IN SECOND REGRESSION RUN             
         MVC   10(10,R5),=CL10'Y'                                               
         LA    R5,20(R5)                                                        
         AHI   R0,1                                                             
         SPACE                                                                  
DISOPT25 TM    TAPDOPT5,TAPDONHW                                                
         JZ    DISOPT30                                                         
         MVC   0(10,R5),=CL10'NHW' PROCESS IN SECOND REGRESSION RUN             
         MVC   10(10,R5),=CL10'Y'                                               
         LA    R5,20(R5)                                                        
         AHI   R0,1                                                             
         DROP  R4                                                               
         SPACE                                                                  
DISOPT30 XC    SVFEE,SVFEE                                                      
         SPACE 1                                                                
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATFEE))                                     
         BNE   DISOPT40                                                         
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BRAS  RE,CVTR1EUR                                                      
         BRAS  RE,CVTR1CN                                                       
         ST    R1,SVFEE                                                         
         MVC   0(10,R5),=CL10'FEE'                                              
         NI    EDITSTAT,X'FF'-NODEC                                             
         BRAS  RE,EDITBLK                                                       
         LA    R5,20(R5)                                                        
         AHI   R0,1                                                             
         DROP  R4                                                               
         SPACE                                                                  
         USING TAFND,R4                                                         
DISOPT40 MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTASC))                                     
         BNE   DISOPTX                                                          
         L     R4,TGELEM                                                        
         ICM   R1,15,TAFNNAME                                                   
         MVC   0(10,R5),=CL10'ASC'                                              
         MVC   10(6,R5),TAFNNAME                                                
         MVC   16(4,R5),SPACES                                                  
         LA    R5,20(R5)                                                        
         AHI   R0,1                                                             
         DROP  R4                                                               
         SPACE                                                                  
DISOPTX  LTR   R0,R0               TEST IF ANY OPTIONS TO DISPLAY               
         BZ    XIT                                                              
         SPACE                                                                  
         MVI   TEMPOPTH,68                                                      
         GOTO1 UNSCAN,DMCB,((R0),BLOCK),TEMPOPTH                                
         OI    SINOPTH+6,X'80'                                                  
         CLI   TEMPOPTH+28,C' '                                                 
         BNE   DISOPTXX                                                         
         MVC   SINOPT,TEMPOPTH+8                                                
         B     XIT                                                              
         SPACE                                                                  
DISOPTXX MVI   SINOPT,C'*'         IF NOT ENOUGH ROOM TO DISPLAY                
         OI    SINOPTH+6,X'80'                                                  
         OI    SINPF16H+1,X'08'    ALL OPTIONS, DISPLAY *                       
         NI    SINPF16H+1,X'FB'    AND GIVE PFKEY TO OPTIONS/DISPLAY            
         OI    SINPF16H+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              OPTION DISPLAY ROUTINES, FILLS IN BLOCK FOR UNSCAN               
         SPACE 2                                                                
DISAPH   DS    0H                  APPLY CREDITS-CODE FROM HOLDING FEE          
         MVC   0(10,R5),=CL10'A'                                                
         MVC   10(10,R5),=CL10'H'                                               
         B     XIT                                                              
         SPACE 2                                                                
DISNAC   DS    0H                  DON'T APPLY CREDITS                          
         MVC   0(10,R5),=CL10'A'                                                
         MVC   10(10,R5),=CL10'N'                                               
         B     XIT                                                              
         SPACE 2                                                                
DISNGC   DS    0H                  DON'T TAKE GUARANTEE CREDITS                 
         MVC   0(10,R5),=CL10'G'                                                
         MVC   10(10,R5),=CL10'N'                                               
         B     XIT                                                              
         SPACE 2                                                                
DISCAN   DS    0H                  TAKE CANADIAN TAXES                          
         MVC   0(10,R5),=CL10'C'                                                
         MVC   10(10,R5),=CL10'Y'                                               
         B     XIT                                                              
         SPACE 2                                                                
DISURG   DS    0H                  URGENT PAYMENT - DUE TODAY                   
         MVC   0(10,R5),=CL10'U'                                                
         MVC   10(10,R5),=CL10'Y'                                               
         B     XIT                                                              
         SPACE 2                                                                
DISNOI   DS    0H                  NO INTERFACE                                 
         MVC   0(10,R5),=CL10'NI'                                               
         MVC   10(10,R5),=CL10'Y'                                               
         B     XIT                                                              
         SPACE 2                                                                
DISGRE   DS    0H                  G=E                                          
         MVC   0(10,R5),=CL10'G'                                                
         MVC   10(10,R5),=CL10'E'                                               
         B     XIT                                                              
         SPACE 2                                                                
DISDCL   DS    0H                  DUE COMPANY RECOVER FROM CLIENT ONLY         
         MVC   0(10,R5),=CL10'R'                                                
         MVC   10(10,R5),=CL10'C'                                               
         B     XIT                                                              
         SPACE 2                                                                
DISDAY   DS    0H                  DUE COMPANY RECOVER FROM AGENCY ONLY         
         MVC   0(10,R5),=CL10'R'                                                
         MVC   10(10,R5),=CL10'A'                                               
         B     XIT                                                              
         EJECT                                                                  
*              OPTION DISPLAY ROUTINES, CONTINUED                               
         SPACE 2                                                                
         USING TAPAD,R4                                                         
DISPHR   DS    0H                  P=P&H RATE IN PERCENT                        
         MVI   ELCODE,TAPAELQ      GET PAYMENT OPTIONS ELEMENT                  
         GOTO1 GETL,DMCB,(1,=AL1(TAPATPHR))                                     
         BNE   DISPHR5                                                          
         L     R4,TGELEM                                                        
         XR    R1,R1                                                            
         ICM   R1,3,TAPADATA                                                    
         B     DISPHR10                                                         
         SPACE                                                                  
         USING TAPDD,R4                                                         
DISPHR5  MVC   ELCODE,HTAPDELQ     GET PAYMENT DETAILS ELEMENT                  
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   10(10,R5),=CL10'?'  SET P= TO ?                                  
         OC    TAPDSPNH,TAPDSPNH                                                
         BZ    DISPHRX             IF SUBJECT TO P&H = 0                        
         MVC   10(10,R5),=CL10'0'                                               
         OC    TAPDPNH,TAPDPNH                                                  
         BZ    DISPHRX                                                          
         SR    R0,R0                                                            
         L     R1,TAPDPNH          P&H AMOUNT                                   
         M     R0,=F'100000'       X 100000                                     
         D     R0,TAPDSPNH         / SUBJECT TO P&H                             
         XR    R0,R0                                                            
         D     R0,=F'5'            ROUND UP IF NECESSARY                        
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
DISPHR10 BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
DISPHRX  MVC   0(10,R5),=CL10'P'                                                
         B     XIT                                                              
         EJECT                                                                  
*              OPTION DISPLAY ROUTINES, CONTINUED                               
         SPACE 2                                                                
DISTAX   DS    0H                  T=TAX AMOUNT OVERRIDE                        
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ      USE PAY OPTIONS ELEMENT IF AROUND            
         GOTO1 GETL,DMCB,(1,=AL1(TAPATTAX))                                     
         BNE   DIST2                                                            
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BRAS  RE,CVTR1EUR                                                      
         BRAS  RE,CVTR1CN                                                       
         B     DIST5                                                            
         SPACE                                                                  
         USING TABDD,R4                                                         
DIST2    MVC   ELCODE,HTABDELQ     ELSE GET BILLING DETAILS ELEMENT             
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         L     R1,TABDTAX                                                       
DIST5    MVC   0(10,R5),=CL10'T'                                                
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         SPACE 2                                                                
DISHND   DS    0H                  HANDLING AMOUNT OVERRIDE                     
         USING TAPDD,R4                                                         
         MVI   BYTE,0                                                           
         TM    TAPDOPT3,TAPDOHNC   SET IF CORP HANDLING OVERRIDE                
         BZ    *+8                                                              
         MVI   BYTE,C'Y'                                                        
         SPACE                                                                  
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ      GET PAY OPTIONS ELEMENT                      
         GOTO1 GETL,DMCB,(1,=AL1(TAPATHND))                                     
         BNE   DISH2                                                            
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BRAS  RE,CVTR1EUR                                                      
         BRAS  RE,CVTR1CN                                                       
         B     DISH5                                                            
         SPACE                                                                  
DISH2    MVC   ELCODE,HTABDELQ     GET BILLING DETAILS ELEMENT                  
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         SPACE                                                                  
         USING TABDD,R4                                                         
         L     R1,TABDHND          GET OVERRIDE AMT FROM INDIV HANDLING         
         CLI   BYTE,C'Y'           IF CORP HANDLING OVERRIDE                    
         BNE   *+8                                                              
         L     R1,TABDHNDC         THEN GET AMT FROM CORP HANDLING              
DISH5    MVC   0(10,R5),=CL10'H'                                                
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         EJECT                                                                  
*              OPTION DISPLAY ROUTINES, CONTINUED                               
         SPACE 2                                                                
DISDAL   DS    0H                  DUE COMPANY RECOVER FROM ALL                 
         MVC   0(10,R5),=CL10'R'                                                
         MVC   10(10,R5),=CL10'L'                                               
         B     XIT                                                              
         SPACE 2                                                                
DISFCR   DS    0H                  F=FICA CREDITS OVERRIDE                      
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ      USE PAY OPTIONS ELEMENT IF AROUND            
         GOTO1 GETL,DMCB,(1,=AL1(TAPATFIC))                                     
         BNE   DISF2                                                            
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BRAS  RE,CVTR1EUR                                                      
         BRAS  RE,CVTR1CN                                                       
         B     DISF5                                                            
         SPACE                                                                  
         USING TABDD,R4                                                         
DISF2    MVC   ELCODE,HTABDELQ     GET BILLING DETAILS ELEMENT                  
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         L     R1,TABDFICR                                                      
DISF5    MVC   0(10,R5),=CL10'F'                                                
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         SPACE 2                                                                
         USING TAPDD,R4                                                         
DISPTU   DS    0H                  TU=PREVIOUS TOTAL USES PAID OVERRIDE         
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   0(10,R5),=CL10'TU'                                               
         LH    R1,TAPDSTUS         STARTING USE NUMBER                          
         BCTR  R1,0                -1 = PREVIOUS TOTAL USES PAID                
         SPACE                                                                  
         OI    EDITSTAT,NODEC      SET NO DECIMALS                              
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         SPACE 2                                                                
DISPLU   DS    0H                  LU=PREVIOUS LIFT USES PAID OVERRIDE          
         MVC   0(10,R5),=CL10'LU'                                               
         LH    R1,LIFTSTUS         STARTING USE NUMBER FOR LIFT                 
         BCTR  R1,0                -1 = PREVIOUS LIFT USES PAID                 
         SPACE                                                                  
         OI    EDITSTAT,NODEC      SET NO DECIMALS                              
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         EJECT                                                                  
*              OPTION DISPLAY ROUTINES, CONTINUED                               
         SPACE 2                                                                
         USING TAPDD,R4                                                         
DISNUS   DS    0H                  US=NUMBER OF USES PAID OVERRIDE              
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   0(10,R5),=CL10'US'                                               
         LH    R1,TAPDUSES                                                      
         OI    EDITSTAT,NODEC      SET NO DECIMALS                              
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         SPACE 2                                                                
         USING TAPAD,R4                                                         
DISHNW   DS    0H                  HW=+- H&W ADJUSTMENT AMOUNT                  
         MVI   ELCODE,TAPAELQ      GET PAYMENT OPTIONS ELEMENT                  
         GOTO1 GETL,DMCB,(1,=AL1(TAPATHNW))                                     
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         MVC   0(10,R5),=CL10'HW'                                               
         ICM   R1,15,TAPADATA                                                   
         BRAS  RE,CVTR1EUR                                                      
         BRAS  RE,CVTR1CN                                                       
         OI    EDITSTAT,FLOAT      SET FLOATING SIGN                            
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         SPACE 2                                                                
         USING TABDD,R4                                                         
DISCCR   DS    0H                  RA=CANADIAN CONVERSION RATE                  
         MVC   ELCODE,HTABDELQ     GET BILLING DETAILS ELEMENT                  
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   0(10,R5),=CL10'RA'                                               
         ZAP   DUB,TABDCCVT        CONVERT FORMAT TO BINARY                     
         CVB   R1,DUB                                                           
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         SPACE 2                                                                
DISDUM   DS    0H                  DUMMY PAYMENT                                
         MVC   0(10,R5),=CL10'D'                                                
         MVC   10(10,R5),=CL10'Y'                                               
         B     XIT                                                              
         EJECT                                                                  
*              OPTION DISPLAY ROUTINES, CONTINUED                               
         SPACE 2                                                                
DISRET   DS    0H                  RETROACTIVE PAYMENT                          
         MVC   0(10,R5),=CL10'R'                                                
         MVC   10(10,R5),=CL10'Y'  DEFAULT TO Y IF NO RET4INV SAVED             
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANURT4I))                                     
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         GOTO1 TINVCON,DMCB,TANUMBER,10(R5),DATCON                              
         B     XIT                                                              
         SPACE 2                                                                
DISCOD   DS    0H                  COD INVOICE                                  
         MVC   0(10,R5),=CL10'PUR'                                              
         MVC   10(10,R5),=CL10'Y'                                               
         B     XIT                                                              
         SPACE 2                                                                
DISCSF   DS    0H                  CONTRACT SERVICE FEE                         
         MVC   0(10,R5),=CL10'CSF'                                              
         MVC   10(10,R5),=CL10'Y'  DEFAULT TO Y IF NO TAPAD ELEMENT             
         SPACE 1                                                                
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATCSF))                                     
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA      ELSE, USE PAY OPTIONS AMT OVERRIDE           
         BRAS  RE,CVTR1EUR                                                      
         BRAS  RE,CVTR1CN                                                       
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         SPACE 2                                                                
DISEUR   DS    0H                  EURO-BASED PAYMENT                           
         MVC   0(10,R5),=CL10'EUR'                                              
         MVC   10(10,R5),=CL10'Y'                                               
         B     XIT                                                              
         SPACE 2                                                                
DISGST   DS    0H                  GST=GST AMOUNT OVERRIDE                      
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ      USE PAY OPTIONS ELEMENT IF AROUND            
         GOTO1 GETL,DMCB,(1,=AL1(TAPATGST))                                     
         BNE   DISG2                                                            
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BRAS  RE,CVTR1EUR                                                      
         BRAS  RE,CVTR1CN                                                       
         B     DISG5                                                            
         SPACE                                                                  
         USING TABDD,R4                                                         
DISG2    MVC   ELCODE,HTABDELQ     GET BILLING DETAILS ELEMENT                  
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         L     R1,TABDGST                                                       
DISG5    MVC   0(10,R5),=CL10'GST'                                              
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         SPACE 2                                                                
DISAPS   DS    0H                  APPLY CREDITS-CODE FROM SESSION              
         MVC   0(10,R5),=CL10'A'                                                
         MVC   10(10,R5),=CL10'S'                                               
         B     XIT                                                              
         EJECT                                                                  
*              OPTION DISPLAY ROUTINES, CONTINUED                               
         SPACE 2                                                                
DISCRR   DS    0H                  NEW CABLE RATE                               
         MVI   ELCODE,TAPAELQ      GET PAYMENT OPTIONS ELEMENT                  
         GOTO1 GETL,DMCB,(1,=AL1(TAPATSOC))                                     
         BNE   DISCRR5             IF ELEM FOUND                                
         USING TAPAD,R4                                                         
         L     R4,TGELEM                                                        
         XR    R1,R1                                                            
         ICM   R1,3,TAPADATA       GET SAVED RATE                               
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     DISCRRX             AND EXIT                                     
         SPACE 1                   ELSE, MUST BE OLDER RECORD                   
DISCRR5  MVC   10(10,R5),=CL10'Y'   SO SHOW RR=Y                                
DISCRRX  MVC   0(10,R5),=CL10'RR'                                               
         B     XIT                                                              
         EJECT                                                                  
*              OPTION DISPLAY ROUTINES, CONTINUED                               
         SPACE 2                                                                
DISHDA   DS    0H                  CH=CHLOE HANNDLING AMOUNT OVERRIDE           
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ      GET PAY OPTIONS ELEMENT                      
         GOTO1 GETL,DMCB,(1,=AL1(TAPATCHA))                                     
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         BRAS  RE,CVTR1EUR                                                      
         BRAS  RE,CVTR1CN                                                       
                                                                                
         MVC   0(10,R5),=CL10'CH'                                               
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         EJECT                                                                  
*              OPTION DISPLAY ROUTINES, CONTINUED                               
         SPACE 2                                                                
DISHDP   DS    0H                  CH=CHLOE HANNDLING RATE OVERRIDE             
         USING TAPAD,R4                                                         
         MVI   ELCODE,TAPAELQ      GET PAY OPTIONS ELEMENT                      
         GOTO1 GETL,DMCB,(1,=AL1(TAPATCHO))                                     
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         ICM   R1,15,TAPADATA                                                   
         OC    SINEINV,SINEINV     IF CREDIT PAYMENT                            
         BZ    *+6                                                              
         LCR   R1,R1               CHANGE SIGN, EDITBLK DOES THIS               
                                                                                
         BRAS  RE,CVTR1EUR                                                      
         BRAS  RE,CVTR1CN                                                       
                                                                                
         MVC   0(10,R5),=CL10'CP'                                               
         BRAS  RE,EDITBLK          EDIT OUT TO BLOCK                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS AMOUNTS FROM 80 ELEMENT TO SCREEN               
*              AND KEEPS TRACK OF INVOICE TOTAL                                 
         SPACE                                                                  
         USING TAPDD,R4                                                         
DISPA80  NTR1                                                                   
         L     R3,TAPDGRS          GROSS                                        
         LA    R5,SINSCAL                                                       
         BAS   RE,EDITAMTZ         EDIT TO SCREEN, PRINT 0 ALSO                 
         L     R3,TAPDPNH          P&H                                          
         LA    R5,SINPNH                                                        
         BAS   RE,EDITAMT          EDIT TO SCREEN                               
         L     R3,TAPDAPPL         APPLIED CREDITS                              
         LA    R5,SINAPPL                                                       
         BAS   RE,EDITAMT          EDIT TO SCREEN                               
         L     R3,TAPDHNW          H&W                                          
         LA    R5,SINHNW                                                        
         BAS   RE,EDITAMT          EDIT TO SCREEN                               
         L     R3,TAPDGUAR         GUARANTEE CREDITS                            
         LA    R5,SINGUAR                                                       
         BAS   RE,EDITAMT          EDIT TO SCREEN                               
         L     R3,TAPDINR          I&R                                          
         LA    R5,SININR                                                        
         BAS   RE,EDITAMT          EDIT TO SCREEN                               
         L     R3,TAPDPAYI         INDIV. PAYMENT                               
         LA    R5,SINIPMT                                                       
         BAS   RE,EDITAMTZ         EDIT TO SCREEN, PRINT 0 ALSO                 
         L     R3,TAPDREXP         REIMBURSED EXPENSE                           
         LA    R5,SINREXP                                                       
         BAS   RE,EDITAMT          EDIT TO SCREEN                               
         L     R3,TAPDPAYC         CORP. PAYMENT                                
         LA    R5,SINCPMT                                                       
         BAS   RE,EDITAMTZ         EDIT TO SCREEN, PRINT 0 ALSO                 
         L     R3,TAPDSPNH         SUBJECT TO P&H                               
         LA    R5,SINSPNH                                                       
         BAS   RE,EDITAMT          EDIT TO SCREEN                               
         L     R3,TAPDMDED         MISC. DEDUCTIONS                             
         A     R3,TAPDDUES         + UNION DUES                                 
         LA    R5,SINMDED                                                       
         BAS   RE,EDITAMT          EDIT TO SCREEN                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS AMOUNTS FROM 84 ELEMENT TO SCREEN               
*              FOR A BILLED INVOICE                                             
         SPACE                                                                  
         USING TABDD,R4                                                         
DISPA84B NTR1                                                                   
         L     R3,TABDTAX          PAYROLL TAXES                                
         LA    R5,SINTAX                                                        
         BAS   RE,EDITAMT          EDIT TO SCREEN                               
         L     R3,TABDHND          HANDLING                                     
         LA    R5,SINHND                                                        
         BAS   RE,EDITAMT                                                       
         L     R3,TABDHNDC         CORPORATION HANDLING                         
         LA    R5,SINHNDC                                                       
         BAS   RE,EDITAMT                                                       
         L     R3,TABDFICR         FICA CREDIT AMOUNT                           
         LA    R5,SINFICR                                                       
         BAS   RE,EDITAMT                                                       
         CLI   TABDLEN,TABDLN2Q                                                 
         BL    D841                                                             
         L     R3,TABDACOM         AGENCY COMMISSION                            
         A     R3,TABDSIGN                                                      
         LA    R5,SINACOM                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
D841     L     R3,TABDGST          IF GST AMOUNT                                
         A     R3,TABDPST          ADD IN PST AMOUNT                            
         A     R3,TABDCSF          ADD IN CSF AMOUNT                            
         LTR   R3,R3                                                            
         BZ    D845                                                             
         LA    R5,SINGST                                                        
         BAS   RE,EDITAMT          DISPLAY CSF                                  
*                                                                               
D845     L     R3,TABDTOT          INVOICE TOTAL                                
         LA    R5,SINITOT                                                       
         BAS   RE,EDITAMTZ         EDIT TO SCREEN, PRINT 0 ALSO                 
         SPACE                                                                  
         TM    DISSTAT,DISCAND     IF DISPLAYING IN CAN$                        
         BZ    D846                                                             
         TM    PDSTAT,TAPDSCAN     AND US$ INVOICE                              
         BO    D84X                                                             
         EDIT  TABDCCVT,(12,SINCCVT),2,ZERO=NOBLANK,TRAIL=C'%'                  
         B     D84X                                                             
D846     TM    PDSTAT,TAPDSCAN     DISPLAY CONV RATE IF CAN$ PAYMENT            
         BO    D847                                                             
         TM    PDPST2,TAPDPEUR     OR EURO PAYMENT                              
         BZ    D84X                                                             
D847     OC    TABDCCVT,TABDCCVT   AND WE HAVE PACKED NUMBER                    
         BZ    D84X                                                             
         EDIT  TABDCCVT,(12,SINCCVT),2,ZERO=NOBLANK,TRAIL=C'%'                  
D84X     B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE DISPLAYS AMOUNTS FROM 84 ELEMENT TO SCREEN               
*              FOR AN INVOICE THAT'S NOT BILLED YET                             
         SPACE                                                                  
         USING TABDD,R4                                                         
DISPA84  NTR1                                                                   
         B     XIT                 DON'T DISPLAY IF NOT BILLED 3/27/92          
**********************                                                          
         TM    PDOPT1,TAPDOTAX     IF TAX AMOUNT OVERRIDE                       
         BZ    DISPA841                                                         
         L     R3,TABDTAX          PAYROLL TAXES                                
         LA    R5,SINTAX                                                        
         BAS   RE,EDITAMTZ         EDIT TO SCREEN, PRINT 0 ALSO                 
DISPA841 TM    PDOPT3,TAPDOHNC     IF CORP HANDLING OVERRIDE                    
         BZ    DISPA842                                                         
         L     R3,TABDHNDC         CORP HANDLING                                
         LA    R5,SINHNDC                                                       
         BAS   RE,EDITAMTZ         EDIT TO SCREEN, PRINT 0 ALSO                 
         B     DISPA844                                                         
DISPA842 TM    PDOPT1,TAPDOHND     IF HANDLING OVERRIDE                         
         BZ    DISPA844                                                         
         L     R3,TABDHND          HANDLING                                     
         LA    R5,SINHND                                                        
         BAS   RE,EDITAMTZ         EDIT TO SCREEN, PRINT 0 ALSO                 
DISPA844 TM    PDOPT2,TAPDOFCR     IF FICA CREDITS OVERRIDE                     
         BZ    DISPA84X                                                         
         L     R3,TABDFICR         FICA CREDIT AMOUNT                           
         LA    R5,SINFICR                                                       
         BAS   RE,EDITAMTZ         EDIT TO SCREEN, PRINT 0 ALSO                 
DISPA84X B     XIT                                                              
         EJECT                                                                  
*              ROUTINE EDITS AMOUNT AT R3 TO ADDRESS AT R5                      
         SPACE                                                                  
EDITAMT  DS    0H                                                               
         LTR   R3,R3               IF AMOUNT NOT 0                              
         BZR   RE                                                               
EDITAMTZ EDIT  (R3),(12,0(R5)),2,MINUS=YES                                      
         BR    RE                                                               
         SPACE 3                                                                
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE                                                                  
CLOSLOCK MVI   ERROR,ERCLSLCK      JOB IS CLOSED/LOCKED                         
         B     THEEND                                                           
         SPACE                                                                  
DFTJOB   MVC   MYMSGNO,=Y(ERDFTJOB)                                             
         B     ERREND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
DATEINV  MVI   ERROR,INVDATE       INVALID DATE                                 
         B     THEEND                                                           
         SPACE                                                                  
INVAMT   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     THEEND                                                           
         SPACE                                                                  
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
         SPACE                                                                  
NOTPD    MVI   ERROR,ERNOTPD       INVOICE NOT PAID                             
         B     THEEND                                                           
         SPACE                                                                  
ACTERR   MVI   ERROR,INVRCACT      RECORD/ACTION COMBINATION INVALID            
         LA    R2,CONACTH                                                       
         B     THEEND                                                           
         SPACE                                                                  
NOCANERR LHI   RE,ERAGNOCN         AGY/CLIENT DOES NOT HAVE CAN STAT            
         STH   RE,MYMSGNO                                                       
         B     ERREND                                                           
         SPACE                                                                  
INVVIEW  LHI   RE,ERCANINV         ALT VIEW INVALID FOR THIS INVOICE            
         STH   RE,MYMSGNO                                                       
         B     ERREND                                                           
         SPACE                                                                  
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
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
         LTORG                                                                  
         EJECT                                                                  
PFTAB    DS    0C                  TABLE TO COVER PF KEYS                       
         SPACE                                                                  
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'CHECK   ',CL8'LIST   '                               
PF13     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYTWA,L'SINAGY-1),AL2(SINAGY-T702FFD)                     
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYTWA,L'SININV-1),AL2(SININV-T702FFD)                     
         SPACE                                                                  
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'CLA     ',CL8'DISPLAY'                               
PF14     DC    AL1(KEYTYTWA,L'SINAGY-1),AL2(SINAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'SININV-1),AL2(SININV-T702FFD)                     
         SPACE                                                                  
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'COMMENT ',CL8'DISPLAY'                               
PF15     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'COMTYPC-1),AL2(COMTYPC-T702FFD)                   
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         SPACE                                                                  
PF15X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'OPTIONS ',CL8'DISPLAY'                               
PF16     DC    AL1(KEYTYTWA,L'SINAGY-1),AL2(SINAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'SININV-1),AL2(SININV-T702FFD)                     
         SPACE                                                                  
PF16X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF18X-*,18,0,(PF18X-PF18)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'BOVER   ',CL8'DISPLAY'                               
PF18     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'SININV-1),AL2(SININV-T702FFD)                     
         SPACE                                                                  
PF18X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
*              TABLE TO COVER OPTION FIELD ENTRIES                              
         SPACE 1                                                                
OPTTAB   DS    0H                                                               
         DC    AL1(TAPDOPT1-TAPDD,TAPDOAPH),AL2(DISAPH-T70227)  A=H             
         DC    AL1(TAPDOPT1-TAPDD,TAPDONAC),AL2(DISNAC-T70227)  A=N             
         DC    AL1(TAPDOPT1-TAPDD,TAPDONGC),AL2(DISNGC-T70227)  G=N             
         DC    AL1(TAPDOPT1-TAPDD,TAPDOCAN),AL2(DISCAN-T70227)  C=Y             
         DC    AL1(TAPDOPT1-TAPDD,TAPDOPHR),AL2(DISPHR-T70227)  P=RATE          
         DC    AL1(TAPDOPT1-TAPDD,TAPDOTAX),AL2(DISTAX-T70227)  T=AMNT          
         DC    AL1(TAPDOPT1-TAPDD,TAPDOHND),AL2(DISHND-T70227)  H=AMNT          
         DC    AL1(TAPDOPT2-TAPDD,TAPDOURG),AL2(DISURG-T70227)  U=Y             
         DC    AL1(TAPDOPT2-TAPDD,TAPDODCL),AL2(DISDCL-T70227)  R=C             
         DC    AL1(TAPDOPT2-TAPDD,TAPDODAY),AL2(DISDAY-T70227)  R=A             
         DC    AL1(TAPDOPT2-TAPDD,TAPDODAL),AL2(DISDAL-T70227)  R=L             
         DC    AL1(TAPDOPT2-TAPDD,TAPDOFCR),AL2(DISFCR-T70227)  F=AMNT          
         DC    AL1(TAPDOPT2-TAPDD,TAPDOAPS),AL2(DISAPS-T70227)  A=S             
         DC    AL1(TAPDOPT2-TAPDD,TAPDOPTU),AL2(DISPTU-T70227)  TU=#            
         DC    AL1(TAPDOPT2-TAPDD,TAPDOPLU),AL2(DISPLU-T70227)  LU=#            
         DC    AL1(TAPDOPT3-TAPDD,TAPDONUS),AL2(DISNUS-T70227)  US=#            
         DC    AL1(TAPDOPT3-TAPDD,TAPDOHNW),AL2(DISHNW-T70227) HW=+-AMT         
         DC    AL1(TAPDOPT3-TAPDD,TAPDOCCR),AL2(DISCCR-T70227)  RA=RATE         
         DC    AL1(TAPDOPT3-TAPDD,TAPDODUM),AL2(DISDUM-T70227)  D=Y             
         DC    AL1(TAPDOPT3-TAPDD,TAPDORET),AL2(DISRET-T70227)  R=Y             
         DC    AL1(TAPDOPT3-TAPDD,TAPDOCOD),AL2(DISCOD-T70227)  COD=Y           
         DC    AL1(TAPDOPT4-TAPDD,TAPDOCSF),AL2(DISCSF-T70227)  CSF=Y           
         DC    AL1(TAPDOPT1-TAPDD,TAPDOGST),AL2(DISGST-T70227)  GST=AMT         
         DC    AL1(TAPDOPT4-TAPDD,TAPDOSOC),AL2(DISCRR-T70227)  RR=Y            
         DC    AL1(TAPDOPT4-TAPDD,TAPDONOI),AL2(DISNOI-T70227)  NI=Y            
         DC    AL1(TAPDOPT4-TAPDD,TAPDGRTE),AL2(DISGRE-T70227)  NI=Y            
         DC    AL1(TAPDOPT4-TAPDD,TAPDOJPC),AL2(DISCSF-T70227)  CSF=Y           
         DC    AL1(TAPDOPT4-TAPDD,TAPDOEUR),AL2(DISEUR-T70227)  CSF=Y           
         DC    AL1(TAPDOPT4-TAPDD,TAPDOHDA),AL2(DISHDA-T70227)  CH=AMNT         
         DC    AL1(TAPDOPT4-TAPDD,TAPDOHDP),AL2(DISHDP-T70227)  CP=RATE         
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE GETS THE USE DETAILS AND DISPLAYS THEM                   
*              AT SINUSE+17 BECAUSE MAX LENGTH OF USE NAME IS 16.               
*              R4=A(80 ELEMENT)                                                 
         SPACE                                                                  
         USING TAPDD,R4                                                         
SETDETS  NTR1  BASE=*,LABEL=*                                                   
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(L'TGUSNAME),TGUSNAME  USE NAME AND TYPE                   
         SPACE                                                                  
         OC    TAPDAREA(6),TAPDAREA   IF THERE'S PRINT AREA AND USE             
         BZ    SETD2                                                            
         MVC   MYWORK+17(3),TAPDAREA  DISPLAY IT                                
         MVC   MYWORK+21(3),TAPDPUSE                                            
         B     SETDX                                                            
SETD2    TM    TGUSTYST,USES                                                    
         BO    SETD10                       BRANCH IF USES REQUIRED             
         TM    TGUSTYST,MAJORS                                                  
         BO    SETD3                        BRANCH IF MAJORS REQUIRED           
         TM    TGUSTYST,UNITS                                                   
         BO    SETD5                        BRANCH IF UNITS REQUIRED            
         TM    TGUSTYST,INSERTS                                                 
         BO    SETD7                        BRANCH IF INSERTS REQUIRED          
         CLI   TGUSEQU,UTAG                                                     
         BE    SETD8                        BRANCH IF TAG PAYMENT               
         CLI   TGUSEQU,UDEM                                                     
         BE    SETD9                        BRANCH IF DEMO PAYMENT              
         CLI   TGUSEQU,USNA                                                     
         BE    SETD9                        BRANCH IF SPANISH DEM PAY           
         CLI   TGUSEQU,UCDM                                                     
         BE    SETD9                        BRANCH IF CANADIAN DEM PAY          
         CLI   TGUSEQU,UITN                                                     
         BE    SETD9A                       BRANCH IF ITN PAYMENT               
         B     SETDXX                       ELSE DONE                           
         SPACE                                                                  
SETD3    GOTO1 MAJVAL,DMCB,(X'80',TAPDMAJ) VALIDATE MAJORS                      
         BNE   *+10                                                             
         MVC   MYWORK+17(L'TGMACHAR),TGMACHAR                                   
SETD5    OC    TAPDUNIT,TAPDUNIT   IF THERE ARE UNITS                           
         BZ    SETDX                                                            
         LA    RF,MYWORK+17+L'TGMACHAR-1 FIND END OF MAJORS                     
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         EDIT  TAPDUNIT,(5,2(RF)),ALIGN=LEFT NUMBER OF UNITS                    
         B     SETDX                                                            
         SPACE                                                                  
SETD7    OC    TAPDINS,TAPDINS     IF THERE ARE INSERTS                         
         BZ    SETDX                                                            
         LA    RF,MYWORK+17                                                     
         EDIT  TAPDINS,(5,2(RF)),ALIGN=LEFT NUMBER OF INSERTS                   
         B     SETDX                                                            
         SPACE                                                                  
SETD8    OC    TAPDTAGS,TAPDTAGS   IF THERE ARE TAGS                            
         BZ    SETDX                                                            
         LA    RF,MYWORK+17                                                     
         EDIT  TAPDTAGS,(3,2(RF)),ALIGN=LEFT NUMBER OF TAGS                     
         B     SETDX                                                            
         SPACE                                                                  
SETD9    OC    TAPDDEMS,TAPDDEMS   IF THERE ARE DEMOS                           
         BZ    SETDX                                                            
         LA    RF,MYWORK+17                                                     
         EDIT  TAPDDEMS,(3,2(RF)),ALIGN=LEFT NUMBER OF DEMOS                    
         B     SETDX                                                            
         SPACE                                                                  
         USING TAUHD,R4                                                         
SETD9A   L     R4,AIO              FOR ITN PAYMENT                              
         MVI   ELCODE,TAUHELQ      IF USAGE HISTORY ELEMENT                     
         BRAS  RE,GETEL            IS NOT PRESENT                               
         BE    SETD9B              MUST BE AUTO PAYMENT                         
         MVC   MYWORK+20(4),=C'AUTO'                                            
         B     SETDX                                                            
         SPACE                                                                  
SETD9B   LA    R5,MYWORK+20         OTHERWISE PRINT OUT USE #'S                 
         MVC   0(10,R5),SPACES                                                  
         EDIT  TAUHIFUS,(3,(R5)),ZERO=NOBLANK,ALIGN=LEFT                        
         CLI   0(R5),C' '                                                       
         BE    *+12                                                             
         LA    R5,1(R5)                                                         
         B     *-12                                                             
         MVI   0(R5),C'-'                                                       
         LH    RE,TAUHIFUS                                                      
         AH    RE,TAUHINUS                                                      
         SHI   RE,1                                                             
         EDIT  (RE),(3,1(R5)),ZERO=NOBLANK,ALIGN=LEFT                           
         B     SETDX                                                            
         SPACE                                                                  
         USING TANDD,R4                                                         
SETD10   MVI   ELCODE,TANDELQ      GET NETWORK/CLASS A DETAILS ELEMENT          
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   SETDXX                                                           
         SR    R2,R2                                                            
         ICM   R2,3,TANDSTUS       FIND TOTAL START USE NUMBER                  
         BNZ   *+8                                                              
         AHI   R2,1                + 1 IF 0 FROM CONVERSION                     
         SPACE                                                                  
         SR    R1,R1                                                            
         ICM   R1,3,TANDSTUL                                                    
         BNZ   *+8                                                              
         AHI   R1,1                                                             
         STH   R1,LIFTSTUS         SAVE STARTING USE NUMBER FOR LIFT            
         SPACE                                                                  
         AR    R2,R1                                                            
         BCTR  R2,0                R2=TOTAL START USE NUMBER                    
         LA    R1,MYWORK+17                                                     
         EDIT  (R2),(5,(R1)),ALIGN=LEFT                                         
         LA    RF,MYWORK+17+4      FIND END OF START USE NUMBER                 
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         AH    R2,TANDUSES         FIND TOTAL END USE NUMBER                    
         AH    R2,TANDUSEL                                                      
         BCTR  R2,0                                                             
         EDIT  (R2),(5,2(RF)),ALIGN=LEFT                                        
         SPACE                                                                  
         OC    TANDUSEL,TANDUSEL   IF THERE ARE LIFT USES                       
         BZ    SETDX                                                            
         MVI   MYWORK+27,C'L'      SHOW LIFT USES                               
         LA    R1,MYWORK+28                                                     
         EDIT  (2,TANDSTUL),(5,(R1)),ALIGN=LEFT  LIFT START USE NUMBER          
         LA    RF,MYWORK+28+4      FIND END OF LIFT START USE NUMBER            
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         LH    R2,TANDSTUL         FIND LIFT END USE NUMBER                     
         AH    R2,TANDUSEL                                                      
         BCTR  R2,0                                                             
         EDIT  (R2),(5,2(RF)),ALIGN=LEFT                                        
         SPACE                                                                  
SETDX    GOTO1 SQUASHER,DMCB,MYWORK,L'MYWORK  SQUASH IT                         
SETDXX   MVC   SINUSE,MYWORK       MOVE TO SCREEN                               
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS THE GLOBAL COMMERCIAL ID IN CASE HISTORY            
*              IS COPIED FROM ANOTHER COMMERCIAL OR CID CHANGED                 
*              FULL=INTERNAL COMMERCIAL NUMBER                                  
         SPACE                                                                  
SETCID   NTR1  BASE=*,LABEL=*                                                   
         OC    FULL,FULL           GET OUT IF NO CID (ADJUSTMENTS)              
         JZ    XIT                                                              
         MVC   AIO,AIO2            DON'T CREAM RECORD IN AIO                    
SETCID5  GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',FULL) GET COMM'L RECORD              
         BNE   SETCIDX                           IN CASE CID CHANGED            
         L     R4,AIO                                                           
         MVI   ELCODE,TAOCELQ      GET OLD AGENCY/CID ELEMENT                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
SETCID10 BRAS  RE,NEXTEL                                                        
         BNE   SETCID15                                                         
         USING TAOCD,R4                                                         
         CLI   TAOCLEN,TAOCSTAT+L'TAOCSTAT-TAOCD  TEST ELEMENT LENGTH           
         BL    SETCID10                                                         
         TM    TAOCSTAT,TAOCSTO    FIND "TO" ELEMENT                            
         BZ    SETCID10                                                         
         MVC   FULL,TAOCCOM                                                     
         B     SETCID5             GET COMMERCIAL RECORD                        
         SPACE                                                                  
         USING TLCOD,R4                                                         
SETCID15 L     R4,AIO                                                           
         MVC   TGAGY,TLCOAGY       SET GLOBAL AGENCY IN CASE CHANGED            
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   TGCID,TACOCID       SET GLOBAL CID                               
SETCIDX  MVC   AIO,AIO1            RESTORE AIO                                  
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE GETS BILLING RULES ELEMENT AT AIO AND                    
*              SETS THE INTERFACE FLAG                                          
         SPACE                                                                  
GETRULES NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TABRELQ      GET BILLING RULES ELEMENT                    
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   GRX                                                              
         USING TABRD,R4                                                         
         TM    TABRSTAT,TABRSINT   TEST ON PRODUCTION INTERFACE                 
         BZ    *+8                                                              
         MVI   INTER,C'Y'          SET LOCAL SWITCH                             
         SPACE                                                                  
         TM    TABRSTAT,TABRSNIN   TEST NOT ON PROD INTERFACE (CLIENTS)         
         BZ    *+8                                                              
         MVI   INTER,C'N'          SET LOCAL SWITCH                             
GRX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE DISPLAYS GRS/EPI FROM ELEMENT AT R4 TO SINEPIS           
*                                                                               
         USING TASBD,R4                                                         
         USING CABLED,R3                                                        
DISPGRSE NTR1  BASE=*,LABEL=*                                                   
         LA    R3,BLOCK                                                         
         XC    BLOCK(CABLNQ),BLOCK                                              
         MVC   CABLABLE,=C'GRS/EPI'     MOVE IN LABLE                           
         EDIT  TASBGRSE,(11,CABGRSE),2  EDIT GRS/EPI AMOUNT                     
         MVC   CABCOMMA,=C','           MOVE IN COMMA                           
         MVC   CABEPIS,SINEPIS          MOVE EPIS INPUT AFTER GRS/EPI           
         GOTO1 SQUASHER,DMCB,BLOCK,CABLNQ  SQUASH IT                            
         MVC   SINEPIS,BLOCK               MOVE TO SCREEN                       
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE EDITS AMOUNT AT R1 TO 10(10,R5)                          
*              CHOPS OFF INSIGNIFICANT ZEROS AT THE END                         
         SPACE                                                                  
EDITBLK  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,10(R5)           R4=A(EDITED AMOUNT)                          
         LTR   R1,R1               TEST FOR AMOUNT OF 0                         
         BNZ   *+14                                                             
         MVC   0(10,R4),=CL10'0'                                                
         B     EDITBX                                                           
                                                                                
         OC    SINEINV,SINEINV     IF CREDIT PAYMENT                            
         BZ    EDITB2                                                           
         LCR   R1,R1               COMPLEMENT IT TO SHOW WHAT WAS INPUT         
                                                                                
EDITB2   TM    EDITSTAT,FLOAT               IF WANT FLOATING SIGN               
         BZ    EDITB3                                                           
         EDIT  (R1),(9,1(R4)),2,ALIGN=LEFT  MAKE ROOM,EDIT W/2 DECIMALS         
         B     EDITB10                                                          
         SPACE                                                                  
EDITB3   TM    EDITSTAT,NODEC             TEST FOR NO DECIMAL PLACES            
         BZ    EDITB5                                                           
         EDIT  (R1),(10,(R4)),ALIGN=LEFT  EDIT W/O DECIMALS & NO MINUS          
         B     EDITBX                                                           
         SPACE                                                                  
EDITB5   EDIT  (R1),(10,(R4)),2,ALIGN=LEFT,FLOAT=-  FLOAT MINUS ONLY            
         SPACE                                                                  
EDITB10  CLI   0(R4),C'.'          FIND "."                                     
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         B     EDITB10                                                          
         SPACE                                                                  
         CLC   0(3,R4),=C'.00'     IF THERE'S A ".00"                           
         BNE   EDITB20                                                          
         MVC   0(3,R4),=3C' '      BLANK IT OUT                                 
         SPACE                                                                  
         TM    EDITSTAT,FLOAT                                                   
         BO    EDITB25                                                          
         B     EDITBX                                                           
         SPACE                                                                  
EDITB20  CLI   2(R4),C'0'          IF DECIMAL ENDS IN 0                         
         BNE   *+8                                                              
         MVI   2(R4),C' '          BLANK IT OUT                                 
         SPACE                                                                  
         TM    EDITSTAT,FLOAT      IF WANT FLOATING SIGN                        
         BZ    EDITBX                                                           
EDITB25  MVI   10(R5),C'+'         MOVE IN PLUS SIGN TO FRONT                   
         LTR   R1,R1               IF NEGATIVE AMOUNT                           
         BP    EDITBX                                                           
         MVI   10(R5),C'-'         MOVE IN MINUS SIGN                           
EDITBX   XIT                                                                    
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET UP COMMENT SCREEN IF PF15 HIT                     
         SPACE 1                                                                
PFCMNT   NTR1  BASE=*,LABEL=*                                                   
         CLI   PFAID,15                                                         
         BNE   PFCMNTX                                                          
         MVI   COMTYPC,TLCMTCOM    COMMENT RECORD TYPE C (PF15)                 
         MVI   SVRECUSE,HI         SAVE WHERE WE CAME FROM                      
PFCMNTX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO HIDE PFKEY TO COMMENT                                 
         SPACE 2                                                                
PF2COM   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGCTSTTY,TASTTYPP   IF THIS IS NOT A PROGRAMMER                  
         BE    P2CX                OR LEVEL 2, HIDE COMMENT PFKEY               
         CLI   TGCTSTTY,TASTTYP2                                                
         BE    P2CX                                                             
         OI    SINPFCMH+1,X'0C'    TURN COMMENT PFKEY TO LOW INTENSITY          
         OI    SINPFCMH+6,X'80'                                                 
P2CX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE DISPLAY FIELD                                
         SPACE 2                                                                
VALDISP  NTR1  BASE=*,LABEL=*                                                   
         MVI   HTAPDELQ,TAPDELQ                                                 
         MVI   HTABDELQ,TABDELQ                                                 
         MVI   DISSTAT,0                                                        
         SPACE 1                                                                
         CLI   SINDISPH+5,0                                                     
         BE    VOX                                                              
         SPACE 1                                                                
         LA    R2,SINDISPH         R2=A(OPTIONS FIELD)                          
         L     R4,AIO                                                           
         SPACE 1                                                                
         CLC   =C'EMS',8(R2)       IF INPUT IS EMS                              
         BE    VDISP10                                                          
         CLC   =C'CHL',8(R2)       OR CHL                                       
         BNE   VDISP20                                                          
VDISP10  MVI   ELCODE,TABDELQ2     EMS OR CHL DETAILS ELEMENT                   
         BRAS  RE,GETEL            MUST EXIST                                   
         BNE   VOFLDINV                                                         
         MVI   HTABDELQ,TABDELQ2                                                
         B     VOX                                                              
         SPACE 1                                                                
VDISP20  ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   =C'EUROS'(0),8(R2)  IF INPUT IS EUROS ...                        
         BNE   VDISP50                                                          
         SPACE 1                                                                
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   VDISP30                                                          
         TM    TAPDPST2,TAPDPEUR   INVOICE MUST BE IN EUROS                     
         BO    VDISP30                                                          
         CLI   MODE,DISPKEY        UNLESS WE'RE IN DK MODE                      
         BNE   VOFLDINV            WHERE EUROS IS LEFT OVER                     
         B     VOX                 FROM PREVIOUS INVOICE                        
         SPACE 1                                                                
VDISP30  OI    DISSTAT,DISEUROS    SET TO DISPLAY IN EUROS                      
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAIND,R4                                                         
         L     R4,AIO              IF ALREADY BILLED ..                         
         MVI   ELCODE,TAINELQ      DISPLAY THE EURO TOTALS                      
         BRAS  RE,GETEL                                                         
         BNE   VOX                                                              
         TM    TAINSTAT,TAINSBIL                                                
         BO    VDISP40                                                          
         TM    TAINSTA2,TAINSHLP                                                
         BZ    VOX                                                              
VDISP40  MVI   HTAPDELQ,TAEUELQ                                                 
         MVI   HTABDELQ,TABDELQ3                                                
         B     VOX                                                              
                                                                                
VDISP50  ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   =C'CAN  '(0),8(R2)  IF INPUT IS CAN ...                          
         BNE   VOFLDINV                                                         
                                                                                
         TM    CLISTAT2,TACISTCA DOES CLIENT HAVE CAN STATUS?                   
         BO    VDISP60                                                          
         TM    AGYSTAT6,TAAYSTCA DOES AGENCY HAVE CAN STATUS?                   
         JZ    NOCANERR                                                         
                                                                                
         USING TAPDD,R4                                                         
VDISP60  L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   VOFLDINV                                                         
         TM    TAPDPST2,TAPDPEUR   INVOICE MUST NOT BE IN EUROS                 
         JO    INVVIEW                                                          
                                                                                
         MVI   HTABDELQ,TABDELQ4   LOOK FOR CANADIAN TABD                       
         TM    TAPDSTAT,TAPDSCAN   IF US$ INVOICE,                              
         BO    VDISP70                                                          
         MVI   HTAPDELQ,TAPDELQ4   LOOK FOR CANADIAN TAPD                       
         DROP  R4                                                               
                                                                                
         USING TABDD,R4                                                         
VDISP70  L     R4,AIO              GET TABDELQ4                                 
         MVC   ELCODE,HTABDELQ                                                  
         BRAS  RE,GETEL                                                         
         JNE   INVVIEW                                                          
         OC    TABDCCVT,TABDCCVT   DO WE HAVE CAN CONV RATE?                    
         BZ    INVVIEW                                                          
         OI    DISSTAT,DISCAND     SET TO DISPLAY IN CAN$                       
VOX      XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
VOFLDINV MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE GETS LENGTH OF CYCLE AND RETURNS STRING IN TGDUB         
*              RETURNS CC NOT EQUAL IF L'CYCLE=0                                
         SPACE                                                                  
SETLCYC  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSWKS,0                                                        
         JE    NO                                                               
         MVC   BYTE,TGUSWKS        SET L'CYCLE FROM USE TABLE                   
         SPACE 1                                                                
         CLI   BYTE,13             IF CYCLE DEFINED AS 13 WEEKS                 
         JNE   *+16                                                             
         TM    AGYSTAT,TAAYS13W    TEST THIS AGENCY USES 13 WEEK CYCLES         
         JO    *+8                                                              
         MVI   BYTE,X'80'+3        ELSE SET TO 3 MONTHS                         
         SPACE 1                                                                
         ZIC   RF,BYTE             ISOLATE NUMBER                               
         SLL   RF,26                                                            
         SRL   RF,26                                                            
         SPACE 1                                                                
         MVC   TGDUB,SPACES                                                     
         MVI   TGDUB,C'('          BUILD DISPLAY FORMAT                         
         SPACE 1                                                                
         CLI   TGUSEQU,UDOR                                                     
         JNE   *+14                                                             
         MVC   TGDUB+1(5),=C'26W1)'                                             
         J     YES                                                              
         SPACE 1                                                                
         EDIT  (RF),(3,TGDUB+1),ALIGN=LEFT                                      
         LR    R1,R0                                                            
         LA    R1,TGDUB+1(R1)      BUMP PAST NUMBER                             
         SPACE 1                                                                
         MVI   0(R1),C'M'          SET WEEKS OR MONTHS OR DAYS                  
         TM    BYTE,X'80'                                                       
         JO    SETLC5                                                           
         MVI   0(R1),C'D'                                                       
         TM    BYTE,X'40'                                                       
         JO    SETLC5                                                           
         MVI   0(R1),C'W'                                                       
         SPACE 1                                                                
SETLC5   MVI   1(R1),C')'          END WITH TRAILING PARENTHESIS                
         J     YES                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
CLEARSCR NTR1  BASE=*,LABEL=*                                                   
         TWAXC SINCIDH                                                          
         XC    SINLFT,SINLFT       CLEAR AND TRANSMIT PROTECTED FIELDS          
         OI    SINLFTH+6,X'80'                                                  
         XC    SINLID,SINLID                                                    
         OI    SINLIDH+6,X'80'                                                  
         XC    SINCLIN,SINCLIN                                                  
         OI    SINCLINH+6,X'80'                                                 
         XC    SINCOMN,SINCOMN                                                  
         OI    SINCOMNH+6,X'80'                                                 
         XC    SINMEDN,SINMEDN                                                  
         OI    SINMEDNH+6,X'80'                                                 
         XC    SINSEC,SINSEC                                                    
         OI    SINSECH+6,X'80'                                                  
         XC    SINCLPR,SINCLPR                                                  
         OI    SINCLPRH+6,X'80'                                                 
         XC    SINLCYC,SINLCYC                                                  
         OI    SINLCYCH+6,X'80'                                                 
         XC    SINUSE,SINUSE                                                    
         OI    SINUSEH+6,X'80'                                                  
         XC    SINMSG,SINMSG                                                    
         OI    SINMSGH+1,X'0C'     MAKE ZERO INTENSITY                          
         OI    SINMSGH+6,X'80'                                                  
         XC    SINUSCN,SINUSCN                                                  
         OI    SINUSCNH+1,X'0C'     MAKE ZERO INTENSITY                         
         OI    SINUSCNH+6,X'80'                                                 
         OI    SINADVH+1,X'0C'                                                  
         OI    SINADVH+6,X'80'                                                  
         NI    SINESTH+1,X'DF'     UNPROTECT ESTIMATE NUMBER                    
         OI    SINWEBHH+1,X'0C'                                                 
         OI    SINWEBHH+6,X'80'                                                 
         XC    SINWEBI,SINWEBI                                                  
         OI    SINWEBIH+6,X'80'                                                 
         SPACE                                                                  
         CLI   HTABDELQ,TABDELQ2                                                
         JNE   CSCR10                                                           
         MVC   SINDISP,=CL5'EMS'                                                
         CLI   TGOFF,C'O'          CHLOE PRODUCTIONS ONLY                       
         JE    CSCR00                                                           
         CLI   TGOFF,C'Q'                                                       
         JNE   *+10                                                             
CSCR00   MVC   SINDISP,=CL5'CHL'                                                
         MVI   SINDISPH+5,3                                                     
         SPACE 1                                                                
CSCR10   TM    DISSTAT,DISEUROS    IF USER REQUESTED TO DISPLAY                 
         JO    CSCR20              IN EUROS                                     
         TM    DISSTAT,DISCAND     IF USER REQUESTED TO DISPLAY                 
         JO    CSCR30              IN CAN$                                      
         SPACE 1                                                                
         L     R4,AIO                                                           
         CLI   0(R4),TLHCCDQ                                                    
         JE    XIT                                                              
         SPACE 1                                                                
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ      OR INVOICE WAS PAID IN EUROS                 
         BRAS  RE,GETEL                                                         
         JNE   CSCR20                                                           
         TM    TAPDPST2,TAPDPEUR                                                
         JZ    XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         TM    TAINSTAT,TAINSBIL   BUT HAS NOT BEEN BILLED                      
         JO    XIT                                                              
         TM    TAINSTA2,TAINSHLP   OR PUR PRINTED                               
         JO    XIT                                                              
         OI    DISSTAT,DISEUROS                                                 
         DROP  R4                                                               
         SPACE 1                                                                
CSCR20   MVC   SINDISP,=C'EUROS'   DISPLAY IN EUROS                             
         MVI   SINDISPH+5,4                                                     
         J     XIT                                                              
CSCR30   MVC   SINDISP,=C'CAN'     DISPLAY IN CAN$                              
         MVI   SINDISPH+5,3                                                     
         J     XIT                                                              
                                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*        SET EURO CONVERSION RATE                                               
*        ON ENTRY ... P1 = A(INVOICE RECORD)                                    
                                                                                
SETEURT  NTR1  BASE=*,LABEL=*                                                   
         XC    ECVTRATE,ECVTRATE                                                
                                                                                
         TM    DISSTAT,DISEUROS    IF USER REQUESTED TO DISPLAY                 
         JZ    XIT                 IN EUROS                                     
                                                                                
         L     R2,0(R1)            R2=A(INVOICE RECORD)                         
                                                                                
         LR    R4,R2                                                            
         MVI   ELCODE,TAEUELQ      EXIT IF WE DON'T HAVE EURO-BASED             
         BRAS  RE,GETEL            PAYMENT DETAILS ELEMENT                      
         JNE   XIT                                                              
                                                                                
         USING TABDD,R4                                                         
         LR    R4,R2               GET BILLING DETAILS ELEMENT                  
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         ZAP   DUB,TABDCCVT        AND SAVE EURO CONVERSION RATE                
         CVB   R1,DUB              IN BINARY                                    
         ST    R1,ECVTRATE                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*        CONVERT US$ AMOUNT IN R1 TO EUROS                                      
*        ON ENTRY ... R1 = AMOUNT                                               
                                                                                
CVTR1EUR NTR1  BASE=*,LABEL=*                                                   
         OC    ECVTRATE,ECVTRATE   IF EURO CONVERSION RATE                      
         JZ    CR1EX               HAS BEEN SAVED                               
                                                                                
         XR    R0,R0               DIVIDE US$ AMOUNT BY                         
         MHI   R1,10000            EURO CONVERSION RATE                         
         D     R0,ECVTRATE                                                      
                                                                                
         MHI   R0,10                                                            
         L     RE,ECVTRATE         ROUND THE RESULT                             
         AHI   RE,1                                                             
         SRA   RE,1                                                             
         CR    R0,RE                                                            
         BL    *+8                                                              
         AHI   R1,1                                                             
                                                                                
CR1EX    XIT1  REGS=(R1)           AND RETURN IT IN R1                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*        SET CANADIAN CONVERSION RATE                                           
*        ON ENTRY ... P1 = A(INVOICE RECORD)                                    
                                                                                
SETCNRT  NTR1  BASE=*,LABEL=*                                                   
         XC    CCVTRATE,CCVTRATE                                                
                                                                                
         TM    DISSTAT,DISCAND     IF USER REQUESTED TO DISPLAY                 
         JZ    XIT                 IN CAN$                                      
                                                                                
         L     R2,0(R1)            R2=A(INVOICE RECORD)                         
                                                                                
         USING TABDD,R4                                                         
         LR    R4,R2               GET BILLING DETAILS ELEMENT                  
         MVI   ELCODE,TABDELQ4                                                  
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         ZAP   DUB,TABDCCVT        CANADIAN CONVERSION RATE                     
         CVB   R1,DUB              CONVERT RATE TO BINARY                       
         ST    R1,CCVTRATE         AND SAVE CAN CONVERSION RATE                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*        CONVERT US$ AMOUNT IN R1 TO CAN$                                       
*        ON ENTRY ... R1 = AMOUNT                                               
                                                                                
CVTR1CN  NTR1  BASE=*,LABEL=*                                                   
         TM    DISSTAT,DISCAND     IF USER REQUESTED TO DISPLAY                 
         JZ    XIT                 IN CAN$                                      
                                                                                
         TM    PDSTAT,TAPDSCAN     US$ INVOICES ONLY                            
         JNZ   XIT                                                              
                                                                                
         OC    CCVTRATE,CCVTRATE   IF CAN CONVERSION RATE                       
         JZ    CR1CTX              HAS BEEN SAVED                               
                                                                                
         XR    R0,R0                                                            
         MHI   R1,10000            CAN CONVERSION RATE                          
         D     R0,CCVTRATE                                                      
                                                                                
         L     RE,CCVTRATE         ROUND THE RESULT                             
         AHI   RE,1                                                             
         SRA   RE,1                                                             
         CR    R0,RE                                                            
         BL    *+8                                                              
         AHI   R1,1                                                             
                                                                                
CR1CTX   XIT1  REGS=(R1)           AND RETURN IT IN R1                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
OPTD     DSECT                     DSECT TO COVER OPTTAB                        
OPTSTAT  DS    AL1                 DISPLACEMENT TO STATUS BYTE IN 80 EL         
OPTBIT   DS    AL1                 BIT MASK FOR OPTION                          
OPTDIS   DS    AL2                 DISPLACEMENT TO DISPLAY ROUTINE              
OPTNEXT  EQU   *                                                                
         SPACE 2                                                                
CABLED   DSECT                     DSECT TO COVER OPTTAB                        
CABLABLE DS    CL7                 GRS/EPI LABLE                                
         DS    CL1                                                              
CABGRSE  DS    CL11                GRS/EPI AMOUNT                               
CABCOMMA DS    CL1                 COMMA                                        
         DS    CL1                                                              
CABEPIS  DS    CL(L'SINEPIS)       EPISODE INPUT                                
CABLNQ   EQU   *-CABLED                                                         
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR27D                                                       
         EJECT                                                                  
         ORG   SINWORK                                                          
MYWORK   DS    CL64                                                             
SVTGSSN  DS    CL9                 SAVED GLOBAL SSN                             
SVKEY    DS    CL38                SAVED KEY WITH ADDRESS                       
SVAGY    DS    CL6                       AGENCY                                 
SVAGY2   DS    CL6                       AGENCY                                 
INV      DS    CL6                 INVOICE NUMBER WORK SPACE                    
CINV     DS    CL6                 COMPLEMENTED INVOICE NUMBER                  
AGYSTAT  DS    CL1                 AGENCY STATUS                                
AGYSTAT4 DS    CL1                 AGENCY STATUS                                
INTER    DS    CL1                 ON INTERFACE                                 
PRODCLI  DS    CL3                 PRODUCTION CLIENT                            
PRODPRD  DS    CL3                 PRODUCTION PRODUCT                           
PDOPT1   DS    XL1                 PAYMENT OPTIONS FROM TAPDEL                  
PDOPT2   DS    XL1                                                              
PDOPT3   DS    XL1                                                              
PDOPT4   DS    XL1                                                              
PDSTAT   DS    XL1                 STATUS FROM TAPDEL                           
PDPST2   DS    XL1                 SECOND STATUS FROM TAPDEL                    
INSTAT   DS    XL1                 STATUS FROM TAIN                             
HTAPDELQ DS    XL1                 PAYMENT DETAILS ELEMENT CODE TO USE          
HTABDELQ DS    XL1                 BILLING DETAILS ELEMENT CODE TO USE          
EDITSTAT DS    CL1                 STATUS FOR EDITING OPTIONS                   
NODEC    EQU   X'80'               NO DECIMAL PLACES                            
FLOAT    EQU   X'40'               FLOATING SIGNS (+ AND -)                     
LIFTSTUS DS    H                   STARTING USE NUMBER FOR LIFT ONLY            
ADISPA84 DS    A                   ADDRESS OF DISPLAY ROUTINE FOR 84 EL         
PTRS     DS    CL(L'TLDRREC*7+1)   SAVED ACTIVE AND 6 PASSIVE PTRS              
COMMID   DS    CL(L'TLCOICID)                                                   
COMTITLE DS    CL(L'SINCOMN)                                                    
COMTYPC  DS    C                   COMMENT RECORD TYPE C (PF18)                 
USCANINV DS    XL6                 US/CANADIAN LINKED INVOICE                   
SVFEE    DS    F                   SAVED FEE AMOUNT                             
TEMPOPTH DS    XL76                TEMPORARY OPTIONS FIELD                      
DISSTAT  DS    X                   DISPLAY STATUS                               
DISEUROS EQU   X'80'               DISPLAY EUROS (Y/N)                          
DISCAND  EQU   X'40'               DISPLAY CAN$ (Y/N)                           
ECVTRATE DS    F                   EURO CONVERSION RATE                         
AGYSTAT6 DS    CL1                 AGENCY STATUS                                
CLISTAT2 DS    CL1                 CLIENT STATUS                                
CCVTRATE DS    F                   CANADIAN CONVERSION RATE                     
SVBDACOM DS    XL(L'TABDACOM)      SAVED AGENCY COMMISSION                      
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TAGENEQUS                                                                     
* TASYSDSECT                                                                    
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
         ORG   TWAHOLE                                                          
SVRECUSE DS    CL1               SAVED REC/USE                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'092TAGEN27   08/20/15'                                      
         END                                                                    
