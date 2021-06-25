*          DATA SET TAGEN4D    AT LEVEL 028 AS OF 05/01/02                      
*PHASE T7024DA,*                                                                
         TITLE 'T7024D - CONTROL RECORD LIST'                                   
T7024D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7024D                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=A(TWAHOLE)                                
         USING WORKD,R7                                                         
         SPACE 3                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   *+16                                                             
         LA    R2,LISTAR                                                        
         BAS   RE,LREC                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         XC    KEY,KEY             INSURE START REPORT FROM BEG OF LIST         
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P+1                                                           
         BAS   RE,LREC                                                          
         B     XIT                                                              
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY                                          
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,SCTAGYH          R2=A(AGENCY)                                 
         SPACE 1                                                                
         TM    SCRSTAT,SCRCHG      IF FIRST TIME IN, GET GLOBAL AGENCY          
         BO    VK10                                                             
         TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BO    VK20                                                             
         NI    SCTSTRH+4,X'DF'                                                  
         XC    TIFAGY,TIFAGY                                                    
         CLI   5(R2),0             NOT REQUIRED                                 
         BE    VK20                                                             
VK10     GOTO1 RECVAL,DMCB,TLAYCDQ,(X'04',(R2))                                 
         MVC   TIFAGY,TGAGY        SET AGENCY                                   
VK20     OI    4(R2),X'20'         SET FIELD VALIDATED                          
         SPACE 1                                                                
         TM    SCTSTRH+4,X'20'     PREVIOUSLY VALIDATED                         
         BO    VK30                                                             
         NI    SCTMTHH+4,X'DF'                                                  
         XC    TIQSTART,TIQSTART                                                
         CLI   SCTSTRH+5,0         ONLY ALLOW START IF AGENCY PRESENT           
         BE    VK30                                                             
         CLI   5(R2),0             R2=A(AGENCY)                                 
         BE    FLDMISS                                                          
         MVC   TIQSTART(L'SCTSTR),SCTSTR  AND START CLIENT                      
         OC    TIQSTART(L'SCTSTR),SPACES                                        
         SPACE 1                                                                
VK30     OI    SCTSTRH+4,X'20'     SET FIELD VALIDATED                          
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'40',SCTMTHH),SCTSUPPH  IF NOTHING'S CHGD          
         BE    VKX                                   GET OUT                    
         SPACE 1                                                                
         BAS   RE,VALFILTS         VALIDATE FILTERS                             
         SPACE 1                                                                
         BAS   RE,INIT             INITIALIZE FOR SYSIO                         
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES SELECTION FILTERS                              
         SPACE 1                                                                
VALFILTS NTR1                                                                   
         LA    R2,SCTMTHH          R2=A(FIRST FILTER FIELD)                     
         LA    R3,FILTTAB          R3=A(VALIDATION ROUTINE TABLE)               
         USING FILTD,R3                                                         
         MVC   FILTERS(FILTLNQ),SPACES                                          
         SPACE 1                                                                
VFLT2    CLI   0(R3),X'FF'                                                      
         BE    XIT                                                              
         CLI   5(R2),0             ANYTHING TO VALIDATE                         
         BE    VFLT8                                                            
         MVI   BYTE,C' '           INITIALIZE 'ALL EXCEPT' INDICATOR            
         LA    R4,8(R2)            SET R4=A(DATA)                               
         ZIC   R1,5(R2)                R1=L'DATA                                
         SPACE 1                                                                
         CLI   0(R2),8+2+8         IF L'FIELD GREATER THAN ONE                  
         BL    VFLT6                                                            
         CLI   8(R2),C'*'          TEST FOR 'ALL EXCEPT'                        
         BNE   VFLT6                                                            
         CLI   5(R2),1             INSURE THERE'S ADDITIONAL INPUT              
         BE    FLDINV                                                           
         MVI   BYTE,C'*'           SET INDICATOR                                
         LA    R4,1(R4)            BUMP DATA POINTER                            
         BCTR  R1,0                AND DECREMENT L'DATA                         
         SPACE 1                                                                
VFLT6    LH    RF,FILTVAL          RF=DISP. TO VALIDATION ROUTINE               
         AR    RF,RB                                                            
         BASR  RE,RF               VALIDATE INPUT                               
         SPACE 1                                                                
         ZIC   R1,FILTWS           R1=DISP. TO FILTER IN W/S                    
         LA    R1,FILTERS(R1)                                                   
         MVC   0(1,R1),BYTE        SAVE 'ALL EXCEPT' FLAG                       
         MVC   1(1,R1),0(R4)       SAVE DATA IN W/S                             
         LA    RE,FILTRATE                                                      
         CR    RE,R1               IF THIS IS COMM RATE                         
         BNE   *+10                                                             
         MVC   1(2,R1),0(R4)       NEED TO SAVE TWO BYTES                       
         SPACE 1                                                                
VFLT8    OI    4(R2),X'20'         SET VALIDATED BIT                            
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         IC    R1,0(R2)            BUMP PAST TAG                                
         AR    R2,R1                                                            
         LA    R3,FILTNEXT         AND TO NEXT TABLE ENTRY                      
         B     VFLT2                                                            
         EJECT                                                                  
*              FILTER FIELD VALIDATION ROUTINES                                 
         SPACE 1                                                                
*                                  R2=A(FIELD HEADER)                           
*                                  R4=A(DATA IN FIELD)                          
*                                  R1=L'DATA                                    
         SPACE 1                                                                
VALCHAR  DS    0H                  RF=A(LIST OF VALID CHOICES)                  
         CLI   BYTE,C'*'           IF USER DIDN'T SPECIFY ALL EXCEPT            
         BE    *+12                                                             
         CLI   5(R2),1             L'INPUT SHOULD BE ONE                        
         BH    FLDINV                                                           
         SPACE 1                                                                
VCHR2    CLC   0(1,R4),0(RF)                                                    
         BER   RE                                                               
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '          END OF LIST MARKER                           
         BH    VCHR2                                                            
         B     FLDINV                                                           
         SPACE 2                                                                
VALYORN  DS    0H                                                               
         LA    RF,=C'YN '                                                       
         B     VALCHAR                                                          
         SPACE 2                                                                
VALCOMM  DS    0H                                                               
         LA    RF,=AL1(TAEPCYES,TAEPCPAY,TAEPCTNH,TAEPCNO,TAEPCCLI,TAEP*        
               CHND,0)                                                          
         B     VALCHAR                                                          
         SPACE 2                                                                
VALFILT  DS    0H                                                               
         LA    RF,=AL1(TAEPFCYC,TAEPFBIL,TAEPFCES,TAEPFBES,0)                   
         B     VALCHAR                                                          
         SPACE 2                                                                
VALEST   DS    0H                                                               
         LA    RF,=C'YNP '                                                      
         B     VALCHAR                                                          
         SPACE 2                                                                
VALFMT   DS    0H                                                               
         LA    RF,=AL1(TAEPFCOM,TAEPFUSE,TAEPFLB,TAEPFQTR,TAEPFCAT,TAEP*        
               FV1,TAEPFA,TAEPFB,TAEPFDWN,TAEPFNOW,TAEPFW,0)                    
         B     VALCHAR                                                          
         SPACE 2                                                                
VALAUTO  DS    0H                                                               
         LA    RF,=C'YMX123456 '                                                
         B     VALCHAR                                                          
         SPACE 2                                                                
VALHORZ  DS    0H                                                               
         LA    RF,=C'NYC '                                                      
         B     VALCHAR                                                          
         SPACE 2                                                                
VALACTL  DS    0H                                                               
         LA    RF,=C'NYXPOUZ '                                                  
         B     VALCHAR                                                          
         SPACE 2                                                                
VALRCAP  DS    0H                                                               
         LA    RF,=AL1(NO,TAEPRQTR,TAEPRMTH,TAEPRVQT,TAEPRUSE,TAEPRWC,T*        
               AEPRXCM,TAEPRTTL,TAEPR1,TAEPRCO,TAEPRCOG,TAEPRPRJ,TAEPR2*        
               ,0)                                                              
         B     VALCHAR                                                          
         EJECT                                                                  
*              FILTER FIELD VALIDATION ROUTINES (CONT'D)                        
         SPACE 1                                                                
*                                  R2=A(FIELD HEADER)                           
*                                  R4=A(DATA IN FIELD)                          
*                                  R1=L'DATA                                    
         SPACE 1                                                                
VALMTH   DS    0H                                                               
         MVC   WORK(3),=3X'F0'     INSURE VALID NUMERIC FIELD                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),0(R4)                                                    
         CLC   WORK(3),=3X'F0'                                                  
         BNE   FLDINV                                                           
         XC    DUB,DUB                                                          
         EX    R1,*+8              CVT TO PACKED                                
         B     *+10                                                             
         PACK  DUB+6(2),0(0,R4)                                                 
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    FLDINV                                                           
         CH    R1,=H'12'           INSURE VALID MONTH                           
         BH    FLDINV                                                           
         STC   R1,WORK             SAVE BINARY VALUE                            
         LA    R4,WORK             IN WORK                                      
         BR    RE                                                               
         SPACE 3                                                                
VALRATE  NTR1                                                                   
         LR    RF,R1               VALIDATE COMMISSION RATE                     
         GOTO1 CASHVAL,DMCB,(R4),(RF)                                           
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         TM    4(R1),X'80'                                                      
         BO    FLDINV                                                           
         MVC   WORK(2),6(R1)       RETURN VALUE IN WORK                         
         LA    R4,WORK                                                          
         XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*              INITIALIZE FOR SYSIO                                             
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    KEY,KEY             INITIALIZE KEY                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM SCREEN           
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLCTCDQ      SET FOR READS                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL LISTING RECORDS                               
         SPACE 1                                                                
*                                  R2=A(SCREEN LINE OR P)                       
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   PASS SOME MORE THINGS                        
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,14           MAKE GENCON THINK WE HAVE 14 LINES           
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  PASS CONTROL TO SYSIO FOR LIST            
         SPACE 1                                                                
         MVI   NLISTS,13           WE REALLY HAVE 13                            
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF WE'RE SPOOLING                            
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       AND WE REPORTED SOMETHING                    
         BE    LRX                                                              
         BAS   RE,PRNTIT           SKIP A LINE ...                              
         SPACE 1                                                                
         EDIT  COUNTER,(8,P+1),COMMAS=YES,ALIGN=LEFT  AND PRINT TOTALS          
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(17,R1),=C'CONTROL RECORDS'                                     
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING NOW                              
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS SYSIO HOOKS                                   
         SPACE 1                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BE    DISPLAY                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY A RECORD                                      
         SPACE 1                                                                
         USING LINED,R2            R2=A(DISPLAY LINE)                           
DISPLAY  DS    0H                                                               
         MVC   LINDATA,SPACES      INITIALIZE                                   
         MVC   LINAGY,TIAGY        AGENCY                                       
         MVC   LINCLI,TICLI        CLIENT                                       
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAEPELQ      GET ESTIMATE PROFILE ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   DIS20                                                            
         USING TAEPD,R4            R4=A(ELEMENT)                                
         SPACE 1                                                                
         LA    R3,FILTTAB          R3=A(FILTER TABLE)                           
         USING FILTD,R3                                                         
DIS2     ZIC   R1,FILTWS           R1=DISP. TO FIELD IN W/S                     
         LA    R1,FILTERS(R1)                                                   
         ZIC   RF,FILTEL           RF=DISP. TO FIELD IN EL.                     
         AR    RF,R4                                                            
         LA    RE,FILTRATE         RE=A(RATE FILTER FIELD)                      
         SPACE 1                                                                
         CLI   1(R1),C' '          TEST THERE'S A FILTER                        
         BE    DIS6                                                             
         CLI   0(R1),C'*'          IF 'ALL EXCEPT' REQUESTED                    
         BNE   DIS4                                                             
         CR    RE,R1               IF THIS IS COMM RATE                         
         BNE   DIS3                                                             
         CLC   1(2,R1),0(RF)       MUST NOT MATCH FOR TWO BYTES                 
         BE    DISX                                                             
         B     DIS6                                                             
DIS3     CLC   1(1,R1),0(RF)       MUST NOT MATCH FILTER VALUE                  
         BE    DISX                                                             
         B     DIS6                                                             
         SPACE 1                                                                
DIS4     CLC   1(1,R1),0(RF)       ELSE MUST MATCH FILTER VALUE                 
         BNE   DISX                                                             
         CR    RE,R1               IF THIS IS COMM RATE                         
         BNE   *+14                                                             
         CLC   1(2,R1),0(RF)       MUST MATCH FOR TWO BYTES                     
         BNE   DISX                                                             
         SPACE 1                                                                
DIS6     ZIC   RE,FILTLINE         RE=DISP. TO FIELD IN DISPLAY LINE            
         AR    RE,R2                                                            
         MVC   0(1,RE),0(RF)       MOVE FIELD FROM ELEMENT TO DISPLAY           
         SPACE 1                                                                
         LA    R3,FILTNEXT         BUMP TO NEXT TABLE ENTRY                     
         CLI   0(R3),X'FF'                                                      
         BNE   DIS2                AND CONTINUE IF NOT END                      
         SPACE 1                                                                
         EDIT  (1,TAEPMTH),(2,LINMTH),ZERO=BLANK  NEED TO EDIT MONTH            
         EDIT  (2,TAEPRATE),(5,LINRATE),2,ZERO=BLANK       AND RATE             
         SPACE 1                                                                
DIS20    CLI   MODE,PRINTREP                                                    
         BNE   DIS30                                                            
         GOTO1 CATCHIOS            INSURE DON'T DO TOO MANY I/O'S               
         BAS   RE,PRNTIT           PRINT REPORT                                 
         AP    COUNTER,=P'1'                                                    
         B     DISX                                                             
         SPACE 1                                                                
DIS30    CLI   LISTNUM,13          IF THIS IS LAST RECORD TO DISPLAY            
         BE    DONE                HANDLE DISPLAY MESSAGE FOR GENCON            
         SPACE 1                                                                
         MVC   DMDSKADD,TIDSKADD   ELSE SET DISK ADDRESS                        
         GOTO1 LISTMON             AND MOVE DISPLAY LINE TO SCREEN              
DISX     B     XIT                 RETURN TO SYSIO                              
         EJECT                                                                  
*              ROUTINE TO SEND A LINE TO QUEUE                                  
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         MVC   HEAD4+9(6),SCTAGY                                                
         MVC   HEAD4+64(6),SCTSTR                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS/EXITS                                                     
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
         SPACE 1                                                                
DONE     MVC   MYMSGNO1,OKNO       SET TO GIVE GENCON'S MESSAGE NUMBER          
         MVI   MYMSYS,X'FF'                                                     
         LA    R2,SCTSELH          CURSOR TO FIRST SELECT FIELD                 
         B     INFEND                                                           
         SPACE 1                                                                
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 3                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
*                         FILTER TABLE (MUST MATCH ORDER ON SCREEN)             
FILTTAB  DS    0CL4                                                             
         DC    AL2(VALMTH-T7024D),AL1(FILTMTH-FILTERS)   MONTH                  
         DC    AL1(LINMTH-LINED,TAEPMTH-TAEPD)                                  
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTEXP-FILTERS)  EXPIRY                 
         DC    AL1(LINEXP-LINED,TAEPEXP-TAEPD)                                  
*                                                                               
         DC    AL2(VALFILT-T7024D),AL1(FILTFILT-FILTERS) ACTL FLT BASIS         
         DC    AL1(LINFILT-LINED,TAEPFILT-TAEPD)                                
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTMUS-FILTERS)  MUSIC                  
         DC    AL1(LINMUS-LINED,TAEPMUS-TAEPD)                                  
*                                                                               
         DC    AL2(VALCOMM-T7024D),AL1(FILTCOMM-FILTERS) COMMISSION             
         DC    AL1(LINCOMM-LINED,TAEPCOMM-TAEPD)                                
*                                                                               
         DC    AL2(VALRATE-T7024D),AL1(FILTRATE-FILTERS) COMM RATE              
         DC    AL1(LINRATE-LINED,TAEPRATE-TAEPD)                                
*                                                                               
         DC    AL2(VALEST-T7024D),AL1(FILTEST-FILTERS)  ESTIMATE                
         DC    AL1(LINEST-LINED,TAEPEST-TAEPD)                                  
*                                                                               
         DC    AL2(VALFMT-T7024D),AL1(FILTFMT-FILTERS)   FORMAT                 
         DC    AL1(LINFMT-LINED,TAEPFMT-TAEPD)                                  
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTCAST-FILTERS) CAST                   
         DC    AL1(LINCAST-LINED,TAEPCAST-TAEPD)                                
*                                                                               
         DC    AL2(VALAUTO-T7024D),AL1(FILTAUTO-FILTERS) AUTO                   
         DC    AL1(LINAUTO-LINED,TAEPAUTO-TAEPD)                                
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTPBSS-FILTERS) PBSS                   
         DC    AL1(LINPBSS-LINED,TAEPPBSS-TAEPD)                                
*                                                                               
         DC    AL2(VALHORZ-T7024D),AL1(FILTHORZ-FILTERS) HORZ                   
         DC    AL1(LINHORZ-LINED,TAEPHORZ-TAEPD)                                
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTCOML-FILTERS) COML                   
         DC    AL1(LINCOML-LINED,TAEPCOML-TAEPD)                                
*                                                                               
         DC    AL2(VALRCAP-T7024D),AL1(FILTRCAP-FILTERS) RCAP                   
         DC    AL1(LINRCAP-LINED,TAEPRCAP-TAEPD)                                
*                                                                               
         DC    AL2(VALACTL-T7024D),AL1(FILTACTL-FILTERS) ACTL                   
         DC    AL1(LINACTL-LINED,TAEPACTL-TAEPD)                                
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTLEFT-FILTERS) LEFT                   
         DC    AL1(LINLEFT-LINED,TAEPLEFT-TAEPD)                                
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTFREE-FILTERS) FREE MUS               
         DC    AL1(LINFREE-LINED,TAEPFREE-TAEPD)                                
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTPAGE-FILTERS) NEW PG/COMML           
         DC    AL1(LINPAGE-LINED,TAEPPAGE-TAEPD)                                
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTNOBX-FILTERS) SUPPRESS BOXES         
         DC    AL1(LINNOBX-LINED,TAEPNOBX-TAEPD)                                
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTDLR-FILTERS)  PAY HLD IN DLR         
         DC    AL1(LINDLR-LINED,TAEPDLR-TAEPD)                                  
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTSUPP-FILTERS) SUP INACTV COM         
         DC    AL1(LINSUPP-LINED,TAEPSUPP-TAEPD)                                
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTREL-FILTERS)  EST ON REL COM         
         DC    AL1(LINREL-LINED,TAEPREL-TAEPD)                                  
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTTNH-FILTERS)  TNH OVERRIDE           
         DC    AL1(LINTNH-LINED,TAEPTNH-TAEPD)                                  
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTHNW-FILTERS)  HNW WITH NET           
         DC    AL1(LINHNW-LINED,TAEPHNW-TAEPD)                                  
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTTNHC-FILTERS)  TNH COMBINED          
         DC    AL1(LINTNHC-LINED,TAEPTNHC-TAEPD)                                
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTSIG-FILTERS)   PRINT SIG LNS         
         DC    AL1(LINSIG-LINED,TAEPSIG-TAEPD)                                  
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTCLA-FILTERS)  MERGE CLA PYTS         
         DC    AL1(LINCLA-LINED,TAEPCLA-TAEPD)                                  
*                                                                               
         DC    AL2(VALYORN-T7024D),AL1(FILTDET-FILTERS)  PRINT EST REC          
         DC    AL1(LINDET-LINED,TAEPDET-TAEPD)                                  
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
NO       EQU   C'N'                                                             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,32,C'CONTROL RECORD LIST'                                     
         SSPEC H2,32,C'-------------------'                                     
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SSPEC H4,2,C'AGENCY'                                                   
         SSPEC H4,58,C'START'                                                   
         SPACE 1                                                                
         SSPEC H6,2,C'AGENCY CLIENT CODES'                                      
         SSPEC H7,2,C'------ ------ -----'                                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
WORKD    DSECT                                                                  
COUNTER  DS    PL4                 LINE COUNTER                                 
*                                                                               
FILTERS  DS    0C                  LIST FILTERS (*=ALL EXCEPT,DATA)             
FILTMTH  DS    CL1,CL1                                                          
FILTCOMM DS    CL1,CL1                                                          
FILTRATE DS    CL1,CL2                                                          
FILTEXP  DS    CL1,CL1                                                          
FILTFILT DS    CL1,CL1                                                          
FILTMUS  DS    CL1,CL1                                                          
FILTEST  DS    CL1,CL1                                                          
FILTFMT  DS    CL1,CL1                                                          
FILTCAST DS    CL1,CL1                                                          
FILTAUTO DS    CL1,CL1                                                          
FILTPBSS DS    CL1,CL1                                                          
FILTHORZ DS    CL1,CL1                                                          
FILTCOML DS    CL1,CL1                                                          
FILTRCAP DS    CL1,CL1                                                          
FILTACTL DS    CL1,CL1                                                          
FILTLEFT DS    CL1,CL1                                                          
FILTFREE DS    CL1,CL1                                                          
FILTPAGE DS    CL1,CL1                                                          
FILTNOBX DS    CL1,CL1                                                          
FILTDLR  DS    CL1,CL1                                                          
FILTSUPP DS    CL1,CL1                                                          
FILTREL  DS    CL1,CL1                                                          
FILTTNH  DS    CL1,CL1                                                          
FILTHNW  DS    CL1,CL1                                                          
FILTTNHC DS    CL1,CL1                                                          
FILTSIG  DS    CL1,CL1                                                          
FILTCLA  DS    CL1,CL1                                                          
FILTDET  DS    CL1,CL1                                                          
FILTLNQ  EQU   *-FILTERS                                                        
         EJECT                                                                  
*              DSECT TO COVER FILTER TABLE                                      
         SPACE 1                                                                
FILTD    DSECT                                                                  
FILTVAL  DS    H                   DISP. TO VALIDATION ROUTINE                  
FILTWS   DS    AL1                 DISP. TO FIELD IN W/S                        
FILTLINE DS    AL1                 DISP. TO FIELD IN DISPLAY LINE               
FILTEL   DS    AL1                 DISP. TO FIELD IN ELEMENT                    
FILTNEXT EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER DISPLAY LINE                                      
         SPACE 1                                                                
LINED    DSECT                                                                  
LINDATA  DS    0CL(L'SCTDATA)                                                   
LINAGY   DS    CL6                                                              
         DS    CL1                                                              
LINCLI   DS    CL6                                                              
         DS    CL1                                                              
LINMTH   DS    CL2                                                              
         DS    CL1                                                              
LINEXP   DS    CL1                                                              
         DS    CL1                                                              
LINFILT  DS    CL1                                                              
         DS    CL1                                                              
LINMUS   DS    CL1                                                              
         DS    CL1                                                              
LINCOMM  DS    CL1                                                              
         DS    CL1                                                              
LINRATE  DS    CL5                                                              
         DS    CL1                                                              
LINEST   DS    CL1                                                              
         DS    CL1                                                              
LINFMT   DS    CL1                                                              
         DS    CL1                                                              
LINCAST  DS    CL1                                                              
         DS    CL1                                                              
LINAUTO  DS    CL1                                                              
         DS    CL1                                                              
LINPBSS  DS    CL1                                                              
         DS    CL1                                                              
LINHORZ  DS    CL1                                                              
         DS    CL1                                                              
LINCOML  DS    CL1                                                              
         DS    CL1                                                              
LINRCAP  DS    CL1                                                              
         DS    CL1                                                              
LINACTL  DS    CL1                                                              
         DS    CL1                                                              
LINLEFT  DS    CL1                                                              
         DS    CL1                                                              
LINFREE  DS    CL1                                                              
         DS    CL1                                                              
LINPAGE  DS    CL1                                                              
         DS    CL1                                                              
LINNOBX  DS    CL1                                                              
         DS    CL1                                                              
LINDLR   DS    CL1                                                              
         DS    CL1                                                              
LINSUPP  DS    CL1                                                              
         DS    CL1                                                              
LINREL   DS    CL1                                                              
         DS    CL1                                                              
LINTNH   DS    CL1                                                              
         DS    CL1                                                              
LINHNW   DS    CL1                                                              
         DS    CL1                                                              
LINTNHC  DS    CL1                                                              
         DS    CL1                                                              
LINSIG   DS    CL1                                                              
         DS    CL1                                                              
LINCLA   DS    CL1                                                              
         DS    CL1                                                              
LINDET   DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR4DD                                                       
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA                         MUST FOLLOW LAST SCREEN                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028TAGEN4D   05/01/02'                                      
         END                                                                    
