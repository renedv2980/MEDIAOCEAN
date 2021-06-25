*          DATA SET RERMP09    AT LEVEL 042 AS OF 04/13/09                      
*PHASE T81009C,*                                                                
*INCLUDE CLPACK                                                                 
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - INIT'            
*******************************************************************             
*                                                                 *             
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT           *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 13APR09 (KUI) NEW INVENTORY KEY SUPPORT                         *             
*                                                                 *             
*                    ***  END TOMBSTONE  ***                      *             
*******************************************************************             
         SPACE 2                                                                
T81009   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1009**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO09                                                        
         L     R1,=V(CLPACK)                                                    
         A     R1,RELO09                                                        
         ST    R1,ACLPACK                                                       
         SPACE                                                                  
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
*                                                                               
         B     XIT                                                              
XIT      XIT1                                                                   
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - VREC'            
*******************************************************************             
*                                                                 *             
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT           *             
*                                                                 *             
*              VALIDATE RECORD ROUTINES                           *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
VREC     DS    0H                                                               
*                                                                               
         GOTO1 VALAGY              GET PARENT REP                               
*                                                                               
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - VSTA'            
*******************************************************************             
*                                                                 *             
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT           *             
*                                                                 *             
*              VALIDATE STATION                                   *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
VSTA     DS    0H                                                               
         LA    R2,TITSTATH         STATION                                      
*                                                                               
         GOTO1 =A(VALSTA),RR=RELO09                                             
*                                                                               
VSTAX    DS    0H                                                               
*                                                                               
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - VSVC'            
***********************************************************************         
*                                                                     *         
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT               *         
*                                                                     *         
*              VALIDATE RATING SERVICE                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VSVC     DS    0H                                                               
         LA    R2,TITSVCH          SERVICE                                      
         MVI   CSOURCE,C'N'        DEFAULT TO SOURCE 'N' FOR VALIDATION         
         CLI   5(R2),0                                                          
         BE    VR060                                                            
         MVC   CSOURCE,8(R2)       SAVE SOURCE                                  
****     CLI   8(R2),C'A'          ARB                                          
****     BE    VR060                                                            
         CLI   8(R2),C'N'          NSI                                          
         BE    VR060                                                            
         CLI   8(R2),C'M'          MFX                                          
         BE    VR060                                                            
         CLI   8(R2),C'S'          SRC                                          
         BNE   VR900                                                            
*                                                                               
VR060    DS    0H                                                               
*                                                                               
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - DPTVAL'          
***********************************************************************         
*                                                                     *         
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT               *         
*                                                                     *         
*              VALIDATE DAYPART                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTVAL   LA    R2,TITDPTH          VALIDATE DAYPART                             
*                                                                               
         XC    DPLIST,DPLIST       INIT DAYPART LIST                            
         XC    DPMENU,DPMENU       INIT DAYPART MENU CODE                       
*                                                                               
         CLI   5(R2),0             IF NOT ENTERED                               
         BE    DPTVALX                                                          
         CLC   8(3,R2),=C'ALL'     OR ALL                                       
         BNE   *+14                                                             
         MVC   DPMENU,=C'ALL '        USE MENU 'ALL '                           
         B     DPTMENU                                                          
*                                                                               
         CLC   =C'M=',8(R2)        MENU IF IT STARTS 'M='                       
         BNE   DPT05                                                            
*                                                                               
         MVC   DPMENU,10(R2)       SAVE MENU CODE                               
         OC    DPMENU,SPACES       SPACE FILL                                   
*                                                                               
         B     DPTMENU                                                          
*                                                                               
DPT05    DS    0H                                                               
*                                                                               
         ZIC   RF,5(R2)            NUMBER OF DAYPARTS                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),8(R2)     SAVE DAYPART LIST                            
*                                                                               
         B     DPTMENUX                                                         
*                                                                               
*        READ SET RECORD FOR DAYPART MENU                                       
*                                                                               
DPTMENU  DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH SET RECORD KEY                     
         USING RSETKEY,R4                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ   SET AS SET RECORD RECORD                     
         MVC   RSETKREP,AGENCY     SET REP CODE                                 
         MVC   RSETKSET,=C'DP'     SET SET CODE                                 
         MVC   RSETKID,DPMENU      SET SET IDENTIFIER                           
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTMENUE                                                         
*                                                                               
         GOTO1 GETREC              READ IN SET RECORD                           
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,RSETMCDQ     FIND MEMBERS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DPTMENUE            MUST FIND ELEMENT                            
*                                                                               
         USING RSETMEMD,R6         ESTABLISH MEMBERS ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETMTOV)     DECREMENT BY OVERHEAD LENGTH                 
         BNP   DPTMENUE            MUST HAVE SOME MEMBERS                       
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),RSETMEMB  COPY DAYPARTS                                
*                                                                               
DPTMENUX DS    0H                                                               
*                                                                               
*        VALIDATE INDIVIDUALLY ENTERED DAYPARTS                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH DAYPART RECORD KEY                 
         USING RRDPKEY,R4                                                       
*                                                                               
         MVI   RRDPKTYP,RRDPKIDQ   SET AS RESEARCH DAYPART RECORD               
         MVC   RRDPKREP,AGENCY     SET REP CODE                                 
*                                                                               
         LA    R5,DPLIST           START OF INPUT                               
*                                                                               
         LA    R0,L'DPLIST         MAX NUMBER OF DAYPARTS IN LIST               
*                                                                               
         LA    R3,DPTBL            ESTABLISH DAYPART TABLE                      
         USING DPTBLD,R3                                                        
         XC    DPTBLD(DPTBLL),DPTBLD   INIT FIRST ENTRY                         
*                                                                               
DPTLOOP  DS    0H                                                               
*                                                                               
         CLI   0(R5),C' '          DONE IF END OF LIST REACHED                  
         BNH   DPTDONE                                                          
*                                                                               
         MVC   RRDPKDPT,0(R5)      SET NEXT DAYPART IN KEY                      
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTINVE                                                          
*                                                                               
         CLI   ACTNUM,ACTREP       READ RECORD IF DOING REPORT                  
         BNE   DPTCONT                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,IO               POINT TO FOUND RECORD                        
         MVI   ELCODE,X'01'        SEARCH FOR DAYPART ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DPTCONT             IGNORE IF NOT FOUND                          
*                                                                               
         USING RRDPELEM,R6         ESTABLISH DAYPART ELEMENT                    
*                                                                               
         MVC   DPTBCODE,RRDPKDPT   SAVE DAYPART CODE                            
         MVC   DPTBSNAM,RRDPSNAM   SAVE SHORT NAME                              
         MVC   DPTBLNAM,RRDPLNAM   SAVE LONG NAME                               
*                                                                               
         LA    R3,DPTBLL(R3)       BUMP TO NEXT ENTRY IN DPTBL                  
         XC    DPTBLD(DPTBLL),DPTBLD  INIT NEXT ENTRY                           
*                                                                               
DPTCONT  DS    0H                                                               
*                                                                               
         LA    R5,1(R5)            BUMP TO NEXT ENTERED DAYPART                 
         BCT   R0,DPTLOOP                                                       
*                                                                               
DPTDONE  DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    DPTNUMX                                                          
*                                                                               
         TM    WHEN,X'20'          SKIP IF NOT SOON REPORT                      
         BNO   DPTNUMX                                                          
*                                                                               
         CLI   DPLIST+1,C' '       MAX ONE DPT FOR SOON REQUEST                 
         BH    DPTSOONE                                                         
*                                                                               
DPTNUMX  DS    0H                                                               
*                                                                               
         B     DPTVALX                                                          
*                                                                               
DPTMENUE DS    0H                  INVALID DAYPART MENU ID                      
*                                                                               
         MVC   RERROR,=AL2(DPTMNNF)                                             
         B     DPTERR                                                           
*                                                                               
DPTSOONE DS    0H                  MAX ONE DPT ON SOON REQUEST                  
*                                                                               
         MVC   RERROR,=AL2(MAXDPTSN)                                            
         B     DPTERR                                                           
*                                                                               
DPTINVE  DS    0H                                                               
*                                                                               
         MVC   RERROR,=AL2(INVDP)  INVALID DAYPART                              
*                                                                               
DPTERR   DS    0H                                                               
*                                                                               
         GOTO1 MYERROR                                                          
*                                                                               
DPTVALX  DS    0H                                                               
*                                                                               
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - VR160'           
***********************************************************************         
*                                                                     *         
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT               *         
*                                                                     *         
*              VALIDATE PRINT DETAILS OPTION                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR160    LA    R2,TITDETH                                                       
         CLI   5(R2),0                                                          
         BE    VR180                                                            
         CLI   8(R2),C'Y'                                                       
         BE    VR180                                                            
         CLI   8(R2),C'N'                                                       
         BNE   VR900                                                            
*                                                                               
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - VR180'           
***********************************************************************         
*                                                                     *         
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT               *         
*                                                                     *         
*              VALIDATE DELETE TYPE                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR180    DS    0H                                                               
*                                                                               
         MVI   DELBYTE,0           INIT DELETE SWITCHES                         
*                                                                               
         LA    R2,TITDELH                                                       
         GOTO1 ANY                                                              
         LA    R5,8(R2)                                                         
         LA    R6,2                                                             
*                                                                               
VR200    CLI   0(R5),C'T'                                                       
         BNE   *+12                                                             
         OI    DELBYTE,DELTXTQ     TEXT                                         
         B     VR220                                                            
*                                                                               
         CLI   0(R5),C'I'                                                       
         BNE   *+12                                                             
         OI    DELBYTE,DELINVQ     INV                                          
         B     VR220                                                            
*                                                                               
         CLI   0(R5),C'A'                                                       
         BNE   VR900                                                            
         OI    DELBYTE,DELALLQ     ALL                                          
*                                                                               
VR220    LA    R5,1(R5)                                                         
         CLI   5(R2),1                                                          
         BNH   *+8                                                              
         BCT   R6,VR200                                                         
         TM    DELBYTE,DELALLQ+DELTXTQ    DON'T ALLOW ALL AND TEXT              
         BO    VR900                                                            
         TM    DELBYTE,DELALLQ+DELINVQ    DON'T ALLOW ALL AND INV               
         BO    VR900                                                            
         TM    DELBYTE,DELALLQ     DON'T ALLOW ALL AND SERVICE OPTION           
         BZ    VR240                                                            
         LA    R2,TITSVCH                                                       
         CLI   5(R2),0                                                          
         BNE   VR900                                                            
*                                                                               
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - VR240'           
***********************************************************************         
*                                                                     *         
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT               *         
*                                                                     *         
*              VALIDATE TEXT TYPE                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR240    LA    R2,TITTYPH                                                       
         TM    DELBYTE,DELTXTQ                                                  
         BO    VR260                                                            
         CLI   5(R2),0                                                          
         BE    VR320                                                            
         B     VR900                                                            
*                                                                               
VR260    CLI   5(R2),0                                                          
         BE    VR320                                                            
         CLI   8(R2),C'A'          ALL TEXT                                     
         BE    VR300                                                            
         CLI   8(R2),C'I'          INVENTORY TEXT                               
         BE    VR300                                                            
         CLI   8(R2),C'M'          MARKET TEXT                                  
         BE    VR280                                                            
         CLI   8(R2),C'S'          STATION TEXT                                 
         BNE   VR900                                                            
*                                                                               
VR280    CLI   DELBYTE,DELTXTQ     DON'T ALLOW ONLY MKT/STN TEXT                
         BNE   VR300                       AND DAYPART                          
         LA    R2,TITDPTH                                                       
         CLI   5(R2),0                                                          
         BE    VR320                                                            
         MVC   RERROR,=AL2(TXTMS)                                               
         GOTO1 MYERROR                                                          
*                                                                               
VR300    DS    0H                                                               
         LA    R2,TITDPTH                                                       
         CLI   5(R2),0             MUST HAVE A DAYPART FOR ALL/INV TEXT         
         BNE   *+14                                                             
         MVC   RERROR,=AL2(TXTDP)                                               
         GOTO1 MYERROR                                                          
*                                                                               
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - VR320'           
***********************************************************************         
*                                                                     *         
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT               *         
*                                                                     *         
*              VALIDATE BOOKS OR TEXT/INVENTORY RANGE                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR320    LA    R2,TITBKRGH         BOOKS/RANGE                                  
*                                                                               
         TM    DELBYTE,DELTXTQ+DELINVQ                                          
         BNZ   VR340               ONLY ALLOW FOR TEXT OR INV DELETE            
*                                                                               
         CLI   5(R2),0                                                          
         BE    VR460                                                            
*                                                                               
         B     VRBKS1E                                                          
*                                                                               
VR340    GOTO1 ANY                 REQUIRED ENTRY FOR TXT OR INV DELETE         
*                                                                               
         MVC   DMCB+8(4),=C',=,-'  SET TO SEARCH FOR HYPHENS                    
         GOTO1 SCANNER,DMCB,(R2),(4,BLOCK)                                      
*                                                                               
         LA    R4,BLOCK                                                         
*                                                                               
         CLI   DMCB+4,1                                                         
         BH    VR360               MORE THAN 1 FIELD - TRY BOOKS                
*                                                                               
         BL    VR900               ENTRY IS INVALID                             
*                                                                               
         CLI   1(R4),0             1 FIELD - TEST LEN OF 2ND HALF               
         BNE   VR400                         NON ZERO - TRY RANGE               
*                                                                               
VR360    MVI   MAX,4               MAX 4 BOOKS                                  
*                                                                               
         GOTO1 VALIBOK                                                          
*                                                                               
*        SAVE TABLE OF BOOKS                                                    
*                                                                               
         LA    R4,DELBOOKS                                                      
         LA    R5,CBOOKS                                                        
         ZIC   R6,ACTUAL                                                        
*                                                                               
VR380    MVC   0(4,R4),0(R5)                                                    
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,VR380                                                         
*                                                                               
         MVI   0(R4),X'FF'         FLAG END OF TABLE                            
*                                                                               
         B     VR460                                                            
*                                                                               
VR400    TM    DELBYTE,DELTXTQ+DELINVQ DON'T ALLOW RANGE FOR TEXT AND           
         BO    VRRNG1E              INV DELETE TOGETHER                         
*                                                                               
         OI    DELBYTE,DELRNGQ     RANGE                                        
*                                                                               
         TM    DELBYTE,DELTXTQ                                                  
         BNO   VR420                                                            
*                                                                               
         OC    4(4,R4),4(R4)       TEXT NUMBERS IN RANGE MUST BE                
         BZ    VRRNGNNE              NUMERIC                                    
*                                                                               
         OC    8(4,R4),8(R4)                                                    
         BZ    VRRNGNNE                                                         
*                                                                               
         CLC   4(4,R4),8(R4)       AND START MUST BE BEFORE END                 
         BH    VRRNG2E                                                          
*                                                                               
         MVC   TXRANGE(2),6(R4)                                                 
         MVC   TXRANGE+2(2),10(R4)                                              
*                                                                               
         B     VR460                                                            
*                                  CHECK INV RANGE                              
VR420    LA    R0,2                                                             
         LR    R5,R4                                                            
         LA    R6,12(R4)                                                        
         LA    R7,INRANGE                                                       
         MVC   INRANGE(8),SPACES   INIT RANGE HOLDAREA                          
         SR    RF,RF                                                            
*                                                                               
VRINVLP  DS    0H                                                               
*                                                                               
         ICM   RF,1,0(R5)          LENGTH OF RANGE COMPONENT                    
         BZ    VRINVCN             NONE ENTERED                                 
*                                                                               
         CH    RF,=H'4'            MAX 4 POSITIONS                              
         BH    VR900                                                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R6)       SAVE RANGE COMPONENT                         
*                                                                               
VRINVCN  DS    0H                                                               
*                                                                               
         LA    R5,1(R4)                                                         
         LA    R6,22(R4)                                                        
         LA    R7,4(R7)                                                         
         BCT   R0,VRINVLP                                                       
*                                                                               
VRINVDN  DS    0H                                                               
*                                                                               
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - VR460'           
***********************************************************************         
*                                                                     *         
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT               *         
*                                                                     *         
*              VALIDATE START DATE FOR 'ALL' OPTION                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR460    LA    R2,TITSTRTH                                                      
         LA    R3,STDATE                                                        
         BAS   RE,CHKDATE          VALIDATE START DATE ENTRY                    
         BNE   VR900                                                            
*                                                                               
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - VRENDDT'         
***********************************************************************         
*                                                                     *         
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT               *         
*                                                                     *         
*              VALIDATE END   DATE FOR 'ALL' OPTION                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRENDDT  DS    0H                                                               
*                                                                               
         LA    R2,TITENDH          ALL END DATE                                 
         LA    R3,ENDATE                                                        
         BAS   RE,CHKDATE          VALIDATE DATE ENTRY                          
         BNE   VR900                                                            
*                                                                               
         TM    DELBYTE,DELALLQ     SKIP IF NOT DELETING 'ALL'                   
         BZ    VRECX                                                            
*                                                                               
*        AT LEAST ONE OF START OR END DATE REQUIRED FOR 'ALL'                   
*                                                                               
         LA    R2,TITSTRTH                                                      
*                                                                               
         OC    ENDATE,ENDATE                                                    
         BNZ   VR480                                                            
*                                                                               
         OC    STDATE,STDATE                                                    
         BZ    VR990                                                            
*                                                                               
         B     VRECX                                                            
*                                                                               
VR480    CLC   STDATE,ENDATE       CHECK START DATE LE END DATE                 
         BH    VRSTENDE                                                         
*                                                                               
         B     VRECX                                                            
*                                                                               
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - VRERR'           
***********************************************************************         
*                                                                     *         
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT               *         
*                                                                     *         
*              ERROR ROUTINES                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRSTENDE MVC   RERROR,=AL2(ERRSTEND) END DATE BEFORE START DATE.                
         B     ERREND                                                           
*                                                                               
VRBKS1E  MVC   RERROR,=AL2(ERRBKS1)  BOOKS NOT ALLOWED FOR 'ALL' DELETE         
         B     ERREND                                                           
*                                                                               
VRRNG1E  MVC   RERROR,=AL2(ERRRNG1)  RANGE NOT ALLOWED FOR INV AND ALL          
         B     ERREND                                                           
*                                                                               
VRRNG2E  MVC   RERROR,=AL2(ERRRNG2)  START OF RANGE MUST BE BEFORE END          
         B     ERREND                                                           
*                                                                               
VRRNGNNE MVC   RERROR,=AL2(ERRRNNM)  RANGE ENTRIES MUST BE NUMERIC              
         B     ERREND                                                           
*                                                                               
VR900    MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
VR990    MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
*                                                                               
ERRBKS1  EQU   641                 BOOKS NOT VALID WITH TYPE ALL                
ERRRNG1  EQU   642                RANGE INVALID IF TXT AND INV TOGETHER         
ERRRNNM  EQU   643                 RANGE ENTRIES MUST BE NUMERIC                
ERRRNG2  EQU   644                 RANGE START BEFORE END                       
ERRSTEND EQU   64                  END DATE BEFORE START DATE                   
*                                                                               
VRECX    DS    0H                                                               
*                                                                               
*        CHECK IF STATION LOCKED                                                
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    VRLCKX                                                           
*                                                                               
         TM    WHEN,X'20'          SKIP IF NOT SOON REPORT                      
         BNO   VRLCKX                                                           
*                                                                               
VRLOCK   DS    0H                                                               
*                                                                               
         MVI   TWAWHEN,5                SET FOR UPDATIVE SOON                   
*                                                                               
         LA    R4,WORK                                                          
         USING LKKEYD,R4                                                        
*                                                                               
         XC    WORK,WORK                                                        
         L     R6,ACOMFACS                                                      
*                                                                               
         L     RF,CGETFACT-COMFACSD(,R6)                                        
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         MVC   LOCKSE,FASYS-FACTSD(RE)                                          
*                                                                               
         MVC   LOCKAGY,AGENCY                                                   
         MVC   LOCKRTY,=CL2'RI'                                                 
         XC    LOCKKEY+5(5),LOCKKEY+5                                           
*                                                                               
*        FIND V(LOCKET)                                                         
*                                                                               
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QLOCKET                                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,DMCB             A(LOCKET)                                    
*                                                                               
         LA    R3,STLIST           POINT TO STATION LIST                        
         USING STLISTD,R3          ESTABLISH STATION LIST ENTRY                 
*                                                                               
VRLOCKLP DS    0H                                                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD DONE AT END OF LSIT                     
         BZ    VRLOCKDN                                                         
*                                                                               
         MVC   LOCKKEY(5),STLSSTAC SET CALL LETTERS                             
*                                                                               
         GOTO1 (R5),DMCB,('LKLOCKQ',LKKEYD),(R6),0                              
*                                                                               
         CLI   DMCB+4,0            ANY ERRORS                                   
         BNE   VRLCKMSG            LOCK STATION                                 
*                                                                               
VRLOCKCN DS    0H                                                               
         LA    R3,STLISTL(R3)      NEXT STATION IN LIST                         
         B     VRLOCKLP                                                         
*                                                                               
VRLOCKDN DS    0H                                                               
*                                                                               
         B     VRLCKX                                                           
*                                                                               
*        ERROR - STATION LOCKED ALREADY                                         
*                                                                               
VRLCKMSG DS    0H                                                               
*                                                                               
*        UNLOCK ALL STATIONS ALREADY LOCKED                                     
*                                                                               
         ST    R3,FULL             SAVE A(STATION IN ERROR)                     
*                                                                               
         LA    R3,STLIST           POINT TO STATION LIST                        
         USING STLISTD,R3          ESTABLISH STATION LIST ENTRY                 
*                                                                               
VRUNLKLP DS    0H                                                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD DONE AT END OF LSIT                     
         BZ    VRUNLKDN                                                         
*                                                                               
         C     R3,FULL             STOP IF STATION IN ERROR REACHED             
         BE    VRUNLKDN                                                         
*                                                                               
         MVC   CSTAT,STLSSTAC      SET CALL LETTERS                             
*                                                                               
         GOTO1 (R5),(R1),('LKUNLKQ',LKKEYD),(R6)                                
*                                                                               
VRUNLKCN DS    0H                                                               
         LA    R3,STLISTL(R3)      NEXT STATION IN LIST                         
         B     VRUNLKLP                                                         
*                                                                               
VRUNLKDN DS    0H                                                               
*                                                                               
*        BUILD STATION CALL LETTERS FOR ERROR MESSAGE                           
*                                                                               
         L     R3,FULL             POINT TO STATION ALREADY LOCKED              
*                                                                               
         MVC   WORK,SPACES                                                      
         LA    R1,WORK             POINT TO WORKAREA                            
         STCM  R1,7,RTXTADR        PASS A(MESSGE) TO GETTXT                     
*                                                                               
         MVC   0(4,R1),STLSSTAC    CALL LETTERS                                 
*                                                                               
         LA    R1,3(R1)            END OF CALL LETTERS                          
*                                                                               
         CLI   0(R1),C' '          IF IT ENDS IN BLANK                          
         BH    *+6                                                              
         BCTR  R1,0                   BACK UP                                   
*                                                                               
         LA    R1,1(R1)            NEXT AVAIL PRINT POSITION                    
*                                                                               
         CLI   STLSSTAC+4,C'T'     SKIP IF MEDIA T OR BLANK                     
         BE    *+8                                                              
         CLI   STLSSTAC+4,C' '                                                  
         BNH   *+18                                                             
         MVI   0(R1),C'-'          PRINT MEDIA                                  
         MVC   0(1,R1),STLSSTAC+4                                               
         LA    R1,2(R1)            BUMP TO NEXT PRINT POSITION                  
*                                                                               
         LA    RF,WORK                                                          
         SR    R1,RF               TEXT LENGTH                                  
         STC   R1,RTXTLEN          PASS TEXT LENGTH TO GETTXT                   
*                                                                               
         LA    R2,TITSTATH         CURSOR TO STATION FIELD                      
*                                                                               
         MVC   RERROR,=AL2(STALOCK)  STATION LOCKED ALREADY                     
         GOTO1 MYERROR                                                          
*                                                                               
STALCKD  EQU   547                 STATION LOCKED                               
STALOCK  EQU   795                 STATION LOCKED                               
*                                                                               
VRLCKX   DS    0H                                                               
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
*        ISSUE LTRANS REQUEST IF ON-LINE AND REP WANTS IT                       
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    VRLTRNX                                                          
*                                                                               
         TM    WHEN,X'70'          SKIP IF NOT A REPORT                         
         BZ    VRLTRNX                                                          
*                                                                               
         LA    R3,STLIST           POINT TO STATION LIST                        
         USING STLISTD,R3          ESTABLISH STATION LIST ENTRY                 
*                                                                               
VRLTRNLP DS    0H                                                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD DONE AT END OF LSIT                     
         BZ    VRLTRNDN                                                         
*                                                                               
         MVC   CSTAT(5),STLSSTAC SET CALL LETTERS                               
*                                                                               
         GOTO1 VLTRANS             GO CHECK IF LTRANS TO BE ISSUED              
*                                                                               
VRLTRNCN DS    0H                                                               
         LA    R3,STLISTL(R3)      NEXT STATION IN LIST                         
         B     VRLTRNLP                                                         
*                                                                               
VRLTRNDN DS    0H                                                               
*                                                                               
VRLTRNX  DS    0H                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
         LA    R2,TITSTATH         CURSOR GOES TO STATION FIELD                 
*                                                                               
EXIT     XIT1  REGS=(R2)                                                        
*                                                                               
         TITLE 'T81009 - RERMP09 - OVERNIGHT DELETION REPORT - CHKDATE'         
***********************************************************************         
*                                                                     *         
*        RERMP09 (T81009) --- OVERNIGHT DELETION REPORT               *         
*                                                                     *         
*        ROUTINE TO CHECK THE START/END DATE FIELDS                   *         
*                                                                     *         
*        INPUT:  R2=HEADER ADDR                                       *         
*                R3=A(COMPRESSED DATE)                                *         
*        OUTPUT: CC EQ VALID                                          *         
*                   NE INVALID                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CHKDATE  NTR1  LABEL=*                                                          
*                                                                               
         CLI   5(R2),0                                                          
         BE    CHKDTOK                                                          
*                                                                               
         TM    DELBYTE,DELALLQ     ONLY FOR OPTION 'ALL'                        
         BZ    CHKDTNAE                                                         
*                                                                               
CD010    GOTO1 ANY                                                              
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK  VALIDATE DATE                        
         CLI   DMCB+3,0            CHECK FOR INVALID DATE                       
         BE    CHKDTNVE                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,0(R3))   COMPRESS DATE                   
*                                                                               
CHKDTOK  DS    0H                                                               
*                                                                               
         CR    RB,RB               CC EQ                                        
         B     CHKDATEX                                                         
*                                                                               
CHKDATEX DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        CHECK DATE ERROR ROUTINES                                              
*                                                                               
CHKDTNVE DS    0H                                                               
         MVC   RERROR,=AL2(ERRINVDT) DATE INVALID                               
         B     CHKDTERR                                                         
*                                                                               
CHKDTNAE DS    0H                                                               
         MVC   RERROR,=AL2(ERRNALL)  DELETE OPTION IS NOT 'ALL'                 
         B     CHKDTERR                                                         
*                                                                               
CHKDTERR DS    0H                                                               
         GOTO1 MYERROR                                                          
*                                                                               
ERRINVDT EQU   13                  DATE INVALID                                 
ERRNALL  EQU   645                 DELETE OPTION IS NOT 'ALL'                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL (R6),34,ELCODE                                                   
*                                                                               
         TITLE 'T81009 --- RERMP09 --- OVERNIGHT DELETES '                      
********************************************************************            
*                                                                  *            
*        VALIDATE STATION                                          *            
*                                                                  *            
*        LIST OF STATIONS OR A MENU ID DESIGNATED AS M=XXXX        *            
*                                                     *XXXX        *            
*                                                                  *            
*        R2 ==> STATION FIELD                                      *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VALSTA   NTR1  BASE=*,LABEL=*                                                   
*                                  **********************************           
*                                  *                                *           
*                                  *           STATION              *           
*                                  *                                *           
*                                  **********************************           
*                                                                               
         USING GEND,RC                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         XC    STMENU,STMENU       INIT STATION MENU CODE                       
         XC    STMENUNM,STMENUNM   INIT STATION MENU NAME                       
*                                                                               
         LA    R5,STLIST           ESTABLISH STATION LIST                       
         USING STLISTD,R5                                                       
         XC    STLISTD(STLISTL),STLISTD   INIT FIRST ENTRY IN LIST              
*                                                                               
         GOTO1 ANY                 INPUT REQUIRED                               
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(16,BUFF),0,0 SCAN INPUT                       
*                                                                               
         MVC   ACTUAL,DMCB+4       SAVE NUMBER OF ENTRIES                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,ACTUAL           NUMBER OF ENTRIES IN FIELD                   
*                                                                               
         LA    R4,BUFF             START OF SCAN BLOCK ENTRIES                  
*                                                                               
*        IF ENTRY STARTS WITH '*', MUST BE A MENU ID                            
*                                                                               
         CLI   12(R4),C'*'         MENU INDICATED                               
         BNE   VSTAMN20                                                         
*                                                                               
         CLI   ACTUAL,1            MAX 1 SCANNED ENTRY ALLOWED                  
         BH    VSTAMN1E                                                         
*                                                                               
         CLI   1(R4),5             MENU ID MAX 4 LONG (ID PLUS *)               
         BH    VSTAMNXE                                                         
*                                                                               
         MVC   STMENU,13(R4)       SAVE MENU CODE                               
         OC    STMENU,SPACES       SPACE FILL                                   
*                                                                               
         B     VSTAMN50                                                         
*                                                                               
VSTAMN20 DS    0H                                                               
*                                                                               
*        IF ENTRY IS 'M' THEN MUST BE A MENU ID                                 
*                                                                               
         CLI   0(R4),1             IF ENTRY IS 1 LONG                           
         BNE   VSTAMNUN                                                         
         CLI   12(R4),C'M'         AND MENU INDICATED                           
         BNE   VSTAMNUN                                                         
*                                                                               
         CLI   ACTUAL,1            MAX 1 SCANNED ENTRY ALLOWED                  
         BH    VSTAMN1E                                                         
*                                                                               
         CLI   1(R4),4             MENU ID MAX 4 LONG                           
         BH    VSTAMNXE                                                         
*                                                                               
         MVC   STMENU,22(R4)       SAVE MENU CODE                               
         OC    STMENU,SPACES       SPACE FILL                                   
*                                                                               
VSTAMN50 DS    0H                                                               
*                                                                               
*        READ MARKET STATIONS LIST                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY              ESTABLISH SET RECORD KEY                     
         USING RSETKEY,R3                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ   SET AS SET RECORD RECORD                     
         MVC   RSETKREP,CPARREP    SET REP CODE                                 
         MVC   RSETKSET,=C'MS'     SET RECORD ID                                
         MVC   RSETKID,STMENU      SET SET IDENTIFIER                           
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   VSTAMNNF                                                         
*                                                                               
         GOTO1 GETREC              READ IN SET RECORD                           
*                                                                               
         SR    RF,RF                                                            
         LA    R6,IO                                                            
         LA    R6,RSETELEM-RSETREC(R6) FIRST ELEMENT                            
*                                                                               
         USING RSETDESD,R6         ESTABLISH DESCRIPTIVE ELEMENT                
*                                                                               
         CLI   RSETDCDE,0          CHECK FOR END OF RECORD                      
         BE    VSTAMN10               SKIP IF ELEMENT NOT FOUND                 
         CLI   RSETDCDE,RSETDCDQ   LOOKING FOR DESCRIPTIVE ELEMENT              
         BE    *+16                                                             
         IC    RF,RSETDELN         GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)                                                      
         B     *-24                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETDELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETDOV)      DECREMENT BY OVERHEAD LENGTH                 
         BNP   VSTAMN10            IGNORE IF NOT THERE                          
*                                                                               
         MVC   STMENUNM,SPACES     INIT DESCRIPTION                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   STMENUNM(0),RSETDESC SAVE MENU NAME                              
*                                                                               
VSTAMN10 DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         LA    R6,IO                                                            
         LA    R6,RSETELEM-RSETREC(R6) FIRST ELEMENT                            
*                                                                               
         USING RSETMEMD,R6         ESTABLISH DESCRIPTIVE ELEMENT                
*                                                                               
         CLI   RSETMCDE,0          CHECK FOR END OF RECORD                      
         BE    VSTAMNVD               MUST FIND ELEMENT                         
         CLI   RSETMCDE,RSETMCDQ   LOOKING FOR MEMBERS ELEMENT                  
         BE    *+16                                                             
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)                                                      
         B     *-24                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETMTOV)     DECREMENT BY OVERHEAD LENGTH                 
         BNP   VSTAMNVD            MUST HAVE SOME MEMBERS                       
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'5'            CALCULATE NUMBER OF STATIONS IN LIST         
*                                                                               
         STC   RF,MENUCNT          SET NUMBER OF STATIONS                       
         SR    R0,R0               INIT STATION SORT ORDER                      
*                                                                               
         LA    R1,RSETMEMB         POINT TO FIRST STATION IN LIST               
*                                                                               
VSTAMNLP DS    0H                                                               
*                                                                               
         MVC   STLSSTAC,0(R1)      SAVE STATION CALL LETTERS                    
*                                                                               
         CLI   STLSSTAC+4,C' '     IF BAND IS BLANK                             
         BH    *+8                                                              
         MVI   STLSSTAC+4,C'T'        SET TO TV                                 
*                                                                               
         STC   R0,STLSSTCD         SET SORT ORDER                               
*                                                                               
VSTAMNCN DS    0H                                                               
*                                                                               
         LA    R1,5(R1)            BUMP TO NEXT STATION                         
         LA    R5,STLISTL(R5)      BUMP TO NEXT STATION AREA                    
         XC    STLISTD(STLISTL),STLISTD   INIT NEXT ENTRY IN LIST               
         AH    R0,=H'1'            BUMP SORT ORDER ID                           
*                                                                               
         BCT   RF,VSTAMNLP                                                      
*                                                                               
VSTAMNDN DS    0H                                                               
*                                                                               
         B     VSTASTAX            END OF MENU LIST                             
*                                                                               
VSTAMNUN DS    0H                                                               
*                                                                               
*        BUILD LIST OF INDIVIDUALLY ENTERED STATIONS                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,ACTUAL           NUMBER OF ENTRIES                            
         STC   RE,MENUCNT          NUMBER OF REQUESTED STATIONS                 
*                                                                               
         SR    R0,R0               INIT STATION SORT ORDER                      
         SR    RF,RF                                                            
*                                                                               
VSTASTLP DS    0H                                                               
*                                                                               
         OC    STLSSTAC,SPACES     INIT STATION CALL LETTERS                    
         ICM   RF,1,0(R4)          ENTRY LENGTH                                 
         BZ    VSTASTNE            ENTRY REQUIRED                               
*                                                                               
         LA    R3,12-1(RF,R4)      POINT TO LAST OF STATION ID                  
*                                                                               
         CLI   0(R3),C'-'          FIND '-'                                     
         BE    *+18                                                             
         BCTR  R3,0                BACK UP A CHARACTER                          
         BCT   RF,*-10                                                          
         IC    RF,0(R4)            USE FULL ID LENGTH                           
         B     *+6                                                              
*                                                                               
         BCTR  RF,0                RECTIFY CALL LETTERS LENGTH                  
*                                                                               
         CH    RF,=H'4'            MAX 4 CHARACTERS FOR CALL LETTERS            
         BH    VSTASTXE                                                         
*                                                                               
         MVC   STLSSTAC,SPACES     INIT CALL LETTERS                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   STLSSTAC(0),12(R4)  SAVE STATION CALL LETTERS                    
*                                                                               
         MVI   STLSSTAC+4,C'T'     ASSUME TV                                    
*                                                                               
         CLI   0(R3),C'-'          IF THERE IS A BAND ENTERED                   
         BNE   *+10                                                             
         MVC   STLSSTAC+4(1),1(R3)    ADD IT TO CALL LETTERS                    
*                                                                               
         STC   R0,STLSSTCD         SET SORT ORDER                               
*                                                                               
VSTASTCN DS    0H                                                               
*                                                                               
         LA    R4,32(R4)           POINT TO NEXT STATION                        
         LA    R5,STLISTL(R5)      BUMP TO NEXT STATION AREA                    
         XC    STLISTD(STLISTL),STLISTD   INIT NEXT ENTRY IN LIST               
         AH    R0,=H'1'            BUMP SORT ORDER ID                           
*                                                                               
         BCT   RE,VSTASTLP                                                      
*                                                                               
VSTASTDN DS    0H                                                               
*                                                                               
VSTASTAX DS    0H                                                               
*                                                                               
*        VALIDATE STATIONS IN LIST                                              
*                                                                               
         LA    R5,STLIST           LIST OF STATIONS TO BE VALIDATED             
*                                                                               
VSTAVALL DS    0H                                                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD CHECK FOR END OF LIST                   
         BZ    VSTAVALD                                                         
*                                                                               
*        READ STATION FILE TO VALIDATE STATION                                  
*                                                                               
         XC    KEY,KEY             ESTABLISH STATION RECORD KEY                 
         LA    R3,KEY                                                           
         USING RSTAKEY,R3                                                       
*                                                                               
         MVI   RSTAKTYP,X'02'      RECORD TYPE                                  
         MVC   RSTAKREP,AGENCY     REP ID                                       
         MVC   RSTAKSTA,STLSSTAC   STATION                                      
*                                                                               
         CLI   RSTAKSTA+4,C'T'     MEDIA IS BLANK FOR ANY TV STATION            
         BE    VSTASTE5                                                         
         CLI   RSTAKSTA+4,X'F0'    IE. MEDIA= T,1-9                             
         BL    VSTASTE9                                                         
         CLI   RSTAKSTA+4,X'F9'                                                 
         BH    VSTASTE9                                                         
*                                                                               
VSTASTE5 DS    0H                                                               
*                                                                               
         MVI   RSTAKSTA+4,C' '                                                  
*                                                                               
VSTASTE9 DS    0H                                                               
*                                                                               
         GOTO1 HIGH                READ FOR STATION POINTER                     
*                                                                               
         CLC   RSTAKEY,KEYSAVE     MUST FIND STATION                            
         BNE   VSTASTNV                                                         
*                                                                               
VSTAVALC DS    0H                                                               
*                                                                               
         LA    R5,STLISTL(R5)      NEXT STATION IN LIST                         
         B     VSTAVALL                                                         
*                                                                               
VSTAVALD DS    0H                                                               
*                                                                               
         B     VALSTAX                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VSTASTXE DS    0H                  STATION ID TOO LONG                          
VSTASTNV DS    0H                  STATION NOT ON FILE                          
         MVC   RERROR,=AL2(INVSTA)                                              
         B     VSTAERR                                                          
*                                                                               
VSTASTNE DS    0H                  STATION ENTRY NEEDED                         
*                                                                               
         MVC   RERROR,=AL2(MISSING)                                             
         B     VSTAERR                                                          
*                                                                               
VSTAMNVD DS    0H                  EMPTY MENU                                   
         MVC   RERROR,=AL2(MENUVOID)                                            
         B     VSTAERR                                                          
*                                                                               
VSTAMNNF DS    0H                  MENU NOT FOUND                               
         MVC   RERROR,=AL2(MENUNOTF)                                            
         B     VSTAERR                                                          
*                                                                               
VSTAMNXE DS    0H                  MENU ID MAX 4 LONG                           
         MVC   RERROR,=AL2(MENUBIG)                                             
         B     VSTAERR                                                          
*                                                                               
VSTAMNNE DS    0H                  NO MENU ID                                   
         MVC   RERROR,=AL2(MISSING)                                             
         B     VSTAERR                                                          
*                                                                               
VSTAMN1E DS    0H                  AT MOST ONE MENU                             
         MVC   RERROR,=AL2(MENUMANY)                                            
         B     VSTAERR                                                          
*                                                                               
VSTAERR  DS    0H                                                               
*                                                                               
         ST    R2,FADDR            A(FIELD IN ERROR)                            
*                                                                               
         LA    RF,STLIST           START OF STATIONS                            
         SR    R5,RF                                                            
         LR    RF,R5                                                            
         SR    RE,RE                                                            
         D     RE,=A(STLISTL)      RELATIVE NUMBER OF CURRENT STATION           
*                                                                               
         LA    RF,1(RF)            ABSOLUTE NUMBER                              
         STC   RF,FADDR            SET ITEM NUMBER                              
*                                                                               
VSTAERR1 DS    0H                                                               
*                                                                               
         GOTO1 =A(MYCURS),RR=RELO09                                             
*                                                                               
VALSTAX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        ERROR EQUATES                                                          
*                                                                               
MENUVOID EQU   791                 MENU HAS NO MEMBERS                          
MENUNOTF EQU   792                 MENU NOT ON FILE                             
MENUBIG  EQU   793                 MENU ID TOO BIG                              
MENUMANY EQU   794                 ONLY ONE MENU ALLOWED                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T81009 - RERMP09 - EDIT FOR DELETES - MYCURS'                   
********************************************************************            
*                                                                  *            
*        POSITION CURSOR TO CORRECT FIELD IN ERRORS                *            
*                                                                  *            
*        INPUT : FADDR = AL1(FLD NUMBER),AL3(SCREEN HEADER)        *            
*                                                                  *            
*        R2 ==> STATION FIELD                                      *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
MYCURS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    MYCURSX                                                          
*                                                                               
         L     R2,FADDR            POINT TO FIRST FIELD IN ERROR                
*                                                                               
         L     R1,ATIOB            ESTABLISH TIOB                               
         USING TIOBD,R1                                                         
*                                                                               
         OI    6(R2),X'80'         TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
*                                                                               
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
*                                                                               
         CLI   FADDR,0             APPLICATION MUST SET FIELD NUMBER            
         BE    MYCURSX                                                          
*                                                                               
         LA    RE,8(R2)            START OF FIRST FIELD                         
*                                                                               
         ZIC   RF,FADDR            NUMBER OF FIELD IN ERROR                     
         BCT   RF,*+8              RELATIVE FIELD NUMBER                        
         B     MYCURS0D            FIRST FIELD IS ONE IN ERROR                  
*                                                                               
         SR    R0,R0                                                            
*                                                                               
MYCURS0L DS    0H                                                               
*                                                                               
         IC    R0,5(R2)            R0 HAS FIELD LENGTH                          
*                                                                               
MYCURS1L DS    0H                                                               
*                                                                               
         CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   MYCURS1C                                                         
*                                  IF COMMA FOUND                               
         BCT   RF,MYCURS1C            DECREMENT FIELD COUNTER                   
*                                     IF FIELD FOUND                            
         LA    RE,1(RE)                  BUMP TO START OF NEXT ITEM             
         B     MYCURS0D                  DONE                                   
*                                                                               
MYCURS1C DS    0H                                                               
*                                                                               
         LA    RE,1(RE)                                                         
         BCT   R0,MYCURS1L                                                      
*                                                                               
MYCURS1D DS    0H                  END OF DATA IN SCREEN FIELD                  
*                                                                               
MYCURS0C DS    0H                                                               
*                                                                               
         IC    R0,0(R2)            SCREEN FIELD LENGTH                          
         AR    R2,R0               BUMP TO NEXT SCREEN FIELD                    
*                                                                               
         BCT   RF,MYCURS0L         DECREMENT FIELD COUNTER                      
*                                     IF FIELD FOUND                            
         LA    RE,8(R2)                  POINT TO FIRST IN FIELD                
*                                                                               
MYCURS0D DS    0H                                                               
*                                                                               
         LA    RF,8(R2)            START OF FIELD                               
         SR    RE,RF               DISPLACEMENT TO POSITION IN FIELD            
         STC   RE,TIOBCURI                                                      
*                                                                               
MYCURSX  DS    0H                                                               
         GOTO1 MYERROR                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
*                                                                               
* FALOCKETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FALOCKETD                                                      
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*REGENINVA                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENINVA                                                      
         PRINT ON                                                               
*RERMPPROF                                                                      
         PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
         PRINT ON                                                               
*REGENSTA                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
* DMREQHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMREQHDR                                                       
         PRINT ON                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPFFD                                                                      
* DDGENTWA                                                                      
* RERMPD6D                                                                      
* REGENMKT                                                                      
* REGENRDP                                                                      
* REGENSET                                                                      
* SRBLKD DSECT                                                                  
*      SPRANSIDD                                                                
* RERMPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE REGENRDP                                                       
       ++INCLUDE REGENSET                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPD6D                                                       
         EJECT                                                                  
       ++INCLUDE REGENMKT                                                       
         EJECT                                                                  
SRBLKD   DSECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
         EJECT                                                                  
       ++INCLUDE RERMPWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
DPLIST   DS    CL20                DAYPART LIST                                 
*                                                                               
DELBYTE  DS    X                   OPTION SWITCHES                              
DELALLQ  EQU   X'01'                 X'01' ALL DELETE                           
DELTXTQ  EQU   X'02'                 X'02' TEXT DELETE                          
DELINVQ  EQU   X'04'                 X'04' INV DELETE                           
DELRNGQ  EQU   X'08'                 X'08' DELETE RANGE                         
*                                                                               
TXRANGE  DS    XL4                 TEXT RANGE                                   
INRANGE  DS    XL8                 INV RANGE                                    
DELBOOKS DS    CL20                TABLE OF BOOKS                               
STDATE   DS    XL2                 START DATE                                   
ENDATE   DS    XL2                 END DATE                                     
DPMENU   DS    CL4                 DAYPART MENU                                 
DPTBL    DS    XL(24*DPTBLL)       DAYPART TABLE                                
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
RELO09   DS    A                                                                
RELO0B   DS    A                                                                
ACLPACK  DS    A                                                                
FADDR    DS    A                                                                
*                                                                               
STMENU   DS    CL4                 STATION MENU CODE                            
STMENUNM DS    CL60                STATION MENU NAME                            
*                                                                               
STLIST   DS    XL(24*STLISTL)      STATIONS LIST                                
*                                                                               
SBUFF    DS    A                   A(BUFFER)                                    
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
         EJECT                                                                  
*  DDREPMASTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
*  TWADCOND                                                                     
TWADCOND DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDTWADCONS                                                     
INVDP    EQU   234                 INVALID DAYPART                              
TXTDP    EQU   568                 ALL/INV TEXT DELETE REQUIRES DPT             
TXTMS    EQU   569                 MKT/STA TEXT DEL INDEPENDENT OF DPT          
MAXDPTSN EQU   654                 MAX 1 DPT FOR SOON REQUEST                   
DPTMNNF  EQU   655                 DAYPART MENU NOT FOUND                       
*                                                                               
         TITLE 'T81009 --- RERMP09 --- OVERNIGHT DELETES - DPTBL'               
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DAYPART CODES AND NAMES                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTBLD   DSECT                                                                  
DPTBCODE DS    CL(L'RRDPCODE)      DAYPART CODE                                 
DPTBSNAM DS    CL(L'RRDPSNAM)      DAYPART SHORT NAME                           
DPTBLNAM DS    CL(L'RRDPLNAM)      DAYPART LONG NAME                            
DPTBLL   EQU   *-DPTBLD            LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'T81021 --- RERMP21 --- OVERNIGHT TRANSFERS - STLISTD'           
***********************************************************************         
*                                                                     *         
*        DSECT FOR LIST OF STATIONS                                   *         
*                                                                     *         
***********************************************************************         
STLISTD  DSECT                                                                  
STLSSTCD DS    CL1                 STATION SORT CODE                            
STLSSTAC DS    CL5                 STATION CALL LETTERS                         
STLISTL  EQU   *-STLISTD           LENGTH OF TABLE ENTRY                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042RERMP09   04/13/09'                                      
         END                                                                    
