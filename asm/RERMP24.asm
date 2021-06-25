*          DATA SET RERMP24    AT LEVEL 071 AS OF 04/13/09                      
*PHASE T81024C,*                                                                
         TITLE 'T81024 - RERMP24 - EDIT FOR PROJECTION'                         
*                                                                               
*********************************************************************           
*                                                                   *           
*        RERMP24 --- VREC SCREEN/REQUEST FOR OVERNIGHT PROJECTIONS  *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* MAR14/90 (MRR) --- DO NOT ALLOW 'SAME' FOR FROM OR 'TO' BOOK      *           
*                                                                   *           
* FEB28/02 (BU ) --- SET TWAWHEN = 5 FOR UPDATIVE SOONS             *           
*                                                                   *           
* DEC01/04 (BU ) --- FORCE BOOK TYPE TO SAME                        *           
*                                                                   *           
* APR01/05 (BU ) --- PERMIT DIFF BK TYPES IF HISP / HISP LPM        *           
*                                                                   *           
* APR13/09 (KUI) --- NEW INVENTORY KEY SUPPORT                      *           
*                                                                   *           
*********************************************************************           
         TITLE 'T81024 - RERMP24 - EDIT FOR PROJECTION - INIT'                  
*********************************************************************           
*                                                                   *           
*        RERMP24 --- VREC SCREEN/REQUEST FOR OVERNIGHT PROJECTIONS  *           
*              INITIALIZATION                                       *           
*                                                                   *           
*********************************************************************           
         PRINT NOGEN                                                            
T81024   CSECT                                                                  
         NMOD1 0,T81024**,RR=RE                                                 
*                                                                               
         L     RC,0(R1)            ESTABLISH GENCON WORKAREA                    
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA             ESTABLISH SCREEN AREA                        
         USING CONHEADH-64,RA                                                   
*                                                                               
         L     R9,ASYSD            ESTABLISH SYSTEM WORKAREA                    
         USING SYSD,R9                                                          
*                                                                               
         L     R8,ASPOOLD          ESTABLISH SPOOL AREA                         
         USING SPOOLD,R8                                                        
*                                                                               
         ST    RE,RELO24           SAVE RELOCATION FACTOR                       
*                                                                               
         TITLE 'T81024 - RERMP24 - EDIT FOR PROJECTION - PRMODE'                
*********************************************************************           
*                                                                   *           
*        RERMP24 --- VREC SCREEN/REQUEST FOR OVERNIGHT PROJECTIONS  *           
*              DETERMINE MODE                                       *           
*                                                                   *           
*********************************************************************           
         SPACE 2                                                                
PRMODE   DS    0H                                                               
*                                                                               
*        PROCESS MODES HANDLED BY THIS OVERLAY                                  
*                                                                               
         CLI   MODE,VALREC         VALREC                                       
         BE    VREC                                                             
*                                                                               
         B     XIT                                                              
*                                                                               
XIT      XIT1                      PROGRAM EXIT POINT                           
*                                                                               
         TITLE 'T81024 - RERMP24 - EDIT FOR PROJECTION - VREC'                  
*********************************************************************           
*                                                                   *           
*        RERMP24 --- VREC SCREEN/REQUEST FOR OVERNIGHT PROJECTIONS  *           
*              VREC - VREC INPUT                                    *           
*                                                                   *           
*********************************************************************           
         SPACE 2                                                                
VREC     DS    0H                                                               
*                                                                               
         GOTO1 VALAGY              SET PARENT REP                               
*                                                                               
         LA    R2,TITSRCEH         VALIDATE SOURCE                              
         GOTO1 VALISVC                                                          
*                                                                               
         MVI   MAX,1               MAXIMUM 1 BOOK                               
*                                                                               
         LA    R2,TITBOOKH         VALIDATE FROM BOOK                           
         GOTO1 VALIBOK                                                          
         GOTO1 =A(CHKBTYPE),RR=RELO24  VALIDATE BOOK TYPE                       
*                                                                               
         CLC   8(2,R2),=C'SA'      ILLEGAL BOOK OPTION                          
         BE    VRECBNVE                                                         
*                                                                               
         MVC   FROM,CBOOKS         SAVE FROM BOOK                               
*                                                                               
         LA    RF,FROM             POINT TO RETURNED BOOK                       
*                                                                               
         TM    0(RF),X'A4'        FROM BOOK INVALID IF ESTIMATED (X'20)         
         BNZ   VRECBNVE             OR PROJECTED(X'04') OR COMPRESS             
*                                     CPM (X'80')                               
         LA    R2,TITTOH           VALIDATE TO BOOK                             
         GOTO1 VALIBOK                                                          
         GOTO1 =A(CHKBTYPE),RR=RELO24  VALIDATE BOOK TYPE                       
*                                                                               
         CLC   8(2,R2),=C'SA'      INVALID BOOK OPTION                          
         BE    VRECBNVE                                                         
*                                                                               
         MVC   TO,CBOOKS           SAVE TO BOOK                                 
*                                                                               
         LA    RF,TO               POINT TO RETURNED BOOK                       
*                                                                               
         TM    0(RF),X'88'        'TO BOOK' INVALID IF TIME PERIOD              
         BNZ   VRECBNVE              OR COMPRESS CPM'S                          
                                                                                
                                                                                
*                                                                               
         CLC   FROM+3(1),TO+3      MUST HAVE SAME BOOK TYPES                    
***>>>   BNE   VRECBKNE                                                         
         BE    VREC0004            SAME - ACCEPTED                              
* WE HAVE TO LET THEM  MIX AND MATCH NOW * MAINLY BECAUSE THEY WILL             
* HAVE TO MIX AND MATCH WITH THE LPM BOOKTYPES DURING PARALLEL PERIODS          
*                  ---- BOOKTYPE P ONLY -----                                   
         CLI   FROM+3,C'P'         IF FROM BOOK IS A P THEN                     
         BE    *+10                DONT SYCHRONIZE                              
         MVC   TO+3(1),FROM+3      DIFFERENT: SET 'TO' TO 'FROM'                
*                                                                               
VREC0004 EQU   *                                                                
         MVC   BOOK,FROM           SAVE FROM BOOK                               
*                                                                               
         LA    R2,TITUPH           READ IN UPGRADE OPTION                       
         GOTO1 ANY                                                              
*                                                                               
         XC    DMCB,DMCB           FIND ADDRESS OF UPVAL                        
         MVC   DMCB+4(4),=X'D9000A13'                                           
         GOTO1 CALLOV,DMCB                                                      
*                                                                               
         L     RF,0(R1)            VALIDATE UPGRADE OPTION                      
         GOTO1 (RF),DMCB,(R2),UPEL,ACOMFACS                                     
*                                                                               
         CLI   DMCB,0              MUST HAVE AN UPGRADE RETURNED                
         BE    VRECUPIE                                                         
*                                                                               
         LA    RE,UPEL             ESTABLISH UPGRADE ELEMENT                    
         USING RAVLNEL,RE                                                       
*                                                                               
         CLI   RAVLNTYP,0          MANIPULATION OF UPGRADE DATA                 
         BE    VREC10                                                           
*                                                                               
         OC    RAVLNOP1,RAVLNOP1                                                
         BZ    VREC10                                                           
*                                                                               
         OC    RAVLNOP2,RAVLNOP2                                                
         BNZ   *+10                                                             
         MVC   RAVLNOP2,FROM+1                                                  
* UPGRADE BOOKTYPE SHOULD  BE AS ENTERED                                        
*                                                                               
         CLI   RAVLNBT,C' '        IF NO BOOK TYPE GIVEN                        
         BH    VREC005                                                          
         CLI   FROM+3,C'P'                                                      
         BE    *+10                                                             
         MVC   RAVLNBT,FROM+3         USE FROM BOOK'S                           
         B     VREC10                                                           
VREC005  EQU   *                                                                
*                                                                               
*   IF FROM BOOKTYPE IS REG HISPANIC, AND TO BOOKTYPE IS LPM HISPANIC,          
*        PERMIT THIS COMBINATION TO PASS VALIDATION.  IN ALL OTHER              
*        INSTANCES, FROM AND TO BOOKTYPES MUST MATCH.                           
*                                                                               
         CLI   RAVLNBT,C'I'        BOOK-TO = (I) / HISP LPM?                    
         BNE   VREC006             NO                                           
         CLI   FROM+3,C'H'         BOOK-FROM = (H) / HISP REG?                  
         BE    VREC10              YES - PERMIT THIS TO GO THROUGH              
         CLC   RAVLNBT,FROM+3      ELSE MUST MATCH FROM BOOK'S                  
         BNE   VRECBKNE                                                         
VREC006  EQU   *                                                                
*                                                                               
         DROP  RE                                                               
*                                                                               
VREC10   DS    0H                                                               
*                                                                               
*        VALIDATE STATION FIELD                                                 
*                                                                               
VSTA     DS    0H                                                               
*                                                                               
         LA    R2,TITSTATH         STATION                                      
*                                                                               
         MVC   TITMKT,SPACES       CLEAR MARKET NAME                            
         OI    TITMKTH+6,X'80'     FORCE FIELD TRANSMISSION                     
*                                                                               
         GOTO1 =A(VALSTA),RR=RELO24 VALIDATE STATION FIELD                      
*                                                                               
VSTAX    DS    0H                                                               
*                                                                               
*                                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *          DAYPART(S)            *           
*                                  *                                *           
*                                  **********************************           
DPTVAL   LA    R2,TITDPTH          VALIDATE DAYPART                             
         GOTO1 ANY                                                              
*                                                                               
         XC    DPLIST,DPLIST       INIT DAYPART LIST                            
         XC    DPMENU,DPMENU       INIT DAYPART MENU CODE                       
*                                                                               
         CLI   5(R2),0             IF NOT ENTERED                               
         BE    *+10                                                             
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
DPTINVE  DS    0H                  INVALID DAYPART                              
*                                                                               
         MVC   RERROR,=AL2(INVDP)  INVALID DAYPART                              
*                                                                               
DPTERR   DS    0H                                                               
*                                                                               
         GOTO1 MYERROR                                                          
*                                                                               
DPTVALX  DS    0H                                                               
*                                                                               
         TITLE 'T81024 - RERMP24 - EDIT FOR PROJECTION - INVRVAL'               
***********************************************************************         
*                                                                     *         
*        RERMP24 - EDIT FOR PROJECTION   - INVENTORY RANGE            *         
*                                                                     *         
*        VALIDATE INVENTORY RANGE - NNNN-NNNN - SECOND NOT NEEDED     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INVRVAL  DS    0H                                                               
*                                                                               
         LA    R2,TITINVRH         POINT TO INVENTORY RANGE                     
*                                                                               
         XC    INVLIST,INVLIST     INIT INVENTORY SAVEAREA                      
*                                                                               
         LA    R0,INVMAX           INIT INV COUNTER                             
*                                                                               
         CLI   5(R2),0             IF THERE IS NO INPUT                         
         BNE   INVR10                                                           
*                                                                               
         CLC   STMENU,SPACES          INV REQ'D IF USING STATION MENU           
         BH    INVRREQE                                                         
*                                                                               
         CLI   TITDPTH+5,0            WE MUST HAVE A DAYPART ENTERED            
         BE    INVRNOTE                                                         
*                                                                               
         B     INVRVALX               NO INPUT OKAY                             
*                                                                               
INVR10   DS    0H                                                               
*                                                                               
         GOTO1 ANY                 READ IN INVENTORY NUMBERS                    
*                                                                               
         LH    R1,=Y(BUFF-SYSD)    GET START OF BUFFER ADDRESS                  
         LA    R1,SYSD(R1)                                                      
         ST    R1,SBUFF            SAVE A(BUFFER START)                         
*                                                                               
         MVC   DMCB+8(4),=C',=,-'  SCAN FOR SINGLE AND RANGES                   
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),('INVMAX',SBUFF)                               
*                                                                               
         CLI   DMCB+4,0            MUST HAVE AT LEAST ONE INV NO.               
         BNH   INVRNOTV                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4         NUMBER OF SCANNED ENTRIES                    
*                                                                               
         CH    R0,=Y(INVMAX)       MAKE SURE IT IS NOT TOO MANY                 
         BH    INVRMAXE                                                         
*                                                                               
         L     R4,SBUFF            POINT TO SCANNER BLOCK                       
         LA    R5,INVLIST          POINT TO INVENTORY SAVEAREA                  
*                                                                               
         CH    R0,=H'1'            IF ONLY ONE INVENTORY NUMBER                 
         BH    INVR30                                                           
*                                                                               
         CLI   0(R4),3                OF LENGTH 3                               
         BNE   INVR30                                                           
*                                                                               
         CLC   =C'ALL',12(R4)         COULD BE 'ALL'                            
         BE    INVRDONE               AND IS ALLOWED                            
*                                                                               
INVR30   DS    0H                                                               
*                                                                               
INVRLOOP DS    0H                                                               
*                                                                               
         MVC   0(4,R5),SPACES      INIT SAVED INVENTORY NUMBER                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,0(R4)          LENGTH OF FIRST INVENTORY NUMBER             
         BZ    INVRDONE            END OF INPUT                                 
*                                                                               
         CH    RF,=AL2(L'RINVKINV) CHECK ON MAX LENGTH                          
         BH    INVRLNGE                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),12(R4)      FIRST INVENTORY NUMBER                       
*                                                                               
         MVC   4(4,R5),0(R5)       COPY INV NUMBER                              
*                                                                               
         ICM   RF,1,1(R4)          LENGTH OF SECOND INVENTORY NUMBER            
         BZ    INVRLP10            SINGLE INVENTORY NUMBER                      
*                                                                               
         CH    RF,=AL2(L'RINVKINV) CHECK ON MAX LENGTH                          
         BH    INVRLNGE                                                         
*                                                                               
         MVC   4(4,R5),SPACES      INIT SECOND INVENTORY NUMBER                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R5),22(R4)      SECOND INVENTORY NUMBER                      
*                                                                               
         CLC   0(4,R5),4(R5)       SECOND MUST BE HIGHER THAN FIRST             
         BH    INVRERR2                                                         
*                                                                               
INVRLP10 DS    0H                                                               
*                                                                               
         CLI   MENUCNT,1           SKIP IF MORE THAN ONE STATION                
         BH    INVRCONT                                                         
*                                                                               
*        READ FOR INVENTORY MASTER POINTER                                      
*                                                                               
         XC    KEY,KEY             ESTABLISH INVENTORY RECORD KEY               
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
*                                                                               
         MVI   RINVKTYP,RINVKTYQ   SET INVENTORY RECORD TYPE                    
         MVC   RINVKREP,AGENCY     USE SIGN ON REP                              
         LA    R1,STLIST           POINT TO STATION LIST                        
         MVC   RINVKSTA,STLSSTAC-STLISTD(R1)   ACTIVE STATION                   
         MVC   RINVKINV,0(R5)      USE STARTING INVENTORY NUMBER                
*                                                                               
         GOTO1 HIGH                READ FOR POINTER                             
*                                                                               
         CLC   RINVKEY(RINVKINV-RINVKEY),KEYSAVE   SAME STATION                 
         BNE   INVRNFE                                                          
*                                                                               
         CLC   RINVKINV,0(R5)      INV NO. MUST BE IN RANGE                     
         BL    INVRNFE                                                          
*                                                                               
         CLC   RINVKINV,4(R5)      INV NO. MUST BE IN RANGE                     
         BH    INVRNFE                                                          
*                                                                               
INVRCONT DS    0H                                                               
*                                                                               
         LA    R4,32(R4)           NEXT SCANNED BLOCK                           
         LA    R5,8(R5)            NEXT SAVEAREA                                
         BCT   R0,INVRLOOP                                                      
*                                                                               
INVRDONE DS    0H                  ALL INV NOS. VALID                           
*                                                                               
         B     INVRX                                                            
*                                                                               
*        INVENTORY VALIDATION ERROR MESSAGES                                    
*                                                                               
INVRERR2 DS    0H                  ILLEGAL INVENTORY RANGE                      
         MVC   RERROR,=AL2(INVRSEQE)                                            
         B     INVRERR                                                          
*                                                                               
INVRNFE  DS    0H                  INVENTORY NUMBER NOT FOUND                   
         MVC   RERROR,=AL2(INVRNTFD)                                            
         B     INVRERR                                                          
*                                                                               
INVRLNGE DS    0H                  INVENTORY NUMBER TOO LONG                    
         MVC   RERROR,=AL2(INVRLNGQ)                                            
         B     INVRERR                                                          
*                                                                               
INVRMAXE DS    0H                  TOO MANY INVENTORY NUMBERS                   
         MVC   RERROR,=AL2(INVRMAXQ)                                            
         B     INVRERR                                                          
*                                                                               
INVRREQE DS    0H                  STATIONS MENU REQUIRES INV ENTRY             
         MVC   RERROR,=AL2(INVRMNRQ)                                            
         B     INVRERR                                                          
*                                                                               
INVRNOTE DS    0H                  DAYPART REQUIRED                             
         LA    R2,TITDPTH          PUT CURSOR AT DAYPART FIELD                  
         MVC   RERROR,=AL2(MISSING) MISSING INPUT                               
         B     INVRERR                                                          
*                                                                               
INVRNOTV DS    0H                  INVALID INPUT                                
         MVC   RERROR,=AL2(INVALID)                                             
         B     INVRERR                                                          
*                                                                               
INVRERR  DS    0H                                                               
*                                                                               
         ST    R2,FADDR            A(FIELD IN ERROR)                            
*                                                                               
         LA    RF,INVMAX           MAXIMUM ALLOWED INV NOS.                     
         SR    RF,R0                                                            
         LA    RF,1(RF)            NUMBER OF INV NO. IN ERROR                   
         STC   RF,FADDR            PASS ITEM IN ERROR NUMBER                    
*                                                                               
         GOTO1 MYERROR                                                          
*                                                                               
INVRX    DS    0H                                                               
*                                                                               
*                                                                               
INVNOTVE EQU   690                 INVENTORY ID NOT VALID                       
INVRNGER EQU   691                 INVALID RANGE                                
INVRNTFD EQU   797                 INVENTORY NOT FOUND                          
INVRLNGQ EQU   798                 INVENTORY NUMBER TOO LONG                    
INVRMAXQ EQU   799                 TOO MANY INVENTORY NUMBERS                   
INVRMNRQ EQU   800                 INVENTORY REQ'D WITH STATION MENU            
INVRSEQE EQU   691                 INVENTORY RANGE ILLEGAL                      
*                                                                               
INVRVALX DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE START DATE                                                    
*                                                                               
         TITLE 'RERMP24 - EDIT FOR O/N TRANSFER - VSTRT'                        
***********************************************************************         
*                                                                     *         
*        RERMP24 - EDIT FOR PROJECTION   - START DATE                 *         
*                                                                     *         
*        VALIDATE START DATE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VSTRT    DS    0H                                                               
*                                                                               
         LA    R2,TITSTRTH         POINT TO START DATE                          
         XC    STRTOPT,STRTOPT     INIT START DATE OPTIONS                      
*                                                                               
         CLI   5(R2),0             NO ENTRY                                     
         BE    VREC14                                                           
*                                                                               
         CLC   8(3,R2),=C'ALL'     AND 'ALL' VALID ENTRIES                      
         BE    VREC14                                                           
*                                  VALIDATE START DATE                          
         GOTO1 DATVAL,PARAS,(0,8(R2)),WORK                                      
         CLI   PARAS+3,0                                                        
         BE    VRECDTEE            INVALID DATE                                 
*                                                                               
         GOTO1 DATCON,PARAS,(0,WORK),(3,STRTOPT) SAVE START DATE                
*                                                                               
VREC14   LA    R2,TITENDH          POINT TO END DATE                            
         MVC   ENDOPT,=X'FFFFFF'   INIT END DATE                                
*                                                                               
         CLI   5(R2),0             DONE IF NO ENTRY                             
         BE    VREC16                                                           
         CLC   8(3,R2),=C'ALL'     OR 'ALL'                                     
         BE    VREC16                                                           
*                                  VALIDATE END DATE                            
         GOTO1 DATVAL,PARAS,(0,8(R2)),WORK                                      
         CLI   PARAS+3,0                                                        
         BE    VRECDTEE            INVALID DATE                                 
*                                  SAVE END DATE                                
         GOTO1 DATCON,PARAS,(0,WORK),(3,ENDOPT)                                 
*                                                                               
VREC16   LA    R2,TITDETH          DETAILS                                      
*                                                                               
         CLI   5(R2),0             OKAY IF NONE ENTERED                         
         BE    VREC18                                                           
*                                                                               
         CLI   8(R2),C'Y'          MUST BE 'Y'                                  
         BE    VREC18                                                           
         CLI   8(R2),C'N'          OR 'N'                                       
         BE    VREC18                                                           
*                                                                               
         B     VRECNVE             INVALID DETAIL OPTION                        
*                                                                               
VREC18   LA    R2,TITACTH          USE HIGHER OF ACTUAL OR PROJ.                
*                                                                               
         XC    CHKBOOK,CHKBOOK     INIT BOOK FOR COMPARISON                     
         XC    CHKDEMO,CHKDEMO     INIT DEMO FOR COMPARISON                     
*                                                                               
         CLI   5(R2),0                                                          
         BE    VREC26                                                           
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BE    VREC26                                                           
*                                                                               
         CLI   8(R2),C'N'                                                       
         BE    VREC26                                                           
*                                                                               
         B     VRECNVE             INVALID OPTION                               
*                                                                               
VREC26   DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        VALIDATE DEMO FOR ACTIVITY CHECKING                          *         
*        DEFAULT IS HOUSEHOLDS                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRCDM    DS    0H                                                               
*                                                                               
         LA    R2,TITADEMH         POINT TO DEMO FIELD                          
*                                                                               
         CLI   5(R2),0             OKAY IF NOT ENTERED                          
         BE    VRCDMX                                                           
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
         MVC   DBFILE,=C'INV'      INIT DBLOCK                                  
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         GOTO1 DEMOVAL,DMCB,(R2),(1,WORK),(0,DBLOCK)                            
*                                                                               
         DROP  R4                                                               
*                                                                               
         CLI   DMCB+4,0            TEST NUMBER OF DEMOS FOUND                   
         BZ    VRCDMERR              NO VALID DEMOS                             
*                                                                               
         MVC   CHKDEMO,WORK        SAVE REFERENCE DEMO                          
*                                                                               
         CLI   CHKDEMO+1,C'R'      MUST BE A RATING                             
         BE    *+8                                                              
         CLI   CHKDEMO+1,C'S'      OR SHARE                                     
         BNE   VRCDMER1                                                         
*                                                                               
         B     VRCDMX                                                           
*                                                                               
VRCDMERR DS    0H                  INVALID REFERENCE DEMO                       
*                                                                               
         LHI   RF,INVALID          DEMO INPUT INVALID                           
         STCM  RF,3,RERROR         SET ERROR MESSAGE CODE                       
         GOTO1 MYERROR                                                          
*                                                                               
VRCDMER1 DS    0H                  CHEKDEMO MUST BE A RATING                    
*                                                                               
         LHI   RF,RMEDMRTG         DEMO INPUT MUST BE A RATING                  
         STCM  RF,3,RERROR         SET ERROR MESSAGE CODE                       
         GOTO1 MYERROR                                                          
*                                                                               
VRCDMX   DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        VALIDATE BOOK FOR ACTIVITY CHECKING                          *         
*        DEFAULT IS YEAR AGO TRACK                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRCBK    DS    0H                                                               
*                                                                               
         LA    R2,TITABKH          POINT TO REFERENCE BOOK                      
*                                                                               
         CLI   5(R2),0                                                          
         BE    VRCBKX              OKAY IF NOT ENTERED                          
*                                                                               
         MVI   MAX,1               MAXIMUM 1 BOOK                               
*                                                                               
         GOTO1 VALIBOK             VALIDATE BOOK INPUT                          
*                                                                               
         MVC   CHKBOOK,CBOOKS      SAVE REFERENCE BOOK                          
*                                                                               
         TM    CHKBOOK,X'AC'      FROM BOOK INVALID IF ESTIMATED (X'20)         
         BNZ   VRECBACT             OR PROJECTED(X'04') OR COMPRESS             
*                                     CPM (X'80') OR TIME PERIOD X'08'          
VRCBKX   DS    0H                                                               
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
         OI    REQRTYP,X'80'       INDICATE AN UPDATIVE SOON                    
         MVI   TWAWHEN,5           SET FOR UPDATIVE SOON                        
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
         LA    R2,TITBOOKH         DONE RETURN POINTING TO BOOK FIELD           
         B     EXIT                                                             
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VRECBNVE EQU   *                   INVALID RATING BOOK                          
*                                                                               
         MVC   RERROR,=AL2(INVBOOK)                                             
         GOTO1 MYERROR                                                          
*                                                                               
VRECBACT EQU   *                   BOOK MUST BE AN ACTUAL BOOK                  
*                                                                               
         LHI   RF,RMEBKACT                                                      
         STCM  RF,3,RERROR                                                      
         GOTO1 MYERROR                                                          
*                                                                               
VRECBKNE EQU   *                   BOOK TYPES MUST BE THE SAME                  
*                                                                               
         MVC   RERROR,=AL2(BKTYPNE)                                             
         GOTO1 MYERROR                                                          
*                                                                               
VRECUPIE EQU   *                   INVALID UPGRADE                              
*                                                                               
         MVC   RERROR,=AL2(235)    INVALID UPGRADE                              
         GOTO1 MYERROR                                                          
*                                                                               
VRECDPTE EQU   *                   INVALID DAYPART                              
*                                                                               
         MVC   RERROR,=AL2(INVDP)                                               
         GOTO1 MYERROR                                                          
*                                                                               
VRECDTEE EQU   *                   INVALID DATE                                 
*                                                                               
         MVC   RERROR,=AL2(INVDATE)                                             
         GOTO1 MYERROR                                                          
*                                                                               
VRECNVE  EQU   *                   INVALID ENTRY                                
*                                                                               
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 MYERROR                                                          
*                                                                               
EXIT     XIT1  REGS=(R2)                                                        
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T81024 --- RERMP24 --- OVERNIGHT PROJECTIONS '                  
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
*        VALIDATE STATION/MARKET COMBINATION                                    
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK                            
*                                                                               
         MVC   DBSELAGY,CPARREP                                                 
         MVC   DBFILE,=C'TPT'                                                   
*                                                                               
         MVC   DBAREC,AIO2             A(DEMO WORK AREA)                        
*                                                                               
         MVI   DBFUNCT,DBVLSTBK    VALIDATE STATION BOOK                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELSRC,TITSRCE    SET SOURCE FIELD                             
         MVC   DBSELBK,FROM+1      SET BOOK                                     
*                                                                               
         MVC   DBSELSTA,STLSSTAC   SET STATION CALL LETTERS                     
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         MVC   DBBTYPE,FROM+3      SET BOOK TYPE                                
*                                                                               
***>>>   NI    DBBTYPE,X'FF'-X'40' MAKE 'BOOK TYPE' LOWER CASE                  
*                                                                               
*                                                                               
         CLI   DBSELSTA+4,C'T'     CHANGE CALL LETTER SUFFIX                    
         BE    *+8                                                              
         CLI   DBSELSTA+4,C'2'     FOR A PS/1 STATION                           
         BE    *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCK,0,0                                           
         CLI   DBERROR,0           EXIT ON ERROR                                
         BNE   VRBKSTAE                                                         
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
VRBKSTAE DS    0H                                                               
*                                                                               
         MVC   RERROR,=AL2(INVBKSTA) INVALID BOOK FOR STATION                   
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
         GOTO1 =A(MYCURS),RR=RELO24                                             
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
         TITLE 'T81024 --- RERMP24 --- OVERNIGHT PROJECTIONS '                  
********************************************************************            
*                                                                  *            
*        CHECK FOR INVALID BOOK TYPE 'D'                           *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
CHKBTYPE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R3,CBOOKS                                                        
*                                                                               
CHKBT10  DS    0H                                                               
         CLI   0(R3),X'FF'         END OF LIST?                                 
         BE    CHKBTYPX                                                         
*                                                                               
         CLI   3(R3),C'D'          INVALID BOOK TYPE?                           
         BE    CHKBTINV                                                         
         LA    R3,4(R3)                                                         
         B     CHKBT10                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
CHKBTINV DS    0H                                                               
         MVC   RERROR,=AL2(INVDTYPE)                                            
         B     CHKBTERR                                                         
*                                                                               
CHKBTERR DS    0H                                                               
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
CHKBERR1 DS    0H                                                               
*                                                                               
         GOTO1 =A(MYCURS),RR=RELO24                                             
*                                                                               
CHKBTYPX DS    0H                                                               
         XIT1                                                                   
*                                                                               
INVDTYPE EQU   859                 D TYPE MUST OTRANS FIRST                     
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T81024 - RERMP24 - EDIT FOR PROJECTION - MYCURS'                
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
         ZIC   R0,0(R2)            SCREEN FIELD LENGTH                          
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
         TITLE 'T81024 - RERMP24 - EDIT FOR PROJECTION - WORKAREAS'             
       ++INCLUDE RERMPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPE2D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
*                                                                               
         EJECT                                                                  
* RERMPWORKD                                                                    
       ++INCLUDE RERMPWORKD                                                     
         ORG   SYSSPARE            PROGRAM WORK FIELDS                          
STRTOPT  DS    CL3                                                              
ENDOPT   DS    CL3                                                              
FROM     DS    CL4                                                              
TO       DS    CL4                                                              
UPEL     DS    CL24                                                             
DPLIST   DS    CL20                                                             
BOOK     DS    XL4                 RATINGS BOOK WORKAREA                        
*                                                                               
RELO24   DS    A                   OVLY 24 RELOCATION FACTOR                    
RELO26   DS    A                   OVLY 26 RELOCATION FACTOR                    
FADDR    DS    A                   A(FIELD IN ERROR)                            
*                                                                               
DPMENU   DS    CL4                 DAYPART MENU ID                              
*                                                                               
DPTBL    DS    XL(24*DPTBLL)       DAYPART TABLE                                
*                                                                               
STMENU   DS    CL4                 STATION MENU CODE                            
STMENUNM DS    CL60                STATION MENU NAME                            
*                                                                               
STLIST   DS    XL(24*STLISTL)      STATIONS LIST                                
*                                                                               
INVLIST  DS    XL((INVMAX+1)*2*L'RINVKINV)  INVENTORY NUMBERS LIST              
*                                                                               
INVMAX   EQU   X'1E'               MAXIMUM NUMBER OF INVENTORY NUMBERS          
*                                                                               
SBUFF    DS    A                   A(BUFFER)                                    
*                                                                               
CHKBOOK  DS    XL4                 *CHECK BOOK                                  
CHKDEMO  DS    XL3                 *CHECK DEMO                                  
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
*        ERROR MESSAGE EQUATES                                                  
*                                                                               
INVBOOK  EQU   232                 INVALID  RATINGS BOOK                        
INVDP    EQU   234                 INVALID  DAYPART                             
INVBKSTA EQU   639                 BOOK NOT VALID FOR STATION                   
BKTYPNE  EQU   640                 BOOK TYPES MUST BE THE SAME                  
MAXDPTSN EQU   654                 MAX 1 DPT FOR SOON REQUEST                   
DPTMNNF  EQU   655                 DAYPART MENU NOT FOUND                       
RMEDMRTG EQU   831                 CHECK DEMO MUST BE A RATING                  
RMEBKACT EQU   832                 CHECK BOOK MUST BE AN ACTUAL BOOK            
*                                                                               
         TITLE 'T81024 --- RERMP24 --- OVERNIGHT PROJECTIONS - DPTBL'           
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
         TITLE 'T81024 --- RERMP24 --- OVERNIGHT TRANSFERS - STLISTD'           
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
         TITLE 'T81024 - RERMP24 - EDIT FOR PROJ - INCLUDED BOOKS'              
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*RERMPPROF                                                                      
         PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
         PRINT ON                                                               
* DDTWADCOND                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDTWADCOND                                                     
         PRINT ON                                                               
* DMREQHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMREQHDR                                                       
         PRINT ON                                                               
* FALOCKETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FALOCKETD                                                      
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* REGENAVL                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENAVL                                                       
         PRINT ON                                                               
* REGENSTA                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
* REGENINVA                                                                     
         PRINT OFF                                                              
       ++INCLUDE REGENINVA                                                      
         PRINT ON                                                               
* REGENRDP                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENRDP                                                       
         PRINT ON                                                               
* REGENSET                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENSET                                                       
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071RERMP24   04/13/09'                                      
         END                                                                    
