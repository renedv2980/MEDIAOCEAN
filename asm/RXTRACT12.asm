*          DATA SET RXTRACT12  AT LEVEL 020 AS OF 02/20/13                      
*PHASE RXTRACTB                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE REGENBUC                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE RXROUT12                 XTRACT RECORD CREATION MODULE                 
*INCLUDE RXCNV12                  CONVERSION ROUTINES FOR ALL ABOVE             
*INCLUDE REPVAL5                  CONTRACT BUCKET MODULE                        
*INCLUDE COVAIL                   VIRTUAL MEMORY ALLOCATION                     
*INCLUDE BINSRCH2                                                               
*INCLUDE UNBOOK                                                                 
         SPACE 1                                                                
         TITLE 'REPXTRACT - EXTRACT REP SYSTEM FILE SQL DATA'                   
***********************************************************************         
*  NEW DEVELOPMENT VERSION OF EXTRACT                                 *         
*  CONTROL OF GENERATION OF FORECAST CONTRACTS - SEARCH FOR VALUE     *         
*        'FORECAST CONTROL'  - CODE TO SKIP ORDERS IS LOCATED AT      *         
*         THAT TAG.                                                   *         
*                                                                     *         
***********************************************************************         
*  REP  SQL SUB SYSTEM EXTRACT CONTROL MODULE                         *         
*                                                                     *         
* CONTROL IS PASSED FROM DXTRACT WITH PARAMETERS:                     *         
* R1=A(EXTRACT CONTROL DATA BLOCK - SEE DSECT DXBLOCKD)               *         
*                                                                     *         
* MODULE ENTERED WITH ONE OF FOLLOWING MODES IN DXMODE:               *         
*   DXOPENQ  - OPEN SYSTEM FILES                                      *         
*   DXCLOSEQ - CLOSE SYSTEM FILES                                     *         
*   DXLOADQ  - EXTRACT FROM FILES IN LOAD MODE                        *         
*   DXUPDTQ  - EXTRACT FROM FILES IN UPDATE MODE                      *         
*                                                                     *         
* FOR DXLOADQ AND DXUPDTQ MODES,                                      *         
* DXBLOCKD CONTAINS DXSTPTR WHICH IS:                                 *         
* A(CURRENT ENTRY IN SUB SYSTEM EXTRACT DRIVER TABLE - SEE DSECT      *         
*                                                      SYSTABD)       *         
*                                                                     *         
* RETURN CC .NE. IF ERROR CONDITION ELSE CC .EQ. FOR OK               *         
*                                                                     *         
* DXUSER = 32 BYTE INPUT CARD FROM USERPARM, SEE RXUSERD              *         
*---------------------------------------------------------------------*         
*                                                                     *         
* 09/05/2001  JRD   HANDLE REPCODE OVERRIDE FROM RXUSERD              *         
*                   INCLUDE XRER FLAG ON CONTRACT                     *         
*                                                                     *         
* 09/18/2001  JRD   REMOVE GROUP FROM XSP EXTRACT                     *         
*                                                                     *         
* 09/19/2001  JRD   INCLUDE XRER FLAG ON DOLLAR/BUCKET                *         
*                                                                     *         
* 10/30/2001  JRD   MAKE CONTRACT TYPE A SUBSIDIARY RECORD            *         
*                   CLEAN UP TABLE DRIVEN RECORD TYPES TO             *         
*                   USE A SINGLE ENTRY POINT                          *         
*                                                                     *         
* 10/31/2001  JRD   MAKE CONTRACT TYPE A MASTER RECORD BUT            *         
*                   HANDLE IT SEPARATELY IN JCL FOR CLEAR CHANNEL     *         
*                   SINCE KATZ STILL HAS NOT LEARNED TO USE SQL       *         
*                                                                     *         
* 11/28/2001  JRD   MAKE CONTRACT TYPE A SUBSIDIARY RECORD            *         
*                                                                     *         
* 07/03/2003  BU    PENDING AND FORECAST                              *         
*                                                                     *         
* 08/04/2003  BU    POINT PERSON CHANGES                              *         
*                                                                     *         
* 02/07/2004  BU    KATZ STRADA VERSION                               *         
*                                                                     *         
* 02/25/2004  BU    KATZ STRADA: CHANGES ONLY VERSION                 *         
*                                                                     *         
* 06/28/2004  BU    INTEREP VERSION:  EXTRA FIELDS (RXRECDIR)         *         
*                                                                     *         
* 06/29/2004  BU    MODIFY STATION FILTER ROUTINE FOR MASTER          *         
*                                                                     *         
* 09/09/2004  BU    INCLUDE INFINITY/'Q' CONTRACTS                    *         
*                                                                     *         
* 07/07/2005  BU    NEW VERSION FOR FUTURE DEVELOPMENT                *         
*                                                                     *         
* 08/10/2005  BU    NEW FIELDS, RESEQUENCE RXRECD LAYOUT              *         
*                                                                     *         
* 08/18/2005  BU    LINK 'F' VERSION FOR RXROUT2F CHANGES             *         
*                                                                     *         
* 09/07/2005  BU    LINK 'G' VERSION FOR RXROUT2G CHANGES             *         
*                                                                     *         
* 09/13/2005  BU    STATION TABLE FILTERING FROM STA/OFFICE INFO      *         
*                   LINK 'H' VERSION                                  *         
*                                                                     *         
* 09/30/2005  BU    INITIAL VERSION 'I' FOR ESS CHANGES / MO          *         
*                      REQUESTED BY R.MARTINEZ / B. HANNAH            *         
*                                                                     *         
* 10/07/2005  BU    SWITCH FILTERS / STATION PIDS VERSION             *         
*                                                                     *         
* 11/03/2005  BU    SKIP STATION TABLE FILTERING                      *         
*                   SKIP COVAIL, ADD FIXED SPACE                      *         
*                                                                     *         
* 11/07/2005  BU    REINSTATE STATION OFFICE FILTERING, ACTIVATE      *         
*                   USE OF FIXED SPACE FOR TABLING.                   *         
*                   FIX UPDATE OF DOLLAR SUBRECORD TYPES.             *         
*                                                                     *         
* 11/09/2005  BU    ADD 'LOSS' DATA FOR CALCULATION, FLAG FOR LOSSES  *         
*                                                                     *         
* 11/23/2005  BU    INCREASE TABLE SIZE, TABLE CONFIRMATION COMMENT   *         
*                   ORDER NUMBERS                                     *         
*                                                                     *         
* 11/30/2005  BU    NEW CTR FIELDS IN BOOKS, LENGTHS RECORDS          *         
*                                                                     *         
* 12/12/2005  BU    TURN OFF TABLE SETUP:  CONSIDER TABLES EMPTY      *         
*                   NEW RECORDS:  STA OFFICE FILTER, CONFIRM COMMENT  *         
*                                                                     *         
* 03/27/2006  BU    NEW VERSION:  ADDITIONAL DATA, RECORDS PER        *         
*                   ROBERT MARTINEZ.                                  *         
*                                                                     *         
* 06/12/2006  BU    MODIFY DATE SETUP CALCULATIONS, BASED ON 'TODAY'  *         
*                                                                     *         
* 11/16/2006  BU    ADD BONUS/TRADE/ETC TEXT TO CONTRACT HEADER       *         
*                                                                     *         
* 03/29/2007  BU    REPAIR ERROR IN SKIP READ OF LOADDOL              *         
*                                                                     *         
* 05/02/2007  BU    ADD 'SWI' REC TO BRSTAB                           *         
*                                                                     *         
* 11/13/2012  KUI   SKIP IF COPY CHANGE PAIR RECORD TYPE DON'T MATCH  *         
***********************************************************************         
         EJECT                                                                  
RXTRACT  CSECT                                                                  
         ENTRY COMFACS                                                          
         ENTRY MASTC                                                            
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NMOD1 WORKL,**RXTK**                                                   
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
*                                                                               
         L     R9,=A(LOCLWORK)     LOCAL WORKING STORAGE AREA                   
         USING LOCLWORK,R9                                                      
*                                                                               
         LA    RA,ADDRESS                                                       
         USING ADDRESSD,RA                                                      
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         LA    RF,TYPTAB                                                        
         ST    RF,ATYPTAB                                                       
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R7                                                      
DXU      USING RXUSERD,DXUSER                                                   
         L     R6,DXSTPTR          R6=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R6                                                       
         MVC   PLATFORM,SXDTPLFM                                                
*                                                                               
* COPY THE DSPACE BYTE FROM CONTROLLER'S SSB TO LOCAL SSB                       
* SO CXTRACT CAN RUN FROM TEST CONTROL FILE                                     
*                                                                               
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),DXDSPAC                           
*                                                                               
         MVC   VERSION,SXDTVER                                                  
         OC    VERSION,VERSION                                                  
         BNZ   MAIN                                                             
         MVI   VERSION,1                                                        
         B     MAIN                                                             
*                                                                               
LOW      SR    RC,RC                                                            
         CHI   RC,256                                                           
         J     EXIT                                                             
*                                                                               
HIGH     CHI   RC,0                                                             
         J     EXIT                                                             
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
MAIN     DS    0H                  THIS MUST BE BEFORE ANY DMGR CALLS           
*                                                                               
         OC    DXDDSIO,DXDDSIO                                                  
         BZ    MINIT010                                                         
*                                                                               
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(8,RF),DXDDSIO     SET ALTERNATE DDSIO                          
*                                                                               
MINIT010 DS    0H                                                               
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         BNE   MCLOSE                                                           
*                                                                               
         BAS   RE,PROCOPEN         OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
         MVC   TYPECODE,SXDTTYP                                                 
*                                                                               
         GOTO1 AGETTYP             SET UP RECORD TYPE TABLE DATA                
*                                                                               
         BRAS  RE,SETDATES                                                      
         BNE   MERR                                                             
         B     MXIT                                                             
*                                                                               
MCLOSE   EQU   *                                                                
         CLI   DXMODE,DXCLOSEQ                                                  
         BNE   MLOAD                                                            
         BAS   RE,PROCCLOS         CLOSE SYSTEM FILES                           
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MLOAD    CLI   DXMODE,DXLOADQ                                                   
         BNE   MUPDT                                                            
*&&DO                                                                           
*   TEST                                                                        
         LA    RF,ASTNAREA                                                      
         LA    RE,7                                                             
         LNR   RE,RE                                                            
         DC    H'0'                                                             
*   TEST END                                                                    
*&&                                                                             
         BAS   RE,PROCLOAD         EXTRACT FROM FILES IN LOAD MODE              
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MUPDT    CLI   DXMODE,DXUPDTQ                                                   
         BNE   MERR                                                             
         BAS   RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
         SPACE 1                                                                
GENINIT  NTR1                                                                   
*                                                                               
         MVC   REPALPHA,SXDTAGY    SET REPALPHA CODE FROM SYSTEM TABLE          
*                                                                               
         L     R4,=A(VDEMADDR)                                                  
         OC    0(4,R4),0(R4)       IS DEMADDR ALREADY LOADED?                   
         BNZ   INIT0010            YES: V(DEMADDR) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00ADE'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
*   SET STATION OFFICE FILTER SPACE                                             
*        THE WORK AREA WAS FORMERLY RETRIEVED BY COVAIL.  A FIXED               
*        FIELD HAS BEEN INSERTED, SO VALUES NORMALLY RETURNED IN                
*        P'N' FIELDS HAVE BEEN PLUGGED IN FOR SIMPLICITY SAKE.                  
*                                                                               
INIT0010 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*                                                                               
         CLC   STABREP,=C'  '      STATION TABLE ALREADY SET?                   
*                                                                               
*   TURN OFF TABLE SETUP                                                        
*                                                                               
         B     GENINITX            YES                                          
         MVC   STABREP,REPALPHA    SET REP FOR TABLE                            
*                                                                               
         L     RF,=F'200000'       SET L(FIELD TO BE CLEARED)                   
*                                  STABLOCK + CONBLOCK                          
         ST    RF,P3                                                            
*                                                                               
         LR    RF,RC               SET A(WORKD)                                 
         L     RF,=A(STABLOCK)                                                  
*                                                                               
         ST    RF,P2               SET A(FIELD TO CLEAR)                        
*                                                                               
         XCEFL 0(RF),P3            CLEAR STATION/OFFICE BLOCK                   
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         L     RF,P2               INSERT A BANNER                              
         MVC   0(8,RF),=C'*S*T*A*T'                                             
*                                                                               
         LA    RF,8(RF)                                                         
         ST    RF,P2                                                            
*                                                                               
         MVC   ABLDAREA,P2         ADDITIONAL WORKSPACE                         
         MVC   ASTNAREA,P2         A(STATION TABLE)                             
         MVC   ANEXTSTA,P2         SET A(NEXT STATION SLOT)                     
*                                                                               
         L     RF,=A(CONBLOCK)                                                  
         MVC   0(8,RF),=C'*C*O*N*F'                                             
         LA    RF,8(RF)                                                         
         ST    RF,ACONAREA         SET A(CONFIRMATION CON STORE)                
         ST    RF,ANEXTCON                                                      
*                                                                               
         GOTO1 =A(STATABL),DMCB,(RC)                                            
*                                  LOAD STATION FILTER TABLE                    
         GOTO1 =A(CONTABL),DMCB,(RC)                                            
*                                  LOAD CONFIRM FILTER TABLE                    
GENINITX EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN REP     SYSTEM FILES                        *         
***********************************************************************         
         SPACE 1                                                                
PROCOPEN NTR1  ,                   SET UTL SENUM                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMOPEN,REP,REPFILES,IO                             
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE REP     SYSTEM FILES                      *         
***********************************************************************         
         SPACE 1                                                                
PROCCLOS NTR1  ,                                                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,REP,0,IO                                    
         CLI   8(R1),0                                                          
         JE    YES                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
*                                                                               
REP      DC    CL8'REP    '                                                     
REPFILES DC    C'NREPDIR NREPFILEX'                                             
VUTL     DC    V(UTL)                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS REP     FILE DATA IN LOAD MODE                              *         
***********************************************************************         
         SPACE 1                                                                
PROCLOAD NTR1  ,                                                                
         MVC   REPALPHA,SXDTAGY    SET REPALPHA CODE FROM SYSTEM TABLE          
*                                                                               
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET UP RECORD TYPE TABLE DATA                
*                                                                               
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                     AFTER FILES ARE OPEN                      
         BRAS  RE,REPINIT                                                       
*                                                                               
         TM    TYPEFLAG,TYPFLTAB   LOAD FROM TABLE?                             
         JZ    LOAD10              NO                                           
*                                                                               
         ICM   R3,15,TYPEALOD                                                   
         USING TYPTABD,R3                                                       
LOAD02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,TYPVER                                                   
         JL    LOAD04                                                           
         MVC   TYPECODE,TYPNAME                                                 
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,TYPLOAD                                                    
         LTR   RF,RF                                                            
         BZ    LOAD04                                                           
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOAD04   LA    R3,TYPTABLQ(R3)                                                  
         J     LOAD02                                                           
         DROP  R3                                                               
*                                                                               
LOAD10   DS    0H                                                               
         L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         BASR  RE,RF                                                            
         JNE   NO                  ERROR EXIT                                   
         J     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* PROCESS REP     FILE DATA IN UPDATE MODE READ RECOVERY FILES        *         
***********************************************************************         
         SPACE 1                                                                
PROCUPDT NTR1  ,                                                                
         L     R5,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R5                                                         
         MVC   REPALPHA,SXDTAGY    SET REPALPHA CODE FROM SYSTEM TABLE          
*                                                                               
         MVC   TYPECODE,SXDTTYP                                                 
*                                                                               
         GOTO1 AGETTYP             SET TYPE TABLE DATA                          
*                                                                               
         CLI   RFILTY,REPFILQ      TEST REPFIL FILE RECORD TYPE                 
         JNE   YES                 ELSE IGNORE RECORD                           
         BAS   RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         JNE   YES                 EITHER IGNORE RECORD                         
*                                                                               
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                     AFTER FILES ARE OPEN                      
         BRAS  RE,REPINIT                                                       
*                                                                               
         TM    TYPEFLAG,TYPFLTAB   UPDATE FROM TABLE?                           
         JZ    UPDT10              NO                                           
*                                                                               
         ICM   R3,15,TYPEAUPD                                                   
         USING TYPTABD,R3                                                       
UPDT02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,TYPVER                                                   
         JL    UPDT04                                                           
         MVC   TYPECODE,TYPNAME                                                 
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,TYPUPDT                                                    
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDT04   LA    R3,TYPTABLQ(R3)                                                  
         J     UPDT02                                                           
         DROP  R3                                                               
*                                                                               
UPDT10   DS    0H                                                               
         L     RF,TYPEAUPD         ELSE CALL UPDATE PROCESS ROUTINE             
         GOTO1 (RF),DMCB,(RC)                                                   
         JNE   NO                  EXIT ERROR                                   
         J     YES                 EXIT OK                                      
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
* NTRY: R5 = A(RECOVERY RECORD)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RECDS,R5                                                         
RX       USING RREPREC,RECVHDR+L'RECVHDR                                        
PROCKEY  NTR1  ,                                                                
         GOTO1 ARECCMP             COMPARE COPY/CHANGE RECORDS                  
         JNE   NO                  NO CHANGES                                   
*                                                                               
         TM    RX.RREPCNTL,X'80'   IS THIS RECORD DELETED?                      
         BZ    PKEY02              NO                                           
         CLI   DXACTION,C'C'                                                    
         JNE   NO                                                               
         CLI   RRECTY,X'02'        IS THIS A CHANGE RECORD?                     
         JNE   NO                  NO                                           
*                                                                               
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+RREPCNTL-RREPREC+4(R4),X'80'                           
         JNZ   NO                  AVOID DELETED 'CHANGED' RECORDS              
         MVI   DXACTION,C'D'                                                    
         J     YES                                                              
*                                                                               
*              TEST RESTORED RECORD USING SAVED RECOVERY COPY RECORD            
*                                                                               
PKEY02   CLI   RRECTY,X'02'        WITH CHANGE RECOVERY RECORD TYPE             
         JNE   YES                                                              
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+RREPCNTL-RREPREC+4(R4),X'80'                           
         JZ    YES                                                              
*&&DO                                                                           
*   TEST                                                                        
**       CLC   =X'0013218F',12(R4)                                              
**       BNE   TEST0X20                                                         
         LA    RF,1                                                             
         L     RE,DXACPYB                                                       
         DC    H'0'                                                             
TEST0X20 EQU   *                                                                
*   TEST END                                                                    
*&&                                                                             
         CLI   RX.RREPREC,X'47'                                                 
*                                  CONFIRM COMMENT RECORD?                      
         JE    YES                 LEAVE AS 'CHANGE' - NOT 'ADD'                
*                                                                               
         MVI   DXACTION,C'A'                                                    
         J     YES                                                              
         DROP  RX,R5                                                            
         EJECT                                                                  
***********************************************************************         
* COMMON ADDRESSES FOR ALL ROUTINES - COVERED BY ADDRESSD DSECT       *         
* ADDRESS IS AT AADDRESS IN W/S                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
ADDRESS  DC    CL8'EXTERNS'                                                     
         DC    V(DATAMGR)                                                       
         DC    V(DMOD000)                                                       
         DC    V(DADDS)                                                         
         DC    V(LOGIO)                                                         
         DC    V(DATCON)                                                        
         DC    V(RXCNVX)                                                        
**       DC    V(AXDACC)                                                        
**       DC    V(AXDTRC)                                                        
*                                                                               
         DC    A(CONERR)           CONTRACT HEADER                              
         DC    V(REPDOLC)          CONTRACT DOLLARS                             
         DC    V(REPAGYC)          AGENCY                                       
         DC    V(REPADVC)          ADVERTISER                                   
         DC    V(REPSALC)          SALESPERSON                                  
         DC    V(REPCTYC)          CONTRACT TYPE                                
         DC    V(REPDCTC)          DEV CONTRACT TYPE                            
         DC    V(REPSTAC)          STATION                                      
         DC    V(REPDSPC)          DEV SALESPERSON                              
         DC    V(REPTEMC)          TEAM                                         
         DC    V(REPPRDC)          PRODUCT CODE                                 
         DC    V(REPCLSC)          CLASS                                        
         DC    V(REPCATC)          CATEGORY                                     
         DC    V(REPOFFC)          OFFICE                                       
         DC    V(REPOWNC)          OWNER                                        
         DC    V(REPMKTC)          MARKET                                       
         DC    V(REPGRPC)          GROUP/SUBGROUP                               
         DC    V(REPRGNC)          REGION                                       
         DC    V(REPTERC)          TERRITORY                                    
         DC    V(REPMSTC)          SUB/MASTER                                   
         DC    V(REPREPC)          REP INFO                                     
         DC    V(REPBD1C)          STATION BUDGET INFO                          
         DC    V(REPBD2C)          OFFICE  BUDGET INFO                          
         DC    V(REPPTPC)          POINT PERSON                                 
         DC    V(REPSETC)          SET CODES                                    
         DC    V(REPSWIC)          SWITCH CODES                                 
         DC    V(CONERR)           COMPETITIVE STATIONS                         
         DC    V(CONERR)           DAYPART/CPP                                  
         DC    V(CONERR)           LENGTH CODES                                 
         DC    V(CONERR)           BOOKS                                        
         DC    V(REPCCOC)          CONFIRM COMMENT RECORD                       
         DC    V(REPSCMC)          STANDARD COMMENT                             
         DC    V(REPOFCC)          OFFICE   COMMENT                             
         DC    V(REPEAGC)          EOP AGENCY RECORDS                           
         DC    V(REPEADC)          EOP ADVERT RECORDS                           
         DC    V(REPEOFC)          EOP OFFICE RECORDS                           
         DC    V(REPESPC)          EOP S/P    RECORDS                           
         DC    V(REPBCDC)          BUYCODE    RECORDS                           
         DC    V(REPBUYC)          BUY RECORDS                                  
*                                                                               
         SPACE 1                                                                
         DC    CL8'FOR_ALL'        COMMON ROUTINES USED BY ALL SUBS             
         DC    A(ACCLOAD)                                                       
         DC    A(ACCUPDT)                                                       
         DC    A(DECIOC)                                                        
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(GETTYP)                                                        
         DC    A(GETIT)                                                         
         DC    A(READHI)                                                        
         DC    A(RECCMP)                                                        
         SPACE 1                                                                
         DC    CL8'LOADING'        ADDRESSES OF LOAD ROUTINES                   
         DC    A(CONERR)           CONTRACT HEADER                              
         DC    A(LOADDOL)          CONTRACT DOLLARS                             
         DC    A(LOADAGY)          AGENCY                                       
         DC    A(LOADADV)          ADVERTISER                                   
         DC    A(LOADSAL)          SALESPERSON                                  
         DC    A(LOADCTY)          CONTRACT TYPE                                
         DC    A(LOADDCT)          DEV CONTRACT TYPE                            
         DC    A(LOADSTA)          STATION                                      
         DC    A(LOADDSP)          DEV SALESPERSON                              
         DC    A(LOADTEM)          TEAM                                         
         DC    A(LOADPRD)          PRODUCT CODE          T                      
         DC    A(LOADCLS)          CLASS                                        
         DC    A(LOADCAT)          CATEGORY                                     
         DC    A(LOADOFF)          OFFICE                                       
         DC    A(LOADOWN)          OWNER                                        
         DC    A(LOADMKT)          MARKET                                       
         DC    A(LOADGRP)          GROUP/SUBGROUP                               
         DC    A(LOADRGN)          REGION                                       
         DC    A(LOADTER)          TERRITORY                                    
         DC    A(LOADMST)          SUB/MASTER                                   
         DC    A(LOADREP)          REP INFO                                     
         DC    A(LOADBD1)          STATION BUDGET INFO                          
         DC    A(LOADBD2)          OFFICE  BUDGET INFO                          
         DC    A(LOADPTP)          POINT PERSON                                 
         DC    A(LOADSET)          SET CODES                                    
         DC    A(LOADSWI)          SWITCH CODES                                 
         DC    A(CONERR)           COMPETITIVE STATIONS                         
         DC    A(CONERR)           DAYPART/CPP                                  
         DC    A(CONERR)           LENGTH CODES                                 
         DC    A(CONERR)           BOOKS                                        
         DC    A(LOADCCO)          CONFIRM COMMENT RECORD                       
         DC    A(LOADSCM)          STANDARD COMMENT                             
         DC    A(LOADOFC)          OFFICE COMMENT                               
         DC    A(LOADEAG)          EOP AGENCY                                   
         DC    A(LOADEAD)          EOP ADVERT                                   
         DC    A(LOADEOF)          EOP OFFICE                                   
         DC    A(LOADESP)          EOP SALESPERSON                              
         DC    A(LOADBCD)          BUYCODE                                      
         DC    A(LOADBUY)          BUY RECORD                                   
         SPACE 1                                                                
         DC    CL8'UPDTING'                                                     
         DC    A(CONERR)           CONTRACT HEADER                              
         DC    A(UPDTDOL)          CONTRACT DOLLARS                             
         DC    A(UPDTAGY)          AGENCY                                       
         DC    A(UPDTADV)          ADVERTISER                                   
         DC    A(UPDTSAL)          SALESPERSON                                  
         DC    A(UPDTCTY)          CONTRACT TYPE                                
         DC    A(UPDTDCT)          DEV CONTRACT TYPE                            
         DC    A(UPDTSTA)          STATION                                      
         DC    A(UPDTDSP)          DEV SALESPERSON                              
         DC    A(UPDTTEM)          TEAM                                         
         DC    A(UPDTPRD)          PRODUCT CODE                                 
         DC    A(UPDTCLS)          CLASS                                        
         DC    A(UPDTCAT)          CATEGORY                                     
         DC    A(UPDTOFF)          OFFICE                                       
         DC    A(UPDTOWN)          OWNER                                        
         DC    A(UPDTMKT)          MARKET                                       
         DC    A(UPDTGRP)          GROUP/SUBGROUP                               
         DC    A(UPDTRGN)          REGION                                       
         DC    A(UPDTTER)          TERRITORY                                    
         DC    A(UPDTMST)          SUB/MASTER                                   
         DC    A(UPDTREP)          REP INFO                                     
         DC    A(UPDTBD1)          STATION BUDGET INFO                          
         DC    A(UPDTBD2)          OFFICE  BUDGET INFO                          
         DC    A(UPDTPTP)          POINT PERSON                                 
         DC    A(UPDTSET)          SET CODES                                    
         DC    A(UPDTSWI)          SWITCH CODES                                 
         DC    A(CONERR)           COMPETITIVE STATIONS                         
         DC    A(CONERR)           DAYPART/CPP                                  
         DC    A(CONERR)           LENGTH CODES                                 
         DC    A(CONERR)           BOOKS                                        
         DC    A(UPDTCCO)          CONFIRM COMMENT RECORD                       
         DC    A(UPDTSCM)          STANDARD COMMENT                             
         DC    A(UPDTOFC)          OFFICE COMMENT                               
         DC    A(UPDTEAG)          EOP AGENCY                                   
         DC    A(UPDTEAD)          EOP ADVERT                                   
         DC    A(UPDTEOF)          EOP OFFICE                                   
         DC    A(UPDTESP)          EOP SALESPERSON                              
         DC    A(UPDTBCD)          BUYCODE                                      
         DC    A(UPDTBUY)          BUY RECORD                                   
         SPACE 1                                                                
         DC    CL8'FILTERS'                                                     
         DC    A(CONERR)           CONTRACT HEADER                              
         DC    A(FILTDOL)          CONTRACT DOLLARS                             
         DC    A(FILTAGY)          AGENCY                                       
         DC    A(FILTADV)          ADVERTISER                                   
         DC    A(FILTSAL)          SALESPERSON                                  
         DC    A(FILTCTY)          CONTRACT TYPE                                
         DC    A(FILTDCT)          DEV CONTRACT TYPE                            
         DC    A(FILTSTA)          STATION                                      
         DC    A(FILTDSP)          DEV SALESPERSON                              
         DC    A(FILTTEM)          TEAM                                         
         DC    A(FILTPRD)          PRODUCT CODE                                 
         DC    A(FILTCLS)          CLASS                                        
         DC    A(FILTCAT)          CATEGORY                                     
         DC    A(FILTOFF)          OFFICE                                       
         DC    A(FILTOWN)          OWNER                                        
         DC    A(FILTMKT)          MARKET                                       
         DC    A(FILTGRP)          GROUP/SUBGROUP                               
         DC    A(FILTRGN)          REGION                                       
         DC    A(FILTTER)          TERRITORY                                    
         DC    A(FILTMST)          SUB/MASTER                                   
         DC    A(FILTREP)          REP INFO                                     
         DC    A(FILTBD1)          STATION BUDGET INFO                          
         DC    A(FILTBD2)          OFFICE  BUDGET INFO                          
         DC    A(FILTPTP)          POINT PERSON                                 
         DC    A(FILTSET)          SET CODES                                    
         DC    A(FILTSWI)          SWITCH CODES                                 
         DC    A(CONERR)           COMPETITIVE STATIONS                         
         DC    A(CONERR)           DAYPART/CPP                                  
         DC    A(CONERR)           LENGTH CODES                                 
         DC    A(CONERR)           BOOKS                                        
         DC    A(FILTCCO)          CONFIRM COMMENT RECORD                       
         DC    A(FILTSCM)          STANDARD COMMENT                             
         DC    A(FILTOFC)          OFFICE COMMENT                               
         DC    A(FILTEAG)          EOP AGENCY                                   
         DC    A(FILTEAD)          EOP ADVERT                                   
         DC    A(FILTEOF)          EOP OFFICE                                   
         DC    A(FILTESP)          EOP SALESPERSON                              
         DC    A(FILTBCD)          BUYCODE                                      
         DC    A(FILTBUY)          BUY RECORD                                   
         SPACE 1                                                                
         DC    CL8'INITS'                                                       
         DC    A(INITALL)          GENERAL INITIALISATION                       
         DC    A(CONERR)           CONTRACT HEADER                              
         DC    A(INITDOL)          CONTRACT DOLLARS                             
         DC    A(INITAGY)          AGENCY                                       
         DC    A(INITADV)          ADVERTISER                                   
         DC    A(INITSAL)          SALESPERSON                                  
         DC    A(INITCTY)          CONTRACT TYPE                                
         DC    A(INITDCT)          DEV CONTRACT TYPE                            
         DC    A(INITSTA)          STATION                                      
         DC    A(INITDSP)          DEV SALESPERSON                              
         DC    A(INITTEM)          TEAM                                         
         DC    A(INITPRD)          PRODUCT CODE          T                      
         DC    A(INITCLS)          CLASS                                        
         DC    A(INITCAT)          CATEGORY                                     
         DC    A(INITOFF)          OFFICE                                       
         DC    A(INITOWN)          OWNER                                        
         DC    A(INITMKT)          MARKET                                       
         DC    A(INITGRP)          GROUP/SUBGROUP                               
         DC    A(INITRGN)          REGION                                       
         DC    A(INITTER)          TERRITORY                                    
         DC    A(INITMST)          SUB/MASTER                                   
         DC    A(INITREP)          REP INFO                                     
         DC    A(INITBD1)          STATION BUDGET INFO                          
         DC    A(INITBD2)          OFFICE  BUDGET INFO                          
         DC    A(INITPTP)          POINT PERSON                                 
         DC    A(INITSET)          SET CODES                                    
         DC    A(INITSWI)          SWITCH CODES                                 
         DC    A(CONERR)           COMPETITIVE STATIONS                         
         DC    A(CONERR)           DAYPART/CPP                                  
         DC    A(CONERR)           LENGTH CODES                                 
         DC    A(CONERR)           BOOKS                                        
         DC    A(INITCCO)          CONFIRM COMMENT RECORD                       
         DC    A(INITSCM)          STANDARD COMMENT                             
         DC    A(INITOFC)          OFFICE COMMENT                               
         DC    A(INITEAG)          EOP AGENCY                                   
         DC    A(INITEAD)          EOP ADVERT                                   
         DC    A(INITEOF)          EOP OFFICE                                   
         DC    A(INITESP)          EOP SALESPERSON                              
         DC    A(INITBCD)          BUYCODE                                      
         DC    A(INITBUY)          BUY RECORD                                   
         SPACE 1                                                                
         DC    C'OPEN   '                                                       
         DC    C'DMREAD '                                                       
         DC    C'DMRSEQ '                                                       
         DC    C'DMRDHI '                                                       
         DC    C'DMCLSE '                                                       
         DC    C'DMFAST '                                                       
         DC    C'GETREC '                                                       
         DC    C'RECOVER'                                                       
         DC    C'CONTROL'                                                       
         DC    C'CTFILE '                                                       
         DC    C'REPDIR '                                                       
         DC    C'REPFIL '                                                       
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    A(COPYBUFF)                                                      
         DC    CL1'Y'                                                           
         DC    80C' '                                                           
*                                                                               
*                                                                               
         LTORG                                                                  
REPFILQ  EQU   X'82'                                                            
MXTRTQ   EQU   X'5E'               FIELD SEPARATOR CHR                          
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
***********************************************************************         
* TYPTAB DEFINES PROCESS RECORD TYPES & IS COVERED BY TYPTABD         *         
*                                                                     *         
* CL3    TYPE NAME                                                    *         
* AL1    N/D                                                                    
* AL1    TYPE FLAGS                                                   *         
* AL3    N/D                                                          *         
* AL4    LOAD ROUTINE ADDRESS                                         *         
* AL4    UPDATE ROUTINE ADDRESS                                       *         
***********************************************************************         
TYPTAB   DS    0L                                                               
*****                                                                           
***** RECORD GROUP TABLES                                                       
*****                                                                           
         DC    CL3'ALL',AL1(00,01,00,00,00),AL4(ALLTAB,ALLTAB)                  
         DC    CL3'MAS',AL1(00,01,00,00,00),AL4(MASTAB,MASTAB)                  
         DC    CL3'SUB',AL1(00,01,00,00,00),AL4(SUBTAB,SUBTAB)                  
         DC    CL3'SUP',AL1(00,01,00,00,00),AL4(SUPTAB,SUPTAB)                  
         DC    CL3'XSP',AL1(00,01,00,00,00),AL4(XSPTAB,XSPTAB)                  
         DC    CL3'BRS',AL1(00,01,00,00,00),AL4(BRSTAB,BRSTAB)                  
         DC    CL3'BRM',AL1(00,01,00,00,00),AL4(BRMTAB,BRMTAB)                  
         DC    CL3'BRX',AL1(00,01,00,00,00),AL4(BRXTAB,BRXTAB)                  
         DC    CL3'BRZ',AL1(00,01,00,00,00),AL4(BRZTAB,BRZTAB)                  
         DC    CL3'BAL',AL1(00,01,00,00,00),AL4(BALTAB,BALTAB)                  
         DC    CL3'ORD',AL1(00,01,00,00,00),AL4(ORDTAB,ORDTAB)                  
         DC    CL3'BUY',AL1(00,01,00,00,00),AL4(BUYTAB,BUYTAB)                  
*****                                                                           
***** INDIVIDUAL RECORD ROUNTINES                                               
*****                                                                           
*                                                                               
* HANDLED BY THE DOLLAR RECORD                                                  
*        DC    CL3'CON',AL1(00,00,00,00,00),AL4(LOADCON,UPDTCON)                
*                                                                               
         DC    CL3'DOL',AL1(00,00,00,00,00),AL4(LOADDOL,UPDTDOL)                
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'GRP',AL1(00,00,00,00,00),AL4(LOADGRP,UPDTGRP)                
         DC    CL3'RGN',AL1(00,00,00,00,00),AL4(LOADRGN,UPDTRGN)                
         DC    CL3'TER',AL1(00,00,00,00,00),AL4(LOADTER,UPDTTER)                
         DC    CL3'MST',AL1(00,00,00,00,00),AL4(LOADMST,UPDTMST)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    CL3'BD1',AL1(00,00,00,00,00),AL4(LOADBD1,UPDTBD1)                
         DC    CL3'BD2',AL1(00,00,00,00,00),AL4(LOADBD2,UPDTBD2)                
         DC    CL3'PTP',AL1(00,00,00,00,00),AL4(LOADPTP,UPDTPTP)                
         DC    CL3'SET',AL1(00,00,00,00,00),AL4(LOADSET,UPDTSET)                
         DC    CL3'SWI',AL1(00,00,00,00,00),AL4(LOADSWI,UPDTSWI)                
         DC    CL3'SCM',AL1(00,00,00,00,00),AL4(LOADSCM,UPDTSCM)                
         DC    CL3'OFC',AL1(00,00,00,00,00),AL4(LOADOFC,UPDTOFC)                
         DC    CL3'EAG',AL1(00,00,00,00,00),AL4(LOADEAG,UPDTEAG)                
         DC    CL3'EAD',AL1(00,00,00,00,00),AL4(LOADEAD,UPDTEAD)                
         DC    CL3'EOF',AL1(00,00,00,00,00),AL4(LOADEOF,UPDTEOF)                
         DC    CL3'ESP',AL1(00,00,00,00,00),AL4(LOADESP,UPDTESP)                
         DC    CL3'BCD',AL1(00,00,00,00,00),AL4(LOADBCD,UPDTBCD)                
*                                                                               
*   HANDLED BY THE DOL ROUTINE                                                  
*                                                                               
***      DC    CL3'CPT',AL1(00,00,00,00,00),AL4(LOADCPT,UPDTCPT)                
***      DC    CL3'DPT',AL1(00,00,00,00,00),AL4(LOADDPT,UPDTDPT)                
***      DC    CL3'LEN',AL1(00,00,00,00,00),AL4(LOADLEN,UPDTLEN)                
***      DC    CL3'BKS',AL1(00,00,00,00,00),AL4(LOADBKS,UPDTBKS)                
*                                                                               
*   HANDLED BY THE STA ROUTINE                                                  
*                                                                               
***      DC    CL3'SOF',AL1(00,00,00,00,00),AL4(LOADCPT,UPDTCPT)                
*                                                                               
         DC    CL3'CCO',AL1(00,00,00,00,00),AL4(LOADCCO,UPDTCCO)                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*-------------------------------------------------------------------            
* ALL RECORDS - ORIGINAL EXTRACT(S)                                             
*-------------------------------------------------------------------            
ALLTAB   DS    0L                                                               
         DC    CL3'DOL',AL1(00,00,00,00,00),AL4(LOADDOL,UPDTDOL)                
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'GRP',AL1(00,00,00,00,00),AL4(LOADGRP,UPDTGRP)                
         DC    CL3'RGN',AL1(00,00,00,00,00),AL4(LOADRGN,UPDTRGN)                
         DC    CL3'TER',AL1(00,00,00,00,00),AL4(LOADTER,UPDTTER)                
         DC    CL3'MST',AL1(00,00,00,00,00),AL4(LOADMST,UPDTMST)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    CL3'BD1',AL1(00,00,00,00,00),AL4(LOADBD1,UPDTBD1)                
         DC    CL3'BD2',AL1(00,00,00,00,00),AL4(LOADBD2,UPDTBD2)                
         DC    CL3'PTP',AL1(00,00,00,00,00),AL4(LOADPTP,UPDTPTP)                
         DC    CL3'SET',AL1(00,00,00,00,00),AL4(LOADSET,UPDTSET)                
         DC    CL3'SWI',AL1(00,00,00,00,00),AL4(LOADSWI,UPDTSWI)                
         DC    CL3'SCM',AL1(00,00,00,00,00),AL4(LOADSCM,UPDTSCM)                
         DC    CL3'OFC',AL1(00,00,00,00,00),AL4(LOADOFC,UPDTOFC)                
         DC    CL3'EAG',AL1(00,00,00,00,00),AL4(LOADEAG,UPDTEAG)                
         DC    CL3'EAD',AL1(00,00,00,00,00),AL4(LOADEAD,UPDTEAD)                
         DC    CL3'EOF',AL1(00,00,00,00,00),AL4(LOADEOF,UPDTEOF)                
         DC    CL3'ESP',AL1(00,00,00,00,00),AL4(LOADESP,UPDTESP)                
         DC    CL3'BCD',AL1(00,00,00,00,00),AL4(LOADBCD,UPDTBCD)                
         DC    CL3'BUY',AL1(00,00,00,00,00),AL4(LOADBUY,UPDTBUY)                
*                                                                               
*   HANDLED BY THE DOL ROUTINE                                                  
*                                                                               
***      DC    CL3'CPT',AL1(00,00,00,00,00),AL4(LOADCPT,UPDTCPT)                
***      DC    CL3'DPT',AL1(00,00,00,00,00),AL4(LOADDPT,UPDTDPT)                
***      DC    CL3'LEN',AL1(00,00,00,00,00),AL4(LOADLEN,UPDTLEN)                
***      DC    CL3'BKS',AL1(00,00,00,00,00),AL4(LOADBKS,UPDTBKS)                
*                                                                               
*   HANDLED BY THE STA ROUTINE                                                  
*                                                                               
***      DC    CL3'SOF',AL1(00,00,00,00,00),AL4(LOADCPT,UPDTCPT)                
*                                                                               
         DC    CL3'CCO',AL1(00,00,00,00,00),AL4(LOADCCO,UPDTCCO)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* ALL RECORDS  - BUSINESS REPORTING EXTRACTS                                    
*-------------------------------------------------------------------            
*                                                                               
BALTAB   DS    0L                                                               
         DC    CL3'DOL',AL1(00,00,00,00,00),AL4(LOADDOL,UPDTDOL)                
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'GRP',AL1(00,00,00,00,00),AL4(LOADGRP,UPDTGRP)                
         DC    CL3'RGN',AL1(00,00,00,00,00),AL4(LOADRGN,UPDTRGN)                
         DC    CL3'TER',AL1(00,00,00,00,00),AL4(LOADTER,UPDTTER)                
***      DC    CL3'MST',AL1(00,00,00,00,00),AL4(LOADMST,UPDTMST)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    CL3'BD1',AL1(00,00,00,00,00),AL4(LOADBD1,UPDTBD1)                
         DC    CL3'BD2',AL1(00,00,00,00,00),AL4(LOADBD2,UPDTBD2)                
         DC    CL3'PTP',AL1(00,00,00,00,00),AL4(LOADPTP,UPDTPTP)                
         DC    CL3'SET',AL1(00,00,00,00,00),AL4(LOADSET,UPDTSET)                
         DC    X'00'                                                            
*                                                                               
*-------------------------------------------------------------------            
* ORD RECORDS  - DOLLAR AND CONFIRM COMMENT RECORDS                             
*-------------------------------------------------------------------            
*                                                                               
ORDTAB   DS    0L                                                               
         DC    CL3'DOL',AL1(00,00,00,00,00),AL4(LOADDOL,UPDTDOL)                
         DC    CL3'CCO',AL1(00,00,00,00,00),AL4(LOADCCO,UPDTCCO)                
         DC    CL3'BUY',AL1(00,00,00,00,00),AL4(LOADBUY,UPDTBUY)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* BUY RECORDS  - BUY RECORDS                                                    
*-------------------------------------------------------------------            
*                                                                               
BUYTAB   DS    0L                                                               
         DC    CL3'BUY',AL1(00,00,00,00,00),AL4(LOADBUY,UPDTBUY)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* SUBSIDIARY LEVEL RECORDS                                                      
*-------------------------------------------------------------------            
SUBTAB   DS    0L                                                               
         DC    CL3'DOL',AL1(00,00,00,00,00),AL4(LOADDOL,UPDTDOL)                
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'MST',AL1(00,00,00,00,00),AL4(LOADMST,UPDTMST)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    CL3'SCM',AL1(00,00,00,00,00),AL4(LOADSCM,UPDTSCM)                
         DC    CL3'OFC',AL1(00,00,00,00,00),AL4(LOADOFC,UPDTOFC)                
         DC    CL3'EAG',AL1(00,00,00,00,00),AL4(LOADEAG,UPDTEAG)                
         DC    CL3'EAD',AL1(00,00,00,00,00),AL4(LOADEAD,UPDTEAD)                
         DC    CL3'EOF',AL1(00,00,00,00,00),AL4(LOADEOF,UPDTEOF)                
         DC    CL3'ESP',AL1(00,00,00,00,00),AL4(LOADESP,UPDTESP)                
         DC    CL3'BCD',AL1(00,00,00,00,00),AL4(LOADBCD,UPDTBCD)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* SUPPORT RECORDS AT THE SUBSIDIARY LEVEL                                       
*-------------------------------------------------------------------            
SUPTAB   DS    0L                                                               
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'MST',AL1(00,00,00,00,00),AL4(LOADMST,UPDTMST)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* MASTER LEVEL RECORDS                                                          
*-------------------------------------------------------------------            
MASTAB   DS    0L                                                               
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'GRP',AL1(00,00,00,00,00),AL4(LOADGRP,UPDTGRP)                
         DC    CL3'TER',AL1(00,00,00,00,00),AL4(LOADTER,UPDTTER)                
         DC    CL3'MST',AL1(00,00,00,00,00),AL4(LOADMST,UPDTMST)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* EXTRA SUPPORT RECORDS AT THE MASTER LEVEL FOR CLEAR CHANNEL                   
*  THESE RECORDS ARE EXTRACTED FROM K3 AND OVERRIDDEN TO 'NU'                   
*  AS WOULD BE DONE BY THE INTEREP TABLE IN DATAMANAGER                         
*                                                                               
*  GROUP, REP, MASTERSUBSIDIARY                                                 
*  ARE HANDLED IN THE JCL AND EXTRACTED FROM 'NU' AS 'NU'                       
*-------------------------------------------------------------------            
XSPTAB   DS    0L                                                               
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    X'00'                                                            
*                                                                               
*-------------------------------------------------------------------            
* SUPPORT RECORDS FOR BUSINESS REPORTING - STANDALONE REP                       
*-------------------------------------------------------------------            
BRZTAB   DS    0L                                                               
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'GRP',AL1(00,00,00,00,00),AL4(LOADGRP,UPDTGRP)                
         DC    CL3'RGN',AL1(00,00,00,00,00),AL4(LOADRGN,UPDTRGN)                
         DC    CL3'TER',AL1(00,00,00,00,00),AL4(LOADTER,UPDTTER)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    CL3'PTP',AL1(00,00,00,00,00),AL4(LOADPTP,UPDTPTP)                
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    CL3'BD1',AL1(00,00,00,00,00),AL4(LOADBD1,UPDTBD1)                
         DC    CL3'BD2',AL1(00,00,00,00,00),AL4(LOADBD2,UPDTBD2)                
         DC    CL3'SET',AL1(00,00,00,00,00),AL4(LOADSET,UPDTSET)                
         DC    CL3'SWI',AL1(00,00,00,00,00),AL4(LOADSWI,UPDTSWI)                
         DC    CL3'SCM',AL1(00,00,00,00,00),AL4(LOADSCM,UPDTSCM)                
         DC    CL3'OFC',AL1(00,00,00,00,00),AL4(LOADOFC,UPDTOFC)                
         DC    CL3'EAG',AL1(00,00,00,00,00),AL4(LOADEAG,UPDTEAG)                
         DC    CL3'EAD',AL1(00,00,00,00,00),AL4(LOADEAD,UPDTEAD)                
         DC    CL3'EOF',AL1(00,00,00,00,00),AL4(LOADEOF,UPDTEOF)                
         DC    CL3'ESP',AL1(00,00,00,00,00),AL4(LOADESP,UPDTESP)                
         DC    CL3'BCD',AL1(00,00,00,00,00),AL4(LOADBCD,UPDTBCD)                
         DC    X'00'                                                            
*                                                                               
*-------------------------------------------------------------------            
* SUPPORT RECORDS AT THE SUBSIDIARY FOR BUSINESS REPORTING                      
*-------------------------------------------------------------------            
BRSTAB   DS    0L                                                               
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    CL3'BD1',AL1(00,00,00,00,00),AL4(LOADBD1,UPDTBD1)                
         DC    CL3'BD2',AL1(00,00,00,00,00),AL4(LOADBD2,UPDTBD2)                
         DC    CL3'SET',AL1(00,00,00,00,00),AL4(LOADSET,UPDTSET)                
         DC    CL3'SCM',AL1(00,00,00,00,00),AL4(LOADSCM,UPDTSCM)                
         DC    CL3'OFC',AL1(00,00,00,00,00),AL4(LOADOFC,UPDTOFC)                
         DC    CL3'EAG',AL1(00,00,00,00,00),AL4(LOADEAG,UPDTEAG)                
         DC    CL3'EAD',AL1(00,00,00,00,00),AL4(LOADEAD,UPDTEAD)                
         DC    CL3'EOF',AL1(00,00,00,00,00),AL4(LOADEOF,UPDTEOF)                
         DC    CL3'ESP',AL1(00,00,00,00,00),AL4(LOADESP,UPDTESP)                
         DC    CL3'SWI',AL1(00,00,00,00,00),AL4(LOADSWI,UPDTSWI)                
         DC    CL3'BCD',AL1(00,00,00,00,00),AL4(LOADBCD,UPDTBCD)                
         DC    CL3'RGN',AL1(00,00,00,00,00),AL4(LOADRGN,UPDTRGN)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* MASTER LEVEL RECORDS FOR BUSINESS REPORTING                                   
*-------------------------------------------------------------------            
BRMTAB   DS    0L                                                               
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'GRP',AL1(00,00,00,00,00),AL4(LOADGRP,UPDTGRP)                
         DC    CL3'RGN',AL1(00,00,00,00,00),AL4(LOADRGN,UPDTRGN)                
         DC    CL3'TER',AL1(00,00,00,00,00),AL4(LOADTER,UPDTTER)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    CL3'PTP',AL1(00,00,00,00,00),AL4(LOADPTP,UPDTPTP)                
         DC    CL3'SET',AL1(00,00,00,00,00),AL4(LOADSET,UPDTSET)                
         DC    CL3'SWI',AL1(00,00,00,00,00),AL4(LOADSWI,UPDTSWI)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* BUSINESS REPORTING                                                            
* EXTRA SUPPORT RECORDS AT THE MASTER LEVEL FOR CLEAR CHANNEL                   
*  THESE RECORDS ARE EXTRACTED FROM K3 AND OVERRIDDEN TO 'NU'                   
*  AS WOULD BE DONE BY THE INTEREP TABLE IN DATAMANAGER                         
*                                                                               
*  GROUP, REP                                                                   
*  ARE HANDLED IN THE JCL AND EXTRACTED FROM 'NU' AS 'NU'                       
*                                                                               
*-------------------------------------------------------------------            
BRXTAB   DS    0L                                                               
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'PTP',AL1(00,00,00,00,00),AL4(LOADPTP,UPDTPTP)                
***>>>   DC    CL3'SET',AL1(00,00,00,00,00),AL4(LOADSET,UPDTSET)                
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
***>>>>STATABL                                                                  
******************************************************************              
* STATABL SETUP:  READ STATION RECORDS AND TABLE OFFICE                         
*        FILTER INFORMATION.                                                    
*        IF A STATION HAS NO CONVERTED OFFICES, THE STATION                     
*        WILL NOT BE INCLUDED IN THE TABLE.                                     
*                                                                               
******************************************************************              
STATABL  NTR1  LABEL=*,BASE=*                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,IO               SET A(IO AREA)                               
         XC    STATABCT,STATABCT                                                
         MVC   STATABFL,=C'*FLG'                                                
         XC    IOKEY,IOKEY                                                      
         LA    R5,IOKEY                                                         
         USING RSTAKEY,R5          SET RECORD DEFINITION                        
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         SPACE                                                                  
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,REPDIR,IOKEY,(R2),DMWORK                    
STAB0020 DS   0H                                                                
         LA    R5,IOKEY            RESET TO PROCESS KEY                         
         MVC   IOKEY(34),IO        KEY TAKEN FROM IO AREA                       
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMRSEQ,REPDIR,IOKEY,(R2),DMWORK                    
STAB0040 DS   0H                                                                
         CLI   IOKEY,X'02'         STATION RECORD?                              
         BNE   STAB0100            NO  - FINISHED                               
         CLC   RSTAKREP,REPALPHA   SIGNON REP?                                  
         BNE   STAB0100            NO  - FINISHED                               
         MVC   REPADDR,IO+28       SET DA(RECORD)                               
*                                  (KEY FOUND IS SET IN IO AREA)                
         DROP  R5                                                               
STAB0060 EQU   *                                                                
         GOTO1 AGETIT              RETRIEVE RECORD                              
         USING RSTAREC,R2                                                       
*                                                                               
         LA    R5,RSTAELEM         SET A(1ST ELEMENT)                           
STAB0070 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    STAB0020            NO ELEMENT - SKIP THIS RECORD                
         CLI   1(R5),0             INVALID ELEMENT LENGTH?                      
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
         CLI   0(R5),X'2D'         STA / OFFC ELEMENT?                          
         BE    STAB0080            NO ELEMENT - SKIP THIS RECORD                
         ZIC   RF,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         B     STAB0070            GO BACK FOR NEXT                             
STAB0080 EQU   *                                                                
*                                                                               
EX       USING RSTAOFEL,R5                                                      
*                                                                               
*   IF ELEMENT EXISTS, IT WILL HAVE OFFICES ASSOCIATED WITH IT THAT             
*        HAVE BEEN CONVERTED.  THESE MUST BE TABLED UP.                         
*                                                                               
         L     R1,ANEXTSTA         SET A(NEXT OPEN SLOT)                        
         USING DSTATD,R1                                                        
*                                                                               
         ZIC   RF,EX.RSTAOFLN      SET A(LEN OF 2D ELEMENT)                     
         SH    RF,=H'13'           SUBTRACT CTRL, DATE, LUID BYTES              
         SRL   RF,1                CALCULATE NUMBER OF OFFICES                  
         L     RE,STATABCT         INCREMENT COUNTER                            
         LA    RE,1(RE)                                                         
         ST    RE,STATABCT         RESTORE COUNTER                              
*                                                                               
         MVC   DSTACALL,RSTAKSTA   INSERT STATION CALL LETTERS                  
*                                                                               
*   ORDER OF NEXT TWO INSTRUCTIONS IS IMPORTANT.  'EX.RSTAOFFC' IS              
*        USING R5 FOR EX., AND 2ND INSTRUCTION IS RESETTING R5                  
*                                                                               
         LA    R3,EX.RSTAOFFC      SET A(1ST STA RECORD OFFICE)                 
         LA    R5,DSTAOFFS         SET A(1ST STA OFFICE IN TABLE)               
STAB0090 EQU   *                                                                
         MVC   0(2,R5),0(R3)                                                    
         LA    R5,2(R5)            BUMP RECEIVING TABLE                         
         LA    R3,2(R3)            BUMP SENDING RECORD                          
         BCT   RF,STAB0090         GO BACK FOR NEXT                             
*                                                                               
         LA    R1,100(R1)          BUMP TO NEXT TABLE SLOT                      
         ST    R1,ANEXTSTA                                                      
         XC    0(100,R1),0(R1)     CLEAR NEXT SLOT                              
         B     STAB0020            GO BACK FOR NEXT RECORD                      
*                                                                               
         DROP  EX,R1                                                            
                                                                                
STAB0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***>>>>STATABL                                                                  
***>>>>CONTABL                                                                  
******************************************************************              
* CONTABL SETUP:  READ CONFIRM X'47' RECORDS AND TABLE CON # AS                 
*        FILTER INFORMATION.                                                    
*                                                                               
******************************************************************              
CONTABL  NTR1  LABEL=*,BASE=*                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,IO               SET A(IO AREA)                               
         XC    CONTABCT,CONTABCT                                                
         MVC   CONTABFL,=C'*FLG'                                                
         XC    IOKEY,IOKEY                                                      
         LA    R5,IOKEY                                                         
         USING RCFCKEY,R5          SET RECORD DEFINITION                        
         MVI   RCFCKTYP,X'47'                                                   
         MVC   RCFCKREP,REPALPHA                                                
         SPACE                                                                  
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,REPDIR,IOKEY,(R2),DMWORK                    
CONT0020 DS   0H                                                                
         LA    R5,IOKEY            RESET TO PROCESS KEY                         
         MVC   IOKEY(34),IO        KEY TAKEN FROM IO AREA                       
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMRSEQ,REPDIR,IOKEY,(R2),DMWORK                    
CONT0040 DS   0H                                                                
         CLI   IOKEY,X'47'         CONFIRM RECORD?                              
         BNE   CONT0100            NO  - FINISHED                               
         CLC   RCFCKREP,REPALPHA   SIGNON REP?                                  
         BNE   CONT0100            NO  - FINISHED                               
*                                  (KEY FOUND IS SET IN IO AREA)                
         L     RE,CONTABCT                                                      
         LA    RE,1(RE)                                                         
         ST    RE,CONTABCT         INCREMENT COUNTER                            
*                                                                               
         CLC   CONTABCT,=A(CONFMAX)                                             
         BNH   *+6                                                              
         DC    H'0'                TABLE EXCEEDED: KILL JOB                     
*                                                                               
         L     RF,ANEXTCON         SET A(NEXT CONFIRM SLOT)                     
         MVC   0(4,RF),RCFCKCON    STORE CONTRACT #                             
         LA    RF,4(RF)            BUMP TO NEXT SLOT                            
         ST    RF,ANEXTCON         SAVE IT                                      
*                                                                               
         DROP  R5                                                               
         B     CONT0020            GO BACK FOR NEXT RECORD                      
*                                                                               
CONT0100 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***>>>>CONTABL                                                                  
***********************************************************************         
* GET TYPE TABLE ACTION FROM 3 CHARACTER CODE                         *         
***********************************************************************         
         SPACE 1                                                                
GETTYP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ATYPTAB                                                       
         USING TYPTABD,RF                                                       
GTYP02   CLI   0(RF),FF            END OF TABLE                                 
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TYPECODE,TYPNAME    COMPARE NAME                                 
         BE    GTYP04                                                           
         LA    RF,TYPTABLQ(RF)     GET NEXT ENTRY                               
         B     GTYP02                                                           
*                                                                               
GTYP04   MVC   TYPENAME,TYPNAME    MATCH FOUND - GET TABLE INFORMATION          
         MVC   TYPEDEEP,TYPLDEEP                                                
         MVC   TYPEFLAG,TYPFLAG                                                 
         MVC   TYPEALOD,TYPLOAD                                                 
         MVC   TYPEAUPD,TYPUPDT                                                 
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DECREMENT MAXIMUM IO COUNT                                          *         
***********************************************************************         
         SPACE 1                                                                
DECIOC   NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,MAXIOS                                                     
         BCTR  RF,0                                                             
         STCM  RF,15,MAXIOS                                                     
         JNZ   YES                                                              
*                                                                               
         LA    R3,DECMSG           OUTPUT IO COUNT EXCEEDED MESSAGE             
         MVC   DECTYPE,TYPENAME                                                 
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     NO                                                               
*                                                                               
DECMSGL  DC    AL2(50)                                                          
DECMSG   DC    CL50' '                                                          
         ORG   DECMSG                                                           
         DC    C'IO COUNT EXCEEDED - TYPECODE = '                               
DECTYPE  DC    CL3' '                                                           
         EJECT                                                                  
***********************************************************************         
* CALL DMGR TO GET A RECORD                                           *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       REPADDR      = DISK ADDRESS TO READ                           *         
* EXIT: CC EQUAL     = RECORD READ OK                                 *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
         SPACE 1                                                                
GETIT    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),REPFIL,REPADDR,(R2),DMWORK          
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'10'                                                      
         JO    LOW                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,REPADDR,GETDA,L'REPADDR,0                        
         GOTO1 (RF),(R1),DMCB+8,GETRC,1                                         
*                                                                               
         LA    R3,GETMSGL          OUTPUT DISK READ ERROR MESSAGE               
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     HIGH                                                             
*                                                                               
GETMSGL  DC    AL2(50)                                                          
GETMSG   DC    CL50' '                                                          
         ORG   GETMSG                                                           
         DC    C'DMGR GETREC ERROR - D/A = '                                    
GETDA    DC    CL8' '                                                           
         DC    C','                                                             
         DC    C' RC = '                                                        
GETRC    DC    CL2' '                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
***********************************************************************         
* CALL DMGR TO PERFORM A READHI                                       *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       IOKEY        = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
         SPACE 1                                                                
READHI   NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,REPDIR,IOKEY,(R2),DMWORK                    
*                                                                               
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,IOKEYSAV,RDHKEY,L'IOKEYSAV,0                     
*                                                                               
         XR    R0,R0                                                            
         WTO   TEXT=((RDHHL,C),(RDH1L,D),(0,E)),MCSFLAG=HRDCPY                  
         J     NO                                                               
*                                                                               
RDHHL    DC    AL2(40)                                                          
         DC    CL40'DMGR READHI ERROR KEY HEXOUT FOLLOWS'                       
*                                                                               
RDH1L    DC    AL2(90)                                                          
RDH1M    DC    CL90' '                                                          
         ORG   RDH1M                                                            
         DC    C'KEY='                                                          
RDHKEY   DC    CL84' '                                                          
         ORG   RDH1M+L'RDH1M                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE COPY AND CHANGE RECORDS TO SEE IF THEY ARE DIFFERENT        *         
*                                                                     *         
* NTRY:                                                               *         
* EXIT: CC EQ    RECORD TO BE PROCESSED                               *         
*     : CC NE    RECORD NOT TO BE PROCESSED                           *         
***********************************************************************         
         SPACE 1                                                                
RECCMP   NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB          GET CHANGE RECORD ADDRESS                    
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
CHG      USING RREPREC,R2                                                       
         CLI   RRECTY,3            ADD OF NEW RECORD?                           
         JE    YES                 YES                                          
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
*                                                                               
* SKIP IF COPY IS MISSING INSTEAD OF ABENDING                                   
*                                                                               
         OC    RECLN,RECLN                                                      
         JNZ   RECCMP20                                                         
*        CLI   RECCFLG,C'Y'        ALREADY NOTIFIED?                            
*        BE    RECCMP10                                                         
*        MVI   RECCFLG,C'Y'        NOTIFY ONLY ONCE                             
*                                                                               
*        GOTO1 VDATAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*SKUI:RXTRACT IS MISS+        
               ING RECOVERY COPIES. RECORDS SKIPPED.'                           
*                                                                               
RECCMP10 DS    0H                                                               
         J     NO                                                               
*                                                                               
RECCMP20 DS    0H                                                               
*                                                                               
         LA    R4,RECVHDR+L'RECVHDR                                             
*                                                                               
CPY      USING RREPREC,R4                                                       
*                                                                               
* SKIP IF COPY/CHANGE PAIR DIFFERENT RECORD TYPE                                
*                                                                               
         CLC   CHG.RREPKEY(1),CPY.RREPKEY                                       
         JE    RECCMP30                                                         
         CLI   RECCFLG,C'Y'        ALREADY NOTIFIED?                            
         JE    NO                                                               
         MVI   RECCFLG,C'Y'        NOTIFY ONLY ONCE                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),=AL2(100)                                                
         MVC   WORK+2(14),=CL14'AUTONOTE*SKUI:'                                 
         MVC   WORK+16(7),=CL7'BAD CC '                                         
         GOTO1 =V(HEXOUT),PARM,CHG.RREPKEY,WORK+23,27,0                         
         WTO   TEXT=WORK                                                        
         J     NO                                                               
*                                                                               
RECCMP30 DS    0H                                                               
         CLC   CHG.RREPLEN,CPY.RREPLEN                                          
         JNE   YES                 RECORD LENGTH HAS CHANGED                    
         XR    R3,R3                                                            
         ICM   R3,3,CPY.RREPLEN                                                 
         LR    R5,R3                                                            
         CLCL  R2,R4               COMPARE TWO RECORDS                          
         JNE   YES                                                              
         J     NO                  RECORDS ARE IDENTICAL                        
*                                                                               
RECCFLG  DC    C'N'                                                             
*                                                                               
         LTORG                                                                  
         DROP  CHG,CPY                                                          
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL EXTRACT RECORDS                                      *         
* NTRY: R1 = LENGTH OF EXTRACT RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
INITALL  NTR1  BASE=*,LABEL=*                                                   
         L     R0,DXAXREC          R0=A(EXTRACT RECORD)                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R3,DXAXREC          R3=A(EXTRACT RECORD AREA)                    
         USING DXHDRD,R3                                                        
*                                                                               
         LA    RF,DXHDRHL                                                       
         SLL   RF,16                                                            
         ST    RF,DXHDRLEN         SET MINIMUM RECORD LEN IN HDR                
         MVI   DXHDRRTY-1,MXTRTQ                                                
         MVC   DXHDRRTY,DXACTION                                                
         MVI   DXHDRCDT-1,MXTRTQ                                                
         CLI   DXMODE,DXLOADQ      LOAD MODE?                                   
         JE    IALL02              YES                                          
*                                                                               
         L     R5,DXARECB          HERE IF UPDATE MODE                          
         USING RECDS,R5                                                         
         GOTO1 VDATCON,DMCB,(3,RDATE),(X'20',DXHDRCDT+2)                        
         MVC   DXHDRCDT(2),DXCENT                                               
         MVI   DXHDRCTI-1,MXTRTQ                                                
         ICM   RF,15,RTIME          FORMAT DATE & TIME FROM RECOVERY            
         TM    RTIME,X'80'                                                      
         JNO   *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
         XC    DUB,DUB                                                          
         STCM  RF,15,DUB+4                                                      
         OI    DUB+7,X'0C'                                                      
         UNPK  DXHDRCTI(6),DUB+4(4)                                             
         OI    DXHDRCTI+5,X'F0'                                                 
         J     YES                                                              
*                                  HERE IF LOAD MODE                            
IALL02   MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
         MVI   DXHDRCTI-1,MXTRTQ                                                
         MVC   DXHDRCTI,DXTIMEN                                                 
         CLI   DXDAFORM,C'Y'                                                    
         JNE   YES                                                              
         MVI   DXHDRCDT+00,C''''                                                
         MVC   DXHDRCDT+01(6),DXDATEN+2                                         
         MVC   DXHDRCDT+07(2),SPACES                                            
         MVC   DXHDRCDT+09(2),DXTIMEN                                           
         MVI   DXHDRCDT+11,C':'                                                 
         MVC   DXHDRCDT+12(2),DXTIMEN+2                                         
         MVI   DXHDRCDT+14,C''''                                                
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* CONTRACT RECORD CALLS BRANCH HERE                                             
*---------------------------------------------------------------------*         
CONERR   DC    H'0'                                                             
         DC    C'CONTRACT ROUTINES SHOULD NOT BE CALLED, ALL DATA IS'           
         DC    C' HANDLED IN THE DOLLAR RECORD ROUTINES'                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD CONTRACT DOLLAR RECORD                                         *         
*        ROUTINE WILL BUILD BOTH DOLLAR BUCKET RECORDS AND            *         
*        CONTRACT HEADER RECORDS AT THE SAME TIME                     *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADDOL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CON RECORD             
*                                                                               
         ST    R2,ARGIOKEY         SAVE A(IOKEY) FOR RXROUTS CALL               
         ST    R7,ARGDXBLK         SAVE A(DXBLOCKD) FOR RXROUTS CALL            
         LA    RF,RESTART                                                       
         ST    RF,ARGRSTRT         SET A(RESTART) FOR RXROUTS CALL              
         MVI   RESTART,0           SET RESTART TO 'FIRST PASS'                  
*                                                                               
         USING RCONREC,R2                                                       
         XC    RCONKEY,RCONKEY                                                  
*                                                                               
         MVI   RCON8ETP,X'8E'                                                   
*                                                                               
         MVC   RCON8ERP,REPALPHA   NO  - INSERT REP CODE INTO KEY               
         CLC   DXU.RXUSTA,SPACES   STATION FILTER?                              
         JNH   LDOL0020            NO                                           
         MVC   RCON8EST,DXU.RXUSTA YES - INSERT STATION                         
LDOL0020 EQU   *                                                                
         L     R2,DXARECB          A(RECORD DELIVERY AREA)                      
         GOTO1 AREADHI             READ KEY                                     
         JNE   NO                  ERROR ON READ HIGH                           
         CLI   RCON8EID-RCON8ETP(R2),3    TYPE 3 RECORD?                        
         BE    LDOL0040            YES - PROCESS IT                             
         ZIC   RF,RCON8EID-RCON8ETP(R2)   NO  - BUMP PAST THIS KEY              
         LA    RF,1(RF)                                                         
         STC   RF,RCON8EID-RCON8ETP(R2)   RESTORE KEY FOR SKIP                  
         XC    RCON8EAG-RCON8ETP(10,R2),RCON8EAG-RCON8ETP(R2)                   
*                                  CLEAR OUT LOW KEY                            
         MVC   IOKEY(27),RCONREC                                                
         B     LDOL0020            GO BACK FOR TYPE 3                           
LDOL0040 EQU   *                                                                
         TM    DMCB+8,X'80'        EOF?                                         
         JO    YES                 YES                                          
         CLI   0(R2),X'8E'         STILL 8E KEY?                                
         JNE   YES                 NO  - FINISHED                               
LDOL0060 EQU   *                                                                
         CLC   DXU.RXUSTA,SPACES   STATION FILTER?                              
         JH    LDOL0080            YES - TEST THROUGH STATION                   
*                                  NO  - TEST THRU REP ONLY                     
         CLC   IOKEY(RCON8EST-RCONKEY),0(R2)                                    
         JNE   YES                 FINISHED                                     
         B     LDOL0100                                                         
LDOL0080 EQU   *                                                                
*                                  TEST THROUGH STATION                         
         CLC   IOKEY(RCON8EFS-RCONKEY),0(R2)                                    
         JNE   YES                 FINISHED                                     
         B     LDOL0100                                                         
LDOL0100 EQU   *                                                                
         L     R5,=A(STAXREC)      SET A(FAKE SPACEND TABLE)                    
*                                     FOR SETVAL IN RXROUTS                     
         CLC   4(5,R5),RCON8EST-RCONKEY(R2)                                     
         JE    LDOL0140            STATION DIDN'T CHANGE                        
*                                                                               
         OC    4(5,R5),4(R5)       FIRST TIME?                                  
         JZ    LDOL0120            YES                                          
*                                                                               
         CLC   DXU.RXUSTA,SPACES   STATION FILTER?                              
         JH    YES                 YES - WE MUST BE DONE                        
*                                                                               
LDOL0120 DS    0H                                                               
         MVC   WORK(L'IOKEY),0(R2)      SAVE THE CONTRACT KEY                   
K        USING RSTAKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,RCON8ERP-RCONKEY(R2)                                  
         MVC   K.RSTAKSTA,RCON8EST-RCONKEY(R2)                                  
         DROP  K                                                                
*                                                                               
         LA    R2,IO                                                            
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(L'RSTAKEY),0(R2)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
R        USING RSTAREC,R2                                                       
         MVC   00(2,R5),=X'0248'   SET UP FAKE SPACEND TABLE                    
         MVC   02(2,R5),R.RSTAKREP INSERT REP CODE                              
         MVC   04(5,R5),R.RSTAKSTA                                              
         MVC   09(3,R5),R.RSTASTRT                                              
         MVC   12(3,R5),R.RSTAEND                                               
         MVC   15(2,R5),R.RSTAKREP INSERT REP CODE                              
         MVC   17(20,R5),R.RSTAMKT                                              
         OC    17(20,R5),SPACES    SET X'00' TO SPACES                          
         MVC   37(2,R5),R.RSTAGRUP                                              
         MVC   39(3,R5),R.RSTAAFFL STATION AFFILIATE                            
         MVC   42(2,R5),=C'T '     DEFAULT NY TEAM CODE                         
         MVC   44(2,R5),R.RSTATVB  TVB REGION                                   
         MVC   46(3,R5),R.RSTAOWN  STATION OWNER                                
         MVC   49(1,R5),R.RSTARANK STATION MARKET RANK                          
         MVC   50(2,R5),=C'T '     DEFAULT CH TEAM CODE                         
         XC    52(2,R5),WORK+52    DISP OF LAST STA IN MKT                      
         MVC   68(2,R5),R.RSTACLDT INSERT CLOSE DATE OF STATION                 
         DROP  R                                                                
*                                                                               
         MVC   IOKEY,WORK          RESTORE CONTRACT                             
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             ERROR ON READ HIGH                           
         JNE   NO                                                               
*                                                                               
LDOL0140 DS    0H                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
*                                                                               
         GOTO1 AFILTDOL            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LDOL0360            SKIP THIS RECORD                             
         GOTO1 AGETIT              GET RECORD                                   
         JH    NO                  ERROR ON GETREC                              
         JL    LDOL0360                                                         
*                                                                               
         MVI   CONFPART,C'C'       SET TO 'CONFIRM'                             
*                                     (ORDER MAY NOT BE CONFIRMED:              
*                                       THIS IS JUST A FLAG)                    
         OC    CONTABCT,CONTABCT   ANY VALUE IN COUNT?                          
         BZ    LDOL0150            NO  - DON'T SEARCH                           
*                                                                               
         L     R4,ACONAREA         SET A(CONFIRM TABLE AREA)                    
         L     R3,CONTABCT         SET CONFIRM COUNT IN TABLE                   
*                                                                               
         GOTO1            ,DMCB,(0,RCONKCON),(R4),(R3),4,(0,4),(R3)             
*                                                                               
         GOTO1 =V(BINSRCH)                                                      
*                                  FIND CONFIRM IN TABLE                        
         CLI   DMCB,X'01'          CONFIRM FOUND?                               
         BE    LDOL0150            NO  - CAN BE PROCESSED                       
         MVI   CONFPART,C'P'       SET 'PARTIAL CONFIRM'                        
LDOL0150 DS    0H                                                               
*&&DO                                                                           
*   TEST                                                                        
R        USING RCONREC,R2                                                       
         CLC   =X'13869908',RCONKCON                                            
         BNE   TEST0400                                                         
         LA    RF,RCONREC                                                       
         LA    RE,IOKEY                                                         
         LA    R1,3                                                             
         LNR   R1,R1                                                            
         MVC   DIELDOL(2),=X'0000'                                              
         DROP  R                                                                
TEST0400 EQU   *                                                                
*   TEST END                                                                    
*&&                                                                             
*                                                                               
         CLI   DXU.RXPNDFC,C'Y'    INCLUDE PEND/FC/BB?                          
         BE    LDOL0160            YES - IGNORE BB FILTERING                    
*                                                                               
         TM    REPPFLG,X'40'       FILTER BACK BILLING?                         
         JZ    LDOL0160            NO                                           
*                                                                               
R        USING RCONREC,R2                                                       
         CLC   =C'ACC-BB',R.RCONBUYR     BACK BILLING?                          
         JE    LDOL0360            YES - TOSS RECORD                            
         DROP  R                                                                
*                                                                               
LDOL0160 DS    0H                                                               
         LR    RE,R2               POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         ICM   R0,3,RCONLEN                                                     
         AR    R0,RE               A(EOR)                                       
         LA    RE,RCONELEM-RCONREC(RE)                                          
*                                                                               
R        USING RCONELEM,RE                                                      
         TM    R.RCONMODR,X'10'                                                 
         JO    LDOL0220            BUY ADDED - KEEP RECORD                      
         DROP  R                                                                
*                                                                               
LDOL0180 CLI   0(RE),0             END OF RECORD - TOSS                         
         JE    LDOL0360                                                         
         CR    RE,R0               PAST EOR - BAIL OUT                          
         JNL   LDOL0360                                                         
         CLI   0(RE),X'03'         EST BUCKET - KEEP RECORD                     
         JE    LDOL0220                                                         
         CLI   0(RE),X'04'         INV BUCKET - KEEP RECORD                     
         JE    LDOL0220                                                         
*                                  SAR REQUIRED FOR P/F MONEY                   
         CLI   0(RE),X'06'         SPL ELEMENT?                                 
         JNE   LDOL0185            NO                                           
         USING RCONSPEL,RE         YES - CHECK FOR LOSS !!                      
         TM    RCONSPES,X'40'      NO REP'D DOLLARS SET?                        
         JNO   LDOL0200            NO  - SKIP THE ELEMENT                       
         J     LDOL0220            YES - KEEP THE RECORD                        
         DROP  RE                                                               
*                                                                               
LDOL0185 EQU   *                                                                
         CLI   DXU.RXPNDFC,C'Y'    INCLUDE PEND/FC/BB?                          
         JNE   *+12                NO - DON'T CHECK SAR ELEMENT                 
*                                                                               
*   THIS MAKES ABSOLUTELY NO SENSE.  THIS BRANCH DROPS RIGHT INTO               
*        THE 'TM' BELOW, WITHOUT EVEN FIGURING IF THE ELEMENT IS                
*        AN X'12'.  THE DATA COULD BE JUST ABOUT ANYTHING HERE.                 
*        THIS BRANCH WILL BE TAKEN IF THE USERPARAM IN COLUMN 40                
*        IS NOT A 'Y'.  ALL RUNS SO FAR HAVE BEEN WITH PARAM = Y.               
*                                                                               
         CLI   0(RE),X'12'         SAR ELEMENT ?                                
         JNE   LDOL0200            NO                                           
         USING RSARXEL,RE          YES - CHECK FOR TRUE FORECAST                
         TM    RSARXFLG,X'10'      'FORECAST?' FLAG SET?                        
*                                                                               
         DROP  RE                                                               
*                                                                               
*   FORECAST CONTROL                                                            
*                                                                               
***      JO    LDOL0360            YES - TOSS RECORD                            
LDOL0190 EQU   *                                                                
         J     LDOL0220            ACCEPT RECORD                                
LDOL0200 DS    0H                                                               
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         J     LDOL0180                                                         
*                                                                               
LDOL0220 DS    0H                                                               
         GOTO1 AINITDOL            INITIALISE EXTRACT BUFFER                    
         MVI   RESTART,RESTARTN    SET RESTART TO 'NO'                          
*                                                                               
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6),(0,(RA)),ARGBLOCK                           
*                                  GET NEXT UNCOMMITTED RECORD                  
*                                  P5 = 0 = LOAD PASS + A(ADDRESSD)             
*                                  P6 = ARGBLOCK:                               
*                                     A(DXBLOCKD)+A(IOKEY)+A(RESTART)           
LDOL0240 DS    0H                                                               
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6),(0,(RA)),ARGBLOCK                           
*                                  P5 = 0 = LOAD PASS +A(ADDRESSD)              
*                                  P6 = ARGBLOCK:                               
*                                     A(DXBLOCKD)+A(IOKEY)+A(RESTART)           
         CLI   8(R1),FF                                                         
         JE    LDOL0320            NO MORE RECORDS LEFT                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                DO NOT OVERRIDE REPCODE                      
         MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   LDOL0320            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   *+10                                                             
         MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    LDOL0300            DO NOT CONVERT RECORD                        
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   LDOL0244                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'CON'),VERSION                                              
         J     LDOL0290                                                         
*                                                                               
LDOL0244 DS    0H                                                               
         CLC   X.RECONTYP,T.REPCPTQ                                             
         JNE   LDOL0248                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'CPT'),VERSION                                              
         J     LDOL0290                                                         
LDOL0248 DS    0H                                                               
         CLC   X.RECONTYP,T.REPDPTQ                                             
         JNE   LDOL0252                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'DPT'),VERSION                                              
         J     LDOL0290                                                         
LDOL0252 DS    0H                                                               
         CLC   X.RECONTYP,T.REPLENQ                                             
         JNE   LDOL0256                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'LEN'),VERSION                                              
         J     LDOL0290                                                         
LDOL0256 DS    0H                                                               
         CLC   X.RECONTYP,T.REPBKSQ                                             
         JNE   LDOL0260                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'BKS'),VERSION                                              
         J     LDOL0290                                                         
LDOL0260 DS    0H                                                               
         CLC   X.RECONTYP,T.REPPFCQ                                             
         JNE   LDOL0270                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'PFC'),VERSION                                              
         J     LDOL0290                                                         
LDOL0270 DS    0H                                                               
         CLC   X.RECONTYP,T.REPSPCQ                                             
         JNE   LDOL0280                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'SPC'),VERSION                                              
         J     LDOL0290                                                         
LDOL0280 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
LDOL0290 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LDOL0300 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LDOL0240                                                         
*                                                                               
LDOL0320 DS    0H                                                               
*                                                                               
LDOL0360 EQU   *                                                                
*                                                                               
*   RETRIEVE NEXT CONTRACT X'8E' KEY                                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
         MVC   IOKEY(27),0(R2)     UPDATE IOKEY WITH NEXT KEY                   
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDOL0040                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CONTRACT DOLLAR RECORD                                       *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTDOL  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RCONREC,R2                                                       
*                                                                               
         ST    R2,ARGIOKEY         SAVE A(IOKEY) FOR RXROUTS CALL               
         ST    R7,ARGDXBLK         SAVE A(DXBLOCKD) FOR RXROUTS CALL            
         MVI   RESTART,RESTARTN    SET RESTART TO 'NO'                          
*                                     UPDATE RUN NEEDS NO RESTART               
*                                                                               
         CLI   0(R2),X'0C'         CONTRACT?                                    
         JNE   YES                 NO                                           
*                                                                               
         CLC   DXU.RXUSTA,SPACES   STATION FILTER?                              
         JNH   UDOL0010            NO                                           
         CLC   RCONKSTA,DXU.RXUSTA SAME STATION?                                
         JNE   YES                 NO MATCH: SKIP IT                            
*                                                                               
UDOL0010 EQU   *                                                                
         XC    IOKEY,IOKEY         BUILD FAKE 8E KEY FOR FILTER                 
K        USING RCONKEY,IOKEY                                                    
         MVI   K.RCON8ETP,X'8E'                                                 
         MVI   K.RCON8EID,X'03'                                                 
         MVC   K.RCON8EST,RCONKSTA                                              
         MVC   K.RCON8ERP,RCONKREP                                              
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(2,K.RCON8EFS)                         
         GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(2,K.RCON8EFE)                       
         MVC   K.RCON8ECN,RCONKCON                                              
         DROP  K                                                                
*                                                                               
         LA    R2,IOKEY                                                         
         GOTO1 AFILTDOL            FILTER IT                                    
         JNE   YES                 NO MATCH                                     
*                                                                               
         MVI   CONFPART,C'C'       SET TO 'CONFIRM'                             
*                                     (ORDER MAY NOT BE CONFIRMED:              
*                                       THIS IS JUST A FLAG)                    
         LA    R2,RECVHDR+L'RECVHDR                                             
         TM    REPPFLG,X'40'       FILTER BACK BILLING?                         
         JZ    UDOL0020            NO                                           
*                                                                               
         CLC   RCONBUYR,=C'ACC-BB'                                              
         JE    YES                                                              
*                                                                               
         OC    CONTABCT,CONTABCT   ANY VALUE IN COUNT?                          
         BZ    UDOL0020            NO  - DON'T SEARCH                           
*                                                                               
         L     R4,ACONAREA         SET A(CONFIRM TABLE AREA)                    
         L     R3,CONTABCT         SET CONFIRM COUNT IN TABLE                   
*                                                                               
         GOTO1            ,DMCB,(0,RCONKCON),(R4),(R3),4,(0,4),(R3)             
*                                                                               
         GOTO1 =V(BINSRCH)                                                      
*                                  FIND CONFIRM IN TABLE                        
         CLI   DMCB,X'01'          CONFIRM FOUND?                               
         BE    UDOL0020            NO  - CAN BE PROCESSED                       
         MVI   CONFPART,C'P'       SET TO 'PARTIAL CONFIRM'                     
UDOL0020 DS    0H                                                               
         LR    RE,R2               POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         ICM   R0,3,RCONLEN                                                     
         AR    R0,RE               A(EOR)                                       
         LA    RE,RCONELEM-RCONREC(RE)                                          
*                                                                               
         CLI   DXU.RXPNDFC,C'Y'    INCLUDE PEND/FC/BB?                          
         BE    UDOL0060            YES                                          
*                                                                               
R        USING RCONELEM,RE                                                      
         TM    R.RCONMODR,X'10'                                                 
         JO    UDOL0060            BUY ADDED - KEEP RECORD                      
         DROP  R                                                                
*                                                                               
UDOL0040 CLI   0(RE),0             END OF RECORD - TOSS                         
         JE    YES                                                              
         CR    RE,R0               PAST EOR - BAIL OUT                          
         JNL   YES                                                              
         CLI   0(RE),X'03'         EST BUCKET - KEEP RECORD                     
         JE    UDOL0060                                                         
         CLI   0(RE),X'04'         INV BUCKET - KEEP RECORD                     
         JE    UDOL0060                                                         
*                                                                               
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         J     UDOL0040                                                         
*                                                                               
UDOL0060 DS    0H                                                               
         L     R4,=A(STAXREC)      SET A(FAKE SPACEND TABLE                     
*                                     FOR SETVAL IN RXROUTS                     
         CLC   4(5,R4),RCONKSTA                                                 
         JE    UDOL0080            STATION DIDN'T CHANGE                        
*                                                                               
K        USING RSTAKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,RCONKREP                                              
         MVC   K.RSTAKSTA,RCONKSTA                                              
         DROP  K                                                                
*                                                                               
         LA    R2,IO                                                            
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(L'RSTAKEY),0(R2)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
R        USING RSTAREC,R2                                                       
         MVC   00(2,R4),=X'0248'                                                
         MVC   02(2,R4),REPALPHA    INSERT REP CODE                             
         MVC   04(5,R4),R.RSTAKSTA                                              
         MVC   09(3,R4),R.RSTASTRT                                              
         MVC   12(3,R4),R.RSTAEND                                               
         MVC   15(2,R4),REPALPHA    INSERT REP CODE                             
         MVC   17(20,R4),R.RSTAMKT                                              
         OC    17(20,R4),SPACES SET X'00' TO SPACES                             
         MVC   37(2,R4),R.RSTAGRUP                                              
         MVC   39(3,R4),R.RSTAAFFL STATION AFFILIATE                            
         MVC   42(2,R4),=C'T '   DEFAULT NY TEAM CODE                           
         MVC   44(2,R4),R.RSTATVB  TVB REGION                                   
         MVC   46(3,R4),R.RSTAOWN  STATION OWNER                                
         MVC   49(1,R4),R.RSTARANK STATION MARKET RANK                          
         MVC   50(2,R4),=C'T '   DEFAULT CH TEAM CODE                           
         XC    52(2,R4),WORK+52  DISP OF LAST STA IN MKT                        
         MVC   68(2,R4),R.RSTACLDT INSERT CLOSE DATE OF STATION                 
         DROP  R                                                                
*                                                                               
UDOL0080 DS    0H                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         L     R3,DXAXREC                                                       
         USING RECOND,R3                                                        
*                                                                               
         GOTO1 AINITDOL                                                         
*&&DO                                                                           
*   TESTSJ                                                                      
         CLC   =C'SJ',2(R2)        YES - SJ= REP?                               
         BNE   TESTSJ20            NO                                           
         CLC   =C'WCBS',6(R2)      YES - WCBS= STATION?                         
         BNE   TESTSJ20            NO                                           
         DC    H'0'                                                             
TESTSJ20 EQU   *                                                                
*   TEST END                                                                    
*&&                                                                             
         CLI   RECONACT,C'D'       DELETE?                                      
         JE    UDOL0100            YES- CONSIDER IT A 'CHANGE'                  
*                                                                               
         CLI   RECONACT,C'C'       CHANGE?                                      
         JNE   UDOL0280            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
UDOL0100 EQU   *                                                                
*                                  BUILD KILL RECORDS                           
*                                                                               
*   FIRST POINT TO 'COPY' RECORD RATHER THAN 'CHANGE' RECORD                    
*                                                                               
         L     R5,DXACPYB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,=A(STAXREC)),(R6),(1,(RA)),ARGBLOCK                           
*                                  P5 = 1 = UPDT PASS +A(ADDRESSD)              
*                                  P6 = ARGBLOCK:                               
*                                       A(DXBLOCKD)+A(IOKEY)                    
*                                                                               
*                                                                               
*   THEN  RESET TO 'CHANGE' RECORD RATHER THAN 'COPY' RECORD                    
*                                                                               
         L     R5,DXARECB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UDOL0120 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6),(1,(RA)),ARGBLOCK                           
*                                  P5 = 1 = UPDT PASS +A(ADDRESSD)              
*                                  P6 = ARGBLOCK:                               
*                                       A(DXBLOCKD)+A(IOKEY)                    
         CLI   8(R1),FF            FINISHED?                                    
         JE    UDOL0280                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                DO NOT OVERRIDE REPCODE                      
         MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   *+10                                                             
         MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UDOL0120            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RECONACT,C'K'       NOW ADD COPY RECORD DETAILS                  
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    UDOL0260            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   UDOL0140                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'CON'),VERSION                                           
         J     UDOL0250                                                         
*                                                                               
UDOL0140 DS    0H                                                               
         CLC   X.RECONTYP,T.REPCPTQ                                             
         JNE   UDOL0160                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'CPT'),VERSION                                              
         J     UDOL0250                                                         
UDOL0160 DS    0H                                                               
         CLC   X.RECONTYP,T.REPDPTQ                                             
         JNE   UDOL0180                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'DPT'),VERSION                                              
         J     UDOL0250                                                         
UDOL0180 DS    0H                                                               
         CLC   X.RECONTYP,T.REPLENQ                                             
         JNE   UDOL0200                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'LEN'),VERSION                                              
         J     UDOL0250                                                         
UDOL0200 DS    0H                                                               
         CLC   X.RECONTYP,T.REPBKSQ                                             
         JNE   UDOL0220                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'BKS'),VERSION                                              
         J     UDOL0250                                                         
UDOL0220 DS    0H                                                               
         CLC   X.RECONTYP,T.REPPFCQ                                             
         JNE   UDOL0230                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'PFC'),VERSION                                              
         J     UDOL0250                                                         
UDOL0230 DS    0H                                                               
         CLC   X.RECONTYP,T.REPSPCQ                                             
         JNE   UDOL0240                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'SPC'),VERSION                                              
         J     UDOL0250                                                         
UDOL0240 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',TYPENAME),VERSION                                          
UDOL0250 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UDOL0260 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UDOL0120                                                         
*                                                                               
UDOL0280 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6),(1,(RA)),ARGBLOCK                           
*                                  P5 = 1 = UPDT PASS +A(ADDRESSD)              
*                                  P6 = ARGBLOCK:                               
*                                       A(DXBLOCKD)+A(IOKEY)                    
*                                                                               
UDOL0300 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6),(1,(RA)),ARGBLOCK                           
*                                  P5 = 1 = UPDT PASS +A(ADDRESSD)              
*                                  P6 = ARGBLOCK:                               
*                                       A(DXBLOCKD)+A(IOKEY)                    
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                DO NOT OVERRIDE REPCODE                      
         MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   *+10                                                             
         MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UDOL0300            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RECONACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UDOL0440            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   UDOL0320                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'CON'),VERSION                                              
         J     UDOL0430                                                         
*                                                                               
****>>>>                                                                        
*                                                                               
UDOL0320 DS    0H                                                               
         CLC   X.RECONTYP,T.REPCPTQ                                             
         JNE   UDOL0340                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'CPT'),VERSION                                              
         J     UDOL0430                                                         
UDOL0340 DS    0H                                                               
         CLC   X.RECONTYP,T.REPDPTQ                                             
         JNE   UDOL0360                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'DPT'),VERSION                                              
         J     UDOL0430                                                         
UDOL0360 DS    0H                                                               
         CLC   X.RECONTYP,T.REPLENQ                                             
         JNE   UDOL0380                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'LEN'),VERSION                                              
         J     UDOL0430                                                         
UDOL0380 DS    0H                                                               
         CLC   X.RECONTYP,T.REPBKSQ                                             
         JNE   UDOL0400                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'BKS'),VERSION                                              
         J     UDOL0430                                                         
UDOL0400 DS    0H                                                               
         CLC   X.RECONTYP,T.REPPFCQ                                             
         JNE   UDOL0410                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'PFC'),VERSION                                              
         J     UDOL0430                                                         
UDOL0410 DS    0H                                                               
         CLC   X.RECONTYP,T.REPSPCQ                                             
         JNE   UDOL0420                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'SPC'),VERSION                                              
         J     UDOL0430                                                         
UDOL0420 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
UDOL0430 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UDOL0440 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UDOL0300                                                         
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CONTRACT RECORD ACTIVE KEY                                             
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTDOL  NTR1  BASE=*,LABEL=*                                                   
         USING RCONREC,R2                                                       
         CLC   RCON8ERP,REPALPHA   SAME REP?                                    
         JNE   NO                                                               
         CLI   RCON8ETP,X'8E'      SAME KEY?                                    
         JNE   NO                                                               
         CLI   RCON8EID,X'03'      ONLY CHECK THE 03 SUB KEY -                  
*                                     IT CONTAINS 'OFFICE CODE'                 
         JNE   NO                                                               
         TM    RCONKEY+27,X'80'    DELETED KEY?                                 
         JO    NO                                                               
         GOTO1 STAOFFLT            CHECK STATION OFFICE FILTER                  
         JNZ   NO                  CONVERTED STATION/OFFICE: SKIP IT            
*&&DO                                                                           
*   TEST                                                                        
         CLC   =X'13871032',RCON8ECN TEST SPECIAL CON #                         
         BNE   TCON0020                                                         
         L     RE,ASTNAREA                                                      
         LA    RF,1                                                             
         LNR   RF,RF                                                            
         DC    H'0'                                                             
TCON0020 EQU   *                                                                
*&&                                                                             
*                                                                               
         L     RE,=A(DATEBLK)                                                   
         USING DATEBLK,RE                                                       
*                                                                               
*   SELECT CURRENT YEAR ONLY                                                    
*                                                                               
         J     FLTDOL2             CURRENT YEAR ONLY                            
*                                                                               
*&&DO                                                                           
         CLC   RCON8EFE,PRIBST     CHECK FOR DATE OVERLAP W/PREVIOUS            
         JL    FLTDOL2                                                          
         CLC   RCON8EFS,PRIBND                                                  
         JH    FLTDOL2                                                          
         J     YES                                                              
*&&                                                                             
FLTDOL2  DS    0H                                                               
*                                                                               
         CLC   RCON8EFE,CURBST     CHECK FOR DATE OVERLAP W/CURRENT             
         JL    NO                                                               
         CLC   RCON8EFS,CURBND                                                  
         JH    NO                                                               
         J     YES                                                              
         LTORG                                                                  
         DROP  R2,RE                                                            
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* FILTER CONTRACT RECORD BY STATION OFFICE: CONVERTED TO MEDIA OCEAN            
*---------------------------------------------------------------------*         
         SPACE 1                                                                
STAOFFLT NTR1  BASE=*,LABEL=*                                                   
         USING RCONREC,R2                                                       
*                                                                               
*   STATION TABLE WILL CONTAIN ONLY STATIONS THAT HAVE HAD OFFICES              
*        CONVERTED TO MEDIA OCEAN.  IF THE STATION IS FOUND IN THE              
*        TABLE, THE OFFICE OF THE ORDER MUST BE SOUGHT IN THE                   
*        STATION'S OFFICE LIST.  IF FOUND, THE ORDER IS TO BE                   
*        BYPASSED.                                                              
*                                                                               
         MVC   WORK(5),RCON8EST    SET UP SEARCH PARAM                          
         L     R4,ASTNAREA         SET A(STATION TABLE AREA)                    
         L     R3,STATABCT         SET STATION COUNT IN TABLE                   
*                                                                               
         GOTO1            ,DMCB,(0,WORK),(R4),(R3),100,(0,5),(R3)               
*                                                                               
         GOTO1 =V(BINSRCH)                                                      
*                                  FIND STATION IN TABLE                        
         CLI   DMCB,X'01'          STATION FOUND?                               
         BE    STOF0060            NO  - CAN BE PROCESSED                       
*                                                                               
*   FOUND:  CHECK OFFICE OF ORDER AGAINST STATION'S OFFICES                     
*        TABLE ENTRY (COVERED BY DSTATD):                                       
*        BYTES 1 - 5   = STATION CALL LETTERS                                   
*        BYTES 6 - 85  = MAX OF 40 2-BYTE OFFICES CODES                         
*        BYTES 86-100  = SPARE                                                  
*                                                                               
         L     R4,DMCB             SET A(TABLE ENTRY)                           
         LA    R4,DDSTAOFF(R4)     DISPLACE TO OFFICES IN LIST                  
STOF0020 EQU   *                                                                
         CLC   0(2,R4),SPACES      OFFICE SLOT EMPTY?                           
         BNH   STOF0060            YES - OFFICE NOT IN LIST - PROCESS           
         CLC   RCON8EOF,0(R4)      NO  - OFFICE OF ORDER IN LIST?               
         BE    STOF0040            YES - SKIP THIS ORDER                        
         LA    R4,2(R4)            NO  - BUMP TO NEXT TABLE SLOT                
         B     STOF0020            GO BACK FOR NEXT                             
STOF0040 EQU   *                                                                
*                                                                               
*        STA|OFF FILTER: DROPPED                                                
*                                                                               
         B     STOF0080            GO BACK FOR NEXT CONTRACT                    
STOF0060 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO: PROCESS STATION               
         B     STOF0100                                                         
STOF0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO:  SKIP                       
STOF0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE CONTRACT HEADER/DOLLAR RECORD                            *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITDOL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RECONDL          R1=L'CON RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         SPACE 2                                                                
***********************************************************************         
* LOAD AGENCY RECORDS                                                           
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADAGY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RAGYREC,R2                                                       
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKTYP,X'1A'                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
LAGY0040 EQU   *                                                                
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(RAGYKAGY-RAGYKEY),0(R2)                                    
         JNE   YES                   ALL DONE IF TYPE CHANGES                   
*                                                                               
         MVC   REPADDR,28(R2)                                                   
*                                                                               
         GOTO1 AFILTAGY            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LAGY0180                                                         
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    NO                  ERROR ON GETREC                              
         JL    LAGY0180                                                         
*                                                                               
LAGY0060 DS    0H                                                               
*                                                                               
         GOTO1 AINITAGY            INITIALISE EXTRACT BUFFER                    
         GOTO1 VREPAGYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LAGY0080 DS    0H                                                               
         GOTO1 VREPAGYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF                                                         
         JE    LAGY0180            NO MORE RECORDS LEFT                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
*                                                                               
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   LAGY0180            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    LAGY0160            DO NOT CONVERT RECORD                        
*                                                                               
         CLC   X.RECONTYP,T.REPAGYQ                                             
         JNE   LAGY0100                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         J     LAGY0140                                                         
*                                                                               
LAGY0100 DS    0H                                                               
         CLC   X.RECONTYP,T.REPDAOQ                                             
         JNE   LAGY0110                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'DAO'),VERSION                                              
         J     LAGY0140                                                         
LAGY0110 DS    0H                                                               
         CLC   X.RECONTYP,T.REPACOQ                                             
         JNE   LAGY0120                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'ACO'),VERSION                                              
*                                                                               
         J     LAGY0140                                                         
LAGY0120 DS    0H                                                               
         DC    H'0'                UNRECOGNIZED RECORD TYPE ENCOUNTERED         
LAGY0140 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LAGY0160 EQU   *                                                                
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LAGY0080                                                         
*                                                                               
LAGY0180 DS    0H                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
*                                                                               
         JNZ   LAGY0040                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE AGENCY RECORD DATA                                                     
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTAGY  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RAGYREC,R2                                                       
*                                                                               
         GOTO1 AFILTAGY                                                         
         JNE   YES                                                              
*                                                                               
         L     R3,DXAXREC                                                       
         USING REAGYD,R3                                                        
*                                                                               
         GOTO1 AINITAGY                                                         
*                                                                               
         CLI   REAGYACT,C'D'       DELETE?                                      
         JE    UAGY0100            YES- CONSIDER IT A 'CHANGE'                  
*                                                                               
         CLI   REAGYACT,C'C'       CHANGE?                                      
         JNE   UAGY0280            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
UAGY0100 EQU   *                                                                
*                                  BUILD KILL RECORDS                           
*                                                                               
*   FIRST POINT TO 'COPY' RECORD RATHER THAN 'CHANGE' RECORD                    
*                                                                               
         L     R5,DXACPYB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UAGY0110 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPAGYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,=A(STAXREC)),(R6)                                             
*                                                                               
*                                                                               
*   THEN  RESET TO 'CHANGE' RECORD RATHER THAN 'COPY' RECORD                    
*                                                                               
         L     R5,DXARECB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UAGY0120 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPAGYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    UAGY0280                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REAGYD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UAGY0120            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   REAGYACT,C'K'       NOW ADD COPY RECORD DETAILS                  
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    UAGY0260            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.REAGYTYP,T.REPDAOQ                                             
         JNE   UAGY0130                                                         
*                                                                               
*                                                                               
*   TEST RETURN                                                                 
****>>>  MVC   DIE(2),=X'0000'                                                  
*   TEST END                                                                    
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'DAO'),VERSION                                           
         J     UAGY0240                                                         
*                                                                               
UAGY0130 DS    0H                                                               
*                                                                               
         CLC   X.REAGYTYP,T.REPACOQ                                             
         JNE   UAGY0140                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'ACO'),VERSION                                           
         J     UAGY0240                                                         
*                                                                               
UAGY0140 DS    0H                                                               
         CLC   X.REAGYTYP,T.REPAGYQ                                             
         JNE   UAGY0180                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         J     UAGY0240                                                         
UAGY0180 DS    0H                                                               
         DC    H'0'                UNRECOGNIZED REC TYPE ENCOUNTERED            
UAGY0240 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
DIE      EQU   *                                                                
*                                                                               
UAGY0260 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
         J     UAGY0120                                                         
*                                                                               
UAGY0280 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPAGYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                                                               
UAGY0300 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPAGYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REAGYD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UAGY0300            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   REAGYACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UAGY0360            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.REAGYTYP,T.REPDAOQ                                             
         JNE   UAGY0320                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'DAO'),VERSION                                              
         J     UAGY0340                                                         
*                                                                               
UAGY0320 DS    0H                                                               
*                                                                               
         CLC   X.REAGYTYP,T.REPACOQ                                             
         JNE   UAGY0330                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'ACO'),VERSION                                              
         J     UAGY0340                                                         
*                                                                               
UAGY0330 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         DROP  X,T                                                              
UAGY0340 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UAGY0360 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UAGY0300                                                         
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER AGY RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTAGY  NTR1  BASE=*,LABEL=*                                                   
         USING RAGYREC,R2                                                       
         CLC   RAGYKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RAGYKTYP,X'1A'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE AGENCY RECORD                                                      
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITAGY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REAGYDL          R1=L'AGY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD ADV RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADADV  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RADVREC,R2                                                       
         XC    RADVKEY,RADVKEY                                                  
         MVI   RADVKTYP,X'08'                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LADV02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RADVKADV-RADVKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE CHANGES                     
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPADVC,AINITADV,AFILTADV                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LADV02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE ADV RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTADV  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RADVREC,R2                                                       
*                                                                               
         GOTO1 AFILTADV                                                         
         JNE   YES                                                              
         GOTO1 AINITADV                                                         
         GOTO1 AACCUPDT,DMCB,VREPADVC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER ADV RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTADV  NTR1  BASE=*,LABEL=*                                                   
         USING RADVREC,R2                                                       
         CLC   RADVKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RADVKTYP,X'08'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE ADV RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITADV  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,READVDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD SAL RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADSAL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RSALREC,R2                                                       
         XC    RSALKEY,RSALKEY                                                  
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSAL02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RSALKSAL-RSALKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   WORK(L'IOKEY),0(R2)                                              
         MVC   IOKEY,0(R2)                                                      
         XC    IO(255),IO                                                       
*                                                                               
         LA    R2,IOKEY                                                         
         MVI   IOKEY,X'46'         SAL2 FOR EMAIL                               
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 AREADHI                                                          
         JNE   LSAL20                                                           
*                                                                               
         CLC   IOKEY(27),IOKEYSAV                                               
         JNE   LSAL20                                                           
*                                                                               
         LA    R2,IO                                                            
         MVC   REPADDR,IOKEY+28    SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   LSAL20                                                           
*                                                                               
LSAL20   DS    0H                                                               
         MVC   IOKEY,WORK          RESTORE KEY                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY,0(R2)                                                      
         JNE   NO                                                               
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPSALC,AINITSAL,AFILTSAL                         
*                                                                               
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSAL02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE SAL RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTSAL  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RSALREC,R2                                                       
*                                                                               
         GOTO1 AFILTSAL                                                         
         JNE   YES                                                              
         GOTO1 AINITSAL                                                         
*                                                                               
         XC    IO(255),IO                                                       
*                                                                               
         MVC   IOKEY,0(R2)                                                      
         LA    R2,IOKEY                                                         
         MVI   IOKEY,X'46'         SAL2 FOR EMAIL                               
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 AREADHI                                                          
         JNE   USAL20                                                           
*                                                                               
         CLC   IOKEY(27),IOKEYSAV                                               
         JNE   USAL20                                                           
*                                                                               
         LA    R2,IO                                                            
         MVC   REPADDR,IOKEY+28    SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   USAL20                                                           
*                                                                               
USAL20   DS    0H                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AACCUPDT,DMCB,VREPSALC,TYPECODE                                  
*                                                                               
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER SAL RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTSAL  NTR1  BASE=*,LABEL=*                                                   
         USING RSALREC,R2                                                       
         CLC   RSALKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RSALKTYP,X'06'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE SAL RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITSAL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RESALDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CTY RECORDS (CONTRACT TYPE)                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADCTY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RCTYREC,R2                                                       
         XC    RCTYKEY,RCTYKEY                                                  
         MVI   RCTYKTYP,X'32'                                                   
         MVC   RCTYKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCTY02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RCTYKCTY-RCTYKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPCTYC,AINITCTY,AFILTCTY                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCTY02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CTY RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTCTY  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RCTYREC,R2                                                       
*                                                                               
         GOTO1 AFILTCTY                                                         
         JNE   YES                                                              
         GOTO1 AINITCTY                                                         
         GOTO1 AACCUPDT,DMCB,VREPCTYC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CTY RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTCTY  NTR1  BASE=*,LABEL=*                                                   
         USING RCTYREC,R2                                                       
         CLC   RCTYKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RCTYKTYP,X'32'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE CTY RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITCTY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RECTYDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD DCT RECORDS (DEVELOPMENT CONTRACT TYPE)                                  
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADDCT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RDCTREC,R2                                                       
         XC    RDCTKEY,RDCTKEY                                                  
         MVI   RDCTKTYP,X'3B'                                                   
         MVC   RDCTKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDCT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RDCTKCTY-RDCTKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPDCTC,AINITDCT,AFILTDCT                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDCT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE DCT RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTDCT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RDCTREC,R2                                                       
*                                                                               
         GOTO1 AFILTDCT                                                         
         JNE   YES                                                              
         GOTO1 AINITDCT                                                         
         GOTO1 AACCUPDT,DMCB,VREPDCTC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER DCT RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTDCT  NTR1  BASE=*,LABEL=*                                                   
         USING RDCTREC,R2                                                       
         CLC   RDCTKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RDCTKTYP,X'3B'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE DCT RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITDCT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REDCTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD STA RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADSTA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RSTAREC,R2                                                       
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
LSTA0040 EQU   *                                                                
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RSTAKSTA-RSTAKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
*                                                                               
         GOTO1 AFILTSTA            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LSTA0180                                                         
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    NO                  ERROR ON GETREC                              
         JL    LSTA0180                                                         
*                                                                               
LSTA0060 DS    0H                                                               
*                                                                               
         GOTO1 AINITSTA            INITIALISE EXTRACT BUFFER                    
         GOTO1 VREPSTAC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LSTA0080 DS    0H                                                               
         GOTO1 VREPSTAC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF                                                         
         JE    LSTA0180            NO MORE RECORDS LEFT                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
*                                                                               
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   LSTA0180            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    LSTA0160            DO NOT CONVERT RECORD                        
*                                                                               
         CLC   X.RECONTYP,T.REPSTAQ                                             
         JNE   LSTA0100                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         J     LSTA0140                                                         
*                                                                               
LSTA0100 DS    0H                                                               
         CLC   X.RECONTYP,T.REPPIDQ                                             
         JNE   LSTA0110                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'PID'),VERSION                                              
         J     LSTA0140                                                         
LSTA0110 DS    0H                                                               
         CLC   X.RECONTYP,T.REPSOFQ                                             
         JNE   LSTA0112                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'SOF'),VERSION                                              
         J     LSTA0140                                                         
LSTA0112 DS    0H                                                               
         CLC   X.RECONTYP,T.REPCMSQ                                             
         JNE   LSTA0114                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'CMS'),VERSION                                              
         J     LSTA0140                                                         
LSTA0114 DS    0H                                                               
         CLC   X.RECONTYP,T.REPOTMQ                                             
         JNE   LSTA0116                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'OTM'),VERSION                                              
         J     LSTA0140                                                         
LSTA0116 DS    0H                                                               
         CLC   X.RECONTYP,T.REPSEMQ                                             
         JNE   LSTA0118                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'SEM'),VERSION                                              
         J     LSTA0140                                                         
LSTA0118 DS    0H                                                               
         DC    H'0'                UNRECOGNIZED RECORD TYPE ENCOUNTERED         
LSTA0140 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LSTA0160 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LSTA0080                                                         
*                                                                               
LSTA0180 DS    0H                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
*                                                                               
         JNZ   LSTA0040                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE STA RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTSTA  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RSTAREC,R2                                                       
*                                                                               
         GOTO1 AFILTSTA                                                         
         JNE   YES                                                              
*                                                                               
         L     R3,DXAXREC                                                       
         USING RESTAD,R3                                                        
*                                                                               
         GOTO1 AINITSTA                                                         
*                                                                               
         CLI   RESTAACT,C'D'       DELETE?                                      
         JE    USTA0100            YES- CONSIDER IT A 'CHANGE'                  
*                                                                               
         CLI   RESTAACT,C'C'       CHANGE?                                      
         JNE   USTA0280            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
USTA0100 EQU   *                                                                
*                                  BUILD KILL RECORDS                           
*                                                                               
*   FIRST POINT TO 'COPY' RECORD RATHER THAN 'CHANGE' RECORD                    
*                                                                               
         L     R5,DXACPYB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
USTA0110 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPSTAC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,=A(STAXREC)),(R6)                                             
*                                                                               
*                                                                               
*   THEN  RESET TO 'CHANGE' RECORD RATHER THAN 'COPY' RECORD                    
*                                                                               
         L     R5,DXARECB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
USTA0120 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPSTAC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    USTA0280                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RESTAD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   USTA0120            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RESTAACT,C'K'       NOW ADD COPY RECORD DETAILS                  
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    USTA0260            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.RESTATYP,T.REPPIDQ                                             
         JNE   USTA0130                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'PID'),VERSION                                           
         J     USTA0240                                                         
*                                                                               
USTA0130 DS    0H                                                               
*                                                                               
         CLC   X.RESTATYP,T.REPSOFQ                                             
         JNE   USTA0140                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'SOF'),VERSION                                           
         J     USTA0240                                                         
*                                                                               
USTA0140 DS    0H                                                               
         CLC   X.RESTATYP,T.REPSTAQ                                             
         JNE   USTA0150                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         J     USTA0240                                                         
USTA0150 DS    0H                                                               
         CLC   X.RESTATYP,T.REPCMSQ                                             
         JNE   USTA0160                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'CMS'),VERSION                                           
         J     USTA0240                                                         
USTA0160 DS    0H                                                               
         CLC   X.RESTATYP,T.REPOTMQ                                             
         JNE   USTA0170                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'OTM'),VERSION                                           
         J     USTA0240                                                         
USTA0170 DS    0H                                                               
         CLC   X.RESTATYP,T.REPSEMQ                                             
         JNE   USTA0180                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'SEM'),VERSION                                           
         J     USTA0240                                                         
USTA0180 DS    0H                                                               
         DC    H'0'                UNRECOGNIZED REC TYPE ENCOUNTERED            
USTA0240 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
USTA0260 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
         J     USTA0120                                                         
*                                                                               
USTA0280 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPSTAC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                                                               
USTA0300 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPSTAC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RESTAD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   USTA0300            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RESTAACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    USTA0360            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.RESTATYP,T.REPPIDQ                                             
         JNE   USTA0320                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'PID'),VERSION                                              
         J     USTA0340                                                         
*                                                                               
USTA0320 DS    0H                                                               
*                                                                               
         CLC   X.RESTATYP,T.REPSOFQ                                             
         JNE   USTA0324                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'SOF'),VERSION                                              
         J     USTA0340                                                         
*                                                                               
USTA0324 DS    0H                                                               
*                                                                               
         CLC   X.RESTATYP,T.REPCMSQ                                             
         JNE   USTA0326                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'CMS'),VERSION                                              
         J     USTA0340                                                         
*                                                                               
USTA0326 DS    0H                                                               
*                                                                               
         CLC   X.RESTATYP,T.REPOTMQ                                             
         JNE   USTA0328                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'OTM'),VERSION                                              
         J     USTA0340                                                         
*                                                                               
USTA0328 DS    0H                                                               
         CLC   X.RESTATYP,T.REPSEMQ                                             
         JNE   USTA0330                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'SEM'),VERSION                                              
         J     USTA0340                                                         
*                                                                               
USTA0330 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         DROP  X,T                                                              
USTA0340 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
USTA0360 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     USTA0300                                                         
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER STA RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTSTA  NTR1  BASE=*,LABEL=*                                                   
         USING RSTAREC,R2                                                       
         CLC   RSTAKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RSTAKTYP,X'02'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE STA RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITSTA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RESTADL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD GRP RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADGRP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RGRPREC,R2                                                       
         XC    RGRPKEY,RGRPKEY                                                  
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGRP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RGRPKGRP-RGRPKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPGRPC,AINITGRP,AFILTGRP                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGRP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE GRP RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTGRP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RGRPREC,R2                                                       
*                                                                               
         GOTO1 AFILTGRP                                                         
         JNE   YES                                                              
         GOTO1 AINITGRP                                                         
         GOTO1 AACCUPDT,DMCB,VREPGRPC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER GRP RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTGRP  NTR1  BASE=*,LABEL=*                                                   
         USING RGRPREC,R2                                                       
         CLC   RGRPKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RGRPKTYP,X'07'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE GRP RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITGRP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REGRPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***+++++++++>>>>  REGION                                                        
***********************************************************************         
* LOAD RGN RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADRGN  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RREGREC,R2                                                       
         XC    RREGKEY,RREGKEY                                                  
         MVI   RREGKTYP,X'03'                                                   
         MVC   RREGKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LRGN02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RREGKREG-RREGKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPRGNC,AINITRGN,AFILTRGN                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LRGN02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE RGN RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTRGN  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RREGREC,R2                                                       
*                                                                               
         GOTO1 AFILTRGN                                                         
         JNE   YES                                                              
         GOTO1 AINITRGN                                                         
         GOTO1 AACCUPDT,DMCB,VREPRGNC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER RGN RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTRGN  NTR1  BASE=*,LABEL=*                                                   
         USING RREGREC,R2                                                       
         CLC   RREGKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RREGKTYP,X'03'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE RGN RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITRGN  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RERGNDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***+++++++++>>>>  REGION                                                        
***=========>>>>  TERRITORY                                                     
***********************************************************************         
* LOAD TER RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADTER  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RTERREC,R2                                                       
         XC    RTERKEY,RTERKEY                                                  
         MVI   RTERKTYP,X'3D'                                                   
         MVC   RTERKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LTER02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RTERKTER-RTERKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPTERC,AINITTER,AFILTTER                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTER02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE TER RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTTER  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RTERREC,R2                                                       
*                                                                               
         GOTO1 AFILTTER                                                         
         JNE   YES                                                              
         GOTO1 AINITTER                                                         
         GOTO1 AACCUPDT,DMCB,VREPTERC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER TER RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTTER  NTR1  BASE=*,LABEL=*                                                   
         USING RTERREC,R2                                                       
         CLC   RTERKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RTERKTYP,X'3D'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE TER RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITTER  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RETERDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***=========>>>>  TERRITORY                                                     
***********************************************************************         
* LOAD DSP RECORDS (DEVELOPMENT SALESPERSON)                                    
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADDSP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RDSPREC,R2                                                       
         XC    RDSPKEY,RDSPKEY                                                  
         MVI   RDSPKTYP,X'3A'                                                   
         MVC   RDSPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDSP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RDSPKSAL-RDSPKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPDSPC,AINITDSP,AFILTDSP                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDSP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE DSP RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTDSP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RDSPREC,R2                                                       
*                                                                               
         GOTO1 AFILTDSP                                                         
         JNE   YES                                                              
         GOTO1 AINITDSP                                                         
         GOTO1 AACCUPDT,DMCB,VREPDSPC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER DSP RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTDSP  NTR1  BASE=*,LABEL=*                                                   
         USING RDSPREC,R2                                                       
         CLC   RDSPKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RDSPKTYP,X'3A'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE DSP RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITDSP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REDSPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD TEM RECORDS (TEAM)                                                       
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADTEM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RTEMREC,R2                                                       
         XC    RTEMKEY,RTEMKEY                                                  
         MVI   RTEMKTYP,X'05'                                                   
         MVC   RTEMKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LTEM02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RTEMKTEM-RTEMKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPTEMC,AINITTEM,AFILTTEM                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTEM02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE TEM RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTTEM  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RTEMREC,R2                                                       
*                                                                               
         GOTO1 AFILTTEM                                                         
         JNE   YES                                                              
         GOTO1 AINITTEM                                                         
         GOTO1 AACCUPDT,DMCB,VREPTEMC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER TEM RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTTEM  NTR1  BASE=*,LABEL=*                                                   
         USING RTEMREC,R2                                                       
         CLC   RTEMKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RTEMKTYP,X'05'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE TEM RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITTEM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RETEMDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRD RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADPRD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RPRDREC,R2                                                       
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPRD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RPRDKADV-RPRDKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPPRDC,AINITPRD,AFILTPRD                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPRD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE PRD RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTPRD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RPRDREC,R2                                                       
*                                                                               
         GOTO1 AFILTPRD                                                         
         JNE   YES                                                              
         GOTO1 AINITPRD                                                         
         GOTO1 AACCUPDT,DMCB,VREPPRDC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER PRD RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTPRD  NTR1  BASE=*,LABEL=*                                                   
         USING RPRDREC,R2                                                       
         CLC   RPRDKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RPRDKTYP,X'09'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE PRD RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITPRD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REPRDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CLS RECORDS (CLASS)                                                      
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADCLS  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RCLSREC,R2                                                       
         XC    RCLSKEY,RCLSKEY                                                  
         MVI   RCLSKTYP,X'0D'                                                   
         MVC   RCLSKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCLS02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RCLSKCLS-RCLSKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPCLSC,AINITCLS,AFILTCLS                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCLS02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CLS RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTCLS  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RCLSREC,R2                                                       
*                                                                               
         GOTO1 AFILTCLS                                                         
         JNE   YES                                                              
         GOTO1 AINITCLS                                                         
         GOTO1 AACCUPDT,DMCB,VREPCLSC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CLS RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTCLS  NTR1  BASE=*,LABEL=*                                                   
         USING RCLSREC,R2                                                       
         CLC   RCLSKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RCLSKTYP,X'0D'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE CLS RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITCLS  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RECLSDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CAT RECORDS (PRODUCT CATEGORY)                                           
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADCAT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RCTGREC,R2                                                       
         XC    RCTGKEY,RCTGKEY                                                  
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCAT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RCTGKCTG-RCTGKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPCATC,AINITCAT,AFILTCAT                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCAT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CAT RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTCAT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RCTGREC,R2                                                       
*                                                                               
         GOTO1 AFILTCAT                                                         
         JNE   YES                                                              
         GOTO1 AINITCAT                                                         
         GOTO1 AACCUPDT,DMCB,VREPCATC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CAT RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTCAT  NTR1  BASE=*,LABEL=*                                                   
         USING RCTGREC,R2                                                       
         CLC   RCTGKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RCTGKTYP,X'0F'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE CAT RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITCAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RECATDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD OFF RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADOFF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ROFFREC,R2                                                       
         XC    ROFFKEY,ROFFKEY                                                  
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOFF02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(ROFFKOFF-ROFFKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   WORK(L'IOKEY),0(R2)                                              
         MVC   IOKEY,0(R2)                                                      
         XC    IO(255),IO                                                       
*                                                                               
         LA    R2,IOKEY                                                         
         MVI   IOKEY,X'44'         SAL2 FOR EMAIL                               
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 AREADHI                                                          
         JNE   LOFF20                                                           
*                                                                               
         CLC   IOKEY(27),IOKEYSAV                                               
         JNE   LOFF20                                                           
*                                                                               
         LA    R2,IO                                                            
         MVC   REPADDR,IOKEY+28    SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   LOFF20                                                           
*                                                                               
LOFF20   DS    0H                                                               
         MVC   IOKEY,WORK          RESTORE KEY                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY,0(R2)                                                      
         JNE   NO                                                               
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPOFFC,AINITOFF,AFILTOFF                         
*                                                                               
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOFF02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE OFF RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTOFF  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ROFFREC,R2                                                       
*                                                                               
         GOTO1 AFILTOFF                                                         
         JNE   YES                                                              
         GOTO1 AINITOFF                                                         
*                                                                               
         XC    IO(255),IO                                                       
*                                                                               
         MVC   IOKEY,0(R2)                                                      
         LA    R2,IOKEY                                                         
         MVI   IOKEY,X'44'         OFF2 FOR FAX, NAT/LOCAL                      
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 AREADHI                                                          
         JNE   UOFF20                                                           
*                                                                               
         CLC   IOKEY(27),IOKEYSAV                                               
         JNE   UOFF20                                                           
*                                                                               
         LA    R2,IO                                                            
         MVC   REPADDR,IOKEY+28    SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   UOFF20                                                           
*                                                                               
UOFF20   DS    0H                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AACCUPDT,DMCB,VREPOFFC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER OFF RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTOFF  NTR1  BASE=*,LABEL=*                                                   
         USING ROFFREC,R2                                                       
         CLC   ROFFKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   ROFFKTYP,X'04'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE OFF RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITOFF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REOFFDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD OWN RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADOWN  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ROWNREC,R2                                                       
         XC    ROWNKEY,ROWNKEY                                                  
         MVI   ROWNKTYP,X'2A'                                                   
         MVC   ROWNKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOWN02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(ROWNKOWN-ROWNKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPOWNC,AINITOWN,AFILTOWN                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOWN02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE OWN RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTOWN  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ROWNREC,R2                                                       
*                                                                               
         GOTO1 AFILTOWN                                                         
         JNE   YES                                                              
         GOTO1 AINITOWN                                                         
         GOTO1 AACCUPDT,DMCB,VREPOWNC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER OWN RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTOWN  NTR1  BASE=*,LABEL=*                                                   
         USING ROWNREC,R2                                                       
         CLC   ROWNKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   ROWNKTYP,X'2A'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE OWN RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITOWN  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REOWNDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD MKT RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADMKT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RMKTREC,R2                                                       
         XC    RMKTKEY,RMKTKEY                                                  
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMKT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RMKTKMKT-RMKTKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPMKTC,AINITMKT,AFILTMKT                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMKT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE MKT RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTMKT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RMKTREC,R2                                                       
*                                                                               
         GOTO1 AFILTMKT                                                         
         JNE   YES                                                              
         GOTO1 AINITMKT                                                         
         GOTO1 AACCUPDT,DMCB,VREPMKTC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER MKT RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTMKT  NTR1  BASE=*,LABEL=*                                                   
         USING RMKTREC,R2                                                       
         CLC   RMKTKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RMKTKTYP,X'2B'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE MKT RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITMKT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REMKTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD MST RECORDS (SEE-ME/READ-ME MASTER/SUB RECORDS                           
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADMST  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RREPREC,R2                                                       
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMST02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(27),0(R2)                                                  
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPMSTC,AINITMST,AFILTMST                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMST02                                                           
         J     YES                                                              
*                                                                               
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE MST RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTMST  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RREPREC,R2                                                       
*                                                                               
         GOTO1 AFILTMST                                                         
         JNE   YES                                                              
         GOTO1 AINITMST                                                         
         GOTO1 AACCUPDT,DMCB,VREPMSTC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER MST RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTMST  NTR1  BASE=*,LABEL=*                                                   
         USING RREPREC,R2                                                       
         CLC   RREPKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RREPKTYP,X'01'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE MKT RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITMST  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REMSTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD REP RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADREP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RREPREC,R2                                                       
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LREP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(27),0(R2)                                                  
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPREPC,AINITREP,AFILTREP                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LREP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE REP RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTREP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RREPREC,R2                                                       
*                                                                               
         GOTO1 AFILTREP                                                         
         JNE   YES                                                              
         GOTO1 AINITREP                                                         
         GOTO1 AACCUPDT,DMCB,VREPREPC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER REP RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTREP  NTR1  BASE=*,LABEL=*                                                   
         USING RREPREC,R2                                                       
         CLC   RREPKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RREPKTYP,X'01'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE REP RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITREP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REREPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***>>>                                                                          
**BD1**>>>                                                                      
***********************************************************************         
* LOAD STATION/OFFICE BUDGET RECORDS                                            
*---------------------------------------------------------------------*         
LOADBD1  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST BUD RECORD             
         USING RBUDREC,R2                                                       
         XC    RBUDKEY,RBUDKEY                                                  
*                                                                               
         MVI   RBUDKTYP,X'13'                                                   
         MVC   RBUDKREP,REPALPHA                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             ERROR ON READ HIGH                           
         JNE   NO                                                               
*                                                                               
LBD10010 DS    0H                                                               
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(RBUDKYR-RBUDKEY),0(R2)                                     
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AFILTBD1            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LBD10120                                                         
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    NO                  ERROR ON GETREC                              
         JL    LBD10120                                                         
*                                                                               
LBD10020 DS    0H                                                               
         GOTO1 AINITBD1            INITIALISE EXTRACT BUFFER                    
         GOTO1 VREPBD1C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,0),(R6)                                                       
*                                  GET NEXT UNCOMMITTED RECORD                  
LBD10040 DS    0H                                                               
         GOTO1 VREPBD1C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,0),(R6)                                                       
*                                                                               
         CLI   8(R1),FF                                                         
         JE    LBD10120            NO MORE RECORDS LEFT                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REBD1D,RF                                                        
*                                                                               
***      CLC   DXU.RXUOVREP,SPACES                                              
***      JE    *+10                DO NOT OVERRIDE REPCODE                      
***      MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   LBD10120            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
***      CLC   X.RECONTYP,T.REPCONQ                                             
***      JNE   *+10                                                             
***      MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    LBD10100            DO NOT CONVERT RECORD                        
*                                                                               
         CLC   X.REBD1TYP,T.REPBD1Q                                             
         JNE   LBD10060                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'BD1'),VERSION                                              
         J     LBD10080                                                         
*                                                                               
LBD10060 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
LBD10080 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LBD10100 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LBD10040                                                         
*                                                                               
LBD10120 DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LBD10010            GO BACK AND PROCESS THIS RECORD              
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE STATION/OFFICE BUDGET RECORD                                 *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTBD1  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RBUDREC,R2                                                       
         CLI   0(R2),X'13'         STATION BUDGET?                              
         JNE   YES                 NO                                           
*                                                                               
         XC    IOKEY,IOKEY         BUILD 13 KEY FOR FILTER                      
K        USING RBUDKEY,IOKEY                                                    
         MVI   K.RBUDKTYP,X'13'                                                 
         MVC   K.RBUDKREP,RBUDKREP                                              
         DROP  K                                                                
*                                                                               
         LA    R2,IOKEY                                                         
         GOTO1 AFILTBD1            FILTER IT                                    
         JNE   YES                 NO MATCH                                     
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
         LR    RE,R2               POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         ICM   R0,3,RBUDLEN                                                     
         AR    R0,RE               A(EOR)                                       
         LA    RE,RBUDELEM-RBUDREC(RE)                                          
*                                                                               
         L     R3,DXAXREC                                                       
         USING REBD1D,R3                                                        
*                                                                               
         GOTO1 AINITBD1                                                         
*                                                                               
         CLI   REBD1ACT,C'C'       CHANGE?                                      
         JNE   UBD10100            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
*                                  BUILD KILL RECORDS                           
*                                                                               
*   FIRST POINT TO 'COPY' RECORD RATHER THAN 'CHANGE' RECORD                    
*                                                                               
         L     R5,DXACPYB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 VREPBD1C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,0),(R6)                                                       
*                                                                               
*   THEN  RESET TO 'CHANGE' RECORD RATHER THAN 'COPY' RECORD                    
*                                                                               
         L     R5,DXARECB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UBD10020 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPBD1C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,0),(R6)                                                       
         CLI   8(R1),FF            FINISHED?                                    
         JE    UBD10100                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REBD1D,RF                                                        
*                                                                               
****     CLC   DXU.RXUOVREP,SPACES                                              
****     JE    *+10                DO NOT OVERRIDE REPCODE                      
****     MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
***      CLC   X.RECONTYP,T.REPCONQ                                             
***      JNE   *+10                                                             
***      MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UBD10020            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   REBD1ACT,C'K'       NOW ADD COPY RECORD DETAILS                  
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    UBD10080            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.REBD1TYP,T.REPBD1Q                                             
         JNE   UBD10040                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'BD1'),VERSION                                           
         J     UBD10060                                                         
*                                                                               
UBD10040 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',TYPENAME),VERSION                                          
UBD10060 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UBD10080 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UBD10020                                                         
*                                                                               
UBD10100 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPBD1C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,0),(R6)                                                       
*                                                                               
UBD10120 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPBD1C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,0),(R6)                                                       
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REBD1D,RF                                                        
*                                                                               
****     CLC   DXU.RXUOVREP,SPACES                                              
***      JE    *+10                DO NOT OVERRIDE REPCODE                      
****     MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
***      CLC   X.RECONTYP,T.REPCONQ                                             
***      JNE   *+10                                                             
***      MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UBD10120            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   REBD1ACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UBD10180            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.REBD1TYP,T.REPBD1Q                                             
         JNE   UBD10140                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'BD1'),VERSION                                              
         J     UBD10160                                                         
*                                                                               
UBD10140 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
UBD10160 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UBD10180 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UBD10120                                                         
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER STATION/OFFICE BUDGET RECORD ACTIVE KEY                                
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTBD1  NTR1  BASE=*,LABEL=*                                                   
         USING RBUDREC,R2                                                       
*                                                                               
         CLC   RBUDKREP,REPALPHA   SAME REP?                                    
         JNE   NO                                                               
         CLI   RBUDKTYP,X'13'      SAME KEY?                                    
         JNE   NO                                                               
         TM    RBUDKEY+27,X'80'    DELETED KEY?                                 
         JO    NO                                                               
         J     YES                 ACCEPT - PASSES TESTS                        
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE CONTRACT HEADER/DOLLAR RECORD                            *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITBD1  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REBD1DL          R1=L'CON RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***>>>                                                                          
***********************************************************************         
* LOAD OFFICE BUDGET (X'19') RECORDS                                            
*---------------------------------------------------------------------*         
**BD2**>>>                                                                      
LOADBD2  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CON RECORD             
         USING ROBDREC,R2                                                       
         XC    ROBDKEY,ROBDKEY                                                  
*                                                                               
         MVI   ROBDKTYP,X'19'                                                   
         MVC   ROBDKREP,REPALPHA                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             ERROR ON READ HIGH                           
         JNE   NO                                                               
*                                                                               
LBD20010 DS    0H                                                               
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(ROBDKYR-ROBDKEY),0(R2)                                     
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AFILTBD2            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LBD20120                                                         
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    NO                  ERROR ON GETREC                              
         JL    LBD20120                                                         
*                                                                               
LBD20020 DS    0H                                                               
         GOTO1 AINITBD2            INITIALISE EXTRACT BUFFER                    
         GOTO1 VREPBD2C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,0),(R6)                                                       
*                                  GET NEXT UNCOMMITTED RECORD                  
LBD20040 DS    0H                                                               
         GOTO1 VREPBD2C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,0),(R6)                                                       
         CLI   8(R1),FF                                                         
         JE    LBD20120            NO MORE RECORDS LEFT                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REBD2D,RF                                                        
*                                                                               
***      CLC   DXU.RXUOVREP,SPACES                                              
***      JE    *+10                DO NOT OVERRIDE REPCODE                      
***      MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   LBD20120            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
***      CLC   X.RECONTYP,T.REPCONQ                                             
***      JNE   *+10                                                             
***      MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    LBD20100            DO NOT CONVERT RECORD                        
*                                                                               
         CLC   X.REBD2TYP,T.REPBD2Q                                             
         JNE   LBD20060                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'BD2'),VERSION                                              
         J     LBD20080                                                         
*                                                                               
LBD20060 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
LBD20080 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LBD20100 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LBD20040                                                         
*                                                                               
LBD20120 DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LBD20010            GO BACK AND PROCESS THIS RECORD              
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE STATION/OFFICE BUDGET RECORD                                 *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTBD2  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ROBDREC,R2                                                       
         CLI   0(R2),X'19'         OFFICE  BUDGET?                              
         JNE   YES                 NO                                           
*                                                                               
         XC    IOKEY,IOKEY         BUILD 19 KEY FOR FILTER                      
K        USING ROBDKEY,IOKEY                                                    
         MVI   K.ROBDKTYP,X'19'                                                 
         MVC   K.ROBDKREP,ROBDKREP                                              
         DROP  K                                                                
*                                                                               
         LA    R2,IOKEY                                                         
         GOTO1 AFILTBD2            FILTER IT                                    
         JNE   YES                 NO MATCH                                     
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
         LR    RE,R2               POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         ICM   R0,3,ROBDLEN                                                     
         AR    R0,RE               A(EOR)                                       
         LA    RE,ROBDELEM-ROBDREC(RE)                                          
*                                                                               
         L     R3,DXAXREC                                                       
         USING REBD2D,R3                                                        
*                                                                               
         GOTO1 AINITBD2                                                         
*                                                                               
         CLI   REBD2ACT,C'C'       CHANGE?                                      
         JNE   UBD20100            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
*                                  BUILD KILL RECORDS                           
*                                                                               
*   FIRST POINT TO 'COPY' RECORD RATHER THAN 'CHANGE' RECORD                    
*                                                                               
         L     R5,DXACPYB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 VREPBD2C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,0),(R6)                                                       
*                                                                               
*   THEN  RESET TO 'CHANGE' RECORD RATHER THAN 'COPY' RECORD                    
*                                                                               
         L     R5,DXARECB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UBD20020 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPBD2C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,0),(R6)                                                       
         CLI   8(R1),FF            FINISHED?                                    
         JE    UBD20100                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REBD2D,RF                                                        
*                                                                               
****     CLC   DXU.RXUOVREP,SPACES                                              
****     JE    *+10                DO NOT OVERRIDE REPCODE                      
****     MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
***      CLC   X.RECONTYP,T.REPCONQ                                             
***      JNE   *+10                                                             
***      MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UBD20020            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   REBD2ACT,C'K'       NOW ADD COPY RECORD DETAILS                  
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    UBD20080            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.REBD2TYP,T.REPBD2Q                                             
         JNE   UBD20040                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'BD2'),VERSION                                           
         J     UBD20060                                                         
*                                                                               
UBD20040 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',TYPENAME),VERSION                                          
UBD20060 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UBD20080 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UBD20020                                                         
*                                                                               
UBD20100 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPBD2C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,0),(R6)                                                       
*                                                                               
UBD20120 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPBD2C,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,0),(R6)                                                       
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REBD2D,RF                                                        
*                                                                               
****     CLC   DXU.RXUOVREP,SPACES                                              
***      JE    *+10                DO NOT OVERRIDE REPCODE                      
****     MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
***      CLC   X.RECONTYP,T.REPCONQ                                             
***      JNE   *+10                                                             
***      MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UBD20120            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   REBD2ACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UBD20180            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.REBD2TYP,T.REPBD2Q                                             
         JNE   UBD20140                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'BD2'),VERSION                                              
         J     UBD20160                                                         
*                                                                               
UBD20140 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
UBD20160 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UBD20180 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UBD20120                                                         
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER STATION/OFFICE BUDGET RECORD ACTIVE KEY                                
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTBD2  NTR1  BASE=*,LABEL=*                                                   
         USING ROBDREC,R2                                                       
*                                                                               
         CLC   ROBDKREP,REPALPHA   SAME REP?                                    
         JNE   NO                                                               
         CLI   ROBDKTYP,X'19'      SAME KEY?                                    
         JNE   NO                                                               
         TM    ROBDKEY+27,X'80'    DELETED KEY?                                 
         JO    NO                                                               
         J     YES                 ACCEPT - PASSES TESTS                        
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE CONTRACT HEADER/DOLLAR RECORD                            *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITBD2  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REBD2DL          R1=L'CON RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
**BD2**>>>                                                                      
**PTP**<<<                                                                      
***********************************************************************         
* LOAD POINT PERSON RECORDS                                                     
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADPTP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RPTPREC,R2                                                       
         XC    RPTPKEY,RPTPKEY                                                  
         MVI   RPTPKTYP,X'31'                                                   
         MVC   RPTPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPTP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
*   TEST DUMP                                                                   
**       DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(RPTPKREC-RPTPKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AACCLOAD,DMCB,VREPPTPC,AINITPTP,AFILTPTP                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPTP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE POINT PERSON RECORD DATA                                               
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTPTP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RPTPREC,R2                                                       
*                                                                               
         GOTO1 AFILTPTP                                                         
         JNE   YES                                                              
         GOTO1 AINITPTP                                                         
         GOTO1 AACCUPDT,DMCB,VREPPTPC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER POINT PERSON RECORD AT R2                                    *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTPTP  NTR1  BASE=*,LABEL=*                                                   
         USING RPTPREC,R2                                                       
         CLC   RPTPKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RPTPKTYP,X'31'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE SAL RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITPTP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REPTPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
**PTP**<<<                                                                      
**SET**<<<                                                                      
***********************************************************************         
* LOAD SET RECORDS                                                    *         
*        ROUTINE WILL BUILD BOTH SET HEADER RECORDS    AND            *         
*        SET CODE RECORDS        AT THE SAME TIME                     *         
*    SET RECORD PROCESSING IS UNLIKE THAT OF ANY OTHER RECORD.  SETS  *         
*        FOR MASTER RECORDS EXIST AT THE MASTER LEVEL, WHILE SETS     *         
*        FOR SUBSIDIARY RECORD TYPES EXIST AT THE SUB LEVEL AS WELL   *         
*        AS THE MASTER LEVEL.                                         *         
*    TO GET A COMPLETE SET OF SET RECORDS FOR BOTH MASTER AND SUBSIDS,*         
*        SPECIAL CONSIDERATIONS ARE MADE                              *         
*    MASTER:  ALL SET RECORDS THAT EXIST AT THE MASTER LEVEL.  THIS   *         
*        WILL INCLUDE NON-MASTER SET RECORDS, BECAUSE THESE HAVE BEEN *         
*        ENTERED TO FACILITATE CROSS-COMPANY REPORT ENTERED AT THE    *         
*        MASTER LEVEL SPECIFYING A NON-MASTER SET TYPE.               *         
*    SUB:     ALL SET RECORDS FOR **MASTER** RECORD TYPES, AS WELL    *         
*        AS NON-MASTER RECORD TYPE SET RECORDS FOR THE SUBSIDIARY.    *         
*    THIS WILL RESULT IN MASTER-LEVEL SET RECORDS BEING USED MORE     *         
*        THAN ONCE.  ALL SET RECORDS WILL BE CREATED AT THE SUB LEVEL.*         
*                                                                     *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADSET  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVC   SETALPHA,REPALPHA                                                
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST SET RECORD             
         USING RSETREC,R2                                                       
         XC    RSETKEY,RSETKEY                                                  
*                                                                               
         MVI   RSETKTYP,X'38'      READ ALL SET RECORDS ON FILE                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             ERROR ON READ HIGH                           
         JNE   SETNO                                                            
*                                                                               
LSET0020 TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    SETYES                                                           
         CLC   IOKEY(RSETKREP-RSETKEY),0(R2)                                    
         JNE   SETYES              ALL DONE IF TYPE CHANGES                     
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AFILTSET            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LSET0320                                                         
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    SETNO               ERROR ON GETREC                              
         JL    LSET0320                                                         
*                                                                               
***********************************************************************         
*  PROCESSING LOGIC:  SET OF SETS                                     *         
*     REGULAR SET TURNS SETFLAG TO 'N'                                *         
*        1.  CURRENT RECORD KEY IS SAVED OFF                          *         
*        2.  SET OF SETS TURNS SETFLAG TO 'H' (HEADER) FIRST          *         
*        3.  MEMBER SAVE AREA IS CLEARED                              *         
*        4.  MEMBER ELEMENT FROM SET OF SETS IS SAVED OFF             *         
*        5.  ANEXTSET IS SET TO FIRST ENTRY IN SAVE AREA              *         
*        6.  PROCESSING ROUTINE IS CALLED FOR SET OF SETS RECORD      *         
*            A.  WILL PRODUCE ONLY A HEADER 05622 RECORD              *         
*            B.  RXROUTS CONTROLS PROCESSING WITH SETFLAG = H         *         
*        7.  RETURN WILL LOOK FOR ANOTHER ENTRY IN SAVE AREA          *         
*        8.  MORE DATA AVAILABLE:                                     *         
*            A.  SET RECORD KEY IS BUILT                              *         
*            B.  SET RECORD IS RETRIEVED                              *         
*            C.  SETFLAG IS SET TO 'D' (DETAILS)                      *         
*            D.  ANEXTSET IS BUMPED TO NEXT SLOT                      *         
*            E.  PROCESSING ROUTINE IS CALLED FOR DETAILS RECORD      *         
*            F.  WILL PRODUCE DETAIL 05623 RECORDS                    *         
*            G.  RXROUTS CONTROLS PROCESSING WITH SETFLAG = D         *         
*            H.  RETURN TO # 7, ABOVE                                 *         
*        9.  NO MORE DATA AVAILABLE:                                  *         
*            A.  RESTORE SAVED KEY                                    *         
*            B.  RETURN RECORD TO RESTORE SEQUENCE OF DATA            *         
***********************************************************************         
*                                                                               
         LR    RE,R2               SET A(RECORD)                                
         SR    R0,R0                                                            
         ICM   R0,3,RSETLEN        SET A(END OF RECORD)                         
         AR    R0,RE               A(EOR)                                       
         LA    RE,RSETELEM-RSETREC(RE)                                          
*                                  POINT TO FIRST ELEMENT                       
R        USING RSETELEM,RE                                                      
         MVI   SETFLAG,C'N'        TURN ON 'NOT SET OF SETS'                    
         MVC   SETNAME,SPACES      CLEAR SETNAME OF SET OF SETS                 
         XC    SETCTR,SETCTR                                                    
         CLI   RSET1DES,1          BASIC INFORMATION ELEMENT?                   
         BNE   LSET0100            NO  -                                        
         TM    RSET1FLG,X'80'      SET OF SETS?                                 
         BNO   LSET0100            NO                                           
         DROP  R                                                                
         MVC   SETNAME,RSETKID     SAVE SETNAME OF SET OF SETS                  
         MVI   SETFLAG,SETHDR      YES - TURN ON 'PROCESS HEADER'               
         XC    TESTCTR,TESTCTR     CLEAR TEST SET COUNTER                       
         XC    SETOFSET,SETOFSET   CLEAR SAVE AREA                              
         LA    R1,SETOFSET         SET A(1ST ENTRY IN AREA)                     
         ST    R1,ANEXTSET         SET A(NEXT SLOT)                             
         MVC   SETKEY,0(R2)        SAVE SET KEY FOR RESTART                     
LSET0040 EQU   *                                                                
         ZIC   RF,1(RE)            FIND X'20' MEMBERS ELEMENT                   
         AR    RE,RF                                                            
         CLI   0(RE),0             END OF RECORD?                               
         BE    LSET0100            YES                                          
         CLI   0(RE),X'20'         MEMBERS ELEMENT?                             
         BNE   LSET0040            NO  - GO BACK FOR NEXT                       
         ZIC   RF,1(RE)            GET ELEMENT LENGTH                           
*                                                                               
*   SUBTRACT 2 FOR CONTROL, 1 FOR 'LENGTH OF MEMBER FIELD', 1 FOR EX            
*                                                                               
         SH    RF,=H'4'            SUBTRACT FOR MOVE                            
         C     RF,=F'119'          30X4 ENTRIES MAX ALLOWED                     
         BNH   LSET0060            OKAY                                         
         LA    RF,119              TOO MUCH: SET MAX                            
LSET0060 EQU   *                                                                
         EX    RF,LSET0080         MOVE MEMBERS BY LENGTH                       
         B     LSET0100                                                         
LSET0080 MVC   SETOFSET(0),3(RE)                                                
LSET0100 EQU   *                                                                
*                                                                               
LSET0120 DS    0H                                                               
         GOTO1 AINITSET            INITIALISE EXTRACT BUFFER                    
         GOTO1 VREPSETC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(SETFLAG,(R6)),SETCTR                            
KILLIT   EQU   *                                                                
*&&DO                                                                           
*                                                                               
   TO BUILD A BUFFER OF RECORDS FOR A SET, OR A SET OF SETS                     
*                                                                               
         DXAXREC     ->    EXTRACT RECORD                                       
         REPPFLG     ->    ?                                                    
         R2          ->    A(SET RECORD)                                        
         P3/BYTE 1   ->    ROUTINE CONTROL INDICATOR                            
         P3/2 - 4    ->    FAKE SPACEND                                         
         P4/BYTE 1   =>    SETFLAG INDICATOR : SET OF SETS USE                  
         P4/2 - 4    ->    R6 = ??                                              
*&&                                                                             
*                                                                               
*                                  GET NEXT UNCOMMITTED RECORD                  
LSET0140 DS    0H                                                               
*                                                                               
         GOTO1 VREPSETC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(SETFLAG,(R6)),SETCTR                            
         CLI   8(R1),FF                                                         
         JNE   LSET0150            MORE RECORDS TO PROCESS                      
         CLI   SETFLAG,SETDET      PROCESSING SET DETAIL?                       
         JNE   LSET0220            NO                                           
         L     RF,ANEXTSET         YES - NEED TO BUMP TABLE ADDR                
         LA    RF,4(RF)                                                         
         ST    RF,ANEXTSET                                                      
         J     LSET0220            NO MORE RECORDS LEFT                         
*                                                                               
LSET0150 DS    0H                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RESTND,RF                                                        
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                DO NOT OVERRIDE REPCODE                      
         MVC   X.RESTNREP,DXU.RXUOVREP                                          
*                                                                               
*   WHAT DOES THIS OPTION MEAN?                                                 
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   LSET0220            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
*   A CHANGE FROM CONTRACT HEADER TO CONTRACT DOLLARS RESULTS IN                
*        INSERTINON OF MASTER CODE?  I DON'T UNDERSTAND THIS.                   
*                                                                               
***      CLC   X.RESTNTYP,T.REPCONQ                                             
***      JNE   *+10                                                             
***      MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    LSET0200            DO NOT CONVERT RECORD                        
*                                                                               
*   CHECK CONVERT: HEADER VS DETAILS:  HOW DIFFERENT?                           
*                                                                               
         CLC   X.RESTNTYP,T.REPSTNQ                                             
         JNE   LSET0160                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'STN'),VERSION                                              
         J     LSET0180                                                         
*                                                                               
LSET0160 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'STC'),VERSION                                              
LSET0180 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LSET0200 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   SETYES                                                           
         J     LSET0140                                                         
*                                                                               
LSET0220 DS    0H                                                               
         CLI   SETFLAG,SETSETNO    SET OF SETS?                                 
         BE    LSET0320            NO  -                                        
         MVI   SETFLAG,SETDET      TURN ON 'PROCESS DETAIL'                     
LSET0240 DS    0H                                                               
*                                                                               
         L     RF,TESTCTR          TEST LOOP CONTROL                            
         LA    RF,1(RF)                                                         
         ST    RF,TESTCTR                                                       
*                                                                               
         L     RF,ANEXTSET         YES - CHECK FOR ANOTHER MEMBER               
         OC    0(4,RF),0(RF)       ANY ENTRY?                                   
         BZ    LSET0280            NO  - RESET TO SET OF SETS REC               
         MVC   SETKEY2,RSETREC     SAVE OFF KEY                                 
         MVC   IOKEY(27),RSETREC   SET NEW KEY                                  
*                                                                               
         MVC   IOKEY+RSETKID-RSETREC(4),0(RF)                                   
*                                  INSERT MEMBER ID INTO KEY                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             READ MEMBER RECORD                           
         JNE   LSET0260            NOT FOUND: RESET FOR NEXT                    
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    SETNO               ERROR ON GETREC                              
         JL    LSET0220                                                         
         CLI   SETFLAG,SETDET      SET DETAILS?                                 
         BNE   LSET0120            NO                                           
         MVC   RSETKID,SETNAME     YES - STUFF NAME OF SET OF                   
*                                     SETS INTO RECORD KEY                      
*                                                                               
*&&DO                                                                           
*   TEST                                                                        
         CLI   SETFLAG,C'N'        SET OF SETS?                                 
         BE    TEST0020            NO                                           
         LA    R0,3                                                             
         LNR   R0,R0                                                            
         CLI   TESTCTR+3,2         TEST DUMP                                    
         BL    TEST0020            DUMP ON COUNT                                
         DC    H'0'                                                             
**       MVC   KILLIT(2),=X'0000'                                               
TEST0020 EQU   *                                                                
*   TEST END                                                                    
*&&                                                                             
*                                                                               
         J     LSET0120            PROCESS THIS MEMBER RECORD                   
*                                                                               
LSET0260 DS    0H                                                               
         MVC   IOKEY(27),SETKEY2   RESET KEY WHEN NOT FOUND                     
         L     RF,ANEXTSET                                                      
         LA    RF,4(RF)            BUMP TO NEXT SLOT                            
         ST    RF,ANEXTSET                                                      
         B     LSET0240            CHECK NEXT MEMBER                            
LSET0280 DS    0H                                                               
         MVC   IOKEY(27),SETKEY    RESTORE SET OF SETS REC KEY                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             READ SET OF SETS RECORD                      
         JE    *+6                 FOUND                                        
LSET0300 DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    LSET0300            ERROR ON GETREC: DIE                         
LSET0320 DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSET0020                                                         
         J     SETYES                                                           
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE SET RECORDS                                                  *         
*        THIS LOGIC IS IN ERROR.  WHILE IT IS POSSIBLE TO KILL ALL    *         
*        REFERENCES TO A SET (AND A SET OF SETS) ON THE DB BY         *         
*        DELETING ALL MEMBERS, IT IS MUCH MORE CONVOLUTED TO REBUILD  *         
*        THAT SET (OR SET OF SETS) FROM THE SAME INFORMATION.         *         
* A REGULAR SET THAT HAS CHANGED CAN HAVE ITS HEADER AND MEMBERS      *         
*        DELETED, BASED ON THE 'COPY' ON THE RECOVERY FILE.  THE      *         
*        'CHANGE' RECORD ON THE RECOVERY FILE COULD THEN BE USED      *         
*        TO REBUILD THE SET ON THE DB.                                *         
* HOWEVER, THAT REGULAR SET MAY BE REFERENCED WITHIN ONE OR MORE SETS *         
*        OF SETS.  THESE CAN NOT BE CHANGED, AS THE PARTICIPATING     *         
*        ENTRIES HAVE BEEN PROPOGATED INTO EACH OF THE SETS OF SETS.  *         
* IT IS THEREFORE RECOMMENDED THAT THESE RECORDS ONLY BE BUILT VIA    *         
*        LOAD MODE.                                                   *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTSET  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         MVC   SETALPHA,REPALPHA                                                
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RSETREC,R2                                                       
         CLI   0(R2),X'38'         SET RECORD?                                  
         JNE   SETYES              NO                                           
*                                                                               
         GOTO1 AFILTSET            FILTER IT                                    
         JNE   SETYES              NO MATCH                                     
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
         LR    RE,R2                                                            
         SR    R0,R0                                                            
         ICM   R0,3,RSETLEN                                                     
         AR    R0,RE               A(EOR)                                       
         LA    RE,RSETELEM-RSETREC(RE)                                          
*                                  POINT TO FIRST ELEMENT                       
USET0080 DS    0H                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         L     R3,DXAXREC                                                       
         USING RESTND,R3                                                        
*                                                                               
         GOTO1 AINITSET                                                         
*                                                                               
*                                  BUILD KILL RECORDS                           
         MVI   SETFLAG,C'N'        KILL THE HEADER                              
*                                                                               
*                                                                               
         CLI   RESTNACT,C'D'       DELETE?                                      
         JE    USET0012            YES- CONSIDER IT A 'CHANGE'                  
*                                                                               
         CLI   RESTNACT,C'C'       CHANGE?                                      
         JNE   USET0040            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
USET0012 EQU   *                                                                
*                                                                               
*   FIRST POINT TO 'COPY' RECORD RATHER THAN 'CHANGE' RECORD                    
*                                                                               
         L     R5,DXACPYB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         XC    SETCTR,SETCTR                                                    
*                                                                               
         GOTO1 VREPSETC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,=A(STAXREC)),(SETFLAG,(R6)),SETCTR                            
*                                                                               
*   THEN  RESET TO 'CHANGE' RECORD RATHER THAN 'COPY' RECORD                    
*                                                                               
USET0040 EQU   *                                                                
         L     R5,DXARECB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         XC    SETCTR,SETCTR                                                    
*                                                                               
USET0100 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPSETC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6),SETCTR                                      
         CLI   8(R1),FF            FINISHED?                                    
         JE    USET0180                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RESTND,RF                                                        
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                DO NOT OVERRIDE REPCODE                      
         MVC   X.RESTNREP,DXU.RXUOVREP                                          
*                                                                               
***      CLC   X.RESTNTYP,T.REPCONQ                                             
***      JNE   *+10                                                             
***      MVC   X.RESTNMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   USET0100            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RESTNACT,C'K'       NOW ADD COPY RECORD DETAILS                  
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    USET0160            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.RESTNTYP,T.REPSTNQ                                             
         JNE   USET0120                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'STN'),VERSION                                           
         J     USET0140                                                         
*                                                                               
USET0120 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'STC'),VERSION                                           
USET0140 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
USET0160 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     USET0100                                                         
*                                                                               
USET0180 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPSETC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(SETFLAG,(R6)),SETCTR                            
*                                                                               
USET0200 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPSETC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(SETFLAG,(R6)),SETCTR                            
         CLI   8(R1),FF            FINISHED?                                    
         JE    SETYES                                                           
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RESTND,RF                                                        
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                DO NOT OVERRIDE REPCODE                      
         MVC   X.RESTNREP,DXU.RXUOVREP                                          
*                                                                               
***      CLC   X.RESTNTYP,T.REPSTNQ                                             
***      JNE   *+10                                                             
***      MVC   X.RESTNMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   USET0200            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RESTNACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    USET0260            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.RESTNTYP,T.REPSTNQ                                             
         JNE   USET0220                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'STN'),VERSION                                              
         J     USET0240                                                         
*                                                                               
USET0220 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'STC'),VERSION                                              
USET0240 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
USET0260 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     USET0200                                                         
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER SET RECORD:  MASTER VS SUBSIDIARY                                      
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTSET  NTR1  BASE=*,LABEL=*                                                   
         USING RSETREC,R2                                                       
*                                                                               
         LA    RF,SUBSETS          SET A(SUBSIDIARY PROCESSING TABLE)           
         CLC   =C'NU',REPALPHA     TREAT CLEAR CHANNEL AS SUB                   
         BE    FSET0020                                                         
*                                                                               
         CLC   MASALPHA,REPALPHA   REP IN PROCESS MASTER?                       
         JNE   FSET0020            NO  - USE SUBSIDIARY TABLE                   
         LA    RF,MSTSETS          YES - SET A(MASTER PROCESSING TABLE)         
FSET0020 EQU   *                                                                
         CLI   0(RF),X'00'         END OF TABLE REACHED?                        
         JE    SETNO               UNRECOGNIZED SET TYPE                        
*                                                                               
         CLC   RSETKSET,0(RF)      SET TYPE FOUND IN TABLE?                     
         JE    FSET0040            YES -                                        
         LA    RF,LSUBSETS(RF)     NO  - BUMP TO NEXT SLOT IN TABLE             
         B     FSET0020            GO BACK AND CHECK NEXT                       
FSET0040 EQU   *                                                                
         CLI   2(RF),C'M'          TAKE THIS TYPE FROM MASTER?                  
         BE    FSET0060            YES                                          
         CLC   RSETKREP,REPALPHA   NO  - SUB: IS IT SUBSIDIARY?                 
         BNE   SETNO               NO  - REC FROM MASTER: SKIP IT               
         B     SETYES              YES - REC FROM SUB:    TAKE IT               
FSET0060 EQU   *                                                                
*                                                                               
*   CLEAR CHANNEL MUST USE KATZ RADIO GROUP MASTER RECORDS.  THIS               
*        IS AN UNHOLY SITUATION THAT MUST BE FORCED.                            
*                                                                               
         CLC   REPALPHA,=C'NU'     IS SUBSIDIARY 'CLEAR CHANNEL'?               
         BNE   FSET0080            NO  -                                        
         CLC   RSETKREP,=C'K3'     YES - REC FROM KATZ RADIO GROUP?             
         BNE   SETNO               NO  - SKIP IT                                
         B     SETYES              YES - REC FROM MASTER: TAKE IT               
FSET0080 EQU   *                                                                
         CLC   RSETKREP,MASALPHA   MASTER: IS REC FROM MASTER?                  
         BNE   SETNO               NO  - REC FROM SUB:    SKIP IT               
         B     SETYES              YES - REC FROM MASTER: TAKE IT               
*                                                                               
*                                                                               
***                                                                             
*  SET TYPE TABLES                                                              
*                                                                               
*  SUBSETS:  BY SET TYPE, WHICH SETS TO EXTRACT FROM SUB OR MASTER              
*                                                                               
SUBSETS  DC    C'GSM'              GROUP/SUBGROUP - MASTER                      
LSUBSETS EQU   *-SUBSETS                                                        
         DC    C'SPM'              SALESPERSON    - MASTER                      
         DC    C'ADM'              ADVERTISER     - MASTER                      
         DC    C'AGM'              AGENCY         - MASTER                      
         DC    C'STS'              STATION        - MASTER                      
         DC    C'PPM'              POINTPERSON    - MASTER                      
         DC    C'CTS'              CONTRACT TYPE  - MASTER                      
         DC    C'DTM'              DEV CON TYPE   - MASTER                      
         DC    C'OFS'              OFFICE         - MASTER                      
****>>>> DC    C'REM'              REP            - MASTER                      
         DC    C'MKM'              MARKET         - MASTER                      
         DC    X'0000'             DELIMITER                                    
*                                                                               
*  MSTSETS:  BY SET TYPE, WHICH SETS TO EXTRACT FROM SUB OR MASTER              
*                                                                               
MSTSETS  DC    C'GSM'              GROUP/SUBGROUP - MASTER                      
LMSTSETS EQU   *-MSTSETS                                                        
         DC    C'SPM'              SALESPERSON    - MASTER                      
         DC    C'ADM'              ADVERTISER     - MASTER                      
         DC    C'AGM'              AGENCY         - MASTER                      
         DC    C'STM'              STATION        - MASTER                      
         DC    C'PPM'              POINTPERSON    - MASTER                      
         DC    C'CTM'              CONTRACT TYPE  - MASTER                      
         DC    C'DTM'              DEV CON TYPE   - MASTER                      
         DC    C'OFM'              OFFICE         - MASTER                      
         DC    C'REM'              REP            - MASTER                      
         DC    C'MKM'              MARKET         - MASTER                      
         DC    X'0000'             DELIMITER                                    
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE SET RECORD HEADER/DETAIL RECORD                          *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITSET  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RESTNDL          R1=L'SET RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     SETYES                                                           
         SPACE 2                                                                
SETYES   SR    RC,RC               RETURN CC EQUAL                              
SETNO    LTR   RC,RC               RETURN CC NOT EQUAL                          
SETEXIT  XIT1  ,                                                                
**SET**<<<                                                                      
***SWITCH**<<<                                                                  
*********CONFIRM COMMENT                                                        
***********************************************************************         
* LOAD CCO RECORDS (CONFIRM COMMENT TYPE)                                       
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADCCO  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RCFCREC,R2                                                       
         XC    RCFCKEY,RCFCKEY                                                  
         MVI   RCFCKTYP,X'47'                                                   
         MVC   RCFCKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCCO02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RCFCKCON-RCFCKEY),0(R2)                                    
*                                                                               
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPCCOC,AINITCCO,AFILTCCO                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCCO02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CCO RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
*&&DO                                                                           
UPDTCCO  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RCFCREC,R2                                                       
*                                                                               
         GOTO1 AFILTCCO                                                         
         JNE   YES                                                              
         GOTO1 AINITCCO                                                         
         GOTO1 AACCUPDT,DMCB,VREPCCOC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
*&&                                                                             
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTCCO  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RCFCREC,R2                                                       
*&&DO                                                                           
*   TEST                                                                        
**       CLI   RCFCREC,X'47'                                                    
**       BNE   TESTCF20                                                         
         TM    RCFCCNTL,X'80'                                                   
         BNO   TESTCF20                                                         
         LA    RE,1                                                             
         DC    H'0'                                                             
TESTCF20 EQU   *                                                                
*   TEST END                                                                    
*&&                                                                             
         GOTO1 AFILTCCO                                                         
         JNE   YES                                                              
*                                                                               
         L     R3,DXAXREC                                                       
         USING RECCOD,R3                                                        
*                                                                               
         GOTO1 AINITCCO                                                         
*                                                                               
         CLI   RECCOACT,C'D'       DELETE?                                      
         JE    UCCO0100            YES- CONSIDER IT A 'CHANGE'                  
*                                                                               
         CLI   RECCOACT,C'C'       CHANGE?                                      
         JNE   UCCO0280            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
UCCO0100 EQU   *                                                                
*                                  BUILD KILL RECORDS                           
*                                                                               
*   FIRST POINT TO 'COPY' RECORD RATHER THAN 'CHANGE' RECORD                    
*                                                                               
         L     R5,DXACPYB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UCCO0110 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPCCOC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,=A(STAXREC)),(R6)                                             
*                                                                               
*                                                                               
*   THEN  RESET TO 'CHANGE' RECORD RATHER THAN 'COPY' RECORD                    
*                                                                               
         L     R5,DXARECB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UCCO0120 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPCCOC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    UCCO0280                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECCOD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UCCO0120            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RECCOACT,C'K'       NOW ADD COPY RECORD DETAILS                  
         MVI   RECCODEL,C' '       CLEAR IF SET TO 'DELETED'                    
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    UCCO0260            DO NOT CONVERT RECORDS                       
*                                                                               
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UCCO0260 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
****     J     UCCO0120            NO LOOP: ONLY ONE RECORD                     
*                                                                               
UCCO0280 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPCCOC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                                                               
UCCO0300 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPCCOC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECCOD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UCCO0300            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RECCOACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UCCO0360            DO NOT CONVERT RECORDS                       
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UCCO0360 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
***      J     UCCO0300            NO LOOP: ONLY ONE RECORD                     
         J     YES                 EXIT ROUTINE                                 
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CCO RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTCCO  NTR1  BASE=*,LABEL=*                                                   
         USING RCFCREC,R2                                                       
         CLC   RCFCKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RCFCKTYP,X'47'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE CCO RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITCCO  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RECCODL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*********CONFIRM COMMENT                                                        
****^^^^^^^^^^^^^^^^^^^>>>>   STANDARD COMMENT                                  
***********************************************************************         
* LOAD STANDARD COMMENT RECORDS                                                 
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADSCM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RCMTREC,R2                                                       
         XC    RCMTKEY,RCMTKEY                                                  
         MVI   RCMTKTYP,X'2E'                                                   
         MVC   RCMTKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
LSCM0040 EQU   *                                                                
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RCMTKOFF-RCMTKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
*                                                                               
         GOTO1 AFILTSCM            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LSCM0180                                                         
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    NO                  ERROR ON GETREC                              
         JL    LSCM0180                                                         
*                                                                               
LSCM0060 DS    0H                                                               
*                                                                               
         GOTO1 AINITSCM            INITIALISE EXTRACT BUFFER                    
         GOTO1 VREPSCMC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LSCM0080 DS    0H                                                               
         GOTO1 VREPSCMC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
*&&DO                                                                           
*   TEST                                                                        
         LA    RF,1                                                             
         LNR   RF,RF                                                            
         DC    H'0'                                                             
*   TEST END                                                                    
*&&                                                                             
         CLI   8(R1),FF                                                         
         JE    LSCM0180            NO MORE RECORDS LEFT                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
*                                                                               
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   LSCM0180            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    LSCM0160            DO NOT CONVERT RECORD                        
*                                                                               
         CLC   X.RECONTYP,T.REPSCMQ                                             
         JNE   LSCM0100                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         J     LSCM0140                                                         
*                                                                               
LSCM0100 DS    0H                                                               
         DC    H'0'                UNRECOGNIZED RECORD TYPE ENCOUNTERED         
LSCM0140 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LSCM0160 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LSCM0080                                                         
*                                                                               
LSCM0180 DS    0H                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
*                                                                               
         JNZ   LSCM0040                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE STANDARD COMMENT RECORD                                                
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTSCM  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RCMTREC,R2                                                       
*                                                                               
         GOTO1 AFILTSCM                                                         
         JNE   YES                                                              
*                                                                               
         L     R3,DXAXREC                                                       
         USING RESCMD,R3                                                        
*                                                                               
         GOTO1 AINITSCM                                                         
*                                                                               
         CLI   RESCMACT,C'D'       DELETE?                                      
         JE    USCM0100            YES- CONSIDER IT A 'CHANGE'                  
*                                                                               
         CLI   RESCMACT,C'C'       CHANGE?                                      
         JNE   USCM0280            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
USCM0100 EQU   *                                                                
*                                  BUILD KILL RECORDS                           
*                                                                               
*   FIRST POINT TO 'COPY' RECORD RATHER THAN 'CHANGE' RECORD                    
*                                                                               
         L     R5,DXACPYB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
USCM0110 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPSCMC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,=A(STAXREC)),(R6)                                             
*                                                                               
*                                                                               
*   THEN  RESET TO 'CHANGE' RECORD RATHER THAN 'COPY' RECORD                    
*                                                                               
         L     R5,DXARECB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
USCM0120 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPSCMC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    USCM0280                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RESCMD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   USCM0120            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RESCMACT,C'K'       NOW ADD COPY RECORD DETAILS                  
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    USCM0260            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.RESCMTYP,T.REPSCMQ                                             
         JNE   USCM0160                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         J     USCM0240                                                         
USCM0160 DS    0H                                                               
         DC    H'0'                UNRECOGNIZED REC TYPE ENCOUNTERED            
USCM0240 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
USCM0260 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
         J     USCM0120                                                         
*                                                                               
USCM0280 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPSCMC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                                                               
USCM0300 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPSCMC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RESCMD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   USCM0300            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RESCMACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    USCM0360            DO NOT CONVERT RECORDS                       
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
USCM0340 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
USCM0360 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     USCM0300                                                         
*                                                                               
         DROP  X,T                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER STANDARD COMMENT RECORD                                      *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTSCM  NTR1  BASE=*,LABEL=*                                                   
         USING RCMTREC,R2                                                       
         CLC   RCMTKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RCMTKTYP,X'2E'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE STANDARD COMMENT RECORD                                            
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITSCM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RESCMDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
****^^^^^^^^^^^^^^^^^^^>>>>   STANDARD COMMENT                                  
***  ********  OFFICE COMMENT                                                   
***********************************************************************         
* LOAD OFFICE COMMENT RECORDS                                                   
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADOFC  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ROCMREC,R2                                                       
         XC    ROCMKEY,ROCMKEY                                                  
         MVI   ROCMKTYP,X'34'                                                   
         MVC   ROCMKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
LOFC0040 EQU   *                                                                
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(ROCMKOFC-ROCMKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
*                                                                               
         GOTO1 AFILTOFC            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LOFC0180                                                         
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    NO                  ERROR ON GETREC                              
         JL    LOFC0180                                                         
*                                                                               
LOFC0060 DS    0H                                                               
*                                                                               
         GOTO1 AINITOFC            INITIALISE EXTRACT BUFFER                    
         GOTO1 VREPOFCC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LOFC0080 DS    0H                                                               
         GOTO1 VREPOFCC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
*&&DO                                                                           
*   TEST                                                                        
         LA    RF,1                                                             
         LNR   RF,RF                                                            
         DC    H'0'                                                             
*   TEST END                                                                    
*&&                                                                             
         CLI   8(R1),FF                                                         
         JE    LOFC0180            NO MORE RECORDS LEFT                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
*                                                                               
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   LOFC0180            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    LOFC0160            DO NOT CONVERT RECORD                        
*                                                                               
         CLC   X.RECONTYP,T.REPOFCQ                                             
         JNE   LOFC0100                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         J     LOFC0140                                                         
*                                                                               
LOFC0100 DS    0H                                                               
         DC    H'0'                UNRECOGNIZED RECORD TYPE ENCOUNTERED         
LOFC0140 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LOFC0160 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LOFC0080                                                         
*                                                                               
LOFC0180 DS    0H                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
*                                                                               
         JNZ   LOFC0040                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE OFFICE COMMENT RECORD                                                  
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTOFC  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ROCMREC,R2                                                       
*                                                                               
         GOTO1 AFILTOFC                                                         
         JNE   YES                                                              
*                                                                               
         L     R3,DXAXREC                                                       
         USING REOFCD,R3                                                        
*                                                                               
         GOTO1 AINITOFC                                                         
*                                                                               
         CLI   REOFCACT,C'D'       DELETE?                                      
         JE    UOFC0100            YES- CONSIDER IT A 'CHANGE'                  
*                                                                               
         CLI   REOFCACT,C'C'       CHANGE?                                      
         JNE   UOFC0280            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
UOFC0100 EQU   *                                                                
*                                  BUILD KILL RECORDS                           
*                                                                               
*   FIRST POINT TO 'COPY' RECORD RATHER THAN 'CHANGE' RECORD                    
*                                                                               
         L     R5,DXACPYB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UOFC0110 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPOFCC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,=A(STAXREC)),(R6)                                             
*                                                                               
*                                                                               
*   THEN  RESET TO 'CHANGE' RECORD RATHER THAN 'COPY' RECORD                    
*                                                                               
         L     R5,DXARECB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UOFC0120 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPOFCC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    UOFC0280                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REOFCD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UOFC0120            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   REOFCACT,C'K'       NOW ADD COPY RECORD DETAILS                  
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    UOFC0260            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.REOFCTYP,T.REPOFCQ                                             
         JNE   UOFC0160                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         J     UOFC0240                                                         
UOFC0160 DS    0H                                                               
         DC    H'0'                UNRECOGNIZED REC TYPE ENCOUNTERED            
UOFC0240 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UOFC0260 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
         J     UOFC0120                                                         
*                                                                               
UOFC0280 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPOFCC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                                                               
UOFC0300 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPOFCC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REOFCD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UOFC0300            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   REOFCACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UOFC0360            DO NOT CONVERT RECORDS                       
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
UOFC0340 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UOFC0360 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UOFC0300                                                         
*                                                                               
         DROP  X,T                                                              
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER OFFICE COMMENT RECORD                                        *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTOFC  NTR1  BASE=*,LABEL=*                                                   
         USING ROCMREC,R2                                                       
         CLC   ROCMKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   ROCMKTYP,X'34'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE OFFICE COMMENT RECORD                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITOFC  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REOFCDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***  ********  OFFICE COMMENT                                                   
***>>>   EOP AGENCY RECORDS                                                     
***********************************************************************         
* LOAD EOP AGENCY RECORDS                                                       
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADEAG  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING REO2REC,R2                                                       
         XC    REO2KEY,REO2KEY                                                  
         MVI   REO2KTYP,X'1C'                                                   
         MVC   REO2KREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEAG02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(REO2KSYS-REO2KEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPEAGC,AINITEAG,AFILTEAG                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LEAG02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE EAG RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTEAG  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING REO2REC,R2                                                       
*                                                                               
         GOTO1 AFILTEAG                                                         
         JNE   YES                                                              
         GOTO1 AINITEAG                                                         
         GOTO1 AACCUPDT,DMCB,VREPEAGC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER EAG RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTEAG  NTR1  BASE=*,LABEL=*                                                   
         USING REO2REC,R2                                                       
         CLC   REO2KREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   REO2KTYP,X'1C'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE EAG RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITEAG  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REEPADL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***>>>   EOP AGENCY RECORDS                                                     
******>>>EOP ADVERT RECORDS                                                     
***********************************************************************         
* LOAD EOP ADVERT RECORDS                                                       
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADEAD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING REOPREC,R2                                                       
         XC    REOPKEY,REOPKEY                                                  
         MVI   REOPKTYP,X'1B'                                                   
         MVC   REOPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEAD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(REOPKSYS-REOPKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPEADC,AINITEAD,AFILTEAD                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LEAD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE EAD RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTEAD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING REOPREC,R2                                                       
*                                                                               
         GOTO1 AFILTEAD                                                         
         JNE   YES                                                              
         GOTO1 AINITEAD                                                         
         GOTO1 AACCUPDT,DMCB,VREPEADC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER EAD RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTEAD  NTR1  BASE=*,LABEL=*                                                   
         USING REOPREC,R2                                                       
         CLC   REOPKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   REOPKTYP,X'1B'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE EAD RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITEAD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REEPDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
******>>>EOP ADVERT RECORDS                                                     
***<<<<  EOP OFFICE RECORDS                                                     
***********************************************************************         
* LOAD EOP OFFICE RECORDS                                                       
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADEOF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING REO3REC,R2                                                       
         XC    REO3KEY,REO3KEY                                                  
         MVI   REO3KTYP,X'1D'                                                   
         MVC   REO3KREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEOF02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(REO3KSYS-REO3KEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPEOFC,AINITEOF,AFILTEOF                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LEOF02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE EOF RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTEOF  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING REO3REC,R2                                                       
*                                                                               
         GOTO1 AFILTEOF                                                         
         JNE   YES                                                              
         GOTO1 AINITEOF                                                         
         GOTO1 AACCUPDT,DMCB,VREPEOFC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER EOF RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTEOF  NTR1  BASE=*,LABEL=*                                                   
         USING REO3REC,R2                                                       
         CLC   REO3KREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   REO3KTYP,X'1D'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE EOF RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITEOF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REEPODL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***<<<<  EOP OFFICE RECORDS                                                     
****++++ EOP SALESPERSON RECORDS                                                
***********************************************************************         
* LOAD EOP SALESPERSON RECORDS                                                  
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADESP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING REO4REC,R2                                                       
         XC    REO4KEY,REO4KEY                                                  
         MVI   REO4KTYP,X'1E'                                                   
         MVC   REO4KREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LESP02   TM    DMCB+8,X'80'        ALL DONE IF ESP                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(REO4KSYS-REO4KEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPESPC,AINITESP,AFILTESP                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LESP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE ESP RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTESP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING REO4REC,R2                                                       
*                                                                               
         GOTO1 AFILTESP                                                         
         JNE   YES                                                              
         GOTO1 AINITESP                                                         
         GOTO1 AACCUPDT,DMCB,VREPESPC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER ESP RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTESP  NTR1  BASE=*,LABEL=*                                                   
         USING REO4REC,R2                                                       
         CLC   REO4KREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   REO4KTYP,X'1E'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE ESP RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITESP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REEPSDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
****++++ EOP SALESPERSON RECORDS                                                
********+++BUYCODE RECORDS                                                      
***********************************************************************         
* LOAD BCD RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADBCD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RSPBREC,R2                                                       
         XC    RSPBKEY,RSPBKEY                                                  
         MVI   RSPBKTYP,X'4B'                                                   
         MVC   RSPBKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LBCD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RSPBKTCD-RSPBKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPBCDC,AINITBCD,AFILTBCD                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LBCD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE BCD RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTBCD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RSPBREC,R2                                                       
*                                                                               
         GOTO1 AFILTBCD                                                         
         JNE   YES                                                              
         GOTO1 AINITBCD                                                         
         GOTO1 AACCUPDT,DMCB,VREPBCDC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER BCD RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTBCD  NTR1  BASE=*,LABEL=*                                                   
         USING RSPBREC,R2                                                       
         CLC   RSPBKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RSPBKTYP,X'4B'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE BCD RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITBCD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REBCDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
********+++BUY RECORDS                                                          
***********************************************************************         
* LOAD BUY RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADBUY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RBUYREC,R2                                                       
         XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LBUY0040 TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RBUYKCON-RBUYKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AFILTBUY            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LBUY0180                                                         
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    NO                  ERROR ON GETREC                              
         JL    LBUY0180                                                         
*                                                                               
LBUY0060 DS    0H                                                               
*                                                                               
         GOTO1 AINITBUY            INITIALISE EXTRACT BUFFER                    
         GOTO1 VREPBUYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LBUY0080 DS    0H                                                               
         GOTO1 VREPBUYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF                                                         
         JE    LBUY0180            NO MORE RECORDS LEFT                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
*                                                                               
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REBUYD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   LBUY0180            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    LBUY0160            DO NOT CONVERT RECORD                        
*                                                                               
         LA    R1,=C'BYH'                                                       
         CLC   X.REBUYTYP,T.REPBYHQ                                             
         JE    LBUY0090                                                         
         LA    R1,=C'BDT'                                                       
         CLC   X.REBUYTYP,T.REPBDTQ                                             
         JE    LBUY0090                                                         
         LA    R1,=C'BED'                                                       
         CLC   X.REBUYTYP,T.REPBEDQ                                             
         JE    LBUY0090                                                         
         LA    R1,=C'BCM'                                                       
         CLC   X.REBUYTYP,T.REPBYCQ                                             
         JE    LBUY0090                                                         
         LA    R1,=C'BDD'                                                       
         CLC   X.REBUYTYP,T.REPBADQ                                             
         JE    LBUY0090                                                         
         LA    R1,=C'BRD'                                                       
         CLC   X.REBUYTYP,T.REPBRDQ                                             
         JE    LBUY0090                                                         
         LA    R1,=C'BPT'                                                       
         CLC   X.REBUYTYP,T.REPBPTQ                                             
         JE    LBUY0090                                                         
         LA    R1,=C'BPG'                                                       
         CLC   X.REBUYTYP,T.REPBPGQ                                             
         JE    LBUY0090                                                         
         LA    R1,=C'BBC'                                                       
         CLC   X.REBUYTYP,T.REPBBCQ                                             
         JE    LBUY0090                                                         
         LA    R1,=C'BSB'                                                       
         CLC   X.REBUYTYP,T.REPBSBQ                                             
         JE    LBUY0090                                                         
         LA    R1,=C'BOC'                                                       
         CLC   X.REBUYTYP,T.REPBOCQ                                             
         JE    LBUY0090                                                         
         LA    R1,=C'DOL'                                                       
         CLC   X.REBUYTYP,T.REPDOLQ                                             
         JNE   LBUY0100                                                         
*                                                                               
LBUY0090 DS    0H                                                               
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,0                                                         
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),,VERSION            
         J     LBUY0110                                                         
*                                                                               
LBUY0100 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
LBUY0110 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LBUY0160 EQU   *                                                                
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LBUY0080                                                         
*                                                                               
LBUY0180 DS    0H                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
*                                                                               
         JNZ   LBUY0040                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
************                                                                    
*---------------------------------------------------------------------*         
* UPDATE BUY RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTBUY  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RBUYREC,R2                                                       
*                                                                               
* TEMP CODE TO SKIP CORRUPTED BUY RECORD                                        
*                                                                               
*&&DO                                                                           
         CLC   =C'FB',RBUYKREP                                                  
         BNE   UBUY0010                                                         
         CLC   =X'03614169',RBUYKCON                                            
         BNE   UBUY0010                                                         
         CLC   =X'0101',RBUYKMLN                                                
         JE    YES                                                              
*&&                                                                             
*                                                                               
UBUY0010 DS    0H                                                               
         GOTO1 AFILTBUY                                                         
         JNE   YES                                                              
*                                                                               
         L     R3,DXAXREC                                                       
         USING REBUYD,R3                                                        
*                                                                               
         GOTO1 AINITBUY                                                         
*                                                                               
         CLI   REBUYACT,C'D'       DELETE?                                      
         JE    UBUY0100            YES- CONSIDER IT A 'CHANGE'                  
*                                                                               
         CLI   REBUYACT,C'C'       CHANGE?                                      
         JNE   UBUY0280            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
UBUY0100 EQU   *                                                                
*                                  BUILD KILL RECORDS                           
*                                                                               
*   FIRST POINT TO 'COPY' RECORD RATHER THAN 'CHANGE' RECORD                    
*                                                                               
         L     R5,DXACPYB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UBUY0110 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPBUYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,=A(STAXREC)),(R6)                                             
*                                                                               
*   THEN  RESET TO 'CHANGE' RECORD RATHER THAN 'COPY' RECORD                    
*                                                                               
         L     R5,DXARECB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UBUY0120 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPBUYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    UBUY0280                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REBUYD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UBUY0120            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   REBUYACT,C'K'       NOW ADD COPY RECORD DETAILS                  
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    UBUY0260            DO NOT CONVERT RECORDS                       
*                                                                               
         LA    R1,=C'BYH'                                                       
         CLC   X.REBUYTYP,T.REPBYHQ                                             
         JE    UBUY0130                                                         
         LA    R1,=C'BDT'                                                       
         CLC   X.REBUYTYP,T.REPBDTQ                                             
         JE    UBUY0130                                                         
         LA    R1,=C'BED'                                                       
         CLC   X.REBUYTYP,T.REPBEDQ                                             
         JE    UBUY0130                                                         
         LA    R1,=C'BCM'                                                       
         CLC   X.REBUYTYP,T.REPBYCQ                                             
         JE    UBUY0130                                                         
         LA    R1,=C'BDD'                                                       
         CLC   X.REBUYTYP,T.REPBADQ                                             
         JE    UBUY0130                                                         
         LA    R1,=C'BRD'                                                       
         CLC   X.REBUYTYP,T.REPBRDQ                                             
         JE    UBUY0130                                                         
         LA    R1,=C'BPT'                                                       
         CLC   X.REBUYTYP,T.REPBPTQ                                             
         JE    UBUY0130                                                         
         LA    R1,=C'BPG'                                                       
         CLC   X.REBUYTYP,T.REPBPGQ                                             
         JE    UBUY0130                                                         
         LA    R1,=C'BBC'                                                       
         CLC   X.REBUYTYP,T.REPBBCQ                                             
         JE    UBUY0130                                                         
         LA    R1,=C'BSB'                                                       
         CLC   X.REBUYTYP,T.REPBSBQ                                             
         JE    UBUY0130                                                         
         LA    R1,=C'BOC'                                                       
         CLC   X.REBUYTYP,T.REPBOCQ                                             
         JE    UBUY0130                                                         
         LA    R1,=C'DOL'                                                       
         CLC   X.REBUYTYP,T.REPDOLQ                                             
         JNE   UBUY0180                                                         
Z        USING REDOLD,RF                                                        
         CLI   Z.REDOLFLG,C'Z'                                                  
         JNE   UBUY0130                                                         
         LA    R1,=C'DOZ'                                                       
         DROP  Z                                                                
*                                                                               
UBUY0130 DS    0H                                                               
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,C'K'                                                      
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),,VERSION            
         J     UBUY0240                                                         
*                                                                               
UBUY0180 DS    0H                                                               
         DC    H'0'                UNRECOGNIZED REC TYPE ENCOUNTERED            
UBUY0240 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UBUY0260 DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
         J     UBUY0120                                                         
*                                                                               
UBUY0280 DS    0H                  BUILD RECORDS                                
         TM    29(R2),X'C0'        MAKEGOOD KEY CHANGE                          
         JO    YES                 DON'T POST ADD COPY                          
*                                                                               
UBUY0290 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPBUYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                                                               
UBUY0300 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPBUYC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING REBUYD,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UBUY0300            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   REBUYACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
* SPECIAL CODE TO GENERATE EXTRA KILL RECORD TO AVOID DB2 INSERTION             
* ERRORS IN ESS                                                                 
*                                                                               
         USING REDOLD,R3                                                        
         CLC   REDOLTYP,T.REPDOLQ                                               
         BNE   UBUY0305                                                         
         CLI   REDOLFLG,C'X'       SPECIAL KILL FLAG?                           
         BNE   UBUY0305                                                         
         MVI   REDOLACT,C'K'                                                    
         MVI   REDOLFLG,C'Z'                                                    
         DROP  R3                                                               
*                                                                               
UBUY0305 DS    0H                  GET NEXT RECORD                              
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UBUY0360            DO NOT CONVERT RECORDS                       
*                                                                               
         LA    R1,=C'BYH'                                                       
         CLC   X.REBUYTYP,T.REPBYHQ                                             
         JE    UBUY0310                                                         
         LA    R1,=C'BDT'                                                       
         CLC   X.REBUYTYP,T.REPBDTQ                                             
         JE    UBUY0310                                                         
         LA    R1,=C'BED'                                                       
         CLC   X.REBUYTYP,T.REPBEDQ                                             
         JE    UBUY0310                                                         
         LA    R1,=C'BCM'                                                       
         CLC   X.REBUYTYP,T.REPBYCQ                                             
         JE    UBUY0310                                                         
         LA    R1,=C'BDD'                                                       
         CLC   X.REBUYTYP,T.REPBADQ                                             
         JE    UBUY0310                                                         
         LA    R1,=C'BRD'                                                       
         CLC   X.REBUYTYP,T.REPBRDQ                                             
         JE    UBUY0310                                                         
         LA    R1,=C'BPT'                                                       
         CLC   X.REBUYTYP,T.REPBPTQ                                             
         JE    UBUY0310                                                         
         LA    R1,=C'BPG'                                                       
         CLC   X.REBUYTYP,T.REPBPGQ                                             
         JE    UBUY0310                                                         
         LA    R1,=C'BBC'                                                       
         CLC   X.REBUYTYP,T.REPBBCQ                                             
         JE    UBUY0310                                                         
         LA    R1,=C'BSB'                                                       
         CLC   X.REBUYTYP,T.REPBSBQ                                             
         JE    UBUY0310                                                         
         LA    R1,=C'BOC'                                                       
         CLC   X.REBUYTYP,T.REPBOCQ                                             
         JE    UBUY0310                                                         
         LA    R1,=C'DOL'                                                       
         CLC   X.REBUYTYP,T.REPDOLQ                                             
         JNE   UBUY0322                                                         
Z        USING REDOLD,RF                                                        
         CLI   Z.REDOLFLG,C'Z'                                                  
         JNE   UBUY0310                                                         
         LA    R1,=C'DOZ'                                                       
         DROP  Z                                                                
*                                                                               
UBUY0310 DS    0H                                                               
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,0                                                         
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),,VERSION            
         J     UBUY0340                                                         
*                                                                               
UBUY0322 DS    0H                                                               
         DC    H'0'                                                             
UBUY0330 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         DROP  X,T                                                              
UBUY0340 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UBUY0360 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UBUY0300                                                         
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER BUY RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTBUY  NTR1  BASE=*,LABEL=*                                                   
         USING RBUYREC,R2                                                       
         CLC   =X'0B00',RBUYKTYP                                                
         JNE   NO                                                               
         CLC   RBUYKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE BUY RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITBUY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REBUYDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD SWI RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADSWI  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RSWIREC,R2                                                       
         XC    RSWIKEY,RSWIKEY                                                  
         MVI   RSWIKTYP,X'28'                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
LSWI0040 EQU   *                                                                
         TM    DMCB+8,X'80'        EOF?                                         
         JO    YES                 YES                                          
*                                                                               
         CLC   IOKEY(RSWIKTYP-RSWIKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE CHANGES                     
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
*                                                                               
         GOTO1 AFILTSWI            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LSWI0180                                                         
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    NO                  ERROR ON GETREC                              
         JL    LSWI0180                                                         
*                                                                               
LSWI0060 DS    0H                                                               
*                                                                               
         GOTO1 AINITSWI            INITIALISE EXTRACT BUFFER                    
         GOTO1 VREPSWIC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LSWI0080 DS    0H                                                               
         GOTO1 VREPSWIC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
*&&DO                                                                           
*   TEST                                                                        
         LA    RF,1                                                             
         LNR   RF,RF                                                            
         DC    H'0'                                                             
*   TEST END                                                                    
*&&                                                                             
         CLI   8(R1),FF                                                         
         JE    LSWI0180            NO MORE RECORDS LEFT                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
*                                                                               
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   LSWI0180            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    LSWI0160            DO NOT CONVERT RECORD                        
*                                                                               
         CLC   X.RECONTYP,T.REPSWIQ                                             
         JNE   LSWI0100                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         J     LSWI0140                                                         
*                                                                               
LSWI0100 DS    0H                                                               
         CLC   X.RECONTYP,T.REPSWFQ                                             
         JNE   LSWI0120                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'SWF'),VERSION                                              
         J     LSWI0140                                                         
LSWI0120 DS    0H                                                               
         DC    H'0'                UNRECOGNIZED RECORD TYPE ENCOUNTERED         
LSWI0140 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LSWI0160 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LSWI0080                                                         
*                                                                               
LSWI0180 DS    0H                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
*                                                                               
         JNZ   LSWI0040                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE SWI RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTSWI  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RSWIREC,R2                                                       
**>>>    LA    R2,IOKEY                                                         
*                                                                               
         GOTO1 AFILTSWI            FILTER IT                                    
         JNE   YES                 NO MATCH                                     
*                                                                               
         L     R3,DXAXREC                                                       
         USING RESWID,R3                                                        
*                                                                               
         GOTO1 AINITSWI                                                         
*                                                                               
         CLI   RESWIACT,C'D'       DELETE?                                      
         JE    USWI0100            YES- CONSIDER IT A 'CHANGE'                  
*                                                                               
         CLI   RESWIACT,C'C'       CHANGE?                                      
         JNE   USWI0280            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
USWI0100 EQU   *                                                                
*                                  BUILD KILL RECORDS                           
*                                                                               
*   FIRST POINT TO 'COPY' RECORD RATHER THAN 'CHANGE' RECORD                    
*                                                                               
         L     R5,DXACPYB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
USWI0110 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPSWIC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,=A(STAXREC)),(R6)                                             
*                                                                               
*                                                                               
*   THEN  RESET TO 'CHANGE' RECORD RATHER THAN 'COPY' RECORD                    
*                                                                               
         L     R5,DXARECB                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
USWI0120 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPSWIC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    USWI0280                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RESWID,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   USWI0120            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RESWIACT,C'K'       NOW ADD COPY RECORD DETAILS                  
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    USWI0260            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.RESWITYP,T.REPSWFQ                                             
         JNE   USWI0140                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'SWF'),VERSION                                           
         J     USWI0240                                                         
*                                                                               
USWI0140 DS    0H                                                               
         CLC   X.RESWITYP,T.REPSWIQ                                             
         JNE   USWI0160                                                         
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
         J     USWI0240                                                         
USWI0160 DS    0H                                                               
         DC    H'0'                UNRECOGNIZED REC TYPE ENCOUNTERED            
USWI0240 DS    0H                                                               
         DROP  X,T                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
USWI0260 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*&&DO                                                                           
*   TEST                                                                        
         LA    RF,2                                                             
         LNR   RF,RF                                                            
         DC    H'0'                                                             
*   TEST                                                                        
*&&                                                                             
         J     USWI0120                                                         
*                                                                               
USWI0280 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPSWIC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                                                               
USWI0300 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPSWIC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RESWID,RF                                                        
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   USWI0300            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RESWIACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    USWI0360            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.RESWITYP,T.REPSWFQ                                             
         JNE   USWI0320                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'SWF'),VERSION                                              
         J     USWI0340                                                         
*                                                                               
USWI0320 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
USWI0340 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
USWI0360 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     USWI0300                                                         
*                                                                               
*****<<<<<<<<<<<<<<<<<<                                                         
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER SWI RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTSWI  NTR1  BASE=*,LABEL=*                                                   
         USING RSWIREC,R2                                                       
         CLC   RSWIKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
*                                                                               
         CLI   RSWIKTYP,X'28'      SWITCH RECORD TYPE?                          
         JNE   NO                                                               
         CLC   RSWIKYMD,SWISTRT    SWITCH RECORD APPLIED?                       
         JH    NO                  REC DATE AFTER SELECT DATE: SKIP IT          
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE SWI RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITSWI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RESWIDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
***SWITCH**<<<                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN LOAD MODE                  *         
* R2 = A(ACCOUNT DIRECTORY RECORD BUFFER)                             *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
***********************************************************************         
         SPACE 1                                                                
ACCLOAD  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               FILTER ROUTINE?                              
         JZ    ALOA02                                                           
         GOTO1 (R5)                FILTER RECORD                                
         JNE   ALOA06              NOT VALID - GET NEXT                         
*                                                                               
ALOA02   GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 (R4)                INITIALISE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),IO                                 
         TM    DMCB+8,X'80'                                                     
         JO    ALOA06              ERROR - NO WRITE                             
         CLI   DMCB+8,FF                                                        
         JE    ALOA06              DATA NOT COMPLETE - NO WRITE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                DO NOT OVERRIDE REPCODE                      
         MVC   RECONREP-RECOND(2,RF),DXU.RXUOVREP                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   ALOA06              CONTROLLER REQUESTS NO WRITE                 
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    ALOA04              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',DXAXREC),(PLATFORM,DXASQLB),        *        
               (0,TYPENAME),VERSION                                             
*                                                                               
         L     RF,DXASQLB                                                       
ALOA04   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7) UNCONVERTED RECORD TO EXTRACT               
*                                                                               
ALOA06   GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JNE   NO                  TOO MANY IOS                                 
*                                                                               
         MVC   IOKEY(L'RREPKEY),0(R2) READ NEXT RECORD - SEQUENTIAL             
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,REPDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN UPDATE MODE                *         
* R2 = A(ACCOUNT RECORD BUFFER)                                       *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(3 CHAR MNEMONIC)                                             *         
***********************************************************************         
         SPACE 1                                                                
ACCUPDT  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),IO                                 
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    NO                                                               
         TM    DMCB+8,X'80'                                                     
         JO    NO                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                   DO NOT OVERRIDE REPCODE                   
         MVC   RECONREP-RECOND(2,RF),DXU.RXUOVREP                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    UPDL04              DO NOT CONVERT RECORD                        
*                                                                               
         CLI   DXACTION,C'C'       SPECIAL CODE FOR CHANGES                     
         JNE   UPDL02                                                           
*                                                                               
         L     R0,ACOPYBUF         R0=A(EXTRACT RECORD AREA FOR COPY)           
         LH    R1,=Y(L'COPYBUFF)                                                
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         GOTO1 (R3),DMCB,ACOPYBUF,(R2),0,(R6)         BUILD COPY REC            
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+14                   DO NOT OVERRIDE REPCODE                   
*                                                                               
         L     RF,ACOPYBUF            RF=A(COPY RECORD)                         
         MVC   RECONREP-RECOND(2,RF),DXU.RXUOVREP                               
*                                                                               
         L     R0,DXAXREC          R0=A(CHANGE SQL RECORD)                      
         L     RE,ACOPYBUF         RE=A(COPY SQL RECORD)                        
         LH    RF,0(RE)            RF=L'RECORD                                  
         LH    R1,=AL2(RECONREP-RECOND) DISP TO REPALPHA                        
         AR    R0,R1               BUMP TO REPALPHA CODE ALL RECORDS            
         AR    RE,R1                                                            
         SR    RF,R1               REDUCE LENGTH APPROPRIATELY                  
*                                  DON'T LOOK AT TRAILING BYTES                 
         AHI   RF,-((RECONCON-1)-RECOND)                                        
         LR    R1,RF                                                            
*                                                                               
*   CERTAIN RECORDS ARE CONSTRUCTED OF MORE THAN ONE MAINFRAME                  
*        RECORDS.  AS THE RECOVERY SYSTEM TRIGGERS OFF A SINGLE                 
*        RECORD, THE SECONDARY RECORD IS RETRIEVED FROM THE DATA                
*        BASE.  THERE IS NO MECHANISM TO LINK THE SEPARATE PARTS                
*        OF THE 'COPY' RECORD ON THE RECOVERY FILE TO BUILD THE                 
*        'OLD' RECORD FOR COMPARISON.                                           
*        THEREFORE, THE ASSUMPTION MUST BE THAT THE RECORD IS TO BE             
*        CHANGED / UPDATED IN ALL CASES FOR THESE RECORD TYPES.                 
*        BILL   OCT13/2005.                                                     
*                                                                               
*   FORCING SKIPS TO PROCESS ALL RECORDS REGARDLESS OF COMPARISON               
*                                                                               
         J     UPDL02                                                           
*                                                                               
         CLC   =C'SAL',0(R4)       SALESPERSON CODE?                            
         JE    UPDL02              YES - IGNORE COMPARISON                      
*                                                                               
         CLC   =C'CAT',0(R4)       SALESPERSON CODE?                            
         JE    UPDL02              YES - IGNORE COMPARISON                      
*                                                                               
         CLCL  R0,RE               IF EXTRACT VERSIONS OF COPY/CHANGE           
         JE    UPDL020             ARE THE SAME THEN SKIP                       
*                                                                               
UPDL02   DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',DXAXREC),(PLATFORM,DXASQLB),        *        
               (R4),VERSION                                                     
*                                                                               
         L     RF,DXASQLB                                                       
UPDL04   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
UPDL020  EQU   *                                                                
         J     YES                                                              
QUIKCTR  DC    F'0'                                                             
         LTORG                                                                  
         DROP  R5                                                               
***********************************************************************         
* SETUP DATES REQUIRED BY THE PROGRAM                                           
***********************************************************************         
SETDATES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* ROLLING WEEK CALCULATION, BASED ON TODAY'S DATE                               
*                                                                               
         GOTO1 VDATCON,DMCB,(5,WORK),(2,WEEKSTRT)                               
*                                  SET TODAY'S DATE: 7TH DAY                    
         GOTO1 VDATCON,DMCB,(5,WORK),(0,WORK+12)                                
         GOTO1 =V(ADDAY),DMCB,WORK+12,WORK,-6                                   
         GOTO1 VDATCON,DMCB,(0,WORK),(2,WEEKEND)                                
*                                                                               
* CALCULATE NEXT YEAR'S BROADCAST YEAR, BASED ON TODAY'S DATE                   
*                                                                               
         CLC   DXU.RXTODAY,SPACES ANY AS AT DATE OVERRIDE?                      
         BNH   SDAT0001            NO                                           
         GOTO1 VDATCON,DMCB,(0,DXU.RXTODAY),(3,WORK)                            
*                                  USE OVERRIDE TO CALCULATE                    
         B     SDAT0002                                                         
SDAT0001 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(5,WORK),(3,WORK)                                   
*                                  BINARY "TODAY'S DATE"                        
SDAT0002 EQU   *                                                                
         ZIC   RF,WORK             EXTRACT THIS YEAR                            
         LA    RF,1(RF)            BUMP TO NEXT YEAR                            
         STC   RF,WORK             REPLACE                                      
*                                                                               
*   CALCULATE START DATE                                                        
*                                                                               
         MVI   WORK+1,1            SET MONTH TO JANUARY                         
         MVI   WORK+2,15           SET DAY TO 15                                
*                                                                               
*   TRANSLATE 3-CHAR BINARY DATE TO EBCDIC IN SAME SPACE                        
*                                                                               
         GOTO1 VDATCON,DMCB,(3,WORK),(0,WORK+24)                                
         MVC   WORK(6),WORK+24                                                  
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK+12),(2,CURBST)                              
*                                  SET CURBST TO NEXT YEAR                      
*&&DO                                                                           
* CURRENT YEAR BROADCAST START - NO LONGER USED                                 
*                                                                               
         GOTO1 VDATCON,DMCB,(9,DXU.RXUCURST),(0,WORK)                           
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+12),(2,CURBST)                              
*&&                                                                             
*                                                                               
* PRIOR YEAR BROADCAST START                                                    
*                                                                               
         GOTO1 =V(ADDAY),DMCB,(C'Y',WORK),(0,WORK),-1                           
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+12),(2,PRIBST)                              
*                                                                               
* CALCULATE NEXT YEAR'S BROADCAST YEAR, BASED ON TODAY'S DATE                   
*                                                                               
         CLC   DXU.RXTODAY,SPACES ANY AS AT DATE OVERRIDE?                      
         BNH   SDAT0003            NO                                           
         GOTO1 VDATCON,DMCB,(0,DXU.RXTODAY),(3,WORK)                            
*                                  USE OVERRIDE TO CALCULATE                    
         B     SDAT0004                                                         
SDAT0003 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(5,WORK),(3,WORK)                                   
*                                  BINARY "TODAY'S DATE"                        
SDAT0004 EQU   *                                                                
         ZIC   RF,WORK             EXTRACT THIS YEAR                            
         LA    RF,1(RF)            BUMP TO NEXT YEAR                            
         STC   RF,WORK             REPLACE                                      
*                                                                               
*   CALCULATE START DATE                                                        
*                                                                               
         MVI   WORK+1,12           SET MONTH TO DECEMBER                        
         MVI   WORK+2,15           SET DAY TO 15                                
*                                                                               
*   TRANSLATE 3-CHAR BINARY DATE TO EBCDIC IN SAME SPACE                        
*                                                                               
         GOTO1 VDATCON,DMCB,(3,WORK),(0,WORK+24)                                
         MVC   WORK(6),WORK+24                                                  
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+18),(2,CURBND)                              
*                                  SET CURBND TO NEXT YEAR                      
*&&DO                                                                           
*   TEST                                                                        
         LA    R0,13                                                            
         LA    RE,WORK                                                          
         LA    RF,CURBST                                                        
         DC    H'0'                                                             
*&&                                                                             
*&&DO                                                                           
* CURRENT YEAR BROADCAST END - NO LONGER USED                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(9,DXU.RXUCURND),(0,WORK)                           
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+18),(2,CURBND)                              
*&&                                                                             
*                                                                               
* PRIOR YEAR BROADCAST END                                                      
*                                                                               
         GOTO1 =V(ADDAY),DMCB,(C'Y',WORK),(0,WORK),-1                           
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+18),(2,PRIBND)                              
*                                                                               
         L     R2,=V(RXQWK)                                                     
         USING QWKD,R2                                                          
         GOTO1 VDATCON,DMCB,(2,CURBST),(3,QWCURST3)                             
         GOTO1 VDATCON,DMCB,(2,CURBND),(3,QWCURND3)                             
         GOTO1 VDATCON,DMCB,(2,PRIBST),(3,QWPRIST3)                             
         GOTO1 VDATCON,DMCB,(2,PRIBND),(3,QWPRIND3)                             
         DROP  R2                                                               
         GOTO1 =V(SETUPMON),DMCB,DXUSER                                         
*                                                                               
* SET SWITCH RECORD DATE                                                        
*        IF LOAD IS DONE ON OTHER THAN A SATURDAY OR SUNDAY,                    
*        THE DATE OF THE SWITCH RECORDS THAT WILL BE ACCEPTED                   
*        MUST BE SET BACK ONE WEEK.  THIS IS BECAUSE THE                        
*        RECORDS WILL NOT HAVE BEEN APPLIED UNTIL THE UPCOMING                  
*        WEEKEND.  AS SUCH, THEY ARE TO BE IGNORED UNTIL APPLIED.               
*                                                                               
         MVI   SWIADJ,0            CLEAR SWIADJ DATE                            
         CLC   DXU.RXTODAY,SPACES ANY AS AT DATE OVERRIDE?                      
         BNH   SDAT0010            NO                                           
         GOTO1 VDATCON,DMCB,(0,DXU.RXTODAY),(0,WORK+6)                          
*                                  USE OVERRIDE TO CALCULATE                    
         B     SDAT0015                                                         
SDAT0010 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(5,WORK),(0,WORK+6)                                 
*                                  GET TODAY'S DATE IN EBCDIC                   
SDAT0015 EQU   *                                                                
         GOTO1 =V(GETDAY),DMCB,WORK+6,WORK+12                                   
         CLI   DMCB,0              ERROR RETURN:                                
         BNE   *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
         CLI   DMCB,6              SATURDAY OR SUNDAY?                          
         BNL   SDAT0020            YES -                                        
*                                                                               
*   NOT SATURDAY OR SUNDAY:  A MIDWEEK LOAD - ADJUST ONE WEEK EARLY             
*                                                                               
         MVI   SWIADJ,7            NO  - SET ADDITIONAL WEEK ADJUSTMENT         
SDAT0020 EQU   *                                                                
         ZIC   R3,DMCB             GET DAY OF WEEK                              
         BCTR  R3,0                MAKE ZERO RELATIVE                           
         ZIC   RE,SWIADJ           GET ADDITIONAL ADJUSTMENT                    
         AR    R3,RE               CALCULATE TOTAL ADJUSTMENT                   
         LNR   R3,R3               MAKE NEGATIVE (BACK UP DATE)                 
         GOTO1 =V(ADDAY),DMCB,WORK+6,WORK+12,(R3)                               
*                                  ADD ADJUSTMENT TO TODAY'S DATE               
         GOTO1 VDATCON,DMCB,(0,WORK+12),(3,SWISTRT)                             
*                                                                               
*   CHECK FOR POSSIBLE EXPANSION OF YEARS COVERED BY REQUEST                    
*                                                                               
*   YEAR ADJUSTMENT:  REQUEST MAY BE FOR A START DATE 2 TO 7                    
*        YEARS PRIOR TO THE START DATE ON THE RXUSERD FIELD                     
*        (THE DETAILS OF THE REQUEST)                                           
*   COL 41 OF THAT CARD CONTAIN AN INDICATOR FOR THE TYPE OF DATA               
*        THAT IS TO BE DEVELOPED.  A NUMERIC VALUE FROM 1 - 7                   
*        WILL INDICATE THAT 'FULL DATA' WITH A START YEAR ADJUSTMENT            
*        IS TO BE DEVELOPED.  A VALUE OF 0 OR 1 WILL INDICATE THAT              
*        THE START DATE OF THE DATE RANGE IS ACCEPTABLE.  A VALUE               
*        FROM 2 THRU 7 WILL INDICATE THAT THE REPORT IS TO SPAN                 
*        FROM 2 TO 7 YEARS.  THE START DATE WILL BE BACKED UP                   
*****>   FROM 1 TO 6 YEARS IN THAT CASE.                                        
*        FROM 2 TO 7 YEARS IN THAT CASE.                                        
*        (NOTE:  BECAUSE ROUTINE HAS BEEN CHANGED TO LOOK FORWARD               
*            1 YEAR (INTO NEXT YEAR), THE RANGE BACKWARD HAS BEEN               
*            INCREASED TO KEEP IT LOOKING BACK TO THE ORIGINAL YEAR)            
*                                                                               
*                                                                               
         CLI   DXU.RXFULLYR,C'1'   CHECK FOR YEAR ADJUSTMENT 1 - 7              
         BL    SDAT0040            NOT A YEAR ADJUSTMENT                        
         MVC   BYTE,DXU.RXFULLYR   YES - ADJUST THE START YEAR                  
         NI    BYTE,X'0F'          TURN OFF ZONE BITS                           
         CLI   BYTE,1              ADJUST BY 1 YR OR LESS?                      
         BNH   SDAT0040            YES - NO ADJUSTMENT                          
         GOTO1 VDATCON,DMCB,(2,CURBST),(3,WORK)                                 
*                                  NO  - CONVERT START DATE TO 3-BYTE           
         ZIC   RF,WORK             ADJUST START DATE                            
         ZIC   RE,BYTE             SET ADJUSTMENT FACTOR                        
****>    BCTR  RE,0                MAKE ZERO RELATIVE                           
         SR    RF,RE               SUBTRACT ADJUSTMENT FACTOR                   
         STC   RF,WORK                                                          
         GOTO1 VDATCON,DMCB,(3,WORK),(0,WORK+32)                                
         MVC   WORK+36(2),=C'15'                                                
         GOTO1 =V(GETBROAD),DMCB,WORK+32,WORK+12                                
         GOTO1 VDATCON,DMCB,(0,WORK+12),(2,CURBST)                              
SDAT0040 DS    0H                                                               
*&&DO                                                                           
*   TEST                                                                        
         LA    R0,15                                                            
         LA    RE,WORK                                                          
         LA    RF,CURBST                                                        
         DC    H'0'                                                             
*&&                                                                             
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REPFILE INITIALISATION                                              *         
***********************************************************************         
         SPACE 1                                                                
REPINIT  NTR1  BASE=*,LABEL=*                                                   
K        USING RREPKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         LA    R2,IO                                                            
         GOTO1 AREADHI                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RREPKEY),0(R2)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
R        USING RREPREC,R2                                                       
         MVC   MASALPHA,R.RREPKREP   REP COMPANY                                
         CLC   R.RREPMAST,SPACES      NO MASTER CONTROL                         
         JNH   REPINI10                                                         
         CLC   R.RREPMAST,=X'FFFF'    THIS IS THE MASTER                        
         JE    REPINI10                                                         
*                                                                               
         MVC   MASALPHA,R.RREPMAST   SET THE MASTER                             
*                                                                               
REPINI10 DS    0H                                                               
         MVI   REPPFLG,0                                                        
         CLI   R.RREPPROF+10,C'Y'                                               
         JNE   *+8                                                              
         OI    REPPFLG,X'80'                                                    
         CLI   R.RREPPROF+15,C'B'                                               
         JNE   *+8                                                              
         OI    REPPFLG,X'40'                                                    
*                                                                               
*   OPTION TO INCLUDE PEND/FC/BB IS BEING TAKEN FROM RXUSERD.  CODE             
*        IS LEFT IN FOR FUTURE USE.                                             
*                                                                               
         CLI   DXU.RXPNDFC,C'Y'    INCLUDE BUSINESS REPORTING RECORDS?          
         JNE   *+8                 NO                                           
         OI    REPPFLG,X'20'       YES - SET OPTION                             
         DROP  R                                                                
*                                                                               
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RXRECID                                                                       
***********************************************************************         
RXRECIDT DS    0C                                                               
       ++INCLUDE RXRECID12                                                      
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* BROADCAST DATE BLOCK                                                          
***********************************************************************         
DATEBLK  DS    0C                                                               
CURBST   DS    XL2                 COMPRESSED CURRENT BROADCAST START           
CURBND   DS    XL2                 COMPRESSED CURRENT BROADCAST END             
PRIBST   DS    XL2                 COMPRESSED PRIOR BROADCAST START             
PRIBND   DS    XL2                 COMPRESSED PRIOR BROADCAST END               
WEEKSTRT DS    XL2                 COMPRESSED WEEK START DATE                   
WEEKEND  DS    XL2                 COMPRESSED WEEK END   DATE                   
SWISTRT  DS    XL3                 BINARY SWITCH DATE                           
SWIADJ   DS    XL1                 BINARY SWITCH DATE ADJUSTMENT                
         SPACE 3                                                                
***********************************************************************         
* FAKE SPACEND TABLE FOR SETVAL IN RXROUTS                                      
***********************************************************************         
STAXREC DC     XL72'0000'                                                       
         SPACE 3                                                                
***********************************************************************         
* AREA USED FOR DETERMINING WHETHER CHANGES NEED TO BE EXTRACTED                
***********************************************************************         
COPYBUFF DS    CL10000                                                          
*                                                                               
STABLOCK DS    50000C              STATION TABLE AREA                           
CONBLOCK DS    150000C             CONFIRM TABLE AREA                           
CONFLEN  EQU   *-CONBLOCK                                                       
CONFMAX  EQU   ((CONFLEN/4)-2)                                                  
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORGE AREA                                                     
***********************************************************************         
LOCLWORK EQU   *                                                                
ANEXTSET DS    A                                                                
*                                                                               
LBLDAREA DS    F                                                                
ABLDAREA DS    A                                                                
ASTNAREA DS    A                                                                
ANEXTSTA DS    A                                                                
ACONAREA DS    A                                                                
ANEXTCON DS    A                                                                
STATABFL DS    F                   STATION TABLE FLAG                           
STATABCT DS    F                                                                
CONTABFL DS    F                   CONFIRM TABLE FLAG                           
CONTABCT DS    F                                                                
IOCTR    DS    F                                                                
*                                                                               
STABREP  DC    CL2'  '                                                          
*                                                                               
***********************************************************************         
* TF SSB                                                              *         
***********************************************************************         
*&&DO                                                                           
         DS    0L                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      CSECT                                                                  
         DC    H'0'                                                             
         DC    X'FF'               OFFLINE EXTENDED                             
         DC    XL256'00'                                                        
*&&                                                                             
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         DC    X'02'               FOR DATAMGR (OFFLINE NO RECOVERY)            
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* COPY OF COMFACS FOR THOSE DAMN DEMOS                                *         
***********************************************************************         
         SPACE 1                                                                
COMFACS  CSECT                     COMMON FACILITIES LIST                       
         DC    V(DATAMGR)                                                       
         DC    A(0)                CALLOFF)                                     
         DC    A(0)                GETMSG)                                      
         DC    A(0)                GETTXT)                                      
         DC    A(0)                SWITCH)                                      
         DC    A(0)                HELLO)                                       
         DC    A(0)                SCANNER)                                     
         DC    A(0)                UNSCAN)                                      
         DC    A(0)                HEXIN)                                       
         DC    V(HEXOUT)                                                        
         DC    A(0)                CASHVAL)                                     
         DC    A(0)                DATVAL)                                      
         DC    V(DATCON)                                                        
         DC    A(0)                TERMVAL)                                     
         DC    A(0)                SCUNKEY)                                     
         DC    V(ADDAY)                                                         
         DC    V(GETDAY)                                                        
         DC    A(0)                GETPROF)                                     
         DC    V(PERVERT)                                                       
         DC    A(0)                GETFACT)                                     
         DC    A(0)                XSORT)                                       
         DC    A(0)                REQTWA)                                      
         DC    A(0)                GETFLD)                                      
*&&UK                                                                           
         DC    V(PERVAL)                                                        
         DC    V(DLFLD)                                                         
         DC    V(GENERAL)                                                       
         DC    18A(0)                                                           
*&&                                                                             
*&&US                                                                           
         DC    A(0)                DDISPSRT)                                    
VDEMADDR DC    A(0)                DEMADDR)                                     
         DC    A(0)                DEMDISP)                                     
         DC    A(0)                DBOOK)                                       
         DC    A(0)                DSTATION)                                    
         DC    A(0)                DMASTER)                                     
         DC    A(0)                DFORMULA)                                    
         DC    A(0)                DNAME)                                       
         DC    A(0)                DCODE)                                       
         DC    A(0)                DCONTROL)                                    
         DC    A(0)                DADJUST)                                     
         DC    A(0)                DEMOUT)                                      
         DC    A(0)                DEMEL)                                       
         DC    A(0)                DEMAINT)                                     
         DC    A(0)                DEMAND)                                      
         DC    A(0)                DEMOMATH)                                    
         DC    A(0)                DEMOVAL)                                     
         DC    A(0)                GENERAL)                                     
         DC    V(PERVAL)                                                        
         DC    A(0)                DLFLD)                                       
         DC    A(0)                                                             
*&&                                                                             
         DC    A(0)                GLOBBER)                                     
         DC    A(0)                MINIO)                                       
         DC    A(0)                PARSNIP)                                     
         DC    A(0)                DICTATE)                                     
         DC    A(0)                EDITOR)                                      
         DC    A(0)                GETHELP)                                     
         DC    A(0)                CUREDIT)                                     
         DC    A(0)                GETRET)                                      
         DC    A(0)                REPORT)                                      
         DC    A(0)                BLDCUR)                                      
         DC    A(0)                GETCUR)                                      
         DC    A(0)                GETNARR)                                     
         DC    A(0)                DEJAVU)                                      
         DC    A(0)                SECRET)                                      
         DC    A(0)                BILLIT)                                      
         DC    A(0)                                                             
         DC    A(0)                PQPROF)                                      
         DC    2A(0)                                                            
         DC    A(0)                BINSRCH)                                     
         DC    A(0)                PROTON)                                      
         DC    A(0)                PROTOFF)                                     
         DC    A(0)                HELEN)                                       
         DC    A(0)                MQIO)                                        
         DC    A(0)                EUREKA                                       
         DC    V(LOCKUP)                                                        
         DC    V(MASTC)            MASTC                                        
         DC    V(LOCKSPC)          LOCKSPACE                                    
         DC    8A(0)               SPARE                                        
         EJECT                                                                  
***********************************************************************         
* MASTC CSECT                                                         *         
***********************************************************************         
         SPACE 1                                                                
MASTC    CSECT                                                                  
       ++INCLUDE DDMASTC                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER GLOBAL WORKING STORAGE                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
         ORG   DMCB                                                             
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
PARM     DS    6F                                                               
DMWORK   DS    12D                                                              
ATYPTAB  DS    A                   A(TYPTAB) - TYPTABD                          
REPADDR  DS    CL4                                                              
IOKEY    DS    CL64                                                             
IOKEYSAV DS    CL42                                                             
BYTE     DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
RECTYPE  DS    XL1                                                              
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
REPALPHA DS    XL2                                                              
MASALPHA DS    XL2                                                              
VERSION  DS    XL1                                                              
PLATFORM DS    XL1                                                              
*                                                                               
SALEMAIL DS    CL60                                                             
*                                                                               
REPPFLG DS     XL1                 REP PROFILE FLAGS                            
*                                   X'80' #11 ZERO BUCKETS = Y                  
*                                   X'40' #16 SUPPRESS BACK BILLING = Y         
*                                   X'20' #25 INCLUDE BUS. REPORTING            
*                                        RECORDS                                
*                                                                               
TYPECODE DS    CL3                                                              
TYPENAME DS    CL3                                                              
TYPEDEEP DS    XL1                                                              
TYPEFLAG DS    XL1                                                              
TYPFLSET EQU   X'02'               RUN SETDATE DURING INIT                      
TYPEALOD DS    A                                                                
TYPEAUPD DS    A                                                                
*                                                                               
WORK     DS    XL128                                                            
SPARE    DS    XL32                                                             
*                                                                               
*&&DO                                                                           
*****MOVED TO THE LOCAL STORAGE AREA, YYUN, 12/09/05*******************         
ANEXTSET DS    A                                                                
*                                                                               
LBLDAREA DS    F                                                                
ABLDAREA DS    A                                                                
ASTNAREA DS    A                                                                
ANEXTSTA DS    A                                                                
ACONAREA DS    A                                                                
ANEXTCON DS    A                                                                
STATABFL DS    F                   STATION TABLE FLAG                           
STATABCT DS    F                                                                
CONTABFL DS    F                   CONFIRM TABLE FLAG                           
CONTABCT DS    F                                                                
IOCTR    DS    F                                                                
*&&                                                                             
***********************************************************************         
*                                                                               
*   ADDRESSES OF 'SETCTR' AND 'SETALPHA' ARE PASSED IN AS ONE                   
*        ARGUMENT.  DO NOT INSERT ANYTHING BETWEEN THESE TWO                    
*        FIELDS.  THANK YOU.                                                    
*                                                                               
SETCTR   DS    F                                                                
SETALPHA DS    CL2                 REPALPHA TO PASS IN                          
*                                                                               
SETKEY   DS    CL27                SAVE KEY AREA                                
SETKEY2  DS    CL27                SAVE KEY AREA                                
SETNAME  DS    CL4                 SET OF SETS NAME                             
*                                                                               
SETOFSET DS    CL120               SET OF SETS: SAVE 30X4 CHARS                 
*                                                                               
SETFLAG  DS    CL1                                                              
SETSETNO EQU   C'N'                NOT SET OF SETS                              
SETHDR   EQU   C'H'                SET OF SETS: PROCESS HEADER                  
SETDET   EQU   C'D'                SET OF SETS: PROCESS DETAILS                 
*                                                                               
RESTART  DS    CL1                 KEY SEQUENCE RESTART NEEDED                  
RESTARTY EQU   C'Y'                                                             
RESTARTN EQU   C'N'                                                             
*                                                                               
***MOVED TO LOCLWORK***STABREP  DS    CL2                                       
*                                                                               
TESTCTR  DS    F                                                                
*                                                                               
ARGBLOCK DS    6F                  ARGUMENT BLOCK FOR VREPDOLC CALLS            
         ORG   ARGBLOCK                                                         
ARGDXBLK DS    A                                                                
ARGIOKEY DS    A                                                                
ARGRSTRT DS    A                                                                
         DS    2F                  SPARE                                        
ARGBYTES DS    F                                                                
         ORG   ARGBYTES                                                         
CONFPART DC    CL1'C'              CONFIRM / PARTIAL FLAG                       
         DS    CL3                 SPARE                                        
         ORG                                                                    
*                                                                               
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    4096X                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TYPTAB TABLE                                         *         
***********************************************************************         
         SPACE 1                                                                
TYPTABD  DSECT                                                                  
TYPNAME  DS    XL3                 3 CHAR MNEMONIC FOR RECORD TYPE              
TYPLDEEP DS    XL1                 DEPTH INTO LEDGER FOR COMPARE (LOAD)         
TYPFLAG  DS    XL1                 FLAGS                                        
TYPFLTAB EQU   X'01'               LOAD/UPDATE ARE TABLE ADDRESSES              
TYPVER   DS    XL1                 VERSION                                      
         DS    XL2                 N/D                                          
TYPLOAD  DS    XL4                 A(LOAD ROUTINE)                              
TYPUPDT  DS    XL4                 A(UPDATE ROUTINE)                            
TYPTABLQ EQU   *-TYPTABD                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER COMMON ADDRESSES                                     *         
***********************************************************************         
         SPACE 1                                                                
ADDRESSD DSECT                                                                  
COMMON   DS    CL8                                                              
VDATAMGR DS    V                                                                
VDMOD000 DS    V                                                                
VDADDS   DS    V                                                                
VLOGIO   DS    V                                                                
VDATCON  DS    V                                                                
VREPCNVX DS    V                                                                
***PDACC DS    V                                                                
***PDTRC DS    V                                                                
VREPCONC DS    V          CONTRACT HEADER                                       
VREPDOLC DS    V          CONTRACT DOLLARS                                      
VREPAGYC DS    V          AGENCY                                                
VREPADVC DS    V          ADVERTISER                                            
VREPSALC DS    V          SALESPERSON                                           
VREPCTYC DS    V          CONTRACT TYPE                                         
VREPDCTC DS    V          DEV CONTRACT TYPE                                     
VREPSTAC DS    V          STATION                                               
VREPDSPC DS    V          DEV SALESPERSON                                       
VREPTEMC DS    V          TEAM                                                  
VREPPRDC DS    V          PRODUCT CODE                                          
VREPCLSC DS    V          CLASS                                                 
VREPCATC DS    V          CATEGORY                                              
VREPOFFC DS    V          OFFICE                                                
VREPOWNC DS    V          OWNER                                                 
VREPMKTC DS    V          MARKET                                                
VREPGRPC DS    V          GROUP/SUBGROUP                                        
VREPRGNC DS    V          REGION                                                
VREPTERC DS    V          TERRITORY                                             
VREPMSTC DS    V          SEE ME/READ ME SUB/MASTER RECORD                      
VREPREPC DS    V          REP INFO                                              
VREPBD1C DS    V          STATION BUDGET INFO                                   
VREPBD2C DS    V          OFFICE  BUDGET INFO                                   
VREPPTPC DS    V          POINT PERSON                                          
VREPSETC DS    V          SET CODES                                             
VREPSWIC DS    V          SWITCH CODES                                          
VREPCPTC DS    V          COMPETITIVE STATIONS                                  
VREPDPTC DS    V          DAYPART/CPP                                           
VREPLENC DS    V          LENGTH CODES                                          
VREPBKSC DS    V          BOOKS                                                 
VREPCCOC DS    V          CONFIRM COMMENT                                       
VREPSCMC DS    V          STANDARD COMMENT                                      
VREPOFCC DS    V          OFFICE   COMMENT                                      
VREPEAGC DS    V          EOP AGENCY                                            
VREPEADC DS    V          EOP ADVERT                                            
VREPEOFC DS    V          EOP OFFICE                                            
VREPESPC DS    V          EOP SALESPERSON                                       
VREPBCDC DS    V          BUYCODE                                               
VREPBUYC DS    V          BUY RECORD                                            
         SPACE 1                                                                
         DS    CL8                 COMMON INTERNAL ROUTINES                     
AACCLOAD DS    A                                                                
AACCUPDT DS    A                                                                
ADECIOC  DS    A                                                                
         DS    A                                                                
         DS    A                   SPARE                                        
AGETTYP  DS    A                                                                
AGETIT   DS    A                                                                
AREADHI  DS    A                                                                
ARECCMP  DS    A                                                                
         SPACE 1                                                                
         DS    CL8                 LOAD ROUTINES                                
ALOADCON DS    A          CONTRACT HEADER                                       
ALOADDOL DS    A          CONTRACT DOLLARS                                      
ALOADAGY DS    A          AGENCY                                                
ALOADADV DS    A          ADVERTISER                                            
ALOADSAL DS    A          SALESPERSON                                           
ALOADCTY DS    A          CONTRACT TYPE                                         
ALOADDCT DS    A          DEV CONTRACT TYPE                                     
ALOADSTA DS    A          STATION                                               
ALOADDSP DS    A          DEV SALESPERSON                                       
ALOADTEM DS    A          TEAM                                                  
ALOADPRD DS    A          PRODUCT CODE                                          
ALOADCLS DS    A          CLASS                                                 
ALOADCAT DS    A          CATEGORY                                              
ALOADOFF DS    A          OFFICE                                                
ALOADOWN DS    A          OWNER                                                 
ALOADMKT DS    A          MARKET                                                
ALOADGRP DS    A          GROUP/SUBGROUP                                        
ALOADRGN DS    A          REGION                                                
ALOADTER DS    A          TERRITORY                                             
ALOADMST DS    A          SEE ME/READ ME SUB/MASTER RECORD                      
ALOADREP DS    A          REP INFO                                              
ALOADBD1 DS    A          STATION BUDGET INFO                                   
ALOADBD2 DS    A          OFFICE  BUDGET INFO                                   
ALOADPTP DS    A          POINT PERSON                                          
ALOADSET DS    A          SET RECORD                                            
ALOADSWI DS    A          SWITCH RECORD                                         
ALOADCPT DS    A          COMPETITIVE STATIONS                                  
ALOADDPT DS    A          DAYPART/CPP                                           
ALOADLEN DS    A          LENGTH CODES                                          
ALOADBKS DS    A          BOOKS                                                 
ALOADCCO DS    A          CONFIRM COMMENTS                                      
ALOADSCM DS    A          STANDARD COMMENT                                      
ALOADOFC DS    A          OFFICE   COMMENT                                      
ALOADEAG DS    A          EOP AGENCY                                            
ALOADEAD DS    A          EOP ADVERT                                            
ALOADEOF DS    A          EOP OFFICE                                            
ALOADESP DS    A          EOP SALESPERSON                                       
ALOADBCD DS    A          BUYCODE                                               
ALOADBUY DS    A          BUY                                                   
         SPACE 1                                                                
         DS    CL8                 UPDATE ROUTINES                              
AUPDTCON DS    A          CONTRACT HEADER                                       
AUPDTDOL DS    A          CONTRACT DOLLARS                                      
AUPDTAGY DS    A          AGENCY                                                
AUPDTADV DS    A          ADVERTISER                                            
AUPDTSAL DS    A          SALESPERSON                                           
AUPDTCTY DS    A          CONTRACT TYPE                                         
AUPDTDCT DS    A          DEV CONTRACT TYPE                                     
AUPDTSTA DS    A          STATION                                               
AUPDTDSP DS    A          DEV SALESPERSON                                       
AUPDTTEM DS    A          TEAM                                                  
AUPDTPRD DS    A          PRODUCT CODE                                          
AUPDTCLS DS    A          CLASS                                                 
AUPDTCAT DS    A          CATEGORY                                              
AUPDTOFF DS    A          OFFICE                                                
AUPDTOWN DS    A          OWNER                                                 
AUPDTMKT DS    A          MARKET                                                
AUPDTGRP DS    A          GROUP/SUBGROUP                                        
AUPDTRGN DS    A          REGION                                                
AUPDTTER DS    A          TERRITORY                                             
AUPDTMST DS    A          SEE ME/READ ME SUB/MASTER RECORD                      
AUPDTREP DS    A          REP INFO                                              
AUPDTBD1 DS    A          STATION BUDGET INFO                                   
AUPDTBD2 DS    A          OFFICE  BUDGET INFO                                   
AUPDTPTP DS    A          POINT PERSON                                          
AUPDTSET DS    A          SET RECORD                                            
AUPDTSWI DS    A          SWITCH RECORD                                         
AUPDTCPT DS    A          COMPETITIVE STATIONS                                  
AUPDTDPT DS    A          DAYPART/CPP                                           
AUPDTLEN DS    A          LENGTH CODES                                          
AUPDTBKS DS    A          BOOKS                                                 
AUPDTCCO DS    A          CONFIRM COMMENTS                                      
AUPDTSCM DS    A          STANDARD COMMENT                                      
AUPDTOFC DS    A          OFFICE   COMMENT                                      
AUPDTEAG DS    A          EOP AGENCY                                            
AUPDTEAD DS    A          EOP ADVERT                                            
AUPDTEOF DS    A          EOP OFFICE                                            
AUPDTESP DS    A          EOP SALESPERSON                                       
AUPDTBCD DS    A          BUYCODE                                               
AUPDTBUY DS    A          BUY RECORD                                            
         SPACE 1                                                                
         DS    CL8                 FILTER ROUTINES                              
AFILTCON DS    A          CONTRACT HEADER                                       
AFILTDOL DS    A          CONTRACT DOLLARS                                      
AFILTAGY DS    A          AGENCY                                                
AFILTADV DS    A          ADVERTISER                                            
AFILTSAL DS    A          SALESPERSON                                           
AFILTCTY DS    A          CONTRACT TYPE                                         
AFILTDCT DS    A          DEV CONTRACT TYPE                                     
AFILTSTA DS    A          STATION                                               
AFILTDSP DS    A          DEV SALESPERSON                                       
AFILTTEM DS    A          TEAM                                                  
AFILTPRD DS    A          PRODUCT CODE                                          
AFILTCLS DS    A          CLASS                                                 
AFILTCAT DS    A          CATEGORY                                              
AFILTOFF DS    A          OFFICE                                                
AFILTOWN DS    A          OWNER                                                 
AFILTMKT DS    A          MARKET                                                
AFILTGRP DS    A          GROUP/SUBGROUP                                        
AFILTRGN DS    A          REGION                                                
AFILTTER DS    A          TERRITORY                                             
AFILTMST DS    A          SEE ME/READ ME SUB/MASTER RECORD                      
AFILTREP DS    A          REP INFO                                              
AFILTBD1 DS    A          STATION BUDGET INFO                                   
AFILTBD2 DS    A          OFFICE  BUDGET INFO                                   
AFILTPTP DS    A          POINT PERSON                                          
AFILTSET DS    A          SET RECORD                                            
AFILTSWI DS    A          SWITCH RECORD                                         
AFILTCPT DS    A          COMPETITIVE STATIONS                                  
AFILTDPT DS    A          DAYPART/CPP                                           
AFILTLEN DS    A          LENGTH CODES                                          
AFILTBKS DS    A          BOOKS                                                 
AFILTCCO DS    A          CONFIRM COMMENT                                       
AFILTSCM DS    A          STANDARD COMMENT                                      
AFILTOFC DS    A          OFFICE   COMMENT                                      
AFILTEAG DS    A          EOP AGENCY                                            
AFILTEAD DS    A          EOP ADVERT                                            
AFILTEOF DS    A          EOP OFFICE                                            
AFILTESP DS    A          EOP SALESPERSON                                       
AFILTBCD DS    A          BUYCODE                                               
AFILTBUY DS    A          BUY RECORD                                            
         SPACE 1                                                                
         DS    CL8                 INITIALISATION ROUTINES                      
AINITALL DS    A                   GENERAL INITIALISATION                       
AINITCON DS    A          CONTRACT HEADER                                       
AINITDOL DS    A          CONTRACT DOLLARS                                      
AINITAGY DS    A          AGENCY                                                
AINITADV DS    A          ADVERTISER                                            
AINITSAL DS    A          SALESPERSON                                           
AINITCTY DS    A          CONTRACT TYPE                                         
AINITDCT DS    A          DEV CONTRACT TYPE                                     
AINITSTA DS    A          STATION                                               
AINITDSP DS    A          DEV SALESPERSON                                       
AINITTEM DS    A          TEAM                                                  
AINITPRD DS    A          PRODUCT CODE                                          
AINITCLS DS    A          CLASS                                                 
AINITCAT DS    A          CATEGORY                                              
AINITOFF DS    A          OFFICE                                                
AINITOWN DS    A          OWNER                                                 
AINITMKT DS    A          MARKET                                                
AINITGRP DS    A          GROUP/SUBGROUP                                        
AINITRGN DS    A          REGION                                                
AINITTER DS    A          TERRITORY                                             
AINITMST DS    A          SEE ME/READ ME SUB/MASTER RECORD                      
AINITREP DS    A          REP INFO                                              
AINITBD1 DS    A          STATION BUDGET INFO                                   
AINITBD2 DS    A          OFFICE  BUDGET INFO                                   
AINITPTP DS    A          POINT PERSON                                          
AINITSET DS    A          SET RECORD                                            
AINITSWI DS    A          SWITCH RECORD                                         
AINITCPT DS    A          COMPETITIVE STATIONS                                  
AINITDPT DS    A          DAYPART/CPP                                           
AINITLEN DS    A          LENGTH CODES                                          
AINITBKS DS    A          BOOKS                                                 
AINITCCO DS    A          CONFIRM COMMENT                                       
AINITSCM DS    A          STANDARD COMMENT                                      
AINITOFC DS    A          OFFICE   COMMENT                                      
AINITEAG DS    A          EOP AGENCY                                            
AINITEAD DS    A          EOP ADVERT                                            
AINITEOF DS    A          EOP OFFICE                                            
AINITESP DS    A          EOP SALESPERSON                                       
AINITBCD DS    A          BUYCODE                                               
AINITBUY DS    A          BUY RECORD                                            
         SPACE 1                                                                
DMOPEN   DS    CL7                                                              
DMREAD   DS    CL7                                                              
DMRSEQ   DS    CL7                                                              
DMRDHI   DS    CL7                                                              
DMCLSE   DS    CL7                                                              
DMFAST   DS    CL7                                                              
GETREC   DS    CL7                                                              
DMRFIL   DS    CL7                                                              
CONTROL  DS    CL7                                                              
CTFILE   DS    CL7                                                              
REPDIR   DS    CL7                                                              
REPFIL   DS    CL7                                                              
DMDA     DS    F                                                                
DTFADDR  DS    F                                                                
ACOPYBUF DS    A                                                                
ACTIVITY DS    CL1                                                              
SPACES   DS    CL80                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER STATION/OFFICE TABLE ENTRIES                         *         
***********************************************************************         
DSTATD   DSECT                                                                  
DSTACALL DS    CL5                                                              
DDSTAOFF EQU   *-DSTACALL          DISPLACEMENT TO OFFICES                      
DSTAOFFS DS    CL80                                                             
DSPARE   DS    CL15                                                             
DSTATDL  EQU   *-DSTATD                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER RECOVERY HEADER                                      *         
***********************************************************************         
         SPACE 1                                                                
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS REQUIRED FOR THIS PROGRAM                              *         
***********************************************************************         
         SPACE 1                                                                
* RXUSERD                                                                       
       ++INCLUDE RXUSERD1                                                       
* RXRECD                                                                        
       ++INCLUDE RXRECD12                                                       
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
         SPACE 1                                                                
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         SPACE 1                                                                
* DDPERVALD                                                                     
       ++INCLUDE DDPERVALD                                                      
         SPACE 1                                                                
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
         SPACE 1                                                                
* DMGREQUS                                                                      
       ++INCLUDE DMGREQUS                                                       
         SPACE 1                                                                
* REGENALL1A                                                                    
* REGENDSP                                                                      
* REREPQWKDD                                                                    
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REGENDSP                                                       
       ++INCLUDE REGENDCT                                                       
       ++INCLUDE REGENOBUD                                                      
       ++INCLUDE REGENSET                                                       
       ++INCLUDE REGENSWI                                                       
       ++INCLUDE REGENCFC                                                       
       ++INCLUDE REGENOCM                                                       
       ++INCLUDE REGENCMT                                                       
       ++INCLUDE REGENTER                                                       
       ++INCLUDE REGENEOP                                                       
       ++INCLUDE REGENSPEC                                                      
       ++INCLUDE REREPQWKDD                                                     
* FACTRYEQUS                                                                    
       ++INCLUDE FACTRYEQUS                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020RXTRACT12 02/20/13'                                      
         END                                                                    
