*          DATA SET SPSFM2D    AT LEVEL 045 AS OF 12/05/11                      
*PHASE T2172DA                                                                  
*                                                                               
***********************************************************************         
* HWON 045 01DEC11  PREVENT USERS FROM ADDING DUPE 3-CHAR STATION     *         
* AKAT 044 06JAN11  PREVENT USER FROM ADDING A NETDEF RECORD FOR A    *         
*                   STATION THAT HAS A NON-ZERO MARKET                *         
* AKAT 043 16OCT09  COST% MUST EQUAL 100% WHEN ADDING A NEW STA       *         
* AKAT 042 01OCT09  COST% MUST EQUAL 100% ON A EST SPECIFIC ADD AND   *         
*                   VALIDATE ESTIMATE                                 *         
* AKAT 041 23SEP09  COST% MUST EQUAL 100%                             *         
* PWES 039   JUL04  STOP NEW USE OF REGION + REORG SCREEN FIELD ORDER *         
* PWES 038 29APR02  DON'T CHECK FOR MASTER WHEN ADD & STATION IS ZZZZ *         
* PWES 037 25JAN02  ENSURE NDEFNET GREATER THAN 5 (NOTE CABLE IS 1 !) *         
* PWES 036 24OCT01  DELREC REFINEMENTS FOR LOWER LEVEL CHECK          *         
* PWES 035 20SEP01  ALWAYS CHECK FOR CLIENT LEVEL RECS WHEN ADD EST   *         
* PWES 034 19SEP01  DON'T SHOW MKTDEF/CBLDEF RECORDS                  *         
*                   DON'T ALLOW DELETE OF RECS IF LOWER LEVELS EXIST  *         
* ABEA 033 26JUN01  ADD SEPARATE AGENCY CHECK TO LR                   *         
* SEAN     10/29/99        MADE LIVE CODES THAT ALLOWS DDS TERMINALS  *         
*                          TO DELETE NETDEF RECORDS, AND ITS PASSIVE  *         
*                          KEYS.                                      *         
***********************************************************************         
*                                                                     *         
*  TITLE: T2172D - NETWORK DEFINITION MAINTENANCE                     *         
*                                                                     *         
*  CALLED FROM: SPOT CONTROLLER (T21700), WHICH CALLS                 *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  INPUTS: SCREENS SPSFMAF  (T217AF) -- MAINTENANCE                   *         
*          SCREENS SPSFMD0  (T217D0) -- LIST                          *         
*                                                                     *         
*  OUTPUTS: UPDATED NETWORK RECORDS                                   *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - DEFINITION RECORD                                     *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - SECOND BASE REGISTER                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T2172D NETWORK MAINTENANCE'                                     
T2172D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2172D*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD           GENERAL PRINT AREAS                         
         USING SPOOLD,R8                                                        
*                                                                               
*        XC    TEMPFLD,TEMPFLD                                                  
*        MVC   TEMPFLD(9),=XL9'0900000000010000D5' N FOR NETWORK                
*        LA    R2,TEMPFLD                                                       
*        GOTO1 VALIMED                                                          
*        CLI   SVAPROF+7,C'C'                                                   
*        BNE   EXIT                                                             
*                                                                               
         OI    GENSTAT4,NODELLST   NO DELETE FROM THE LIST                      
         OI    GENSTAT1,APPLIC                                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       ADD  REQUEST FOR NSL REP (44)                
         BE    REQREC                                                           
         CLI   MODE,XRECADD        AFTER ADDREC                                 
         BE    REQREC                                                           
         CLI   MODE,XRECPUT        AFTER PUTREC                                 
         BE    REQREC                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  VALIDATE KEY ROUTINE                              *          
**********************************************************************          
                                                                                
VK       LA    R4,MYKEY                                                         
         USING NDEFRECD,R4                                                      
         XC    MYKEY,MYKEY                                                      
*                                                                               
         MVC   NDEFKTYP,=X'0D11'   NETWORK RECORD ID                            
         MVC   NDEFKAGY,AGENCY     AGENCY ID                                    
*                                                                               
         BAS   RE,VNTWK            VALIDATE NETWORK                             
*                                                                               
         BAS   RE,VMED             VALIDATE MEDIA                               
*                                                                               
         LA    R2,NWKCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VKEX                SKIP CLIENT, ESTIMATE                        
*                                                                               
         BAS   RE,VCLT             VALIDATE CLIENT                              
*                                                                               
         CLI   ACTNUM,ACTLIST      BUTTOM LINE FOR LIST                         
         BE    VKEX                                                             
*                                                                               
         BAS   RE,HR               CHECK FOR HIEARARCHY OF FILTERS              
*                                                                               
         BAS   RE,VEST             VALIDATE ESTIMATE                            
*                                                                               
         B     VKEX                                                             
*                                                                               
VKERR    B     INV                 ERROR                                        
*                                                                               
VKEX     MVC   KEY,MYKEY                                                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  VALIDATE NETWORK                                  *          
**********************************************************************          
                                                                                
VNTWK    NTR1                                                                   
         LA    R4,MYKEY                                                         
         USING NDEFRECD,R4                                                      
*                                                                               
         LA    R2,NWKNTWKH         NETWORK                                      
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VN3                 NOT REQUIRED IF LIST                         
         CLI   LWKCLT,C'>'                                                      
         BNE   VN5                                                              
*                                                                               
VN3      GOTO1 ANY                 REQUIRED                                     
*                                                                               
VN5      LLC   R5,5(R2)                                                         
         LTR   R5,R5                                                            
         BNP   VNEX                CHECK CLIENT                                 
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   NDEFKNET(0),NWKNTWK       MOVE NETWORK INTO KEY                  
         OC    NDEFKNET,SPACES     REPLACE 0 BY SPACE                           
         MVC   SVNTWK,NDEFKNET                                                  
VNEX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  VALIDATE MEDIA                                    *          
**********************************************************************          
                                                                                
VMED     NTR1                                                                   
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000E3'  T FOR NETWORK                  
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  VALIDATE CLIENT                                   *          
**********************************************************************          
                                                                                
VCLT     NTR1                                                                   
         LA    R4,MYKEY                                                         
         USING NDEFRECD,R4                                                      
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VCLT10                                                           
*                                                                               
         CLI   8(R2),C'>'          START AT FILTER                              
         BNE   VCLT20              NO                                           
         MVC   TEMPCLT,8(R2)       SAVE TO RESTORE IT LATER                     
         ICM   R5,15,8(R2)                                                      
         SLL   R5,8                GET RID OF C'>'                              
         STCM  R5,15,8(R2)                                                      
         OC    8(4,R2),SPACES                                                   
         LLC   RF,5(R2)            DECRIMENT LENGTH                             
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BP    *+8                                                              
         B     INV                 ERROR                                        
         STC   RF,5(R2)                                                         
         OI    FLAG,CLSTART    START AT FILTER                                  
         B     VCLT25                                                           
*                                                                               
VCLT10   EQU   *                                                                
*                                                                               
         MVC   KEY,NDEFKEY         CHECK FOR NETWORK KEY ON FILE                
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEY(13),KEYSAVE     SAME KEY (NETWORK LEVEL ON FILE)?            
         JE    VCLT20              YES - SO CONTINUE                            
*                                                                               
         LA    R2,NWKNTWKH         ELSE - GET A(NETWORK FIELD)                  
         B     NFND                ERROR                                        
*                                                                               
VCLT20   EQU   *                                                                
*                                                                               
         NI    FLAG,X'FF'-CLSTART                                               
         GOTO1 VALICLT                                                          
*                                                                               
VCLT25   TM    FLAG,CLSTART                                                     
         BZ    VCLT30                                                           
         CLI   5(R2),2                                                          
         BL    VCERR                                                            
*                                                                               
         MVC   QCLT,SPACES                                                      
         MVI   QCLT+2,C'A'                                                      
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QCLT(0),8(R2)                                                    
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   VCERR                                                            
         MVC   8(L'TEMPCLT,R2),TEMPCLT  RESTORE ORIGINAL VALUE                  
         LLC   RF,5(R2)            INCREMENT LENGTH                             
         LA    RF,1(RF)                                                         
         STC   RF,5(R2)                                                         
*                                                                               
VCLT30   MVC   NDEFKCLT,BCLT       MOVE CLIENT INTO THE KEY                     
         MVC   SVCLT,BCLT                                                       
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  VALIDATE ESTIMATE                                 *          
**********************************************************************          
                                                                                
VEST     NTR1                                                                   
         LA    R4,MYKEY                                                         
         USING NDEFRECD,R4                                                      
*                                                                               
         LA    R2,NWKESTH          ESTIMATE                                     
         CLI   5(R2),0                                                          
         BE    VESTEX              SKIP ESTIMATE                                
         MVC   QPRD,=C'POL'        POL ESTIMATE SHOULD EXIST!                   
         GOTO1 VALIEST                                                          
*                                                                               
         MVC   NDEFKEST,BEST       SET BINARY ESTIMATE                          
*                                                                               
VESTEX   B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  CHECK FOR HIERARCHY OF FILTERS                    *          
**********************************************************************          
                                                                                
HR       NTR1                                                                   
         CLI   NWKCLTH+5,0         CLIENT?                                      
         BNE   HREX                                                             
         CLI   NWKESTH+5,0         ESTIMATE?                                    
         BE    HREX                                                             
         LA    R2,NWKCLTH                                                       
         B     MIS                 ERROR                                        
*                                                                               
HREX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*                  DISPLAY RECORD                                    *          
**********************************************************************          
                                                                                
DREC     NTR1                                                                   
*                                                                               
DR       BAS   RE,CLEAR            CLEAR FIELDS                                 
         BAS   RE,RGNPROT          ENSURE ALL REGION FLDS INIT PROT             
*                                                                               
         L     R3,AIO              CANNOT DISP/CHANGE MKTDEF/CBLDEF             
         MVI   ELCODE,NDEFNELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   *+12                ASSUME NOT CABLE                             
         CLI   2(R3),NDEFCABQ      CABLE RECORD ?                               
         BE    NFND                                                             
*                                                                               
         L     R3,AIO                                                           
         SR    R5,R5               INITIALIZE TOTAL PERCENTAGE COST             
         SR    R6,R6               ACCUMULATOR (# OF LINES ON SCREEN)           
         STC   R6,NUMLINES                                                      
         LA    R2,NWKSTATH                                                      
         MVI   ELCODE,NDEFELQ      X'01' ELEMENT                                
         BAS   RE,GETEL                                                         
DR10     BNE   DREX                                                             
         USING NDEFEL01,R3                                                      
*                                                                               
         CLI   NUMLINES,MAXL       FULL SCREEN?                                 
         BE    DREX                                                             
         LA    R6,1(R6)            INCREMENT ACCUMULATOR                        
         STC   R6,NUMLINES                                                      
                                                                                
*------- STATION ----->                                                         
         MVC   FHDAD(L'NDEFSTA,R2),NDEFSTA                                      
         OI    FHOID(R2),FHOITR                                                 
         BAS   RE,NEXT             GO TO NEXT FIELD                             
*                                                                               
*------- COST & TOTAL COST ------->                                             
         CLC   NDEFPCT,=F'-1'      NOT BOUGHT                                   
         BNE   *+14                                                             
         MVC   FHDAD(L'NWKCOST,R2),=CL7'NB'                                     
         B     DR15                                                             
         EDIT  NDEFPCT,(L'NWKCOST,8(R2)),3,ALIGN=LEFT                           
         ICM   R4,15,NDEFPCT       NDEFPCT IS NOT ALIGNED                       
         AR    R5,R4               TOTAL PERCENTAGE COST                        
DR15     OI    FHOID(R2),FHOITR                                                 
         BAS   RE,NEXT                                                          
*                                                                               
*------- OFFSET ------>                                                         
         EDIT  NDEFOSET,(L'NWKOFF,8(R2)),0,FLOAT=-,ZERO=BLANK,         >        
               ALIGN=LEFT                                                       
         OI    FHOID(R2),FHOITR                                                 
         BAS   RE,NEXT                                                          
*                                                                               
*------- REGION ------>                                                         
         OC    NDEFRGN,NDEFRGN     TEST ANY REGION IN USE                       
         BZ    DR20                                                             
         TM    FHATD(R2),FHATPR    TEST FIELDS ALREADY UNPROT                   
         BZ    *+8                                                              
         BAS   RE,RGNUNPR          RECORD USES REGION - UNPROT ALL FLDS         
         MVC   8(L'NDEFRGN,R2),NDEFRGN                                          
         OI    FHOID(R2),FHOITR                                                 
DR20     BAS   RE,NEXT             BUMP TO NEXT FIELD                           
         BAS   RE,NEXTEL           FIND NEXT ELEMENT                            
         B     DR10                                                             
*                                                                               
DREX     EDIT  (R5),(L'NWKTOT,NWKTOT),3,ALIGN=LEFT   % OF NETWORK BUY           
         OI    NWKTOTH+6,X'80'                                                  
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   EXIT                                                             
         LA    R2,CONACTH                                                       
         B     REQOV               REPORT WILL BE GENERATED OV                  
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
*                  VALIDATE RECORD                                   *          
**********************************************************************          
                                                                                
VR       MVC   MYKEY,KEY                                                        
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         MVC   0(13,R3),MYKEY                                                   
         OI    GENSTAT2,RETEQSEL   REDISPLAY SELECTION FROM LIST                
*                                                                               
         CLI   NWKASTAH+5,0                                                     
         BE    VR10                                                             
         OC    NWKASTA,SPACES                                                   
*                                                                               
         CLC   =C'$DEL',NWKASTA    USER WANTS TO DELETE THE RECORD?             
         BNE   VR5                                                              
         BAS   RE,DELREC           CALL THE DELETE ROUTINE                      
         TM    15(R3),X'80'                                                     
         BNO   *+8                                                              
         BAS   RE,DELPKEY          DELETE PASSIVE POINTERS                      
         B     VREX10              EXIT THE PROGRAM                             
*                                                                               
VR5      LA    R2,NWKACOSH                                                      
         GOTO1 ANY                                                              
         CLC   =C'DEL',NWKACOS                                                  
         BE    INV                                                              
*        CLI   NWKCLTH+5,0         **NETWORK LEVEL - WHY??????                  
*        BE    VR7                 **ADD AFTER FIELD ISN''T REQUIRED            
*        LA    R2,NWKADDAH                                                      
*        GOTO1 ANY                                                              
                                                                                
*--ADD NEW STATION TO THE EXISTING RECORD OR ---------------->                  
*--ADD NEW NETWORK RECORD------------------------------------>                  
                                                                                
VR7      BAS   RE,ADDR                                                          
         B     VREX                                                             
                                                                                
*------------------------------------------------------------>                  
                                                                                
VR10     CLI   NWKACOSH+5,0                                                     
         BE    VR20                                                             
         LA    R2,NWKASTAH                                                      
         GOTO1 ANY                                                              
         LA    R2,NWKAAFTH                                                      
         GOTO1 ANY                                                              
*                                                                               
VR20     CLI   NWKAAFTH+5,0                                                     
         BE    VR30                                                             
         LA    R2,NWKASTAH                                                      
         GOTO1 ANY                                                              
         LA    R2,NWKACOSH                                                      
         GOTO1 ANY                                                              
*                                                                               
VR30     CLI   ACTNUM,ACTADD                                                    
         BNE   VR40                                                             
         MVI   NUMLINES,0          CLEAR DISPLAYED LINES ON ADD                 
                                                                                
*-- ADD NEW CLIENT RECORD ?  ---------------------------->                      
                                                                                
         CLI   NWKCLTH+5,0         CLIENT LEVEL?                                
         BNE   ADDCLT              ADD CLIENT WITH NETWORK STATIONS             
         LA    R2,NWKASTAH                                                      
         GOTO1 ANY                                                              
*                                                                               
VR40     BAS   RE,CHANGE           USER WANTS TO CHANGE DATA OR                 
*                                  REMOVE A STATION                             
VREX     DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VREX10                                                           
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                RESTORE ORIGINAL SEQUENCE                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
VREX10   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*                  BUMP THE POINTER                                  *          
**********************************************************************          
                                                                                
NEXT     NTR1                                                                   
         LLC   R4,0(R2)                                                         
         AR    R2,R4               BUMP TO NEXT FIELD                           
*                                                                               
NEXTEX   XIT1  REGS=(R2)        DON'T RESTORE OLD VALUE OF R2                   
         EJECT                                                                  
**********************************************************************          
*                  CLEAR FIELDS ROUTINE                              *          
**********************************************************************          
                                                                                
CLEAR    NTR1                                                                   
         BAS   RE,CLEAR1           CLEAR ADD NEW STATION FIELDS                 
*                                                                               
         BAS   RE,CLEAR2           CLEAR SCREEN                                 
*                                                                               
CLEAREX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  CLEAR ADD NEW STATION FIELDS                      *          
**********************************************************************          
                                                                                
CLEAR1   NTR1                                                                   
         LA    R2,NWKASTAH         FIRST FIELD TO CLEAR                         
         LA    R6,NWKAREGH         LAST FIELD                                   
*                                                                               
CLEAR5   CR    R2,R6               HAVE ADD FIELDS BEEN CLEARED?                
         BH    CLEAR1EX                                                         
         LLC   R5,5(R2)            TOTAL LENGTH                                 
*        SH    R5,=H'8'            MAX LENGTH OF DATA                           
         LTR   R5,R5               NO DATA?                                     
         BNP   CLEAR7                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         OI    6(R2),X'80'                                                      
*                                                                               
CLEAR7   BAS   RE,NEXT             NEXT FIELD                                   
         BAS   RE,NEXT                                                          
         B     CLEAR5                                                           
*                                                                               
CLEAR1EX B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  CLEAR SCREEN                                      *          
**********************************************************************          
                                                                                
CLEAR2   NTR1                                                                   
         LA    R2,NWKSTATH         FIRST FIELD TO CLEAR                         
         LA    R6,NWKLAST          LAST FIELD                                   
*                                                                               
CLEAR10  CR    R2,R6               ALL FIELDS HAVE BEEN CLEARED?                
         BNL   CLEAR2EX                                                         
         LLC   R5,0(R2)            TOTAL LENGTH                                 
         SHI   R5,8                MAX LENGTH OF DATA                           
         LTR   R5,R5               NO DATA?                                     
         BNP   CLEAR20                                                          
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         OI    6(R2),X'80'                                                      
*                                                                               
CLEAR20  BAS   RE,NEXT             NEXT FIELD                                   
         B     CLEAR10                                                          
*                                                                               
CLEAR2EX B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  ADD NEW NETWORK, RECORD, STATION                  *          
**********************************************************************          
                                                                                
ADDR     NTR1                                                                   
*                                                                               
         L     R3,AIO                                                           
         BAS   RE,ADD02            CREATE AND BUILD 02 ELEMENT                  
         BAS   RE,ADDSTAT          ADD STATION                                  
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*     CHANGE EXISTING RECORDS                                        *          
**********************************************************************          
                                                                                
CHANGE   NTR1                                                                   
         XC    TOTALPCT,TOTALPCT                                                
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL            FIRST ELEMENT                                
         BNE   CHANGEX                                                          
*                                                                               
CH5      LA    R2,NWKSTATH          START POSITION                              
         LA    R6,NWKENDH           LIMIT                                       
*                                                                               
CH10     CR    R2,R6                                                            
         BNL   CHANGEX                                                          
*                                                                               
         GOTO1 RECUP,DMCB,(0,AIO),(R3)     REMOVE AN ELEMENT                    
*                                                                               
         LR    R5,R2               POINT R5 TO COST FIELD                       
         SR    RF,RF                                                            
         IC    RF,0(R2)            (NEXT FIELD AFTER STATION)                   
         AR    R5,RF                                                            
* AFTER BUILDEL R2 POINTS TO REGION FIELD                                       
         BAS   RE,BUILDEL          BUILD AN ELEMENT                             
         CLC   =C'DEL',FHDAD(R5)                                                
         BNE   CH15                                                             
*                                                                               
         XR    R2,R5               SWAP R2 FOR R5 INCASE ERROR                  
         XR    R5,R2                                                            
         XR    R2,R5                                                            
         MVC   SVADDR,DMDSKADD                                                  
         BAS   RE,STADEL      DOES ST APPEARS IN ANY NET REC BELOW IT?          
         MVC   DMDSKADD,SVADDR                                                  
         XR    R2,R5               RESTORE ORIGINAL R2 (& R5)                   
         XR    R5,R2                                                            
         XR    R2,R5                                                            
*                                                                               
         BAS   RE,NEXT                                                          
         CLC   ELCODE,0(R3)        DOUBLE CHECK                                 
         BE    CH10                                                             
         B     CHANGEX                                                          
*                                                                               
CH15     GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R3)   ADD AN ELEMENT                    
         BAS   RE,NEXT             NEXT STATION                                 
         BAS   RE,NEXTEL           NEXT ELEMENT                                 
         BE    CH10                                                             
*                                                                               
CHANGEX  CLC   TOTALPCT,=F'0'       IS TOTAL COST% 0?                           
         BE    EXIT                 YES                                         
         CLC   TOTALPCT,=F'100000'  IS TOTAL COST% 100?                         
         BE    EXIT                 YES                                         
         EDIT  TOTALPCT,(L'NWKTOT,NWKTOT),3,ALIGN=LEFT                          
         OI    NWKTOTH+6,X'80'                                                  
         LA    R2,NWKCOSTH          POINT TO FIRST COST%                        
         B     PCTERR               AND GIVE AN ERROR                           
* ******************************************************* *                     
* DOES STATION APPEARS IN ANY NETWORK DEFINITION BELOW IT *                     
* IF IT DOES THAN CAN NOT DELETE THE STATION              *                     
* ******************************************************* *                     
STADEL   NTR1                                                                   
         OC    MYKEY+10(1),MYKEY+10 ESTIMATE LEVEL?                             
         BO    SDEX                 YES - NOTHING TO CHECK                      
*                                                                               
         MVC   KEY,MYKEY                                                        
         MVC   AIO,AIO2                                                         
         LA    R4,ELEM                                                          
         GOTO1 HIGH                                                             
         B     SD20                                                             
*                                                                               
SD05     EQU   *                                                                
         OC    MYKEY+8(2),MYKEY+8  CLIENT LEVEL?                                
         BZ    *+18                NO                                           
         CLC   KEY(10),MYKEY       CHECK ALL ESTIMATES RECORDS                  
         BNE   SDEX                                                             
         B     *+14                                                             
         CLC   KEY(8),MYKEY        CHECK ALL CLIENT, EST RECORDS                
         BNE   SDEX                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ      X'01'                                        
         BAS   RE,GETEL                                                         
*                                                                               
SD10     EQU   *                                                                
         CLC   2(4,R4),2(R3)       HAS STATION BEEN FOUND?                      
         BE    DELERR              YES                                          
         BAS   RE,NEXTEL                                                        
         BE    SD10                                                             
*                                                                               
SD20     EQU   *                                                                
         GOTO1 SEQ                                                              
         B     SD05                                                             
*                                                                               
SDEX     EQU   *                                                                
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE                                      
*        GOTO1 HIGH                                                             
*        GOTO1 GETREC                                                           
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*     ADD A CLIENT WITH ALL NETWORK STATIONS                         *          
**********************************************************************          
                                                                                
ADDCLT   XC    TOTALPCT,TOTALPCT                                                
         BAS   RE,ADD02            ADD 02 ELEMENT                               
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFNETQ     X'02'                                        
         BAS   RE,GETEL                                                         
         LR    R4,R3               R4 POINTS TO AIO1                            
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R5,8                INITIALIZE, 8 BYTES TO COMPARE               
         MVC   KEY(8),MYKEY        NETWORK                                      
         CLI   NWKESTH+5,0                                                      
         BE    *+14                                                             
         MVC   KEY+8(2),MYKEY+8    CLIENT                                       
         LA    R5,2(R5)            NEED TO COMPARE 10 BYTES                     
         GOTO1 HIGH                                                             
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BE    ADDCLT5                                                          
         LA    R2,NWKNTWKH                                                      
         B     NFND                ERROR                                        
*                                                                               
ADDCLT5  GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ      X'01'                                        
         BAS   RE,GETEL                                                         
*                                                                               
ACLT10   BNE   ADDCLTEX                                                         
*                                                                               
         USING NDEFEL01,R3                                                      
         ICM   R5,15,NDEFPCT                                                    
         C     R5,=F'-1'                                                        
         BE    ACLT15                                                           
         L     R6,TOTALPCT                                                      
         AR    R6,R5                                                            
         ST    R6,TOTALPCT                                                      
ACLT15   GOTO1 RECUP,DMCB,(0,AIO1),(R3),(R4)   ADD AN ELEMENT                   
         LA    R4,ELEMLEN(R4)                                                   
         BAS   RE,NEXTEL           NEXT ELEMENT                                 
         BE    ACLT10                                                           
*                                                                               
ADDCLTEX MVC   AIO,AIO1                                                         
         BAS   RE,DREC                                                          
         CLC   TOTALPCT,=F'0'       IS TOTAL COST% 0?                           
         BE    EXIT                 YES                                         
         CLC   TOTALPCT,=F'100000'  IS TOTAL COST% 100?                         
         BE    EXIT                 YES                                         
         EDIT  TOTALPCT,(L'NWKTOT,NWKTOT),3,ALIGN=LEFT                          
         OI    NWKTOTH+6,X'80'                                                  
         LA    R2,NWKCOSTH          POINT TO FIRST COST%                        
         B     PCTERR2              AND GIVE AN ERROR                           
         EJECT                                                                  
                                                                                
**********************************************************************          
*        ADD NEW STATTION                                            *          
**********************************************************************          
                                                                                
ADDSTAT  NTR1                                                                   
         LA    R2,NWKASTAH                                                      
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   ADDST1                                                           
         MVI   NUMLINES,0          CLEAR NUMLINES ON ADDREC                     
*                                                                               
         CLI   NWKESTH+5,0         CLIENT MUST EXIST IF EST ADD                 
         BE    ADDST1                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(10),MYKEY                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         CLC   KEY(10),MYKEY                                                    
         BE    ADDST1                                                           
         LA    R2,NWKCLTH                                                       
         B     NFND                                                             
*                                                                               
ADDST1   BAS   RE,BUILDEL          BUILD AN ELEMENT                             
         L     R3,AIO                                                           
         LLC   R5,NUMLINES                                                      
         LA    R2,NWKAAFTH                                                      
                                                                                
* --------- ADD AS A FIRST OR LAST ELEMENT ----->                               
                                                                                
         OC    8(4,R2),SPACES                                                   
         CLC   8(4,R2),SPACES                                                   
         BE    ADDST2                                                           
         CLC   8(4,R2),=C'FRST'                                                 
         BNE   ADDST3                                                           
         CLI   ACTNUM,ACTADD                                                    
         BE    ADDST3                                                           
*                                                                               
ADDST2   DS    0H                                                               
         USING NDEFRECD,R3                                                      
         LA    R3,NDEFEL           DISPLACEMENT TO 1 ELEMENT                    
         DROP  R3                                                               
         B     ADDST30                                                          
*                                                                               
ADDST3   CLI   ACTNUM,ACTADD       FIRST IS ONLY ALLOWED OPTION                 
         BNE   ADDST4              IF ACTION IS ADD                             
         LA    R2,NWKAAFTH                                                      
         B     INV                 ERROR                                        
*                                                                               
ADDST4   CLC   8(4,R2),=C'LAST'                                                 
         BNE   ADDST5                                                           
*                                                                               
         MVI   ELCODE,NDEFELQ        X'01'                                      
         BAS   RE,GETEL                                                         
*                                                                               
ADDST4A  BNE   ADDST30             FIND LAST ELEMENT                            
         BAS   RE,NEXTEL                                                        
         B     ADDST4A                                                          
                                                                                
* ---------------------------------------------->                               
                                                                                
ADDST5   MVI   ELCODE,NDEFELQ      X'01'                                        
         BAS   RE,GETEL                                                         
         USING NDEFEL01,R3                                                      
*                                                                               
ADDST10  OC    8(L'NWKAAFT,R2),SPACES                                           
         CLC   NDEFSTA,8(R2)       FIND MATCHING ELEMENT                        
         BE    ADDST20                                                          
         BAS   RE,NEXTEL           X'01'                                        
         BCT   R5,ADDST10                                                       
*                                                                               
         LA    R2,NWKAAFTH                                                      
         B     NFND                ERROR                                        
*                                                                               
ADDST20  LLC   R4,1(R3)            SKIP THIS ELEMENT                            
         AR    R3,R4                                                            
*                                                                               
ADDST30  GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R3)   ADD AN ELEMENT                    
*                                                                               
ADDSTEX  XC    TOTALPCT,TOTALPCT                                                
         L     R3,AIO              A(NETDEF RECORD)                             
*                                                                               
         MVI   ELCODE,NDEFELQ      X'01'                                        
         BAS   RE,GETEL            GET THE FIRST X'01' ELEMENT                  
         B     *+8                                                              
*                                                                               
ADDSTEX1 BAS   RE,NEXTEL           HAVE ANOTHER X'01' ELEMENT?                  
         BNE   CHANGEX             NO - CHECK FOR NET% ERRORS                   
         ICM   R5,15,NDEFPCT       NETWORK%                                     
         C     R5,=F'-1'           NOT BOUGHT?                                  
         BE    ADDSTEX1            YES - DON'T COUNT THIS TOWARDS%              
         L     R6,TOTALPCT         ADD THIS NETWORK%                            
         AR    R6,R5                                                            
         ST    R6,TOTALPCT                                                      
         B     ADDSTEX1            LOOK FOR THE NEXT X'01' ELEMENT              
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
*                  BUILD AN ELEMENT                                  *          
* ENTRY - R2=A(STATION FIELD)                                        *          
**********************************************************************          
                                                                                
BUILDEL  NTR1                                                                   
         LA    R4,ELEM                                                          
         USING NDEFEL01,R4                                                      
         XC    ELEM,ELEM                                                        
         MVI   ELEM,NDEFELQ        X'01'                                        
         MVI   ELEM+1,ELEMLEN                                                   
*                                                                               
         CLI   NWKASTAH+5,0                                                     
         BE    B10                 NO NEED TO VALIDATE STATION                  
*                                  IF CHANGING EXISTING RECORDS                 
*                                                                               
         BAS   RE,VALSTA           VALIDATE STATION                             
*                                                                               
B10      MVC   NDEFSTA,8(R2)                                                    
         OC    NDEFSTA,SPACES                                                   
         BAS   RE,NEXT             BUMP TO COST FIELD                           
         CLI   NWKASTAH+5,0        MORE BUMPS REQUIRED?                         
         BE    B20                                                              
         BAS   RE,NEXT             (SKIP ADD AFTER HDR)                         
         BAS   RE,NEXT             (SKIP ADD AFTER FLD)                         
         BAS   RE,NEXT             (SKIP ADD COST HDR)                          
*                                                                               
B20      BAS   RE,VALCOST          VALIDATE PERCENTAGE OF NETWORK BUY           
         BAS   RE,NEXT             BUMP TO OFFSET FIELD                         
         CLI   NWKASTAH+5,0        BUMP TWICE? (SKIP ADD OFFSET HDR)            
         BE    *+8                                                              
         BAS   RE,NEXT                                                          
*                                                                               
         BAS   RE,VALOFF           VALIDATE OFFSET                              
         BAS   RE,NEXT             BUMP TO REGION FIELD                         
         CLI   NWKASTAH+5,0        BUMP TWICE? (SKIP ADD REGION HDR)            
         BE    *+8                                                              
         BAS   RE,NEXT                                                          
*                                                                               
         TM    FHATD(R2),FHATPR                                                 
         BNZ   *+10                                                             
         MVC   NDEFRGN,8(R2)       REGION                                       
         XIT1  REGS=(R2)                                                        
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  VALIDATE STATION                                  *          
**********************************************************************          
                                                                                
VALSTA   NTR1                                                                   
*                                                                               
         OC    8(L'NWKSTAT,R2),8(R2)                                            
         BNZ   VS1                                                              
         B     MIS                 ERROR                                        
*                                                                               
VS1      DS    0H                                                               
*                                                                               
         CLI   NUMLINES,MAXL       MAXIMUM # OF RECORDS?                        
         BNL   RECF                YES, ERROR                                   
*                                                                               
VS3      CLC   8(4,R2),=C'ZZZZ'    BYPASS READ                                  
         BE    VS30                                                             
         MVI   USEIONUM,2          USE AIO2                                     
         GOTO1 VALISTA             VALIDATE STATION (SPSFM00 LEVEL)             
         CLC   QMKT,=C'0000'                                                    
         BE    ERRNETW             CAN'T ADD NETWORK AS A STATION               
                                                                                
* --------- DOES STATION EXIST ON NETWORK LEVEL? --------->                     
                                                                                
         B     STEXIST                                                          
                                                                                
*-------- NEED TO MAKE SURE STATION IS NOT A NETWORK -------->                  
                                                                                
VS20     B     STNET                                                            
                                                                                
*-NEED TO MAKE SURE THERE IS A STATION MASTER REC FOR THIS NETWORK->            
                                                                                
VS30     B     MASTER                                                           
                                                                                
* -- MAKE SURE THE STATION TO BE ADDED DOES NOT ALREADY EXIST -->               
* -- OR STATION TO BE DELETED EXISTS --------------------------->               
                                                                                
VS40     B     DUPLIC                                                           
*                                                                               
VSEX     B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
* DOES STATION EXIST ON NETWORK LEVEL?                               *          
*                                                                    *          
* NOTE: SINCE ONLY NETWORK LEVEL REC CHECKED, IT IS POSSIBLE TO ADD  *          
* A STATION TO EST LEVEL REC THAT DOES NOT EXIST AT CLIENT LEVEL     *          
**********************************************************************          
                                                                                
STEXIST  MVC   KEY,MYKEY           ORIGINAL KEY                                 
         OC    KEY+8(2),KEY+8      CLIENT LEVEL?                                
         BZ    STEX                NO NETWORK                                   
         XC    KEY+8(3),KEY+8      CLEAR CLIENT                                 
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ      X'01'                                        
         BAS   RE,GETEL                                                         
         BNE   ST20                                                             
         OC    NWKASTA,SPACES                                                   
         USING NDEFEL01,R3                                                      
*                                                                               
ST10     DS    0H                                                               
         CLC   NDEFSTA,NWKASTA                                                  
         BE    STEX                                                             
         BAS   RE,NEXTEL                                                        
         BE    ST10                                                             
*                                                                               
ST20     B     NOTEX               ERROR                                        
*                                                                               
STEX     MVC   AIO,AIO1                                                         
         B     VS20                                                             
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*     NEED TO MAKE SURE STATION IS NOT NETWORK                       *          
**********************************************************************          
                                                                                
STNET    MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(4),MYKEY        MOVE OD11/AGY                                
         MVC   KEY+4(4),NWKASTA                                                 
         OC    KEY+4(4),SPACES                                                  
         CLC   KEY+4(4),NWKNTWK    COMPARE TO NETWORK IN PROC                   
         BNE   SN20                                                             
*                                                                               
SN10     B     CONF                ERROR                                        
*                                                                               
SN20     GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    SN10                                                             
         B     VS30                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
* MAKE SURE THERE IS A MASTER RECORD FOR THIS NETWORK                *          
**********************************************************************          
                                                                                
MASTER   CLI   ACTNUM,ACTADD                                                    
         BNE   MASTEREX                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVI   KEY+1,C'N'                                                       
         MVC   KEY+2(4),MYKEY+4    CALL LETTERS                                 
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(2),AGENCY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO2                 
         L     R3,AIO2                                                          
         CLC   KEY(9),0(R3)                                                     
         BNE   NOST                ERROR                                        
         USING STARECD,R3          MASTER RECORD DSECT                          
         CLC   SMKT,=C'0000'       MARKET 0000?                                 
         BNE   NOSSTA              NO - ERROR                                   
         DROP  R3                  DROP MASTER RECORD DSECT USING               
*                                                                               
MASTEREX B     VS40                                                             
                                                                                
**********************************************************************          
* IF STATION ALREADY EXISTS THAN DO NOT DISPLAY ERROR MESSAGE        *          
* DELETE THIS STATION AND LATER THIS STATION WOULD BE ADDED AGAIN    *          
* IN A NEW PLACE                                                     *          
**********************************************************************          
                                                                                
DUPLIC   MVI   USEIONUM,0          RESTORE AIO1                                 
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   DUP30            NO STATIONS IN RECORD                           
*                                                                               
DUP10    LA    R2,NWKASTAH                                                      
         CLC   2(4,R3),NWKASTA                                                  
         BE    DUP20                                                            
         BAS   RE,NEXTEL                                                        
         BE    DUP10                                                            
         B     DUP30           STATION NOT IN RECORD                            
*                                                                               
DUP20    BAS   RE,DELELEM       PERFORM MOVE (DELETE AND LATER ADD)             
*                                                                               
DUP30    B     VSEX                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  VALIDATE OFFSET                                   *          
**********************************************************************          
                                                                                
VALOFF   NTR1                                                                   
                                                                                
* ---- VALIDATE HOUR OFFSET FIELD - NOT REQUIRED -------->                      
                                                                                
         LA    R6,ELEM                                                          
         USING NDEFEL01,R6                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BE    VOEX                                                             
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R5)                                          
         CLI   DMCB,0                                                           
         BE    VO10                                                             
         B     INV                                                              
*                                                                               
VO10     L     R5,DMCB+4                                                        
         C     R5,=F'21000'          3.5 HOURS MAX                              
         BH    INV                                                              
         C     R5,=F'-21000'                                                    
         BL    INV                                                              
         CVD   R5,DUB                                                           
         DP    DUB,=P'1500'                                                     
         CP    DUB+5(3),=P'0'      MUST BE 15 MIN MULTIPLES                     
         BNE   INV                                                              
         M     R4,=F'1'                                                         
         D     R4,=F'100'                                                       
         STH   R5,NDEFOSET         HOUR OFFSET (SIGNED) FIELD                   
*                                                                               
VOEX     B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
*                  VALIDATE COST                                     *          
**********************************************************************          
                                                                                
VALCOST  NTR1                                                                   
         GOTO1 ANY                   REQUIRED                                   
*                                                                               
         CLC   8(3,R2),=C'DEL'                                                  
         BE    VCEX                                                             
*                                                                               
         LA    R4,ELEM                                                          
         USING NDEFEL01,R4                                                      
         CLC   8(2,R2),=C'NB'      NOT BOUGHT                                   
         BNE   VC10                                                             
         MVC   NDEFPCT,=F'-1'                                                   
         B     VCEX                                                             
*                                                                               
VC10     DS    0H                                                               
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(3,8(R2)),(R5)                                      
         CLI   DMCB,0                                                           
         BE    *+8                                                              
         B     INV                                                              
*                                                                               
         L     R5,DMCB+4                                                        
         C     R5,=F'0'            CAN'T BE NEGATIVE                            
         BL    INV                                                              
         C     R5,=F'100000'       CAN'T EXCEED 100 PERCENT                     
         BH    INV                                                              
         MVC   NDEFPCT,DMCB+4                                                   
         L     R6,TOTALPCT         SUM UP ALL THE PERCENTAGES                   
         AR    R6,R5                                                            
         ST    R6,TOTALPCT                                                      
*                                                                               
VCEX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*                  DELETE ELEMENT                                    *          
**********************************************************************          
                                                                                
DELELEM  NTR1                                                                   
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+8                                                              
         B     INV                                                              
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ      X'01'                                        
         BAS   RE,GETEL               FIRST ELEMENT                             
         USING NDEFEL01,R3                                                      
         BE    DEL4                                                             
         B     DERROR                                                           
*                                                                               
DEL3     BAS   RE,NEXTEL                                                        
         BNE   DERROR                                                           
*                                                                               
DEL4     CLC   NDEFSTA,NWKASTA                                                  
         BNE   DEL3                                                             
         GOTO1 RECUP,DMCB,(0,AIO),(R3)      ELEMENT FOUND, REMOVE IT            
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*                  DELETE RECORD                                     *          
**********************************************************************          
DELREC   NTR1                                                                   
         LA    R2,NWKASTAH                                                      
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   INV                                                              
*        CLI   NWKCLTH+5,0         CLIENT SPECIFIC?                             
*        BNE   DELREC10                                                         
*        CLI   NWKESTH+5,0         ESTIMATE SPECIFIC?                           
*        BE    INV                                                              
*                                  ENSURE NO LOWER LEVEL RECORDS                
         CLI   MYKEY+10,X'00'                                                   
         BNE   DELREC10            EST IS A LOW AS YOU CAN GO                   
         MVC   SVADDR,DMDSKADD                                                  
         MVC   KEY,MYKEY                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 SEQ                 READ RECORD AFTER ONE WANT TO DEL            
         OC    MYKEY+8(2),MYKEY+8  DELETING CLIENT LEVEL?                       
         BZ    *+14                NO - CHECK FOR CLIENT RECORDS                
         CLC   KEY(10),MYKEY       YES- CHECK FOR ESTIMATE RECORDS              
         B     *+10                IF NWK/CLI SAME MUST BE EST REC              
         CLC   KEY(8),MYKEY        IF NWK SAME MUST BE CLI REC                  
         MVC   AIO,AIO1            RESTORE                                      
         MVC   KEY,MYKEY                                                        
         MVC   DMDSKADD,SVADDR                                                  
         BE    DELERR                                                           
*                                                                               
DELREC10 MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         USING NDEFRECD,R3                                                      
         OI    NDPCNTL,X'80'       MARK RECORD FOR DELETION                     
         OI    KEY+13,X'80'        MARK ACTIVE KEY FOR DELETION                 
         GOTO1 WRITE                                                            
         GOTO1 PUTREC                                                           
         MVI   IOOPT,C'Y'          GENCON DOESN'T NEED TO DO ANYTHING           
         DROP  R3                                                               
DELRECX  B     EXIT                                                             
**********************************************************************          
*                  DELETE PASSIVE POINTERS                           *          
**********************************************************************          
DELPKEY  NTR1                                                                   
         CLI   NWKCLTH+5,0         CLIENT SPECIFIC?                             
         BNE   DELPKEYX            THEN DON'T DELETE PASSIVE                    
         CLI   NWKESTH+5,0         ESTIMATE SPECIFIC?                           
         BNE   DELPKEYX            THEN DON'T DELETE PASSIVE                    
*                                                                               
         MVC   MYKEY,KEY                                                        
         XC    KEY,KEY                                                          
*                                                                               
         LA    R5,MYKEY                                                         
KEYA     USING NDEFRECD,R5                                                      
*                                                                               
         LA    R6,KEY                                                           
KEYB     USING NDEFRECD,R6                                                      
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         USING NDEFEL02,R3                                                      
         MVC   KEYB.NWKPSEQ,NDEFNET          SEQUENCE NUMBER                    
         DROP  R3                                                               
         MVI   KEYB.NWKPTYP,NWKPTYPQ         X'0D'                              
         MVI   KEYB.NWKPSUB,NWKPSUBQ         X'91'                              
         MVC   KEYB.NWKPAGY,KEYA.NDEFKAGY    AGY ID                             
         MVC   KEYB.NWKPNET,KEYA.NDEFKNET    NETWORK                            
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE               DOES PASSIVE KEY EXIST?            
         BNE   DELPKEYX                                                         
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
         MVC   AIO,AIO1                                                         
DELPKEYX MVC   KEY,MYKEY                     RESTORE THE KEY                    
         B     EXIT                                                             
         DROP  KEYA                                                             
         DROP  KEYB                                                             
**********************************************************************          
*                  LIST RECORDS                                      *          
**********************************************************************          
                                                                                
LR       LA    R4,KEY                                                           
         USING NDEFRECD,R4                                                      
*                                                                               
         OI    GENSTAT2,DISTHSPG                                                
*                                                                               
         OC    KEY,KEY         IS IT FIRST TIME THROUGH                         
         BNZ   LR10            NO                                               
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,AGENCY                                                  
*                                                                               
         CLI   LWKNTWKH+5,0    LIST ALL NETWORKS?                               
         BE    LR5                                                              
         MVC   NDEFKNET,SVNTWK   NETWORK                                        
*                                                                               
LR5      CLI   LWKCLTH+5,0    LIST CLIENTS FOR SPECIFIC NETWORK?                
         BE    LR10                                                             
         MVC   NDEFKCLT,SVCLT   CLIENT                                          
*                                                                               
LR10     MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH            GET FIRST RECORD                                 
*                                                                               
LR20     CLC   KEY(4),SAVEKEY                                                   
         BNE   LREND                                                            
         CLC   KEY+2(2),AGENCY  SINCE THIS WAS WRITTEN BY                       
         BNE   LREND               SOMEONE NOT TOO GOOD                         
*                                                                               
LR23     CLI   LWKCLTH+5,0         SHOW ALL CLIENTS                             
         BE    LR25                                                             
         TM    FLAG,CLSTART    START AT FILTER?                                 
         BO    LR24                YES                                          
         CLC   NDEFKCLT,SVCLT                                                   
         BNE   LR30                PURE CLIENT FILTER                           
         B     LR25                                                             
LR24     CLC   NDEFKCLT,SVCLT                                                   
         BL    LR30                                                             
*                                                                               
LR25     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFNELQ     DON'T SHOW IF MKTDEF/CBLDEF RECORD           
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         CLI   NDEFNET-NDEFEL02(R3),NDEFCABQ                                    
         BE    LR30                                                             
*                                                                               
         XC    LISTAR,LISTAR                                                    
*                                                                               
         MVC   LISTNET,NDEFKNET    NETWORK                                      
*                                                                               
         GOTO1 CLUNPK,DMCB,NDEFKCLT,QCLT  UNPACK CLIENT FOR DISPLAY             
         MVC   LISTCLT,QCLT        CLIENT UNPACKED FORMAT                       
*                                                                               
         EDIT  NDEFKEST,(3,LISTEST),ALIGN=LEFT  ESTIMATE NUMBER                 
*                                                                               
         GOTO1 LISTMON                                                          
*                                                                               
LR30     MVC   SAVEKEY,KEY                                                      
         GOTO1 SEQ                                                              
         B     LR20                                                             
*                                                                               
LREND    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  DISPLAY KEY                                       *          
**********************************************************************          
                                                                                
DK       L     R4,AIO                                                           
         USING NDEFRECD,R4        NETWORK DEFINITION RECORD                     
         MVC   NWKNTWK,NDEFKNET    NETWORK                                      
         OI    NWKNTWKH+6,X'80'                                                 
         OC    NDEFKCLT,NDEFKCLT   CLIENT IN PACKED FORMAT                      
         BZ    DK10                                                             
         GOTO1 CLUNPK,DMCB,NDEFKCLT,QCLT                                        
         MVC   NWKCLT(L'QCLT),QCLT        CLIENT UNPACKED FORMAT                
         OI    NWKCLTH+6,X'80'     CLIENT                                       
*                                                                               
DK10     EDIT  NDEFKEST,(3,NWKEST),ALIGN=LEFT ESTIMATE NUMBER                   
         OI    NWKESTH+6,X'80'                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  ADD 02 ELEMENT                                    *          
**********************************************************************          
                                                                                
ADD02    NTR1                                                                   
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'        SEE IF 02 ELEM EXISTS                        
         BAS   RE,GETEL                                                         
         BE    ADD02X                                                           
***      CLI   ACTNUM,ACTADD       CHANGED TO JUST ADD IF NOT THERE!!           
***      BE    *+6                                                              
***      DC    H'0'                NO 02 ELEM FOUND                             
*                                                                               
         LA    R1,STAWRK           GET MAX NETWORKS                             
         XC    STAWRK,STAWRK       CLEAR STAWRK                                 
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'V'                                                     
         MVC   STAPAGY,AGENCY                                                   
         GOTO1 VSTAPACK,(R1)                                                    
         LA    R5,127              MAX NETWORKS                                 
         LA    R6,5                LOWEST NTWK NUMBER                           
         DROP  R1                                                               
*                                                                               
         CLI   NWKCLTH+5,0         CLIENT  LEVEL                                
         BE    ADD02A              NO                                           
         MVC   AIO,AIO2            GET NEXT AVAILABLE NUMBER                    
         XC    KEY,KEY                                                          
         MVC   KEY(8),MYKEY                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                NETWORK LEVEL MUST EXIST                     
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LLC   R6,2(R3)            MUST USE SAME NETWORK NUMBER                 
         B     ADD02C              FOR CLIENT LEVEL RECS                        
*                                                                               
ADD02A   XC    KEY,KEY                                                          
         MVC   KEY(4),MYKEY                                                     
         OI    KEY+1,X'80'         READ PASSIVE PTR 0D91                        
         STC   R6,KEY+4            START AT MIN/05 - SKIP CABLE PSVS/01         
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      CHK AGENCY                                   
         BNE   ADD02C                                                           
         LLC   R6,KEY+4            NEW LOWEST NUMBER                            
         LA    R6,1(R6)            LOOK FOR NEXT NUMBER                         
*                                                                               
ADD02B   GOTO1 SEQ                                                              
         CLC   KEY(4),KEYSAVE      CHK AGENCY                                   
         BNE   ADD02C                                                           
         LLC   R4,KEY+4            NETWORK NUMBER                               
         CR    R6,R4               DOES NUMBER EXIST                            
         BL    ADD02C              NO, SO USE IT NOW                            
         LLC   R6,KEY+4            NEW LOWEST USED NUMBER                       
         LA    R6,1(R6)            LOOK FOR NEXT NUMBER                         
         B     ADD02B                                                           
*                                                                               
ADD02C   DS    0H                                                               
         CR    R6,R5               IS NUMBER PAST THE MAX                       
         BH    NOMORE                                                           
*                                                                               
         LLC   R0,0(R5)            SAVE NUMBER                                  
         XC    ELEM(10),ELEM                                                    
         MVC   ELEM(2),=X'0203'                                                 
         STC   R6,ELEM+2           SET NUMBER IN 02 ELEM                        
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL            POINT R3 TO INSERTION ELEM                   
         BNE   *+6                                                              
         DC    H'0'                SHOULDN'T BE ONE                             
         GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R3)                                     
*                                                                               
ADD02X   B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*  ADD PASSIVE POINTER IF ADDING NEW NETWORK                         *          
**********************************************************************          
*                                                                               
ADDPTR   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(8),MYKEY        GET NETWORK LEVEL DA                         
         GOTO1 HIGH                                                             
         MVC   DSKAD,KEY+14        SAVE DISK ADDRESS                            
*                                                                               
         XC    KEY,KEY                                                          
         L     R3,AIO1                                                          
OLD      USING NDEFRECD,R3                                                      
         LA    R4,KEY                                                           
NEW      USING NDEFRECD,R4                                                      
*                                                                               
         MVI   NEW.NWKPTYP,NWKPTYPQ                                             
         MVI   NEW.NWKPSUB,NWKPSUBQ                                             
         MVC   NEW.NWKPAGY,OLD.NDEFKAGY          AGENCY ID                      
         MVC   NEW.NWKPNET,OLD.NDEFKNET          NETWORK                        
         MVC   KEY+14(L'DSKAD),DSKAD             DISK ADDRESS                   
         DROP  OLD                                                              
*                                                                               
         MVI   ELCODE,X'02'        FIND 02 ELEMENT                              
         BAS   RE,GETEL                                                         
         USING NDEFEL02,R3                                                      
         MVC   NEW.NWKPSEQ,NDEFNET               NETWORK NUMBER                 
         DROP  R3,NEW                                                           
*                                                                               
         OI    DMINBTS,X'88'       READ FOR DELETE AND UPDATE                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     EXIST?                                       
         BNE   ADDPTR10                                                         
         TM    KEY+13,X'80'        MARKED DELETED                               
         BNO   ADDPTRX             NO, THEN ALREADY EXISTS                      
         NI    KEY+13,X'FF'-X'80'                                               
         GOTO1 WRITE               ELSE UNDELETE                                
         B     ADDPTRX                                                          
*                                                                               
ADDPTR10 MVC   KEY,KEYSAVE         RESTORE THE KEY                              
         GOTO1 ADD                 ADD KEY                                      
*                                                                               
ADDPTRX  NI    DMINBTS,X'FF'-X'88'                                              
         B     DR                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*  ROUTINES TO PROTECT/UNPROTECT ALL REGION RELATED FIELDS            *         
***********************************************************************         
RGNPROT  NTR1  ,                                                                
         OI    NWKAREGH+FHATD,FHATPR   FIELDS FOR ADDING NEW                    
         OI    NWKAREGH+FHOID,FHOITR                                            
         OI    NWKREGTH+FHATD,FHATLO                                            
         OI    NWKREGTH+FHOID,FHOITR                                            
*                                                                               
         MVC   NWKHDR+21(3),SPACES     HEADING TEXT                             
         MVC   NWKHDR+47(3),SPACES                                              
         MVC   NWKHDR+73(3),SPACES                                              
         OI    NWKHDRH+FHOID,FHOITR                                             
*                                                                               
         LA    RF,NWKREGLH             FIELDS FOR EACH STATION                  
         LA    R2,NWKREGH                                                       
RGNPR10  OI    FHATD(R2),FHATPR                                                 
         OI    FHOID(R2),FHOITR                                                 
         CR    R2,RF                                                            
         BNL   RGNUNX                                                           
         BAS   RE,NEXT             STATION                                      
         BAS   RE,NEXT             COST                                         
         BAS   RE,NEXT             OFFSET                                       
         BAS   RE,NEXT             NEXT REGION                                  
         B     RGNPR10                                                          
RGNPRX   B     EXIT                                                             
*                                                                               
RGNUNPR  NTR1  ,                                                                
         NI    NWKAREGH+FHATD,255-FHATPR  FIELDS FOR ADDING NEW                 
         OI    NWKAREGH+FHOID,FHOITR                                            
         NI    NWKREGTH+FHATD,255-FHATLO                                        
         OI    NWKREGTH+FHOID,FHOITR                                            
*                                                                               
         MVC   NWKHDR+21(3),=X'D98587'    HEADING TEXT (REG M/C)                
         MVC   NWKHDR+47(3),=X'D98587'                                          
         MVC   NWKHDR+73(3),=X'D98587'                                          
         OI    NWKHDRH+FHOID,FHOITR                                             
*                                                                               
         LA    RF,NWKREGLH                FIELDS FOR EACH STATION               
         LA    R2,NWKREGH                                                       
RGNUN10  NI    FHATD(R2),255-FHATPR                                             
         OI    FHOID(R2),FHOITR                                                 
         CR    R2,RF                                                            
         BNL   RGNUNX                                                           
         BAS   RE,NEXT             STATION                                      
         BAS   RE,NEXT             COST                                         
         BAS   RE,NEXT             OFFSET                                       
         BAS   RE,NEXT             NEXT REGION                                  
         B     RGNUN10                                                          
RGNUNX   B     EXIT                                                             
**********************************************************************          
                                                                                
REQREC   DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+16                                                             
         LA    R2,CONWHENH                                                      
         CLI   CONWHENH+5,0        NO PRINT OPTIONS                             
         BNE   INVPRT                                                           
*                                                                               
         CLC   =C'$DEL',NWKASTA    USER WANTS TO DELETE THE RECORD?             
         BE    EXIT                                                             
*                                                                               
         L     R3,AIO2                                                          
         XC    0(110,R3),0(R3)                                                  
         MVI   10(R3),44                                                        
         MVI   14(R3),106                                                       
         MVI   26(R3),X'40'                                                     
         MVC   27(79,R3),26(R3)                                                 
         MVC   26(2,R3),=C'44'                                                  
         MVC   28(2,R3),AGENCY                                                  
         MVC   30(1,R3),QMED                                                    
         MVC   31(3,R3),=C'ALL'                                                 
         OC    BCLT,BCLT                                                        
         BZ    *+10                                                             
         MVC   31(3,R3),QCLT                                                    
         MVC   40(4,R3),MYKEY+4           NETWORK IN MKT                        
         CLI   MYKEY+10,0                SEE IF ESTIMATE EXCEPTION              
         BE    REQREC5                                                          
         EDIT  (B1,MYKEY+10),(3,49(R3)),0,FILL=0                                
*                                                                               
REQREC5  MVI   87(R3),C'N'                                                      
         MVC   94(7,R3),=C'CONTROL'                                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO2,AIO2                     
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                RESTORE ORIGINAL REC FOR DISPLAY             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         TM    15(R3),X'80'                                                     
         BO    REQRECX                                                          
         CLI   MODE,PRINTREP                                                    
         BNE   ADDPTR              ADD PASSIVE POINTER                          
REQRECX  B     DR                                                               
         EJECT                                                                  
*********************************************************************           
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         DROP  RF                                                               
         GOTO1 ERREX                                                            
SPINFEX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMINF                                                    
         MVI   GTMSYS,2                                                         
         DROP  RF                                                               
         GOTO1 ERREX                                                            
*********************************************************************           
NOMORE   B     MAXN                ERROR                                        
         XMOD1 1                                                                
*********************************************************************           
VCERR    LA    R2,NWKCLTH          INVALID CLIENT                               
         MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*********************************************************************           
DERROR   LA    R2,NWKSTATH                                                      
         MVI   ERROR,NOTFOUND                                                   
         B     ERRX                                                             
*********************************************************************           
RECF     MVC   ERRNUM,=AL2(RECFULL)                                             
         B     SPERREX                                                          
*********************************************************************           
NOTEX    MVC   ERRNUM,=AL2(NOTEXIST)                                            
         B     SPERREX                                                          
*********************************************************************           
CONF     MVC   ERRNUM,=AL2(CONFLICT)                                            
         B     SPERREX                                                          
*********************************************************************           
DELERR   MVC   ERRNUM,=AL2(STDELERR)                                            
         B     SPERREX                                                          
*********************************************************************           
NOST     LA    R2,NWKNTWKH                                                      
         MVC   ERRNUM,=AL2(NOSTAT)                                              
         B     SPERREX                                                          
*********************************************************************           
NOSSTA   LA    R2,NWKNTWKH                                                      
         MVC   ERRNUM,=AL2(1375)                                                
         B     SPERREX                                                          
*********************************************************************           
MAXN     MVC   ERRNUM,=AL2(MAXNET)                                              
         B     SPERREX                                                          
*********************************************************************           
ERRNETW  MVC   ERRNUM,=AL2(NETSTA)  CAN NOT ADD NETWORK AS STATION              
         B     SPERREX                                                          
*********************************************************************           
PCTERR   MVC   ERRNUM,=AL2(238)                                                 
         B     SPERREX                                                          
*********************************************************************           
PCTERR2  MVC   ERRNUM,=AL2(1345)                                                
         B     SPERREX                                                          
*********************************************************************           
REQOV    MVC   ERRNUM,=AL2(REPGEN)                                              
         B     SPINFEX                                                          
*********************************************************************           
INV      MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*********************************************************************           
MIS      MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
*********************************************************************           
NFND     MVI   ERROR,NOTFOUND                                                   
         B     ERRX                                                             
*********************************************************************           
INVPRT   MVI   ERROR,INVPRINT                                                   
         B     ERRX                                                             
*********************************************************************           
RECFULL  EQU   391                                                              
NOTEXIST EQU   392                                                              
CONFLICT EQU   393                                                              
NOSTAT   EQU   394                                                              
MAXNET   EQU   395                                                              
STDELERR EQU   408                                                              
REPGEN   EQU   411                                                              
NETSTA   EQU   421                 CAN NOT ADD NETWORK AS STATION               
*********************************************************************           
         EJECT                                                                  
         GETEL R3,DATADISP,ELCODE                                               
*                                                                               
ERRX     GOTO1 ERREX                                                            
*                                                                               
ELEMLEN  EQU   16                  ELEMENT LENGTH                               
* NDEFELQ  EQU   X'01'               ELEMENTS EQUATED SYMBOLS                   
NDEFNETQ EQU   X'02'                                                            
MAXL     EQU   48             MAX # OF LINES THAT FIT ON THE SCREEN             
STNETERR EQU   140                 NETWORK/STATION CONFLICT                     
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD               SPOOL DSECT                             
         EJECT                                                                  
       ++INCLUDE SPGENNDEF              NETWORK DEFINITION DSECT                
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA               MASTER RECORD DSECT                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FACTRY                                                         
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFH                                                           
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPSFMAFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPSFMD0D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
**********************************************************************          
         ORG   SYSSPARE                                                         
ERRNUM   DS    XL2                                                              
NUMLINES DS    X                                                                
TEMPFLD  DS    CL9                 WORK AREA                                    
MYKEY    DS    CL48                                                             
STAWRK   DS    XL32                                                             
SAVEKEY  DS    XL48                                                             
SVNTWK   DS    CL4                                                              
SVADDR   DS    A                                                                
DSKAD    DS    XL4                                                              
SVCLT    DS    CL2                                                              
TEMPCLT  DS    CL4                                                              
FLAG     DS    X                                                                
CLSTART  EQU   X'80'               START AT CLIENT FILTER                       
TOTALPCT DS    F                                                                
**********************************************************************          
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LISTNET  DS    CL4                                                              
         DS    CL7                                                              
LISTCLT  DS    CL3                                                              
         DS    CL7                                                              
LISTEST  DS    CL3                                                              
         DS    CL2                                                              
**********************************************************************          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPSFM2D   12/05/11'                                      
         END                                                                    
