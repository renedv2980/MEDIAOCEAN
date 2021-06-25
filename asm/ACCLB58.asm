*          DATA SET ACCLB58    AT LEVEL 033 AS OF 08/17/00                      
*PHASE T62158A                                                                  
CLB58    TITLE '- PC COMMS - BILL LIST'                                         
CLB58    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB58**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,BORELO                                                        
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
*                                                                               
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         LA    RF,MAPTAB           SET A(MAP TABLE)                             
         ST    RF,AMAPTAB                                                       
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                                                               
         XC    SCR99CNT,SCR99CNT                                                
         MVI   REVALL,0                                                         
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ELEMENT DATA RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
                                                                                
RCVBILNO DS    0H                  FOR REVERSAL LIST                            
         MVC   BILLNUM,DATA                                                     
         B     EXITY                                                            
                                                                                
RCVJOB   DS    0H                  FOR PREV BILL OR REVERSAL                    
         MVC   THISJOB,DATA                                                     
         B     EXITY                                                            
                                                                                
RCVRVACT DS    0H                  PENDING REVERSAL ACTION (1=CLEAR)            
         MVC   RACTION,DATA                                                     
         B     EXITY                                                            
                                                                                
RCV99DA  DS    0H                                                               
         MVC   SCR99DA,DATA        D/A OF 99 POSTING WHEN PENDING REV           
         B     EXITY                                                            
RCVALLRV DS    0H                                                               
         MVC   REVALL,DATA         ALL ITEMS MARKED FOR REVERSAL                
         B     EXITY                                                            
                                                                                
***********************************************************************         
* RECEIVE DA OF TRANSACTION TO MARK/CLEAR AS PENDING REVERSAL         *         
***********************************************************************         
                                                                                
RCVDA    DS    0H                                                               
         ICM   RF,15,SCR99CNT      INCREMENT SCREEN COUNT OF REVERSALS          
         LA    RF,1(RF)                                                         
         STCM  RF,15,SCR99CNT                                                   
                                                                                
         MVC   TRNDA,DATA                                                       
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING PRORATAD,LSPRATA                                                 
                                                                                
         MVC   IOKEY,BCSPACES      GET JOB REC INTO IO3                         
         MVC   IOKEY(1),CUABIN                                                  
         MVC   IOKEY+1(2),BCCPYEL+(CPYPROD-CPYELD)                              
         MVC   IOKEY+3(L'THISJOB),THISJOB                                       
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         GOTO1 ASETUP,BODMCB,IOKEY+3,0,0                                        
                                                                                
         MVC   IODAOVER,TRNDA      GET TRANSACTION INTO IO1                     
         GOTO1 AIO,IOACCMST+IOGETRUP+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XR    R0,R0               CALL PRORATA                                 
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO1,AGOPBLK,ACOM,(R0),LSPRATA,0                 
                                                                                
         LA    R0,LSPRATAS         COPY LSPRATA TO LSPRATAS                     
         L     R1,=A(PR$LNQ)                                                    
         LA    RE,LSPRATA                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   RACTION,1           TEST CLEARING PENDING REVERSAL               
         BE    RCVRCL00                                                         
                                                                                
         GOTO1 AREVTRN,BOPARM,(C'R',AIO1),LSPRATA,BILLNUM                       
                                                                                
RCVREV06 GOTO1 AIO,IOACCMST+IOPUTREC+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1             UPDATE DIRECTORY STATUS                      
R        USING TRNRECD,R2                                                       
K        USING TRNRECD,IOKEY                                                    
         MVC   K.TRNKEY,R.TRNKEY                                                
         GOTO1 AIO,IOREAD+IOLOCK+IOACCDIR                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   K.TRNKSTA,R.TRNRSTA                                              
         GOTO1 AIO,IOPUT+IOACCDIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  K,R                                                              
                                                                                
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO1),AIO3,LSPRATAS,LSPRATA                 
                                                                                
         GOTO1 AIO,IO3+IOACCMST+IOPUT   WRITE BACK JOB RECORD                   
         BE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* CLEAR PENDING REVERSAL                                              *         
***********************************************************************         
RCVRCL00 GOTO1 AREVTRN,BOPARM,(C'C',AIO1),LSPRATA,BILLNUM                       
         B     RCVREV06            WRITE BACK TRANS,JOB ETC                     
         EJECT                                                                  
***********************************************************************         
* DELETE DRAFT BILL, DATA=DA                                          *         
***********************************************************************         
         SPACE 1                                                                
RCVDEL   DS    0H                                                               
         MVC   BILLDA,DATA                                                      
         USING BEDRECD,R2                                                       
         MVC   IODAOVER,BILLDA     GET FILE RECORD                              
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         L     R2,AIO1                                                          
         MVC   IOKEY,BEDKEY                                                     
         LA    R2,IOKEY            DELETE DIRECTORY RECORD                      
         GOTO1 AIO,IOACCDIR+IORDUP                                              
         OI    BEDKSTAT,BEDSDELT                                                
         GOTO1 AIO,IOACCDIR+IOWRITE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1             DELETE FILE RECORD                           
         OI    BEDRSTAT,BEDSDELT                                                
         GOTO1 AIO,IOWRITE+IOACCMST+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         XC    BEDPAS,BEDPAS       DELETE PASSIVE                               
         MVI   BEDPTYP,BEDPTYPQ                                                 
         MVC   BEDPCPY,CUABIN                                                   
         MVI   BEDPSUB,BEDPSUBQ                                                 
         MVC   BEDPBLNO,BILLNUM                                                 
         MVC   BEDPIND,BEDPIDFT                                                 
         GOTO1 AIO,IOACCDIR+IOHIUPD                                             
         CLC   BEDPAS(BEDPIND-BEDPAS),IOKEYSAV                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    BEDKSTA,BEDSDELT                                                 
         GOTO1 AIO,IOACCDIR+IOWRITE                                             
         BE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LAST FOR MAP HEADER RECEIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
         OC    SCR99CNT,SCR99CNT   ANY CHANGE TO COUNT THIS SCREEN?             
         BZ    RCVLSTX                                                          
         MVC   IODAOVER,SCR99DA    GET BILL POSTING                             
         GOTO1 AIO,IOGET+IOLOCK+IOACCMST+IO3                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         SR    R0,R0                                                            
         USING RATELD,R2                                                        
SCRL02   IC    R0,RATLN                                                         
         AR    R2,R0                                                            
         CLI   RATEL,0                                                          
         BE    SCRL06              NO RATE ELEMENT, ADD NEW ONE                 
         CLI   RATEL,RATETAXQ                                                   
         BNE   SCRL02                                                           
         NI    RATRATE,FF-X'80'                                                 
         CLI   RACTION,1           TEST CLEARING PENDING REVERSAL               
         BE    SCRL04                                                           
         ICM   RE,15,SCR99CNT      COUNT OF REVERSALS                           
         AH    RE,RATRATE          ADD TO EXISTING COUNT                        
         STCM  RE,3,RATRATE                                                     
         CLI   REVALL,0                                                         
         BE    *+8                                                              
         OI    RATRATE,X'80'       SET ALL ITEMS MARKED AS REVERSAL             
         B     SCRL08                                                           
SCRL04   SR    RE,RE                                                            
         LH    RE,RATRATE          EXISTING REVERSALS                           
         ICM   RF,15,SCR99CNT      CLEARED REVERSALS THIS TIME                  
         SR    RE,RF               SUBTRACT FROM EXISTING COUNT                 
         STCM  RE,3,RATRATE                                                     
         B     SCRL08                                                           
                                                                                
SCRL06   LA    R2,BOELEM                                                        
         MVI   RATEL,TRNELQ+1      ENSURE NEW ELEMENT GOES AFTER TRNEL          
         MVI   RATLN,RATLNQ                                                     
         MVC   RATRATE,SCR99CNT+2  THIS SCREEN COUNT OF REVERSALS               
         GOTO1 VHELLO,BODMCB,(C'P',=C'ACCMST '),AIO3,BOELEM,0,0                 
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,16(R1)           PICK-UP A(ELEMENT) IN RECORD                 
         MVI   RATEL,RATETAXQ      AND SET CORRECT ELEMENT CODE                 
         DROP  R2                                                               
SCRL08   SR    RE,RE                                                            
*        ICM   RE,3,CSHIRECN       GET HIGH TSAR RECORD NUMBER                  
*        SR    R0,R0                                                            
*        ICM   R0,3,CSPSRECN       HIGH TSAR PREVIOUS SESSION                   
*        SR    RE,R0                                                            
*        CLM   RE,3,RATRATE        TEST EVERYTHING MARKED FOR REVERSAL          
*        BNE   *+8                                                              
*        OI    RATRATE,X'80'                                                    
         MVC   IODAOVER,SCR99DA    WRITE BACK UPDATED BILL POSTING              
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RCVLSTX  B     EXITY                                                            
         DROP  R5                                                               
         SPACE 1                                                                
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         ICM   RF,15,OSND                                                       
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND BILL LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
SNDBL    DS    0H                                                               
         OC    BILLDA,BILLDA                                                    
         BNZ   EXITY                                                            
         MVI   FININDS,0                                                        
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
                                                                                
         LA    R2,IOKEY            INITIALIZE THE KEY                           
         USING BEDRECD,R2                                                       
         XC    BEDPAS,BEDPAS                                                    
         MVI   BEDPTYP,BEDPTYPQ                                                 
         MVC   BEDPCPY,CUABIN                                                   
         MVI   BEDPSUB,BEDPSUBQ                                                 
         MVI   READINDS,0                                                       
         OI    READINDS,READPC                                                  
         B     GETFPAS                                                          
                                                                                
SNDBL2   EQU   *                                                                
         TM    FININDS,FINISH                                                   
         BO    EXITY                                                            
         OI    FININDS,FINISH                                                   
         NI    READINDS,X'FF'-READPC                                            
         OI    READINDS,READMN                                                  
                                                                                
         XC    BEDPAS,BEDPAS                                                    
         MVI   BEDPTYP,PBRPTYPQ                                                 
         MVI   BEDPSUB,PBRPPASQ                                                 
         MVC   BEDPCPY,CUABIN                                                   
         B     GETFPAS                                                          
*                                                                               
GETNPAS  IC    RE,BEDPAS+L'BEDPAS-1 * READ THROUGH PASSIVE RECORDS *            
         LA    RE,1(RE)                                                         
         STC   RE,BEDPAS+L'BEDPAS-1                                             
                                                                                
GETFPAS  GOTO1 AIO,IOHI+IOACCDIR                                                
         CLC   BEDPAS(BEDPBLNO-BEDPAS),IOKEYSAV                                 
         BNE   SNDBL2                                                           
         MVC   BILLDA,BEDKDA                                                    
*                                                                               
         MVC   TLBPAS,IOKEY                                                     
         MVC   BOWORK1(L'IOKEY),IOKEY                                           
         MVC   IODAOVER,BEDKDA                                                  
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         MVI   ALLSTAT,C' '                                                     
         MVI   REPLICA,0                                                        
         BAS   RE,SETCUR                                                        
                                                                                
P        USING BEDRECD,TLBPAS                                                   
         GOTO1 ASNDHDR,BODMCB,MH#BIL                                            
         GOTO1 ASNDDATA,BODMCB,1,BILLDA                                         
         GOTO1 (RF),(R1),2,P.BEDPJOB                                            
         GOTO1 (RF),(R1),3,P.BEDPBLNO                                           
         GOTO1 (RF),(R1),4,P.BEDPFORM                                           
         GOTO1 (RF),(R1),5,SENDNET                                              
         GOTO1 (RF),(R1),6,SENDCOM                                              
         GOTO1 (RF),(R1),15,ADVNETAM                                            
         GOTO1 (RF),(R1),16,ADVCOMAM                                            
         GOTO1 (RF),(R1),7,P.BEDPCRED            DATE CREATED                   
         GOTO1 (RF),(R1),8,EXPDATE               DATE EXPIRES                   
         CLI   TMPNMLN,0                                                        
         BE    GET08                                                            
         GOTO1 (RF),(R1),21,(TMPNMLN,TMPNAME)                                   
                                                                                
GET08    OC    P.BEDPBILD,P.BEDPBILD                                            
         BZ    GET10                                                            
         GOTO1 (RF),(R1),9,P.BEDPBILD            DATE BILLED                    
GET10    GOTO1 (RF),(R1),10,TLBCUR                                              
         GOTO1 (RF),(R1),13,TLBCUR+(CURTDECP-CURTABD)                           
                                                                                
         OC    P.BEDPBILD,P.BEDPBILD   IS IT A LIVE BILL?                       
         BNZ   GET40                      -DONT NEED TO CHECK ALLOC             
         GOTO1 ASETUP,BODMCB,(X'10',P.BEDPJOB),0,0  SETUP CURRENCY              
         MVI   ALLSTAT,C'?'                                                     
         CLC   TLBCUR(3),CSBILCUR     TEST BILL CURRENCY = ALLOCATION           
         BNE   GET40                                                            
         CLC   TLBCUR(3),CSCPYCUR                                               
         BE    GET12                                                            
         CLC   TLBCUR(3),BCCPYSEC                                               
         BE    GET12                                                            
*?       CLC   BILLRRAT,CSEXCRAT   TEST BILL EXC. RATE = ALLOCATION             
         BNE   GET40                                                            
GET12    MVI   ALLSTAT,C' '                                                     
K        USING TRNRECD,IOKEY                                                    
         MVC   K.TRNKEY,BCSPACES                                                
         MVC   K.TRNKCPY,P.BEDPCPY                                              
         MVC   K.TRNKUNT(L'BCCPYPRD),BCCPYPRD                                   
         MVC   K.TRNKACT,P.BEDPJOB                                              
         GOTO1 AIO,IO2+IOACCDIR+IOREAD                                          
         BE    *+6                                                              
         DC    H'0'                JOB NOT ON FILE                              
         MVC   IODAOVER,TRNDA                                                   
         GOTO1 AIO,IO2+IOACCMST+IOGET                                           
         L     R3,AIO2                                                          
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING SCIELD,R3                                                        
         XR    RE,RE                                                            
GET20    CLI   SCIEL,0                                                          
         BE    GET40               NO ALLOCATION SO MUST BE ADVANCE             
         CLI   SCIEL,SCIELQ                                                     
         BNE   GET22                                                            
         CLI   SCITYPE,SCITCBAP                                                 
         BE    GET30                                                            
GET22    IC    RE,SCILN                                                         
         AR    R3,RE                                                            
         B     GET20                                                            
                                                                                
GET30    SP    SENDNET,ADVNETAM    SUBTRACT ADVANCES FROM TOTAL                 
         SP    SENDCOM,ADVCOMAM                                                 
         CP    SENDNET,SCIAMNT     TEST ALLOCATION SAME AS JOB RECORD           
         BNE   *+14                                                             
         CP    SENDCOM,SCIAMNT+L'SCIAMNT                                        
         BE    GET40                                                            
         ZAP   BODUB1,SENDNET                                                   
         AP    BODUB1,SENDCOM                                                   
         ZAP   BODUB2,SCIAMNT                                                   
         AP    BODUB2,SCIAMNT+L'SCIAMNT                                         
         MVI   ALLSTAT,C'*'                                                     
         CP    BODUB1,BODUB2                                                    
         BE    GET40                                                            
         MVI   ALLSTAT,C'-'                                                     
         BH    *+8                                                              
         MVI   ALLSTAT,C'+'                                                     
         DROP  R3                                                               
                                                                                
GET40    GOTO1 ASNDDATA,BODMCB,14,ALLSTAT                                       
         GOTO1 (RF),(R1),20,REPLICA                                             
         TM    READINDS,READPC                                                  
         BO    GET42                                                            
         GOTO1 ASNDDATA,BODMCB,17,READINDS                                      
         B     GET45                                                            
GET42    GOTO1 (RF),(R1),18,PERSON                                              
         GOTO1 (RF),(R1),19,DATE                                                
GET45    MVC   IOKEY,TLBPKEY                                                    
         B     GETNPAS                                                          
         DROP  P,K,R2                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET CURRENCY ENTRY ON TSAR RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
SETCUR   NTR1  ,                                                                
*                                                                               
         ZAP   SENDNET,BCPZERO                                                  
         ZAP   SENDCOM,BCPZERO                                                  
         ZAP   ADVNETAM,BCPZERO                                                 
         ZAP   ADVCOMAM,BCPZERO                                                 
         MVI   TMPNMLN,0           TEMPLATE NAME LENGTH                         
                                                                                
         L     R2,AIO1                                                          
         USING PBRRECD,R2                                                       
                                                                                
         LA    R3,PBRRFST                                                       
         USING RACELD,R3                                                        
         XR    RF,RF                                                            
SETCUR5  CLI   RACEL,X'00'                                                      
         BE    SETCUR25                                                         
         CLI   RACEL,NAMELQ                                                     
         BE    SETCUR22                                                         
         CLI   RACEL,RACELQ                                                     
         BNE   SETCUR10                                                         
         CLI   RACTYPE,RACTCHA                                                  
         BE    SETCUR20                                                         
SETCUR10 IC    RF,RACLN                                                         
         AR    R3,RF                                                            
         B     SETCUR5                                                          
                                                                                
SETCUR20 GOTO1 AGETPID,RACPERS                                                  
         MVC   PERSON,BCWORK                                                    
         MVC   DATE,RACDATE                                                     
         B     SETCUR25                                                         
         DROP  R3                                                               
                                                                                
         USING NAMELD,R3                                                        
SETCUR22 MVC   TMPNAME,NAMEREC                                                  
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q                                                       
         STC   RE,TMPNMLN                                                       
         B     SETCUR10                                                         
         DROP  R3                                                               
                                                                                
SETCUR25 TM    READINDS,READPC                                                  
         BO    SETCURPC                                                         
                                                                                
         LA    R4,PBRRFST                                                       
         USING BLHELD,R4                                                        
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R4,RF,*-12                                                       
*                                                                               
                                                                                
         MVC   EXPDATE,BLHEXPD                                                  
         OC    BLHCUR,BLHCUR       TEST USING COMPANY CURRENCY                  
         BZ    *+14                                                             
         CLC   BLHCUR,CSCPYCUR                                                  
         BNE   SCURMF02                                                         
         MVC   TLBCUR,CSCURCPY                                                  
         B     SCURMF04                                                         
*                                                                               
SCURMF02 CLC   BLHCUR,CSBILCUR     TEST USING BILLING CURRENCY                  
         BNE   *+14                                                             
         MVC   TLBCUR,CSCURBIL                                                  
         B     SCURMF04                                                         
         GOTO1 VBLDCUR,BOPARM,BLHCUR,TLBCUR,ACOM                                
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
                                                                                
SCURMF04 LA    R1,PBRRFST                                                       
         USING NDXELD,R1           R1=A(INDEX ELEMENT)                          
         XR    RF,RF                                                            
         CLI   NDXEL,NDXELQ                                                     
         BE    *+12                                                             
         IC    RF,NDXLN                                                         
         BXH   R1,RF,*-12                                                       
         XR    R0,R0                                                            
         ICM   R0,1,NDXACTV        R0=NO. OF ACTIVE ENTRIES                     
         BZ    SETCURX                                                          
         LA    R6,NDXINDX          R6=A(LIST OF ACTIVE ENTRIES)                 
         DROP  R1                                                               
                                                                                
SCURMF06 MVC   IOKEY,PBRKEY                                                     
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY+(PBRKPARA-PBRRECD)(L'PBRKPARA),0(R6)                       
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AIO2                                                          
         LA    R1,PBRRFST-PBRRECD(R1)                                           
         USING PGHELD,R1                                                        
         XR    RF,RF                                                            
         CLI   PGHEL,PGHELQ                                                     
         BE    *+12                                                             
         IC    RF,PGHLN                                                         
         BXH   R1,RF,*-12                                                       
         AP    SENDNET,PGHNET                                                   
         AP    SENDCOM,PGHCOM                                                   
         DROP  R1                                                               
         LA    R6,1(R6)                                                         
         BCT   R0,SCURMF06                                                      
         B     SETCURX                                                          
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
SETCURPC L     R4,AIO1             EXTRACT CURRENCY CODE                        
         LA    R4,BEDRFST-BEDRECD(R4)                                           
         USING SCIELD,R4                                                        
SCURPC02 CLI   SCIEL,0                                                          
         BE    SETCURX                                                          
         CLI   SCIEL,BLHELQ                                                     
         BE    SCURPC08                                                         
         CLI   SCIEL,SCIELQ                                                     
         BNE   SCURPC06                                                         
         CLI   SCITYPE,SCITCBAP                                                 
         BNE   SCURPC04                                                         
         ZAP   SENDNET,SCIAMNT                                                  
         ZAP   SENDCOM,SCIAMNT+L'SCIAMNT                                        
SCURPC04 CLI   SCITYPE,SCITTADV                                                 
         BNE   SCURPC06                                                         
         ZAP   ADVNETAM,SCIAMNT                                                 
         ZAP   ADVCOMAM,SCIAMNT+L'SCIAMNT                                       
SCURPC06 XR    RF,RF                                                            
         IC    RF,SCILN                                                         
         AR    R4,RF                                                            
         B     SCURPC02                                                         
*                                                                               
         USING BLHELD,R4                                                        
SCURPC08 TM    BLHINDS1,BLHIREP                                                 
         BNO   *+8                                                              
         MVI   REPLICA,1                                                        
                                                                                
         MVC   EXPDATE,BLHEXPD                                                  
         OC    BLHCUR,BLHCUR       TEST USING COMPANY CURRENCY                  
         BZ    *+14                                                             
         CLC   BLHCUR,CSCPYCUR                                                  
         BNE   SCURPC10                                                         
         MVC   TLBCUR,CSCURCPY                                                  
         B     SCURPC06                                                         
*                                                                               
SCURPC10 CLC   BLHCUR,CSBILCUR     TEST USING BILLING CURRENCY                  
         BNE   *+14                                                             
         MVC   TLBCUR,CSCURBIL                                                  
         B     SCURPC06                                                         
         GOTO1 VBLDCUR,BOPARM,BLHCUR,TLBCUR,ACOM                                
         CLI   0(R1),0                                                          
         BE    SCURPC06                                                         
         DC    H'0'                                                             
*                                                                               
SETCURX  B     EXIT                                                             
         DROP  R4,R5                                                            
                                                                                
***********************************************************************         
* SEND PREV BILLS / TRANSACTIONS FOR BILL REVERSE                     *         
***********************************************************************         
         SPACE 1                                                                
SNDBLREV DS    0H                                                               
         OC    TRNDA,TRNDA         IF DA THEN MUSTVE BEEN MARKING ITEM          
         BNZ   EXITY                                                            
         L     R3,ALSVALS                                                       
         USING LSVALSD,R3                                                       
         USING PRORATAD,LSPRATA                                                 
         GOTO1 ASETUP,BODMCB,THISJOB,0,0                                        
                                                                                
K        USING TRNRECD,IOKEY                                                    
         MVC   K.TRNKEY,BCSPACES                                                
         MVC   K.TRNKCPY,CUABIN                                                 
         MVC   K.TRNKUNT(L'BCCPYPRD),BCCPYPRD                                   
         MVC   K.TRNKACT,THISJOB                                                
         OC    BILLNUM,BILLNUM     IF NO BILL# THEN MUST BE PREV BILL           
         BZ    SNDPREVB                                                         
                                                                                
***********************************************************************         
* TRANSACTIONS FOR BILL REVERSE                                       *         
***********************************************************************         
                                                                                
         GOTO1 AIO,IOACCDIR+IOHI+IO1  GET JOB RECORD                            
         BNE   EXITN                                                            
         CLC   K.TRNKCULA,IOKEYSAV+TRNKCULA-TRNKEY                              
         BNE   EXITY                                                            
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO1             A(JOB RECORD)                                
         SR    R0,R0                                                            
         XC    RJOBCURR,RJOBCURR                                                
         LA    RE,ACTRFST-ACTRECD(RE)                                           
SNDBLR10 CLI   0(RE),0             TEST E-O-R                                   
         BE    GETNEXT                                                          
         CLI   0(RE),AFCELQ        AFCCURR=CURR OF PENDING ACTIVITY             
         BE    SNDBLR20                                                         
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     SNDBLR10                                                         
SNDBLR20 CLC   AFCCURR-AFCELD(,RE),BCSPACES                                     
         BNH   GETNEXT                                                          
         MVC   RJOBCURR,AFCCURR-AFCELD(RE)                                      
         DROP  K                                                                
*                                                                               
GETNEXT  GOTO1 AIO,IOACCDIR+IOSQ+IO1  * GET NEXT RECORD FOR LIST *              
         BNE   EXITN                                                            
         LA    R2,IOKEY                                                         
         USING TRNRECD,R2                                                       
                                                                                
         CLC   TRNKCULA,IOKEYSAV+TRNKCULA-TRNKEY                                
         BNE   EXITY               FINISHED WITH THIS JOB                       
         CLC   TRNKDATE,BCSPACES   NOT TRANSACTION RECORD                       
         BE    GETNEXT                                                          
         OI    LSSTAT1,LSSORDER    INCLUDE ORDERS                               
         GOTO1 ASETTRN,BOPARM,(C'D',TRNRECD)                                    
         BNE   GETNEXT             FILTER DIRECTORY RECORD                      
*                                                                               
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
*                                                                               
E        USING TRNELD,TRNRFST                                                   
GETN110  LA    RE,E.TRNELD                                                      
         DROP  E                                                                
         SR    R0,R0                                                            
         USING PTAELD,RE                                                        
GETN120  CLI   PTAEL,0             TEST E-O-R                                   
         BE    GETNEXT                                                          
         CLI   PTAEL,PTAELQ        FIND ACTIVITY ELEMENT                        
         BNE   GETN140                                                          
         CLI   PTATYPE,PTATRAL     CHECK ALLOCATED TO BILL                      
         BNE   GETN140                                                          
         TM    PTASTAT1,PTASPEND   TEST IF STILL PENDING UPDATE                 
         BO    GETN140                                                          
         CLC   PTARBLNO,BILLNUM    CHECK MATCHES BILL BEING REVERSED            
         BNE   GETN140                                                          
*&&UK*&& TM    PTASTAT2,PTASOXFR                                                
*&&UK*&& BO    GETN140                                                          
         TM    PTASTAT1,PTASREVU   TEST REVERSAL OF BILL IS UPDATED             
         BO    GETNEXT                                                          
         OC    RJOBCURR,RJOBCURR   TEST ANY ACTIVITY PENDING ON JOB             
         BZ    *+14                NO                                           
         CLC   PTACUR,RJOBCURR     TEST MATCH WITH PENDING ACTIVITY             
         BNE   GETNEXT                                                          
*        CLC   PTACUR,BCCPYSEC     TEST ALLOCATION IN SECOND CURRENCY           
*        BNE   GETN150                                                          
*        CLC   CSBILCUR,CSCPYCUR   TEST BILLING CURR AGENCY PRIMARY             
*        BNE   GETN150                                                          
         MVC   CSBILCUR,PTACUR     THEN ASSUME THIS IS THE RIGHT ONE            
         B     GETN150                                                          
GETN140  IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     GETN120                                                          
         DROP  RE                                                               
*                                  GENERAL FILTERS                              
GETN150  GOTO1 ASETTRN,BOPARM,(C'M',TRNRECD)                                    
         BNE   GETNEXT                                                          
         BAS   RE,GETPTREV                                                      
         CLC   TRNORDER,TRNKWORK                                                
         BNE   GETN200                                                          
         CLI   0(R4),0             TEST MASSAGED PTAEL FOUND                    
         BE    GETNEXT             NO - MUST BE FULLY MATCHED ORDER             
                                                                                
GETN200  GOTO1 CHGPRO,BOPARM,(R3),(R4),LSPRATA                                  
         USING PTAELD,R3                                                        
COP      USING PTAELD,R4                                                        
                                                                                
         GOTO1 ASNDHDR,BODMCB,MH#BLR    SEND TRANSACTION INFO                   
         GOTO1 ASNDDATA,BODMCB,1,IOKEY+(TRNKDA-TRNRECD)                         
         GOTO1 (RF),(R1),2,TRNKWORK                                             
         GOTO1 (RF),(R1),3,(L'TRNKULC,TRNKULC)                                  
         GOTO1 (RF),(R1),4,TRNKDATE                                             
         GOTO1 (RF),(R1),5,TRNKREF                                              
                                                                                
         ZAP   BODUB1,COP.PTANET                                                
         ZAP   BODUB2,COP.PTARCOM                                               
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    SNDBLR30                                                         
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    SNDBLR30                                                         
         ZAP   BODUB1,COP.PTANETF                                               
         ZAP   BODUB2,COP.PTARFCOM                                              
SNDBLR30 MP    BODUB1,=P'-1'                                                    
         MP    BODUB2,=P'-1'                                                    
         TM    PTASTAT1,PTASREVD   TEST ALREADY MARKED FOR REVERSAL             
         BO    SNDBLR32                                                         
         GOTO1 ASNDDATA,BODMCB,10,BODUB1                                        
         GOTO1 ASNDDATA,BODMCB,13,BODUB2                                        
         B     SNDBLR34                                                         
                                                                                
SNDBLR32 GOTO1 ASNDDATA,BODMCB,12,BODUB1                                        
         GOTO1 ASNDDATA,BODMCB,14,BODUB2                                        
                                                                                
SNDBLR34 GOTO1 AIO,IOACCDIR+IOREAD+IO1      RESTORE READ SEQUENCE               
         BE    GETNEXT                                                          
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* PREV BILL                                                           *         
***********************************************************************         
                                                                                
SNDPREVB DS    0H                                                               
K        USING TRNRECD,IOKEY                                                    
         MVC   K.TRNKWORK,WC99                                                  
         LA    R1,IOHIGH+IOACCDIR                                               
         B     *+8                                                              
SNDPBNXT LA    R1,IOSEQ+IOACCDIR                                                
         GOTO1 AIO                                                              
         BNE   EXITN                                                            
         CLC   K.TRNKEY(TRNKCULC-TRNKEY),IOKEYSAV                               
         BNE   EXITY               DIFF JOB                                     
         OC    K.TRNKDATE,K.TRNKDATE   ENSURE HAVE TRANSACTION RECORD           
         BZ    SNDPBNXT                                                         
         CLC   K.TRNKDATE,BCSPACES                                              
         BE    SNDPBNXT                                                         
         MVI   READINDS,0          READINDS USED AS REVERSAL COUNT HERE         
                                                                                
         MVC   IODAOVER,K.TRNKDA                                                
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         L     R2,AIO1                                                          
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         USING TRNELD,R2                                                        
         XR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SH    RF,=Y(TRNLN1Q+1)                                                 
         BM    SNDPBNXT                                                         
         EX    RF,*+8                                                           
         BE    SNDPB01             BILL DID NOT ORIGINATE FROM CBIL             
         CLC   TRNNARR(0),=C'Client billing'                                    
*        EX    RF,*+8                                                           
*        BE    SNDPB01                                                          
*        CLC   TRNNARR(0),CLIBG2                                                
         B     SNDPBNXT                                                         
         DROP  K                                                                
                                                                                
SNDPB01  DS    0H                                                               
         ZAP   BODUB1,TRNAMNT      BODUB1=AGY NET                               
         ZAP   BODUB2,BCPZERO      BODUB2=AGY COM                               
         ZAP   BODUB3,BCPZERO      BODUB3=BILL NET                              
         XC    BODUB4,BODUB4       BODUB4=BILL COMM IF FOUND ELSE X'00'         
         MVC   BILLCURR,CSCPYCUR   DEFAULT TO COMPANY CURRENCY                  
         USING AFCELD,R2                                                        
         XR    RF,RF                                                            
SNDPB02  CLI   AFCEL,0                                                          
         BE    SNDPB10                                                          
         CLI   AFCEL,AFCELQ                                                     
         BE    SNDPB06                                                          
         CLI   AFCEL,RATETAXQ                                                   
         BE    SNDPB08                                                          
         CLI   AFCEL,SCIELQ                                                     
         BE    SNDPB09                                                          
SNDPB04  IC    RF,AFCLN                                                         
         BXH   R2,RF,SNDPB02                                                    
SNDPB06  MVC   BILLCURR,AFCCURR                                                 
         ZAP   BODUB3,AFCAMNT      BODUB3=BILL NET AMOUNT                       
         LA    RE,BCWORK           FILL IN EUREKA BLOCK FOR LATER               
         USING EURKBLKD,RE                                                      
         XC    0(EURKBLKL,RE),0(RE)       CLEAR BLOCK                           
         MVC   EURKCUFR,CSCPYCUR          FROM-CURRENCY                         
         MVC   EURKCUTO,AFCCURR           TO-CURRENCY                           
         MVC   EURKRULE,AFCX              EXCHANGE RATE RULE                    
         DROP  RE                                                               
         B     SNDPB04                                                          
                                                                                
         USING RATELD,R2                                                        
SNDPB08  CLC   RATRATE,=X'8000'    THIS MEANS FULLY REVERSED & UPDATED          
         BE    SNDPBNXT                                                         
         OC    RATRATE,RATRATE     NON ZERO MEANS                               
         BZ    *+10                                                             
         MVC   READINDS,RATRATE+1  ITEMS ALLOCATED FOR REVERSAL (<255!)         
         B     SNDPB04                                                          
         USING SCIELD,R2                                                        
SNDPB09  CLI   SCITYPE,SCITCOMM                                                 
         BNE   SNDPB04                                                          
         ZAP   BODUB2,SCIAMNT      BODUB2=AGY COM                               
         CLI   SCILN,SCILN1Q                                                    
         BE    SNDPB04                                                          
         ZAP   BODUB4,SCICURA      BODUB4=BILL COM IF THERE                     
         B     SNDPB04                                                          
                                                                                
SNDPB10  DS    0H                                                               
*&&US*&& MVC   BODUB3(L'BODUB1+L'BODUB2),BODUB1                                 
*&&UK                                                                           
         CLC   BILLCURR,CSCPYCUR    TEST AGY CURR BILL                          
         BNE   *+14                                                             
         MVC   BODUB3(L'BODUB1+L'BODUB2),BODUB1                                 
         B     SNDPB20                                                          
                                                                                
         OC    BODUB4,BODUB4       DID WE GET BILL COMM?                        
         BNZ   SNDPB20                                                          
         GOTO1 VEUREKA,BODMCB,('APPLYQ',BCWORK),BODUB2,BODUB4                   
*&&                                                                             
                                                                                
SNDPB20  GOTO1 ASNDHDR,BODMCB,MH#BIL    SEND BILL (PREV BILL)                   
K        USING TRNRECD,IOKEY                                                    
         GOTO1 ASNDDATA,BODMCB,1,K.TRNKDA DA                                    
         GOTO1 (RF),(R1),3,K.TRNKREF    BILL#                                   
         GOTO1 (RF),(R1),5,BODUB3       BILL NET AMNT                           
         GOTO1 (RF),(R1),6,BODUB4       BILL COM AMNT                           
         GOTO1 (RF),(R1),11,K.TRNKDATE  BILL DATE                               
         GOTO1 (RF),(R1),10,BILLCURR    BILL CURRENCY                           
         GOTO1 (RF),(R1),13,CSCURBIL+(CURTDECP-CURTABD)                         
         GOTO1 (RF),(R1),17,READINDS                                            
         B     SNDPBNXT                                                         
         DROP  R2,R3,K                                                          
         EJECT                                                                  
***********************************************************************         
* GETPTREV                                                                      
***********************************************************************         
         SPACE 1                                                                
GETPTREV ST    RE,OVSVRE                                                        
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
         SR    R0,R0                                                            
         SR    R4,R4                                                            
         CLC   TRNKWORK,TRNORDER   ORDER - COPY PTAEL AND MASSAGE               
         BNE   GETP10                                                           
         GOTO1 AMCHORD,BOPARM,(C'I',(R3)),AIO2                                  
         L     R4,AIO2             FIND MATCHING COPIED/MASSAGED PTAEL          
COP      USING PTAELD,R4                                                        
GETP02   CLI   COP.PTAEL,0         TEST MASSAGED PTAEL NOT FOUND                
         BE    GETP10                                                           
         CLI   COP.PTAEL,PTAELQ                                                 
         BNE   GETP06                                                           
         CLI   COP.PTATYPE,PTATRAL                                              
         BNE   GETP06                                                           
         TM    COP.PTASTAT1,PTASPEND                                            
         BO    GETP06                                                           
         CLC   COP.PTARBLNO,BILLNUM                                             
         BE    GETP10              R4=A(MATCHING COPIED/MASSAGED PTAEL)         
GETP06   IC    R0,COP.PTALN                                                     
         AR    R4,R0                                                            
         B     GETP02                                                           
*                                                                               
GETP10   IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         CLI   PTAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   GETP10                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   GETP10                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    GETP10                                                           
         CLC   PTARBLNO,BILLNUM                                                 
         BNE   GETP10                                                           
         LTR   R4,R4               ORDER - R4=A(COPIED/MASSAGED PTAEL)          
         BNZ   *+6                                                              
         LR    R4,R3               ELSE - R4=A(ORIGINAL PTAEL)                  
         L     RE,OVSVRE                                                        
         BR    RE                                                               
         DROP  R3,R2,COP                                                        
         EJECT                                                                  
                                                                                
                                                                                
***********************************************************************         
* FUDGE TO CHANGE PRORATA BLOCK TO CONTAIN BILL AMOUNTS               *         
*                                                                     *         
* NTRY: P1 = A(PTAELD)                                                *         
*       P2 = A(COPIED PTAELD)                                         *         
*       P3 = A(PRO-RATA BLOCK TO UPDATE)                              *         
* THIS IS SO THAT WORKCODE TOTALS / SCREEN TOTALS ARE CORRECT         *         
***********************************************************************         
         SPACE 1                                                                
CHGPRO   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         USING PTAELD,R2                                                        
COP      USING PTAELD,R3                                                        
         USING PRORATAD,R4                                                      
*                                                                               
         ZAP   BILLNET,COP.PTANET  SET NET/COMMISSION BILL AMOUNTS              
         MP    BILLNET,=P'-1'                                                   
         ZAP   BILLCOM,COP.PTARCOM                                              
         MP    BILLCOM,=P'-1'                                                   
         XR    RE,RE                                                            
         ICM   RE,3,COP.PTAHOURS                                                
         CVD   RE,BILLHRS                                                       
         ZAP   BILLNETF,BCPZERO                                                 
         ZAP   BILLCOMF,BCPZERO                                                 
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    CPRO02                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    CPRO02                                                           
         ZAP   BILLNETF,COP.PTANETF                                             
         MP    BILLNETF,=P'-1'                                                  
         ZAP   BILLCOMF,COP.PTARFCOM                                            
         MP    BILLCOMF,=P'-1'                                                  
*                                                                               
CPRO02   TM    PTASTAT1,PTASREVD   TEST IS PENDING REVERSAL                     
         BO    CPRO04                                                           
         ZAP   PM$ANVBL,BILLNET    NO - SET AVAILABLE TO BILL AMOUNTS           
         ZAP   PM$FNVBL,BILLNETF                                                
         ZAP   PM$ACVBL,BILLCOM                                                 
         ZAP   PM$FCVBL,BILLCOMF                                                
         ZAP   PM$HRVBL,BILLHRS                                                 
         ZAP   PP$AALLO,BCPZERO        - AND ALLOCATED TO ZERO                  
         ZAP   PP$ACOMM,BCPZERO                                                 
         ZAP   PP$HRSB,BCPZERO                                                  
         ZAP   PP$FALLO,BCPZERO                                                 
         ZAP   PP$FCOMM,BCPZERO                                                 
         B     CHGPROX                                                          
*                                                                               
CPRO04   ZAP   PP$AALLO,BILLNET    YES - SET ALLOCATED TO BILL AMOUNTS          
         ZAP   PP$ACOMM,BILLCOM                                                 
         ZAP   PP$HRSB,BILLHRS                                                  
         ZAP   PP$FALLO,BILLNETF                                                
         ZAP   PP$FCOMM,BILLCOMF                                                
         ZAP   PM$ANVBL,BCPZERO        - AND SET AVAILABLE TO ZERO              
         ZAP   PM$FNVBL,BCPZERO                                                 
         ZAP   PM$ACVBL,BCPZERO                                                 
         ZAP   PM$FCVBL,BCPZERO                                                 
         ZAP   PM$HRVBL,BCPZERO                                                 
*                                                                               
CHGPROX  B     EXITY                                                            
         DROP  R2,COP,R4                                                        
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
ACCMST   DC    C'ACCMST '                                                       
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* MAP TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
*                                  ** BILL LIST **                              
BLEL     DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#BIL)         ELEMENT CODE                                 
         DC    AL2(BLELX+1-BLEL)   DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDBL-CLB58)    SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(1)              MAPPING CODE                                 
         DC    CL5'DA   '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BEDKDA)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVDEL-CLB58)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(2)              MAPPING CODE                                 
         DC    CL5'JOB  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BEDPJOB)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(3)              MAPPING CODE                                 
         DC    CL5'BLNUM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BEDPBLNO)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBILNO-CLB58) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(4)              MAPPING CODE                                 
         DC    CL5'FRMAT'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BEDPFORM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(5)              MAPPING CODE                                 
         DC    CL5'NET  '          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'SENDNET)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(6)              MAPPING CODE                                 
         DC    CL5'COMM '          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'SENDCOM)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(7)              MAPPING CODE                                 
         DC    CL5'DCRET'          TEXT IDENTIFIER                              
         DC    AL1(MDTCDQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BEDPCRED)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(8)              MAPPING CODE                                 
         DC    CL5'DEXPR'          TEXT IDENTIFIER                              
         DC    AL1(MDTCDQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BEDKEXPD)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(9)              MAPPING CODE                                 
         DC    CL5'DBILL'          TEXT IDENTIFIER                              
         DC    AL1(MDTCDQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BEDPBILD)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE                                 
         DC    CL5'CURR '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CURTCUR)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE                                 
         DC    CL5'BILDT'          TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDATE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(13)             MAPPING CODE                                 
         DC    CL5'CURDP'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CURTDECP)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(14)             MAPPING CODE                                 
         DC    CL5'ASTAT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(15)             MAPPING CODE                                 
         DC    CL5'ADNET'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVNETAM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(16)             MAPPING CODE                                 
         DC    CL5'ADCOM'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ADVCOMAM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(17)             MAPPING CODE                                 
         DC    CL5'INDIC'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(18)             MAPPING CODE                                 
         DC    CL5'PERS'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PERSON)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(19)             MAPPING CODE                                 
         DC    CL5'DATE'           TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (DATE)                             
         DC    AL1(L'RACDATE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(20)             MAPPING CODE                                 
         DC    CL5'REPLC'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(21)             MAPPING CODE                                 
         DC    CL5'NAME '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
BLELX    DC    XL1'00'             END OF ELEMENT FIELDS                        
*                                                                               
BLREL    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#BLR)         ELEMENT CODE                                 
         DC    AL2(BLRELX+1-BLREL) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDBLREV-CLB58) SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(255)            MAPPING CODE                                 
         DC    CL5'JOB  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKACT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJOB-CLB58)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(254)            MAPPING CODE                                 
         DC    CL5'99DA '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDA)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCV99DA-CLB58)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(253)            MAPPING CODE                                 
         DC    CL5'ALLRV'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVALLRV-CLB58) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'DA'             TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDA)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVDA-CLB58)    RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'WC'             TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKWORK)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'CONTR'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'DATE'           TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDATE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'REF'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKREF)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'BLNUM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BEDPBLNO)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBILNO-CLB58) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE                                 
         DC    CL5'AVAIL'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(8)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE                                 
         DC    CL5'RVACT'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVRVACT-CLB58) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(12)             MAPPING CODE                                 
         DC    CL5'ALLOC'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(8)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(13)             MAPPING CODE                                 
         DC    CL5'COMBL'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(8)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(14)             MAPPING CODE                                 
         DC    CL5'COMAL'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(8)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
BLRELX   DC    XL1'00'             END OF ELEMENT FIELDS                        
*                                                                               
MAPTABX  DC    X'00'               E-O-T                                        
*                                                                               
TRNORDER DC    C'**'                                                            
WC99     DC    C'99'                                                            
*                                                                               
**********************************************************************          
* LOCAL DSECTS                                                       *          
**********************************************************************          
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
         DS    (L'OSVALS-(*-OSVALS))X                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
       ++INCLUDE ACCLBLINK                                                      
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   OVRWS                                                            
EXPDATE  DS    XL(L'BLHEXPD)                                                    
PERSON   DS    CL8                                                              
DATE     DS    PL3                                                              
BILLDA   DS    XL(L'BEDKDA)        BILL DA                                      
THISJOB  DS    XL(L'TRNKACT)                                                    
BILLNUM  DS    XL(L'CSBILNUM)                                                   
RJOBCURR DS    CL3                                                              
BILLCURR DS    CL3                                                              
TRNDA    DS    XL(L'TRNKDA)        TRANSACTION DA                               
SENDNET  DS    PL8                 GRAND NET TOTAL ON BILL                      
SENDCOM  DS    PL8                 GRAND COMMISSION TOTAL ON BILL               
ADVNETAM DS    PL6                 TOTAL NET ADVANCE ON BILL                    
ADVCOMAM DS    PL6                 TOTAL NET COMMISSION ON BILL                 
ALLSTAT  DS    CL1                 ALLOCATION STATUS                            
REPLICA  DS    XL1                 NON ZERO=YES                                 
OVSVRE   DS    A                   SAVED RE                                     
RACTION  DS    XL1                 1=CLEAR PENDING REVERSAL                     
FININDS  DS    XL1                                                              
FINISH   EQU   X'80'                                                            
READINDS DS    XL1                                                              
READMN   EQU   X'80'               MAINFRAME CBILL BILLS                        
READPC   EQU   X'40'               PC BILLING BILLS                             
RIREVPND EQU   X'20'               REVERSAL PENDING (PREVBILL)                  
REVALL   DS    XL1                 ALL ITEMS MARKED FOR REVERSAL INDI           
SCR99DA  DS    A                   99 D/A                                       
SCR99CNT DS    A          COUNT OF OF REVERSED ITEMS RECEIVD THIS TIME          
TMPNAME  DS    CL(L'NAMEREC)       TEMPLATE NAME                                
TMPNMLN  DS    XL1                 TEMPLATE NAME LENGTH                         
*                                                                               
         DS    0D                                                               
BILLNET  DS    PL8                 ITEM NET AMOUNT (USED BY CHGPRO)             
BILLNETF DS    PL8                 ITEM FOREIGN NET AMOUNT                      
BILLCOM  DS    PL8                 ITEM COMMISSION AMOUNT                       
BILLCOMF DS    PL8                 ITEM FOREIGN COMMISSION AMOUNT               
BILLHRS  DS    PL8                 ITEM HOURS                                   
REVAGNET DS    PL8                 AGENCY NET AMOUNT (FOREIGN BILL)             
REVAGCOM DS    PL8                 AGENCY COMM AMOUNT (FOREIGN)                 
         DS    (L'OVRWS-(*-OVRWS))X                                             
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033ACCLB58   08/17/00'                                      
         END                                                                    
