*          DATA SET ACCLB57    AT LEVEL 120 AS OF 08/16/00                      
*PHASE T62157A                                                                  
CLB57    TITLE '- PC COMMS - SUMMARY/ENQUIRY'                                   
CLB57    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL SUMMWRKX-SUMMWRKD,**CB57**,R8,RR=RE                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R6,RC                                                            
         USING SUMMWRKD,R6                                                      
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
         SPACE 1                                                                
RCVJOB   MVC   THISJOB,DATA                                                     
         B     EXITY                                                            
**********************************************************************          
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
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         ICM   RF,15,OSND                                                       
         BNZR  RF                                                               
         B     EXITY                                                            
                                                                                
SNDJS    DS    0H                                                               
         CLC   THISJOB,BCSPACES                                                 
         BNH   EXITY                                                            
         GOTO1 ASETUP,BODMCB,THISJOB,0,0                                        
         CLC   CSCPYCUR,CSBILCUR   TEST BILLING IN AGENCY CURRENCY              
         BNE   *+8                                                              
         OI    EINDS,EIAGYCUR                                                   
         BAS   RE,SAVETSAR                                                      
         BAS   RE,READ                                                          
         BAS   RE,SORTBUFF                                                      
SND02    BAS   RE,EXBUFF                                                        
         BNE   EXITY               NO MORE                                      
         GOTO1 ASNDHDR,BODMCB,MH#SUM                                            
         LA    R2,BOELEM                                                        
         USING WBWORKD,R2                                                       
         GOTO1 ASNDDATA,BODMCB,20,RECNUM                                        
         GOTO1 (RF),(R1),1,WBWC                                                 
         GOTO1 (RF),(R1),2,WBCESNET                                             
         GOTO1 (RF),(R1),3,WBCHARGE                                             
         GOTO1 (RF),(R1),4,WBORDERS                                             
         GOTO1 (RF),(R1),5,WBBILNET                                             
         GOTO1 (RF),(R1),6,WBALLNET                                             
         GOTO1 (RF),(R1),7,WBALLCOM                                             
         GOTO1 (RF),(R1),8,WBBILGRS                                             
         GOTO1 (RF),(R1),9,WBUNBNET                                             
         GOTO1 (RF),(R1),10,WBCESGRS                                            
         GOTO1 (RF),(R1),11,WBOESNET                                            
         GOTO1 (RF),(R1),12,WBOESGRS                                            
         GOTO1 (RF),(R1),13,WBUNBGRS                                            
         GOTO1 (RF),(R1),14,WBBILCOM                                            
         GOTO1 (RF),(R1),15,WBUNBCOM                                            
         GOTO1 (RF),(R1),16,WBINVAMT                                            
         GOTO1 (RF),(R1),17,WBINVPCT                                            
         GOTO1 (RF),(R1),18,WBUPDWOF                                            
         B     SND02                                                            
         DROP  R2                                                               
*                                                                               
**********************************************************************          
* READ TRANSACTION RECORDS AND BUILD UNIVERSAL W/C BUFFER ENTRY      *          
**********************************************************************          
         SPACE 1                                                                
         USING PRORATAD,PRATBLK                                                 
READ     ST    RE,RETURN1                                                       
         BAS   RE,UBSET            INITIALISE BUFFER                            
         LA    R2,IOKEY            READ JOB RECORD                              
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   ACTKACT,THISJOB                                                  
         MVC   SIOKEY,IOKEY                                                     
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,ACTKDA                                                  
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETOPT,BOPARM,AIO1 ESTABLISH JOB OPTIONS                        
         BAS   RE,JESTER           GET JOB ESTIMATES                            
         DROP  R2                                                               
*                                                                               
         MVC   IOKEY,SIOKEY                                                     
         GOTO1 AIO,IOREAD+IOACCDIR      RE-READ TO ESTABLISH SEQUENCE           
         BE    *+6                                                              
         DC    H'0'                                                             
READ02   GOTO1 AIO,IOSEQ+IOACCDIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(L'ACTKCULA),SIOKEY                                         
         BNE   READX                                                            
         MVC   IODAOVER,IOKEY+(TRNKDA-TRNRECD)                                  
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,FILTTRN          APPLY TRANSACTION FILTERS                    
         BL    READ02                                                           
         BE    READ04                                                           
*                                                                               
         GOTO1 AGETOPT,BOPARM,AIO1 ESTABLISH W/C OPTIONS                        
         L     RF,AGOPBLK                                                       
         MVC   AGYCOMM,GOAGYCOM-GOBLOCKD(RF)                                    
READ04   XR    R0,R0                                                            
         TM    EINDS,EIAGYCUR                                                   
         BO    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 APRORATA,BOPARM,AIO1,AGOPBLK,ACOM,(R0),PRATBLK,0                 
*                                                                               
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         BAS   RE,UBSET            R1=A(UTILITY BUFFER)                         
         USING WBWORKD,R1                                                       
         MVC   WBWC,TRNKWORK       USE TX KEY W/C UNLESS AN ORDER               
         CLC   TRNKWORK,=C'**'                                                  
         BNE   READ10                                                           
         LA    R3,TRNRFST                                                       
         XR    R0,R0               ESTABLISH W/C FOR ORDERS                     
READ06   CLI   0(R3),0                                                          
         BE    READ12                                                           
         CLI   0(R3),OAMELQ                                                     
         BE    READ08                                                           
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     READ06                                                           
READ08   MVC   WBWC,OAMWORK-OAMELD(R3)                                          
         AP    WBORDERS,PA$NET     NET (ORDERS)                                 
         B     *+10                                                             
READ10   AP    WBCHARGE,PA$NET     NET (CHARGES)                                
         AP    WBBILNET,PA$NETBL   NET BILLING (PRIOR BILLS)                    
         AP    WBBILGRS,PA$GRSBL   GROSS BILLING (TOTAL BILLING)                
         AP    WBALLNET,PP$AALLO   ALLOCATED NET                                
         AP    WBALLCOM,PP$ACOMM   ALLOCATED COMMISSION                         
         AP    WBUNBNET,PA$NETUB   UNBILLED NET                                 
         AP    WBUNBGRS,PA$GRSUB   UNBILLED GROSS                               
         AP    WBBILCOM,PA$COMBL   BILLED COMMISSION                            
         AP    WBUNBCOM,PA$COMUB   UNBILLED COMMISSION                          
         BAS   RE,CRCV             INCOME VARIANCE                              
         AP    WBINVAMT,BODUB1                                                  
         BAS   RE,CRCO             % INCOME VARIANCE (USE OPTIONS COMM)         
         AP    WBINVPCT,BODUB1                                                  
         AP    WBUPDWOF,PA$WOFAM   UPDATED WRITE-OFFS                           
         BAS   RE,UPBUFF           UPDATE UNIVERSAL W/C BUFFER                  
         BAS   RE,UPACC            UPDATE ACCUMULATORS                          
READ12   L     RF,AIO1             RE-READ TO ESTABLISH SEQUENCE                
         MVC   IOKEY,0(RF)                                                      
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     READ02                                                           
READX    L     RE,RETURN1                                                       
         BR    RE                                                               
         DROP  R1,R2                                                            
         EJECT                                                                  
**********************************************************************          
* FILTER TRANSACTIONS                                                *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
FILTTRN  ST    RE,RETURN2                                                       
         L     R2,AIO1                                                          
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ                                                     
         BNE   FILTLO                                                           
         CLC   =C'99',TRNKWORK                                                  
         BE    FILTLO              EXCLUDE BILLING                              
*                                                                               
         TM    TRNRSTAT,TRNSDRFT   IGNORE DRAFT TRANSACTIONS                    
         BZ    *+12                                                             
         CLI   TRNTYPE,99          ALLOCATION VEHICLE                           
         BNE   FILTLO                                                           
*                                                                               
         TM    TRNSTAT,TRNSHOLD    IGNORE HELD ITEMS                            
         BO    FILTLO                                                           
         TM    TRNSTAT,TRNSREV     IGNORE REVERSALS                             
         BO    FILTLO                                                           
*                                                                               
         CLC   TRNKWORK,=C'**'                                                  
         BE    FILTHI                                                           
         CLC   TRNKWORK,SWC                                                     
         BE    FILTEQ                                                           
*                                                                               
FILTHI   CLI   *,0                 WANTED (CALL GETOPT)                         
         B     *+14                                                             
FILTEQ   CR    RE,RE               WANTED (DON'T CALL GETOPT)                   
         B     *+8                                                              
FILTLO   CLI   *,255               NOT WANTED                                   
         L     RE,RETURN2                                                       
         BR    RE                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* GET JOB ESTIMATE VALUES FROM JOBBER                                *          
**********************************************************************          
         SPACE 1                                                                
JESTER   NTR1                                                                   
         LA    RE,COLIST           INITIALISE JOBBER                            
         ST    RE,ACOLIST                                                       
*&&UK*&& GOTO1 VJOBCOL,BOPARM,(X'FF',LOOKFLDH),ACOLIST,ACOM                     
*&&US*&& GOTO1 VJOBCOL,BOPARM,(4,LOOKFLDH),ACOLIST,ACOM                         
*                                                                               
         SR    RE,RE               ENSURE JOBBER BLOCK IS INITIALISED           
         SR    RF,RF                                                            
         L     R0,AJOBBLK                                                       
         LA    R1,JBLOCKL                                                       
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,AJOBBLK          R5=A(JOBBER BLOCK)                           
         USING JOBLOCKD,R5                                                      
         MVC   JBAJOB,AIOA         A(JOB RECORD) - OLD FILE                     
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOM                                                      
         L     RF,AGOPBLK          CLEAR OUT A(BILL EXTENSION BLOCK)            
         XC    (GOABEXT-GOBLOCK)(L'GOABEXT,RF),(GOABEXT-GOBLOCK)(RF)            
         MVC   JBAGOBLK,AGOPBLK                                                 
         MVC   JBGETOPT,VGETOPT                                                 
         MVC   JBAIO,AIO3                                                       
*                                                                               
         LA    RE,SUMMCOLT         COLUMN TABLE                                 
         STCM  RE,15,JBACOLTB                                                   
         LA    RE,SUMMCOLL                                                      
         STCM  RE,15,JBLCOLTB                                                   
         LA    RE,SUMMOPVT         OPERAND VALUE TABLE                          
         STCM  RE,15,JBAOPVTB                                                   
         LA    RE,SUMMOPVL                                                      
         STCM  RE,15,JBLOPVTB                                                   
         GOTO1 VJOBBER,BOPARM,(R5)                                              
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,JBACOLTB         R4=A(COLUMN OUTPUT TABLE)                    
         USING JBCOLD,R4                                                        
*                                                                               
         LR    RE,R4               FIND A PRIMARY WC                            
         USING PRIMED,RE                                                        
         XR    R1,R1                                                            
         ICM   R1,3,JBNROWS                                                     
JEST02   CLI   PRITYPE,JBCOLTWC                                                 
         BNE   JEST08                                                           
         LR    R0,R1                                                            
         BCT   R0,*+8                                                           
         B     JEST10                                                           
         LR    RF,RE               FIND A SECONDARY WC                          
         AH    RF,JBLCOL                                                        
         USING SECOND,RF                                                        
JEST04   CLI   SECTYPE,JBCOLTWC    MATCH PRIMARY AND SECONDARY WCS              
         BNE   JEST06                                                           
         CLC   PRIWC,SECWC                                                      
         BNE   JEST06                                                           
         AP    PRIOE,SECOE         ADD WC VALUES AT PRIMARY LEVEL               
         AP    PRICE,SECCE                                                      
         MVI   SECTYPE,X'FF'                                                    
JEST06   AH    RF,JBLCOL           BUMP SECONDARY POINTER                       
         BCT   R0,JEST04                                                        
JEST08   AH    RE,JBLCOL           BUMP PRIMARY POINTER                         
         BCT   R1,JEST02                                                        
         DROP  RE,RF                                                            
*                                                                               
JEST10   XR    R2,R2                                                            
         ICM   R2,3,JBNROWS                                                     
         LH    R3,JBLCOL                                                        
         BZ    JESTX                                                            
JEST12   CLI   JBCOLTYP,JBCOLTWC                                                
         BNE   JEST14                                                           
         BAS   RE,UBSET            R1=A(UTILITY BUFFER)                         
         USING WBWORKD,R1                                                       
         MVC   WBWC,JBCOLWC                                                     
         AP    WBOESNET,JBCOLVAL                                                
         AP    WBOESGRS,JBCOLVAL+6                                              
         AP    WBCESNET,JBCOLVAL+12                                             
         AP    WBCESGRS,JBCOLVAL+18                                             
         BAS   RE,UPBUFF           UPDATE UNIVERSAL W/C BUFFER                  
         BAS   RE,UPACC            UPDATE ACCUMULATORS                          
JEST14   AR    R4,R3                                                            
         BCT   R2,JEST12                                                        
JESTX    XIT1                                                                   
         DROP  R1,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* INITIALISE UTILITY BUFFER AND ACCUMULATORS                         *          
* EXIT - R1=A(UTILITY BUFFER)                                        *          
**********************************************************************          
         SPACE 1                                                                
UBSET    LA    R1,BOELEM                                                        
         USING WBWORKD,R1                                                       
         XC    BOELEM,BOELEM                                                    
         LA    RF,WBTSADLN                                                      
         LR    R0,RF                                                            
         LA    RF,WBTSADAT                                                      
         AR    R0,RF                                                            
         ZAP   0(WBELM,RF),=P'0'                                                
         LA    RF,WBELM(RF)                                                     
         CR    RF,R0                                                            
         BL    *-12                                                             
         TM    EINDS,EIACCS                                                     
         BO    *+14                                                             
         MVC   ACCBLOCK,BOELEM                                                  
         OI    EINDS,EIACCS                                                     
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 1                                                                
**********************************************************************          
* UPDATE ACCUMULATORS                                                *          
* ENTRY - R1=A(UTILITY BUFFER)                                       *          
**********************************************************************          
         SPACE 1                                                                
UPACC    ST    RE,RETURN2                                                       
         USING WBWORKD,R1                                                       
         LA    RE,WBTSADAT                                                      
         LA    R0,WBTSADX                                                       
         LA    RF,ACCBLOCK+WBTSAKLN                                             
UPA02    CLC   0(WBELM,RE),PL8ZERO                                              
         BE    *+10                                                             
         AP    0(WBELM,RF),0(WBELM,RE)                                          
         LA    RE,WBELM(RE)                                                     
         LA    RF,WBELM(RF)                                                     
         CR    RE,R0                                                            
         BL    UPA02                                                            
         L     RE,RETURN2                                                       
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* COLUMN ROUTINES (PRE-SORT)                                         *          
**********************************************************************          
         SPACE 1                                                                
CRCO     ST    RE,RETURN2          COMM (OPTS) = B.NET * COMM RATE              
         ZAP   BOPL81,AGYCOMM                                                   
         ZAP   MYPACK,PA$NETBL                                                  
         SRP   MYPACK,2,0                                                       
         MP    MYPACK,BOPL81                                                    
         SRP   MYPACK,64-8,5                                                    
         ZAP   BODUB1,MYPACK                                                    
CRCOX    L     RE,RETURN2                                                       
         BR    RE                                                               
*                                                                               
CRCV     ST    RE,RETURN3          VAR = B.COMM (ACT) - COMM (OPTS)             
         BAS   RE,CRCO                                                          
         ZAP   MYPACK,PA$COMBL                                                  
         SP    MYPACK,BODUB1                                                    
         MVC   BODUB1,MYPACK+(L'BODUB1)                                         
CRCVX    L     RE,RETURN3                                                       
         BR    RE                                                               
         SPACE 1                                                                
**********************************************************************          
* COLUMN ROUTINES  (POST-SORT)                                       *          
**********************************************************************          
         SPACE 1                                                                
CRCVP    ST    RE,RETURN2          %VAR = VAR / COMM (OPTS)                     
         ZAP   BODUB1,PL8ZERO                                                   
         LA    R1,BOELEM                                                        
         USING WBWORKD,R1                                                       
         CP    WBINVPCT,PL8ZERO                                                 
         BE    CRCVPX                                                           
         ZAP   MYPACK,WBINVAMT                                                  
         SRP   MYPACK,4,5                                                       
         DP    MYPACK,WBINVPCT                                                  
         ZAP   WBINVPCT,MYPACK(L'WBINVPCT)                                      
CRCVPX   L     RE,RETURN2                                                       
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* SAVE CURRENT TSAR BUFFER TO DISK                                   *          
**********************************************************************          
         SPACE 1                                                                
SAVETSAR NTR1                                                                   
         USING TSARD,R2                                                         
         GOTO1 ATSARIO,TSASAV                                                   
         L     R2,ATSABLK                                                       
         MVI   TSRECI,0            INITIALISE FOR W/C BUFFER                    
         MVI   TSINDS,TSINODSK                                                  
         MVI   TSIND2,0                                                         
         LA    R1,WBWORK                                                        
         ST    R1,TSAREC                                                        
         MVI   TSKEYL,WBTSAKLN                                                  
         MVC   TSRECL,=Y(WBTSARLN)                                              
         MVI   TSACTN,TSAINI                                                    
         GOTO1 VTSAR,(R2)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* EXTRACT DATALINE FROM UNIVERSAL W/C BUFFER                         *          
**********************************************************************          
         SPACE 1                                                                
EXBUFF   NTR1                                                                   
         USING WBWORKD,R3                                                       
         L     R2,ATSABLK                                                       
         LA    R3,WBWORK                                                        
         XR    RF,RF                                                            
         ICM   RF,3,RECNUM                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,3,RECNUM                                                      
         CLM   RF,3,TSPRECN        ALL RECORDS PROCESSED?                       
         BH    EXITN                                                            
         STH   RF,TSRNUM                                                        
         MVI   TSACTN,TSAGET       NO - GET DATALINE                            
         GOTO1 VTSAR,(R2)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BOELEM(WBTSARLN),WBTSAREC                                        
         B     EXITY                                                            
         SPACE 1                                                                
**********************************************************************          
* SORT UNIVERSAL W/C BUFFER                                          *          
**********************************************************************          
         SPACE 1                                                                
SORTBUFF NTR1                                                                   
         L     R2,ATSABLK                                                       
         MVI   TSACTN,TSASRT       ACTION SORT                                  
         MVI   TSRTKSEQ,0          ASCENDING SORT                               
         MVI   TSRTKDSP,0          DISPLACEMENT TO SORT KEY                     
         MVI   TSRTKLEN,WBTSAKLN   LENGTH OF SORT KEY                           
         GOTO1 VTSAR,(R2)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* UPDATE UNIVERSAL W/C BUFFER                                        *          
**********************************************************************          
         SPACE 1                                                                
UPBUFF   NTR1                                                                   
         L     R2,ATSABLK                                                       
         XC    TSRNUM,TSRNUM                                                    
         LA    R3,WBWORK                                                        
*                                                                               
UPB02    XR    R1,R1                                                            
         ICM   R1,3,TSRNUM                                                      
         LA    R1,1(R1)            INCREMENT TSAR RECORD NUMBER                 
         CLM   R1,3,TSPRECN        ALL RECORDS PROCESSED?                       
         BNH   UPB04                                                            
         MVC   WBTSAREC(WBTSARLN),BOELEM  YES - ADD NEW BUFFER ENTRY            
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR,(R2)                                                       
         BE    UPBX                                                             
         DC    H'0'                                                             
UPB04    STH   R1,TSRNUM                                                        
         MVI   TSACTN,TSAGET       GET NEXT BUFFER ENTRY                        
         GOTO1 VTSAR,(R2)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   WBWC,BOELEM         MATCH ON W/C                                 
         BNE   UPB02                                                            
         LA    RE,BOELEM+(WBTSADAT-WBTSAREC)                                    
         LA    RF,WBTSADLN                                                      
         LR    R0,RF                                                            
         LA    RF,WBTSADAT                                                      
         AR    R0,RF                                                            
UPB06    AP    0(WBELM,RF),0(WBELM,RE)                                          
         LA    RE,WBELM(RE)                                                     
         LA    RF,WBELM(RF)                                                     
         CR    RF,R0                                                            
         BL    UPB06                                                            
         MVI   TSACTN,TSAWRT       WRITE BACK BUFFER ENTRY                      
         GOTO1 VTSAR,(R2)                                                       
         BE    UPBX                                                             
         DC    H'0'                                                             
UPBX     XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
ACCMST   DC    C'ACCMST '                                                       
FF       EQU   X'FF'                                                            
*&&UK                                                                           
LOOKFLDH DC    AL2(AC#OEN,AC#OEG,AC#CEN,AC#CEG,0)                               
*&&                                                                             
*&&US                                                                           
LOOKFLDH DC    XL5'00'                                                          
         DC    AL1(L'LOOKFLD)                                                   
         DC    XL3'00'                                                          
LOOKFLD  DC    C'OEN,OEG,CEN,CEG'                                               
*&&                                                                             
PL8ZERO  DC    PL8'0'                                                           
YES      EQU   C'Y'                                                             
         EJECT                                                                  
***********************************************************************         
* MAP TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
*                                  ** JOB SUMMARY **                            
JSEL     DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#SUM)         ELEMENT CODE                                 
         DC    AL2(JSELX+1-JSEL)   DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDJS-CLB57)    SEND ROUTINE                                 
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
         DC    AL2(RCVJOB-CLB57)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(20)             MAPPING CODE                                 
         DC    CL5'RECNM'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'RECNUM)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'WC   '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBWC)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'CENET'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBCESNET)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'CHRGE'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBCHARGE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'ORDRS'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBORDERS)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'BLNET'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBBILNET)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'ALNET'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBALLNET)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             MAPPING CODE                                 
         DC    CL5'ALCOM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBALLCOM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             MAPPING CODE                                 
         DC    CL5'TOTBL'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBBILGRS)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(09)             MAPPING CODE                                 
         DC    CL5'UNBIL'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBUNBNET)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE                                 
         DC    CL5'CEGRS'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBCESGRS)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE                                 
         DC    CL5'OENET'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBOESNET)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(12)             MAPPING CODE                                 
         DC    CL5'OEGRS'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBOESGRS)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(13)             MAPPING CODE                                 
         DC    CL5'UBGRS'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBUNBGRS)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(14)             MAPPING CODE                                 
         DC    CL5'BLCOM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBBILCOM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(15)             MAPPING CODE                                 
         DC    CL5'UBCOM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBUNBCOM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(16)             MAPPING CODE                                 
         DC    CL5'INVAR'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBINVAMT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(17)             MAPPING CODE                                 
         DC    CL5'INPCT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBINVPCT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(18)             MAPPING CODE                                 
         DC    CL5'UPWOF'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WBUPDWOF)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
JSELX    DC    XL1'00'             END OF ELEMENT FIELDS                        
*                                                                               
MAPTABX  DC    X'00'               E-O-T                                        
**********************************************************************          
* LOCAL DSECTS                                                       *          
**********************************************************************          
         SPACE 1                                                                
WBWORKD  DSECT                     >> UNIVERSAL W/C BUFFER <<                   
WBTSAREC DS    0X                  TSAR RECORD                                  
WBTSAKEY DS    0X                  TSAR KEY                                     
WBWC     DS    XL2                 WORKCODE                                     
WBTSAKLN EQU   *-WBTSAKEY          LENGTH OF TSAR KEY                           
WBTSADAT DS    0X                  TSAR COLUMNS                                 
WBCESNET DS    PL8                 CURRENT ESTIMATE NET                         
WBCHARGE DS    PL8                 CHARGES                                      
WBORDERS DS    PL8                 ORDERS                                       
WBBILNET DS    PL8                 NET BILLING (PRIOR BILLS)                    
WBALLNET DS    PL8                 ALLOCATION NET                               
WBALLCOM DS    PL8                 ALLOCATION COMMISSION                        
WBBILGRS DS    PL8                 GROSS BILLING (TOTAL BILLING)                
WBUNBNET DS    PL8                 UNBILLED                                     
WBCESGRS DS    PL8                 CURRENT ESTIMATE GROSS                       
WBOESNET DS    PL8                 ORGINAL ESTIMATE NET                         
WBOESGRS DS    PL8                 ORIGINAL ESTIMATE GROSS                      
WBUNBGRS DS    PL8                 UNBILLED GROSS                               
WBBILCOM DS    PL8                 BILLED COMMISSION                            
WBUNBCOM DS    PL8                 UNBILLED COMMISSION                          
WBINVAMT DS    PL8                 INCOME VARIANCE                              
WBINVPCT DS    PL8                 % INCOME VARIANCE                            
WBUPDWOF DS    PL8                 UPDATED WRITE-OFFS                           
WBTSADX  EQU   *                   END OF TSAR DATA AREA                        
WBELM    EQU   *-WBUPDWOF          LENGTH OF TSAR DATA ELEMENT                  
WBTSADLN EQU   *-WBTSADAT          LENGTH OF TSAR DATA AREA                     
WBTSARLN EQU   *-WBTSAREC          LENGTH OF TSAR RECORD                        
*                                                                               
PRIMED   DSECT                     >> PRIMARY COLUMN VALUES <<                  
         DS    XL2                                                              
PRITYPE  DS    XL1                 COLUMN TYPE                                  
PRIWC    DS    XL2                 WORKCODE                                     
         DS    XL7                                                              
PRIOE    DS    PL6                 ORIGINAL ESTIMATE                            
PRICE    DS    PL6                 CURRENT ESTIMATE                             
         SPACE 1                                                                
SECOND   DSECT                     >> SECONDARY COLUMN VALUES <<                
         DS    XL2                                                              
SECTYPE  DS    XL1                 COLUMN TYPE                                  
SECWC    DS    XL2                 WORKCODE                                     
         DS    XL7                                                              
SECOE    DS    PL6                 ORIGINAL ESTIMATE                            
SECCE    DS    PL6                 CURRENT ESTIMATE                             
*                                                                               
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**********************************************************************          
* MY EXTRA WORKING STORAGE                                           *          
**********************************************************************          
         SPACE 1                                                                
SUMMWRKD DSECT                                                                  
SUMMOPVT DS    (SUMMOPVL)X                                                      
SUMMCOLT DS    (SUMMCOLL)X                                                      
SUMMOPVL EQU   4000                                                             
SUMMCOLL EQU   4000                                                             
SUMMWRKX EQU   *                                                                
         EJECT                                                                  
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
THISJOB  DS    XL(L'TRNKACT)       CLI/PRO/JOB                                  
RETURN1  DS    A                   RETURN ADDRESSES                             
RETURN2  DS    A                                                                
RETURN3  DS    A                                                                
ACOLIST  DS    A                   A(JOBBER COLUMN LIST)                        
RECNUM   DS    XL2                 RECORD NUMBER                                
EINDS    DS    XL1                 INDICATOR BYTE                               
EIAGYCUR EQU   X'80'               BILLING IN AGENCY CURRENCY                   
EIACCS   EQU   X'10'               ACCUMULATORS INITIALISED                     
AGYCOMM  DS    PL4                 AGENCY COMMISSION RATE                       
MYPACK   DS    PL16                                                             
SWC      DS    XL2                 SAVED WORKCODE                               
SIOKEY   DS    XL(L'IOKEY)                                                      
PRATBLK  DS    (PR$LNQ)C                                                        
*                                                                               
COLIST   DS    XL200                                                            
*                                                                               
ACCBLOCK DS    XL(WBTSARLN)        ACCUMULATORS                                 
*                                                                               
WBWORK   DS    XL(WBTSARLN)        W/C BUFFER TSAR RECORD                       
         DS    (L'OVRWS-(*-OVRWS))X                                             
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'120ACCLB57   08/16/00'                                      
         END                                                                    
