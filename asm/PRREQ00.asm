*          DATA SET PRREQ00    AT LEVEL 058 AS OF 06/09/20                      
*PHASE T41200A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PRREQ00   NEW REQUEST BASE T41200'                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BPLA 05/2014  CHANGES FOR AT&T INTERFACE - AI                                 
*                                                                               
* BPLA 04/2014  CHANGES FOR PZ - PFIZER INTERFACE                               
*                                                                               
* BPLA 02/2014  CHANGES FOR PO# PURGE                                           
*                                                                               
* BPLA 01/2013  NEW REPORT/INTERFACE - IN FOR NISSAN INTERFACE                  
*               NEW REPORT/INTERFACE - CH FOR CHOICE HOTELS                     
*                                                                               
* BPLA 03/2012  NEW REPORT/INTERFACE - WB FOR OMG                               
*                                                                               
* BPLA 02/2011  NEW REPORT/INTERFACE - JW  FOR JWT INTERFACE                    
*                                                                               
* BPLA 09/2009  NEW REPORT/INTERFACE - PTD MEG2                                 
*                                                                               
* BPLA 02/2009  NEW REPORT/INTERFACE - (PLO) FOR L'OREAL                        
*                                                                               
* BPLA 09/2008  SOON BILL OPTION FOR THE P10                                    
*                                                                               
* BPLA 05/2008  CHANGES FOR P98'S                                               
*                                                                               
* BPLA 12/2007  CHANGE RECORD TYPE, PURGE OPTION, CLT/PUB PURGE,                
*               AND COMMENT PURGE TO USE ONEVAL (4,05)                          
*                                                                               
* BPLA 09/2007  FOR MEDIA * REQUESTS TRY FOR MEDIA I FIRST                      
*                                                                               
* BPLA 06/2007  ALLOW BILLING BY PRODUCT GROUP - LIMITED RELEASE                
*                                                                               
* BOBY 01/2007  PLANNED COSTS OPTION FOR P49                                    
*                                                                               
* BPLA 08/2006  CHANGES FOR NEW REPORT (EX) XML TRANSMISSION                    
*                                                                               
* KWAN 07/2006  RT74 - ADD OPTIONS FIELD                                        
*                                                                               
* SMYE 03/2006  RT77 - ADD OPTIONS FIELD                                        
*                                                                               
* YKVA 05/2006  ADD GM REPORT - GENERAL MOTORS INTERFACE                        
*                                                                               
* BPLA 10/2005  ADD PH REPORT - PHILIP MORRIS INTERFACE                         
*                                                                               
* BPLA 09/2005  ADD SE REPORT - SPRINT INTERFACE                                
*                                                                               
* SMYE 05/2005  ADD LT REPORT - LABATT INTERFACE                                
*                                                                               
* BPLA 03/2005  ADD SN REPORT - SONY INTERFACE                                  
*      03/2005  ADD CI REPORT - CONTINENTAL INTERFACE                           
*                                                                               
* YKAP 05/2003 "GT" REPORT                                                      
*                                                                               
* YKAP 12/2002 HAPPY NEW YEAR "P07" REPORT                                      
*                                                                               
* YKAP 04/2002 SETTING UP ACCESS                                                
*                                                                               
* YKAP 04/11/2002 USE CORE-RESIDENT PUBVAL                                      
*                                                                               
* KWAN 02/08/02 FOGETINS112)GETINSLGETINS(SINGLE)                               
*                                                                               
* KWAN 04/25/01 ADD REPORT ID IC (I/OCOM) AND CC (CONCOM)                       
*                                                                               
* KWAN 04/09/01 NEW FIELDS FOR CLT/PRD/PUB GRP RECORD PURGES                    
*                                                                               
* SMYE 12/00     ADDED RT94 FOR PCM (CLT/PRD HEADERS CROSS-MEDIA COPY)          
*                AND NEW FIELDS FOR ABOVE IN FLDNUMT                            
*                                                                               
* BPLA 07/00     ALLOW EST=ALL FOR P92                                          
*                                                                               
* KWAN 07/00     ADD NEW FIELD TO PA8 (RT91): PRD SUMMARY FMT                   
*                                                                               
* BPLA 05/00     CHANGES FOR P92 - CLOSE-OUTS                                   
*                                                                               
* KWAN 04/00     UNDO NO-OP ON P41 (SEE BPLA 12/99), MAKING IT LIVE             
*                                                                               
* KWAN 03/00     ACCEPT VALUE "G" IN PURGE OPTION FLD OF RT01 & RT02            
*                                                                               
* KWAN 03/00     ADD NEW FIELD (EST) FOR P92                                    
*                                                                               
* BPLA  12/99    NO-OP NEW P41 FEATURE SO THAT WE CAN RELEASE                   
*                P43 FEATURE - SEE ***P41                                       
*                                                                               
* KWAN 11/99     ADD COMMENT TO P41 (I=INACTIVE CLT/PRD)                        
*                                                                               
* KWAN 10/99     ADD NEW REQ ID: RT01 AND RT02 (DDS ONLY)                       
*                                                                               
* KWAN 06/99     RT77 - ALLOW ESTIMATE FILTER (ALL,NNN)                         
*                                                                               
* SMYE 3/99      RT12 - CHANGED PRT OPT COMMENTS FROM 2 LINES TO 1              
*                  TO MAKE ROOM FOR NEW OPTIONS FIELD (EST=NNN)                 
*                  (SCREEN IS AT MAX LINES)                                     
*                                                                               
* SMYE 3/99      RT77 - CLIENT FIELD FORMAT CHANGED FROM 8F TO CF TO            
*                  PERMIT $N (OFFICE LIST)                                      
*                                                                               
* BPLA 9/98      ACTIVATE PUBLISHER FIELD FOR P14, P18                          
*                NEW COMMENT USED FOR DATE OPTION                               
*                (NOTE - NO PUBLISHER OPTION FOR P16)                           
*                DON'T ACCEPT "ALL" FOR PUBLISHER ON P46 OR P48                 
*                (IT DOESN'T WORK)                                              
*                                                                               
* SMYE 8/98      ADDED RT106 FOR PPCL (YNR COLGATE EXTRACT)                     
*                                                                               
* SMYE 8/10/98   RT10 - MEDIA BITS CHANGED TO PERMIT * (ALL) IN MEDIA           
*                                                                               
* SMYE 5/98      RT48 - CLIENT FIELD FORMAT CHANGED TO 0F TO PERMIT             
*                  *N (OFFICE ENTRY)                                            
*                                                                               
* SMYE 5/98      CHANGED COMMENT FOR LEVEL OPTION IN RT14 (NOP 5/13)            
*                ADDED PUBLISHER FIELD AND CHANGED DATE OPTION FIELD            
*                  COMMENTS TO A NEW SINGLE LINE COMMENT (RT19)                 
*                                                                               
* SMYE 4/98      ADDED RT105 FOR PPEB (EDI BILLING TRANS) - ADDED               
*                205, 206, 207 TO FLDNUMT                                       
*                                                                               
* SMYE 4/98      P48 - PUBLISHER OPTION REACTIVATED - ALSO ADDED                
*                  "OPTIONS" FIELD AS SCREEN HAS MAXIMUM 23 LINES               
*                  REACTIVATED PUB GROUP ASSIGNS (G) (ALL RT48)                 
*                P46 - ADD PUBLISHER OPTION AND MAKE PUBLICATION                
*                  OPTIONAL (RT46)                                              
*                                                                               
* BPLA   3/98    PAY CONTROLS ACTIVATED FOR P48 (RT48)                          
*                                                                               
* BPLA   3/98    CHANGE FOR PP19 (RT19) IMPLEMENTED FOR PPAU (RT219)            
*                                                                               
* SMYE 01/98     CHANGED RT118 "OPTIONS" FIELD FORMAT FROM 05 TO 0D             
*                                                                               
* SMYE 12/97     ADDED OPTIONS FIELD AND SHIFTED "DETAIL" OPTION TO             
*                  QOPT8 TO ALLOW FOR CONTINUATION REQUEST CARD (RT19)          
*                ALSO ADDED OPTIONS FIELD IN RT18                               
*                                                                               
* BPLA 8/97      SET AFIRST TO FIRST INPUT FIELD BELOW SERVICE                  
*                REQUEST                                                        
*                                                                               
* SMYE 1/97      P48  - ALL BELOW 7/96 ITEMS NO-OPED (RT48)                     
*                                                                               
* SMYE 12/96     P48  - PUBLISHER OPTION NO-OPED (RT48)                         
*                                                                               
* SMYE 7/96      P48  - ADD PUB GROUP ASSIGNS OPTION AND SCHEME FILTER          
*                       AND NEW COMMENT FOR OPTION                              
*                                                                               
* SMYE 4/96      P48  - ADD PUBLISHER OPTION                                    
*                                                                               
* BPLA 4/96      P48  - NEW COMMENT FOR TYPE OF LIST NO-OPED                    
*                                                                               
* BPLA 1/96      P43 PUBLISHER OPTIONS REACTIVATED                              
*                                                                               
* BPLA 1/96      P48 - ADD ANOTHER COMMENT FOR TYPE OF LIST                     
*                                                                               
* BPLA 12/95     P43 - TYPE OF LIST OPTION DECATIVATED AGAIN                    
*                -UPDATE STILL PENDING AND FRENCH I/O IS ALMOST                 
*                READY                                                          
*                                                                               
* BPLA 11/95     ADD LANGUAGE FILTER TO P46 AND P48                             
*                AND P43 TYPE OF LIST OPTION REACTIVATED                        
*                                                                               
* BPLA 11/95     NO-OP P43 TYPE OF LIST OPTION                                  
*                SO I CAN RELEASE P12 CHANGES                                   
*                                                                               
* BPLA 8/95      ADD TYPE OF LIST OPTION TO P43                                 
*                                                                               
* BPLA 5/95      CHANGE TO S2 FOR ADCODE RECAP AND COST                         
*                                                                               
* BPLA 3/30/95   CHANGES TO TS REQUEST                                          
*                SORT ADDED AND ES ALLOWED                                      
*                                                                               
* BPLA 2/22/95   CODE FOR NEW REPORT - Z5 (228)                                 
*                                                                               
* BPLA 11/1/94   CODE FOR NEW REPORT - TS                                       
*                                                                               
* BPLA 10/6/94   MOVE P12 FAX OPTION FROM R01-1 (COL 61)                        
*                TO RAPY (COL53)                                                
*                                                                               
* BPLA 8/94      CHANGES FOR FAXING CONTRACTS                                   
*                                                                               
* BPLA 7/94      ALLOW ESTIMATE RANGE FOR PRD=ALL                               
*                                                                               
* BPLA 3/94      DISABLE BA REQ (RT100)                                         
*                                                                               
* BPLA 12/93     ALLOW *N (OFFICE) REQUESTS FOR PA8                             
*                                                                               
* BPLA 11/19/93  NEW COMMENTS FOR 14 (FOR NEW SORT VALUES)                      
*                                                                               
* BPLA 8/10/93   REMOVE CHECK FOR SJR ONLY ACCESS TO RFP                        
*                                                                               
* BPLA 7/22/93   NEW OPTION ON PRA - QOPT4 "C" DISPLAY COST INSTEAD             
*                                              OF GROSS                         
* BPLA 7/21/93   ALLOW  PUB,ALL FOR P46                                         
*                                                                               
* BPLA 6/11/93   AGENCY FILTER TO AC AND AU                                     
*                                                                               
* BPLA 3/18/93   CHANGES FOR RPF                                                
*                                                                               
* BPLA 1/28/93   ADD CODE FOR NEW REPORT PPAR                                   
*                                                                               
* BPLA 10/28/92  CHANGE 52, 54, AND EC  - BILL/PAY TO DATE TYPE                 
*          CHANGE 60, S2, L1, LB, AND 77 - DATE OPTION TO DATE TYPE             
*          ALSO "S" ADDED TO 77 DATE TYPE LIST                                  
*                                                                               
* BPLA 7/2/92   KILL DATE FILTER FOR 48                                         
*                                                                               
* BPLA 4/28/92  ADD NET OPTION TO P49 + P27, P28 ALLOW EST OR EST               
*               RANGE WITH PRD= BLANK OR ALL                                    
*                                                                               
* BPLA 4/1/92   CHANGE TO OVERAGE OPTION ON RT79                                
*                                                                               
* BPLA 3/5/92   ADD CODE FOR PPAC AND PPAU                                      
*                                                                               
* BPLA 2/13/92  RT77 ADD SHIPPING ADDRESS COMMNET                               
*                                                                               
* BPLA 2/3/92   NEW PP48 FIELD RE-ACTIVATIED                                    
*                                                                               
* BPLA 1/29/92  NEW PP48 FIELD DE-ACTIVATED AND RT124 (MY)-DDS ONLY             
*                                                                               
* BPLA 1/21/92  ACTIVATED P49 PERIOD                                            
* BPLA 12/10/91 NO-OP P49 PERIOD  - PENDING UPDATE                              
*               SEE ***P49                                                      
*                                                                               
* BPLA 10/22/91 ALLOW P49 PERIOD                                                
*                                                                               
* BPLA 10/9/91 OFFICE LIST FOR PP27,PP28,PP36,PPNV                              
*                                                                               
* BPLA 7/31/91 ALLOW OFFICE LIST REQUESTS FOR PP37                              
*                                                                               
* BPLA 7/26/91 SALLY'S CHANGES TO PPMY (RT124)                                  
*                                                                               
* BPLA 6/25/91 OPTIONS FIELD ON B1,D1,R1,RD,52,EC ACTIVATED                     
*              SEE ***48 FOR OP-ED NEW FEATURES                                 
*              ALSO CODE ADDED FOR NEW REPORT (MY - RT124)                      
*                                                                               
* BPLA 6/12/91 ALLOW NNN-NNN ESTS FOR 54 AND L2                                 
*         SEE  ***B1,***D1,***R1,***RD,***52,***EC,***48                        
*              NO-OPED NEW FEATURES                                             
*                                                                               
* BPLA 3/26/91 ADD RD - DRAFT REBATE BILLING (RT123)                            
*                                                                               
* ROSA 2/20/91 ADD COMMENT TO S2 REQUEST                           L03          
* ROSA 2/11/91 ADD MARKET OPTION FOR 77 REQUEST                L02              
*                                                                               
* BPLA  2/7/91   ADDED REQUEST FOR P49                                          
*                                                                               
* ****  THE CHANGE BELOW NOW ONLY EXISTS IN PRREQ00F (FRENCH VERSION)           
* BPLA  NEW OPTIONS FIELD FOR B1,D1,R1 ALSO READDED FOR 52 AND EC               
*       *NOTE* FOR P52 DATE OPTION IS NOW MOVED TO NEW OPTIONS                  
*       FIELD (D=D)                                                             
*       B1=110,D1=115,R1=119,EC=118                                             
*                                                                               
* BPLA  ALLOW MEDIA * FOR BA                                                    
*                                                                               
* BPLA  7/90 FEATURES FOR 2 REQUEST CARDS                                       
*                                                                               
* ROSA  1/16/90 ADD NEW LINE TO P10                                 L01         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT NOGEN                                                            
T41200   CSECT                                                                  
         NMOD1 REQTEMPX-REQTEMP,T41200,CLEAR=YES                                
         LR    R9,RC                                                            
         USING REQTEMP,R9                    R9=A(W/S)                          
         ST    R9,ATEMP                                                         
         EJECT                                                                  
         LM    R2,R4,0(R1)                                                      
         USING TWAD,R3                       R3=A(TWA)                          
         ST    R1,APARM                                                         
         ST    R3,ASAVE                                                         
         ST    RB,ABASE                                                         
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
*******  LR    R6,R3                         STORE A(FIRST INPUT FLD)           
*******  AH    R6,0(R2)                                                         
*******  ST    R6,AFIRSTF       AFIRST NOW SET IN MAIN5                         
*                                                                               
         LR    R6,R3                         STORE A(LAST INPUT FLD)            
         AH    R6,2(R2)                                                         
         ST    R6,ALASTF                                                        
         MVC   COUNTF,4(R2)                  STORE INPUT FLD COUNT              
*                                                                               
         MVI   DDS,0                                                            
         CLI   01(R3),C'*'                                                      
         BNE   *+8                                                              
         MVI   DDS,1                         SET DDS TERMINAL                   
         MVC   AGY,14(R3)                    STORE AGY ALPHA                    
         MVC   AGYB,0(R1)                    STORE AGY BINARY                   
         MVC   USRID,10(R3)                  STORE USER ID NUMBER               
         MVI   USRIDF,0                                                         
*                                                                               
         USING COMFACSD,R4                                                      
         L     R4,16(R1)                                                        
*        ST    R4,ACOMFACS                   STORE ACOMFACS                     
         LR    R5,R4                                                            
         MVC   FACLIST,0(R4)                 STORE COMMON FACILITY LIST         
**NEW 1/19/90                                                                   
*                                                                               
         GOTO1 GETFACT,PLIST,0                                                  
         MVC   SECRET,CSECRET      SAVE ADDRESS TO SECRET                       
         DROP  R4                                                               
         ST    R5,ACOMFACS                   STORE ACOMFACS                     
*                                                                               
         L     R1,0(R1)                                                         
         MVC   TODAY(13),4(R1)                                                  
         GOTO1 DATVAL,PLIST,TODAY,TEMP                                          
         MVC   TODAY(6),TEMP                 CONVERT TO YYMMDD                  
* *****************************************************************             
*                                                                               
*      GET AND STORE OFFICER                                                    
* *****************************************************************             
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'      GET OFFICER ADDRESS                  
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   OFFICER,DMCB                STORE OFFICER ADDRESS                
* *****************************************************************             
*                                                                               
*      GET AND STORE PUBVAL                                                     
* *****************************************************************             
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AB8'      GET PUBVAL ADDRESS                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PUBVAL,DMCB                STORE PUBVAL ADDRESS                  
* *****************************************************************             
*                                                                               
*      GET AND STORE GETIDS                                                     
* *****************************************************************             
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFA'      GET GETIDS ADDRESS                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GETIDS,DMCB                STORE GETINS ADDRESS                  
*                                            RELOCATE LOCAL ROUTINES            
         LA    R6,REQTBL                                                        
         ST    R6,AREQTBL                                                       
         LA    R6,OLAY                                                          
         ST    R6,AOLAY                                                         
         LA    R6,READ                                                          
         ST    R6,AREAD                                                         
         LA    R6,INITV                                                         
         ST    R6,AINITV                                                        
         LA    R6,RJN                                                           
         ST    R6,ARJN                                                          
         LA    R6,SECBLK           SECRET BLOCK                                 
         ST    R6,ASECBLK                                                       
         LA    R6,ACCESSTB                                                      
         ST    R6,AACCESST                                                      
*                                                                               
         LA    R1,OFFBLK                                                        
         ST    R1,AOFFBLK                                                       
         LA    R1,REQOFFL                                                       
         ST    R1,AREQOFFL                                                      
         ST    RB,LOLAYNUM                   INIT LAST OLAY NUM & ADR           
         LA    R6,PRTWORK                                                       
         ST    R6,DMCB+16                                                       
         MVC   DMCB+16(1),0(R3)              SET TERMINAL NUM IN DMCB           
         EJECT                                                                  
         LR    R2,R3                         R2=A(TWA SCREEN DATA)              
         USING T412FFD,R2                                                       
*                                                                               
         LA    RF,BVRNAMEH         FIND 1ST MODIFIED FIELD                      
*                                  BELOW SERVICE REQUEST                        
         ST    RF,AFIRSTF                                                       
MAIN5    SR    R1,R1               0 = END OF SCREEN                            
         ICM   R1,1,0(RF)                                                       
         BZ    MAIN10                                                           
         TM    4(RF),X'80'         IF FIELD WAS INPUT THIS TIME THEN            
         BO    *+10                SAVE ADDRESS OF FIELD                        
         AR    RF,R1                                                            
         B     MAIN5                                                            
         ST    RF,AFIRSTF                                                       
*                                                                               
MAIN10   DS    0H                                                               
*                                                                               
* SEE IF SECURITY IS NEEDED ON PRG-IF NOT IN TABLE-NO SECURITY                  
*                                                                               
         USING ACCESSD,R4                                                       
         L     R4,AACCESST         PROGRAM/ACTION TABLE                         
         LA    R0,ACCESSEQ                                                      
MAIN40   CLC   ACCID,BVRNUM        FIND PRG ID IN TABLE                         
         BE    MAIN50                                                           
         CLC   ACCCDE,BVRNUM       FIND PRG CODE IN TABLE                       
         BE    MAIN50                                                           
         LA    R4,ACCESSLN(R4)                                                  
         BCT   R0,MAIN40                                                        
         B     MAIN70              NOT IN TABLE-NO SECURITY                     
*                                                                               
MAIN50   XC    PLIST(24),PLIST     INIT SECRET                                  
         GOTO1 SECRET,PLIST,('SECPINIT',ASECBLK)                                
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         LA    R5,ACCANUM          R5=A(ACTION)                                 
         ICM   R5,8,ACCRNUM        PASS RECORD NUMBER IN HIGH BIT               
         GOTO1 SECRET,PLIST,('SECPRACT',ASECBLK),(R5)                           
         CLI   PLIST,SECPYES                                                    
         BNE   MAIN60                                                           
*                                                                               
         TM    ACCRSTAT,ACCSECL    SHOULD WE LOOK INTO FIELD SEC                
         BNO   MAIN70                                                           
*                                                                               
*                                                                               
MAIN60   LA    RF,BVRNAMEH                                                      
         ST    RF,FADR                                                          
         MVC   FERN,=AL2(SECLOCK)                                               
         B     OERRMSG                                                          
         DROP  R4                                                               
*                                                                               
MAIN70   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              INTERFACE TO $RFP                                      *         
***********************************************************************         
*                                                                               
RFP2     DS    0H                                                               
         LA    R6,RFPINIT                                                       
         ST    R6,ARFP                                                          
         LA    RF,SECBLK                                                        
         AH    RF,=Y(L'SECBLK)                                                  
         ST    RF,AIOCTF                                                        
         AH    RF,=Y(L'IOCTF)                                                   
         ST    RF,AIORFP           SET A(RFP) & A(MINIO) BUFFERS                
         AH    RF,=Y(L'IORFP)                                                   
         ST    RF,AIOMINIO                                                      
         AH    RF,=Y(L'IOMINIO)                                                 
         ST    RF,ARFPTAB                                                       
*                                                                               
         MVI   RFPSTAT,0           DESTINATION MUST BE FILE                     
         ZIC   R1,BVRDESTH+5                                                    
         SH    R1,=H'1'                                                         
         BM    RFPX                                                             
         EXCLC R1,BVRDEST,=CL15'FILE'                                           
         BNE   RFPX                                                             
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPINIT   INITIALIZE $RFP INTERFACE                    
         GOTO1 ARFP,DMCB                                                        
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         ZIC   R1,BVROUTH+5                                                     
         SH    R1,=H'1'                                                         
         BM    RFPX                                                             
*IDFNG*  EX    R1,*+4                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QRFPWORK(0),BVROUT  VALIDATE GROUP NAME                          
         OC    QRFPWORK,SPACES                                                  
         MVI   QRFPMODE,QRFPGVAL                                                
         MVI   RFPSTAT,RFPINUSE                                                 
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BE    RFPX                                                             
         MVC   FERN,=AL2(IGROUPNM)       CHANGE                                 
         LA    R7,BVROUTH                                                       
         ST    R7,FADR                                                          
         B     OERRMSG                                                          
*                                                                               
RFPX     DS    0H                                                               
         EJECT                                                                  
CONTROL  CLI   STATUS,0                                                         
         BE    STATUS0                                                          
         CLI   STATUS,1                                                         
         BE    STATUS1                                                          
         CLI   STATUS,2                                                         
         BE    STATUS2                                                          
         CLI   STATUS,3                                                         
         BE    STATUS3                                                          
         CLI   STATUS,4                                                         
         BE    STATUS4                                                          
         DC    H'0'                                                             
         SPACE 2                                                                
STATUS0  MVI   OLAYNUM,01                    INPUT IS REQ DEFN                  
         GOTO1 AOLAY                         VALIDATE & BUILD SCREEN            
         CLC   FERN,=AL2(FF)                                                    
         BL    OERRMSG                                                          
         B     CONTROL                                                          
         SPACE 2                                                                
STATUS1  CLI   REQACTN,C'D'                                                     
         BE    STATUS1A                                                         
         XC    BVRHDR,BVRHDR                                                    
         CLI   REQACTN,C'A'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(23),=C'ENTER REQUEST AMENDMENT'                           
         B     STATUS1A                                                         
         CLI   REQACTN,C'N'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(18),=C'ENTER REQUEST DATA'                                
         B     STATUS1A                                                         
STATUS1A MVI   STATUS,2                                                         
         LA    R5,BVRFRSTH                   POSN CURSOR                        
         SR    R6,R6                                                            
         TM    1(R5),X'20'                                                      
         BZ    *+14                                                             
         IC    R6,0(R5)                                                         
         AR    R5,R6                                                            
         B     *-14                                                             
         ST    R5,FADR                                                          
         B     OHDR                                                             
         SPACE 2                                                                
STATUS2  MVC   REQNUM(18),LREQNUM            INPUT IS REQUEST DATA              
         MVC   KRT1,LKRT1                                                       
         MVC   KRT2,LKRT2                                                       
         MVC   KUB1,LKUB1                                                       
         MVC   REQREC(L'REQREC),LREQREC                                         
         LA    R6,BVRFRSTH                                                      
         C     R6,AFIRSTF                    WAS A REQ DEFN FLD INPUT           
         BL    *+12                          NO                                 
         MVI   STATUS,0                      YES BACK TO STATUS 0               
         B     CONTROL                                                          
         CLI   REQACTN,C'D'                                                     
         BE    UPDATE                                                           
         B     VALIDATE                                                         
         SPACE 2                                                                
STATUS3  B     UPDATE                        DISPLAY/UPDATE DATA                
         SPACE 2                                                                
STATUS4  XC    BVRHDR,BVRHDR                 INPUT WAS FOR MENU DISPLAY         
         MVC   BVRHDR(22),=C'REQUEST MENU DISPLAYED'                            
         B     UPDATE1                                                          
         EJECT                                                                  
VALIDATE L     R4,AREQTBL                                                       
         AH    R4,REQNDX1                    R4=A(REQ TBL FLD ENTRY)            
         BCTR  R4,0                                                             
         LA    RA,LREQMAP-3                  RA=A(REQ MAP TBL ENTRY)            
         B     VAL5                                                             
         SPACE 2                                                                
VAL1     CLI   0(R4),0                       END OF REQ TBL                     
         BE    VALX                          YES                                
         CLI   0(R4),X'01'                   USED TO BE 127                     
         BNE   VAL1A1                                                           
         LA    R4,2(R4)                      BUMP PAST COMMENT ENTRIES          
         B     VAL1                          NOW 2 LONG INSTEAD OF 1            
VAL1A1   CLI   0(R4),127                                                        
         BNE   *+12                                                             
         LA    R4,1(R4)                      BUMP PAST COMMENT ENTRIES          
         B     VAL1                          NOW 2 LONG INSTEAD OF 1            
         CLC   RNUM(2),=C'92'                SEE IF CLOSE-OUT                   
         BNE   VAL1A                                                            
         CLI   DDS,0                                                            
         BNE   VAL1A                                                            
         CLC   0(2,R4),=X'E505'              SKIPONLY FIELD                     
         BE    VAL7                          NON-DDS OMITS                      
         B     VAL1A                                                            
*                                                                               
*****    TM    2(R4),X'80'                   DDS ONLY FIELD                     
*****    BZ    VAL1A *** 2/13/91 NO LONGER USED ***** L02                       
*****    TM    REQFMT,X'04'                  YES - WAS TST=Y SPECIFIED          
******   BZ    VAL7                                                             
VAL1A    CLC   0(1,RA),0(R4)                                                    
         BE    *+6                                                              
         DC    H'0'                          DUMP IF TBLS OUT OF STEP           
         SPACE 2                                                                
         LA    R6,FLDNUMT                    FIND REQ FLD NUM IN TBL            
VAL2     CLI   0(R6),0                       R6=A(FLD NUM TBL ENTRY)            
         BNE   *+6                                                              
         DC    H'0'                          DUMP IF REQ NUM NOT IN TBL         
         CLC   0(1,R6),0(R4)                                                    
         BE    VAL3                                                             
         LA    R6,4(R6)                                                         
         B     VAL2                                                             
         SPACE 2                                                                
VAL3     MVC   OLAYNUM,1(R6)                 SET OVERLAY REQD                   
         MVC   ROUTNUM,2(R6)                 SET VALIDATION ROUT REQD           
         MVC   ROUTSUB,3(R6)                 SET SUB FLD NUM                    
         SR    R1,R1                                                            
         IC    R1,2(R4)                      SET COL NUM - 1                    
***      SLL   R1,25                                                            
***      SRL   R1,25                                                            
         BCTR  R1,0                                                             
         STH   R1,COLNUM                                                        
         ST    R5,FLDHADR                    SET INPUT FLD HDR ADR              
         GOTO1 AOLAY                         PASS CONTROL TO OVERLAY            
         TM    FIND,X'01'                                                       
         BZ    VAL3C                                                            
         CLC   FERN,=AL2(FF)                                                    
         BNE   OERRMSG                       FLD INPUT INVALID                  
         B     VAL4                          FLD INPUT VALID                    
VAL3C    TM    1(R4),X'01'                   FLD NOT INPUT                      
         BZ    OERRMSG                       AND IS NOT OPTIONAL                
         B     VAL5                                                             
VAL4     NI    FIND,X'FE'                    FIND=B'XXXXXXX0'                   
         MVC   TEMP(1),1(R4)                 TEMP=B'XXXXXXXX'                   
         NC    TEMP(1),FIND                  IS FLD FORMAT OK FOR REQ           
         BNZ   VAL5                          YES                                
         MVC   FERN,=AL2(FMTNAV)             ERROR FORMAT NOT AVAILABLE         
         B     OERRMSG                                                          
         SPACE 2                                                                
VAL5     LA    RA,3(RA)                      FIND NEXT REQ MAP ENTRY            
         MVC   HALF,1(RA)                                                       
         LH    R5,HALF                                                          
         AR    R5,R3                         R5=A(NEXT UNPROT FLD HDR)          
         CLI   0(RA),126                     CARD REQUEST FORMAT                
         BNE   VAL7                                                             
         MVC   RAGY(78),8(R5)                YES SCREEN TO REQ REC              
         B     UPDATE                                                           
         SPACE 2                                                                
VAL7     LA    R4,3(R4)                      FIND NEXT REQ TBL FLD              
         B     VAL1                                                             
         SPACE 2                                                                
VALX     CLI   0(RA),127                     END OF REQ MAP TBL                 
         BE    UPDATE                                                           
         DC    H'0'                          DUMP IF TBLS OUT OF STEP           
         SPACE 2                                                                
UPDATE   MVI   OLAYNUM,02                    PASS CONTROL TO OVERLAY            
         GOTO1 AOLAY                                                            
         CLC   FERN,=AL2(FE)                 CHK IF PASSING OWN MSG             
         BE    OHDR                          (USED FOR SOONABLE REPS)           
         CLC   FERN,=AL2(FF)                 UPDATED/DISPLAYED OK               
         BL    OERRMSG                       NO ERROR                           
         CLI   REQACTN,C'D'                                                     
         BNE   *+16                                                             
         CLI   STATUS,1                                                         
         BE    CONTROL                       ASK FOR CANCELLATIONS              
         B     UPDATE1                                                          
         XC    BVRHDR,BVRHDR                                                    
         CLI   REQACTN,C'A'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(15),=C'REQUEST AMENDED'                                   
         B     UPDATE1                                                          
         CLI   REQACTN,C'N'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(13),=C'REQUEST ADDED'                                     
         B     UPDATE1                                                          
UPDATE1  LA    R6,BVRNAMEH                                                      
         ST    R6,FADR                                                          
         MVI   STATUS,0                                                         
         MVC   PREQNUM(1),LREQNUM            SAVE LAST REQ DEFN                 
         MVC   PREQACTN(1),LREQACTN                                             
         MVC   PREQNDX1(2),LREQNDX1                                             
         B     OHDR                                                             
         EJECT                                                                  
*        SET UP SCREEN HEADER MESSAGE TO CONTAIN THE ERROR                      
*                                                                               
OERRMSG  DS    0H                                                               
         CLC   FERN,=AL2(FE)                                                    
         BE    OHDR                                                             
         XC    BVRHDR,BVRHDR                                                    
         SR    R5,R5                                                            
****                                                                            
****     IC    R5,FERN                                                          
****     L     R6,DATAMGR                                                       
*****    GOTO1 GETMSG,PLIST,((R5),BVRHDR),(04,DMCB),(0(R3),(R6))                
         SPACE 2                                                                
         ICM   R5,3,FERN                                                        
         PRINT GEN                                                              
         LA    R1,PLIST                                                         
         USING GETTXTD,R1                                                       
         MVI   GT2INDS,0          DEFAULT                                       
         MVI   GTMSYS,4           USE PRINT SYSTEM FOR MESSAGES                 
         MVI   GTMLANG,0          DEFAULT TO CONNECTED LANGUAGE                 
         DROP  R1                                                               
         GOTO1 GETTXT,PLIST,(R5),0,(C'E',DMCB),0,0                              
         PRINT NOGEN                                                            
*        OUTPUT SCREEN HEADER MESSAGE , POSITION THE CURSOR , AND               
*        RETURN TO TERMINAL                                                     
*                                                                               
OHDR     OI    BVRHDRH+6,OI1T                                                   
         MVI   BVRHDRH+7,60                                                     
         L     R6,FADR                                                          
         OI    6(R6),OI1C                                                       
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*        THIS ROUTINE PASSES CONTROL TO THE OVERLAY NUMBER SPECIFIED BY         
*        OLAYNUM.IF THE OVERLAY IS NOT IN CORE IT IS READ FROM DISK AND         
*        ITS NUM AND ADDRESS ARE SAVED.                                         
*                                                                               
OLAY     NTR1  BASE=ABASE                                                       
         CLC   OLAYNUM,LOLAYNUM              IS OVERLAY IN CORE                 
         BE    OLAY1                         YES                                
         XC    CALLOVPL(8),CALLOVPL          LOAD FROM DISK OLAYNUM             
         MVC   CALLOVPL(1),OLAYNUM                                              
         ST    R3,CALLOVPL+4                                                    
         GOTO1 CALLOV,CALLOVPL                                                  
         CLI   CALLOVPL+4,X'FF'                                                 
         BNE   *+6                                                              
         DC    H'0'                          DUMP IF CANT LOAD                  
         MVC   LOLAYNUM(4),CALLOVPL          SAVE OVERLAY NUM & ADDR            
OLAY1    GOTO1 LOLAYNUM,PHASEPL,(R9)         PASS CONTROL TO OVERLAY            
OLAYX    XIT1                                                                   
*                                                                               
*        $RFP INTERFACE                                                         
RFPINIT  NTR1  BASE=ABASE                                                       
         MVI   OLAYNUM,9                                                        
         GOTO1 AOLAY                                                            
         B     EXIT                                                             
         EJECT                                                                  
*        THIS ROUTINE READS A PRTFILE/PUBFILE RECORD INTO IOAREA AND            
*        RETURNS A RESULT IN FERN. X'FF'=OK, X'FE'=NOTFOUND, X'00'=ERR.         
*        PARAMS VIA R1                                                          
*        CL3   C'PRT'=PRTFILE, C'PUB'=PUBFILE                                   
*        CL1   C'1'=KEY1, C'2'=KEY2                                             
*                                                                               
READ     NTR   ABASE                                                            
         LR    R2,R1                                                            
         LA    R4,=C'DMRDHI'                                                    
READ1    CLC   0(3,R2),=C'PRT'               PRTDIR                             
         BNE   READ2                                                            
         LA    R5,=C'PRTDIR'                                                    
         CLI   3(R2),C'1'                    KEY1                               
         BNE   *+12                                                             
         LA    R6,KRT1                                                          
         B     READ5                                                            
         CLI   3(R2),C'2'                    KEY2                               
         BNE   *+12                                                             
         LA    R6,KRT2                                                          
         B     READ5                                                            
         DC    H'0'                                                             
READ2    CLC   0(3,R2),=C'PUB'               PUBDIR                             
         BNE   READ3                                                            
         LA    R5,=C'PUBDIR'                                                    
         CLI   3(R2),C'1'                    KEY1                               
         BNE   *+12                                                             
         LA    R6,KUB1                                                          
         B     READ5                                                            
         DC    H'0'                                                             
READ3    DC    H'0'                                                             
*                                                                               
READ5    GOTO1 DATAMGR,DMCB,(R4),(R5),(R6),PRTREC                               
         CLI   DMCB+8,0                                                         
         BNE   READERR                                                          
         CLC   0(6,R5),=C'PRTDIR'                                               
         BNE   READOK                                                           
         CLC   0(25,R6),PRTREC                                                  
         BE    READOK                                                           
READNF   MVC   FERN,=AL2(FE)                 NOT FOUND (PRTDIR)                 
         CLI   2(R6),C'C'                    CNG 01/06/88 RMED TO 2(R6)         
         BE    READNH                                                           
         CLI   2(R6),C'*'                    CNG 01/06/88 RMED TO 2(R6)         
         BNE   READX                                                            
*                                                                               
READNH   DS    0H                                                               
         CLI   3(R6),X'02'                                                      
         BE    READNH3                                                          
         CLI   3(R6),X'30'                                                      
         BNE   READNK                                                           
         LA    R4,=C'DMREAD'                                                    
         B     READNH4                                                          
*                                                                               
READNH3  LA    R4,=C'DMRDHI'                                                    
READNH4  LA    R5,=C'PRTDIR'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(25),0(R6)                                                    
         MVI   KEY+2,C'I'          FIRST TRY FOR MEDIA I                        
         GOTO1 DATAMGR,DMCB,(R4),(R5),KEY,PRTREC                                
         CLI   DMCB+8,0                                                         
         BE    READ6                                                            
         MVC   KEY(25),0(R6)                                                    
         MVI   KEY+2,C'M'          THEN MAGAZINES                               
         GOTO1 DATAMGR,DMCB,(R4),(R5),KEY,PRTREC                                
         CLI   DMCB+8,0                                                         
         BE    READ6                                                            
         MVI   KEY+2,C'N'          THEN NEWSPAPERS                              
         GOTO1 DATAMGR,DMCB,(R4),(R5),KEY,PRTREC                                
         CLI   DMCB+8,0                                                         
         BNE   READERR         IF ONLY O, S, OR T PRESENT                       
*                              I WON'T FIND - UNLIKELY TO HAPPEN                
         B     READ6                                                            
*                                                                               
READNK   XC    PRTREC(250),PRTREC                                               
         MVC   PRTREC(25),0(R6)    MOVE KEY INTO REC                            
         MVC   FERN,=AL2(FF)       SET REC FOUND                                
         B     READX                                                            
READERR  MVC   FERN,=AL2(0)                  DISK ERROR                         
         B     READX                                                            
READOK   MVC   FERN,=AL2(FF)                 FOUND                              
*                                                                               
READ6    CLC   0(6,R4),=C'GETREC'                                               
         BE    READX                                                            
         LA    R4,=C'GETREC'                                                    
         LA    R6,PRTREC+27                                                     
         CLC   0(3,R2),=C'PRT'                                                  
         BNE   *+12                                                             
         LA    R5,=C'PRTFILE'                                                   
         B     READ5                                                            
         LA    R5,=C'PUBFILE'                                                   
         B     READ5                                                            
READX    CLC   FERN,=AL2(FE)                 SET CONDITION CODE                 
*                                     LOW=DISKERR,EQ=NOT FOUND,HIGH=OK          
         XIT                                                                    
         EJECT                                                                  
*        ROUTINE TO RIGHT JUSTIFY A FLD OF LENGTH R5 AT HDR ADR R4 AND          
*        CHECK FOR NUMERIC GT 0 AND LE 999.RETURN IN TEMP                       
*        CL2   BINARY VALUE                                                     
*        CL3   RT JUSTIFIED VALUE                                               
*                                                                               
RJN      NTR   ABASE                                                            
         MVC   TEMP+2(3),=C'0000'                                               
         MVC   FERN,=AL2(FF)                                                    
         LA    R4,IFLDH                                                         
         SR    R5,R5                                                            
         IC    R5,IFLDH+5                                                       
         LR    R8,R5                                                            
         BCTR  R8,R0                                                            
         LA    R7,3                                                             
         SR    R7,R5                                                            
         LA    R7,TEMP+2(R7)                                                    
         EX    R8,*+8                        RT JUST AT TEMP+2(3)               
         B     *+10                                                             
         MVC   0(0,R7),8(R4)                                                    
         MVC   TEMP+10(3),=C'0000'           CHECK FOR NUMERIC                  
         MVZ   TEMP+10(3),TEMP+2                                                
         CLC   TEMP+10(3),=C'0000'                                              
         BE    RJN0                                                             
         MVC   FERN,=AL2(FLDNUM)                                                
         B     RJNX                                                             
RJN0     PACK  DUB,TEMP+2(3)                 CHECK GT 0                         
         CVB   R7,DUB                                                           
         CH    R7,=H'0'                                                         
         BE    RJN1                                                             
         STH   R7,TEMP                       RETURN BINARY VALUE                
         B     RJNX                                                             
RJN1     MVC   FERN,=AL2(FLDINV)             SET ERROR CODE                     
RJNX     XIT                                                                    
         EJECT                                                                  
*        INITIALISE FIND,FERN,FADR FOR FLD NUM N AT TWA HDR AT R1.              
*        MOVE FLD TO IFLD AND SET R4=A(HDR) & R5=L'DATA.                        
*        R6 IS SET TO THE ADDRESS OF THE NEXT INPUT FIELD                       
*                                                                               
INITV    NTR1  BASE=ABASE                                                       
         XC    HALF,HALF                     SAVE FLD NUM                       
         MVC   HALF+1(1),ROUTSUB                                                
         XC    IFLDH,IFLDH                                                      
         MVC   HALF2,=AL2(FF)                                                   
         MVC   IFLD,=CL32' '                                                    
         XC    NAME,NAME                                                        
         L     R4,FLDHADR                    R4=A(FLD HDR)                      
         SR    R5,R5                                                            
         IC    R5,5(R4)                                                         
         LTR   R5,R5                         NULLS OR SPACES NOT INPUT          
         BZ    INITV1                                                           
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),=CL32' '                                                 
         BNE   INITV2                                                           
*                                                                               
INITV1   SR    R5,R5                         MISSING FLD                        
         MVC   HALF2,=AL2(FLDMIS)                                               
         B     INITVX                                                           
*                                                                               
INITV2   LA    R5,1(R5)                      R5=TOTAL INPUT COUNT               
         SR    R1,R1                         R1=DELIMITER COUNT                 
         LA    R2,8(R4)                      R2=A(FLD START)                    
         LR    R3,R2                         R3=A(FLD NEXT CHR)                 
         CLI   ROUTSUB,0                     ONLY ONE FIELD                     
         BE    INITV7                        YES INHIBIT SCAN                   
*                                                                               
INITV3   CLI   0(R3),C','                    SCAN FOR DELIMITER                 
         BE    INITV5                                                           
         CLI   0(R3),C'-'                                                       
         BE    INITV5                                                           
INITV4   LA    R3,1(R3)                                                         
         BCT   R5,INITV3                                                        
*                                                                               
INITV5   LA    R1,1(R1)                      BUMP DELIMITER COUNT               
         CH    R1,HALF                                                          
         BE    INITV6                        FLD#N R2=START R3=END+1            
         LTR   R5,R5                                                            
         BZ    INITV1                                                           
         LA    R2,1(R3)                                                         
         B     INITV4                                                           
*                                                                               
INITV6   LR    R5,R3                                                            
         SR    R5,R2                         R5=FLD#N LENGTH                    
         BZ    INITV1                                                           
INITV7   STC   R5,IFLDH+5                                                       
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),0(R2)                                                    
         LA    R5,1(R5)                                                         
         OI    IFLDH,X'01'                   SET FLD INPUT                      
         CLI   IFLDH+5,3                                                        
         BNE   INITVX                                                           
         CLC   IFLD(3),=C'ALL'                                                  
         BNE   INITVX                                                           
         OI    IFLDH,X'02'                   SET FLD INPUT=C'ALL'               
*                                                                               
INITVX   LA    R6,TEMP                       SET R6=A(NEXT TWA FLD HDR)         
         TM    REQFMT,X'04'                  CHECK DDS OPTION                   
         NOP   *+12                                                             
*        IF THE ABOVE INSTRUCTION IS CHANGED TO BO THE ADDRESS OF THE           
*        NEXT INPUT FIELD IS SET TO A DUMMY LOCATION WHICH WILL NEGATE          
*        ALL FOUTS IN VALIDATION ROUTINES.                                      
         SR    R6,R6                                                            
         IC    R6,0(R4)                                                         
         AR    R6,R4                                                            
         CLI   ROUTSUB,1                     1ST OR ONLY FIELD                  
         BH    *+20                          NO                                 
         MVC   FIND,IFLDH                    YES SET FIND/FERN/FADR             
         MVC   FERN,HALF2                                                       
         ST    R4,FADR                                                          
         CLI   FIND,X'01'          LOW=MISSING,EQL=INPUT,HIGH=C'ALL'            
         XIT1  REGS=(R4,R6)                                                     
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
KEY      DS    CL32                                                             
FLDMIS   EQU   01                            MISSING INPUT FLD                  
FLDINV   EQU   02                            INVALID INPUT FLD                  
FLDNUM   EQU   03                            NON NUMERIC INPUT FLD              
FMTNAV   EQU   02                            FORMAT NOT AVAIL FOR REQ           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        THIS TABLE CONTAINS AN ENTRY FOR EACH REQUEST FIELD DEFINED            
*        EACH ENTRY HAS THE FORMAT :-                                           
*        CL1   FLD NUM (=0 END OF TBL)                                          
*        CL1   OVERLAY NUM OF PHASE CONTAINING VALIDATION CODE                  
*        CL1   ROUTINE NUM WITHIN OVERLAY PHASE                                 
*        CL1   SUB FLD NUM - 1=FIRST - DENOTES POSITION IN INPUT FLD            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FLDNUMT  DS    0CL4                                                             
*                                                                               
         DC    AL1(02,3,01,0)           CLIENT                                  
         DC    AL1(04,3,02,0)           DIVISION                                
         DC    AL1(06,3,03,0)           PRODUCT                                 
         DC    AL1(07,3,09,0)           JOB                                     
         DC    AL1(08,3,04,0)           REGION                                  
         DC    AL1(10,3,05,0)           DISTRICT                                
         DC    AL1(12,3,06,0)           ESTIMATE                                
         DC    AL1(14,3,07,0)           PUBLICATION                             
         DC    AL1(16,3,08,0)           START,END DATES                         
         DC    AL1(20,4,05,0)           BILL/PAY DATE                           
         DC    AL1(22,4,01,0)           SORT MENU                               
         DC    AL1(26,4,05,0)           BILL MODE                               
         DC    AL1(28,4,02,0)           REPORT            N                     
         DC    AL1(29,4,14,0)           FILTER                                  
         DC    AL1(30,4,05,0)           FREQUENCY                               
         DC    AL1(38,4,03,0)           MONTH OF SERVICE                        
         DC    AL1(40,4,03,0)           INVOICE DATE                            
         DC    AL1(42,4,03,0)           PERIOD THRU                             
         DC    AL1(44,4,03,0)           CURRENT MONTH                           
         DC    AL1(46,4,04,0)           REP NUMBER                              
         DC    AL1(48,4,06,0)           MANUAL AMOUNT                           
         DC    AL1(50,4,07,0)           MANUAL C/D                              
         DC    AL1(52,4,08,0)           COMMENT NUMBER                          
         DC    AL1(54,4,03,0)           CONTROL DATE                            
         DC    AL1(56,4,05,0)           PRINT OPTION                            
         DC    AL1(57,4,05,0)           TEST RUN                                
         DC    AL1(58,4,05,0)           TYPE OF ITEMS                           
         DC    AL1(59,4,05,0)           FAX CONTRACTS?                          
         DC    AL1(60,4,05,0)           ONE PUB PER PAGE?                       
         DC    AL1(61,4,05,0)           LIST PREVIOUSLY POSTED                  
         DC    AL1(62,4,05,0)           TYPE OF PUBS                            
         DC    AL1(64,4,05,0)           LIST TYPE                               
         DC    AL1(66,4,05,0)           AGENCY PUBS ONLY?                       
         DC    AL1(68,4,05,0)           AUTHORIZATION DATA                      
         DC    AL1(70,4,09,0)           CHANGE CONTROL DATE                     
         DC    AL1(72,4,05,0)           TYPE OF BUYS                            
         DC    AL1(74,4,05,0)           NEWS SUMMARY ALSO                       
         DC    AL1(75,4,05,0)           TOTALS OPTION                           
         DC    AL1(76,4,05,0)           SUMMARY ONLY                            
         DC    AL1(78,4,10,0)           CONTRACT NUMBER                         
         DC    AL1(80,4,11,0)           DAYS TILL DUE                           
         DC    AL1(82,4,12,0)           BILLING PERIOD                          
         DC    AL1(84,4,03,0)           AS OF DATE                              
         DC    AL1(86,4,05,0)           $ ON SCHEDULE                           
         DC    AL1(88,4,05,0)           REPORT TYPE                             
         DC    AL1(90,4,05,0)           DATE OPTION                             
         DC    AL1(92,4,13,0)           LIST CODE                               
         DC    AL1(93,4,05,0)           SORT CODE                               
         DC    AL1(94,4,05,0)           GRID FORMAT                             
         DC    AL1(95,4,05,0)           CHANGE OPT                              
         DC    AL1(96,4,05,0)           AD NUM OPTION                           
         DC    AL1(97,5,04,0)           OVERAGE OPTION                          
         DC    AL1(98,4,05,0)           MKTS/VENDORS                            
         DC    AL1(99,4,05,0)           DETAIL OPTION                           
         DC    AL1(100,4,05,0)          SPACE OPTION                            
         DC    AL1(101,4,05,0)          WEEKS OPTION                            
         DC    AL1(102,4,05,0)          $ OPTION                                
         DC    AL1(103,3,10,0)          ASPO - DUPONT REPORT                    
         DC    AL1(104,4,05,0)          DATA OPTION                             
         DC    AL1(105,4,05,0)          ADFILE INFO                             
         DC    AL1(106,4,05,0)          PAYING ADDR?                            
         DC    AL1(107,4,05,0)          DOUBLE SPACING                          
         DC    AL1(108,4,05,0)          SCHEDULE SORT                           
         DC    AL1(109,4,15,0)          $ COL OVERRIDE                          
         DC    AL1(110,4,03,0)          CUT-OFF DATE                            
         DC    AL1(111,4,05,0)          ESTIMATE OPTION                         
         DC    AL1(112,4,05,0)          PRDS TOGETHER                           
         DC    AL1(113,4,05,0)          PRODUCE TAPE?                           
         DC    AL1(114,4,05,0)          SUPPRESS LABELS                         
         DC    AL1(115,4,16,0)          VALIDATE USER REPORT NUMBER             
         DC    AL1(116,4,05,0)          SUPPRESS BOXES                          
         DC    AL1(117,4,05,0)          BILLED ITEMS ONLY                       
         DC    AL1(118,4,05,0)          BILLING TYPE                            
         DC    AL1(119,4,05,0)          BILLABLE ITEMS ONLY                     
         DC    AL1(120,4,05,0)          PRINT OPTION 2                          
         DC    AL1(121,4,05,0)          PRINT OPTION 2                          
         DC    AL1(122,4,05,0)          PRINT OPTION 4                          
         DC    AL1(124,4,05,0)          DATE TYPE OVERRIDE                      
         DC    AL1(125,3,08,0)          PAYABLE DATES (ST/END)                  
         DC    AL1(128,4,05,0)          LETTER OPTION                           
         DC    AL1(129,4,05,0)          REBATABLE ITEMS ONLY                    
         DC    AL1(130,4,05,0)          REBATED ITEMS ONLY                      
         DC    AL1(131,4,05,0)          PAYABLE ITEMS ONLY                      
         DC    AL1(132,4,17,0)          52/EC REQUESTED                         
         DC    AL1(133,4,05,0)          SUPPRESS INACTIVE CONTRACTS             
         DC    AL1(134,4,05,0)          SHOW TRAFFIC ADDRESS                    
         DC    AL1(135,4,05,0)          LEVEL OPTION                            
         DC    AL1(136,4,05,0)          TEST OPTION                             
         DC    AL1(137,4,05,0)          TEST OPTION                             
         DC    AL1(138,4,05,0)          AOR OPTION                              
         DC    AL1(139,4,05,0)          COMMISSION ONLY BILLS       L01         
         DC    AL1(140,5,01,0)          OPTIONS                                 
         DC    AL1(141,4,05,0)          CD OPTION                               
         DC    AL1(142,4,05,0)          MARKET OPTION              L02          
         DC    AL1(143,4,05,0)          CLT ACTIVITY FILTER                     
         DC    AL1(144,5,02,0)          BILLING TYPE                            
         DC    AL1(145,4,03,0)          DATE                                    
         DC    AL1(146,4,05,0)          DATE TYPE                               
         DC    AL1(147,4,03,0)          INTERFACE DATE                          
         DC    AL1(148,5,03,0)          BILL NUMBER(S)                          
         DC    AL1(149,4,05,0)          NET?                                    
         DC    AL1(150,4,03,0)          KILL DATE FILTER                        
         DC    AL1(151,4,05,0)          LEVEL/TEST OPTION                       
         DC    AL1(152,5,05,0)          AGENCY FILTER                           
         DC    AL1(153,4,05,0)          STATUS FILTER                           
         DC    AL1(154,4,05,0)          ADCODE RECAP?                           
         DC    AL1(155,4,05,0)          LANGUAGE FILTER                         
         DC    AL1(198,4,05,0)          LIST PREV CONVRESIONS                   
         DC    AL1(199,4,05,0)          REPLACE EXISTING INVOICES               
         DC    AL1(200,4,05,0)          CREATE Y=I2  P=PRINT                    
         DC    AL1(201,4,05,0)          (HIDDEN FIELD)                          
         DC    AL1(202,4,05,0)          POST INVOICE                            
         DC    AL1(203,4,18,0)          PUBLISHER                               
         DC    AL1(204,4,05,0)          SCHEME (ID)                             
         DC    AL1(205,4,19,0)          START INVOICE #                         
         DC    AL1(206,4,20,0)          END INVOICE #                           
         DC    AL1(207,4,05,0)          RERUN                                   
         DC    AL1(208,4,05,0)          RECORD TYPE                             
         DC    AL1(209,4,05,0)          DOWNLOAD FORMAT?                        
         DC    AL1(210,4,05,0)          PURGE OPTION                            
         DC    AL1(212,4,05,0)          CLT/PUB PURGE OPT                       
         DC    AL1(214,4,05,0)          COMMENT PURGE OPT                       
         DC    AL1(216,5,06,0)          FOREIGN BANK CODE                       
         DC    AL1(218,4,05,0)          PRD SUMMARY FMT                         
         DC    AL1(220,5,07,0)          COPY TO MEDIA                           
         DC    AL1(221,4,05,0)          COPY USER-DEF?                          
         DC    AL1(222,4,05,0)          COPY PRD BILL-FORMULA?                  
         DC    AL1(223,4,05,0)          COPY DIVISIONS?                         
         DC    AL1(224,5,08,0)          SCHEME CODE (FOR GRP RECORDS)           
         DC    AL1(225,5,03,0)          REVERSED NUMBER                         
         DC    AL1(226,4,06,0)          AMOUNT                                  
         DC    AL1(227,4,06,0)          DETAILS AMOUNT                          
         DC    AL1(228,4,05,0)          PLANNED COSTS?                          
         DC    AL1(229,4,05,0)          SKIPONLY?  (P92)                        
         DC    AL1(230,4,03,0)          PO# END DATE                            
*                                                                               
FLDNUMTX DC    XL1'00'                                                          
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
ACCESSTB DS    0C                                                               
*   PLN/CONT REPORTS                                                            
         DC    C'12',C'CON',XL1'11',XL1'08',XL1'00'                             
         DC    C'14',C'CLS',XL1'12',XL1'08',XL1'00'                             
         DC    C'16',C'ARC',XL1'13',XL1'08',XL1'00'                             
         DC    C'18',C'CAN',XL1'14',XL1'08',XL1'00'                             
         DC    C'19',C'UTL',XL1'15',XL1'08',XL1'00'                             
*   SCHEDULE REPORTS                                                            
         DC    C'52',C'EST',XL1'16',XL1'01',XL1'00'                             
         DC    C'60',C'MSR',XL1'17',XL1'01',XL1'00'                             
         DC    C'EC',C'ECR',XL1'18',XL1'01',XL1'00'                             
         DC    C'S2',C'SPS',XL1'19',XL1'01',XL1'00'                             
         DC    C'54',C'ESR',XL1'1A',XL1'01',XL1'00'                             
****                                                                            
*   TRAFFIC REPORTS                                                             
         DC    C'72',C'IOR',XL1'21',XL1'02',XL1'00'                             
         DC    C'73',C'IOP',XL1'22',XL1'02',XL1'00'                             
         DC    C'75',C'ADL',XL1'23',XL1'02',XL1'00'                             
         DC    C'77',C'TRA',XL1'24',XL1'02',XL1'00'                             
         DC    C'79',C'SHP',XL1'25',XL1'02',XL1'00'                             
*   BILLING                                                                     
         DC    C'10',C'ILT',XL1'31',XL1'03',XL1'00'                             
         DC    C'B1',C'NBL',XL1'32',XL1'03',XL1'00'                             
         DC    C'D1',C'DBL',XL1'33',XL1'03',XL1'00'                             
         DC    C'B9',C'RET',XL1'34',XL1'03',XL1'00'                             
         DC    C'07',C'07 ',XL1'35',XL1'03',XL1'00'                             
         DC    C'SB',C'SBL',XL1'36',XL1'03',XL1'00'    SOON BILLING             
         DC    C'VL',C'VL ',XL1'8A',XL1'03',XL1'00'                             
*   PAYING                                                                      
         DC    C'20',C'INV',XL1'41',XL1'04',XL1'00'                             
         DC    C'27',C'PAY',XL1'42',XL1'04',XL1'00'                             
         DC    C'28',C'CVS',XL1'43',XL1'04',XL1'00'                             
         DC    C'36',C'VEN',XL1'44',XL1'04',XL1'00'                             
         DC    C'37',C'VPL',XL1'45',XL1'04',XL1'00'                             
*   INVMATCH                                                                    
         DC    C'NV',C'NVR',XL1'46',XL1'09',XL1'00'                             
         DC    C'TS',C'TSR',XL1'47',XL1'09',XL1'00'                             
*   GENERAL MAINTENANCE                                                         
                                                                                
         DC    C'41',C'CPE',XL1'51',XL1'05',XL1'00'                             
         DC    C'42',C'DRD',XL1'52',XL1'05',XL1'00'                             
         DC    C'43',C'REP',XL1'53',XL1'05',XL1'00'                             
         DC    C'45',C'LST',XL1'54',XL1'05',XL1'00'                             
         DC    C'46',C'PIS',XL1'55',XL1'05',XL1'00'                             
         DC    C'47',C'COM',XL1'56',XL1'05',XL1'00'                             
         DC    C'48',C'PUB',XL1'57',XL1'05',XL1'00'                             
         DC    C'74',C'RPL',XL1'58',XL1'05',XL1'00'                             
         DC    C'CC',C'CC ',XL1'59',XL1'05',XL1'00'                             
         DC    C'IC',C'IC ',XL1'5B',XL1'05',XL1'00'                             
         DC    C'NT',C'NT ',XL1'5C',XL1'05',XL1'00'                             
*   UTILITY                                                                     
         DC    C'CM',C'PCM',XL1'5A',XL1'10',XL1'00'                             
         DC    C'01',C'PGR',XL1'5D',XL1'10',XL1'00'                             
*   FINANCIAL                                                                   
         DC    C'49',C'AGY',XL1'61',XL1'06',XL1'00'                             
         DC    C'A8',C'BCR',XL1'62',XL1'06',XL1'00'                             
         DC    C'L2',C'L2 ',XL1'63',XL1'06',XL1'00'                             
*   SPECIAL                                                                     
         DC    C'PM',C'PMI',XL1'71',XL1'07',XL1'00'                             
         DC    C'R1',C'RBL',XL1'72',XL1'07',XL1'00'                             
         DC    C'RD',C'RBD',XL1'73',XL1'07',XL1'00'                             
         DC    C'RA',C'RBA',XL1'74',XL1'07',XL1'00'                             
         DC    C'AC',C'ARC',XL1'75',XL1'07',XL1'00'                             
         DC    C'AR',C'ACR',XL1'76',XL1'07',XL1'00'                             
         DC    C'AU',C'AUR',XL1'77',XL1'07',XL1'00'                             
         DC    C'EB',C'EBT',XL1'78',XL1'07',XL1'00'                             
         DC    C'CL',C'CL ',XL1'79',XL1'07',XL1'00'                             
         DC    C'GT',C'GT ',XL1'7A',XL1'07',XL1'00'                             
         DC    C'SN',C'SN ',XL1'7B',XL1'07',XL1'00'                             
         DC    C'CI',C'CI ',XL1'7C',XL1'07',XL1'00'                             
         DC    C'LT',C'LT ',XL1'7D',XL1'07',XL1'00'                             
         DC    C'SE',C'SE ',XL1'7E',XL1'07',XL1'00'                             
         DC    C'PH',C'PH ',XL1'7F',XL1'07',XL1'00'                             
         DC    C'GM',C'GM-',XL1'80',XL1'07',XL1'00'                             
         DC    C'EX',C'EX ',XL1'81',XL1'07',XL1'00'                             
         DC    C'LO',C'LOR',XL1'82',XL1'07',XL1'00'                             
         DC    C'TD',C'TD ',XL1'83',XL1'07',XL1'00'                             
         DC    C'JW',C'JW ',XL1'84',XL1'07',XL1'00'                             
         DC    C'WB',C'WB ',XL1'85',XL1'07',XL1'00'                             
         DC    C'IN',C'IN ',XL1'86',XL1'07',XL1'00'                             
         DC    C'CH',C'CH ',XL1'87',XL1'07',XL1'00'                             
         DC    C'PZ',C'PZ ',XL1'88',XL1'07',XL1'00'                             
         DC    C'AI',C'AI ',XL1'89',XL1'07',XL1'00'                             
         DC    C'VK',C'VK ',XL1'90',XL1'07',XL1'00'                             
ACCESSEQ EQU   (*-ACCESSTB)/ACCESSLN                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        THIS TABLE CONTAINS A VARIABLE LENGTH ENTRY FOR EACH REQUEST           
*        CL1   ENTRY LENGTH - ZERO=END-OF-TABLE                                 
*        CL2   REQUEST NUM / REQUEST SUB NUM                                    
*        CL1   FORMAT BITS    X'80' = NEWSPAPERS                                
*                             X'40' = MAGAZINES,SUPPLEMENTS,TRADE               
*                             X'20' = OUTDOOR                                   
*                             X'10' = ALL MEDIA *                               
*                             X'08' = CARD REQUIRED                             
*                             X'04' = DDS REQUIRED                              
*                             X'02' = MEDIA REQUIRED                            
*                             X'01' = REQUESTOR REQUIRED                        
*        CL22  REQUEST ID CL3 & REQUEST NAME                                    
*        CL2   N/D                                                              
*        0CLN  ENTRY FOR EACH SCREEN FOR REQUEST                                
*        CL1   ENTRY LENGTH                                                     
*        CL1   MEDIA BITS - B'NMO*0000'                                         
*        CL3   FIELD NUM / FIELD FORMAT / REQUEST CARD COLUMN                   
*              X'80' BIT OF CARD COLUMN = DDS ONLY                              
*                                                                               
*              FIELD NUM EQ 000 (CL1) END-OF-FIELD LIST                         
*                        LE 127 (CL3) DATA FIELD                                
*                        GE 127 (CL1) COMMENT ODD=SAME LINE EVEN=NEW            
*                                                                               
*              FIELD FORMAT (USED AGAINST "FIND" IN EDIT ROUTINES)              
*                           IF X'01' IS ON, FLD IS NOT REQUIRED                 
*                                                                               
*        XL1   ZERO FOR END OF ENTRY                                            
*        CL2   ALPHA REQUEST ID                                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REQTBL   DS    0CL1                                                             
*                                                                               
RT00     DC    AL1(RT00X-*+3,00,0)                                              
         DC    B'11100011',CL22'???-UNKNOWN'                                    
         DC    X'0000'                                                          
         DC    AL1(RT00X-*+1),B'11100000'                                       
         DC    X'0207060181'          O CLI=ALL/XXX                             
         DC    X'0407090181'          O DIV=ALL/NNN                             
         DC    X'08070F0181'          O REG=ALL/NNN                             
         DC    X'0A07120181'          O DST=ALL/NNN                             
         DC    X'06070C0181'          O PRO=ALL/XXX                             
         DC    X'0CFF150181'          O EST=NNN,NNN-NNN,ALL,ALL+F,NO+F          
         DC    X'0E0D1B0181'          O PUB=NUM/NUM,Z,E/ALL                     
         DC    X'100926'              O STRD,ENDD=YYMMDD                        
RT00X    DC    X'00',C'00'                                                      
*                                                                               
*                                                                               
*                                                                               
RT01     DC    AL1(RT01X-*+3,01,0)                                              
         DC    B'11100011',CL22'PGR-PRTPAK REC PURGES'                          
         DC    X'0000'                                                          
         DC    AL1(RT01X-*+1),B'11110000'                                       
         DC    X'D0063E014D014E'        RECORD TYPE=HRJCPLDEF# (1 CHAR)         
         DC    X'D2063F014F0150'        PURGE OPTION=PAG       (1 CHAR)         
         DC    X'0205060181'            CLI=XXX                                 
         DC    X'06070C0181'            PRD=ALL/XXX                             
         DC    X'0E0D1B0181'            PUB=NUM/NUM,Z,E                         
         DC    X'2E051D0181'            REP=NNNN                                
         DC    X'34051D0181'            COMMENT=XXXXXX                          
         DC    X'07051D0181'            JOB=XXXXXX                              
         DC    X'5C051D0181'            LIST CODE=XXX                           
         DC    X'E0051D0181'            SCHEME CODE=X                           
         DC    X'540935'                PO# END DATE                            
RT01X    DC    X'00',C'01'                                                      
*                                                                               
*                                                                               
*                                                                               
RT02     DC    AL1(RT02X-*+3,02,0)                                              
         DC    B'11100111',CL22'PGR-PRTPAK REC PURGES'                          
         DC    X'0000'                                                          
         DC    AL1(RT02X-*+1),B'11110000'                                       
         DC    X'D0063E014D014E'        RECORD TYPE=HRJCPL     (1 CHAR)         
         DC    X'D2063F014F0150'        PURGE OPTION=PAG            "           
         DC    X'3807410151'            PRINT OPTION=NY             "           
         DC    X'D407420153'            CLT/PUB PURGE OPT=Y         "           
         DC    X'D6074301550156'        COMMENT PURGE OPT=CJPEBRUL  "           
         DC    X'0205060181'            CLI=XXX                                 
         DC    X'06070C0181'            PRD=ALL/XXX                             
         DC    X'0E0D1B0181'            PUB=NUM/NUM,Z,E                         
         DC    X'2E051D0181'            REP=NNNN                                
         DC    X'34051D0181'            COMMENT=XXXXXX                          
         DC    X'07051D0181'            JOB=XXXXXX                              
         DC    X'5C051D0181'            LIST CODE=XXX                           
         DC    X'E0051D0181'            SCHEME CODE=X                           
         DC    X'3906440157'            TEST RUN=NY            (1 CHAR)         
RT02X    DC    X'00',C'02'                                                      
*                                                                               
*        RT04 AND RT06 (OLD STYLE BILLING) REMOVED 5/16/89                      
*                                                                               
*                                                                               
         SPACE 2                                                                
RT07     DC    AL1(RT07X-*+3,07,0)                                              
         DC    B'11110111',CL22'07 -UNBILLING '                                 
         DC    X'0000'                                                          
         DC    AL1(RT07X-*+1),B'11110000'                                       
         DC    X'029C060181'            CLI=XXX/*N,XXX-XXX,&N (GROUP)           
         DC    X'06060C0181'            PRD=XXX/ALL                             
         DC    X'0CFE150181'            EST=NNN/ALL/NNN-NNN,NO+F/ALL+F          
         DC    X'76043E015B'            B=NORMAL,M=MANUAL,R=REVERSAL            
         DC    X'280835'                INVOICE DATE=YYMMDD                     
         DC    X'94051C'                BILL NUMBER                             
         DC    X'E10520015F'            REVERSED NUMBER                         
         DC    X'E20465015D'            AMOUNT           COL 21 CARD 2          
         DC    X'E30571015D'            DETAILS AMOUNT   COL 33 CARD 2          
         DC    X'8C0551'              O OPTIONS                                 
*                                                                               
RT07X    DC    X'00',C'07'                                                      
*                                                                               
RT10     DC    AL1(RT10X-*+3,10,0)                                              
*****    DC    B'11100011',CL22'ILT-INVOICE LIST/TAPE'                          
         DC    B'11110011',CL22'ILT-INVOICE LIST/TAPE'                          
         DC    X'0000'                                                          
*****    DC    AL1(RT10X-*+1),B'11100000'                                       
         DC    AL1(RT10X-*+1),B'11110000'                                       
         DC    X'02CE060181'            CLI=ALL/XXX/$N/*N/&N                    
         DC    X'06070C0181'            PRD=ALL/XXX                             
         DC    X'0CB7150181'            EST=NNN,ALL,NNN+NON-SPEC PRD            
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'CD0535'              O START INVOICE#=NNNN                     
         DC    X'CE0539'              O END INVOICE#=NNNN                       
         DC    X'710541'              O PRODUCE TAPE=Y/N                        
         DC    X'8B0543011B011C'      O BILL OPTION                             
         DC    X'7605420169'          O BILLING TYPE (S=SOON ONLY)              
*                                                                   L01         
RT10X    DC    X'00',C'10'                                                      
         SPACE 2                                                                
*                                                                               
RT11     DC    AL1(RT11X-*+3,11,0)                                              
         DC    B'11100011',CL22'GM-GM TAPE INTERFACE'                           
         DC    X'0000'                                                          
         DC    AL1(RT11X-*+1),B'11100000'                                       
         DC    X'02CE060181'            CLI=ALL/XXX/$N/*N/&N                    
         DC    X'06070C0181'            PRD=ALL/XXX                             
         DC    X'0CB7150181'            EST=NNN,ALL,NNN+NON-SPEC PRD            
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'71053E'              O PRODUCE TAPE=Y/N                        
         DC    X'8C0551'              O OPTIONS                                 
*                                                                   L01         
RT11X    DC    X'00',C'GM'                                                      
         SPACE 2                                                                
RT12     DC    AL1(RT12X-*+3,12,0)                                              
         DC    B'11100011',CL22'CON-CONTRACTS'                                  
         DC    X'0000'                                                          
         DC    AL1(RT12X-*+1),B'11100000'                                       
         DC    X'02AE060181'            CLI=ALL/XXX/XXX-XXX/*N/&N               
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0E6D1B0181'          O PUB=NNN/NUM,Z,E/NUM,ALL,ALL             
*                                       NUM,Z,ALL                               
         DC    X'4E0D15'              O CONTRACT=NNN                            
         DC    X'100926'              O STRT,END=YYMMDD                         
         DC    X'16053301B3'          O SORT MENU=05,07                         
*NOP*    DC    X'38053E01910194'      O PRT OPT                                 
         DC    X'38053E01FF'          O PRT OPT   TO 1 LINE (SCRN FULL)         
         DC    X'56053F0199'          O OPT2=N                                  
         DC    X'580540019B'          O OPT3=R                                  
         DC    X'5E0541'              O GRID FORMAT=Y,N                         
         DC    X'5F054201A7'            CHANGE OPT=F/C                          
         DC    X'600543013B'            AD NUM OPT=A/B/1/2/                     
* NO-OP  DC    X'6C054401CF'          O SCHED SORT=P,D                          
         DC    X'46093A'              O CONTROL DATE=YYMMDD                     
         DC    X'340565'              O COMMENT=XXXXXX                          
         DC    X'4B054401ED'          O TOTALS OPTION=I,L                       
         DC    X'3B0535'              O FAX CONTRACTS?  (COL 53)                
         DC    X'8C0518'              O OPTIONS (EST=NNN - TO COL 24)           
* NOTE - MAXIMUM NUMBER OF FIELDS HAVE BEEN REACHED                             
RT12X    DC    X'00',C'12'                                                      
         SPACE 2                                                                
RT14     DC    AL1(RT14X-*+3,14,0)                                              
         DC    B'11100011',CL22'CLS-CONTRACT LISTING'                           
         DC    X'0000'                                                          
         DC    AL1(RT14X-*+1),B'11100000'                                       
         DC    X'029E060181'            CLI=ALL/XXX/*N/&N/XXX-XXX               
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0E4D1B0181'          O PUB=NUM,NUM,Z,E/NUM,ALL                 
         DC    X'CB071E0181'          O PUBLISHER=XXXX/ALL                      
         DC    X'4E0515'              O CONTRACT=NNN                            
         DC    X'100926'              O STRT,ENDD=YYMMDD                        
         DC    X'16053301B301960198'  O SORT MENU=05,07,08,09                   
         DC    X'0405090181'          O DIV=NNN                                 
         DC    X'08070F0181'          O REG=ALL/NNN                             
         DC    X'0A07120181'          O DST=ALL/NNN                             
         DC    X'46093A'              O CONTROL DATE=YYMMDD                     
         DC    X'5A053E0149'          O DATE OPTION=Y,N                         
         DC    X'38053F01C7'          O PRINT OPT=Y                             
         DC    X'5F054101C9'          O CHG OPT=Y                               
**NEW14                                                                         
         DC    X'8705400113'          O LEVEL OPT=H,L,B                         
*NOP*    DC    X'870540014B'          O LEVEL OPT=H,L,B,O,A                     
**NEW14                                                                         
RT14X    DC    X'00',C'14'                                                      
         SPACE 2                                                                
RT16     DC    AL1(RT16X-*+3,16,0)                                              
         DC    B'11100011',CL22'ARC-AUTOMATIC RATE CHG'                         
         DC    X'0000'                                                          
         DC    AL1(RT16X-*+1),B'11100000'                                       
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'0E0C1B0181'            PUB=NUM,NUM,Z,E                         
         DC    X'4E0415'                CON=NNN                                 
         DC    X'38053F01C7'          O PRINT OPT=Y                             
         DC    X'88044001150116'        TEST OPTION=N,Y,H,L                     
RT16X    DC    X'00',C'16'                                                      
         SPACE 2                                                                
RT18     DC    AL1(RT18X-*+3,18,0)                                              
         DC    B'11100011',CL22'CAN-CONTRACT ANALYSIS'                          
         DC    X'0000'                                                          
         DC    AL1(RT18X-*+1),B'11100000'                                       
         DC    X'028E060181'            CLI=ALL/XXX/*N/&N                       
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0E4D1B0181'          O PUB=NUM,NUM,Z,E/NUM,ALL                 
         DC    X'CB071E0181'          O PUBLISHER=XXXX/ALL                      
         DC    X'4E0515'              O CONTRACT=NNN                            
         DC    X'100926'              O STRT,ENDD=YYMMDD                        
         DC    X'16053301B3'          O SORT MENU=05,07                         
         DC    X'46093A'              O CONTROL DATE=YYMMDD                     
         DC    X'5A053E0149'          O DATE OPTION=Y,N                         
         DC    X'38053F01C7'          O PRINT OPT=Y                             
         DC    X'5F054101C9'          O CHG OPT=Y                               
         DC    X'850542'              O SUPPRESS INACTIVE CONTRACTS?            
         DC    X'8705400113'          O LEVEL OPT=H,L,B                         
**NEW18                                                                         
         DC    X'8C0D8E'              O OPTIONS                                 
**NEW18                                                                         
**NOTE                 QOPT6 SET TO Y IF REQUESTED AS 18T                       
**                     TO INCLUDE TEST ESTIMATES                                
RT18X    DC    X'00',C'18'                                                      
         SPACE 2                                                                
RT19     DC    AL1(RT19X-*+3,19,0)                                              
         DC    B'11100011',CL22'UTL-UTILIZATION RPT'                            
         DC    X'0000'                                                          
         DC    AL1(RT19X-*+1),B'11100000'                                       
         DC    X'028E060181'            CLI=ALL/XXX/*N/&N                       
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0E4D1B0181'          O PUB=NUM,NUM,Z,E/NUM,ALL                 
         DC    X'CB071E0181'          O PUBLISHER=XXXX/ALL                      
         DC    X'4E0515'              O CONTRACT=NNN                            
         DC    X'100926'              O STRT,ENDD=YYMMDD                        
         DC    X'16053301B3'          O SORT MENU=05,07                         
         DC    X'460935'              O CONTROL DATE=YYMMDD                     
*****    DC    X'5A053E01B901BA'      O DATE OPTION=Y,N                         
**NEW19                                                                         
         DC    X'5A053E0149'          O DATE OPTION=Y,N                         
         DC    X'38053F01C7'          O PRINT OPT=Y                             
         DC    X'5F054101C9'          O CHG OPT=Y                               
         DC    X'850542'              O SUPPRESS INACTIVE CONTRACTS?            
         DC    X'8705400113'          O LEVEL OPT=H,L,B                         
         DC    X'6C053B01CF'          O SCHEDULE SORT P,D                       
         DC    X'89053C'              O SUPPRESS NET COLUMN                     
*****    DC    X'63053D0119'          O DETAIL OPTION (Y=SUPPRESS)              
**NEW19                                                                         
         DC    X'63058E0119'          O DETAIL OPTION (Y=SUPPRESS)              
         DC    X'8C0D8E'              O OPTIONS                                 
**NEW19                                                                         
**NOTE                 DETAIL OPTION CHANGED TO QOPT8 FROM QOPT1-1              
**                     TO ALLOW FOR CONTINUATION CARD                           
**NOTE                 QOPT6 SET TO Y IF REQUESTED AS 19T                       
**                     TO INCLUDE TEST BUYS                                     
RT19X    DC    X'00',C'19'                                                      
         SPACE 2                                                                
RT20     DC    AL1(RT20X-*+3,20,0)                                              
         DC    B'11100011',CL22'INV-INVOICE CHECK LIST'                         
         DC    X'0000'                                                          
         DC    AL1(RT20X-*+1),B'11100000'                                       
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06060C0181'            PRD=ALL/XXX                             
         DC    X'0E4E1B0181'            PUB=ALL/NUM/NUM,Z,E/NUM,ALL             
         DC    X'100826'                STRD,ENDD=YYMMDD                        
RT20X    DC    X'00',C'20'                                                      
*                                                                               
RT27     DC    AL1(RT27X-*+3,27,0)                                              
         DC    B'11100011',CL22'PAY-PAYER''S LIST'                              
         DC    X'0000'                                                          
         DC    AL1(RT27X-*+1),B'11100000'                                       
         DC    X'02CE060181'            CLI=ALL/XXX/*N/&N (GROUP)/$N            
         DC    X'060F0C0181'          O PRD=ALL/XXX                             
         DC    X'0CEF150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'0E4D1B0181'          O PUB=ALL/NUM/NUM,Z,E/NUM,ALL             
         DC    X'100826'                STRD,ENDD=YYMMDD                        
         DC    X'140532018F'          O BILL/PAY DATE                           
         DC    X'16053301D101D2'      O SORT=06/07/08/09                        
         DC    X'3A053E018D'          O TYPE OF ITEMS                           
         DC    X'540935'              O ASOFD=YYMMDD                            
         DC    X'3C053F'              O ONE PUB PER PASE                        
         DC    X'3E05400189'          O TYPE OF PUBS                            
         DC    X'38054101A301A4'        PRINT OPT=1/2/A/B                       
         DC    X'68054201C1'          O DATA OPT=Y                              
         DC    X'6A0543'              O PAY ADDR=Y/N                            
RT27X    DC    X'00',C'27'                                                      
         SPACE 2                                                                
RT28     DC    AL1(RT28X-*+3,28,0)                                              
         DC    B'11100011',CL22'CVS-CLT/VENDOR SUMMARY'                         
         DC    X'0000'                                                          
         DC    AL1(RT28X-*+1),B'11100000'                                       
         DC    X'02CE060181'            CLI=ALL/XXX/*N/&N (GROUP)/$N            
         DC    X'060F0C0181'          O PRD=ALL/XXX                             
         DC    X'0CEF150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'0E4D1B0181'          O PUB=ALL/NUM/NUM,Z,E/NUM,ALL             
         DC    X'100826'                STRD,END=YYMMDD                         
         DC    X'140532018F'          O BILL PAY DATE                           
         DC    X'16053301D101D2'      O SORT=06/07/08/09                        
         DC    X'3A053E018D'          O TYPE OF ITEMS                           
         DC    X'540935'              O ASOFD=YYMMDD                            
         DC    X'3C053F'              O ONE PUB PER PAGE                        
         DC    X'3E05400189'          O TYPE OF PUBS                            
RT28X    DC    X'00',C'28'                                                      
         SPACE 2                                                                
RT31     DC    AL1(RT31X-*+3,31,0)                                              
         DC    B'11100011',CL22'WRI-WRITE CHECKS'                               
         DC    X'0000'                                                          
         DC    AL1(RT31X-*+1),B'11100000'                                       
         DC    X'36022C0197'            CONTROL DATE =ALL                       
RT31X    DC    X'00',C'31'                                                      
         SPACE 2                                                                
RT36     DC    AL1(RT36X-*+3,36,0)                                              
         DC    B'11100011',CL22'VEN-VENDOR SUMMARY'                             
         DC    X'0000'                                                          
         DC    AL1(RT36X-*+1),B'11100000'                                       
         DC    X'02CE060181'            CLT=ALL/XXX/*N/&N (GROUP)/$N            
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0C87150181'          O EST=NNN,ALL,NO+F                        
         DC    X'0E4D1B0181'          O PUB=ALL/NUM/NUM,Z,E/NUM,ALL             
         DC    X'100826'                STRT,ENDD=YYMMDD                        
         DC    X'140532018F'          O BILL/PAY DATE=P,B                       
         DC    X'160533019F0186'      O SORT=06/07                              
         DC    X'3A053E018D'          O TYPE OF ITEMS=P,U                       
         DC    X'54090F'              O ASOFD=YYMMDD                            
         DC    X'3E05400189'          O TYPE OF PUBS=C,N                        
         DC    X'4C054101D7'          O SUMMARY OPT=C,P                         
         DC    X'63054301E3'          O DETAIL OPT=Y                            
         DC    X'3C054201E1'          O ONE PUB PER PAGE=Y,N                    
RT36X    DC    X'00',C'36'                                                      
         SPACE 2                                                                
RT37     DC    AL1(RT37X-*+3,37,0)                                              
         DC    B'11100011',CL22'VPL-VENDOR PAY LIST'                            
         DC    X'0000'                                                          
         DC    AL1(RT37X-*+1),B'11100000'                                       
         DC    X'02CE060181'            CLT=ALL/XXX/*N/&N/$N                    
         DC    X'060F0C0181'          O PRD=ALL/XXX                             
         DC    X'0CEF150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'0E4D1B0181'          O PUB=ALL/NUM/NUM,Z,E/NUM,ALL             
         DC    X'100826'                STRT,ENDD=YYMMDD                        
         DC    X'140532018F'          O BILL/PAY DATE=P,B                       
         DC    X'16053301D101D2'      O SORT=06/07/08/09                        
         DC    X'3A053E018D'          O TYPE OF ITEMS=P,U                       
         DC    X'54090F'              O ASOFD=YYMMDD                            
         DC    X'3C053F01E1'          O ONE PUB PER PAGE=Y,N                    
         DC    X'3E05400189'          O TYPE OF PUBS=C,N                        
         DC    X'38054101A301A4'        PRINT OPT=1/2/A/B                       
         DC    X'68054201C1'          O DATA OPT=Y                              
         DC    X'6A05430117'          O PAY ADDR=Y/N(USE WITH 08,09)            
RT37X    DC    X'00',C'37'                                                      
         SPACE 2                                                                
RT41     DC    AL1(RT41X-*+3,41,0)                                              
         DC    B'11100011',CL22'CPE-CLT-PRD-EST LIST'                           
         DC    X'0000'                                                          
         DC    AL1(RT41X-*+1),B'11100000'                                       
         DC    X'02CE060181'            CLI=ALL/XXX/*N/$N/&N                    
         DC    X'060F0C0181'          O PRD=ALL/XXX                             
         DC    X'0C13150181'          O EST=ALL,ALL+F                           
         DC    X'38053E0103'          O Y=SUPRESS PROFILES                      
         DC    X'78053F0105'          O Y=SUPRESS BILLING FORMULAS              
         DC    X'79054001070108'      O Y=ACTIVE,I=INACTIVE (CLT/PRD)           
         DC    X'7A05410109'          O Y=SUPRESS ADDRESSES                     
RT41X    DC    X'00',C'41'                                                      
         SPACE 2                                                                
RT42     DC    AL1(RT42X-*+3,42,0)                                              
         DC    B'11100011',CL22'DRD-DIV-REG-DST LIST'                           
         DC    X'0000'                                                          
         DC    AL1(RT42X-*+1),B'11100000'                                       
         DC    X'028E060181'            CLI=ALL/XXX/*N/&N (GROUP)               
         DC    X'0406090181'            DIV=ALL/NNN                             
         DC    X'08070F0181'          O REG=ALL/NNN                             
         DC    X'0A07120181'          O DST=ALL/NNN                             
RT42X    DC    X'00',C'42'                                                      
         SPACE 2                                                                
RT43     DC    AL1(RT43X-*+3,43,0)                                              
         DC    B'11100011',CL22'REP-REP LISTING'                                
         DC    X'0000'                                                          
         DC    AL1(RT43X-*+1),B'11100000'                                       
         DC    X'2E06350181'            REP=ALL/NNNN                            
         DC    X'40053E01430144'      O TYPE OF LIST                            
         DC    X'D8053F0181'          O FOREIGN BANK CODE                       
RT43X    DC    X'00',C'43'                                                      
         SPACE 2                                                                
RT45     DC    AL1(RT45X-*+3,45,0)                                              
         DC    B'11100011',CL22'LST-LIST LISTING'                               
         DC    X'0000'                                                          
         DC    AL1(RT45X-*+1),B'11100000'                                       
         DC    X'020E060181'            CLI=ALL/XXX                             
         DC    X'5C07350181'          O LIST CODE=ALL/XXX                       
RT45X    DC    X'00',C'45'                                                      
         SPACE 2                                                                
RT46     DC    AL1(RT46X-*+3,46,0)                                              
         DC    B'11100011',CL22'PIS-PUB INFO SHEET'                             
         DC    X'0000'                                                          
         DC    AL1(RT46X-*+1),B'11100000'                                       
*NOTREQD DC    X'0E4E1B0181'            PUB=ALL/NUM/NUM,Z,E/NUM,ALL             
         DC    X'0E4F1B0181'            PUB=ALL/NUM/NUM,Z,E/NUM,ALL             
         DC    X'CB051E0181'          O PUBLISHER=XXXX                          
         DC    X'1605330183'          O SORT=NN                                 
         DC    X'9B05420145'          O LANGUAGE FILTER                         
RT46X    DC    X'00',C'46'                                                      
         SPACE 2                                                                
RT47     DC    AL1(RT47X-*+3,47,0)                                              
         DC    B'11100011',CL22'COM-COMMENT LISTING'                            
         DC    X'0000'                                                          
         DC    AL1(RT47X-*+1),B'11100000'                                       
         DC    X'020A060197'            CLI=ALL,*N                              
         DC    X'1D0535'                FILTER=1-6X                             
RT47X    DC    X'00',C'47'                                                      
         SPACE 2                                                                
RT48     DC    AL1(RT48X-*+3,48,0)                                              
         DC    B'11100011',CL22'PUB-PUB LISTING'                                
         DC    X'0000'                                                          
         DC    AL1(RT48X-*+1),B'11100000'                                       
         DC    X'160433018B'            SORT=NN                                 
         DC    X'40053E01870188018A018C018E019A' TYPE OF LIST                   
*NPNG*   DC    X'40053E01870188018A018C018E'      TYPE OF LIST                  
* NO PAY CONTROLS AND NO GROUP ASSIGNMENTS                                      
*                                     COMMENTS 135,136,138,140,142,154          
*NPNG*                                COMMENTS 135,136,138,140,142              
*****    DC    X'0207060181'          O CLI=ALL/XXX (ONLY FOR 'D' LIST          
         DC    X'020F060181'          O CLI=ALL/XXX/*N (*N ONLY 1-4)            
         DC    X'0407090181'          O DIV=ALL/XXX (ONLY FOR 'D' LIST          
         DC    X'5C051E0181'          O LIST CODE=XXX                           
         DC    X'CB051E0181'          O PUBLISHER=XXXX                          
         DC    X'6B0540'              O DBL SPACING=Y                           
         DC    X'36052601F5'          O FILTER ON EFFECTIVE DATE                
         DC    X'8F05410127'          O CLT ACTIVITY FILTER                     
         DC    X'960D350133'          O KILL DATE FILTER  (COL53)               
         DC    X'9B05420145'          O LANGUAGE FILTER                         
*NOP*    DC    X'CC07440147'          O SCHEME (ID)                             
         DC    X'8C0D44'              O OPTIONS                                 
* NOTE - MAXIMUM NUMBER OF FIELDS HAVE BEEN REACHED                             
RT48X    DC    X'00',C'48'                                                      
         SPACE 2                                                                
RT49     DC    AL1(RT49X-*+3,49,0)                                              
         DC    B'11100011',CL22'AGY-AGY SUMMARY REPORT'                         
         DC    X'0000'                                                          
         DC    AL1(RT49X-*+1),B'11100000'                                       
         DC    X'024E060181'            CLI=XXX,ALL,*N,$*                       
         DC    X'100526012D'          O STR/END=MMM/YY-MMM/YY                   
*                                       IF OMITTED REPORT USES                  
*                                       THIS MONTH AND REPORTS                  
*                                       12 MONTHS BACK AND 11 MONTHS            
*                                       FORWARD                                 
         DC    X'88053C011F'          O TEST OPTION=T                           
         DC    X'5A053E0121'          O DATE OPTION=P,S,B                       
         DC    X'8D05410123'          O CD OPTION=C                             
         DC    X'95053A'              O NET?                                    
         DC    AL1(228,05,59,01,101)  O P=PLANNED COSTS VERSION                 
RT49X    DC    X'00',C'49'                                                      
         SPACE 2                                                                
RT52     DC    AL1(RT52X-*+3,52,0)                                              
         DC    B'11110011',CL22'EST-ESTIMATES'                                  
         DC    X'0000'                                                          
         DC    AL1(RT52X-*+1),B'11110000'                                       
         DC    X'0294060181'            CLI=XXX,XXX-XXX,&N (GROUP)              
         DC    X'0407090181'          O DIV=ALL/NNN                             
         DC    X'061F0C0181'            PRD=ALL/ZZZ/*                           
         DC    X'08070F0181'          O REG=ALL/NNN                             
         DC    X'0A07120181'          O DST=ALL/NNN                             
         DC    X'0CFF150181'          O EST=NNN,NNN-NNN,ALL,ALL+F,NO+F          
         DC    X'0E5D1B0181'          O PUB=NUM/NUM,Z,E/JNNNNNN/NUM,ALL         
         DC    X'16053301B3'          O SORT MENU=05,07                         
         DC    X'103826'                STRD,ENDD=ES/YYMMDD                     
         DC    X'92053201C3'          O DATE TYPE                               
         DC    X'460D3A'              O CONTROL DATE=YYMMDD                     
*                                       X'04' =BILL DATE                        
         DC    X'340565'              O COMMENT=XXXXXX                          
         DC    X'4A053F01C5'            SUMMARY MTH OPT=B,P                     
         DC    X'4C054001B7'          O SUMMARY ONLY                            
***52    DC    X'5A0541019D'          O DATE OPT=D                              
****     NOTE = DATE OPTION MOVED TO NEW OPTIONS FIELD                          
****     (D=D)                                                                  
         DC    X'5F054201B5'          O CHG OPT=C                               
         DC    X'6D054301D3'          O $ COL ORIDE=XX                          
         DC    X'8C0D51'              O OPTIONS                                 
*        FOR NEW OPTIONS FIELD                                                  
*        TO INSTALL YOU MUST REMOVE DATE OPTION (IT WILL BE D=D IN              
*        NEW OPTIONS FIELD BECAUSE THERE ARE NO MORE FIELDS AVAILABLE           
RT52X    DC    X'00',C'52'                                                      
         SPACE 2                                                                
RT54     DC    AL1(RT54X-*+3,54,0)                                              
         DC    B'11100011',CL22'ESR-EST SUMMARY REPORT'                         
         DC    X'0000'                                                          
         DC    AL1(RT54X-*+1),B'11100000'                                       
         DC    X'020E060181'            CLI=ALL/XXX/*N                          
         DC    X'060E0C0181'            PRD=ALL/XXX                             
         DC    X'0C7E150181'            EST=NNN,ALL,ALL+F,NNN-NNN,              
*                                       NNN + PRD=ALL,NNN-NNN + PRD=ALL         
         DC    X'101826'                STRD,ENDD=ES/YYMMDD                     
         DC    X'920532018F'          O DATE TYPE                               
         DC    X'66053E01EB'          O $ OPT=G,N                               
RT54X    DC    X'00',C'54'                                                      
         SPACE 2                                                                
RT60     DC    AL1(RT60X-*+3,60,0)                                              
         DC    B'11100011',CL22'MSR-MEDIA SCHEDULE RPT'                         
         DC    X'0000'                                                          
         DC    AL1(RT60X-*+1),B'11100000'                                       
         DC    X'0214060181'            CLI=XXX,XXX-XXX,                        
         DC    X'0407090181'          O DIV=ALL,NNN                             
         DC    X'06170C0181'          O PRD=ALL,XXX,*                           
         DC    X'08070F0181'          O REG=ALL,NNN                             
         DC    X'0A07120181'          O DST=ALL,NNN                             
         DC    X'0CFF150181'          O EST=NNN,NNN-NNN,ALL,ALL+F,NO+F          
         DC    X'101826'                STRD,ENDD=ES/YYMMDD                     
         DC    X'92053201CD'          O DATE TYPE=B,P,C,S                       
         DC    X'16053301B3'          O SORT MENU=05,07                         
         DC    X'63043E01AB'            DETAIL OPT=P,M,D,R,E,B                  
         DC    X'64053F01AD'          O SPACE OPT=S,C                           
         DC    X'65054001AF'          O WEEKS OPT=W,D                           
         DC    X'66054101B1'          O $ OPT=S                                 
         DC    X'68054201BD01BE'        DATA OPT=P,J,X,A,B                      
*****          QOPT7 USED FOR INDICATING TEST INSERTS (SEE 02 PHASE)            
RT60X    DC    X'00',C'60'                                                      
         SPACE 2                                                                
RT66     DC    AL1(RT66X-*+3,66,0)                                              
         DC    B'11100011',CL22'AFR-AUDIT+FORECAST RPT'                         
         DC    X'0000'                                                          
         DC    AL1(RT66X-*+1),B'11100000'                                       
         DC    X'0206060181'            CLI=ALL/XXX                             
         DC    X'06170C0181'          O PRO=ALL/XXX/*                           
         DC    X'100826'                STRD,ENDD=YYMMDD                        
         DC    X'58044301BB'            REPORT TYPE=A,F                         
         DC    X'5A053F0149'          O DATE OPT=Y,N                            
         DC    X'5D044101BF01C0'        SORT CODE=S,D,1,2,3,A,B,C               
         DC    X'678D1C0181'          O PUB=NUM,NUM,Z,E                         
*                                       OR ASPO=ALL,XXXX...                     
         DC    X'69054401A301A4'      O ADFILE INFO=1,2,A,B                     
         DC    X'3C053E'              O ONE PUB PER PAGE=Y                      
         DC    X'4C054001D5'          O SUMMARY ONLY=S                          
RT66X    DC    X'00',C'66'                                                      
       SPACE 2                                                                  
RT68     DC    AL1(RT68X-*+3,68,0)                                              
         DC    B'00100011',CL22'OVR-OUTDOOR VENDOR RPT'                         
         DC    X'0000'                                                          
         DC    AL1(RT68X-*+1),B'00100000'                                       
         DC    X'0E0C1B0181'            PUB=NUM,NUM,Z,E                         
RT68X    DC    X'00',C'68'                                                      
         SPACE 2                                                                
RT72     DC    AL1(RT72X-*+3,72,0)                                              
         DC    B'11100011',CL22'IOR-INS ORDER RUN'                              
         DC    X'0000'                                                          
         DC    AL1(RT72X-*+1),B'11100000'                                       
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06040C0181'            PRO=XXX                                 
         DC    X'0C05150181'          O EST=NNN                                 
         DC    X'07050F'              O JOB=XXXXX                               
         DC    X'0E4D1B0181'          O PUB=NUM,NUM,Z,E/NUM,ALL                 
         DC    X'100826'                SRTD,ENDD=YYMMDD                        
         DC    X'360935'              O CONTROL DATE =YYMMDD                    
         DC    X'38053F01CB'          O PRINT OPT=Y                             
RT72X    DC    X'00',C'72'                                                      
         SPACE 2                                                                
RT73     DC    AL1(RT73X-*+3,73,0)                                              
         DC    B'11100011',CL22'IOP-INS ORD PRF RUN'                            
         DC    X'0000'                                                          
         DC    AL1(RT73X-*+1),B'11100000'                                       
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06060C0181'            PRO=ALL/XXX                             
         DC    X'07060F'                JOB=ALL/XXXXX                           
         DC    X'0E4F1B0181'          O PUB=ALL,NUM,NUM,Z,E/NUM,ALL             
         DC    X'100926'              O STRD,ENDD=YYMMDD                        
RT73X    DC    X'00',C'73'                                                      
         SPACE 2                                                                
RT74     DC    AL1(RT74X-*+3,74,0)                                              
         DC    B'00010011',CL22'RPL-RPT PROFILE LIST'                           
         DC    X'0000'                                                          
         DC    AL1(RT74X-*+1),B'00010000'                                       
         DC    X'1C063E'                RPT=ALL,XX                              
         DC    X'8C0D51'              O OPTIONS                                 
RT74X    DC    X'00',C'74'                                                      
         SPACE 2                                                                
RT75     DC    AL1(RT75X-*+3,75,0)                                              
         DC    B'11100011',CL22'ADL-AD REC LISTING'                             
         DC    X'0000'                                                          
         DC    AL1(RT75X-*+1),B'11100000'                                       
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'060E0C0181'            PRO=ALL/XXX                             
         DC    X'07060F'                JOB=ALL/XXXXX                           
         DC    X'100926'              O STRD,ENDD=YYMMDD                        
RT75X    DC    X'00',C'75'                                                      
*                                                                               
RT77     DC    AL1(RT77X-*+3,77,0)                                              
         DC    B'11100011',CL22'TRA-TRAFFIC LIST'                               
         DC    X'0000'                                                          
         DC    AL1(RT77X-*+1),B'11100000'                                       
         DC    X'02CF060181'          O CLI=ALL/XXX/*N/&N/$N                    
         DC    X'060F0C0181'          O PRO=ALL/XXX                             
         DC    X'0CFF150181'          O EST=NNN,NNN-NNN,ALL,NNN+NSP             
*                                       NNN-NNN+NSP,(ALL,XXX),(NO,XXX)          
         DC    X'07070F'              O JOB=ALL/XXXXX                           
         DC    X'0E4F1B0181'          O PUB=ALL/NUM/NUM,Z,E/NUM,ALL             
         DC    X'101826'                STRD,ENDD=YYMMDD,ES                     
         DC    X'92053201110190'      O DATE TYPE=D/B/P/C/I/M/S                 
         DC    X'38053F01A1'          O PRINT OPT=S/$                           
         DC    X'48053E0195'          O TYPE OF BUYS=A/O/U                      
         DC    X'5D054001A5'          O SORT CODE=A                             
         DC    X'69054101A3'          O ADFILE INFO= N,1,2                      
         DC    X'6B0542'              O DBL SPACING= N,Y                        
         DC    X'860543012F'          O SHOW TRAFFIC ADDR= N,Y,S                
         DC    X'340565'              O COMMENT=XXXXXX                          
         DC    X'8E058E0125'          O MARKET SORT                             
         DC    X'8C0D51'              O OPTIONS                                 
*****          QOPT7 USED FOR INDICATING TEST INSERTS (SEE 02 PHASE)            
RT77X    DC    X'00',C'77'                                                      
         SPACE 2                                                                
RT79     DC    AL1(RT79X-*+3,79,0)                                              
         DC    B'11100011',CL22'SHP-SHIPPING LIST'                              
         DC    X'0000'                                                          
         DC    AL1(RT79X-*+1),B'11100000'                                       
         DC    X'020E060181'            CLI=ALL/XXX/*XX                         
         DC    X'060E0C0181'            PRD=ALL/XXX                             
         DC    X'07060F'                JOB=ALL/NNNNNN                          
         DC    X'100826'                STRD,ENDD=YYMMDD                        
         DC    X'360935'              O CONTROL DATE=YYMMDD                     
         DC    X'5D054001A9'          O SORT CODE=A                             
         DC    X'610D3E0131'          O OVERAGE PCT. (Y=10 PCT)                 
         DC    X'620543'              O MKTS/VENDS=Y,N                          
         DC    X'72043F'                SUPPRESS LABELS                         
RT79X    DC    X'00',C'79'                                                      
*                                                                               
RT81     DC    AL1(RT81X-*+3,81,0)                                              
         DC    B'11100011',CL22'MCR-MANAGMNT CNTRL RPT'                         
         DC    X'0000'                                                          
         DC    AL1(RT81X-*+1),B'11100000'                                       
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'0407090181'            DIV=NNN,ALL                             
         DC    X'06070C0181'            PRD=XXX,ALL                             
         DC    X'0CBF150181'            EST=NNN,NNN-NNN,ALL,ALL+F,NO+F          
         DC    X'100826'                STRD,ENDD=YYMMDD                        
         DC    X'07070F'                AD CODE=XXXX                            
         DC    X'38053E01E9'            D=DUPLICATES ONLY(COMMENT)              
RT81X    DC    X'00',C'81'                                                      
         SPACE 2                                                                
RT91     DC    AL1(RT91X-*+3,91,0)                                              
         DC    B'11100011',CL22'BCR-BILLING/CLRNCE RPT'                         
         DC    X'0000'                                                          
         DC    AL1(RT91X-*+1),B'11100000'                                       
         DC    X'020C060181'            CLI=XXX,*N                              
         DC    X'060E0C0181'            PRO=ALL,XXX                             
         DC    X'2C1435'                CUR MTH=YYMM OR NO                      
         DC    X'6E090F'              O CUT-OFF DATE=YYMMDD                     
         DC    X'6F053E01D9'          O EST OPTN=1,2,3                          
         DC    X'70053F'              O PRD TOGETHER=Y,N                        
         DC    X'68054101DB'          O DATA OPTN=M,P                           
         DC    X'38054201DD'          O PRINT OPT=N,Y                           
         DC    X'5D054301DF'          O SORT CODE=5,7                           
         DC    X'DA0540'              O PRD SUMMARY FMT=N,Y                     
RT91X    DC    X'00',C'A8'                                                      
         SPACE 2                                                                
RT92     DC    AL1(RT92X-*+3,92,0)                                              
         DC    B'11100011',CL22'CLO-CLOSEOUT'                                   
         DC    X'0000'                                                          
         DC    AL1(RT92X-*+1),B'11100000'                                       
         DC    X'02CE060181'            CLI=ALL/XXX/$N/*N/&N                    
******** DC    X'0204060181'            CLI=XXX                                 
         DC    X'06060C0181'            PRO=ALL/XXX                             
         DC    X'0CEF150181'            EST=NNN,NNN-NNN,NO+F,ALL                
         DC    X'36082C'                CONTROL DATE (IN END DATE)              
         DC    X'E50541'                SKIPONLY?   (DDS ONLY)                  
******   DC    X'100826'                STRD,ENDD=YYMMDD                        
RT92X    DC    X'00',C'92'                                                      
         SPACE 2                                                                
RT94     DC    AL1(RT94X-*+3,94,0)                                              
         DC    B'11100011',CL22'PCM-CLIENT/PRD COPY'                            
         DC    X'0000'                                                          
         DC    AL1(RT94X-*+1),B'11100000'                                       
         DC    X'0206060181'            CLI=ALL/XXX                             
         DC    X'06070C0181'          O PRO=ALL/XXX                             
         DC    X'DC043E0181'            COPY TO MEDIA                           
         DC    X'DD04400157'            COPY USER-DEF=Y,N                       
         DC    X'DE04410157'            COPY PRD BILLFORM=Y,N                   
         DC    X'DF04420157'            COPY DIVISIONS=Y,N                      
         DC    X'3904440157'            TEST RUN=Y,N                            
RT94X    DC    X'00',C'CM'                                                      
         SPACE 2                                                                
RT98     DC    AL1(RT98X-*+3,98,0)                                              
         DC    B'11110111',CL22'98 -STATISTICS REPORT'                          
         DC    X'0000'                                                          
         DC    AL1(RT98X-*+1),B'11110000'                                       
         DC    X'024E060181'            CLI=XXX,ALL,*N,$*                       
         DC    X'1008260167'            STRD,ENDD=YYMMDD                        
         DC    X'D10544'                DOWNLOAD FORMAT?                        
         DC    X'8C0551'              O OPTIONS                                 
RT98X    DC    X'00',C'98'                                                      
         SPACE 2                                                                
**100    DC    AL1(RT100X-*+3,100,0)                                            
***      DC    B'11110011',CL22'BTF-BILLING TRANSFER'                           
***      DC    X'0000'                                                          
**       DC    AL1(RT100X-*+1),B'11110000'                                      
***      DC    X'02CE060181'            CLI=ALL/XXX/$N/*N/&N                    
***      DC    X'06060C0181'            PRD=ALL/XXX                             
**       DC    X'100826'                STRD,ENDD=YYMMDD                        
**       DC    X'39053E'                TEST RUN                                
**       DC    X'3B053F'                LIST UNPOSTED                           
**       DC    X'3D0540'                LIST PREV POSTED                        
**100X   DC    X'00',C'BA'                                                      
**       SPACE 2                                                                
RT102    DC    AL1(RT102X-*+3,102,0)                                            
         DC    B'11100011',CL22'INX-INSERTION EXTRACT'                          
         DC    X'0000'                                                          
         DC    AL1(RT102X-*+1),B'11100000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06060C0181'            PRD=ALL/XXX                             
         DC    X'0CFF150181'          O EST=NNN,NNN-NNN,ALL,ALL+F,NO+F          
         DC    X'0E0F1B0181'          O PUB=ALL/NUM/NUM,Z,E                     
         DC    X'101826'                STRD,ENDD=YYMMDD/ES                     
RT102X   DC    X'00',C'IX'                                                      
         SPACE 2                                                                
RT104    DC    AL1(RT104X-*+3,104,0)                                            
         DC    B'11100011',CL22'SXT-STANDARD EXTRACT'                           
         DC    X'0000'                                                          
         DC    AL1(RT104X-*+1),B'11100000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'0407090181'          O DIV=ALL,NNN                             
         DC    X'06060C0181'            PRD=ALL/XXX                             
         DC    X'08070F0181'          O REG=ALL,NNN                             
         DC    X'0A07120181'          O DST=ALL,NNN                             
         DC    X'0CFE150181'            EST=NNN,NNN-NNN,ALL,ALL+F,NO+F          
         DC    X'101826'                STRD,ENDD=YYMMDD/ES                     
         DC    X'5A053E01E5'          O DATE OPTION=B                           
RT104X   DC    X'00',C'XT'                                                      
         SPACE 2                                                                
RT105    DC    AL1(RT105X-*+3,105,0)                                            
         DC    B'11100011',CL22'EBT-EDI BILLING TRANS'                          
         DC    X'0000'                                                          
         DC    AL1(RT105X-*+1),B'11100000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'100826'                STRD,ENDD=YYMMDD                        
         DC    X'CD0535'              O START INVOICE#=NNNN                     
         DC    X'CE0539'              O END INVOICE#=NNNN                       
         DC    X'39043F'                TEST RUN=Y,N                            
         DC    X'CF0540011D'          O RERUN=R                                 
RT105X   DC    X'00',C'EB'                                                      
         SPACE 2                                                                
RT106    DC    AL1(RT106X-*+3,106,0)                                            
         DC    B'11110011',CL22'CL -YNR COLGATE EXTRACT'                        
         DC    X'0000'                                                          
         DC    AL1(RT106X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'100826'                STRD,ENDD=YYMMDD                        
         DC    X'3904420181'            TEST RUN=Y,N                            
*****    DC    X'CF0540011D'          O RERUN=R                                 
RT106X   DC    X'00',C'CL'                                                      
         SPACE 2                                                                
RT107    DC    AL1(RT107X-*+3,107,0)                                            
         DC    B'11100011',CL22'EX -XML BILLING TRANS'                          
         DC    X'0000'                                                          
         DC    AL1(RT107X-*+1),B'11100000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'100826'                STRD,ENDD=YYMMDD                        
         DC    X'CD0535'              O START INVOICE#=NNNN                     
         DC    X'CE0539'              O END INVOICE#=NNNN                       
         DC    X'39043F'                TEST RUN=Y,N                            
         DC    X'CF0540011D'          O RERUN=R                                 
RT107X   DC    X'00',C'EX'                                                      
         SPACE 2                                                                
RT108    DC    AL1(RT108X-*+3,108,0)                                            
         DC    B'11100011',CL22'IPX-INTERPUBLIC XTRACT'                         
         DC    X'0000'                                                          
         DC    AL1(RT108X-*+1),B'11100000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06060C0181'            PRD=ALL/XXX                             
         DC    X'0CFE150181'            EST=NNN,NNN-NNN,ALL,ALL+F,NO+F          
         DC    X'101826'                STRD,ENDD=YYMMDD/ES                     
RT108X   DC    X'00',C'X2'                                                      
         SPACE 2                                                                
RT109    DC    AL1(RT109X-*+3,109,0)                                            
         DC    B'11110011',CL22'USR-USER REPORT'                                
         DC    X'0000'                                                          
         DC    AL1(RT109X-*+1),B'11110000'                                      
         DC    X'029C060181'            CLI=XXX/*N/XXX-XXX/&N                   
         DC    X'06070C0181'          O PRD=XXX/ALL                             
         DC    X'0CAF150181'          O EST=NNN/ALL/NNN-NNN,NO+F                
         DC    X'0E4F1B0181'          O PUB=ALL/NUM,ALL,ALL/NNNNNNNN   X        
                                        NNNNNNNN,XX,X                           
         DC    X'0407090181'          O DIV=ALL,NNN                             
         DC    X'08070F0181'          O REG=ALL,NNN                             
         DC    X'0A07120181'          O DST=ALL,NNN                             
         DC    X'070739'              O JOB=ALL/XXXXXX                          
         DC    X'10182601EF'            STRT,END=YYMMDD,ES                      
         DC    X'920532010F0192'      O DATE TYPE=B/P/C/S/M/I/A                 
         DC    X'734035'                USER REP NUM=XXXX                       
         DC    X'74053F'              O SUPPRESS BOXES=Y/N                      
         DC    X'750540'              O BILLED ITEMS ONLY=N/Y                   
         DC    X'770541'              O BILLABLE ITEMS ONL=N/Y                  
         DC    X'830542'              O PAYABLE ITEMS ONLY = N/Y                
         DC    X'34051F010D'          O COMMENT=XXXXXX                          
*                                     SC=XXXXXX IN COL 28 (RPUB+1)              
*                                     COMVAL USES COLNUM 31                     
*                                     CAN'T BE USED WITH PUB OR RD=             
*****          QOPT7 USED FOR INDICATING TEST INSERTS (SEE 02 PHASE)            
RT109X   DC    X'00',C'L1'                                                      
         SPACE 2                                                                
RT110    DC    AL1(RT110X-*+3,110,0)                                            
         DC    B'11110011',CL22'NBL-NEW BILLING'                                
         DC    X'0000'                                                          
         DC    AL1(RT110X-*+1),B'11110000'                                      
         DC    X'029C060181'            CLI=XXX/*N,XXX-XXX,&N (GROUP)           
         DC    X'0407090181'          O DIV=ALL,NNN                             
         DC    X'08070F0181'          O REG=ALL,NNN                             
         DC    X'0A07120181'          O DST=ALL,NNN                             
         DC    X'060E0C0181'            PRD=XXX/ALL/PRD GROUP                   
         DC    X'0CFE150181'            EST=NNN/ALL/NNN-NNN,NO+F/ALL+F          
         DC    X'0E5D1B0181'          O PUB=ALL/NUM,ALL/NNNNNNNN                
*                                       NNNNNNNN, ALSO JNNNNNNN                 
*                                       PUB,ALL                                 
         DC    X'76043F01F1'            BILLING TYPE=4,5,6,7                    
         DC    X'520C26'                BILL PERIOD=YYMM/YYMMDD,YYMMDD          
         DC    X'280935'              O INVOICE DATE=YYMMDD                     
         DC    X'50053B'              O DAYS DUE=NN                             
         DC    X'307F6B'              O MANUAL AMT FEE=,PCT=,R=,ONLY            
*                                       PREV, NO PREV,PREV,OLD,NON-             
*                                       RETAIL, NNNNNNNN.NN                     
*                                       COL 27 CARD 2                           
         DC    X'32053E'              O MANUAL C/D                              
         DC    X'3A053E0189'          O TYPE OF ITEMS - QOPT1                   
         DC    X'3E05400189'          O TYPE OF PUBS - QOPT3                    
         DC    X'840500'              O 52/EC REQUESTED                         
         DC    X'8C0551'              O OPTIONS                                 
RT110X   DC    X'00',C'B1'                                                      
*                                                                               
RT112    DC    AL1(RT112X-*+3,112,0)                                            
         DC    B'11100011',CL22'PMI-PHILIP MORRIS INTF'                         
         DC    X'0000'                                                          
         DC    AL1(RT112X-*+1),B'11100000'                                      
         DC    X'0204060181'            CLI=XXX (ADVERTISER)                    
         DC    X'360826'                CONTROL DATE=MMMDD/YY                   
         DC    X'390543'                TEST RUN=Y                              
RT112X   DC    X'00',C'PM'                                                      
*                                                                               
RT113    DC    AL1(RT113X-*+3,113,0)                                            
         DC    B'11100011',CL22'SPS-SCHED SPREADSHEET'                          
         DC    X'0000'                                                          
         DC    AL1(RT113X-*+1),B'11100000'                                      
         DC    X'029C060181'            CLI=XXX,XXX-XXX,*NN/&N (GROUP)          
         DC    X'0407090181'          O DIV=ALL,NNN                             
         DC    X'061F0C0181'          O PRD=ALL,XXX,*                           
         DC    X'08070F0181'          O REG=ALL,NNN                             
         DC    X'0A07120181'          O DST=ALL,NNN                             
         DC    X'0CFF150181'          O EST=NNN,NNN-NNN,ALL,ALL+F,NO+F          
         DC    X'101826'                STRD,ENDD=ES/YYMMDD                     
         DC    X'360935'                CONTROL DATE=MMMDD/YY                   
         DC    X'92053201CD'          O DATE TYPE=B,P,C,S                       
         DC    X'16053301B3'          O SORT MENU=05,07                         
         DC    X'65054001AF'          O WEEKS OPT=W,D                           
         DC    X'66054101F301F2'      O $ OPT G=GROSS,N=NET,C=CASH MISC         
*                                       1=GROSS-CD,2=NET,T=COST                 
         DC    X'9A0542'              O ADCODE RECAP?                           
         DC    X'340565'              O STANDARD COMMENT                        
*****          QOPT7 USED FOR INDICATING TEST INSERTS (SEE 02 PHASE)            
RT113X   DC    X'00',C'S2'                                                      
*                                                                               
RT114    DC    AL1(RT114X-*+3,114,0)                                            
         DC    B'11100011',CL22'RET-RETAIL REPORT'                              
         DC    X'0000'                                                          
         DC    AL1(RT114X-*+1),B'11100000'                                      
         DC    X'0214060181'            CLI=XXX,XXX-XXX                         
         DC    X'0407090181'          O DIV=ALL,NNN                             
         DC    X'06160C0181'            PRD=ALL,XXX,*                           
         DC    X'08070F0181'          O REG=ALL,NNN                             
         DC    X'0A07120181'          O DST=ALL,NNN                             
         DC    X'0C0E150181'            EST=NNN,NNN-NNN,ALL                     
         DC    X'100826'                STRD,ENDD/YYMMDD                        
RT114X   DC    X'00',C'B9'                                                      
         SPACE 2                                                                
RT115    DC    AL1(RT115X-*+3,115,0)                                            
         DC    B'11110011',CL22'D1-DRAFT BILLING'                               
         DC    X'0000'                                                          
         DC    AL1(RT115X-*+1),B'11110000'                                      
         DC    X'029C060181'            CLI=XXX/*N,XXX-XXX,&N (GROUP)           
         DC    X'0407090181'          O DIV=ALL,NNN                             
         DC    X'08070F0181'          O REG=ALL,NNN                             
         DC    X'0A07120181'          O DST=ALL,NNN                             
         DC    X'060E0C0181'            PRD=XXX/ALL/PRD GROUP                   
         DC    X'0CFE150181'            EST=NNN/ALL/NNN-NNN,NO+F/ALL+F          
         DC    X'0E5D1B0181'          O PUB=ALL/NUM,ALL/NNNNNNNN                
*                                       NNNNNNNN, ALSO JNNNNNNN                 
*                                       PUB,ALL                                 
         DC    X'76043F01F1'            BILLING TYPE=4,5,6,7                    
         DC    X'520C26'                BILL PERIOD=YYMM/YYMMDD,YYMMDD          
         DC    X'280935'              O INVOICE DATE=YYMMDD                     
         DC    X'50053B'              O DAYS DUE=NN                             
         DC    X'307F6B'              O MANUAL AMT FEE=,PCT=,R=,ONLY            
*                                       PREV, NO PREV,PREV,OLD,NON-             
*                                       RETAIL, NNNNNNNN.NN                     
*                                       COL 27 CARD 2                           
         DC    X'32053E'              O MANUAL C/D                              
         DC    X'3A053E0189'          O TYPE OF ITEMS  - QOPT1                  
         DC    X'3E05400189'          O TYPE OF PUBS    - QOPT3                 
         DC    X'8C0551'              O OPTIONS                                 
RT115X   DC    X'00',C'D1'                                                      
         SPACE 2                                                                
RT116    DC    AL1(RT116X-*+3,116,0)                                            
         DC    B'11110011',CL22'E1 -ESTIMATE BILLING'                           
         DC    X'0000'                                                          
         DC    AL1(RT116X-*+1),B'11110000'                                      
         DC    X'021C060181'            CLI=XXX/*N,XXX-XXX                      
         DC    X'0407090181'          O DIV=ALL,NNN                             
         DC    X'08070F0181'          O REG=ALL,NNN                             
         DC    X'0A07120181'          O DST=ALL,NNN                             
         DC    X'06060C0181'            PRD=XXX/ALL                             
         DC    X'0CBE150181'            EST=NNN/ALL/NNN-NNN,NO+F/ALL+F          
         DC    X'0E5D1B0181'          O PUB=ALL/NUM,ALL/NNNNNNNN                
*                                       NNNNNNNN, ALSO JNNNNNNN                 
*                                       PUB,ALL                                 
         DC    X'76043F01F1'            BILLING TYPE=4,5,6,7                    
         DC    X'520C26'                BILL PERIOD=YYMM/YYMMDD,YYMMDD          
         DC    X'280935'              O INVOICE DATE=YYMMDD                     
         DC    X'50053B'              O DAYS DUE=NN                             
         DC    X'3A05400189'          O TYPE OF ITEMS                           
         DC    X'3E053E0189'          O TYPE OF PUBS                            
RT116X   DC    X'00',C'E1'                                                      
*                                                                               
RT117    DC    AL1(RT117X-*+3,117,0)                                            
         DC    B'11110011',CL22'L2 -ESTIMATE SUMMARY'                           
         DC    X'0000'                                                          
         DC    AL1(RT117X-*+1),B'11110000'                                      
         DC    X'028E060181'            CLI=ALL/XXX/*N/&N                       
         DC    X'060E0C0181'            PRD=ALL/XXX                             
         DC    X'0C7E150181'            EST=NNN,ALL,ALL+F,NNN-NNN,              
*                                       NNN + PRD=ALL,NNN-NNN + PRD=ALL         
         DC    X'101826'                STRD,ENDD=ES/YYMMDD                     
         DC    X'140532018F'          O BILL/PAY DAT                            
         DC    X'66053E01EB'          O $ OPT=G,N                               
RT117X   DC    X'00',C'L2'                                                      
*                                                                               
RT118    DC    AL1(RT118X-*+3,118,0)                                            
         DC    B'11110011',CL22'ECR-ESTIMATE CHG RPT'                           
         DC    X'0000'                                                          
         DC    AL1(RT118X-*+1),B'11110000'                                      
         DC    X'0294060181'            CLI=XXX,XXX-XXX, &N (GROUP)             
         DC    X'0407090181'          O DIV=ALL/NNN                             
         DC    X'061F0C0181'            PRD=ALL/ZZZ/*                           
         DC    X'08070F0181'          O REG=ALL/NNN                             
         DC    X'0A07120181'          O DST=ALL/NNN                             
         DC    X'0CFF150181'          O EST=NNN,NNN-NNN,ALL,ALL+F,NO+F          
         DC    X'0E5D1B0181'          O PUB=NUM/NUM,Z,E/JNNNNNN/NUM,ALL         
         DC    X'16053301B3'          O SORT MENU=05,07                         
         DC    X'103826'                STRD,ENDD=ES/YYMMDD/ES+ NO EST          
         DC    X'92053201C3'          O DATE TYPE                               
         DC    X'460C3A'                CONTROL DATE=YYMMDD                     
*                                        X'04' = BILL DATE                      
         DC    X'340565'              O COMMENT=XXXXXX                          
         DC    X'4A053F01C5'            SUMMARY MTH OPT=B,P                     
         DC    X'4C054001B7'          O SUMMARY ONLY                            
         DC    X'5A0541019D'          O DATE OPT=D                              
         DC    X'5F0542010B'          O CHG OPT = Y,T,$,B                       
         DC    X'8C0D51'              O OPTIONS                                 
*        FOR NEW OPTIONS FIELD                                                  
RT118X   DC    X'00',C'EC'                                                      
         SPACE 2                                                                
RT119    DC    AL1(RT119X-*+3,119,0)                                            
         DC    B'11110011',CL22'RBL-REBATE BILLING'                             
         DC    X'0000'                                                          
         DC    AL1(RT119X-*+1),B'11110000'                                      
         DC    X'021C060181'            CLI=XXX/*N,XXX-XXX                      
         DC    X'0407090181'          O DIV=ALL,NNN                             
         DC    X'08070F0181'          O REG=ALL,NNN                             
         DC    X'0A07120181'          O DST=ALL,NNN                             
         DC    X'06060C0181'            PRD=XXX/ALL                             
         DC    X'0CFE150181'            EST=NNN/ALL/NNN-NNN,NO+F/ALL+F          
         DC    X'0E5D1B0181'          O PUB=ALL/NUM,ALL/NNNNNNNN                
*                                       NNNNNNNN, ALSO JNNNNNNN                 
*                                       PUB,ALL                                 
         DC    X'76043F01F1'            BILLING TYPE=4,5,6,7                    
         DC    X'520C26'                BILL PERIOD=YYMM/YYMMDD,YYMMDD          
         DC    X'280935'              O INVOICE DATE=YYMMDD                     
         DC    X'50053B'              O DAYS DUE=NN                             
         DC    X'30316B'              O PCT=,R=                                 
*                                       NO PREV,PREV,OLD,NON-RETAIL,            
*                                       NNNNNNNN.NN                             
         DC    X'3A053E0189'          O TYPE OF ITEMS QOPT1                     
         DC    X'3E05400189'          O TYPE OF PUBS QOPT3                      
         DC    X'8C0551'              O OPTIONS                                 
RT119X   DC    X'00',C'R1'                                                      
         SPACE 2                                                                
RT120    DC    AL1(RT120X-*+3,120,0)                                            
         DC    B'11110011',CL22'RBA-REBATE ANALYSIS'                            
         DC    X'0000'                                                          
         DC    AL1(RT120X-*+1),B'11110000'                                      
         DC    X'020E060181'            CLT=ALL/XXX                             
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0C07150181'          O EST=NNN,ALL                             
         DC    X'0E4F1B0181'          O PUB=ALL/NUM/NUM,Z,E/NUM,ALL             
         DC    X'100826'                STRT,ENDD=YYMMDD                        
         DC    X'07070F'              O JOB=ALL/NNNNNN                          
         DC    X'63053E01F7'          O SEE ALL ITEMS Y/N DEF=N                 
         DC    X'81053F'              O REBATABLE ITEMS ONLY                    
         DC    X'820540'              O REBATED ITEMS ONLY                      
         DC    X'6605410139'          O $ OPTION                                
RT120X   DC    X'00',C'RA'                                                      
         SPACE 2                                                                
RT121    DC    AL1(RT121X-*+3,121,0)                                            
         DC    B'11100011',CL22'NVR-NV REPORT/LETTERS'                          
         DC    X'0000'                                                          
         DC    AL1(RT121X-*+1),B'11100000'                                      
         DC    X'02CE060181'            CLI=ALL/XXX/*N/&N (GROUP)/$N            
         DC    X'060F0C0181'          O PRD=ALL/XXX                             
         DC    X'0C8F150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'0E4D1B0181'          O PUB=ALL/NUM/NUM,Z,E/NUM,ALL             
         DC    X'7D0826'                STRD,ENDD=YYMMDD                        
         DC    X'7C053201FD'          O BILL/PAY DATE                           
         DC    X'16053301D101D2'      O SORT=06/07/08/09                        
         DC    X'3C053F'              O ONE PUB PER PASE                        
         DC    X'38054101A301A4'        PRINT OPT=1/2/A/B                       
         DC    X'68054201C1'          O DATA OPT=Y                              
         DC    X'6A0543'              O PAY ADDR=Y/N                            
         DC    X'80054401FB01FC'      O L=LETTERS ONLY,R=REPORT ONLY            
*                                     O F=FAX LETRS,S=NO RPT+FAX LETRS          
RT121X   DC    X'00',C'NV'                                                      
         SPACE 2                                                                
RT122    DC    AL1(RT122X-*+3,122,0)                  ADD 12/17/87   *          
         DC    B'11100011',CL22'NT -NVTEXT LISTING'                  *          
         DC    X'0000'                                               *          
         DC    AL1(RT122X-*+1),B'11100000'                           *          
         DC    X'020A060197'            CLI=ALL,*N                   *          
RT122X   DC    X'00',C'NT'                          ******************          
         SPACE 2                                                                
RT123    DC    AL1(RT123X-*+3,123,0)                                            
         DC    B'11110011',CL22'RBD-REBATE DRAFT'                               
         DC    X'0000'                                                          
         DC    AL1(RT123X-*+1),B'11110000'                                      
         DC    X'021C060181'            CLI=XXX/*N,XXX-XXX                      
         DC    X'0407090181'          O DIV=ALL,NNN                             
         DC    X'08070F0181'          O REG=ALL,NNN                             
         DC    X'0A07120181'          O DST=ALL,NNN                             
         DC    X'06060C0181'            PRD=XXX/ALL                             
         DC    X'0CFE150181'            EST=NNN/ALL/NNN-NNN,NO+F/ALL+F          
         DC    X'0E5D1B0181'          O PUB=ALL/NUM,ALL/NNNNNNNN                
*                                       NNNNNNNN, ALSO JNNNNNNN                 
*                                       PUB,ALL                                 
         DC    X'76043F01F1'            BILLING TYPE=4,5,6,7                    
         DC    X'520C26'                BILL PERIOD=YYMM/YYMMDD,YYMMDD          
         DC    X'280935'              O INVOICE DATE=YYMMDD                     
         DC    X'50053B'              O DAYS DUE=NN                             
         DC    X'30316B'              O PCT=,R=                                 
*                                       NO PREV,PREV,OLD,NON-RETAIL,            
*                                       NNNNNNNN.NN                             
         DC    X'3A053E0189'          O TYPE OF ITEMS QOPT1                     
         DC    X'3E05400189'          O TYPE OF PUBS QOPT3                      
         DC    X'8C0551'              O OPTIONS                                 
RT123X   DC    X'00',C'RD'                                                      
         SPACE 2                                                                
RT124    DC    AL1(RT124X-*+3,124,0)                                            
         DC    B'11110111',CL22'MY -BILL INT REVERSAL'                          
         DC    X'0000'                                                          
         DC    AL1(RT124X-*+1),B'11110000'                                      
         DC    X'028E060181'          CLI=XXX/*N/&N (GROUP)/ALL                 
         DC    X'0407090181'        O DIV=ALL,NNN                               
         DC    X'06060C0181'          PRD=XXX/ALL                               
         DC    X'90061B0129'          BILLING TYPE=ALL,REG,AOR,RET,FIN          
         DC    X'91081E'              DATE=YYMMDD                               
         DC    X'920440012B'          DATE TYPE=R,I                             
         DC    X'930971'            O INTERFACE DATE=YYMMDD                     
         DC    X'940565'            O BILL NUMBER(S) MMXXXX,MMXXXX              
         DC    X'8C0951'            O OPTIONS                                   
RT124X   DC    X'00',C'MY'                                                      
*                                                                               
RT125    DC    AL1(RT125X-*+3,125,0)                                            
         DC    B'11100011',CL22'IC -I/OCOM LISTING'                             
         DC    X'0000'                                                          
         DC    AL1(RT125X-*+1),B'11100000'                                      
         DC    X'0202060197'            CLI=ALL,*N                              
RT125X   DC    X'00',C'IC'                                                      
*                                                                               
RT126    DC    AL1(RT126X-*+3,126,0)                                            
         DC    B'11100011',CL22'CC -CONCOM LISTING'                             
         DC    X'0000'                                                          
         DC    AL1(RT126X-*+1),B'11100000'                                      
         DC    X'0202060197'            CLI=ALL,*N                              
RT126X   DC    X'00',C'CC'                                                      
*                                                                               
RT129    DC    AL1(RT129X-*+3,129,0)                                            
         DC    B'11110011',CL22'BSR-BILLING USER RPT'                           
*                                       EXACTLY LIKE PPL1                       
*                                       BUT RUNS AFTER BILLING                  
         DC    X'0000'                                                          
         DC    AL1(RT129X-*+1),B'11110000'                                      
         DC    X'029C060181'            CLI=XXX/*N/XXX-XXX/&N                   
         DC    X'06070C0181'          O PRD=XXX/ALL                             
         DC    X'0CAF150181'          O EST=NNN/ALL/NNN-NNN,NO+F                
         DC    X'0E4F1B0181'          O PUB=ALL/NUM,ALL,ALL/NNNNNNNN   X        
                                        NNNNNNNN,XX,X                           
         DC    X'0407090181'          O DIV=ALL,NNN                             
         DC    X'08070F0181'          O REG=ALL,NNN                             
         DC    X'0A07120181'          O DST=ALL,NNN                             
         DC    X'070739'              O JOB=ALL/XXXXXX                          
         DC    X'10182601EF'            STRT,END=YYMMDD,ES                      
         DC    X'920532010F0192'      O DATE TYPE=B/P/C/S/M/I/A                 
         DC    X'734035'                USER REP NUM=XXXX                       
         DC    X'74053F'              O SUPPRESS BOXES=Y/N                      
         DC    X'750540'              O BILLED ITEMS ONLY=N/Y                   
         DC    X'770541'              O BILLABLE ITEMS ONL=N/Y                  
         DC    X'830542'              O PAYABLE ITEMS ONLY = N/Y                
         DC    X'34051F010D'          O COMMENT=XXXXXX                          
*                                     SC=XXXXXX IN COL 28 (RPUB+1)              
*                                     COMVAL USES COLNUM 31                     
*                                     CAN'T BE USED WITH PUB OR RD=             
*****          QOPT7 USED FOR INDICATING TEST INSERTS (SEE 02 PHASE)            
RT129X   DC    X'00',C'LB'                                                      
         SPACE 2                                                                
RT130    DC    AL1(RT130X-*+3,130,0)                                            
         DC    B'11100011',CL22'TSR-TEARSHEET REPORT'                           
         DC    X'0000'                                                          
         DC    AL1(RT130X-*+1),B'11100000'                                      
         DC    X'02CE060181'            CLI=ALL/XXX/*N/&N (GROUP)/$N            
         DC    X'060F0C0181'          O PRD=ALL/XXX                             
         DC    X'0C9F150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'0E4D1B0181'          O PUB=ALL/NUM/NUM,Z,E/NUM,ALL             
         DC    X'16053301B3'          O SORT MENU=05,07                         
         DC    X'101826'                STRD,ENDD=ES/YYMMDD                     
         DC    X'140532018F'          O BILL/PAY DATE                           
         DC    X'99053E013D'          O STATUS FILTER                           
         DC    X'3A0540018D'          O TYPE OF ITEMS (PAID/UNPAID)             
         DC    X'3A0541013F'          O TYPE OF ITEMS (BILLED/UNBILLED)         
         DC    X'68053F0141'          O DATA OPT=Y (FLAG BL/PD/TRAF)            
RT130X   DC    X'00',C'TS'                                                      
         SPACE 2                                                                
RT131    DC    AL1(RT131X-*+3,131,0)                                            
         DC    B'11110011',CL22'GT -GT EXTRACT'                                 
         DC    X'0000'                                                          
         DC    AL1(RT131X-*+1),B'11110000'                                      
         DC    X'0206060181'            CLI=ALL/XXX                             
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0C9F150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'100826'                STRD,ENDD=ES/YYMMDD                     
         DC    X'710541'              O PRODUCE TAPE                            
RT131X   DC    X'00',C'GT'                                                      
         SPACE 2                                                                
RT132    DC    AL1(RT132X-*+3,132,0)                                            
         DC    B'11110011',CL22'SN -SONY INTERFACE'                             
         DC    X'0000'                                                          
         DC    AL1(RT132X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'9108260163'            DATE (BILLRUN DATE)                     
         DC    X'68043E0161'            DATA OPTION (I,E)                       
         DC    X'3904430115'            TEST RUN=N,Y                            
RT132X   DC    X'00',C'SN'                                                      
         SPACE 2                                                                
RT133    DC    AL1(RT133X-*+3,133,0)                                            
         DC    B'11110011',CL22'CI -CONTINENTAL'                                
         DC    X'0000'                                                          
         DC    AL1(RT133X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
******   DC    X'3904430115'            TEST RUN=N,Y -NO-OPED                   
RT133X   DC    X'00',C'CI'                                                      
         SPACE 2                                                                
RT134    DC    AL1(RT134X-*+3,134,0)                                            
         DC    B'11110011',CL22'LT -LABATT EXTRACT'                             
         DC    X'0000'                                                          
         DC    AL1(RT134X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0C9F150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'710441'                PRODUCE TAPE                            
RT134X   DC    X'00',C'LT'                                                      
         SPACE 2                                                                
RT135    DC    AL1(RT135X-*+3,135,0)                                            
         DC    B'11110011',CL22'SE -SPRINT INTERFACE'                           
         DC    X'0000'                                                          
         DC    AL1(RT135X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'3904430115'            TEST RUN=N,Y                            
RT135X   DC    X'00',C'SE'                                                      
         SPACE 2                                                                
RT136    DC    AL1(RT136X-*+3,136,0)                                            
         DC    B'11110011',CL22'PH -PHILIP MORRIS INTF'                         
         DC    X'0000'                                                          
         DC    AL1(RT136X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'3904430115'            TEST RUN=N,Y                            
RT136X   DC    X'00',C'PH'                                                      
         SPACE 2                                                                
RT137    DC    AL1(RT137X-*+3,137,0)                                            
         DC    B'11110011',CL22'LOR-L''OREAL INTERFACE'                         
         DC    X'0000'                                                          
         DC    AL1(RT137X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0C9F150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'CD0535'              O START INVOICE#=NNNN                     
         DC    X'CE0539'              O END INVOICE#=NNNN                       
         DC    X'710441'                PRODUCE TAPE                            
RT137X   DC    X'00',C'LO'                                                      
         SPACE 2                                                                
RT138    DC    AL1(RT138X-*+3,138,0)                                            
         DC    B'11110011',CL22'TD -TAB DELIMITED'                              
         DC    X'0000'                                                          
         DC    AL1(RT138X-*+1),B'11110000'                                      
         DC    X'020C060181'            CLI=XXX,*N                              
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'3904430115'            TEST RUN=N,Y                            
RT138X   DC    X'00',C'TD'                                                      
         SPACE 2                                                                
RT139    DC    AL1(RT139X-*+3,139,0)                                            
         DC    B'11110011',CL22'JW  -JWT INTERFACE'                             
         DC    X'0000'                                                          
         DC    AL1(RT139X-*+1),B'11110000'                                      
         DC    X'02CE060181'            CLI=ALL/XXX/*N/&N (GROUP)/$N            
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0C9F150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'CD0535'              O START INVOICE#=NNNN                     
         DC    X'CE0539'              O END INVOICE#=NNNN                       
         DC    X'710441'                PRODUCE TAPE                            
RT139X   DC    X'00',C'JW'                                                      
         SPACE 2                                                                
RT140    DC    AL1(RT140X-*+3,140,0)                                            
         DC    B'11110011',CL22'WB -WB INTERFACE'                               
         DC    X'0000'                                                          
         DC    AL1(RT140X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0C9F150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'710541'              O PRODUCE TAPE                            
         DC    X'8C0D8E'              O OPTIONS                                 
RT140X   DC    X'00',C'WB'                                                      
         SPACE 2                                                                
RT141    DC    AL1(RT141X-*+3,141,0)                                            
         DC    B'11110011',CL22'IN -NISSAN INTF'                                
         DC    X'0000'                                                          
         DC    AL1(RT141X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06060C0181'          O PRD=ALL/XXX                             
         DC    X'0CFE150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'710441'              O PRODUCE FILE?                           
RT141X   DC    X'00',C'IN'                                                      
         SPACE 2                                                                
RT142    DC    AL1(RT142X-*+3,142,0)                                            
         DC    B'11110011',CL22'CH -CHOICE HOTELS'                              
         DC    X'0000'                                                          
         DC    AL1(RT142X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
***      DC    X'06070C0181'          O PRD=ALL/XXX                             
***      DC    X'0C9F150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'390443'              O TEST RUN?                               
RT142X   DC    X'00',C'CH'                                                      
         SPACE 2                                                                
RT143    DC    AL1(RT143X-*+3,143,0)                                            
         DC    B'11110011',CL22'PZ -PFIZER INTERFACE'                           
         DC    X'0000'                                                          
         DC    AL1(RT143X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06070C0181'          O PRD=ALL/XXX                             
***      DC    X'0C9F150181'          O EST=NNN,ALL,NO+F,NNN-NNN                
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'390443'                TEST RUN?                               
RT143X   DC    X'00',C'PZ'                                                      
*                                                                               
RT144    DC    AL1(RT144X-*+3,144,0)                                            
         DC    B'11110011',CL22'AI -AT&&T INTERFACE'                            
         DC    X'0000'                                                          
         DC    AL1(RT144X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0C0F150181'          O EST=NNN,ALL,NNN-NNN                     
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'390443'                TEST RUN?                               
RT144X   DC    X'00',C'AI'                                                      
         SPACE 2                                                                
RT145    DC    AL1(RT145X-*+3,145,0)                                            
         DC    B'11100011',CL22'VL-VENDOR LEVEL REPORT'                         
         DC    X'0000'                                                          
         DC    AL1(RT145X-*+1),B'11110000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'06070C0181'          O PRD=ALL/XXX                             
         DC    X'0C0F150181'          O EST=NNN,ALL,NNN-NNN                     
         DC    X'10082601E7'            STRD,ENDD=YYMMDD                        
         DC    X'710541'              O PRODUCE TAPE=Y/N                        
RT145X   DC    X'00',C'VL'                                                      
         SPACE 2                                                                
RT212    DC    AL1(RT212X-*+3,212,0)                                            
         DC    B'11100011',CL22'ARC-AOR CONTRACTS'                              
         DC    X'0000'                                                          
         DC    AL1(RT212X-*+1),B'11100000'                                      
         DC    X'0204060181'            CLI=XXX (ADVERTISER)                    
         DC    X'980F18'              O AGENCY FILTER=ALL,AGY,FILTER            
         DC    X'0E6D1B0181'          O PUB=NNN/NUM,Z,E/NUM,ALL,ALL             
*                                       NUM,Z,ALL                               
         DC    X'4E0D15'              O CONTRACT=NNN                            
         DC    X'100926'              O STRT,END=YYMMDD                         
         DC    X'16053301B3'          O SORT MENU=05,07                         
         DC    X'38053E01910194'      O PRT OPT                                 
         DC    X'56053F0199'          O OPT2=N                                  
         DC    X'580540019B'          O OPT3=R                                  
         DC    X'5E0541'              O GRID FORMAT=Y,N                         
         DC    X'5F054201A7'            CHANGE OPT=F/C                          
         DC    X'60054301A301A4'        AD NUM OPT=A/B/1/2/                     
* NO-OP  DC    X'6C054401CF'          O SCHED SORT=P,D                          
         DC    X'46093A'              O CONTROL DATE=YYMMDD                     
         DC    X'340565'              O COMMENT=XXXXXX                          
         DC    X'4B054401ED'          O TOTALS OPTION=I,L                       
RT212X   DC    X'00',C'AC'                                                      
         SPACE 2                                                                
RT217    DC    AL1(RT217X-*+3,217,0)                                            
         DC    B'11100011',CL22'ACR-AOR CONTRACT RPT'                           
         DC    X'0000'                                                          
         DC    AL1(RT217X-*+1),B'11100000'                                      
         DC    X'58043D01350136'        REPORT TYPE=A,L,R                       
         DC    X'0204060181'            CLI=XXX (ADVERTISER)                    
         DC    X'0E4D1B0181'          O PUB=NUM,NUM,Z,E/NUM,ALL                 
         DC    X'4E0515'              O CONTRACT=NNN                            
         DC    X'97054001370138'      O LEVEL/TEST OPT N,Y,H,L                  
         DC    X'38053F01C7013A'      O PRINT OPT=Y                             
*                                FOR RPT=R; H,L,B FOR RPT=L OR A                
***      FIELDS BELOW ONLY APPLY FOR REPORT TYPES A OR L                        
*                                                                               
         DC    X'100926'              O STRT,ENDD=YYMMDD                        
         DC    X'16053301B3'          O SORT MENU=05,07                         
         DC    X'46093A'              O CONTROL DATE=YYMMDD                     
         DC    X'5A053E01B901BA'      O DATE OPTION=Y,N                         
         DC    X'5F054101C9'          O CHG OPT=Y                               
******   DC    X'850542'              O SUPPRESS INACTIVE CONTRACTS?            
**NOTE                 QOPT6 SET TO Y IF REQUESTED AS ART                       
**                     TO INCLUDE TEST ESTIMATES                                
RT217X   DC    X'00',C'AR'                                                      
         SPACE 2                                                                
RT219    DC    AL1(RT219X-*+3,219,0)                                            
         DC    B'11100011',CL22'AUR-AOR UTILIZATION'                            
         DC    X'0000'                                                          
         DC    AL1(RT219X-*+1),B'11100000'                                      
         DC    X'0204060181'            CLI=XXX (ADVERTISER)                    
         DC    X'980F18'              O AGENCY FILTER=ALL,AGY,FILTER            
         DC    X'0E4D1B0181'          O PUB=NUM,NUM,Z,E/NUM,ALL                 
         DC    X'4E0515'              O CONTRACT=NNN                            
         DC    X'100926'              O STRT,ENDD=YYMMDD                        
         DC    X'16053301B3'          O SORT MENU=05,07                         
         DC    X'460935'              O CONTROL DATE=YYMMDD                     
         DC    X'5A053E01B901BA'      O DATE OPTION=Y,N                         
         DC    X'38053F01C7'          O PRINT OPT=Y                             
         DC    X'5F054101C9'          O CHG OPT=Y                               
         DC    X'850542'              O SUPPRESS INACTIVE CONTRACTS?            
         DC    X'8705400113'          O LEVEL OPT=H,L,B                         
         DC    X'6C053B01CF'          O SCHEDULE SORT P,D                       
         DC    X'89053C'              O SUPPRESS NET COLUMN                     
*******  DC    X'63053D0119'          O DETAIL OPTION (Y=SUPPRESS)              
**NEW19                                                                         
         DC    X'63058E0119'          O DETAIL OPTION (Y=SUPPRESS)              
         DC    X'8C0D8E'              O OPTIONS                                 
**NEW19                                                                         
**NOTE                 DETAIL OPTION CHANGED TO QOPT8 FROM QOPT1-1              
**                     TO ALLOW FOR CONTINUATION CARD                           
**NOTE                 QOPT6 SET TO Y IF REQUESTED AS 19T                       
**                     TO INCLUDE TEST BUYS                                     
RT219X   DC    X'00',C'AU'                                                      
*                                                                               
RT228    DC    AL1(RT228X-*+3,228,0)                                            
         DC    B'11100011',CL22'Z5 -EASI CONVERSION RP'                         
         DC    X'0000'                                                          
         DC    AL1(RT228X-*+1),B'11100000'                                      
         DC    X'0E4D1B0181'            PUB=NUM,NUM,Z,E/NUM,ALL                 
         DC    X'100926'                STRT,ENDD=YYMMDD                        
         DC    X'39043E'              O TEST RUN=Y                              
         DC    X'C60540'              O LIST PREV CONVERSIONS                   
         DC    X'C70541'              O REPLACE EXISTING INVOICES               
         DC    X'C80542'              O Y=AUTO MATCH?                           
*                                     P= CREATE AND PRINT(DDS)                  
         DC    X'CA0543'              O POST INVOICE                            
         DC    X'C9053F'              O TRACE DDS ONLY(HIDDEN FIELD)            
         DC    X'8C0551'              O OPTIONS                                 
RT228X   DC    X'00',C'Z5'                                                      
*                                                                               
RTVK     DC    AL1(RTVK_X-*+3,229,0)                                            
         DC    B'11110011',CL22'VK -VENDOR LOCK REPORT'                         
         DC    X'0000'                                                          
         DC    AL1(RTVK_X-*+1),B'11110000'                                      
         DC    X'910826'                DATE=YYMMDD                             
         DC    X'390441'                TEST RUN=Y,N                            
RTVK_X   DC    X'00',C'VK'                                                      
*                                                                               
         SPACE 2                                                                
*     COL 1: ENTRY NUMBER FROM PREVIOUS TABLE                                   
*     COL 2: FIND IN PREV TABLE WHAT VALIDATION ROUTINE IT IS GOING TO          
*             - IF REQUIRED OR OPTIONAL (CHECK THIS WITH COMMENT AT             
*               HEAD OF ROUTINE                                                 
*     COL 3: COLUMN IN REQUEST CARD                                             
*     COL 4: AND BEYOND ARE COMMENTS TO PRINT OUT NEXT TO FIELDS                
*                                                                               
         EJECT                                                                  
       ++INCLUDE PRREQSAVE                                                      
         EJECT                                                                  
       ++INCLUDE PRREQTEMP                                                      
       ++INCLUDE PRREQFFD                                                       
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAGETTXTD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058PRREQ00   06/09/20'                                      
         END                                                                    
